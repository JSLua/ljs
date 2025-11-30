/*
    Copyright (C) 2025  `zyxwvu` Shi <i@shiyc.cn>

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, see
    <https://www.gnu.org/licenses/>.
*/

#include <lua.h>
#include <lauxlib.h>

#ifdef LJS_ENABLE_LIBEV
# include <ev.h>
int luaJS_flush_task(lua_State *L);

static void task_flush_cb(EV_P_ struct ev_idle *handle, int e)
{
    lua_State *L = ev_userdata(EV_A);
    ev_idle_stop(EV_A_ handle);
    lua_pushcfunction(L, luaJS_flush_task), lua_call(L, 0, 0);
}

static struct ev_idle task_runner = { .cb = task_flush_cb };
#endif

struct js_promise { short state; };
#define PROMISE_FULFILLED   1
#define PROMISE_REJECTED    2

#define PROMISE_REACTION    1
#define PROMISE_ON_REJECT   2
#define PROMISE_VALUE_IDX   2
#define PROMISE_CHAIN_NEXT  3
#define PROMISE_TASK_NEXT   0

static int promise_tostring(lua_State *L)
{
    luaL_Buffer b;
    struct js_promise *self = luaL_checkudata(L, 1, "JS:Promise");
    if (self->state < PROMISE_FULFILLED) {
        lua_pushliteral(L, "Promise { <pending> }");
        return 1;
    }
    luaL_buffinit(L, &b);
    luaL_addlstring(&b, "Promise { ", 10);
    if (self->state == PROMISE_REJECTED)
        luaL_addlstring(&b, "<rejected> ", 11);
    lua_getfenv(L, 1), lua_replace(L, 1);
    lua_pushvalue(L, lua_upvalueindex(1));
    lua_rawgeti(L, 1, PROMISE_VALUE_IDX);
    lua_call(L, 1, 1), luaL_addvalue(&b);
    luaL_addlstring(&b, " }", 2);
    luaL_pushresult(&b);
    return 1;
}

#define PROMISE_REG_HEAD  (-1)
static int tail_ref = 0;

static void promise_enqueue(lua_State *L)
{
    lua_getfenv(L, -1);
    if (!tail_ref) {
        tail_ref = luaL_ref(L, LUA_REGISTRYINDEX);
        lua_rawseti(L, LUA_REGISTRYINDEX, PROMISE_REG_HEAD);
#ifdef LJS_ENABLE_LIBEV
        if (EV_DEFAULT_UC) ev_idle_start(EV_DEFAULT_UC_ &task_runner);
#endif
    } else {
        lua_rawgeti(L, LUA_REGISTRYINDEX, tail_ref), lua_insert(L, -3);
        lua_rawseti(L, LUA_REGISTRYINDEX, tail_ref);
        lua_rawseti(L, -2, PROMISE_TASK_NEXT), lua_pop(L, 1);
    }
}

static void awaiter_exec(lua_State *th, int narg)
{
    int status = lua_resume(th, narg);
    if (status == LUA_YIELD) {
        struct js_promise *p = lua_touserdata(th, 1);
        if (!p) { lua_settop(th, 1); return awaiter_exec(th, 1); }
        lua_getfenv(th, 1);
        lua_pushthread(th), lua_rawseti(th, 2, PROMISE_REACTION);
        if (!p->state)  /* Pending? */
            lua_pushthread(th), lua_rawseti(th, 2, PROMISE_ON_REJECT);
        else lua_settop(th, 1), promise_enqueue(th);
    }
}

#define use_reject_reaction(idx) lua_rawgeti(L, idx, PROMISE_ON_REJECT),\
    lua_rawseti(L, idx, PROMISE_REACTION)

int luaJS_flush_task(lua_State *L)
{
    lua_settop(L, 0), lua_rawgeti(L, LUA_REGISTRYINDEX, PROMISE_REG_HEAD);
    lua_pushnil(L), lua_rawseti(L, LUA_REGISTRYINDEX, PROMISE_REG_HEAD);
    for ( ; !lua_isnil(L, 1); lua_rawgeti(L, 1, PROMISE_TASK_NEXT),
            lua_replace(L, 1), lua_settop(L, 1)) {
        short next_state = *(short *)lua_touserdata(L, 1);
        lua_getfenv(L, 1), lua_replace(L, 1);
        lua_rawgeti(L, 1, PROMISE_REACTION);  /* at index 3 */
        if (lua_isthread(L, 2)) {
            lua_State *th = lua_tothread(L, 2);
            if (next_state == PROMISE_REJECTED) lua_pushboolean(th, 0);
            lua_rawgeti(L, 1, PROMISE_VALUE_IDX), lua_xmove(L, th, 1);
            awaiter_exec(th, 1 + (next_state == PROMISE_REJECTED));
            continue;
        }
        else if (lua_isnil(L, 2))  /* Use "Identity" (x => x) */
            lua_settop(L, 1), lua_rawgeti(L, 1, PROMISE_VALUE_IDX);
        else {
            lua_pushnil(L), lua_rawgeti(L, 1, PROMISE_VALUE_IDX);
            next_state = lua_pcall(L, 2, 1, 0) == LUA_OK ?
                         PROMISE_FULFILLED : PROMISE_REJECTED;
        }

        lua_rawgeti(L, 1, PROMISE_CHAIN_NEXT);  /* at index 3 */
        if (lua_isuserdata(L, 3)) {
            struct js_promise *next = lua_touserdata(L, 3);
            if (next->state < PROMISE_FULFILLED) {  /* Pass if settled */
                lua_getfenv(L, 3);  /* Use the env of next Promise */
                if (next_state == PROMISE_REJECTED) use_reject_reaction(4);
                lua_pushvalue(L, 2), lua_rawseti(L, 4, PROMISE_VALUE_IDX);
                lua_pop(L, 1);  /* env */
                next->state = next_state;
                promise_enqueue(L);  /* next in chain */
            }
        } else if (next_state == PROMISE_REJECTED) {
            if (luaL_callmeta(L, 2, "__tostring")) lua_replace(L, 2);
            fprintf(stderr, "Uncaught reject: %s\n", lua_tostring(L, 2));
        }
    }
    luaL_unref(L, LUA_REGISTRYINDEX, tail_ref);
    tail_ref = 0;  /* last task processed */
    return 0;
}

static int promise_then(lua_State *L)
{
    struct js_promise *self = luaL_checkudata(L, 1, "JS:Promise"), *new;
    lua_settop(L, 3);
    if (!lua_isnil(L, 2)) luaL_checktype(L, 2, LUA_TFUNCTION);
    if (!lua_isnil(L, 3)) luaL_checktype(L, 3, LUA_TFUNCTION);
    lua_getfenv(L, 1);  /* Promise env at index 4 */
    if (self->state == PROMISE_REJECTED)
        lua_pushvalue(L, 3), lua_rawseti(L, 4, PROMISE_REACTION);
    else {
        lua_pushvalue(L, 2), lua_rawseti(L, 4, PROMISE_REACTION);
        if (self->state < PROMISE_FULFILLED)  /* Only if unresolved */
            lua_pushvalue(L, 3), lua_rawseti(L, 4, PROMISE_ON_REJECT);
    }

    new = lua_newuserdata(L, sizeof(*new));  /* at index 5 */
    new->state = 0;
    lua_createtable(L, 3, 0), lua_setfenv(L, 5);
    lua_getmetatable(L, 1), lua_setmetatable(L, 5);
    lua_pushvalue(L, 5), lua_rawseti(L, 4, PROMISE_CHAIN_NEXT);
    if (self->state >= PROMISE_FULFILLED)
        lua_pushvalue(L, 1), promise_enqueue(L);
    return 1;
}

static int promise_resolve(lua_State *L)
{
    struct js_promise *task = lua_touserdata(L, lua_upvalueindex(1));
    if (task->state >= PROMISE_FULFILLED)
        return 0;  /* If already resolved, return undefined (nothing) */
    lua_settop(L, 2);  /* Truncate excess args or push a null */
    lua_getfenv(L, lua_upvalueindex(1)), lua_replace(L, 1);
    if (luaL_testudata(L, 2, "JS:Promise")) {
        struct js_promise *prev = lua_touserdata(L, 2);
        if (lua_rawequal(L, 2, lua_upvalueindex(1)))
            return luaL_error(L, "Chaining cycle detected for promise");
        lua_getfenv(L, 2), lua_replace(L, 2); /* Use its env */
        if (prev->state >= PROMISE_FULFILLED) {
            if (prev->state == PROMISE_REJECTED) use_reject_reaction(1);
            lua_rawgeti(L, 2, PROMISE_VALUE_IDX),
                lua_rawseti(L, 1, PROMISE_VALUE_IDX);
            task->state = prev->state;
        } else {
            lua_pushvalue(L, lua_upvalueindex(1));
            lua_rawseti(L, 2, PROMISE_CHAIN_NEXT);
            return 0;  /* Still pending, no need to enqueue */
        }
    } else {
        lua_pushvalue(L, 2), lua_rawseti(L, 1, PROMISE_VALUE_IDX);
        task->state = PROMISE_FULFILLED;
    }
    lua_pushvalue(L, lua_upvalueindex(1)), promise_enqueue(L);
    return 0;
}

static int promise_reject(lua_State *L)
{
    struct js_promise *task = lua_touserdata(L, lua_upvalueindex(1));
    luaL_checkany(L, 2), lua_settop(L, 2);
    lua_getfenv(L, lua_upvalueindex(1)), lua_replace(L, 1);
    if (task->state < PROMISE_FULFILLED) {
        use_reject_reaction(1), lua_rawseti(L, 1, PROMISE_VALUE_IDX);
        task->state = PROMISE_REJECTED;
        lua_pushvalue(L, lua_upvalueindex(1)), promise_enqueue(L);
    }
    return 0;
}

static int promise__resolve(lua_State *L)  /* metamethod _resolve */
{
    struct js_promise *task = luaL_checkudata(L, 1, "JS:Promise");
    if (task->state >= PROMISE_FULFILLED) return 0;
    task->state = PROMISE_FULFILLED;
    lua_settop(L, 1), promise_enqueue(L);
    return 0;
}

static int promise_new(lua_State *L)
{
    struct js_promise *self;
    if (!lua_istable(L, 2))
        return luaL_error(L, "Promise constructor requires 'new'");
    luaL_checktype(L, 3, LUA_TFUNCTION), lua_settop(L, 3);
    self = lua_newuserdata(L, sizeof(*self));  /* at index 4 */
    self->state = 0;
    lua_pushvalue(L, 2), lua_setfenv(L, 4);
    lua_pushvalue(L, lua_upvalueindex(1)), lua_setmetatable(L, 4);
    lua_pushvalue(L, 3);  /* the executor */
    lua_pushnil(L);  /* this == null */
    lua_pushvalue(L, 4), lua_pushcclosure(L, promise_resolve, 1);
    lua_pushvalue(L, 4), lua_pushcclosure(L, promise_reject, 1);
    if (lua_pcall(L, 3, 0, 0) != LUA_OK && self->state == 0) {
        lua_rawseti(L, 2, PROMISE_VALUE_IDX);
        self->state = PROMISE_REJECTED;
    }
    return 1;
}

static int promise_resolved(lua_State *L)
{
    struct js_promise *self;
    lua_Integer state = lua_tointeger(L, lua_upvalueindex(2));
    lua_settop(L, 2);
    if (state == PROMISE_FULFILLED && luaL_testudata(L, 2, "JS:Promise"))
        return 1;  /* Pass through any Promise received */
    self = lua_newuserdata(L, sizeof(*self));  /* at index 3 */
    lua_createtable(L, 3, 0), lua_pushvalue(L, 2),
            lua_rawseti(L, -2, PROMISE_VALUE_IDX), lua_setfenv(L, 3);
    self->state = (short)state;
    lua_pushvalue(L, lua_upvalueindex(1)), lua_setmetatable(L, 3);
    return 1;
}

static struct js_promise *promise_create(lua_State *L, int mt)
{
    struct js_promise *p = lua_newuserdata(L, sizeof(*p));
    p->state = 0;
    lua_createtable(L, 3, 0), lua_setfenv(L, -2);
    if (mt) lua_pushvalue(L, lua_upvalueindex(mt));
    else luaL_getmetatable(L, "JS:Promise");
    lua_setmetatable(L, -2);
    return p;
}

static int promise_with_resolvers(lua_State *L)
{
    lua_settop(L, 0), promise_create(L, 1);
    lua_createtable(L, 0, 3);  /* Return object */
    lua_pushvalue(L, lua_upvalueindex(2));  /* "promise" */
    lua_pushvalue(L, 1), lua_rawset(L, 2);
    lua_pushvalue(L, lua_upvalueindex(3));  /* "resolve" */
    lua_pushvalue(L, 1), lua_pushcclosure(L, promise_resolve, 1);
    lua_rawset(L, 2);
    lua_pushvalue(L, lua_upvalueindex(4));  /* "reject" */
    lua_pushvalue(L, 1), lua_pushcclosure(L, promise_reject, 1);
    lua_rawset(L, 2);
    return 1;
}

static int promise_race(lua_State *L)
{
    luaL_checktype(L, 2, LUA_TTABLE), lua_settop(L, 2);
    struct js_promise *p = promise_create(L, 1);
    lua_replace(L, 1);
    for (int i = 0; p->state < PROMISE_FULFILLED; i++) {
        lua_settop(L, 2), lua_rawgeti(L, 2, i);
        if (lua_isnil(L, -1)) { if (i != 0) break; }
        else if (luaL_testudata(L, -1, "JS:Promise")) {
            struct js_promise *e = lua_touserdata(L, -1);
            if (e->state < PROMISE_FULFILLED) continue;
            lua_getfenv(L, -1), lua_getfenv(L, 1);
            lua_rawgeti(L, -2, PROMISE_VALUE_IDX);
            lua_rawseti(L, -2, PROMISE_VALUE_IDX);
            p->state = e->state;
        } else {
            lua_getfenv(L, 1), lua_insert(L, 3);
            lua_rawseti(L, 3, PROMISE_VALUE_IDX);
            p->state = PROMISE_FULFILLED;
        }
    }

    if (p->state < PROMISE_FULFILLED) {  /* Still not settled */
        for (int i = 0;; i++) {
            lua_settop(L, 2), lua_rawgeti(L, 2, i);
            if (lua_isnil(L, -1)) { if (i == 0) continue; else break; }
            lua_getfenv(L, -1);
            lua_pushvalue(L, 1), lua_rawseti(L, -2, PROMISE_CHAIN_NEXT);
            lua_pop(L, 1);
        }
    }

    lua_settop(L, 1);
    return 1;
}

int luaopen_js_promise(lua_State *L)
{
    lua_newtable(L);  /* Promise constructor at index 1 */
    lua_newtable(L);  /* Metatable for constructor at index 2 */
    luaL_newmetatable(L, "JS:Promise");  /* Metatable at index 3 */
    lua_getglobal(L, "tostring"), lua_pushcclosure(L, promise_tostring, 1);
    lua_setfield(L, 3, "__tostring");
    lua_newtable(L);  /* __index */
    lua_pushcfunction(L, promise_then), lua_setfield(L, -2, "then");
    lua_pushvalue(L, -1), lua_setfield(L, 1, "prototype");
    lua_setfield(L, 3, "__index");
    lua_pushcfunction(L, promise__resolve), lua_setfield(L, 3, "_resolve");
    lua_pushliteral(L, "JS:Promise"), lua_setfield(L, 3, "__metatable");
    /* Install static methods */
    lua_pushvalue(L, 3), lua_pushinteger(L, PROMISE_FULFILLED);
    lua_pushcclosure(L, promise_resolved, 2), lua_setfield(L, 1, "resolve");
    lua_pushvalue(L, 3), lua_pushinteger(L, PROMISE_REJECTED);
    lua_pushcclosure(L, promise_resolved, 2), lua_setfield(L, 1, "reject");
    lua_pushvalue(L, 3), lua_pushliteral(L, "promise");
    lua_pushliteral(L, "resolve"), lua_pushliteral(L, "reject");
    lua_pushcclosure(L, promise_with_resolvers, 4);
    lua_setfield(L, 1, "withResolvers");
    lua_pushvalue(L, 3), lua_pushcclosure(L, promise_race, 1);
    lua_setfield(L, 1, "race");
    /* Metatable is at -1. Set constructor as __call */
    lua_pushcclosure(L, promise_new, 1), lua_setfield(L, -2, "__call");
    lua_setmetatable(L, 1);  /* Pops the metatable for constructor */
    return 1;
}

__attribute__((visibility("hidden")))
int promise_launch_async(lua_State *L)
{
    int argc = lua_gettop(L);
    lua_State *th = lua_newthread(L);
    lua_getfield(th, LUA_REGISTRYINDEX, "JS:__resolver");
    promise_create(th, 0), lua_pushcclosure(th, promise_resolve, 1);
    lua_pushvalue(L, lua_upvalueindex(1)), lua_xmove(L, th, 1);
    lua_settop(L, argc), lua_xmove(L, th, argc);
    lua_getupvalue(th, 2, 1), lua_xmove(th, L, 1);
    awaiter_exec(th, 2 + argc);
    return 1;
}