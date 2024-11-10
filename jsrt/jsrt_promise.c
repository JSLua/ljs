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
#include <string.h>

#define PROMISE_FULFILLED   1
#define PROMISE_REJECTED    2

#define PROMISE_REACTION    1
#define PROMISE_ON_REJECT   2
#define PROMISE_VALUE_IDX   2
#define PROMISE_CHAIN_NEXT  3
#define PROMISE_TASK_NEXT   0

#define PROMISE_REG_HEAD  (-1)
#define PROMISE_REG_TAIL  (-2)

/**
 *  If not committed, PROMISE_VALUE_IDX is the arg of ON_FULFILL.
 *  If committed, PROMISE_VALUE_IDX is the resolved value.
 */
struct js_promise {
    short state;
};

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
    lua_getglobal(L, "tostring");
    lua_rawgeti(L, 1, PROMISE_VALUE_IDX);
    lua_call(L, 1, 1), luaL_addvalue(&b);
    luaL_addlstring(&b, " }", 2);
    luaL_pushresult(&b);
    return 1;
}

static void promise_enqueue(lua_State *L)
{
    lua_rawgeti(L, LUA_REGISTRYINDEX, PROMISE_REG_TAIL);
    if (lua_isnil(L, -1)) {
        lua_pushvalue(L, -2);
        lua_rawseti(L, LUA_REGISTRYINDEX, PROMISE_REG_HEAD);
    } else {
        lua_getfenv(L, -1), lua_replace(L, -2);  /* Use its env */
        lua_pushvalue(L, -2), lua_rawseti(L, -2, PROMISE_TASK_NEXT);
    }
    lua_pop(L, 1);
    lua_rawseti(L, LUA_REGISTRYINDEX, PROMISE_REG_TAIL);
}

static void awaiter_exec(lua_State *th, int narg)
{
    int status = lua_resume(th, narg);
    if (status == LUA_YIELD) {
        struct js_promise *p = lua_touserdata(th, 1);
        lua_getfenv(th, 1);
        lua_pushthread(th), lua_rawseti(th, 2, PROMISE_REACTION);
        if (!p->state)  /* Pending? */
            lua_pushthread(th), lua_rawseti(th, 2, PROMISE_ON_REJECT);
        else lua_settop(th, 1), promise_enqueue(th);
    }
}

static void process_single_task(lua_State *L)
{
    int task_idx = lua_gettop(L);
    struct js_promise *task = lua_touserdata(L, task_idx);
    lua_getfenv(L, task_idx), lua_replace(L, task_idx);
    lua_rawgeti(L, task_idx, PROMISE_REACTION);

    short next_state = task->state;
    if (lua_isthread(L, -1)) {
        lua_State *th = lua_tothread(L, -1);
        if (task->state == PROMISE_REJECTED)
            lua_pushboolean(th, 0);  /* Throw indicator */
        lua_rawgeti(L, task_idx, PROMISE_VALUE_IDX), lua_xmove(L, th, 1);
        awaiter_exec(th, task->state < PROMISE_REJECTED ? 1 : 2);
        return lua_settop(L, task_idx);
    }
    else if (lua_isnil(L, -1))  /* Use "Identity" (x => x) */
        lua_pop(L, 1), lua_rawgeti(L, task_idx, PROMISE_VALUE_IDX);
    else {
        lua_pushnil(L), lua_rawgeti(L, task_idx, PROMISE_VALUE_IDX);
        next_state = lua_pcall(L, 2, 1, 0) == LUA_OK ?
                PROMISE_FULFILLED : PROMISE_REJECTED;
    }

    lua_rawgeti(L, task_idx, PROMISE_CHAIN_NEXT);
    if (lua_isuserdata(L, -1)) {
        struct js_promise *next = lua_touserdata(L, -1);
        if (next->state < PROMISE_FULFILLED) {  /* Pass if settled */
            lua_getfenv(L, -1);  /* Use the env of next Promise */
            if (next_state == PROMISE_REJECTED)
                lua_rawgeti(L, -1, PROMISE_ON_REJECT),
                        lua_rawseti(L, -2, PROMISE_REACTION);
            lua_pushvalue(L, task_idx + 1),
                    lua_rawseti(L, -2, PROMISE_VALUE_IDX);
            lua_pop(L, 1);  /* env */
            next->state = next_state;
            promise_enqueue(L);  /* next in chain */
        }
    } else if (next_state == PROMISE_REJECTED) {
        if (luaL_callmeta(L, task_idx + 1, "__tostring"))
            lua_replace(L, task_idx + 1);
        fprintf(stderr, "Uncaught reject: %s\n",
                lua_tostring(L, task_idx + 1));
    }
    lua_settop(L, task_idx);
}

int luaJS_flush_task(lua_State *L)
{
    lua_settop(L, 0), lua_rawgeti(L, LUA_REGISTRYINDEX, PROMISE_REG_HEAD);
    lua_pushnil(L), lua_rawseti(L, LUA_REGISTRYINDEX, PROMISE_REG_HEAD);
    while (!lua_isnil(L, -1)) {
        process_single_task(L);
        lua_rawgeti(L, -1, PROMISE_TASK_NEXT), lua_replace(L, -2);
    }
    lua_rawseti(L, LUA_REGISTRYINDEX, PROMISE_REG_TAIL);
    return 0;
}

static int promise_then(lua_State *L)
{
    struct js_promise *self = luaL_checkudata(L, 1, "JS:Promise"), *new;
    lua_settop(L, 3);
    if (!lua_isnil(L, 2)) luaL_checktype(L, 2, LUA_TFUNCTION);
    if (!lua_isnil(L, 3)) luaL_checktype(L, 3, LUA_TFUNCTION);
    lua_getfenv(L, 1);  /* Env of the Promise at index 4 */
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
            lua_rawgeti(L, 2, PROMISE_VALUE_IDX);
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
        lua_rawgeti(L, 1, PROMISE_ON_REJECT),
                lua_rawseti(L, 1, PROMISE_REACTION);
        lua_rawseti(L, 1, PROMISE_VALUE_IDX);
        task->state = PROMISE_REJECTED;
        lua_pushvalue(L, lua_upvalueindex(1)), promise_enqueue(L);
    }
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

    lua_pushvalue(L, 3);
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
    luaL_checkany(L, 2), lua_settop(L, 2);
    if (state == PROMISE_FULFILLED && luaL_testudata(L, 2, "JS:Promise"))
        return 1;  /* Pass through any Promise received */
    self = lua_newuserdata(L, sizeof(*self));  /* at index 3 */
    lua_createtable(L, 3, 0), lua_pushvalue(L, 2),
            lua_rawseti(L, -2, PROMISE_VALUE_IDX), lua_setfenv(L, 3);
    self->state = (short)state;
    lua_pushvalue(L, lua_upvalueindex(1)), lua_setmetatable(L, 3);
    return 1;
}

static int promise_with_resolvers(lua_State *L)
{
    struct js_promise *p = lua_newuserdata(L, sizeof(*p));
    p->state = 0;
    lua_createtable(L, 3, 0), lua_setfenv(L, -2);
    lua_pushvalue(L, lua_upvalueindex(1)), lua_setmetatable(L, -2);
    lua_createtable(L, 0, 3);  /* Return object */
    lua_pushvalue(L, -2), lua_setfield(L, -2, "promise");
    lua_pushvalue(L, -2), lua_pushcclosure(L, promise_resolve, 1);
    lua_setfield(L, -2, "resolve");
    lua_pushvalue(L, -2), lua_pushcclosure(L, promise_reject, 1);
    lua_setfield(L, -2, "reject");
    return 1;
}

static int promise_race(lua_State *L)
{
    luaL_checktype(L, 2, LUA_TTABLE), lua_settop(L, 2);
    struct js_promise *p = lua_newuserdata(L, sizeof(*p));
    p->state = 0;
    lua_createtable(L, 3, 0), lua_setfenv(L, -2);
    luaL_setmetatable(L, "JS:Promise"), lua_replace(L, 1);

    size_t n_elements = lua_objlen(L, 2);
    for (int i = 1; i <= n_elements; i++) {
        lua_settop(L, 2), lua_rawgeti(L, 2, i);
        if (!luaL_testudata(L, -1, "JS:Promise")) {
            lua_getfenv(L, 1), lua_insert(L, 3);
            lua_rawseti(L, 3, PROMISE_VALUE_IDX);
            p->state = PROMISE_FULFILLED;
            goto resolved;
        }

        struct js_promise *e = lua_touserdata(L, -1);
        if (e->state >= PROMISE_FULFILLED) {
            lua_getfenv(L, -1), lua_pushvalue(L, 1),
                lua_rawseti(L, -2, PROMISE_CHAIN_NEXT), lua_pop(L, 1);
            promise_enqueue(L);
            goto resolved;
        }
    }

    /* At this point, none of given object is settled */
    lua_pushvalue(L, 1), lua_pushcclosure(L, promise_resolve, 1);
    for (int i = 1; i <= n_elements; i++)
        lua_pushvalue(L, -1), lua_pushnil(L), lua_rawgeti(L, 2, i),
        lua_call(L, 2, 0);  /* resolve(element) */
resolved:
    lua_settop(L, 1);
    return 1;
}

int promise_new_async(lua_State *L)
{
    int argc = lua_gettop(L);
    struct js_promise *p = lua_newuserdata(L, sizeof(*p));
    p->state = 0;
    lua_createtable(L, 3, 0), lua_setfenv(L, 3);
    luaL_setmetatable(L, "JS:Promise"), lua_insert(L, 1);

    lua_State *th = lua_newthread(L);
    lua_pushvalue(L, lua_upvalueindex(1));  /* the async function */
    lua_pushvalue(L, 1), lua_pushcclosure(L, promise_resolve, 1);
    lua_pushvalue(L, 1), lua_pushcclosure(L, promise_reject, 1);
    lua_xmove(L, th, 3);
    lua_settop(L, argc + 1), lua_xmove(L, th, argc);
    awaiter_exec(th, 2 + argc);
    return 1;
}

int luaopen_js_promise(lua_State *L)
{
    lua_newtable(L);  /* Promise constructor at index 1 */
    lua_newtable(L);  /* Metatable for constructor at index 2 */
    luaL_newmetatable(L, "JS:Promise");  /* Metatable at index 3 */
    lua_pushcfunction(L, promise_tostring), lua_setfield(L, 3, "__tostring");
    lua_newtable(L);  /* __index */
    lua_pushcfunction(L, promise_then), lua_setfield(L, -2, "then");
    lua_pushvalue(L, -1), lua_setfield(L, 1, "prototype");
    lua_setfield(L, 3, "__index");
    lua_pushliteral(L, "JS:Promise"), lua_setfield(L, 3, "__metatable");
    /* Install static methods */
    lua_pushvalue(L, 3), lua_pushinteger(L, PROMISE_FULFILLED);
    lua_pushcclosure(L, promise_resolved, 2), lua_setfield(L, 1, "resolve");
    lua_pushvalue(L, 3), lua_pushinteger(L, PROMISE_REJECTED);
    lua_pushcclosure(L, promise_resolved, 2), lua_setfield(L, 1, "reject");
    lua_pushvalue(L, 3), lua_pushcclosure(L, promise_with_resolvers, 1);
    lua_setfield(L, 1, "withResolvers");
    lua_pushcfunction(L, promise_race), lua_setfield(L, 1, "race");
    /* Metatable is at -1. Set constructor as __call */
    lua_pushcclosure(L, promise_new, 1), lua_setfield(L, -2, "__call");
    lua_setmetatable(L, 1);  /* Pops the metatable for constructor */
    return 1;
}