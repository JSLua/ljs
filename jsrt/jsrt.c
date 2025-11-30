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

#include "jsrt.h"

#include <yyjs/yyjs_parse.h>
#include <lauxlib.h>
#include <stdlib.h>

static int luajit_fr2 = 1, base0_compat = 0;

int luaJS_loadfile(lua_State *L, const char *file, int strip)
{
    int ret = -1;
    struct yy_dump_state d = {};
    struct yy_parse_state *state = yyjs_parser_new();
    if (!state)
        return luaL_error(L, "unable to alloc parser");

    if (luajit_fr2) state->flags |= YY_PARSER_CALL_RSV;
    if (!base0_compat) state->flags |= YY_PARSER_ARR_BASE1;
    if (strip) state->flags |= YY_PARSER_STRIP;
    if (yyjs_parse_file(state, file) != 0) {
        lua_pushfstring(L, "at %s:%d: %s", file,
                        state->err_line, state->err_msg);
        goto cleanup;
    }

    size_t out_len = yyjs_dump_bc(state, &d);
    if (luaL_loadbuffer(L, d.buffer, out_len, NULL) != 0)
        goto cleanup;

    ret = 0;
cleanup:
    if (d.buffer != NULL)
        yy_dump_merge(NULL, &d);
    yyjs_parser_destroy(state);
    return ret;
}

int luaJS_loadstring(lua_State *L, const char *code, size_t len,
                     const char *chunk_name)
{
    int ret = -1;
    struct yy_dump_state d = {};
    struct yy_parse_state *state = yyjs_parser_new();
    if (!state)
        return luaL_error(L, "Failed to alloc parser");

    state->flags |= YY_PARSER_STRIP;
    if (luajit_fr2) state->flags |= YY_PARSER_CALL_RSV;
    if (!base0_compat) state->flags |= YY_PARSER_ARR_BASE1;
    if (yyjs_parse(state, (void *)code, (int)len) != 0) {
        lua_pushfstring(L, "at line %d: %s",
                        state->err_line, state->err_msg);
        goto cleanup;
    }

    size_t out_len = yyjs_dump_bc(state, &d);
    if (luaL_loadbuffer(L, d.buffer, out_len, chunk_name) != 0)
        goto cleanup;

    ret = 0;
cleanup:
    if (d.buffer != NULL)
        yy_dump_merge(NULL, &d);
    yyjs_parser_destroy(state);
    return ret;
}

static int lib_loadfile(lua_State *L)
{
    int shift = lua_isnil(L, 1) ? 1 : 0;
    const char *filename = luaL_checkstring(L, shift + 1);
    if (luaJS_loadfile(L, filename, 0) != 0)
        return lua_error(L);
    return 1;
}

static int lib_loadstring(lua_State *L)
{
    int shift = lua_isnil(L, 1) ? 1 : 0;
    size_t len;
    const char *code = luaL_checklstring(L, shift + 1, &len);
    const char *name = luaL_optstring(L, shift + 2, "user script");
    if (luaJS_loadstring(L, code, len, name) != 0)
        return lua_error(L);
    return 1;
}

/* Runtime for Generator */
static int gen_resume(lua_State *L, lua_State *th, int arg)
{
    int ret = lua_resume(th, arg);
    if (ret == LUA_YIELD || ret == LUA_OK) {
        lua_createtable(L, 0, 2);
        lua_pushboolean(L, ret == LUA_OK);
        lua_setfield(L, -2, "done");
        if (lua_gettop(th) > 0)
            lua_settop(th, 1), lua_xmove(th, L, 1);
        else lua_pushnil(L);
        lua_setfield(L, -2, "value");
        return 1;
    } else {
        lua_xmove(th, L, 1);
        return lua_error(L);
    }
}

static int gen_next(lua_State *L)
{
    luaL_checktype(L, 1, LUA_TTHREAD);
    lua_State *th = lua_tothread(L, 1);
    if (th == L) return luaL_error(L, "Generator is already running");
    else if (lua_status(th) == LUA_YIELD) {
        int arg = !lua_isnoneornil(L, 2);
        if (arg && lua_isboolean(L, 2))
            return luaL_error(L, "cannot pass boolean via next()");
        if (arg)
            lua_xmove(L, th, 1);
        return gen_resume(L, th, arg);
    } else if (lua_status(th) == LUA_OK) {
        /* Not yet started? */
        if (lua_gettop(th) > 0)
            return gen_resume(L, th, lua_gettop(th) - 1);
    }
    lua_createtable(L, 0, 1);
    lua_pushboolean(L, 1), lua_setfield(L, -2, "done");
    return 1;
}

static int gen_throw(lua_State *L)
{
    luaL_checktype(L, 1, LUA_TTHREAD), luaL_checkany(L, 2);
    lua_State *th = lua_tothread(L, 1);
    if (th == L) return luaL_error(L, "Generator is already running");
    if (lua_status(th) != LUA_YIELD) return lua_pushvalue(L, 2), lua_error(L);
    lua_pushboolean(th, 0);  /* false indicates error */
    lua_pushvalue(L, 2), lua_xmove(L, th, 1);
    return gen_resume(L, th, 2);
}

static int gen_launch(lua_State *L)
{
    int n_arg = lua_gettop(L);
    if (n_arg == 0)
        return luaL_error(L, "`this` argument missing");

    lua_State *th = lua_newthread(L);
    lua_pushvalue(L, lua_upvalueindex(1));
    for (int i = 1; i <= n_arg; i++) lua_pushvalue(L, i);
    lua_xmove(L, th, 1 + n_arg);
    return 1;
}

static int lib_generator(lua_State *L)
{
    luaL_checktype(L, 1, LUA_TFUNCTION);
    lua_pushvalue(L, 1), lua_pushcclosure(L, gen_launch, 1);
    return 1;
}

extern int promise_launch_async(lua_State *L);

static int lib_async(lua_State *L)
{
    luaL_checktype(L, 1, LUA_TFUNCTION);
    lua_settop(L, 1);  /* Async function body */
    lua_pushcclosure(L, promise_launch_async, 1);
    return 1;
}

static int lib_await(lua_State *L)
{
    lua_settop(L, 1);
    if (luaL_testudata(L, 1, "JS:Promise"))
        return lua_yield(L, 1);  /* Pass to awaiter */
    else if (lua_isboolean(L, 1))
        return luaL_error(L, "cannot await for boolean values");
    return 1;
}

static int lib_isinstance(lua_State *L)
{
    luaL_checktype(L, 2, LUA_TTABLE);
    lua_getfield(L, 2, "prototype"), lua_replace(L, 2);
    while (lua_getmetatable(L, 1)) {
        lua_getfield(L, -1, "__index");
        lua_replace(L, 1), lua_settop(L, 2);
        if (lua_rawequal(L, 1, 2)) {
            lua_pushboolean(L, 1);
            return 1;
        }
    }
    lua_pushboolean(L, 0);
    return 1;
}

static luaL_Reg lib[] = {
        { "loadFile", lib_loadfile },
        { "loadString", lib_loadstring },
        { "flushTask", luaJS_flush_task },
        { "async", lib_async },
        { "await", lib_await },
        { "generator", lib_generator },
        { "isinstance", lib_isinstance },
        { NULL, NULL }
};

static int iter_helper(lua_State *L)
{
    if (lua_isthread(L, 1)) {
        lua_State *th = lua_tothread(L, 1);
        if (th == L) return luaL_error(L, "Generator is already running");

        int status = lua_status(th), narg = lua_gettop(th) - 1;
        if (status > LUA_YIELD || status == LUA_OK && narg < 0) return 0;
        status = lua_resume(th, status == LUA_YIELD ? 0 : narg);
        switch (status) {
            case LUA_YIELD:
                lua_settop(L, 1);
                lua_settop(th, 1), lua_xmove(th, L, 1);
                return 2;
            case LUA_OK: return 0;
            default:  /* error thrown */
                return lua_xmove(th, L, 1), lua_error(L);
        }
    }

    lua_settop(L, 1);
    lua_call(L, 0, 1);
    lua_pushvalue(L, 1);
    return 2;
}

static int lib_iterate(lua_State *L)
{
    if (lua_istable(L, 1)) {
        lua_rawgeti(L, 1, 0);
        lua_pushvalue(L, lua_upvalueindex(1));
        lua_pushvalue(L, 1);
        lua_pushinteger(L, lua_isnil(L, -3) - 1);
    } else {
        lua_pushvalue(L, lua_upvalueindex(2));
        lua_pushvalue(L, 1);
        lua_pushboolean(L, 1);
    }
    return 3;
}

static int lib_spread(lua_State *L)
{
    int n_el = 0;
    if (lua_isthread(L, 1)) {
        lua_State *th = lua_tothread(L, 1);
        if (th == L) return luaL_error(L, "Generator is already running");
        int status = lua_status(th);
        if (status == LUA_OK) {
            if (lua_gettop(th) == 0) return 0;
            status = lua_resume(th, lua_gettop(th) - 1);
        }
        while (status == LUA_YIELD) {
            lua_settop(th, 1), lua_xmove(th, L, 1), n_el++;
            status = lua_resume(th, 0);
        }
        if (status != LUA_OK)
            return lua_xmove(th, L, 1), lua_error(L);
    } else {
        luaL_checktype(L, 1, LUA_TTABLE);
        lua_settop(L, 1);
        lua_rawgeti(L, 1, 0);
        luaL_checkstack(L, (int)lua_objlen(L, 1), "too many elements");
        for (n_el = 1; ; n_el++) {
            lua_rawgeti(L, 1, n_el);
            if (lua_isnil(L, -1)) { lua_pop(L, 1); break; }
        }
        if (lua_isnil(L, 2)) n_el--;
    }
    return n_el;
}

static const char *cc_escape[32] = {
    "u0000", "u0001", "u0002", "u0003", "u0004", "u0005", "u0006", "u0007",
    "b", "t", "n", "u000b", "f", "r", "u000e", "u000f",
    "u0010", "u0011", "u0012", "u0013", "u0014", "u0015", "u0016", "u0017",
    "u0018", "u0019", "u001a", "u001b", "u001c", "u001d", "u001e", "u001f"
};

/* Partial implementation. Table encode implemented in jsrt_aux.lua */
static int json_stringify(lua_State *L)
{
    int val = lua_gettop(L) == 1 ? 1 : 2;
    switch (lua_type(L, val)) {
        case LUA_TSTRING: {
            luaL_Buffer b;
            size_t len;
            const char *s = luaL_checklstring(L, val, &len);
            luaL_buffinit(L, &b), luaL_addchar(&b, '"');
            for (unsigned i = 0; i < len; i++) {
                if (s[i] == '"' || s[i] == '\\' || (unsigned)s[i] < ' ')
                    luaL_addchar(&b, '\\');
                if ((unsigned)s[i] >> 5) luaL_addchar(&b, s[i]);
                else luaL_addstring(&b, cc_escape[s[i]]);
            }
            luaL_addchar(&b, '"'), luaL_pushresult(&b);
            break;
        }

        case LUA_TNUMBER: {
            union { double v; uint64_t u; } n = { lua_tonumber(L, val) };
            if (~(n.u >> 52) & 0x7ffu)  /* Not Infinity or NaN ? */
                lua_settop(L, val), lua_tostring(L, -1);
            else lua_pushvalue(L, lua_upvalueindex(1));  /* null */
            break;
        }

        case LUA_TBOOLEAN:
            lua_pushvalue(L, lua_upvalueindex(2 + lua_toboolean(L, val)));
            break;

        case LUA_TNIL: case LUA_TFUNCTION:
            lua_pushvalue(L, lua_upvalueindex(1));  /* null */
            break;

        case LUA_TTABLE: lua_settop(L, val); break;
        default: lua_pushliteral(L, "{}");
    }
    return 1;
}

extern int luaJS_json_parse(lua_State *L);
extern int luaopen_js_regexp(lua_State *L);
extern int luaopen_js_promise(lua_State *L);

static int string_fromCodePoint(lua_State *L) {
    int n_top = lua_gettop(L);
    luaL_Buffer b;
    luaL_buffinit(L, &b);
    for (int i = 2; i <= n_top; i++) {
        lua_Integer codepoint = lua_tointeger(L, i) & 0x1fffff;
        u8_encode_mbs(luaL_addchar, &b, codepoint);
    }
    luaL_pushresult(&b);
    return 1;
}

static int string_fromCharCode(lua_State *L) {
    int n_top = lua_gettop(L);
    luaL_Buffer b;
    luaL_buffinit(L, &b);
    for (int i = 2; i <= n_top; i++) {
        lua_Integer char_code = lua_tointeger(L, i) & 0xffff;
        if (char_code >> 8) {  /* Only encode >=256 codes */
            if (char_code >> 11) {
                luaL_addchar(&b, 0xe0 | char_code >> 12);
                luaL_addchar(&b, 0x80 | char_code >> 6 & 63);
            } else luaL_addchar(&b, 0xc0 | char_code >> 6);
            luaL_addchar(&b, 0x80 | char_code & 63);
        } else luaL_addchar(&b, char_code);
    }
    luaL_pushresult(&b);
    return 1;
}

/* Generated with `luajit -b` for luaJIT_BC_jsrt_aux */
#include "jsrt_aux.h"

static int
probe_bytecode_fmt(lua_State *L, const void *p, size_t sz, void *ud)
{
    const char *buf = p;
    if (sz >= 5 && buf[1] == 'L' && buf[2] == 'J')
        luajit_fr2 = (buf[4] & 8) ? 1 : 0;
    return -1;
}

int luaopen_jsrt(lua_State *L)
{
    luaL_loadstring(L, "");
    lua_dump(L, probe_bytecode_fmt, NULL);
    lua_createtable(L, 1, 0);
    lua_pushboolean(L, 0), lua_rawseti(L, -2, 0);
    base0_compat = (int)lua_objlen(L, -1);
    lua_settop(L, 0);

    lua_newthread(L);
    lua_createtable(L, 0, 2), lua_createtable(L, 0, 2);
    lua_pushcfunction(L, gen_next), lua_setfield(L, -2, "next");
    lua_pushcfunction(L, gen_throw), lua_setfield(L, -2, "throw");
    lua_setfield(L, -2, "__index");
    lua_setmetatable(L, -2);  /* This applies to all threads */
    lua_pop(L, 1);

    luaopen_js_regexp(L), lua_setglobal(L, "RegExp");
    luaopen_js_promise(L), lua_setglobal(L, "Promise");

    lua_createtable(L, 0, 2);   /* JSON */
    lua_pushliteral(L, "null"); /* Pre-intern literals for stringify */
    lua_pushliteral(L, "false"), lua_pushliteral(L, "true");
    lua_pushcclosure(L, json_stringify, 3), lua_setfield(L, -2, "stringify");
    lua_pushcfunction(L, luaJS_json_parse), lua_setfield(L, -2, "parse");
    lua_setglobal(L, "JSON");

    lua_createtable(L, 0, 3);   /* String */
    lua_pushlstring(L, "", 0);
    lua_getmetatable(L, -1), lua_getfield(L, -1, "__index");
    lua_setfield(L, 1, "prototype"), lua_settop(L, 1);
    lua_pushcfunction(L, string_fromCodePoint);
    lua_setfield(L, -2, "fromCodePoint");
    lua_pushcfunction(L, string_fromCharCode);
    lua_setfield(L, -2, "fromCharCode");
    lua_setglobal(L, "String");

    lua_newtable(L);
    luaL_setfuncs(L, lib, 0);
    /* jsrt.iterate */
    lua_getglobal(L, "ipairs"), lua_newtable(L), lua_call(L, 1, 1);
    lua_pushcfunction(L, iter_helper);
    lua_pushcclosure(L, lib_iterate, 2), lua_setfield(L, -2, "iterate");
    lua_pushcfunction(L, lib_spread), lua_setfield(L, -2, "spread");

    /* JavaScript library search path */
    const char *js_path = getenv("LJS_PATH");
    if (!js_path)
        js_path = "./?.js;/usr/local/share/ljs/?.js;/usr/share/ljs/?.js";
    lua_pushstring(L, js_path), lua_setfield(L, -2, "path");

    /* Finally, load aux library implemented in Lua */
    if (luaL_loadbuffer(L, (void*)luaJIT_BC_jsrt_aux, luaJIT_BC_jsrt_aux_SIZE,
                        "jsrt.aux") != LUA_OK)
        return lua_error(L);
    lua_pushvalue(L, -2), lua_call(L, 1, 0);
    lua_gc(L, LUA_GCCOLLECT, 0);
    return 1;
}
