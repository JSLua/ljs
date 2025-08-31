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
#include <stdlib.h>
#include <string.h>
#include <regex.h>

#ifndef REG_STARTEND
# define REG_STARTEND 0
# warning REG_STARTEND is not supported by your libc -- \
    strings including NUL tested with RegExp will be truncated
#endif

static inline const char *
check_string_or_buffer(lua_State *L, int n, size_t *len)
{
    if (lua_isuserdata(L, n)) {
        struct { char *w, *e, *b; } *buf = lua_touserdata(L, n);
        if (lua_objlen(L, n) < 3 * sizeof(char *) ||
            !(buf->w >= buf->b && buf->e >= buf->w)) {
            luaL_error(L, "string buffer expected in userdata");
            return NULL;
        }
        if (len) *len = buf->w - buf->b;
        return buf->b;
    }
    return luaL_checklstring(L, n, len);
}

static int regexp_exec(lua_State *L)
{
    regex_t *re = luaL_checkudata(L, 1, "JS:RegExp");
    size_t n_match = 1 + re->re_nsub, str_len;
    const char *str = check_string_or_buffer(L, 2, &str_len);
    regoff_t start_offset = luaL_optinteger(L, 3, 0);
    lua_settop(L, 2);

    regmatch_t *matches = lua_newuserdata(L, sizeof(regmatch_t) * n_match);
    matches[0].rm_so = start_offset;
    matches[0].rm_eo = (regoff_t)str_len;  /* Used by REG_STARTEND */

    int ret = regexec(re, str, n_match, matches, REG_STARTEND);
    if (ret == REG_NOMATCH)
        return 0;
    lua_createtable(L, 1, 0), lua_pushvalue(L, 2), lua_rawseti(L, 4, 1);
    lua_setfenv(L, 3);
    lua_pushvalue(L, lua_upvalueindex(1)), lua_setmetatable(L, 3);
    return 1;
}

static int regexp_test(lua_State *L)
{
    regex_t *re = luaL_checkudata(L, 1, "JS:RegExp");
    size_t str_len;
    const char *str = check_string_or_buffer(L, 2, &str_len);
    regmatch_t range = { 0, (int)str_len };
    lua_pushboolean(L, !regexec(re, str, 1, &range, REG_STARTEND));
    return 1;
}

static int regexp_gc(lua_State *L)
{
    regex_t *re = luaL_checkudata(L, 1, "JS:RegExp");
    regfree(re);
    return 0;
}

#define match_length(L, idx) lua_objlen(L, idx) / sizeof(regmatch_t)

static int match__index(lua_State *L)
{
    luaL_checktype(L, 1, LUA_TUSERDATA);
    regmatch_t *self = lua_touserdata(L, 1);
    int key_type = lua_type(L, 2);
    if (key_type == LUA_TNUMBER) {
        lua_Integer i = lua_tointeger(L, 2);
        if (i < match_length(L, 1) && index >= 0) {
            lua_getfenv(L, 1), lua_rawgeti(L, -1, 1);
            const char *str = lua_tostring(L, -1);
            lua_pushlstring(L, str + self[i].rm_so,
                            self[i].rm_eo - self[i].rm_so);
        } else lua_pushnil(L);
    } else if (key_type == LUA_TSTRING) {
        const char *key = lua_tostring(L, 2);
        if (!strcmp(key, "lastIndex"))
            lua_pushinteger(L, self[0].rm_eo);
        else if (!strcmp(key, "index"))
            lua_pushinteger(L, self[0].rm_so);
        else if (!strcmp(key, "input"))
            lua_getfenv(L, 1), lua_rawgeti(L, -1, 1);
        else lua_pushnil(L);
    } else lua_pushnil(L);
    return 1;
}

static int match__len(lua_State *L)
{
    luaL_checktype(L, 1, LUA_TUSERDATA);
    lua_pushinteger(L, match_length(L, 1));
    return 1;
}

static int match__tostring(lua_State *L)
{
    luaL_Buffer b;
    luaL_checktype(L, 1, LUA_TUSERDATA);
    regmatch_t *self = lua_touserdata(L, 1);
    size_t n_match = match_length(L, 1);
    lua_getfenv(L, 1), lua_rawgeti(L, -1, 1);
    const char *input = lua_tostring(L, -1);
    luaL_buffinit(L, &b);
    for (int i = 0; i < n_match; i++) {
        if (i > 0) luaL_addchar(&b, ',');
        luaL_addlstring(&b, input + self[i].rm_so,
                        self[i].rm_eo - self[i].rm_so);
    }
    luaL_pushresult(&b);
    return 1;
}

static luaL_Reg lib_match_mt[] = {
    { "__index", match__index },
    { "__len", match__len },
    { "__tostring", match__tostring },
    { NULL, NULL }
};

static int string_search(lua_State *L)
{
    size_t str_len, needle_len;
    const char *str = luaL_checklstring(L, 1, &str_len);
    if (lua_isstring(L, 2)) {
        const char *needle = luaL_checklstring(L, 2, &needle_len);
        char *offset = memmem(str, str_len, needle, needle_len);
        lua_pushinteger(L, offset ? offset - str : -1);
    } else {
        regex_t *re = luaL_checkudata(L, 2, "JS:RegExp");
        regmatch_t match = { 0, (int)str_len };
        int ret = regexec(re, str, 1, &match, REG_STARTEND);
        lua_pushinteger(L, ret == REG_NOMATCH ? -1 : match.rm_so);
    }
    return 1;
}

static int string_split(lua_State *L)
{
    size_t len;
    const char *str = luaL_checklstring(L, 1, &len);
    regex_t *re = luaL_checkudata(L, 2, "JS:RegExp");
    int index = 1, offset = 0;
    regmatch_t match = { 0, 0 };
    lua_newtable(L);
    while (offset < len - 1 &&
            regexec(re, str + offset, 1, &match, 0) == 0) {
        if (match.rm_eo == 0)
            match.rm_so = match.rm_eo = 1;
        lua_pushlstring(L, str + offset, match.rm_so);
        lua_rawseti(L, -2, index++);
        offset += (int)match.rm_eo;
    }
    lua_pushlstring(L, str + offset, len - offset);
    lua_rawseti(L, -2, index);
    return 1;
}

static luaL_Reg lib_string[] = {
    { "search", string_search },
    { "split", string_split },
    { NULL, NULL }
};

static int push_regexp(lua_State *L, const char *pattern, int flags)
{
    regex_t *re = lua_newuserdata(L, sizeof(*re));
    int ret = regcomp(re, pattern, flags);
    if (ret != 0) {
        char err[128] = {};
        regerror(ret, re, err, sizeof(err));
        return luaL_error(L, "failed to compile RegExp: %s", err);
    }
    luaL_setmetatable(L, "JS:RegExp");
    return 1;
}

static int lib_regexp(lua_State *L)
{
    int flags = REG_EXTENDED;
    /* Shortcut for cacheable compiler RegExp */
    if (lua_isstring(L, 1)) {
        lua_pushvalue(L, 1);
        lua_rawget(L, lua_upvalueindex(1));
        if (lua_isuserdata(L, -1))
            return 1;  /* Reuse */
        lua_pop(L, 1);

        const char *pack = lua_tostring(L, 1);
        if (*pack & 1) flags |= REG_ICASE;
        if (*pack & 2) flags |= REG_NEWLINE;
        push_regexp(L, pack + 1, flags);
        lua_pushvalue(L, 1), lua_pushvalue(L, -2);
        lua_rawset(L, lua_upvalueindex(1));  /* Cache new object */
        return 1;
    }

    const char *pattern = luaL_checkstring(L, 2);
    const char *flag_str = luaL_optstring(L, 3, "");
    for (; *flag_str; flag_str++) {
        switch (*flag_str) {
            case 'i': flags |= REG_ICASE; break;
            case 'm': flags |= REG_NEWLINE; break;
            case 'g': /* ignore */ break;
            default: luaL_error
                (L, "unsupported re flag '%c'", *flag_str);
        }
    }

    return push_regexp(L, pattern, flags);
}

int luaopen_js_regexp(lua_State *L)
{
    luaL_newmetatable(L, "JS:RegExp");
    lua_newtable(L);  /* __index */
    lua_newtable(L), luaL_setfuncs(L, lib_match_mt, 0);
    lua_pushcclosure(L, regexp_exec, 1), lua_setfield(L, -2, "exec");
    lua_pushcfunction(L, regexp_test), lua_setfield(L, -2, "test");
    lua_setfield(L, -2, "__index");
    lua_pushcfunction(L, regexp_gc), lua_setfield(L, -2, "__gc");

    lua_getglobal(L, "string");
    luaL_setfuncs(L, lib_string, 0);
    lua_settop(L, 0);
    lua_newtable(L);  /* Compiled RegExp cache */
    lua_newtable(L);  /* { __mode = "v" } */
    lua_pushstring(L, "v"), lua_setfield(L, -2, "__mode");
    lua_setmetatable(L, -2);
    lua_pushcclosure(L, lib_regexp, 1);
    return 1;
}