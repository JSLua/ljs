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

%option reentrant noyywrap
%option prefix="json_"
%option extra-type="lua_State *"

%{
#include <lua.h>
#include <lauxlib.h>
size_t yy_unescape_str(char *, const char *, size_t, int);
#define VALUE 1
%}

%%

\"([^"\\\n]|\\["\\/bfnrt]|\\u[0-9A-Za-z]{4})*\" {
    char *buf = malloc(yyleng - 2);
    size_t len = yy_unescape_str(buf, yytext + 1, yyleng - 2, 0);
    lua_pushlstring(yyextra, buf, len);
    free(buf);
    return VALUE;
}

-?([1-9][0-9]*|0)(\.[0-9]+)?([eE][-+]?[0-9]+)? {
    lua_pushnumber(yyextra, strtod(yytext, NULL));
    return VALUE;
}

[ \t\r\n] /* whitespaces */
null    { lua_pushnil(yyextra); return VALUE; }
true    { lua_pushboolean(yyextra, 1); return VALUE; }
false   { lua_pushboolean(yyextra, 0); return VALUE; }
.       return *yytext;

%%

int luaJS_json_parse(lua_State *L)
{
    yyscan_t scan;
    int tok, state[20] = {}, top = -1;
    size_t len;
    const char *text = luaL_checklstring(L, 2, &len);
    if (json_lex_init_extra(L, &scan) != 0)
        return luaL_error(L, "error creating JSON lexer");
    json__scan_bytes(text, len, scan);
    lua_settop(L, 0);
    while ((tok = json_lex(scan))) {
        again: if (top == -1 && tok == VALUE) break;
        else if (state[top] == 1 && tok == VALUE)  /* arary element */
            lua_rawseti(L, -2, lua_objlen(L, -2) + 1), state[top] = 2;
        else if (state[top] == 2 && tok == ',') state[top] = 1;
        else if (state[top] == 3 && tok == VALUE &&  /* object key */
                 lua_isstring(L, -1)) state[top] = 4;
        else if (state[top] == 4 && tok == ':') state[top] = 5;
        else if (state[top] == 5 && tok == VALUE)  /* object value */
            lua_rawset(L, -3), state[top] = 6;
        else if (state[top] == 6 && tok == ',') state[top] = 3;
        else if ((tok == '[' || tok == '{') &&
                 (state[top] == 1 || state[top] == 5 || top == -1))
            lua_newtable(L), state[++top] = tok == '[' ? 1 : 3;
        else if (tok == ']' && (state[top] == 1 || state[top] == 2) ||
                 tok == '}' && (state[top] == 3 || state[top] == 6))
            { top--, tok = VALUE; goto again; }
        else {  /* Syntax error */
            json_lex_destroy(scan);
            return luaL_error(L, "%s in JSON", tok == '"' ?
                "Illegal string" : "Unexpected character");
        }
    }
    tok = json_lex(scan);  /* Expect next token to be EOF */
    json_lex_destroy(scan);
    return !tok ? 1 : luaL_error(L, "Unexpected character after JSON");
}