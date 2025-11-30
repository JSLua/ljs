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

#ifndef LYYJS_JSRT_H
#define LYYJS_JSRT_H

#include <lua.h>

int luaJS_loadfile(lua_State *L, const char *file, int strip);
int luaJS_loadstring(lua_State *L, const char *, size_t len,
                     const char *chunk_name);
int luaopen_jsrt(lua_State *L);

int luaJS_flush_task(lua_State *L);

#endif //LYYJS_JSRT_H
