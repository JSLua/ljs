/*
    Copyright (C) 2025 `zyxwvu` Shi <i@shiyc.cn>

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <https://www.gnu.org/licenses/>.
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <getopt.h>
#include <signal.h>

#include <lua.h>
#include <lualib.h>
#include <lauxlib.h>

#include "jsrt/jsrt.h"

#ifdef HAVE_READLINE
# include "readline/readline.h"
# include "readline/history.h"
# include <unistd.h>
#endif

#ifdef LJS_ENABLE_LIBEV
# include <ev.h>

/* Workaround for termios behaviour of macOS rl_callback */
# if defined(_READLINE_H_) && defined(__APPLE__)
#  define rl_set_prompt(p) \
    rl_callback_handler_install(p, interactive_process_line)
# endif

int luaopen_jsrt_timer(lua_State *L);
#endif

static int write_chunk(lua_State *L, const void *p, size_t sz, void *ud)
{
    fwrite(p, sz, 1, ud);
    return 0;
}

static int process_env_get(lua_State *L)
{
    const char *name = luaL_checkstring(L, 2);
    const char *env = getenv(name);
    if (env) lua_pushstring(L, env);
    else lua_pushnil(L);
    return 1;
}

static void open_process(lua_State *L, int argc, char *argv[]) {
    lua_newtable(L);
    lua_createtable(L, argc, 0);
    lua_pushstring(L, argv[0]), lua_rawseti(L, -2, 0);
    for (int i = optind; i < argc; i++)
        lua_pushstring(L, argv[i]), lua_rawseti(L, -2, i + 1 - optind);
    lua_setfield(L, -2, "argv");
    lua_newuserdata(L, 1), lua_newtable(L);
    lua_pushcfunction(L, process_env_get), lua_setfield(L, -2, "__index");
    lua_setmetatable(L, -2), lua_setfield(L, -2, "env");
}

static int traceback(lua_State *L)
{
    if (!lua_isstring(L, 1)) { /* Non-string error object? Try metamethod. */
        if (lua_isnoneornil(L, 1) ||
            !luaL_callmeta(L, 1, "__tostring") ||
            !lua_isstring(L, -1))
            return 1;  /* Return non-string error object. */
        lua_remove(L, 1);  /* Replace object by result of __tostring metamethod. */
    }
    luaL_traceback(L, L, lua_tostring(L, 1), 2);
    return 1;
}

static void generate_interrupt(lua_State *L, lua_Debug *ar)
{
    (void)ar;  /* unused arg. */
    lua_sethook(L, NULL, 0, 0);
    /* Avoid luaL_error -- a C hook doesn't add an extra frame. */
    luaL_where(L, 0);
    lua_pushfstring(L, "%sinterrupted!", lua_tostring(L, -1));
    lua_error(L);
}

static lua_State *L = NULL;

static void handle_sigint(int i)
{
    signal(i, SIG_DFL); /* allow second SIGINT to do default action */
    lua_sethook(L, generate_interrupt,
                LUA_MASKCALL | LUA_MASKRET | LUA_MASKCOUNT, 1);
}

#ifdef HAVE_READLINE
static int interactive_exit(lua_State *l)
{
    lua_close(l), exit(EXIT_SUCCESS);
    return 0;  /* Never actually return */
}

#ifdef LJS_ENABLE_LIBEV
static struct ev_io stdin_poll;
static void interactive_stdin_cb(struct ev_loop *l, struct ev_io *h, int e)
{
    rl_callback_read_char();
}
#endif

/* Keep previous lines */
static char *input_buffer = NULL;

static void interactive_process_line(char *line)
{
#ifdef LJS_ENABLE_LIBEV
    if (line == NULL) {
        ev_io_stop(EV_DEFAULT_UC_ &stdin_poll);
        rl_callback_handler_remove();
        return;
    }
#endif

    size_t l_siz = strlen(line) + 1;
    if (!input_buffer) input_buffer = line;
    else {
        size_t buffered = strlen(input_buffer);
        if (!(input_buffer = realloc(input_buffer, buffered + 1 + l_siz)))
            return (void)luaL_error(L, "insufficient memory");
        input_buffer[buffered] = '\n';
        memcpy(input_buffer + buffered + 1, line, l_siz);
        free(line);
    }

    /* Try loading user input */
    if (luaJS_loadstring(L, input_buffer, strlen(input_buffer),
                         "@stdin") != LUA_OK) {
        const char *err = lua_tostring(L, -1);
        if (strstr(err, "syntax error, unexpected end of file")) {
            rl_set_prompt("... ");
            lua_pop(L, 1);
            return;  /* More lines to be read */
        }
        printf("%s\n", err);
        goto finish;
    }

    lua_pushvalue(L, LUA_GLOBALSINDEX);
    signal(SIGINT, handle_sigint);
    if (lua_pcall(L, 1, 1, 1) != LUA_OK) {
        printf("%s\n", lua_tostring(L, -1));
        goto finish;
    }
    signal(SIGINT, SIG_IGN);

    /* Show result if anything is yielded */
    if (lua_isnil(L, 2)) goto finish;
    if (luaL_callmeta(L, 2, "__tostring")) lua_replace(L, 2);
    if (lua_isstring(L, 2) || lua_isnumber(L, 2))
        printf("%s\n", lua_tostring(L, 2));
    else if (lua_isboolean(L, 2))
        printf("%s\n", lua_toboolean(L, 2) ? "true" : "false");
    else printf("[%s: %p]\n", luaL_typename(L, 2),
                lua_topointer(L, 2));

finish:
    lua_gc(L, LUA_GCCOLLECT, 0);
    add_history(input_buffer);
    free(input_buffer);
    input_buffer = NULL;
    lua_settop(L, 1);
    rl_set_prompt("> ");
}

static void interactive()
{
    lua_pushcfunction(L, interactive_exit), lua_setglobal(L, "exit");
    lua_settop(L, 0), lua_pushcfunction(L, traceback);
#ifdef LJS_ENABLE_LIBEV
    rl_callback_handler_install("> ", interactive_process_line);
    ev_io_init(&stdin_poll, interactive_stdin_cb, 0, EV_READ);
    ev_io_start(EV_DEFAULT_UC_ &stdin_poll);
    ev_run(EV_DEFAULT_UC_ 0);
#else
    while (1) {
        char *line = readline(input_buffer ? "... " : "> ");
        if (!line) break;  /* EOF */
        else if (!*line) continue;
        interactive_process_line(line);
        lua_pushcfunction(L, luaJS_flush_task);
        lua_call(L, 0, 0);
    }
#endif
}

static char *interactive_comp_global(const char *str, int a)
{
    const char *prefix = (prefix = strrchr(str, '.')) ? prefix + 1 : str;
    while (lua_istable(L, 2) && lua_next(L, 2)) {
        lua_pop(L, 1);  /* Value is not required */
        if (lua_type(L, -1) == LUA_TSTRING) {
            const char *item = lua_tostring(L, -1);
            if (strstr(item, prefix) == item) {
                char *comp = malloc(prefix - str + lua_objlen(L, -1) + 1);
                memcpy(comp, str, prefix - str);
                strcpy(comp + (prefix - str), item);
                return comp;
            }
        }
    }
    /* Also look for its metatable */
    if (lua_getmetatable(L, 2)) {
        lua_getfield(L, -1, "__index"), lua_replace(L, 2);
        if (lua_istable(L, 2)) {
            lua_settop(L, 2), lua_pushnil(L);
            return interactive_comp_global(str, a);  /* continue */
        }
    }
    /* Done */
    lua_settop(L, 1);
    return NULL;
}

static char **interactive_comp(const char *input, int a, int b)
{
    if (*input == '"' || *input == '\'' || *input == '`')
        goto give_up;
    lua_pushvalue(L, LUA_GLOBALSINDEX);
    for (const char *dot, *str = input;
            (dot = strchr(str, '.')); str = dot + 1) {
        lua_pushlstring(L, str, dot - str), lua_gettable(L, 2);
        if (lua_type(L, -1) <= LUA_TNUMBER) goto give_up;
        lua_replace(L, 2);
    }
#ifdef __linux__
    rl_completion_suppress_append = 1;
#endif
    lua_pushnil(L);
    return rl_completion_matches(input, interactive_comp_global);
give_up: rl_attempted_completion_over = 1;
    lua_settop(L, 1);
    return NULL;
}
#endif

void print_usage(char *prog_name) {
    printf("LJS runtime CLI, version %s\n", LJS_VERSION);
    printf("Usage:  %s [-c <out file>|-l] [-s] <filename>\n%s", prog_name,
           "  -c <out file> Dump bytecode into specified file\n"
           "  -l            List compiled bytecode\n"
           "  -s            Do not generate debug information\n"
           "                (local name, upvalue name, line info and etc.)\n"
           "  -v            Print LJS runtime version and exit\n"
           "  -h            Show this help and exit\n"
           "If no filename is specified, an interactive repl will be available.\n");
}

int main(int argc, char *argv[]) {
    if ((L = luaL_newstate()))
        luaL_openlibs(L);
    else {
        fprintf(stderr, "Error: Failed to create Lua VM!\n");
        return EXIT_FAILURE;
    }

    lua_pushnil(L), lua_setfield(L, LUA_GLOBALSINDEX, "loadstring");
    lua_pushnil(L), lua_setfield(L, LUA_GLOBALSINDEX, "dofile");
    /* package.preload.jsrt = luaopen_jsrt */
    lua_getglobal(L, "package");
    lua_getfield(L, -1, "preload");
    lua_pushcfunction(L, luaopen_jsrt);
    lua_setfield(L, -2, "jsrt");
    lua_pop(L, 2);

    char out_file[256] = {};
    int opt, list = 0, strip = 0;
    while ((opt = getopt(argc, argv, "vc:lsh")) != -1) {
        switch (opt) {
            case 'v': printf("%s\n", LJS_VERSION); return 0;
            case 'c': strcpy(out_file, optarg); break;
            case 'l': list = 1; break;
            case 's': strip = 1; break;
            default: case 'h': print_usage(argv[0]);
                return opt == 'h' ? 0 : EXIT_FAILURE;
        }
    }

    char infile[PATH_MAX] = "-";
    if (optind < argc)
        strcpy(infile, argv[optind]);

    /* jsrt = require "jsrt"; jsrt.extend_builtins() */
    lua_getglobal(L, "require");
    lua_pushstring(L, "jsrt");
    lua_call(L, 1, 1);
    lua_getfield(L, -1, "extend_builtins");
    lua_call(L, 0, 0);
    lua_setglobal(L, "jsrt");

    int exit_code = 0;
#ifdef LJS_ENABLE_LIBEV
    if (!ev_default_loop(0))
        return EXIT_FAILURE;
    ev_set_userdata(EV_DEFAULT_UC_ L);
    luaopen_jsrt_timer(L);
#endif

#ifdef HAVE_READLINE
    if (!strcmp(infile, "-") && isatty(0)) {
        rl_initialize();
        rl_basic_word_break_characters = "\t\n */%+-!><=&^|,;{([";
        rl_completion_append_character = 0;
        rl_attempted_completion_function = interactive_comp;
        interactive();
        goto over;
    }
#endif

    if (luaJS_loadfile(L, infile, strip) != LUA_OK) {
        fprintf(stderr, "Error compiling user code:\n%s\n",
                lua_tostring(L, -1));
        exit_code = EXIT_FAILURE;
        goto over;
    }

    if (*out_file) {
        FILE *fp = fopen(out_file, "wb");
        if (!fp) {
            perror("Error opening bytecode file");
            exit_code = EXIT_FAILURE;
            goto over;
        }
        lua_dump(L, write_chunk, fp);
        fclose(fp);
        goto over;
    }

    if (list) {
        /* require("jit.bc").dump(fun, nil, true) */
        lua_getglobal(L, "require");
        lua_pushstring(L, "jit.bc");
        lua_call(L, 1, 1);
        lua_getfield(L, -1, "dump");
        lua_pushvalue(L, -3);
        lua_pushnil(L);
        lua_pushboolean(L, 1);
        lua_call(L, 3, 0);
    } else {
        open_process(L, argc, argv), lua_setglobal(L, "process");
        lua_pushcfunction(L, traceback), lua_insert(L, 1);
        lua_pushvalue(L, LUA_GLOBALSINDEX);
        signal(SIGINT, handle_sigint);
        if (lua_pcall(L, 1, 0, 1) != LUA_OK) {
            fprintf(stderr, "%s\n", lua_tostring(L, -1));
            exit_code = EXIT_FAILURE;
            goto over;
        }

#ifdef LJS_ENABLE_LIBEV
        ev_run(EV_DEFAULT_UC_ 0);
#else
        luaJS_flush_task(L);
#endif
    }

over:
    lua_close(L);
    return exit_code;
}
