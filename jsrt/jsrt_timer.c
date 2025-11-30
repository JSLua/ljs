#include <lua.h>
#include <lauxlib.h>
#include <ev.h>
#include <stdlib.h>

typedef long long LVal;
#define top_ref(L) (*((LVal**)(L) + 5) - 1)
LVal timer_table;  /* number assign table */

int luaJS_loadstring(lua_State *, const char *, size_t, const char *);

static void compile_string_callback(lua_State *L)
{
    if (lua_isstring(L, 2)) {
        if (luaJS_loadstring(L, lua_tostring(L, 2), lua_objlen(L, 2),
                             "timer code") != LUA_OK)
            lua_error(L);
        lua_replace(L, 2);
    }
}

static void timer_cb(EV_P_ struct ev_timer *timer, int e)
{
    lua_State *L = ev_userdata(loop);
    lua_pushnil(L), *top_ref(L) = *(LVal *)&timer->data;
    lua_getfenv(L, -1), lua_rawgeti(L, -1, 1), lua_insert(L, 2);
    if (!timer->repeat) {  /* recycle setTimeout timer */
        lua_rawgeti(L, -1, 0);
        lua_Integer timer_id = lua_tointeger(L, -1);
        *top_ref(L) = timer_table;
        luaL_unref(L, -1, (int)timer_id);
    }

    lua_settop(L, 2);  /* Assuming traceback is at 1 */
    lua_pushvalue(L, LUA_GLOBALSINDEX);
    if (lua_pcall(L, 1, 0, 1) != LUA_OK) {
        fprintf(stderr, "%s\n", lua_tostring(L, -1));
        lua_settop(L, 1);
    }
}

static int launch_timer(lua_State *L, double after, double repeat)
{
    ev_timer *timer = lua_newuserdata(L, sizeof(*timer));
    ev_timer_init(timer, timer_cb, after, repeat);
    *(LVal *)&timer->data = *top_ref(L);
    lua_copy(L, -1, 1);
    lua_pushinteger(L, luaL_ref(L, lua_upvalueindex(1)));
    /* timer env table */
    lua_createtable(L, 1, 0);
    lua_pushvalue(L, 2), lua_rawseti(L, -2, 1);  /* Store cb */
    if (!repeat) lua_pushvalue(L, -2), lua_rawseti(L, -2, 0);
    lua_setfenv(L, 1);
    ev_timer_start(EV_DEFAULT_UC_ timer);
    return 1;
}

static int jsrt_set_interval(lua_State *L)
{
    compile_string_callback(L);
    double interval = luaL_checknumber(L, 3) / 1000;
    return launch_timer(L, interval, interval);
}

static int jsrt_set_timeout(lua_State *L)
{
    compile_string_callback(L);
    return launch_timer(L, luaL_optnumber(L, 3, 0) / 1000, 0);
}

static int jsrt_clear_timer(lua_State *L)
{
    int timer_id = (int)lua_tointeger(L, 2);
    lua_rawgeti(L, lua_upvalueindex(1), timer_id);
    if (lua_isuserdata(L, -1)) {
        ev_timer *timer = lua_touserdata(L, -1);
        ev_timer_stop(EV_DEFAULT_UC_ timer);
        luaL_unref(L, lua_upvalueindex(1), timer_id);
    }
    return 0;
}

static void io_cb(EV_P_ struct ev_io *io, int e)
{
    lua_State *L = ev_userdata(loop);
    lua_settop(L, 2), *top_ref(L) = timer_table;
    lua_rawgeti(L, -1, *(int *)&io->data);
    luaL_unref(L, -2, *(int *)&io->data);
    ev_io_stop(EV_DEFAULT_UC_ io), free(io);
    luaL_callmeta(L, -1, "_resolve"), lua_settop(L, 1);
}

static int jsrt_poll_fd(lua_State *L)
{
    int fd = luaL_checkint(L, 2);
    const char *event = luaL_optstring(L, 3, "r");
    ev_io *io = malloc(sizeof(*io));
    switch (*event) {
        case 'r': ev_io_init(io, io_cb, fd, EV_READ); break;
        case 'w': ev_io_init(io, io_cb, fd, EV_WRITE); break;
        default: return luaL_error(L, "unknown event type");
    }
    *(short *)lua_newuserdata(L, sizeof(short)) = 0;
    lua_createtable(L, 3, 0), lua_setfenv(L, -2);
    luaL_setmetatable(L, "JS:Promise"), lua_copy(L, -1, 1);
    *(int *)&io->data = luaL_ref(L, lua_upvalueindex(1));
    ev_io_start(EV_DEFAULT_UC_ io);
    return lua_settop(L, 1), 1;
}

static luaL_Reg lib[] = {
    { "setInterval", jsrt_set_interval },
    { "setTimeout", jsrt_set_timeout },
    { "clearInterval", jsrt_clear_timer },
    { "clearTimeout", jsrt_clear_timer },
    { "_pollFd", jsrt_poll_fd },
    { NULL, NULL }
};

int luaopen_jsrt_timer(lua_State *L)
{
    lua_pushvalue(L, LUA_GLOBALSINDEX);
    lua_newtable(L), timer_table = *top_ref(L);
    luaL_setfuncs(L, lib, 1);
    lua_pop(L, 1);
    return 0;
}
