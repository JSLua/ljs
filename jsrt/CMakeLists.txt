cmake_minimum_required(VERSION 3.16)

find_path(LUA_INCLUDE_DIR lua.h PATH_SUFFIXES luajit-2.1)

add_library(jsrt SHARED jsrt.c jsrt.h jsrt_aux.h
        jsrt_regexp.c jsrt_promise.c json_parse.c)
target_include_directories(jsrt
        PRIVATE ${CMAKE_CURRENT_BINARY_DIR}
        PUBLIC ${CMAKE_SOURCE_DIR} ${LUA_INCLUDE_DIR})
target_link_libraries(jsrt PRIVATE yyjs)
set_target_properties(jsrt PROPERTIES PREFIX "")

# string.search requires `memmem` only visible with _GNU_SOURCE
target_compile_definitions(jsrt PRIVATE _GNU_SOURCE)

if (LJS_ENABLE_LIBEV)
    target_compile_definitions(jsrt PUBLIC LJS_ENABLE_LIBEV)
    find_library(LIBEV_LIBRARY NAMES ev)
    target_link_libraries(jsrt PUBLIC ${LIBEV_LIBRARY})
endif()

# luajit searches for .so on macOS instead of .dylib
if(CMAKE_SYSTEM_NAME MATCHES "Darwin")
    set_target_properties(jsrt PROPERTIES SUFFIX ".so")
    target_link_options(jsrt PRIVATE -undefined dynamic_lookup)
endif()

# Pre-compile jsrt-extra to bytecode to embed
add_custom_command(OUTPUT jsrt_aux.h
        COMMAND luajit -b ${CMAKE_CURRENT_SOURCE_DIR}/jsrt_aux.lua jsrt_aux.h
        MAIN_DEPENDENCY jsrt_aux.lua
        COMMENT "Pre-compiling jsrt-extra.lua" VERBATIM)

# Use flex to generate a JSON lexer & parser
flex_target(JSON_LEX json.lex ${CMAKE_CURRENT_BINARY_DIR}/json_parse.c)

# The Lua-JS Runtime CLI
add_executable(jsi jsrt_cli.c)
target_include_directories(jsi PRIVATE ${LUA_INCLUDE_DIR})
target_link_libraries(jsi PRIVATE jsrt ${LUA_LIBRARY})
if (LUA_LIBRARY MATCHES ".a$")
    target_link_libraries(jsi PRIVATE -lm ${CMAKE_DL_LIBS})
endif()
target_compile_definitions(jsi PRIVATE
        LJS_VERSION=\"${CMAKE_PROJECT_VERSION}\")
set_target_properties(jsi PROPERTIES
        INSTALL_RPATH ${CMAKE_INSTALL_PREFIX}/${LUA_MODULE_PATH})
if (LJS_ENABLE_LIBEV)
    target_sources(jsi PRIVATE jsrt_timer.c)
endif()

# Enable interactive if libreadline is present
find_library(READLINE_LIBRARY NAMES readline)
if (READLINE_LIBRARY)
    target_compile_definitions(jsi PRIVATE HAVE_READLINE)
    target_link_libraries(jsi PRIVATE ${READLINE_LIBRARY})
endif()
