cmake_minimum_required(VERSION 3.16)
project(ljs LANGUAGES C VERSION 0.2.0)

# Enable libev default loop integration
option(LJS_ENABLE_LIBEV "Enable libev integration" ON)

# On Linux, machine type should be present in module path
if(CMAKE_SYSTEM_NAME STREQUAL "Linux")
	execute_process(COMMAND ${CMAKE_C_COMPILER} -dumpmachine
		OUTPUT_VARIABLE MACHINE_TYPE OUTPUT_STRIP_TRAILING_WHITESPACE)
	set(LUA_MODULE_PATH lib/${MACHINE_TYPE}/lua/5.1)
else()
	set(LUA_MODULE_PATH lib/lua/5.1)
endif()

# Find static luajit library, so JS Runtime CLI can link to it
find_library(LUA_LIBRARY NAMES libluajit-5.1.a PATHS /usr/local/lib)

add_subdirectory(yyjs)
add_subdirectory(jsrt)
file(GLOB LUA_TYPE_DECLS "docs/ts-decl/*.d.ts")

install(TARGETS jsrt DESTINATION ${LUA_MODULE_PATH})
install(TARGETS jsi)
install(FILES ${LUA_TYPE_DECLS} DESTINATION share/ljs/types/jsrt/)