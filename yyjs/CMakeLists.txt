cmake_minimum_required(VERSION 3.16)
find_package(FLEX REQUIRED)
find_package(BISON REQUIRED)

bison_target(YYJS_PARSER yyjs.y ${CMAKE_CURRENT_BINARY_DIR}/yyjs.tab.c)
flex_target(YYJS_LEX yyjs.lex ${CMAKE_CURRENT_BINARY_DIR}/yyjs_lex.c
        DEFINES_FILE ${CMAKE_CURRENT_BINARY_DIR}/yyjs_lex.h)
add_flex_bison_dependency(YYJS_LEX YYJS_PARSER)

include_directories(${CMAKE_SOURCE_DIR}/yyjs)
add_library(yyjs STATIC
        ${FLEX_YYJS_LEX_OUTPUTS} ${BISON_YYJS_PARSER_OUTPUTS}
        yyjs_bcgen.h yyjs_bcgen_lj.c lj_bc.h
        yyjs_parse.h yyjs_parse.c)
target_compile_options(yyjs PRIVATE -fPIC)
target_include_directories(yyjs PRIVATE ${CMAKE_CURRENT_BINARY_DIR})
set_target_properties(yyjs PROPERTIES
        C_VISIBILITY_PRESET hidden
        POSITION_INDEPENDENT_CODE True)

# Avoid GCC complaint about i_buf[0] access
if (CMAKE_C_COMPILER_ID STREQUAL "GNU")
    target_compile_options(yyjs PRIVATE -Wno-stringop-overflow)
endif()