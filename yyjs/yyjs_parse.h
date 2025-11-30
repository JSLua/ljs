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

#ifndef LYYJS_YYJS_PARSE_H
#define LYYJS_YYJS_PARSE_H

#include <stdint.h>

typedef unsigned long long expr_t;
#define YYSTYPE expr_t

#ifndef YYBISON
#include <sys/queue.h>

enum yy_kgc_type { KgcFunction, KgcString };

struct yy_kgc_value {
    SLIST_ENTRY(yy_kgc_value) q_entry, h_entry;
    uint16_t seq;
    uint8_t type;
    uint32_t hash;
    union {
        uint64_t value;  /* For integers */
        uint32_t len; /* For other */
    };
    char str[0];
};

SLIST_HEAD(yy_kgc_value_q, yy_kgc_value);
#define YY_INTERN_BUCKET_NUM 32

struct yy_intern_state {
    uint16_t kgc_next;
    struct yy_kgc_value_q kgc_list;
    struct yy_kgc_value_q kgc_bucket[YY_INTERN_BUCKET_NUM];
};

struct yy_line_mark { uint16_t ins, line; };

struct yy_function {
    STAILQ_ENTRY(yy_function) q_entry;
    uint16_t line_start, line_last;
    uint8_t upc, argc, frame_size, flags;
#define YY_FUN_F_HAS_PROTO  1
#define YY_FUN_F_VARARG     2
#define yy_fun_this_uv(fun) (fun->flags >> 2)
    struct yy_intern_state *intern;

    /* Number interning (for LuaJIT) */
    unsigned int n_avail, n_next;
    uint64_t *numbers;

    /* Line number debug info */
    unsigned int l_avail, l_next;
    struct yy_line_mark *lines;

    uint16_t i_avail, i_next;
    uint32_t i_buf[0];
};

#define yy_get_sealed_uv(fun) (struct yy_upvalue *) \
    ((fun)->i_buf + (fun)->i_next)

struct yy_parse_scope {
    SLIST_ENTRY(yy_parse_scope) s_entry;
    int label;
    uint8_t base, local_top;
    uint16_t start_pc, cont_pc, fall_chain;

#define YY_SCOPE_BREAKABLE  1
#define YY_SCOPE_LOOP       2
#define YY_SCOPE_HAS_UV     4
#define YY_SCOPE_CATCH      8
    uint16_t flags, n_iter_i;
    uint32_t iter_i[10];
};

struct yy_local_var {
    SLIST_ENTRY(yy_local_var) s_entry;
    struct yy_parse_scope *scope;
    int id;
    expr_t expr_val;
};

struct yy_upvalue { uint16_t uv, id; };

struct yy_parse_context {
    SLIST_ENTRY(yy_parse_context) s_entry;

    struct yy_function *fun;
    uint8_t sp, var_hint;
    uint16_t last_target;
    /* Statement label or to-be-declared identifier */
    uint16_t tag_id;
    uint8_t uv_this, flags;
#define YY_MASK_EMIT        1
#define YY_CONTEXT_TEMP     2
#define YY_CONTEXT_ARROW    4
    SLIST_HEAD(, yy_parse_scope) scope_chain;
    SLIST_HEAD(, yy_local_var) local_chain;

    /* Upvalues */
    unsigned int u_next;
    struct yy_upvalue upvalues[39];
};

SLIST_HEAD(yy_parse_context_q, yy_parse_context);
STAILQ_HEAD(yy_function_q, yy_function);

#ifndef YY_TYPEDEF_YY_SCANNER_T
typedef void *yyscan_t;
#define YY_TYPEDEF_YY_SCANNER_T
#endif

struct yy_parse_state {
    /* Error state storage */
    char err_msg[252];
    int err_line;

    uint16_t id_length;   /* For length operator */
    uint16_t id_tostring; /* For the .toString() method */
    uint16_t id_super;    /* To check call of super(...) */
    uint8_t flags;
#define YY_PARSER_CALL_RSV  1 /* For LuaJIT GC64 */
#define YY_PARSER_STRIP     2
#define YY_PARSER_ARR_BASE1 4 /* For unmodified LuaJIT */

    /* Parse context */
    struct yy_function_q fun_list;
    struct yy_parse_context_q ctx_chain;

    /* Lexer states */
    yyscan_t scan;
    struct yy_intern_state *id_intern;
    char input_name[256];
};

#define yy_current_ctx(state) SLIST_FIRST(&state->ctx_chain)

void yy_parser_push_context(struct yy_parse_state *, uint8_t flags);
int yy_parser_pop_context(struct yy_parse_state *);
struct yy_parse_scope *
yy_parser_push_scope(struct yy_parse_state *, uint8_t flags);
void yy_parser_pop_scope(struct yy_parse_state *);
void yy_ctx_reset_stack(struct yy_parse_context *);
int yy_emit_ins(struct yy_parse_context *, uint32_t i);
uint32_t yy_use_number(struct yy_function *, uint64_t);

#endif

enum ExprType { T_LV_REF = 1, T_PROTO, T_STR, T_PRI, T_INT };
#define expr_tag(t)     ((expr_t)t << 48)
#define expr_type(x)    (x >> 48)
#define expr_int(x)     (expr_tag(T_INT) | (unsigned)(x))

/*
 * When expr_type(x) is 0, the expression is already discharged as bytecode.
 * The lower 48 bits are arranged in the following pattern:
 *  | 47 ............ 32 | 31 ............ 16 | 15 ... 4 | 3 ..  |
 *  | negative chain ptr | positive chain ptr | reserved | flags |
 * Flags are defined below:
 */
#define V_EXPR_F_LOGICAL  1
#define V_EXPR_F_BIT_OP   2     /* Check for non-zero at test */
#define V_EXPR_F_CALL     4     /* Suitable for tail call */

/*
 * When expr_type(x) == T_LV_REF, the lower 48 bits are arranged like this:
 *  | 47 . 32 | 31 .. 28 | 27 .. 24 | 23 ..... 16 | 15 ..... 0 |
 *  | unused  | reserved |  flags   | owner table | identifier |
 * Flags are defined below:
 */
#define LV_EXPR_F_RAW   (1 << 24)

typedef struct yy_parse_state yy_parse_state_t;

void yy_set_identifier_tag(struct yy_parse_state *, int id);
void yy_set_variable_hint(struct yy_parse_state *, int hint);
#define YY_VAR_HINT_CONST 0x100
#define YY_VAR_HINT_RAW   0x200 /* this-less library */

int yy_intern_string(struct yy_parse_state *,
                     const char *, uint32_t len, int quoted);
int yy_localize_id(struct yy_parse_state *, uint16_t id);
expr_t yy_pack_number(const char *, int base);
expr_t yy_apply_math(struct yy_parse_state *, int opr, expr_t, expr_t);
expr_t yy_parser_create_lv(struct yy_parse_state *, expr_t t, int id);
int yy_parser_alloc_local(struct yy_parse_state *, int id, expr_t);
void yy_parser_mark_line(struct yy_parse_state *, int line);
void yy_parser_set_error(struct yy_parse_state *, const char *msg);

#ifndef YYBISON

struct yy_dump_state { uint32_t b_avail; char *buffer, *ptr; };
void yy_dump_append_bytes(struct yy_dump_state *, void *, uint32_t);
void yy_dump_append_byte(struct yy_dump_state *, uint8_t b);
void yy_dump_merge(struct yy_dump_state *, struct yy_dump_state *);

long yyjs_dump_bc(struct yy_parse_state *, struct yy_dump_state *);
int yyjs_parse(struct yy_parse_state *, void *, int len);
int yyjs_parse_file(struct yy_parse_state *, const char *filename);
struct yy_parse_state *yyjs_parser_new();
void yyjs_parser_destroy(struct yy_parse_state *);

/* Shared by both the parser and `string.fromCodePoint` */
#define u8_encode_mbs(F, p, codepoint) do { \
    if (codepoint >> 7) { \
        if (codepoint >> 11) { \
            if (codepoint >> 16) { \
                F((p), 0xf0 | codepoint >> 18); \
                F((p), 0x80 | codepoint >> 12 & 63); \
            } else F((p), 0xe0 | codepoint >> 12); \
            F((p), 0x80 | codepoint >> 6 & 63); \
        } else F((p), 0xc0 | codepoint >> 6); \
        F((p), 0x80 | codepoint & 63); \
    } else F((p), codepoint); \
} while(0)

#endif

#endif //LYYJS_YYJS_PARSE_H
