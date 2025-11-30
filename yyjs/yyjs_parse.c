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

#include "yyjs_parse.h"
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <math.h>
#include "yyjs_lex.h"  /* FLEX generated lexer header */

#define I_ALLOC_INCREMENTAL 256
#define ID_INTERN_BASE 0x4000

#define yy_current_fun(state) yy_current_ctx(state)->fun

static struct yy_intern_state *yy_intern_alloc()
{
    struct yy_intern_state *intern = malloc(sizeof(*intern));

    if (intern) {
        intern->kgc_next = 0;
        SLIST_INIT(&intern->kgc_list);
        for (int i = 0; i < YY_INTERN_BUCKET_NUM; i++)
            SLIST_INIT(&intern->kgc_bucket[i]);
    }

    return intern;
}

static void yy_intern_destroy(struct yy_intern_state *intern)
{
    struct yy_kgc_value *val;
    while ((val = SLIST_FIRST(&intern->kgc_list))) {
        SLIST_REMOVE_HEAD(&intern->kgc_list, q_entry);
        free(val);
    }

    free(intern);
}

static void yy_function_destroy(struct yy_function *fun)
{
    if (fun->lines) free(fun->lines);
    if (fun->numbers) free(fun->numbers);
    yy_intern_destroy(fun->intern);
    free(fun);
}

static uint8_t
yy_ctx_retain_upvalue(struct yy_parse_context *ctx, uint16_t uv, int id)
{
    unsigned int u_id = ctx->u_next++;  /* Alloc new upvalue */
    assert(u_id < sizeof(ctx->upvalues) / sizeof(struct yy_upvalue));
    ctx->upvalues[u_id].uv = uv;
    ctx->upvalues[u_id].id = id;
    if (uv >> 15 && !(ctx->flags & YY_CONTEXT_TEMP)) {
        struct yy_parse_context *parent = SLIST_NEXT(ctx, s_entry);
        struct yy_parse_scope *scope = SLIST_FIRST(&parent->scope_chain);
        while ((uint8_t)uv < scope->base && SLIST_NEXT(scope, s_entry))
            scope = SLIST_NEXT(scope, s_entry);
        scope->flags |= YY_SCOPE_HAS_UV;
    }
    return u_id;
}

void yy_parser_push_context(struct yy_parse_state *state, uint8_t flags)
{
    struct yy_parse_context *ctx = malloc(sizeof(*ctx));
    assert(ctx);
    memset(ctx, 0, sizeof(*ctx));
    ctx->fun = malloc(sizeof(struct yy_function) +
            I_ALLOC_INCREMENTAL * sizeof(uint32_t));
    assert(ctx->fun);
    memset(ctx->fun, 0, sizeof(struct yy_function));
    SLIST_INIT(&ctx->local_chain);
    SLIST_INIT(&ctx->scope_chain);
    ctx->fun->i_avail = I_ALLOC_INCREMENTAL;
    ctx->fun->argc = 1;  /* First arg is this */
    ctx->fun->intern = yy_intern_alloc();
    assert(ctx->fun->intern);
    ctx->fun->line_start = ctx->fun->line_last = yyget_lineno(state->scan);
    ctx->flags = flags;
    SLIST_INSERT_HEAD(&state->ctx_chain, ctx, s_entry);
    yy_parser_push_scope(state, 0);
}

int yy_parser_pop_context(struct yy_parse_state *state)
{
    struct yy_parse_context *ctx = yy_current_ctx(state);
    while (SLIST_FIRST(&ctx->scope_chain))
        yy_parser_pop_scope(state);
    SLIST_REMOVE_HEAD(&state->ctx_chain, s_entry);
    if (ctx->flags & YY_MASK_EMIT) {  /* in dead branch */
        yy_function_destroy(ctx->fun);
        free(ctx);
        return -1;
    }

    /* Seal upvalues to the end of function */
    size_t size_uv = ctx->u_next * sizeof(struct yy_upvalue);
    ctx->fun = realloc(ctx->fun, // NOLINT(*-suspicious-realloc-usage)
                       sizeof(struct yy_function) +
                       ctx->fun->i_next * sizeof(uint32_t) + size_uv);
    assert(ctx->fun);
    ctx->fun->i_avail = ctx->fun->i_next;
    memcpy(yy_get_sealed_uv(ctx->fun), ctx->upvalues, size_uv);
    ctx->fun->upc = ctx->u_next;
    ctx->fun->flags |= ctx->uv_this << 2;
    STAILQ_INSERT_TAIL(&state->fun_list, ctx->fun, q_entry);
    free(ctx);

    ctx = yy_current_ctx(state);
    if (ctx) {
        struct yy_kgc_value *kgc = malloc(sizeof(struct yy_kgc_value));
        assert(kgc);
        memset(kgc, 0, sizeof(*kgc));
        kgc->seq = ctx->fun->intern->kgc_next++;
        kgc->type = KgcFunction;
        SLIST_INSERT_HEAD(&ctx->fun->intern->kgc_list, kgc, q_entry);
        ctx->fun->flags |= YY_FUN_F_HAS_PROTO;
        return kgc->seq;
    }

    return -1;
}

struct yy_parse_scope *
yy_parser_push_scope(struct yy_parse_state *state, uint8_t flags)
{
    struct yy_parse_scope *scope;
    struct yy_parse_context *ctx = yy_current_ctx(state);
    assert(ctx);
    scope = malloc(sizeof(*scope));
    assert(scope);
    memset(scope, 0, sizeof(*scope));
    scope->start_pc = scope->cont_pc = ctx->fun->i_next;
    scope->local_top = scope->base = ctx->sp + 1;
    scope->flags = flags;
    if (ctx->tag_id) {
        scope->label = ctx->tag_id;
        scope->flags |= YY_SCOPE_BREAKABLE;
        ctx->tag_id = 0;
    }
    ctx->var_hint = 0;
    SLIST_INSERT_HEAD(&ctx->scope_chain, scope, s_entry);
    return scope;
}

void yy_parser_pop_scope(struct yy_parse_state *state)
{
    struct yy_parse_context *ctx = yy_current_ctx(state);
    struct yy_parse_scope *scope = SLIST_FIRST(&ctx->scope_chain);
    assert(scope);
    for (struct yy_local_var *var; (var = SLIST_FIRST(&ctx->local_chain)); ) {
        if (var->scope != scope) break;
        SLIST_REMOVE_HEAD(&ctx->local_chain, s_entry);
        free(var);
    }
    SLIST_REMOVE_HEAD(&ctx->scope_chain, s_entry);
    free(scope);
    yy_ctx_reset_stack(ctx);
}

int yy_emit_ins(struct yy_parse_context *ctx, uint32_t i)
{
    struct yy_function *fun = ctx->fun;
    if (ctx->flags & YY_MASK_EMIT) return 0;
    if (fun->i_next > fun->i_avail) {
        fun->i_avail += I_ALLOC_INCREMENTAL;
        fun = realloc(fun, // NOLINT(*-suspicious-realloc-usage)
                      sizeof(*fun) + fun->i_avail * sizeof(uint32_t));
        assert(fun);
        ctx->fun = fun;
    }

    uint16_t i_idx = fun->i_next++;
    fun->i_buf[i_idx] = i;
    return i_idx;
}

uint32_t yy_use_number(struct yy_function *fun, uint64_t d)
{
    uint32_t seq;
    for (seq = 0; seq < fun->n_next; seq++)
        if (fun->numbers[seq] == d)
            return seq;
    seq = fun->n_next++;
    if (seq == fun->n_avail) {
        fun->n_avail += 128;
        fun->numbers = realloc( // NOLINT(*-suspicious-realloc-usage)
                fun->numbers, fun->n_avail * sizeof(double));
        assert(fun->numbers);
    }
    fun->numbers[seq] = d;
    return seq;
}

static expr_t yy_ctx_lookup_local(struct yy_parse_context *ctx, int id)
{
    for (struct yy_local_var *var = SLIST_FIRST(&ctx->local_chain); var;
         var = SLIST_NEXT(var, s_entry))
        if (var->id == id)
            return var->expr_val;

    /* Also have a look at upvalues for id */
    for (int i = 0; i < ctx->u_next; i++)
        if (ctx->upvalues[i].id == id)
            return 0x800 | ctx->upvalues[i].uv & 0x700 | i;

    struct yy_parse_context *parent = SLIST_NEXT(ctx, s_entry);
    if (!parent) return 0;
    else if (id == parent->tag_id)  /* Identifier to be declared */
        return 0x800 | parent->var_hint << 8 | yy_ctx_retain_upvalue
            (ctx, SLIST_FIRST(&parent->scope_chain)->local_top | 0x8000, id);

    expr_t parent_local = yy_ctx_lookup_local(parent, id);
    if (expr_type(parent_local)) return parent_local;
    else if (parent_local) {
        int uv_flag = parent_local >> 11 ? 0 : 0x8000;
        return 0x800 | parent_local & 0x700 | yy_ctx_retain_upvalue
                (ctx, parent_local & 0x7ff | uv_flag, id);
    }
    return 0;
}

static void yy_ctx_require_uv_this(struct yy_parse_context *ctx)
{
    if (ctx->uv_this || !(ctx->flags & YY_CONTEXT_ARROW))
        return;

    uint16_t uv = 0x8000 | YY_VAR_HINT_CONST;
    struct yy_parse_context *outer = SLIST_NEXT(ctx, s_entry);
    if (outer->flags & YY_CONTEXT_ARROW) {
        yy_ctx_require_uv_this(outer);
        uv = outer->uv_this - 1;
    }
    ctx->uv_this = 1 + yy_ctx_retain_upvalue(ctx, uv, 0xffff);
}

void yy_ctx_reset_stack(struct yy_parse_context *ctx)
{
    struct yy_parse_scope *scope = SLIST_FIRST(&ctx->scope_chain);
    if (scope) {
        int top_slot = scope->local_top - 1;
        assert(ctx->sp >= top_slot);
        ctx->sp = top_slot;
        ctx->var_hint = 0;
        ctx->tag_id = 0;
    }
}

void yy_set_identifier_tag(struct yy_parse_state *state, int id)
{
    yy_current_ctx(state)->tag_id = id;
}

void yy_set_variable_hint(struct yy_parse_state *state, int hint)
{
    yy_current_ctx(state)->var_hint |= hint >> 8;
}

unsigned int bernstein_hash(const char *str, unsigned int len)
{
    unsigned int hash = 0;
    while (len--)
        hash = (hash * 33) ^ (unsigned int) *str++;
    return hash;
}

static int yy_intern_unquoted(struct yy_intern_state *intern,
                              const char *str, uint32_t len)
{
    uint32_t hash = bernstein_hash(str, len);
    struct yy_kgc_value_q *bucket =
            &intern->kgc_bucket[hash % YY_INTERN_BUCKET_NUM];
    struct yy_kgc_value *val;
    SLIST_FOREACH(val, bucket, h_entry) {
        if (val->type == KgcString && val->hash == hash &&
            val->len == len && !memcmp(val->str, str, len))
            return val->seq;
    }

    val = malloc(sizeof(*val) + len);
    assert(val != NULL);
    val->seq = intern->kgc_next++;
    val->type = KgcString;
    val->hash = hash;
    val->len = len;
    memcpy(val->str, str, val->len);
    SLIST_INSERT_HEAD(&intern->kgc_list, val, q_entry);
    SLIST_INSERT_HEAD(bucket, val, h_entry);
    return val->seq;
}

#define add_char(ptr, ch) *(ptr)++ = (char)(ch)

size_t yy_unescape_str(char *buf, const char *str, size_t len, int re)
{
    char *ptr = buf;
    while (len) {
        if (*str != '\\' || len == 1) {
            *ptr++ = *str++, len--;
            continue;
        }
        switch(*++str) {
            case '0': case '1': case '2': case '3':
            case '4': case '5': case '6': case '7':
                add_char(ptr, *str - '0'); break;
            case 'b': *ptr++ =  8; break; case 't': *ptr++ =  9; break;
            case 'n': *ptr++ = 10; break; case 'v': *ptr++ = 11; break;
            case 'f': *ptr++ = 12; break; case 'r': *ptr++ = 13; break;
            case '\n': break;
            case 'x': /* HexEscapeSequence */
                add_char(ptr, strtol((char[]){ str[1], str[2], 0 }, NULL, 16));
                str += 3, len -= 4;
                continue;
            case 'u': { /* UnicodeEscapeSequence */
                uint32_t codepoint;
                if (str[1] == '{') {
                    char *right_bound;  /* } */
                    codepoint = strtol(str + 2, &right_bound, 16) & 0x1fffffu;
                    len -= ++right_bound - str + 1;
                    str = right_bound;
                } else {
                    char val[] = { str[1], str[2], str[3], str[4], 0 };
                    codepoint = strtol(val, NULL, 16);
                    str += 5, len -= 6;
                }

                /* Encode codepoint as UTF-8 sequence */
                u8_encode_mbs(add_char, ptr, codepoint);
                continue;
            }
            default:
                if (re) *ptr++ = '\\';
                *ptr++ = *str;
        }
        str++, len -= 2;
    }
    return ptr - buf;
}

int yy_intern_string(struct yy_parse_state *state,
                     const char *str, uint32_t len, int quoted)
{
    if (!quoted)
        /* Treat unquoted strings as identifier */
        return yy_intern_unquoted(state->id_intern, str, len);
    /* Skip string literals in dead branch */
    if (yy_current_ctx(state)->flags & YY_MASK_EMIT) return 0;

    struct yy_function *fun = yy_current_fun(state);
    assert(fun->intern->kgc_next < ID_INTERN_BASE);
    if (quoted == 2)
        return yy_intern_unquoted(fun->intern, str, len);
    assert(len >= 2);

    size_t q_len = len - 2;
    if (str[len - 1] == '{')
        q_len--;  /* ${ */
    /* Do not intern empty template spans */
    if ((str[0] == '}' || str[len - 1] == '{') && q_len == 0)
        return -1;

    int re = *str == '/';
    /* Handle regexp */
    if (re) {
        const char *tail = strrchr(str + 1, '/');
        assert(tail);
        q_len = tail - (str + 1);
    }

    int id;
    /* If no escapes, go fast path */
    if (!re && !strchr(str, '\\'))
        id = yy_intern_unquoted(fun->intern, str + 1, q_len);
    else {
        char *unquoted = malloc(q_len + re);
        assert(unquoted);
        q_len = yy_unescape_str(unquoted + re, str + 1, q_len, re);
        if (re) {
            /* Embed RegExp flags in first char */
            *unquoted = 0;
            for (size_t i = len - 1; i > 1; i--) {
                switch (str[i]) {
                    case 'i': *unquoted |= 1; break;
                    case 'm': *unquoted |= 1 << 1; break;
                    default: break;
                }
            }
        }
        id = yy_intern_unquoted(fun->intern, unquoted, q_len + re);
        free(unquoted);
    }

    return id;
}

int yy_localize_id(struct yy_parse_state *state, uint16_t id)
{
    struct yy_kgc_value *it;
    if (id < ID_INTERN_BASE)
        return id;
    SLIST_FOREACH(it, &state->id_intern->kgc_list, q_entry)
        if (it->seq == id)
            break;
    assert(it);
    return yy_intern_unquoted(yy_current_fun(state)->intern,
                              it->str, it->len);
}

expr_t yy_pack_number(const char *str, int base) {
    union { double d; uint64_t u; } v;
    if (base == 10) {
        v.d = strtod(str, NULL);
        if (v.d == (int)v.d && (unsigned)v.d <= INT32_MAX)
            return expr_int(v.d);
    } else {
        unsigned long long ll = strtoll(str, NULL, base);
        if (ll <= INT32_MAX) return expr_int(ll);
        v.d = (double)ll;
    }
    return ~v.u;
}

extern expr_t yy_emit_operator(yy_parse_state_t *, int opr, expr_t);

#include "yyjs.tab.h"

expr_t yy_apply_math(struct yy_parse_state *state,
                     int opr, expr_t lhs, expr_t rhs)
{
    if (expr_type(lhs) >= T_INT) {
        if (expr_type(rhs) == T_PRI) rhs = expr_int((rhs & 3) >> 1);
        if (expr_type(rhs) >= T_INT) {
            union { double d; uint64_t u; } lhn, rhn;
            if (expr_type(lhs) == T_INT) lhn.d = (int)lhs; else lhn.u = ~lhs;
            if (expr_type(rhs) == T_INT) rhn.d = (int)rhs; else rhn.u = ~rhs;
            if (opr == '&' || opr == '^' || opr == '|' ||  /* want bits? */
                    opr == P_SHL || opr == P_ASHR || opr == P_SHR)
                lhn.d += 6755399441055744.0, rhn.d += 6755399441055744.0;
            switch (opr) {
                case '+': lhn.d += rhn.d; break;
                case '-': lhn.d -= rhn.d; break;
                case '*': lhn.d *= rhn.d; break;
                case '/': lhn.d /= rhn.d; break;
                case '%': lhn.d = fmod(lhn.d, rhn.d); break;
                case P_POW: lhn.d = pow(lhn.d, rhn.d); break;
                case '&': return expr_int(lhn.u & rhn.u);
                case '^': return expr_int(lhn.u ^ rhn.u);
                case '|': return expr_int(lhn.u | rhn.u);
                case P_SHL: return expr_int(lhn.u << (rhn.u & 31));
                case P_ASHR: return expr_int((int)lhn.u >> (rhn.u & 31));
                case P_SHR: return expr_int((unsigned)lhn.u >> (rhn.u & 31));
                default: assert(0);
            }
            return lhn.d == (double)(int)lhn.d ?
                   expr_int((int)lhn.d) : ~lhn.u;
        }

        yy_emit_operator(state, '#', rhs);
        rhs = lhs;
        opr = (opr == '&' || opr == '^' || opr == '|' ||
                opr == '+' || opr == '*') ? opr : -opr;
    }
    return yy_emit_operator(state, opr, rhs);
}

expr_t yy_parser_create_lv(struct yy_parse_state *state, expr_t t, int id)
{
    struct yy_parse_context *ctx = yy_current_ctx(state);
    uint32_t lv_ref;
    if (t == -1) {
        lv_ref = 0xff0000u;
        if (id == -1) {  /* this */
            lv_ref |= 0xf000u;
            if (ctx->flags & YY_CONTEXT_ARROW)
                yy_ctx_require_uv_this(ctx);  /* Use captured this */
        } else {
            expr_t local_var = yy_ctx_lookup_local(ctx, id);
            if (expr_type(local_var)) return local_var;
            else if (local_var) {
                lv_ref |= 0xf000u | (local_var & 0xfff);  /* local */
                if (local_var & YY_VAR_HINT_RAW)
                    lv_ref |= LV_EXPR_F_RAW;  /* Mark LV-ref Lua */
            } else {  /* global */
                assert(id < 0xf000);
                lv_ref |= id & 0xffffu;
            }
        }
    } else if (id == -1) {
        assert(ctx->sp >= 1);
        lv_ref = (ctx->sp - 1) << 16 | 0xffffu;
    } else {
        uint8_t table_pos = (
            expr_type(t) == T_LV_REF && (t & 0xfff800u) == 0xfff000u
            ) ? t : ((void)yy_emit_operator(state, 0, t), ctx->sp);
        lv_ref = table_pos << 16 | (id & 0xffffu);
        lv_ref |= t & LV_EXPR_F_RAW;
    }

    return expr_tag(T_LV_REF) | lv_ref;
}

int yy_parser_alloc_local(struct yy_parse_state *state, int id, expr_t x)
{
    struct yy_local_var *var;
    struct yy_parse_context *ctx = yy_current_ctx(state);
    struct yy_parse_scope *scope = SLIST_FIRST(&ctx->scope_chain);
    assert(scope);
    for (var = SLIST_FIRST(&ctx->local_chain);
         var && var->scope == scope; var = SLIST_NEXT(var, s_entry))
        if (var->id == (id & 0xffff)) {
            yy_parser_set_error(state, "Redeclaration of local variable");
            return -1;
        }
    if (ctx->tag_id == id) ctx->tag_id = 0;

    /* Allocate the new local variable */
    var = malloc(sizeof(struct yy_local_var));
    var->scope = scope;
    var->id = id & 0xffff;
    if (!SLIST_NEXT(scope, s_entry) && x == -1)
        ctx->sp = ctx->fun->argc++;
    else if (ctx->sp == scope->local_top - 1 && !x)
        ctx->sp++;  /* Assuming value is already there */
    else if (expr_type(x) >= T_PRI &&
             (ctx->var_hint & (YY_VAR_HINT_CONST >> 8))) {
        if (x == expr_tag(T_PRI)) {
            yy_parser_set_error
                (state, "Missing initializer in const declaration");
            free(var);
            return -2;
        }
        var->expr_val = x;
        goto accept;
    }
    else yy_emit_operator(state, 0, x);

    assert (ctx->sp == scope->local_top);
    scope->local_top++;
    var->expr_val = ctx->var_hint << 8 | ctx->sp;
accept: SLIST_INSERT_HEAD(&ctx->local_chain, var, s_entry);
    return ctx->sp;
}

void yy_parser_mark_line(struct yy_parse_state *state, int line)
{
    struct yy_function *fun = yy_current_fun(state);
    if (line == fun->line_start)
        return;
    fun->line_last = line;
    if (state->flags & YY_PARSER_STRIP)
        return;

    if (fun->lines) {
        struct yy_line_mark *last_line = &fun->lines[fun->l_next - 1];
        if (last_line->ins == fun->i_next) {
            last_line->line = line;  /* Reuse empty lines */
            return;
        }
    }

    if (fun->l_next == fun->l_avail) {
        fun->l_avail += 32;
        fun->lines = realloc( // NOLINT(*-suspicious-realloc-usage)
                fun->lines,
                sizeof(struct yy_line_mark) * fun->l_avail);
        assert(fun->lines);
    }

    struct yy_line_mark *mark = &fun->lines[fun->l_next++];
    mark->ins = fun->i_next;
    mark->line = line;
}

void yy_parser_set_error(struct yy_parse_state *state, const char *msg)
{
    strcpy(state->err_msg, msg);
    state->err_line = yyget_lineno(state->scan);
}

void yy_lex_set_input(yyscan_t scan, void *input, int len);
extern int yyparse(yyscan_t scan);
extern expr_t yy_close_function(yy_parse_state_t *state);

int yyjs_parse(struct yy_parse_state *state, void *f, int len)
{
    int ret;
    yy_lex_set_input(state->scan, f, len); /* f is FILE* if len == -1 */
    yy_parser_push_context(state, 0);
    if (state->flags & YY_PARSER_STRIP) {
        int k_debug_id = yy_intern_string(state, "_DEBUG", 6, 0);
        yy_set_variable_hint(state, YY_VAR_HINT_CONST);
        yy_parser_alloc_local(state, k_debug_id, expr_tag(T_PRI) | 1);
        yy_current_ctx(state)->var_hint = 0;
    }
    ret = yyparse(state->scan);
    if (ret == 0)
        yy_close_function(state);
    return ret;
}

int yyjs_parse_file(struct yy_parse_state *state, const char *filename)
{
    FILE *fp = stdin;
    if (filename[0] == '-' && !filename[1])
        strcpy(state->input_name, "@stdin");
    else {
        if (!(fp = fopen(filename, "r"))) {
            strcpy(state->err_msg, strerror(errno));
            return -1;
        }
        *state->input_name = '@';
        strcpy(state->input_name + 1, filename);
    }

    int ret = yyjs_parse(state, fp, -1);
    if (fp != stdin)
        fclose(fp);
    return ret;
}

struct yy_parse_state *yyjs_parser_new()
{
    struct yy_parse_state *state;
    if (!(state = malloc(sizeof(*state))))
        return NULL;
    memset(state, 0, sizeof(*state));
    SLIST_INIT(&state->ctx_chain);
    STAILQ_INIT(&state->fun_list);
    state->id_intern = yy_intern_alloc();
    if (!state->id_intern)
        goto cleanup;
    state->id_intern->kgc_next = ID_INTERN_BASE;
    state->id_length = yy_intern_unquoted(state->id_intern, "length", 6);
    state->id_tostring = yy_intern_unquoted(state->id_intern, "toString", 8);
    state->id_super =  yy_intern_unquoted(state->id_intern, "super", 5);
    if (yylex_init_extra(state, &state->scan) != 0)
        goto cleanup;
    return state;

cleanup:
    if (state->id_intern)
        yy_intern_destroy(state->id_intern);
    free(state);
    return NULL;
}

void yyjs_parser_destroy(struct yy_parse_state *state)
{
    while (yy_current_ctx(state))
        yy_parser_pop_context(state);

    struct yy_function *fun;
    while ((fun = STAILQ_FIRST(&state->fun_list))) {
        STAILQ_REMOVE_HEAD(&state->fun_list, q_entry);
        yy_function_destroy(fun);
    }

    yylex_destroy(state->scan);
    if (state->id_intern)
        yy_intern_destroy(state->id_intern);
    free(state);
}

static void yy_dump_reserve(struct yy_dump_state *s, size_t len) {
    size_t used = s->ptr - s->buffer;
    size_t target = s->b_avail;
    while (used + len > target)
        target += 0x400;
    if (target != s->b_avail) {
        char *new_buffer = realloc(s->buffer, target);
        assert(new_buffer);
        s->buffer = new_buffer;
        s->ptr = s->buffer + used;
        s->b_avail = target;
    }
}

void yy_dump_append_bytes(struct yy_dump_state *s, void *buf, uint32_t len)
{
    yy_dump_reserve(s, len);
    memcpy(s->ptr, buf, len);
    s->ptr += len;
}

void yy_dump_append_byte(struct yy_dump_state *s, uint8_t b)
{
    yy_dump_reserve(s, 1);
    *(s->ptr++) = (char)b;
}

void yy_dump_merge(struct yy_dump_state *dst, struct yy_dump_state *src)
{
    if (src->buffer) {
        if (dst) {
            size_t len = src->ptr - src->buffer;
            yy_dump_append_bytes(dst, src->buffer, len);
        }

        free(src->buffer);
        memset(src, 0, sizeof(*src));
    }
}
