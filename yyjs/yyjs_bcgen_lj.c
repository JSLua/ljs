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

#include <string.h>
#include <assert.h>

#include "yyjs.tab.h"
#include "yyjs_bcgen.h"

#define LJ_ENDIAN_SELECT(le, be) le
typedef uint32_t BCIns;
typedef uint16_t BCReg;
#include "lj_bc.h"
#define DEAD_TAG 0xffffu

#define last_bc(ctx)  (ctx->fun->i_buf + ctx->fun->i_next - 1)
#define bc_is_cmp(bc) (bc_op(bc) < BC_MOV)

inline static int bc_is_ret(BCIns bc)
{
    BCOp op = bc_op(bc);
    return bc_isret(op) || op == BC_CALLT || op == BC_CALLMT;
}

#define USE_CTX(ctx) struct yy_parse_context *ctx = yy_current_ctx(state)
#define USE_SCOPE(ctx)  struct yy_parse_scope *scope = \
        SLIST_FIRST(&ctx->scope_chain)
#define CALL_RSV        (state->flags & YY_PARSER_CALL_RSV)
#define yy_symbol(str)  yy_intern_string(state, str, strlen(str), 2)
#define update_frame_size(ctx) if (ctx->sp > ctx->fun->frame_size) \
        ctx->fun->frame_size = ctx->sp

expr_t yy_close_function(yy_parse_state_t *state)
{
    int seq;
    USE_CTX(ctx);
    yy_bcg_close_scope(state, 0);
    assert(SLIST_EMPTY(&ctx->scope_chain));
    if (ctx->last_target == ctx->fun->i_next ||
            !bc_is_ret(ctx->fun->i_buf[ctx->fun->i_next - 1]))
        yy_emit_ins(ctx, BCINS_AD(BC_RET0, 0, 1));
    seq = yy_parser_pop_context(state);
    if (SLIST_EMPTY(&state->ctx_chain))
        return -1;  /* Exiting Script function */
    return expr_tag(T_PROTO) | seq;
}

static void bc_materialize_chain(struct yy_function *fun,
                                 uint16_t chain, uint16_t d_target) {
    for (uint16_t next; chain; chain = next) {
        BCIns *i = fun->i_buf + chain, dest = fun->i_next;
        assert(bc_op(*i) == 255), setbc_op(i, BC_JMP);
        next = bc_d(*i);
        if (bc_op(i[-1]) == BC_IST || bc_op(i[-1]) == BC_ISF) {
            if (bc_a(i[-1]) & V_EXPR_F_BIT_OP)  /* Zero test instead */
                i[-1] = BCINS_AD(BC_ISNEN ^ (bc_op(i[-1]) & 1),
                                 bc_d(i[-1]), yy_use_number(fun, 0));
            if (d_target && bc_a(i[-1]) & V_EXPR_F_LOGICAL)
                dest = d_target;
        }
        else if (d_target) dest = d_target;
        setbc_d(i, BCBIAS_J + dest - (chain + 1));
    }
}

static uint16_t
bc_append_to_chain(struct yy_function *fun, uint16_t chain, int negate) {
    BCIns *last_i = fun->i_buf + fun->i_next - 1;
    assert(bc_op(*last_i) == 255);
    if (negate)
        setbc_op(last_i - 1, bc_op(last_i[-1]) ^ 1);
    setbc_d(last_i, chain);
    return fun->i_next - 1;
}

static uint16_t
bc_concat_chain(struct yy_function *fun, uint16_t chain, uint16_t src) {
    if (!chain) return src;
    BCIns *i = fun->i_buf + chain;
    while(bc_d(*i)) i = fun->i_buf + bc_d(*i);
    setbc_d(i, src);
    return chain;
}

static int bc_chain_may_yield_bool(struct yy_function *fun, uint16_t chain)
{
    for (BCIns *i; chain; chain = bc_d(*i)) {
        i = fun->i_buf + chain;
        assert(bc_op(*i) == 255);
        if (bc_op(i[-1]) != BC_IST && bc_op(i[-1]) != BC_ISF ||
            bc_a(i[-1]) & V_EXPR_F_LOGICAL)
            return 1;
    }
    return 0;
}

expr_t yy_emit_call(yy_parse_state_t *state, int arg, int is_new)
{
    USE_CTX(ctx);
    int vararg = (arg >> 8) & 1;
    arg &= 255;

    update_frame_size(ctx);
    ctx->sp -= CALL_RSV + arg;
    yy_emit_ins(ctx, vararg ?
        BCINS_ABC(BC_CALLM, ctx->sp, 2, arg) :
        BCINS_ABC(BC_CALL, ctx->sp, 2, arg + 1));

    if (is_new) {
        yy_emit_ins(ctx, BCINS_AD(BC_ISEQP, ctx->sp, 0));
        yy_emit_jmp(state, 1);
        yy_emit_ins(ctx, BCINS_AD(BC_MOV, ctx->sp - 1, ctx->sp));
        ctx->sp--;
        return 0;
    }

    return V_EXPR_F_CALL;
}

static void yy_emit_safe_tset(yy_parse_state_t *state,
                              uint8_t v_pos, uint8_t t_pos, uint16_t id)
{
    USE_CTX(ctx);
    uint16_t id_local = yy_localize_id(state, id);
    if (id_local < 256)
        yy_emit_ins(ctx, BCINS_ABC(BC_TSETS, v_pos, t_pos, id_local));
    else {
        yy_emit_ins(ctx, BCINS_AD(BC_KSTR, v_pos + 1, id_local));
        yy_emit_ins(ctx, BCINS_ABC(BC_TSETV, v_pos, t_pos, v_pos + 1));
    }
}

int yy_emit_assignment(yy_parse_state_t *state, expr_t target, int keep)
{
    if (expr_type(target) != T_LV_REF || (uint32_t)target == 0xfff000u) {
        yy_parser_set_error(state, "Invalid left-hand side in assignment");
        return 0;
    }

    USE_CTX(ctx);
    uint8_t table_pos = target >> 16;
    uint16_t id = target;
    if (table_pos == 255) {
        if (id >= 0xf000 && (id & 0x100)) {
            yy_parser_set_error(state, "Assignment to constant variable");
            return 0;
        }
        yy_emit_ins(ctx, id >= 0xf000 ?
            BCINS_AD((id & 0x800) ? BC_USETV : BC_MOV, id & 0xff, ctx->sp) :
            BCINS_AD(BC_GSET, ctx->sp, yy_localize_id(state, id)));
        if (id == ctx->tag_id &&  /* workaround for ClassDeclaration */
                SLIST_FIRST(&ctx->scope_chain)->flags & YY_SCOPE_HAS_UV)
            yy_emit_ins(ctx, BCINS_AD(BC_UCLO, ctx->sp, BCBIAS_J));
    } else {
        int value_pos = ctx->sp;
        if (table_pos == 254) {
            ctx->sp = value_pos - (id == 0xffff);
            table_pos = ctx->sp - 1;
        } else {
            USE_SCOPE(ctx);
            if (table_pos >= scope->local_top)
                ctx->sp = table_pos;
            else ctx->sp = value_pos - (id == 0xffff);
        }

        if (id == 0xffff)
            yy_emit_ins(ctx, BCINS_ABC(
                    BC_TSETV, value_pos, table_pos, table_pos + 1));
        else yy_emit_safe_tset(state, value_pos, table_pos, id);
        if (keep)
            yy_emit_ins(ctx, BCINS_AD(BC_MOV, ctx->sp, value_pos));
    }

    if (!keep)
        ctx->sp--;
    return 1;  /* Done */
}

int yy_emit_array_field(yy_parse_state_t *state, int index, int multi)
{
    USE_CTX(ctx);
    if (!index && state->flags & YY_PARSER_ARR_BASE1) index++;
    if (multi) {
        uint64_t first = 0x4330000000000000ull | index;
        if (multi == 2) {
            yy_emit_ins(ctx, BCINS_ABC(
                    BC_VARG, ctx->sp + 1, 0, ctx->fun->argc));
            ctx->fun->flags |= YY_FUN_F_VARARG;
        }
        yy_emit_ins(ctx, BCINS_AD(
                BC_TSETM, ctx->sp + 1, yy_use_number(ctx->fun, first)));
    } else {
        if (ctx->last_target < ctx->fun->i_next &&
            *last_bc(ctx) == BCINS_AD(BC_KPRI, ctx->sp, 0))
            /* ignore null element */;
        else if (index < 256)
            yy_emit_ins(ctx, BCINS_ABC(
                    BC_TSETB, ctx->sp, ctx->sp - 1, index));
        else {
            yy_emit_ins(ctx, BCINS_AD(BC_KSHORT, ctx->sp + 1, index));
            yy_emit_ins(ctx, BCINS_ABC(
                    BC_TSETV, ctx->sp, ctx->sp - 1, ctx->sp + 1));
        }
        ctx->sp--;
    }
    return index + 1;
}

expr_t yy_expr_discharge(yy_parse_state_t *state, expr_t x, int keep_valid)
{
    USE_CTX(ctx);
    if (keep_valid && expr_type(x) != T_LV_REF) {
        yy_parser_set_error(state, "Invalid left-hand side in assignment");
        return -1;
    }

    assert(expr_type(x));
    if (expr_type(x) == T_INT && (int)x == (short)x)
        yy_emit_ins(ctx, BCINS_AD(BC_KSHORT, ++ctx->sp, (uint16_t)x));
    else if (expr_type(x) >= T_INT)
        yy_emit_ins(ctx, BCINS_AD(
                BC_KNUM, ++ctx->sp, yy_use_number(ctx->fun, ~x)));
    else if (expr_type(x) == T_STR)
        yy_emit_ins(ctx, BCINS_AD(BC_KSTR, ++ctx->sp, (uint16_t)x));
    else if (expr_type(x) == T_PROTO)
        yy_emit_ins(ctx, BCINS_AD(BC_FNEW, ++ctx->sp, (uint16_t)x));
    else if (expr_type(x) == T_PRI)
        yy_emit_ins(ctx, BCINS_AD(BC_KPRI, ++ctx->sp, x & 3));
    else {
        uint8_t table_pos = x >> 16;
        uint16_t id = x;
        if (table_pos == 255) {
            if (id >= 0xf000u) yy_emit_ins(ctx, BCINS_AD(
                    (id & 0x800) ? BC_UGET : BC_MOV, ++ctx->sp, id & 255));
            else yy_emit_ins(ctx, BCINS_AD(
                    BC_GGET, ++ctx->sp, yy_localize_id(state, id)));
        } else if (keep_valid) {
            ctx->sp++;
            yy_emit_ins(ctx, id == 0xffff ?
                BCINS_ABC(BC_TGETV, ctx->sp, table_pos, ctx->sp - 1) :
                BCINS_ABC(BC_TGETS, ctx->sp, table_pos, yy_localize_id(state, id)));
        } else {
            USE_SCOPE(ctx);
            if (id == 0xffff) {
                int key_pos = ctx->sp;
                if (table_pos >= scope->local_top)
                    ctx->sp = table_pos;  /* Overwrite table slot */
                BCIns *last_i = last_bc(ctx),
                    const_op = (ctx->last_target < ctx->fun->i_next &&
                            bc_a(*last_i) == key_pos) ? bc_op(*last_i) : 0;
                if (const_op == BC_KSHORT && !(bc_d(*last_i) >> 8)) *last_i =
                        BCINS_ABC(BC_TGETB, ctx->sp, table_pos, bc_d(*last_i));
                else if (const_op == BC_KSTR) *last_i =
                        BCINS_ABC(BC_TGETS, ctx->sp, table_pos, bc_d(*last_i));
                else if (const_op == BC_MOV) *last_i =
                        BCINS_ABC(BC_TGETV, ctx->sp, table_pos, bc_d(*last_i));
                else yy_emit_ins(ctx, BCINS_ABC(
                        BC_TGETV, ctx->sp, table_pos, key_pos));
            } else {
                if (table_pos < scope->local_top)
                    ctx->sp++;
                if (id == state->id_tostring) yy_emit_ins(ctx, BCINS_AD(
                        BC_GGET, ctx->sp, yy_symbol("tostring")));
                else if (id == state->id_length) yy_emit_ins(ctx, BCINS_AD(
                        BC_LEN, ctx->sp, table_pos));
                else yy_emit_ins(ctx, BCINS_ABC(
                        BC_TGETS, ctx->sp, table_pos, yy_localize_id(state, id)));
            }
        }
    }

    update_frame_size(ctx);
    return 0;
}

expr_t yy_emit_concat(yy_parse_state_t *state, int elements)
{
    USE_CTX(ctx);
    if (elements == 1)
        yy_expr_discharge(state, expr_tag(T_STR) | yy_symbol(""), 0);
    else elements -= 1;
    ctx->sp -= elements;
    yy_emit_ins(ctx, BCINS_ABC(BC_CAT, ctx->sp, ctx->sp, ctx->sp + elements));
    return 0;
}

int yy_emit_jmp(yy_parse_state_t *state, int pc)
{
    USE_CTX(ctx);
    if (pc == 0 && ctx->fun->i_next == 0)
        yy_emit_ins(ctx, BCINS_AD(BC_KPRI, ctx->sp + 1, 0));
    if (pc != 0)
        ctx->last_target = ctx->fun->i_next + 1 + pc;
    return yy_emit_ins(ctx, pc != 0 ?
        BCINS_AD(BC_JMP, ctx->sp + 1, BCBIAS_J + pc) :
        BCINS_AD(255, ctx->sp + 1, 0));  /* Placeholder */
}

void yy_fill_jmp_placeholder(yy_parse_state_t *state, uint32_t ji)
{
    USE_CTX(ctx);
    if (ji == DEAD_TAG) { ctx->flags &= ~YY_MASK_EMIT; return; }
    if (ji) ctx->last_target = ctx->fun->i_next;
    bc_materialize_chain(ctx->fun, ji, 0);
}

void yy_fill_table_size(yy_parse_state_t *state, uint32_t i,
                        int n_el, int n_field)
{
    USE_CTX(ctx);
    uint16_t h_siz = 0;
    if (n_field > 1) n_field--;
    while (n_field)  h_siz++, n_field >>= 1;
    if (n_el) {  /* Array part is needed */
        n_el += 1;  /* field 0 is in array part */
        if (n_el < 3) n_el = 3;
        else if (n_el > 1023) n_el = 0x3ff;
    }
    if (bc_op(ctx->fun->i_buf[i]) == BC_TNEW)
        setbc_d(ctx->fun->i_buf + i, h_siz << 11 | n_el);
}

void yy_bcg_open_scope(yy_parse_state_t *state, int type)
{
    USE_CTX(ctx);
    unsigned flags = 0;
    switch (type) {
        /* For function scopes, just push new context and return */
        case KEYWORD_TRY: case KEYWORD_WITH: flags |= YY_CONTEXT_TEMP;
        case P_ARROW: flags |= YY_CONTEXT_ARROW;
        case KEYWORD_FUNCTION: flags |= ctx->flags & YY_MASK_EMIT;
            yy_parser_push_context(state, flags);
            return;

        case KEYWORD_WHILE:
        case KEYWORD_FOR: flags |= YY_SCOPE_LOOP;  /* fallthrough */
        case KEYWORD_SWITCH: flags |= YY_SCOPE_BREAKABLE; break;
        case KEYWORD_CATCH:
            yy_emit_ins(ctx, BCINS_AD(BC_ISEQP, ++ctx->sp, 2));
            yy_emit_jmp(state, 0);
            flags |= YY_SCOPE_CATCH;
            break;

        case KEYWORD_FINALLY:
            ctx->sp += 2;  /* Preserve ok plus err */
            break;

        default: assert(!type);
    }

    struct yy_parse_scope *scope = yy_parser_push_scope(state, flags);
    if (type == KEYWORD_WHILE) {
        scope->iter_i[0] = BCINS_AD(BC_JMP, scope->base, 0);
        scope->n_iter_i = 1;  /* Prepare continuation */
    }
}

void yy_bcg_close_scope(yy_parse_state_t *state, uint16_t out_chain)
{
    USE_CTX(ctx);
    USE_SCOPE(ctx);
    assert(scope);

    if (scope->flags & (YY_SCOPE_BREAKABLE | YY_SCOPE_LOOP)) {
        if (scope->n_iter_i) {
            BCIns *last_i = &scope->iter_i[scope->n_iter_i - 1];
            if (bc_op(*last_i) == BC_ITERL || bc_op(*last_i) == BC_JMP)
                setbc_d(last_i, BCBIAS_J + scope->cont_pc -
                        (ctx->fun->i_next + scope->n_iter_i));
            scope->cont_pc = ctx->fun->i_next;
            for (int i = 0; i < scope->n_iter_i; i++)
                yy_emit_ins(ctx, scope->iter_i[i]);
        }

        for (int i = scope->start_pc; i < ctx->fun->i_next; i++) {
            BCIns *ins = &ctx->fun->i_buf[i];
            if (bc_op(*ins) != BC_JMP)
                continue;
            int val = bc_d(*ins) & 15;
            /* Fill pending JMP of breaks */
            if (bc_d(*ins) >> 4 == 0xfff)
                setbc_d(ins, val == 15 ?
                     0x7fff + ctx->fun->i_next - i : (0xfff0 | val + 1));
            /* Fill pending JMP of continues */
            else if (bc_d(*ins) >> 4 == 0)
                setbc_d(ins, val == 1 ?
                    0x7fff + scope->cont_pc - i : val - 1);
        }

        ctx->last_target = ctx->fun->i_next;
    }

    if (!out_chain && scope->fall_chain)
        out_chain = scope->fall_chain;

    if (out_chain == DEAD_TAG) ctx->flags &= ~YY_MASK_EMIT;
    else if (out_chain)
        bc_materialize_chain(ctx->fun, out_chain, 0);

    if (scope->flags & YY_SCOPE_HAS_UV) {
        int base = !SLIST_NEXT(scope, s_entry) ? 0 : scope->base;
        if (ctx->last_target < ctx->fun->i_next) {
            BCIns *tail = last_bc(ctx), last_op = bc_op(*tail);
            if (last_op == BC_JMP)
                setbc_op(tail, BC_UCLO), setbc_a(tail, base);
            else if (last_op == BC_UCLO && bc_d(*tail) == BCBIAS_J)
                setbc_a(tail, base);
            else if (!bc_is_ret(*tail))
                goto add_uclo;
        } else { add_uclo:
            yy_emit_ins(ctx, BCINS_AD(BC_UCLO, base, BCBIAS_J));
        }
    }

    if (scope->flags & YY_SCOPE_CATCH && scope->start_pc) {
        yy_emit_ins(ctx, BCINS_AD(BC_KPRI, scope->base, 0));
        yy_fill_jmp_placeholder(state, scope->start_pc - 1);
    }

    yy_parser_pop_scope(state);
}

static void yy_emit_return(yy_parse_state_t *state, int nr)
{
    USE_CTX(ctx);
    BCIns tail = nr == 2 ? ctx->fun->i_buf[--ctx->fun->i_next] : 0;
    if (tail) {
        assert(bc_op(tail) == BC_CALL || bc_op(tail) == BC_CALLM);
        setbc_op(&tail, bc_op(tail) + 2);  /* Convert to tail call */
        setbc_b(&tail, 0);
        ctx->sp--;
    }

    struct yy_parse_scope *uv_scope;
    SLIST_FOREACH(uv_scope, &ctx->scope_chain, s_entry)
        if (uv_scope->flags & YY_SCOPE_HAS_UV)
            break;
    if (uv_scope)
        yy_emit_ins(ctx, BCINS_AD(BC_UCLO, 0, BCBIAS_J));
    yy_emit_ins(ctx, tail ? tail : (nr ?
        BCINS_AD(BC_RET1, ctx->sp--, 2) :
        BCINS_AD(BC_RET0, 0, 1)));
}

static void yy_emit_check_ret(yy_parse_state_t *state)
{
    USE_CTX(ctx);
    uint16_t skip_jmp = (
            yy_emit_ins(ctx, BCINS_AD(BC_ISEQP, ctx->sp, 0)),
            yy_emit_jmp(state, 0));
    yy_emit_return(state, 1);
    yy_fill_jmp_placeholder(state, skip_jmp);
    yy_ctx_reset_stack(ctx);
}

void yy_emit_post_try(yy_parse_state_t *state, int caught)
{
    USE_CTX(ctx);
    ctx->sp += 2;  /* take ok plus err */
    if (!caught) {
        yy_emit_ins(ctx, BCINS_AD(BC_IST, 0, ctx->sp - 1));
        yy_emit_jmp(state, 2 + CALL_RSV);
        yy_emit_ins(ctx, BCINS_AD(
                BC_GGET, ctx->sp - 1, yy_symbol("error")));
        if (CALL_RSV)
            yy_emit_ins(ctx, BCINS_AD(
                    BC_MOV, ctx->sp + CALL_RSV, ctx->sp));
        yy_emit_ins(ctx, BCINS_ABC(BC_CALL, ctx->sp - 1, 2, 2));
    }

    yy_emit_check_ret(state);
}

int yy_emit_loop_ctl(yy_parse_state_t *state, int fc_type, int label)
{
    USE_CTX(ctx);
    USE_SCOPE(ctx);
    switch (fc_type) {  // NOLINT(*-multiway-paths-covered)
    case '?': scope->cont_pc = ctx->fun->i_next; return 1;
    case ')': {
        uint16_t i_top = ctx->fun->i_next;
        while (ctx->fun->i_next > scope->cont_pc &&
                bc_op(*last_bc(ctx)) != 255)
            ctx->fun->i_next--;
        scope->n_iter_i = i_top - ctx->fun->i_next;
        if (scope->n_iter_i >= sizeof(scope->iter_i) / 4) {
            yy_parser_set_error(state, "update expression too complex");
            return 0;
        }
        if (scope->n_iter_i)
            memcpy(scope->iter_i, ctx->fun->i_buf + ctx->fun->i_next,
                   scope->n_iter_i * sizeof(uint32_t));
        scope->iter_i[scope->n_iter_i++] =
                BCINS_AD(BC_JMP, scope->base + 1, 0);
        return 1;
    }
    case YYEOF:  /* Return whether an EOF return is applicable */
        return !SLIST_NEXT(scope, s_entry);
    case KEYWORD_CASE:  /* finish a case clause */
        if (!(ctx->flags & YY_MASK_EMIT) &&
            (ctx->last_target == ctx->fun->i_next ||
              bc_op(*last_bc(ctx)) != BC_JMP &&!bc_is_ret(*last_bc(ctx))))
            scope->fall_chain = (yy_emit_jmp(state, 0),
                    bc_append_to_chain(ctx->fun, scope->fall_chain, 0));
        yy_fill_jmp_placeholder(state, label);
        return 1;
    case ':':  /* Fallthrough to here */
        yy_fill_jmp_placeholder(state, scope->fall_chain);
        scope->fall_chain = 0;
        return 1;
    }

    int level = 0;
    SLIST_FOREACH(scope, &ctx->scope_chain, s_entry) {
        if (fc_type == KEYWORD_CONTINUE && !(scope->flags & YY_SCOPE_LOOP))
            goto not_applicable;

        if ((label && scope->label == label) ||
            (!label && scope->flags & YY_SCOPE_BREAKABLE))
            break;

    not_applicable:
        if (scope->flags & (YY_SCOPE_BREAKABLE | YY_SCOPE_LOOP))
            level++;
    }
    assert(level < 15);
    if (!scope) {
        if (label) yy_parser_set_error(state, "Undefined label");
        else if (fc_type == KEYWORD_CONTINUE)
            yy_parser_set_error(state, "continue is not allowed here");
        else if (fc_type == KEYWORD_BREAK)
            yy_parser_set_error(state, "break is not allowed here");
        return 0;
    }

    switch (fc_type) {
        case KEYWORD_CONTINUE:
            yy_emit_ins(ctx, BCINS_AD(BC_JMP, ctx->sp + 1, 1 + level));
            break;
        case KEYWORD_BREAK:
            yy_emit_ins(ctx, BCINS_AD(BC_JMP, ctx->sp + 1, 0xffffu - level));
            break;
        default: assert(0);
    }

    return 1;
}

static expr_t yy_expr_materialize(yy_parse_state_t *state, expr_t x, int bit)
{
    if (expr_type(x)) {
        if (bit == -1 && expr_type(x) > T_LV_REF)
            return ++yy_current_ctx(state)->sp, 0;
        return yy_expr_discharge(state, x, 0);
    }

    USE_CTX(ctx);
    uint16_t tsc = x >> 16, fsc = x >> 32;
    if (x & V_EXPR_F_LOGICAL && !(ctx->flags & YY_MASK_EMIT))
        tsc = bc_append_to_chain(ctx->fun, tsc, 0);

    if (tsc || fsc) {
        if (bit < 0) {
            bc_materialize_chain(ctx->fun, fsc, 0);
            bc_materialize_chain(ctx->fun, tsc, 0);
            return 0;
        }

        uint16_t vt = 0, vf = 0;
        if (bc_chain_may_yield_bool(ctx->fun, tsc) ||
            bc_chain_may_yield_bool(ctx->fun, fsc)) {
            if (!(x & V_EXPR_F_LOGICAL))
                yy_emit_jmp(state, 3);
            vf = yy_emit_ins(ctx, bit ?
                BCINS_AD(BC_KSHORT, ctx->sp, 0) :
                BCINS_AD(BC_KPRI, ctx->sp, 1));
            yy_emit_jmp(state, 1);
            vt = yy_emit_ins(ctx, bit ?
                BCINS_AD(BC_KSHORT, ctx->sp, 1) :
                BCINS_AD(BC_KPRI, ctx->sp, 2));
        }
        bc_materialize_chain(ctx->fun, fsc, vf);
        bc_materialize_chain(ctx->fun, tsc, vt);
        return 0;
    }

    return x;
}

expr_t yy_merge_short_chains(yy_parse_state_t *state, expr_t x1, expr_t x2)
{
    USE_CTX(ctx);
    if (!x1) return x2;
    else if (expr_type(x2))
        x2 = yy_expr_discharge(state, x2, 0);

    uint16_t tsc = x2 >> 16, fsc = x2 >> 32;
    tsc = bc_concat_chain(ctx->fun, tsc, x1 >> 16);
    fsc = bc_concat_chain(ctx->fun, fsc, x1 >> 32);
    return ((long long)tsc << 16) | ((long long)fsc << 32) |
        x2 & (V_EXPR_F_LOGICAL | V_EXPR_F_BIT_OP);
}

static expr_t
yy_emit_binary_operator(yy_parse_state_t *state, BCIns ins, expr_t x)
{
    USE_CTX(ctx);
    unsigned rhs_operand = -1;
    if (expr_type(x) == T_LV_REF && (x & 0xfff800u) == 0xfff000u)
        rhs_operand = x & 255;  /* Local variable */
    else if (expr_type(x) >= T_INT) {  /* T_INT or double */
        switch (ins) {
            case BC_ADDVV: case BC_SUBVV:
            case BC_MULVV: case BC_DIVVV: case BC_MODVV:
                ins -= (BC_ADDVV - BC_ADDVN); break;
            case BC_SUBNV: case BC_DIVNV: case BC_MODNV: break;
            case BC_ISEQV: case BC_ISNEV:
                ins += (BC_ISEQN - BC_ISEQV); break;
            default: goto rhs_discharge;
        }
        rhs_operand = yy_use_number(ctx->fun, ~x);
    }
    else if (ins == BC_ISEQV || ins == BC_ISNEV) {
        if (expr_type(x) == T_STR)
            ins += (BC_ISEQS - BC_ISEQV), rhs_operand = x;
        else if (expr_type(x) == T_PRI)
            ins += (BC_ISEQP - BC_ISEQV), rhs_operand = x & 3;
    }

    /* If right operand is not embedded in ins */
    if (rhs_operand == -1u) {
        rhs_discharge: yy_expr_materialize(state, x, !bc_is_cmp(ins));
        rhs_operand = ctx->sp--;
    }
    yy_emit_ins(ctx, bc_is_cmp(ins) ?
            BCINS_AD(ins, ctx->sp, rhs_operand) :
            BCINS_ABC(ins, ctx->sp, ctx->sp, rhs_operand));
    return bc_is_cmp(ins) && (yy_emit_jmp(state, 0), V_EXPR_F_LOGICAL);
}

static expr_t yy_emit_increment(yy_parse_state_t *state, BCIns op, expr_t x)
{
    USE_CTX(ctx);
    if (expr_type(x) == T_LV_REF)
        yy_expr_discharge(state, x, 1);
    else {
        yy_parser_set_error
            (state, "Invalid left-hand side in postfix operation");
        return -1;
    }

    int new_val = ++ctx->sp;
    yy_emit_ins(ctx, BCINS_ABC(
            op, new_val, ctx->sp - 1,
            yy_use_number(ctx->fun, 0x3ffull << 52)));
    if (!yy_emit_assignment(state, x, 1))
        return -1;
    if (ctx->sp == new_val)
        ctx->sp--;
    /* Table assignment moves value to the table slot */
    else setbc_d(last_bc(ctx), new_val - 1);
    return 0;
}

static void bc_chain_drop_val(struct yy_function *fun, uint16_t chain)
{
    for (BCIns *i; chain; chain = bc_d(*i)) {
        i = fun->i_buf + chain;
        if (bc_op(i[-1]) == BC_IST || bc_op(i[-1]) == BC_ISF)
            setbc_a(i - 1, bc_a(i[-1]) | V_EXPR_F_LOGICAL);
    }
}

static expr_t yy_emit_logical(yy_parse_state_t *state, int opr, expr_t x)
{
    USE_CTX(ctx);
    if (opr == '?' && expr_type(x) == T_PRI && !(x & 2)) {
        ctx->flags |= YY_MASK_EMIT;
        return (expr_t)DEAD_TAG << 32;
    }
    else if (expr_type(x))
        x = yy_expr_discharge(state, x, 0);

    uint16_t tsc = x >> 16, fsc = x >> 32;
    if (opr == '!') {
        if (x & V_EXPR_F_LOGICAL) {
            BCIns *cmp = last_bc(ctx) - 1;
            setbc_op(cmp, bc_op(*cmp) ^ 1);  /* Negate cmp */
        }
        else if (x & V_EXPR_F_BIT_OP) yy_emit_operator(state, P_EQ, ~0);
        else {
            yy_emit_ins(ctx, BCINS_AD(BC_ISF, V_EXPR_F_LOGICAL, ctx->sp));
            yy_emit_jmp(state, 0);
        }
        bc_chain_drop_val(ctx->fun, fsc);
        bc_chain_drop_val(ctx->fun, tsc);
        /* Return with tsc and fsc swapped */
        return V_EXPR_F_LOGICAL | (expr_t)fsc << 16 | (expr_t)tsc << 32;
    }

    if (opr == P_OR) {
        if (x & V_EXPR_F_LOGICAL) {
            tsc = bc_append_to_chain(ctx->fun, tsc, 0);
        } else {
            yy_emit_ins(ctx, BCINS_AD(BC_IST, x & V_EXPR_F_BIT_OP, ctx->sp));
            tsc = yy_emit_ins(ctx, BCINS_AD(255, ctx->sp + 1, tsc));
        }
        bc_materialize_chain(ctx->fun, fsc, 0);
        fsc = 0;
    } else {  /* P_AND & ? */
        if (x & V_EXPR_F_LOGICAL) {
            fsc = bc_append_to_chain(ctx->fun, fsc, 1);
        } else {
            yy_emit_ins(ctx, BCINS_AD(BC_ISF, x & V_EXPR_F_BIT_OP, ctx->sp));
            fsc = yy_emit_ins(ctx, BCINS_AD(255, ctx->sp + 1, fsc));
        }
        bc_materialize_chain(ctx->fun, tsc, 0);
        tsc = 0;
    }

    ctx->sp--;
    return (expr_t)tsc << 16 | (expr_t)fsc << 32;
}

static void
yy_emit_lib_call(yy_parse_state_t *state,
                 const char *name, const char *field, int bin, expr_t x)
{
    USE_CTX(ctx);
    if (!expr_type(x)) x = yy_expr_materialize(state, x, *name == 'b');
    else if (expr_type(x) == T_LV_REF && (uint8_t)~(x >> 16) &&
             (uint32_t)x >= 0xffffu /* Not this.x */)
        x = yy_expr_discharge(state, x, 0);  /* Have temp val */

    if (bin) yy_emit_ins(ctx, BCINS_AD
            (BC_MOV, ctx->sp + CALL_RSV + 1 + (bin < 0), ctx->sp--));
    if (!expr_type(x)) yy_emit_ins(ctx, BCINS_AD
            (BC_MOV, ctx->sp + CALL_RSV + 1, ctx->sp--));
    yy_emit_ins(ctx, BCINS_AD(BC_GGET, ++ctx->sp, yy_symbol(name)));
    if (field) yy_emit_ins(ctx, BCINS_ABC
            (BC_TGETS, ctx->sp, ctx->sp, yy_symbol(field)));
    ctx->sp += CALL_RSV + (bin > 0);
    expr_type(x) ? yy_expr_discharge(state, x, 0) : ++ctx->sp;
    ctx->sp += (bin < 0);
    yy_emit_call(state, 2 - !bin, 0);
}

void yy_emit_bit_operator(yy_parse_state_t *state, int opr, expr_t x)
{
    USE_CTX(ctx);
    if (state->flags & YY_PARSER_ARR_BASE1) {  /* stock VM */
        switch (opr) {  // NOLINT(*-multiway-paths-covered)
        case '|': yy_emit_lib_call(state, "bit", "bor", 1, x); break;
        case '&': yy_emit_lib_call(state, "bit", "band", 1, x); break;
        case '^': yy_emit_lib_call(state, "bit", "bxor", 1, x); break;
        case '~': yy_emit_lib_call(state, "bit", "bnot", 0, x); break;
        }
    } else {  /* Universal bit manipulation */
        int shift_amount = 0;
        if (expr_type(x) == T_INT && opr == '&') {  /* fuse BFX */
            BCIns last_i = *last_bc(ctx);
            if (bc_op(last_i) == BC_UNM && bc_b(last_i) >> 5 == 3)
                shift_amount = bc_b(last_i) & 31, ctx->fun->i_next--;
        }
        yy_expr_materialize(state, x, 1);
        switch (opr) {  // NOLINT(*-multiway-paths-covered)
        case '&': opr = 5; break;
        case '|': opr = 6; break;
        case '~': yy_expr_discharge(state, expr_int(-1u), 0);
        case '^': opr = 7; break;
        }
        yy_emit_ins(ctx, BCINS_ABC
                (BC_UNM, ctx->sp - 1, opr << 5 | shift_amount, ctx->sp--));
    }
}

#define yy_emit_bit_binary_operator(name) \
    yy_emit_lib_call(state, "bit", #name, opr > 0 ? 1 : -1, x); \
    return V_EXPR_F_BIT_OP

expr_t yy_emit_operator(yy_parse_state_t *state, int opr, expr_t x)
{
    USE_CTX(ctx);
    if (opr == KEYWORD_YIELD || opr == KEYWORD_AWAIT || opr == KEYWORD_NEW ||
        opr == KEYWORD_EXTENDS || opr == KEYWORD_IN || opr == '(' || !opr)
        yy_expr_materialize(state, x, 0);

    if (ctx->flags & YY_MASK_EMIT) {
        if (opr == P_AND || opr == P_OR || opr == '?' ||
            opr == KEYWORD_CASE || opr == P_CASE) opr = ',';
        else if (opr == KEYWORD_WHILE || opr == P_OPT) return 0;
        else if (opr == '!') return x;
    }

    int base = ctx->sp;
    switch (opr) {
        case '+': return yy_emit_binary_operator(state, BC_ADDVV, x);
        case '-': return yy_emit_binary_operator(state, BC_SUBVV, x);
        case '*': return yy_emit_binary_operator(state, BC_MULVV, x);
        case '/': return yy_emit_binary_operator(state, BC_DIVVV, x);
        case '%': return yy_emit_binary_operator(state, BC_MODVV, x);
        case -'-': return yy_emit_binary_operator(state, BC_SUBNV, x);
        case -'/': return yy_emit_binary_operator(state, BC_DIVNV, x);
        case -'%': return yy_emit_binary_operator(state, BC_MODNV, x);
        case P_POW: case -P_POW:
            yy_emit_binary_operator(state, BC_POW, x);
            if (opr < 0 && !(ctx->flags & YY_MASK_EMIT))
                setbc_b(last_bc(ctx), ctx->sp + 1),
                setbc_c(last_bc(ctx), ctx->sp);
            break;
        case '&': case '^': case '|': case '~':
            return yy_emit_bit_operator(state, opr, x), V_EXPR_F_BIT_OP;
        case P_SHL: if (!(state->flags & YY_PARSER_ARR_BASE1))
            if (expr_type(x) == T_INT) return yy_emit_ins(ctx, BCINS_ABC
                (BC_UNM, ctx->sp, 2<<5 | x & 31, ctx->sp)), V_EXPR_F_BIT_OP;
        case -P_SHL: /* else */ yy_emit_bit_binary_operator(lshift);
        case P_SHR: if (!(state->flags & YY_PARSER_ARR_BASE1))
            if (expr_type(x) == T_INT) return yy_emit_ins(ctx, BCINS_ABC
                (BC_UNM, ctx->sp, 3<<5 | x & 31, ctx->sp)), V_EXPR_F_BIT_OP;
        case -P_SHR: /* else */ yy_emit_bit_binary_operator(rshift);
        case P_ASHR: case -P_ASHR: yy_emit_bit_binary_operator(arshift);

        case P_EQ: return yy_emit_binary_operator(state, BC_ISEQV, x);
        case P_NE: return yy_emit_binary_operator(state, BC_ISNEV, x);
        case '<': return yy_emit_binary_operator(state, BC_ISLT, x);
        case P_LE: return yy_emit_binary_operator(state, BC_ISLE, x);
        case '>': return yy_emit_binary_operator(state, BC_ISGT, x);
        case P_GE: return yy_emit_binary_operator(state, BC_ISGE, x);

        case P_INC: return yy_emit_increment(state, BC_ADDVN, x);
        case P_DEC: return yy_emit_increment(state, BC_SUBVN, x);
        case P_UNM: yy_emit_ins(ctx, BCINS_AD(BC_UNM, ctx->sp, ctx->sp)); break;
        case '#': return yy_expr_materialize(state, x, 1);
        case P_OR: case P_AND: case '?': case '!':
            return yy_emit_logical(state, opr, x);

        case P_OPT:  /* OptionalChain */
            x = yy_emit_logical(state, P_OR, yy_emit_binary_operator(
                    state, BC_ISEQV, expr_tag(T_PRI)));
            ctx->sp++;
            return x;

        case '(':
            ctx->sp += CALL_RSV;
            if (expr_type(x) == T_LV_REF) {
                if (x & LV_EXPR_F_RAW || ctx->flags & YY_MASK_EMIT)
                    return 0;  /* Raw functions get no `this` */
                uint8_t table_pos = x >> 16;
                if (table_pos != 255) {
                    BCIns *last_i = last_bc(ctx);
                    yy_emit_ins(ctx, *last_i);
                    *last_i = BCINS_AD(BC_MOV, ++ctx->sp, table_pos);
                    return 1;
                } else if ((x & 0xffff) == state->id_super) {
                    yy_emit_ins(ctx, BCINS_AD(BC_MOV, ++ctx->sp, 0));
                    return 1;
                }
            }
            yy_expr_discharge(state, expr_tag(T_PRI), 0);
            return 1;

        case KEYWORD_NEW:
            ctx->sp += CALL_RSV;
            yy_emit_ins(ctx, BCINS_AD(BC_MOV, ++ctx->sp, base - 1));
            break;

        case KEYWORD_RETURN:
            if ((ctx->flags & YY_CONTEXT_TEMP) && x == expr_tag(T_PRI)) {
                yy_parser_set_error(state, "return not allowed in with/try");
                return -1;
            }

            if (x == expr_tag(T_PRI)) yy_emit_return(state, 0);
            else if (x == V_EXPR_F_CALL) yy_emit_return(state, 2);
            else yy_expr_materialize(state, x, 0), yy_emit_return(state, 1);
            break;

        case KEYWORD_DELETE:
            if (expr_type(x) == T_LV_REF) {
                yy_expr_discharge(state, expr_tag(T_PRI), 0);
                yy_emit_assignment(state, x, 0);
            }
            else if (!expr_type(x))
                ctx->sp--;  /* Pop computed value */
            return expr_tag(T_PRI) | 2;

        case KEYWORD_VOID:
            yy_expr_materialize(state, x, -1);
            yy_emit_ins(ctx, BCINS_AD(BC_KPRI, ctx->sp, 0));
            break;

        case KEYWORD_TYPEOF:
            yy_emit_lib_call(state, "type", NULL, 0, x);
            break;

        case KEYWORD_THROW:
            yy_emit_lib_call(state, "error", NULL, 0, x);
            ctx->sp--;  /* throw is a statement */
            break;

        case KEYWORD_INSTANCEOF:
            yy_emit_lib_call(state, "jsrt", "isinstance", 1, x);
            break;

        case KEYWORD_IN:
            yy_emit_ins(ctx, BCINS_ABC(
                    BC_TGETV, ctx->sp, ctx->sp, ctx->sp - 1));
            yy_emit_ins(ctx, BCINS_AD(BC_ISNEP, ctx->sp--, 0));
            yy_emit_jmp(state, 0);
            return 1;

        case TOK_REGEXP:
            yy_emit_ins(ctx, BCINS_AD(
                    BC_GGET, ++ctx->sp, yy_symbol("RegExp")));
            ctx->sp += CALL_RSV;
            yy_expr_discharge(state, x, 0);
            return yy_emit_call(state, 1, 0);

        case P_ELISION:
            yy_emit_lib_call(state, "jsrt", "spread", 0, x);
            setbc_b(last_bc(ctx), 0), ctx->sp--;  /* Need multiret */
            return V_EXPR_F_CALL;

        case KEYWORD_STATIC:
            if (!expr_type(x))  /* Method declaration */
                setbc_b(last_bc(ctx), ctx->sp - 1);  /* Mod last TSET */
            else if (expr_type(x) == T_LV_REF) {
                uint8_t val_pos = ctx->sp--;
                yy_emit_safe_tset(state, val_pos, ctx->sp - 1, x);
            } else {  /* Call static initializer block */
                yy_expr_discharge(state, x, 0);
                yy_emit_ins(ctx, BCINS_ABC(
                        BC_TGETB, ctx->sp + CALL_RSV + 1, ctx->sp - 1, 1));
                yy_emit_ins(ctx, BCINS_ABC(BC_CALL, ctx->sp--, 1, 2));
            }
            break;

        case KEYWORD_EXTENDS:
            yy_emit_ins(ctx, BCINS_ABC(BC_TSETB, ctx->sp, ctx->sp - 1, 0));
            ctx->sp--;
            break;

        case KEYWORD_AWAIT:  if (!(state->flags & YY_PARSER_ARR_BASE1))
            if (x == V_EXPR_F_CALL) {  /* may unwrap async function? */
                BCIns *last_i = last_bc(ctx), call_i = *last_i;
                *last_i = BCINS_AD(BC_ISNEXT, bc_a(call_i), 0);
                yy_emit_ins(ctx, call_i);
            }  /* fallthrough */
        case KEYWORD_YIELD:
            ctx->sp -= CALL_RSV + 1;
            yy_emit_ins(ctx, BCINS_ABC(BC_CALL, ctx->sp, 3, 2));
            yy_emit_ins(ctx, BCINS_AD(BC_ISNEP, ctx->sp, 1));
            yy_emit_jmp(state, 3);
            yy_emit_ins(ctx, BCINS_AD(
                    BC_GGET, ctx->sp, yy_symbol("error")));
            yy_emit_ins(ctx, BCINS_AD(
                    BC_MOV, ctx->sp + CALL_RSV + 1, ctx->sp + 1));
            yy_emit_ins(ctx, BCINS_ABC(BC_CALL, ctx->sp, 1, 2));
            break;

        case P_GENERATOR:
            yy_emit_lib_call(state, "jsrt", "generator", 0, x);
            return V_EXPR_F_CALL;

        case KEYWORD_ASYNC:
            yy_emit_lib_call(state, "jsrt", "async", 0, x);
            return V_EXPR_F_CALL;

        case ',': yy_expr_materialize(state, x, -1);
        case ':': ctx->sp--;
            break;

        case ';':
            yy_expr_materialize(state, x, -1);
            yy_ctx_reset_stack(ctx);
            break;

        case '{':  /* Create a table */
            return yy_emit_ins(ctx, BCINS_AD(BC_TNEW, ++ctx->sp, 0));

        case '}':  /* Object destructuring initialization */
            for (struct yy_local_var *local = SLIST_FIRST(&ctx->local_chain);
                 x--; local = SLIST_NEXT(local, s_entry)) {
                USE_SCOPE(ctx);
                int field_id = yy_localize_id(state, local->id);
                yy_emit_ins(ctx, BCINS_ABC
                    (BC_TGETS, local->expr_val & 255, ctx->sp, field_id));
            }
            ctx->sp--;
            break;

        case KEYWORD_FOR: {
            if (x & V_EXPR_F_CALL) setbc_b(last_bc(ctx), 4);
            else {  /* next plus var */
                yy_expr_discharge(state, expr_tag(T_PRI), 0);
                ctx->sp -= 2;
            }

            USE_SCOPE(ctx);
            ctx->sp += 3;
            scope->local_top = scope->base = ctx->sp;
            scope->n_iter_i = 2;
            scope->iter_i[0] = BCINS_ABC(BC_ITERC, scope->base, 3, 3);
            scope->iter_i[1] = BCINS_AD(BC_ITERL, scope->base, 0);
            yy_emit_loop_ctl(state, KEYWORD_CONTINUE, 0);
            scope->cont_pc = ctx->fun->i_next;
            if (x & V_EXPR_F_CALL)  /* for-of loop? */
                yy_emit_ins(ctx, BCINS_AD(BC_MOV, ctx->sp, ctx->sp + 1));
            break;
        }

        case KEYWORD_WHILE: {
            USE_SCOPE(ctx);
            uint16_t chain = yy_emit_logical(state, P_OR, x) >> 16;
            if (chain) {
                ctx->fun->i_next = scope->start_pc;
                bc_materialize_chain(ctx->fun, chain, 0);
                ctx->fun->i_next = chain + 1;
            }
            scope->n_iter_i = 0;
            break;
        }

        case KEYWORD_CASE: case P_CASE: {
            USE_SCOPE(ctx);
            yy_emit_binary_operator(state, BC_ISNEV, x);
            setbc_a(last_bc(ctx) - 1, scope->base - 1);
            if (opr == P_CASE)  /* empty case clause */
                scope->fall_chain = bc_append_to_chain(
                        ctx->fun, scope->fall_chain, 1);
            else if (scope->fall_chain)
                yy_emit_loop_ctl(state, ':', 0);
            return ctx->fun->i_next - 1;
        }

        case KEYWORD_WITH:
            yy_emit_ins(ctx, BCINS_AD(
                    BC_GGET, ++ctx->sp, yy_symbol("setfenv")));
            ctx->sp += CALL_RSV;
            yy_expr_discharge(state, x, 0);
            yy_emit_ins(ctx, BCINS_AD(BC_MOV, ++ctx->sp, base));
            yy_emit_call(state, 2, 0);
            ctx->sp += CALL_RSV, yy_emit_call(state, 0, 0);
            yy_emit_check_ret(state);
            break;

        case KEYWORD_TRY:
            yy_emit_ins(ctx, BCINS_AD(BC_GGET, ++ctx->sp, yy_symbol("pcall")));
            ctx->sp += CALL_RSV;
            yy_expr_discharge(state, x, 0);
            yy_emit_ins(ctx, BCINS_ABC(BC_CALL, base + 1, 3, 2));
            yy_ctx_reset_stack(ctx);
            break;

        case 0: break;
        default: assert(0);
    }

    return 0;
}

static void yy_dump_append_uleb128(struct yy_dump_state *s, uint64_t w)
{
    uint8_t buf[10], *p = buf;
    for (; w >> 7; w >>= 7) *(p++) = w & 0x7fu | 0x80u;
    *(p++) = w;
    yy_dump_append_bytes(s, buf, p - buf);
}

static void
yy_dump_debug(struct yy_function *fun, struct yy_intern_state *id_intern,
              struct yy_dump_state *s)
{
    int use_short = fun->line_last - fun->line_start > 255,
        current_line = fun->line_start;
    struct yy_line_mark *next_line = fun->lines;
    for (int i = 0; i < fun->i_next; i++) {
        if (next_line && i >= next_line->ins) {
            current_line = next_line->line, next_line++;
            if (next_line == fun->lines + fun->l_next)
                next_line = NULL;
        }

        uint16_t line = current_line - fun->line_start;
        if (use_short) yy_dump_append_bytes(s, &line, sizeof(line));
        else yy_dump_append_byte(s, line);
    }

    /* Upvalue names */
    struct yy_upvalue *upvalues = yy_get_sealed_uv(fun);
    for (int i = 0; i < fun->upc; i++) {
        struct yy_kgc_value *it;
        SLIST_FOREACH(it, &id_intern->kgc_list, q_entry)
            if (it->seq == upvalues[i].id) {
                yy_dump_append_bytes(s, it->str, it->len);
                break;
            }
        yy_dump_append_byte(s, 0);
    }

    yy_dump_append_byte(s, 0);  /* No debug support for locals */
}

static void
yy_dump_proto(struct yy_function *fun, size_t dbg_siz, struct yy_dump_state *s)
{
    yy_dump_append_byte(s, fun->flags & 3);  /* Flags */
    yy_dump_append_byte(s, fun->argc);  /* Num of params */
    yy_dump_append_byte(s, fun->frame_size + 2);  /* Frame size */
    yy_dump_append_byte(s, fun->upc);  /* Upvalue num */
    yy_dump_append_uleb128(s, fun->intern->kgc_next);
    yy_dump_append_uleb128(s, fun->n_next);  /* Num of num consts */

    BCIns pre_i = 0;
    if (yy_fun_this_uv(fun))
        pre_i = BCINS_AD(BC_UGET, 0, yy_fun_this_uv(fun) - 1);
    yy_dump_append_uleb128(s, fun->i_next + (pre_i != 0));
    if (dbg_siz) {
        yy_dump_append_uleb128(s, dbg_siz);
        yy_dump_append_uleb128(s, fun->line_start);
        yy_dump_append_uleb128(s, fun->line_last - fun->line_start);
    }

    /* Append instructions */
    if (pre_i) yy_dump_append_bytes(s, &pre_i, sizeof(BCIns));
    yy_dump_append_bytes(s, fun->i_buf, fun->i_next * sizeof(BCIns));

    struct yy_upvalue *upvalues = yy_get_sealed_uv(fun);
    for (int i = 0; i < fun->upc; i++) {
        uint16_t uv = upvalues[i].uv & 0x80ffu;  /* Drop internal flags */
        if (uv >> 15 && upvalues[i].uv & YY_VAR_HINT_CONST)
            uv |= 0x4000u;  /* PROTO_UV_IMMUTABLE */
        yy_dump_append_bytes(s, &uv, sizeof(uint16_t));
    }

    struct yy_kgc_value *kgc;
    SLIST_FOREACH(kgc, &fun->intern->kgc_list, q_entry) {
        switch (kgc->type) {
            case KgcString:
                yy_dump_append_uleb128(s, 5 + kgc->len);
                yy_dump_append_bytes(s, kgc->str, kgc->len);
                break;
            case KgcFunction: yy_dump_append_byte(s, 0); break;
        }
    }

    for (int i = 0; i < fun->n_next; i++) {
        uint64_t num = fun->numbers[i];
        if (expr_type(~num) == T_INT)
            yy_dump_append_uleb128(s, (~num & 0xffffffffull) << 1);
        else {
            yy_dump_append_uleb128(s, (num & UINT32_MAX) << 1 | 1u);
            yy_dump_append_uleb128(s, num >> 32);
        }
    }
}

#define dump_size(dmp) (dmp.ptr - dmp.buffer)

long yyjs_dump_bc(yy_parse_state_t *state, struct yy_dump_state *d)
{
    yy_dump_append_bytes(d, "\x1b\x4c\x4a\2", 4);
    yy_dump_append_byte(d, CALL_RSV << 3 | (state->flags & YY_PARSER_STRIP));

    if (!(state->flags & YY_PARSER_STRIP)) {
        size_t chunk_name_len = strlen(state->input_name);
        yy_dump_append_uleb128(d, chunk_name_len);
        yy_dump_append_bytes(d, state->input_name, chunk_name_len);
    }

    struct yy_function *fun;
    STAILQ_FOREACH(fun, &state->fun_list, q_entry) {
        struct yy_dump_state proto_s = {}, dbg_s = {};
        if (!(state->flags & YY_PARSER_STRIP))
            yy_dump_debug(fun, state->id_intern, &dbg_s);
        yy_dump_proto(fun, dump_size(dbg_s), &proto_s);
        yy_dump_append_uleb128(d, dump_size(proto_s) + dump_size(dbg_s));
        yy_dump_merge(d, &proto_s);
        yy_dump_merge(d, &dbg_s);
    }

    yy_dump_append_byte(d, 0);
    return d->ptr - d->buffer;
}
