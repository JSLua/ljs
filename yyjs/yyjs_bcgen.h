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

#ifndef LYYJS_YYJS_BCGEN_H
#define LYYJS_YYJS_BCGEN_H

#include "yyjs_parse.h"

int yy_emit_assignment(yy_parse_state_t *state, expr_t target, int keep);
void yy_emit_array_field(yy_parse_state_t *state, int index, int unpack);
expr_t yy_emit_call(yy_parse_state_t *, int arg, int is_new);
expr_t yy_expr_discharge(yy_parse_state_t *, expr_t lv_ref, int keep_idx);

enum { P_UNM = 0x1000, P_TOBIT, P_CASE, P_GENERATOR };

expr_t yy_emit_concat(yy_parse_state_t *state, int elements);
expr_t yy_emit_operator(yy_parse_state_t *state, int opr, expr_t x);
expr_t yy_merge_short_chains(yy_parse_state_t *state, expr_t x1, expr_t x2);

enum ParserAction {
    ACTION_OPEN_TABLE,
    ACTION_OPEN_CLASS,
    ACTION_PUSH_STATIC,
    ACTION_PREPARE_OPTIONAL,
    ACTION_EMIT_RETHROW,
    ACTION_EMIT_CHECK_RET,
    ACTION_CAPTURE_VARARG
};

int yy_bcg_do_action(yy_parse_state_t *state, enum ParserAction);
int yy_emit_jmp(yy_parse_state_t *state, int pc);
void yy_fill_jmp_placeholder(yy_parse_state_t *state, uint32_t ji);
void yy_fill_table_size(yy_parse_state_t *state, uint32_t, int, int);

void yy_bcg_open_scope(yy_parse_state_t *, int type);
void yy_bcg_close_scope(yy_parse_state_t *, uint16_t out_chain);
expr_t yy_close_function(yy_parse_state_t *state);
int yy_emit_loop_ctl(yy_parse_state_t *, int fc_type, int label);

#endif //LYYJS_YYJS_BCGEN_H
