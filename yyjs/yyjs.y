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

%require "3.0"
%define api.pure
%define parse.error verbose
%param { yyscan_t scan }
%lex-param { void *yyssp }  /* For `yy_tok_expected()` */

%code requires {
typedef void *yyscan_t;
#define YY_DECL int yylex \
    (YYSTYPE *yylval_param, yyscan_t yyscanner, void *yyssp)
}

%token KEYWORD_ASYNC KEYWORD_AWAIT KEYWORD_BREAK
%token KEYWORD_CASE KEYWORD_CATCH KEYWORD_CLASS KEYWORD_CONST KEYWORD_CONTINUE
%token KEYWORD_DEFAULT KEYWORD_DELETE KEYWORD_DO KEYWORD_ELSE KEYWORD_EXTENDS
%token KEYWORD_FINALLY KEYWORD_FOR KEYWORD_FUNCTION
%token KEYWORD_IF KEYWORD_IN KEYWORD_INSTANCEOF
%token KEYWORD_LET KEYWORD_NEW KEYWORD_OF
%token KEYWORD_RETURN KEYWORD_STATIC KEYWORD_SWITCH
%token KEYWORD_THIS KEYWORD_THROW KEYWORD_TRY KEYWORD_TYPEOF
%token KEYWORD_VAR KEYWORD_VOID KEYWORD_WHILE KEYWORD_WITH KEYWORD_YIELD

%token P_EQ P_NE P_FEQ P_NFE P_LE P_GE
%token P_INC P_DEC
%token P_SHL P_ASHR P_SHR
%token P_AND P_OR P_OPT
%token P_ADD P_SUB P_MUL P_DIV P_POW P_MOD
%token P_SHLA P_ASHRA P_SHRA
%token P_BAND P_BOR P_XOR
%token P_EXP
%token P_ARROW P_ELISION

%token Identifier NumericLiteral StringLiteral
%token TemplateHead TemplateMiddle TemplateTail
%token TOK_PRI TOK_REGEXP

%code {
#include "yyjs_bcgen.h"
extern YY_DECL;
struct yy_parse_state *yyget_extra(yyscan_t);

#define state yyget_extra(scan)
#define yyerror(scan, msg) yy_parser_set_error(state, msg)
#define OPERATOR(type, x) yyval = yy_emit_operator(state, type, x)
#define MATH(type) yyval = yy_apply_math(state, type, yyvsp[-2], yyvsp[0])
#define LOCAL_DECLARE(id, x) \
        if (yy_parser_alloc_local(state, id, x) < 0) YYERROR;
#define SYMBOL(kw) yy_intern_string(state, kw, sizeof(kw) - 1, 0)
#define GLOBAL(id) (expr_tag(T_LV_REF) | 255 << 16 | (expr_t)(id))
#define LUA_GLOBAL(id) GLOBAL(SYMBOL(id) | LV_EXPR_F_RAW)
#define FIELD_REF(id) (expr_tag(T_LV_REF) | 254 << 16 | (expr_t)(id))
}

/* Dangling else */
%right P_THEN KEYWORD_ELSE

%start FunctionBody

%%

_FOpen: { yy_bcg_open_scope(state, KEYWORD_FUNCTION); };
_TOpen: { OPERATOR('{', 0); };
_M: { OPERATOR(0, $0); };
_N: { if (expr_type($0) < T_PRI) OPERATOR('#', $0);
      else $$ = (expr_type($0) >= T_INT) ? $0 : expr_int(($0&3) >> 1); };

/* ECMAScript 2015 12.1 Identifiers */

BindingIdentifier:
    Identifier |
    KEYWORD_STATIC { $$ = SYMBOL("static"); }
    ;

LabelIdentifier:
    Identifier
    ;

IdentifierName:
    Identifier |
    KEYWORD_ASYNC { $$ = SYMBOL("async"); } |
    KEYWORD_BREAK { $$ = SYMBOL("break"); } |
    KEYWORD_CASE { $$ = SYMBOL("case"); } |
    KEYWORD_CATCH { $$ = SYMBOL("catch"); } |
    KEYWORD_CLASS { $$ = SYMBOL("class"); } |
    KEYWORD_CONTINUE { $$ = SYMBOL("continue"); } |
    KEYWORD_DEFAULT { $$ = SYMBOL("default"); } |
    KEYWORD_DELETE { $$ = SYMBOL("delete"); } |
    KEYWORD_EXTENDS { $$ = SYMBOL("extends"); } |
    KEYWORD_FINALLY { $$ = SYMBOL("finally"); } |
    KEYWORD_FOR { $$ = SYMBOL("for"); } |
    KEYWORD_FUNCTION { $$ = SYMBOL("function"); } |
    KEYWORD_NEW { $$ = SYMBOL("new"); } |
    KEYWORD_STATIC { $$ = SYMBOL("static"); } |
    KEYWORD_SWITCH { $$ = SYMBOL("switch"); } |
    KEYWORD_THROW { $$ = SYMBOL("throw"); } |
    KEYWORD_TYPEOF { $$ = SYMBOL("typeof"); } |
    KEYWORD_WHILE { $$ = SYMBOL("while"); } |
    KEYWORD_WITH { $$ = SYMBOL("with"); } |
    KEYWORD_YIELD { $$ = SYMBOL("yield"); }
    ;

/* ECMAScript 2015 12.2 Primary Expression */

PrimaryExpression:
    KEYWORD_THIS { $$ = yy_parser_create_lv(state, -1, -1); } |
    BindingIdentifier { $$ = yy_parser_create_lv(state, -1, $1); } |
    Literal |
    ArrayLiteral { $$ = 0; } |
    FunctionExpression { $$ = 0; } |
    AsyncFunctionExpression { $$ = 0; } |
    GeneratorExpression { $$ = 0; } |
    ClassExpression { $$ = 0; } |
    TOK_REGEXP { OPERATOR(TOK_REGEXP, expr_tag(T_STR) | $1); } |
    TemplateLiteral |
    '(' ObjectLiteral ')' { $$ = 0; } |
    '(' Expression ')' { $$ = $2; }
    ;

Literal:
    TOK_PRI |
    NumericLiteral |
    StringLiteral { $$ = expr_tag(T_STR) | (uint16_t)$1; }
    ;

ArrayLiteral:
    '[' _TOpen SpreadElement ']' { if($3) yy_emit_array_field(state, 0, 1); } |
    '[' _TOpen ElementList ']' { yy_fill_table_size(state, $2, $3, 0); } |
    '[' _TOpen ElementList ',' SpreadElement ']' {
        if ($5) yy_emit_array_field(state, $3, 1);
        yy_fill_table_size(state, $2, $3, 0);
    }
    ;

ElementList:
    _ExpressionOrObject { $$ = yy_emit_array_field(state, 0, 0); } |
    ',' _ExpressionOrObject { $$ = yy_emit_array_field(state, 1, 0); } |
    ElementList ',' _ExpressionOrObject
        { $$ = yy_emit_array_field(state, $1, 0); }
    ;

SpreadElement:
    P_ELISION AssignmentExpression { OPERATOR(P_ELISION, $2); } | { $$=0; }
    ;

/* ECMAScript 2015 12.2.6 Object Initializer */

ObjectLiteral:
    '{' _TOpen '}' |
    '{' _TOpen PropertyDefinitionList '}'
        { yy_fill_table_size(state, $2, 0, $3); } |
    '{' _TOpen PropertyDefinitionList ',' '}'
        { yy_fill_table_size(state, $2, 0, $3); }
    ;

PropertyDefinitionList:
    PropertyDefinition { $$ = 1; } |
    PropertyDefinitionList ',' PropertyDefinition { $$ = $1 + 1; }
    ;

PropertyDefinition:
    BindingIdentifier {
        OPERATOR(0, yy_parser_create_lv(state, -1, $1));
        yy_emit_assignment(state, FIELD_REF($1), 0);
    } /* IdentifierReference */ |
    PropertyName ':' _ExpressionOrObject {
        yy_emit_assignment(state, $1, 0);
    } |
    MethodDefinition
    ;

PropertyName:
    IdentifierName { $$ = FIELD_REF($1); } |
    StringLiteral { $$ = FIELD_REF((uint16_t)$1); } |
    NumericLiteral { yy_expr_discharge(state, $1, 0),
                     $$ = FIELD_REF(0xffff); } |
    '[' _ExpressionOrObject ']' { $$ = FIELD_REF(0xffff); }
    ;

/* ECMAScript 2015 12.2.9 Template Literals */

_TplLiteral: {
    if (($$ = ($0 != -1)))
        yy_expr_discharge(state, expr_tag(T_STR) | $0, 0);
};

TemplateLiteral:
    TemplateHead _TplLiteral Expression _M TemplateSpans {
        $$ = yy_emit_concat(state, $2 + 1 + $5);
    }
    ;

TemplateSpans:
    TemplateTail _TplLiteral { $$ = $2; } |
    TemplateMiddleList TemplateTail _TplLiteral { $$ = $1 + $3; }
    ;

TemplateMiddleList:
    TemplateMiddle _TplLiteral Expression _M { $$ = $2 + 1; } |
    TemplateMiddleList TemplateMiddle _TplLiteral Expression _M
        { $$ = $1 + $3 + 1; }
    ;

/* ECMAScript 2015 12.3 Left-Hand-Side Expressions */

MemberExpression:
    PrimaryExpression |
    MemberExpression _M '[' Expression _M ']' {
        $$ = yy_parser_create_lv(state, 0, -1);
    } |
    MemberExpression '.' IdentifierName {
        $$ = yy_parser_create_lv(state, $1, $3);
        if ($$ == -1) YYERROR;
    } |
    KEYWORD_NEW _TOpen MemberExpression {
        OPERATOR(KEYWORD_NEW, $3);
    } Arguments {
        $$ = yy_emit_call(state, 1 + $5, 1);
    }
    ;

NewExpression:
    MemberExpression |
    KEYWORD_NEW _TOpen NewExpression {
        OPERATOR(KEYWORD_NEW, $3);
        $$ = yy_emit_call(state, 1, 1);
    }
    ;

CallExpression:
    MemberExpression { OPERATOR('(', $1); } Arguments {
        $$ = yy_emit_call(state, !!$2 + $3, 0);
    } |
    CallExpression { OPERATOR('(', $1); } Arguments {
        $$ = yy_emit_call(state, !!$2 + $3, 0);
    } |
    CallExpression '[' Expression _M ']' {
        $$ = yy_parser_create_lv(state, 0, -1);
    } |
    CallExpression '.' IdentifierName {
        $$ = yy_parser_create_lv(state, 0, $3);
    }
    ;

Arguments:
    '(' ')' { $$ = 0; } |
    '(' ArgumentList ')' { $$ = $2; } |
    '(' ArgumentList ',' ')' { $$ = $2; }
    ;

ArgumentList:
    _ExpressionOrObject { $$ = 1; } |
    P_ELISION AssignmentExpression { OPERATOR(P_ELISION, $2), $$ = 256; } |
    ArgumentList ',' _ExpressionOrObject {
        if (($$ = $1 + 1) >= 240) {
            yy_parser_set_error(state, "too many arguments in call");
            YYERROR;
        }
    } |
    ArgumentList ',' P_ELISION AssignmentExpression {
        OPERATOR(P_ELISION, $4), $$ = $1 | 256;
    }
    ;

OptionalChain:
    P_OPT { OPERATOR(P_OPT, 0); } '[' Expression _M ']' {
        yy_emit_operator(state, 0, yy_parser_create_lv(state, 0, -1));
        $$ = $2;
    } |
    P_OPT IdentifierName {
        OPERATOR(P_OPT, 0);
        yy_emit_operator(state, 0, yy_parser_create_lv(state, 0, $2));
    } |
    P_OPT IdentifierName {
        OPERATOR(P_OPT, 0);
        yy_emit_operator(state, '(', yy_parser_create_lv(state, 0, $2));
    } Arguments { yy_emit_call(state, 1 + $4, 0); $$ = $3; } |
    OptionalChain '[' Expression _M ']'
        { yy_emit_operator(state, 0, yy_parser_create_lv(state, 0, -1)); } |
    OptionalChain '.' IdentifierName
        { yy_emit_operator(state, 0, yy_parser_create_lv(state, 0, $3)); }
    ;

OptionalExpression:
    MemberExpression _M OptionalChain { $$ = $3; }  |
    CallExpression _M OptionalChain { $$ = $3; } |
    OptionalExpression OptionalChain
        { $$ = yy_merge_short_chains(state, $1, $2); }
    ;


LeftHandSideExpression:
    NewExpression |
    CallExpression |
    OptionalExpression { yy_fill_jmp_placeholder(state, $1 >> 16); $$ = 0; }
    ;

/* ECMAScript 2015 12.4 Postfix Expressions */

PostfixExpression:
    LeftHandSideExpression |
    LeftHandSideExpression P_INC {
        OPERATOR(P_INC, $1); if ($$ == -1) YYERROR;
    } |
    LeftHandSideExpression P_DEC {
        OPERATOR(P_DEC, $1); if ($$ == -1) YYERROR;
    }
    ;

/* ECMAScript 2015 12.5 Unary Operators */

UnaryExpression:
    PostfixExpression |
    KEYWORD_DELETE UnaryExpression {
        if (expr_type($2) == T_LV_REF && ($2 & 0xffffff) > 0xfff000) {
            $$ = expr_tag(T_PRI) | 1;
            break;  /* Reject deletion of local variable or upvalue */
        }
        OPERATOR(KEYWORD_DELETE, $2);
    } |
    KEYWORD_VOID UnaryExpression { OPERATOR(KEYWORD_VOID, $2); } |
    KEYWORD_TYPEOF UnaryExpression { OPERATOR(KEYWORD_TYPEOF, $2); } |
    P_INC UnaryExpression {
        if (yy_expr_discharge(state, $2, 1) != 0) YYERROR;
        OPERATOR('+', expr_int(1));
        if (!yy_emit_assignment(state, $2, 1)) YYERROR;
        $$ = 0;
    } |
    P_DEC UnaryExpression {
        if (yy_expr_discharge(state, $2, 1) != 0) YYERROR;
        OPERATOR('-', expr_int(1));
        if (!yy_emit_assignment(state, $2, 1)) YYERROR;
        $$ = 0;
    } |
    '+' UnaryExpression _N { $$ = $3; } |
    '-' UnaryExpression _N {
        if (expr_type($3) > T_INT) $$ = $3 ^ (1ull << 63);
        else if (expr_type($3) == T_INT) $$ =
            (uint32_t)$3 == 1<<31 ? ~0x41e0000000000000ull : expr_int(-$3);
        else OPERATOR(P_UNM, $3);
    } |
    '!' UnaryExpression {
        if (expr_type($2) > T_LV_REF)
            $$ = expr_tag(T_PRI) | 1 +
                (expr_type($2) == T_PRI && ($2 & 3) < 2);
        else OPERATOR('!', $2);
    } |
    '~' UnaryExpression {
        if (expr_type($2) == T_INT) $$ = $2 ^ 0xffffffff;
        else OPERATOR('~', $2);
    } |
    AwaitExpression
    ;

/* ECMAScript 2016 12.6 Multiplicative Operators */

ExponentiationExpression:
    UnaryExpression |
    UnaryExpression _N P_POW ExponentiationExpression { MATH(P_POW); }
    ;

/* ECMAScript 2016 12.7 Multiplicative Operators */

MultiplicativeExpression:
    ExponentiationExpression |
    MultiplicativeExpression _N '*' ExponentiationExpression { MATH('*'); } |
    MultiplicativeExpression _N '/' ExponentiationExpression { MATH('/'); } |
    MultiplicativeExpression _N '%' ExponentiationExpression { MATH('%'); }
    ;

/* ECMAScript 2015 12.7 Additive Operators */

AdditiveExpression:
    MultiplicativeExpression |
    AdditiveExpression _N '+' MultiplicativeExpression { MATH('+'); } |
    AdditiveExpression _N '-' MultiplicativeExpression { MATH('-'); }
    ;

/* ECMAScript 2015 12.8 Bitwise Shift Operators */

ShiftExpression:
    AdditiveExpression |
    ShiftExpression _N P_SHL AdditiveExpression { MATH(P_SHL); } |
    ShiftExpression _N P_SHR AdditiveExpression { MATH(P_SHR); } |
    ShiftExpression _N P_ASHR AdditiveExpression { MATH(P_ASHR); }
    ;

/* ECMAScript 2015 12.9 Relational Operators */

RelationalExpression:
    ShiftExpression |
    ShiftExpression _M '<' ShiftExpression { OPERATOR('<', $4); } |
    ShiftExpression _M '>' ShiftExpression { OPERATOR('>', $4); } |
    ShiftExpression _M P_LE ShiftExpression { OPERATOR(P_LE, $4); } |
    ShiftExpression _M P_GE ShiftExpression { OPERATOR(P_GE, $4); } |
    ShiftExpression _M KEYWORD_IN
        ShiftExpression { OPERATOR(KEYWORD_IN, $4); } |
    ShiftExpression _M KEYWORD_INSTANCEOF
        ShiftExpression { OPERATOR(KEYWORD_INSTANCEOF, $4); }
    ;

/* ECMAScript 2015 12.10 Equality Operators */

EqualityExpression:
    RelationalExpression |
    EqualityExpression _M P_EQ RelationalExpression { OPERATOR(P_EQ, $4); } |
    EqualityExpression _M P_NE RelationalExpression { OPERATOR(P_NE, $4); } |
    EqualityExpression _M P_FEQ RelationalExpression { OPERATOR(P_EQ, $4); } |
    EqualityExpression _M P_NFE RelationalExpression { OPERATOR(P_NE, $4); }
    ;

/* ECMAScript 2015 12.11 Binary Bitwise Operators */

BitwiseANDExpression:
    EqualityExpression |
    BitwiseANDExpression _N '&' EqualityExpression { MATH('&'); }
    ;

BitwiseXORExpression:
    BitwiseANDExpression |
    BitwiseXORExpression _N '^' BitwiseANDExpression { MATH('^'); }
    ;

BitwiseORExpression:
    BitwiseXORExpression |
    BitwiseORExpression _N '|' BitwiseXORExpression { MATH('|'); }
    ;

/* ECMAScript 2015 12.12 Binary Logical Operators */

_And: P_AND {
    if (expr_type($0) == T_PRI && ($0 & 2)) $$ = 0;
    else OPERATOR(P_AND, $0);
};

LogicalANDExpression:
    BitwiseORExpression |
    LogicalANDExpression _And BitwiseORExpression
        { $$ = yy_merge_short_chains(state, $2, $3); } |
    LogicalANDExpression _And ObjectLiteral { $$ = $2 & ~255; }
    ;

_Or: P_OR {
    if (expr_type($0) == T_PRI && !($0 & 2)) $$ = 0;
    else OPERATOR(P_OR, $0);
};

LogicalORExpression:
    LogicalANDExpression |
    LogicalORExpression _Or LogicalANDExpression
        { $$ = yy_merge_short_chains(state, $2, $3); } |
    LogicalORExpression _Or ObjectLiteral { $$ = $2 & ~255; }
    ;

/* ECMAScript 2015 12.13 Conditional Operator */

ConditionalExpression:
    LogicalORExpression |
    LogicalORExpression '?' { OPERATOR('?', $1); }
    _ExpressionOrObject ':' {
        $$ = yy_emit_jmp(state, 0);
        yy_emit_operator(state, ':', 0);
        yy_fill_jmp_placeholder(state, $3 >> 32);
    } _ExpressionOrObject {
        yy_fill_jmp_placeholder(state, $6);
        $$ = 0;
    }
    ;

/* ECMAScript 2015 12.14 Assignment Operators */

AssignmentExpression:
    ConditionalExpression |
    YieldExpression |
    ArrowFunction |
    AsyncArrowFunction |
    LeftHandSideExpression '=' _ExpressionOrObject {
        if (!yy_emit_assignment(state, $1, 1)) YYERROR;
        $$ = 0;
    } |
    LeftHandSideExpression {
        if (yy_expr_discharge(state, $1, 1) != 0) YYERROR;
    } AssignmentOperator AssignmentExpression {
        yy_emit_operator(state, $3, $4);
        if (!yy_emit_assignment(state, $1, 1)) YYERROR;
        $$ = 0;
    }
    ;

AssignmentOperator:
    P_MUL { $$ = '*'; } |
    P_DIV { $$ = '/'; } |
    P_MOD { $$ = '%'; } |
    P_ADD { $$ = '+'; } |
    P_SUB { $$ = '-'; } |
    P_EXP { $$ = P_POW; } |
    P_SHLA { $$ = P_SHL; } |
    P_ASHRA { $$ = P_ASHR; } |
    P_SHRA { $$ = P_SHR; } |
    P_BAND { $$ = '&'; } |
    P_XOR { $$ = '^'; } |
    P_BOR { $$ = '|'; }
    ;

/* ObjectLiteral has reduce-reduce conflicts with statement block, so it is
 * enclosed with `()` in PrimaryExpression. _ExpressionOrObject allows direct
 * use of ObjectLiteral at non-conflict location for convenience. */
_ExpressionOrObject:
    AssignmentExpression { OPERATOR(0, $1); } |
    ObjectLiteral { $$ = 0; }
    ;

/* ECMAScript 2015 12.15 Comma Operator */

Expression:
    AssignmentExpression |
    Expression { OPERATOR(',', $1); } ',' AssignmentExpression { $$ = $4; }
    ;

/* ECMAScript 2015 13.1 Statement Semantics */

Statement:
    ';' |
    Expression ';' { OPERATOR(';', $1); } |
    { yy_bcg_open_scope(state, 0); } Block {
        yy_bcg_close_scope(state, 0);
    }  /* BlockStatement */ |
    IfStatement |
    IterationStatement |
    SwitchStatement |
    ContinueStatement { if (!$1) YYERROR; } |
    BreakStatement    { if (!$1) YYERROR; } |
    ReturnStatement { if ($1==-1) YYERROR; } |
    WithStatement |
    LabelledStatement |
    ThrowStatement |
    TryStatement
    ;

Declaration:
    FunctionDeclaration |
    GeneratorDeclaration |
    AsyncFunctionDeclaration |
    ClassDeclaration |
    LexicalDeclaration
    ;

/* ECMAScript 2015 13.2 Block */

Block:
    '{' StatementList '}' |
    '{' '}'
    ;

StatementList:
    StatementListItem |
    StatementList StatementListItem
    ;

StatementListItem:
    Statement |
    Declaration
    ;

/* ECMAScript 2015 13.3.1 Let and Const Declarations */

LexicalDeclaration:
    LetOrConst BindingList ';' { OPERATOR(';', 0); }
    ;

LetOrConst:
    KEYWORD_LET { $$ = 0; } |
    KEYWORD_CONST {
        yy_set_variable_hint(state, YY_VAR_HINT_CONST);
        $$ = 0;
    } |
    KEYWORD_VAR { $$ = 0; }  /* Treat as let */
    ;

BindingList:
    LexicalBinding |
    BindingList ',' LexicalBinding
    ;

_BindingDecl: BindingIdentifier { yy_set_identifier_tag(state, $1); };

LexicalBinding:
    BindingIdentifier { LOCAL_DECLARE($1, expr_tag(T_PRI)); } |
    _BindingDecl '=' AssignmentExpression { LOCAL_DECLARE($1, $3); } |
    _BindingDecl '=' ObjectLiteral { LOCAL_DECLARE($1, 0); } |
    '{' BindingPropertyList '}' '=' _ExpressionOrObject { OPERATOR('}', $2); }
    ;

BindingPropertyList:
    BindingIdentifier { LOCAL_DECLARE($1, 0); $$ = 1; } |
    BindingPropertyList ',' BindingIdentifier
        { LOCAL_DECLARE($3, 0); $$ = $1 + 1; }
    ;

/* ECMAScript 2015 13.6 The if Statement */

_Cond: Expression {
    if (expr_type($1) == T_PRI && ($1 & 2)) $$ = 0;
    else { OPERATOR('?', $1); $$ >>= 32; }
};

IfStatement:
    KEYWORD_IF '(' _Cond ')' Statement KEYWORD_ELSE {
        $$ = yy_emit_jmp(state, 0);
        yy_fill_jmp_placeholder(state, $3);
    } Statement {
        yy_fill_jmp_placeholder(state, $7);
    } |
    KEYWORD_IF '(' _Cond ')' Statement {
        yy_fill_jmp_placeholder(state, $3);
    } %prec P_THEN
    ;

/* ECMAScript 2015 13.7 Iteration Statements */

_For: KEYWORD_FOR { yy_bcg_open_scope(state, KEYWORD_FOR); };
_OExp: Expression { OPERATOR(';', $1); } | ;
_UExp: _OExp { if (!yy_emit_loop_ctl(state, ')', 0)) YYERROR; };
_Cont: { yy_emit_loop_ctl(state, '?', 0); };

/* `for (lhs in ...)` is removed to avoid a shift/reduce conflict */
IterationStatement:
    KEYWORD_DO {
        yy_bcg_open_scope(state, KEYWORD_WHILE);
    } Statement KEYWORD_WHILE _Cont '(' Expression ')' {
        OPERATOR(KEYWORD_WHILE, $7);
        yy_bcg_close_scope(state, 0);
    } |
    KEYWORD_WHILE {
        yy_bcg_open_scope(state, KEYWORD_WHILE);
    } '(' _Cond ')' Statement {
        yy_bcg_close_scope(state, $4);
    } |
    _For '(' _OExp ';' _Cont ';' _UExp ')'
        Statement { yy_bcg_close_scope(state, 0); } |
    _For '(' _OExp ';' _Cont _Cond ';' _UExp ')'
        Statement { yy_bcg_close_scope(state, $6); } |
    _For '(' LexicalDeclaration _Cont _Cond ';' _UExp ')'
        Statement { yy_bcg_close_scope(state, $5); } |
    _For '(' LetOrConst BindingIdentifier KEYWORD_IN {
        OPERATOR(0, LUA_GLOBAL("next"));
    } _ExpressionOrObject {
        OPERATOR(KEYWORD_FOR, 0);
        yy_parser_alloc_local(state, $4, 0);
    } ')' Statement {
        yy_bcg_close_scope(state, 0);
    } |
    _For '(' LetOrConst BindingIdentifier KEYWORD_OF {
        OPERATOR('(', yy_parser_create_lv(
            state, LUA_GLOBAL("jsrt"), SYMBOL("iterate")));
    } _ExpressionOrObject {
        OPERATOR(KEYWORD_FOR, yy_emit_call(state, 1, 0));
        yy_parser_alloc_local(state, $4, 0);
    } ')' Statement {
        yy_bcg_close_scope(state, 0);
    }
    ;

/* ECMAScript 2015 13.8 The continue Statement */

ContinueStatement:
    KEYWORD_CONTINUE ';'
        { $$ = yy_emit_loop_ctl(state, KEYWORD_CONTINUE, 0); } |
    KEYWORD_CONTINUE LabelIdentifier ';'
        { $$ = yy_emit_loop_ctl(state, KEYWORD_CONTINUE, $2); }
    ;

/* ECMAScript 2015 13.9 The break Statement */

BreakStatement:
    KEYWORD_BREAK ';'
        { $$ = yy_emit_loop_ctl(state, KEYWORD_BREAK, 0); } |
    KEYWORD_BREAK LabelIdentifier ';'
        { $$ = yy_emit_loop_ctl(state, KEYWORD_BREAK, $2); }
    ;

/* ECMAScript 2015 13.10 The return Statement */

ReturnStatement:
    KEYWORD_RETURN ';' { OPERATOR(KEYWORD_RETURN, expr_tag(T_PRI)); } |
    KEYWORD_RETURN Expression ';' { OPERATOR(KEYWORD_RETURN, $2); } |
    Expression YYEOF { OPERATOR(
        yy_emit_loop_ctl(state, YYEOF, 0) ? KEYWORD_RETURN : ';',
        $1); } |
    KEYWORD_RETURN ObjectLiteral { OPERATOR(KEYWORD_RETURN, 0); }
    ;

/* ECMAScript 2015 13.11 The with Statement */

WithStatement:
    KEYWORD_WITH '(' _ExpressionOrObject ')' {
        yy_bcg_open_scope(state, KEYWORD_WITH);
    } Statement {
        OPERATOR(KEYWORD_WITH, yy_close_function(state));
    }
    ;

/* ECMAScript 2015 13.12 The switch Statement */

SwitchStatement:
    KEYWORD_SWITCH '(' Expression _M ')' {
        yy_bcg_open_scope(state, KEYWORD_SWITCH);
    } CaseBlock {
        yy_bcg_close_scope(state, 0);
    }
    ;

/* Rules with CaseClauses after DefaultClause are removed */
CaseBlock:
    '{' CaseClauses '}' |
    '{' CaseClauses DefaultClause '}' |
    '{' DefaultClause '}' |
    '{' '}'
    ;

CaseClauses:
    CaseClause |
    CaseClauses CaseClause
    ;

CaseClause:
    KEYWORD_CASE Expression ':' { OPERATOR(KEYWORD_CASE, $2); }
        StatementList { yy_emit_loop_ctl(state, KEYWORD_CASE, $4); } |
    KEYWORD_CASE Expression ':' { OPERATOR(P_CASE, $2); }
    ;

DefaultClause:
    KEYWORD_DEFAULT ':' { yy_emit_loop_ctl(state, ':', 0); } StatementList |
    KEYWORD_DEFAULT ':'
    ;

/* ECMAScript 2015 13.13 Labelled Statements */

LabelledStatement:
    _Label SwitchStatement |
    _Label IterationStatement
    ;

_Label: LabelIdentifier ':' { yy_set_identifier_tag(state, $1); };

/* ECMAScript 2015 13.14 The throw Statement */

ThrowStatement:
    KEYWORD_THROW Expression ';' { OPERATOR(KEYWORD_THROW, $2); } |
    KEYWORD_THROW ObjectLiteral ';' { OPERATOR(KEYWORD_THROW, 0); }
    ;

/* ECMAScript 2015 13.15 The try Statement */

_TryBody:
    KEYWORD_TRY { yy_bcg_open_scope(state, KEYWORD_TRY); }
        Block { OPERATOR(KEYWORD_TRY, yy_close_function(state)); }
    ;

TryStatement:
    _TryBody Catch { yy_emit_post_try(state, 1); } |
    _TryBody Catch Finally { yy_emit_post_try(state, 1); } |
    _TryBody Finally { yy_emit_post_try(state, 0); }
    ;

Catch:
    KEYWORD_CATCH '(' BindingIdentifier ')' {
        yy_bcg_open_scope(state, KEYWORD_CATCH);
        yy_parser_alloc_local(state, $3, 0);  /* Catch error object */
    } Block {
        yy_bcg_close_scope(state, 0);
    } |
    KEYWORD_CATCH { yy_bcg_open_scope(state, KEYWORD_CATCH); }
        Block { yy_bcg_close_scope(state, 0); }
    ;

Finally:
    KEYWORD_FINALLY { yy_bcg_open_scope(state, KEYWORD_FINALLY); }
        Block { yy_bcg_close_scope(state, 0); }
    ;

/* ECMAScript 2015 14.1 Function Definitions */

FunctionDeclaration:
    KEYWORD_FUNCTION BindingIdentifier _FOpen '(' FormalParameters ')'
    '{' FunctionBody '}' {
        OPERATOR(0, yy_close_function(state));
        yy_emit_assignment(state, GLOBAL($2), 0);
    }
    ;

FunctionExpression:
    KEYWORD_FUNCTION _FOpen '(' FormalParameters ')' '{' FunctionBody '}' {
        OPERATOR(0, yy_close_function(state));
    }
    ;

FormalParameters:
    FunctionRestParameter { $$ = 0; } |
    FormalsList { $$ = $1; } |
    FormalsList ',' FunctionRestParameter { }
    ;

FormalsList:
    FormalParameter { $$ = 1; } |
    FormalsList ',' FormalParameter { $$ = $1 + 1; }
    ;

FunctionRestParameter:
    P_ELISION BindingIdentifier _TOpen {
        yy_emit_array_field(state, 0, 2);
        LOCAL_DECLARE($2, 0);
    } |
    ;

FormalParameter:
    BindingIdentifier { LOCAL_DECLARE($1, -1); } |
    BindingIdentifier {
        LOCAL_DECLARE($1, -1);
        OPERATOR(P_OR, OPERATOR(P_NE, expr_tag(T_PRI)));
    } '=' _ExpressionOrObject {
        yy_fill_jmp_placeholder(state, $2 >> 16);
    }
    ;

FunctionBody:
    StatementList |
    ;

/* ECMAScript 2015 14.2 Arrow Function Definitions */

ArrowFunction:
    ArrowParameters P_ARROW {
        yy_bcg_open_scope(state, P_ARROW);
        if ($1 != -1) LOCAL_DECLARE($1, -1);
    } ConciseBody {
        OPERATOR(0, yy_close_function(state));
    }
    ;

ArrowParameters:
    BindingIdentifier |
    '(' ')' { $$ = -1; }
    ;

ConciseBody:
    AssignmentExpression { OPERATOR(KEYWORD_RETURN, $1); } |
    '{' FunctionBody '}'
    ;

/* ECMAScript 2015 14.3 Method Definitions */

MethodDefinition:
    PropertyName _FOpen '(' FormalParameters ')' '{' FunctionBody '}' {
        OPERATOR(0, yy_close_function(state));
        yy_emit_assignment(state, $1, 0);
    } |
    GeneratorMethod |
    AsyncMethod
    ;

/* ECMAScript 2015 14.4 Generator Function Definitions */

GeneratorMethod:
    '*' PropertyName _FOpen '(' FormalParameters ')' '{' FunctionBody '}' {
        OPERATOR(P_GENERATOR, yy_close_function(state));
        yy_emit_assignment(state, FIELD_REF($2), 0);
    }
    ;

GeneratorDeclaration:
    KEYWORD_FUNCTION '*' BindingIdentifier  _FOpen '(' FormalParameters ')'
    '{' FunctionBody '}' {
        OPERATOR(P_GENERATOR, yy_close_function(state));
        yy_emit_assignment(state, GLOBAL($3), 0);
    }
    ;

GeneratorExpression:
    KEYWORD_FUNCTION '*' _FOpen '(' FormalParameters ')' '{' FunctionBody '}' {
        OPERATOR(P_GENERATOR, yy_close_function(state));
    }
    ;

_Yield: KEYWORD_YIELD {
    OPERATOR('(', yy_parser_create_lv(
        state, LUA_GLOBAL("coroutine"), SYMBOL("yield")));
};

YieldExpression:
    _Yield _ExpressionOrObject { OPERATOR(KEYWORD_YIELD, $2); } |
    _Yield { OPERATOR(KEYWORD_YIELD, expr_tag(T_PRI)); }
    ;

/* ECMAScript 2015 14.5 Class Definitions */

ClassDeclaration:
    KEYWORD_CLASS _BindingDecl ClassTail
        { yy_emit_assignment(state, GLOBAL($2), 0); OPERATOR(';', 0); }
    ;

ClassExpression:
    KEYWORD_CLASS ClassTail
    ;

ClassTail:
    { OPERATOR('(', yy_parser_create_lv(
        state, LUA_GLOBAL("jsrt"), SYMBOL("class"))); } _TOpen _TOpen
    ClassHeritage '{' ClassBody '}'
        { yy_emit_call(state, 2, 0);  /* (ctor, proto) */ };

ClassHeritage:
    KEYWORD_EXTENDS LeftHandSideExpression {
        OPERATOR(KEYWORD_EXTENDS, $2);
    } |
    ;

ClassBody:
    ClassElementList |
    ;

ClassElementList:
    ClassElement |
    ClassElementList ClassElement
    ;

ClassElement:
    MethodDefinition |
    KEYWORD_STATIC MethodDefinition { OPERATOR(KEYWORD_STATIC, 0); } |
    IdentifierName ';' /* For type annotations */ |
    KEYWORD_STATIC IdentifierName ';' |
    KEYWORD_STATIC IdentifierName '=' _ExpressionOrObject ';'
        { OPERATOR(KEYWORD_STATIC, FIELD_REF($2)); } |
    KEYWORD_STATIC _FOpen '{' FunctionBody '}'
        { OPERATOR(KEYWORD_STATIC, yy_close_function(state)); } |
    ';'
    ;

/* ECMAScript 2017 14.6 Async Function Definitions */

AsyncFunctionDeclaration:
    KEYWORD_ASYNC KEYWORD_FUNCTION BindingIdentifier
    _FOpen '(' FormalParameters ')' '{' FunctionBody '}' {
        OPERATOR(KEYWORD_ASYNC, yy_close_function(state));
        yy_emit_assignment(state, GLOBAL($3), 0);
    }
    ;

AsyncFunctionExpression:
    KEYWORD_ASYNC KEYWORD_FUNCTION _FOpen '(' FormalParameters ')'
    '{' FunctionBody '}' {
        OPERATOR(KEYWORD_ASYNC, yy_close_function(state));
    }
    ;

AsyncMethod:
    KEYWORD_ASYNC PropertyName _FOpen '(' FormalParameters ')'
    '{' FunctionBody '}' {
        OPERATOR(KEYWORD_ASYNC, yy_close_function(state));
        yy_emit_assignment(state, $2, 0);
    }
    ;

AwaitExpression:
    KEYWORD_AWAIT {
        OPERATOR('(', yy_parser_create_lv(
            state, LUA_GLOBAL("jsrt"), SYMBOL("await")));
    } UnaryExpression { OPERATOR(KEYWORD_AWAIT, $3); }
    ;

/* ECMAScript 2017 14.7 Async Arrow Function Definitions */

AsyncArrowFunction:
    KEYWORD_ASYNC ArrowParameters P_ARROW {
        yy_bcg_open_scope(state, P_ARROW);
        if ($2 != -1) LOCAL_DECLARE($2, -1);
    } ConciseBody {
        OPERATOR(KEYWORD_ASYNC, yy_close_function(state));
    }
    ;

%%

/* Allow the lexer to check whether tok is expected */
int yy_tok_expected(void *yyssp, int tok) {
    yysymbol_kind_t sym = YYTRANSLATE(tok);
    int n = yypact[+*(yy_state_t*)yyssp] + sym;
    return n >= 0 && n <= YYLAST && yycheck[n] == sym;
}
