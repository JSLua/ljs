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

%{
#include "yyjs_parse.h"
#include "yyjs.tab.h"
#define NUMBER(base, offset) *yylval = \
    yy_pack_number(yytext + offset, base); return NumericLiteral;
extern int yy_tok_expected(void *yyssp, int tok);
%}

%option reentrant bison-bridge
%option noyywrap
%option yylineno
%option extra-type="struct yy_parse_state *"

%x COMMENT
%s TPL

RegExpChar [^\n\r\/\\[*]|\[([^\n\]\\]|\\.)*]|\\.
HexDigit  [0-9A-Fa-f]
EscapeSeq [^ux]|x{HexDigit}{2}|u{HexDigit}{4}|u\{{HexDigit}+\}
TemplateCharacter [^`$\\]|\\{EscapeSeq}

%%

async   { return KEYWORD_ASYNC; }
await   { return KEYWORD_AWAIT; }
break   { return KEYWORD_BREAK; }
case    { return KEYWORD_CASE; }
catch   { return KEYWORD_CATCH; }
class   { return KEYWORD_CLASS; }
const   { return KEYWORD_CONST; }
continue { return KEYWORD_CONTINUE; }
default { return KEYWORD_DEFAULT; }
delete  { return KEYWORD_DELETE; }
do      { return KEYWORD_DO; }
else    { return KEYWORD_ELSE; }
extends { return KEYWORD_EXTENDS; }
finally { return KEYWORD_FINALLY; }
for     { return KEYWORD_FOR; }
function { return KEYWORD_FUNCTION; }
if      { return KEYWORD_IF; }
in      { return KEYWORD_IN; }
instanceof { return KEYWORD_INSTANCEOF; }
let     { return KEYWORD_LET; }
new     { return KEYWORD_NEW; }
of      { return KEYWORD_OF; }
return  { return KEYWORD_RETURN; }
static  { return KEYWORD_STATIC; }
switch  { return KEYWORD_SWITCH; }
this    { return KEYWORD_THIS; }
throw   { return KEYWORD_THROW; }
try     { return KEYWORD_TRY; }
typeof  { return KEYWORD_TYPEOF; }
var     { return KEYWORD_VAR; }
void    { return KEYWORD_VOID; }
while   { return KEYWORD_WHILE; }
with    { return KEYWORD_WITH; }
yield   { return KEYWORD_YIELD; }

null    { *yylval = expr_tag(T_PRI);      return TOK_PRI; }
true    { *yylval = expr_tag(T_PRI) | 2;  return TOK_PRI; }
false   { *yylval = expr_tag(T_PRI) | 1;  return TOK_PRI; }

"//".*          { }
"/*"            BEGIN(COMMENT);
<COMMENT>"*/"   BEGIN(INITIAL);
<COMMENT>@raw   yy_set_variable_hint(yyextra, YY_VAR_HINT_RAW);
<COMMENT>.      { }
^"#!".*         /* shebang */

([0-9]*\.[0-9]+|[1-9][0-9]*|0)([eE][-+]?[0-9]+)? NUMBER(10, 0)
0[Bb][01]+          NUMBER(2, 2)
0[Oo]?[0-7]+        NUMBER(8, 1 + (yytext[1] > '7'))
0[Xx]{HexDigit}+    NUMBER(16, 2)

\"([^"\\\n]|\\{EscapeSeq})*\"|'([^'\\\n]|\\{EscapeSeq})*' {
    *yylval = yy_intern_string(yyextra, yytext, yyleng, 1);
    return StringLiteral;
}

['"] {  /* Catch unsuccessful match of StringLiteral */
    yy_parser_set_error(yyextra, "Invalid escape sequence in string");
    return YYerror;
}

`{TemplateCharacter}*`   {
    *yylval = yy_intern_string(yyextra, yytext, yyleng, 1);
    return StringLiteral;
}

`{TemplateCharacter}*\$\{   {
    BEGIN(TPL);
    *yylval = yy_intern_string(yyextra, yytext, yyleng, 1);
    return TemplateHead;
}

<TPL>\}{TemplateCharacter}*\$\{   {
    *yylval = yy_intern_string(yyextra, yytext, yyleng, 1);
    return TemplateMiddle;
}

<TPL>\}{TemplateCharacter}*`   {
    BEGIN(INITIAL);
    *yylval = yy_intern_string(yyextra, yytext, yyleng, 1);
    return TemplateTail;
}

<TPL>\/\*.*\*\/ {}

\/{RegExpChar}({RegExpChar}|\*)*\/[img]* {
    if (!yy_tok_expected(yyssp, TOK_REGEXP)) {
        yyless(1);
        return '/';
    }

    *yylval = yy_intern_string(yyextra, yytext, yyleng, 1);
    return TOK_REGEXP;
}

\<=     { return P_LE; }
>=      { return P_GE; }
==      { return P_EQ; }
!=      { return P_NE; }
===     { return P_FEQ; }
!==     { return P_NFE; }
&&      { return P_AND; }
\|\|    { return P_OR; }
\+=     { return P_ADD; }
-=      { return P_SUB; }
\*=     { return P_MUL; }
\/=     { return P_DIV; }
%=      { return P_MOD; }
\*\*=   { return P_EXP; }
\<\<=   { return P_SHLA; }
>>=     { return P_ASHRA; }
>>>=    { return P_SHRA; }
&=      { return P_BAND; }
\|=     { return P_BOR; }
\^=     { return P_XOR; }
\+\+    { return P_INC; }
--      { return P_DEC; }
\*\*    { return P_POW; }
\<\<    { return P_SHL; }
>>      { return P_ASHR; }
>>>     { return P_SHR; }
=>      { return P_ARROW; }
"..."   { return P_ELISION; }
"?."    { return P_OPT; }
[{}()[\];,<>+\-*%&|^!~?:=/.]  return *yytext;

[A-Za-z$_\xc2-\xf7][A-Za-z0-9$_\x80-\xf7]* {
    *yylval = yy_intern_string(yyextra, yytext, yyleng, 0);
    return Identifier;
}

[ \t\f\v]   /* WhiteSpace */
<*>\r?\n    yy_parser_mark_line(yyextra, yylineno);
.           return *yytext;

%%

void yy_lex_set_input(yyscan_t scan, void *input, int len)
{
    if (len < 0) {
        yyset_in(input, scan);
        yy_switch_to_buffer(
            yy_create_buffer(yyget_in(scan), YY_BUF_SIZE, scan),
            scan);
    } else {
        yy_scan_bytes(input, len, scan);
        yyset_lineno(1, scan);
    }
}
