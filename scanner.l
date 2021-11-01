%{
#include <stdio.h>
#include "y.tab.h"
int count = 1;
%}
AZ09 [a-zA-Z0-9]
AZ [a-zA-Z]
%Start comm code
%%
				BEGIN code;
<code>"//".*			;
<code>"/*"			BEGIN comm;
<comm>.|\n			;
<comm>"*/"			BEGIN code;
<code>[ \t]+ 			;
<code>\n			; count++;
<code>auto			return AUTO;
<code>"bool"			return BOOL;
<code>double			return DOUBLE;
<code>int			return INT;
<code>struct			return STRUCT;
<code>break			return BREAK;
<code>else			return ELSE;
<code>long			return LONG;
<code>switch			return SWITCH;
<code>case			return CASE;
<code>enum			return ENUM;
<code>register			return REGISTER;
<code>typedef			return TYPEDEF;
<code>char			return CHAR;
<code>extern			return EXTERN;
<code>return			return RETURN;
<code>union			return UNION;
<code>const			return CONST;
<code>float			return FLOAT;
<code>short			return SHORT;
<code>"int*"			return P_INT;
<code>"float*"			return P_FLOAT;
<code>"double*"			return P_DOUBLE;
<code>"long*"			return P_LONG;
<code>"short*"			return P_SHORT;
<code>"char*"			return P_CHAR;
<code>unsigned			return UNSIGNED;
<code>continue			return CONTINUE;
<code>for			return FOR;
<code>signed			return SIGNED;
<code>void			return VOID;
<code>default			return DEFAULT;
<code>goto			return GOTO;
<code>sizeof			return SIZEOF;
<code>volatile			return VOLATILE;
<code>do			return DO;
<code>if			return IF;
<code>static			return STATIC;
<code>while			return WHILE;
<code>"TRUE"			return BOOL_TRUE;
<code>"FALSE"			return BOOL_FALSE;
<code>"NULL"			return VAL_NULL;
<code>_*{AZ}+({AZ09}|_)*	{ yylval = *yytext - 'a'; return IDENTIFIER; }
<code>[0-9]+\.[0-9]+		{ yylval = atoi(yytext); return FLOAT_CONSTANT; }
<code>[0-9]+			{ yylval = atoi(yytext); return INT_CONSTANT; }
<code>\'(.|\n)?\'		{ yylval = *yytext - 'a'; return CHAR_CONSTANT; }
<code>\"[^\"]*?\"		{ yylval = *yytext - 'a'; return CHAR_CONSTANT; }
<code>"."			return DOT;
<code>"="			return ASSIGN;
<code>"+"			return ADD;
<code>"-"			return SUB;
<code>"*"			return MUL;
<code>"/"			return DIV;
<code>"%"			return MOD;
<code>"++"			return INC;
<code>"--"			return DEC;
<code>"=="			return EQUAL;
<code>"!="			return NOT_EQUAL;
<code>">"			return GREATER;
<code>"<"			return LESS;
<code>">="			return GREATER_EQUAL;
<code>"<="			return LESS_EQUAL;
<code>"!"			return NOT;
<code>"&&"			return AND;
<code>"||"			return OR;
<code>"~"			return B_NOT;
<code>"|"			return B_OR;
<code>"<<"			return B_L_SHIFT;
<code>">>"			return B_R_SHIFT;
<code>"&"			return P_ADDRESS;
<code>"^"			return POINTER;
<code>"?"			return QUESTION;
<code>":"			return COLON;
<code>";"			return SEMICOLON;
<code>","			return COMMA;
<code>"+="			return ADD_ASSIGN;
<code>"-="			return SUB_ASSIGN;
<code>"*="			return MUL_ASSIGN;
<code>"/="			return DIV_ASSIGN;
<code>"%="			return MOD_ASSIGN;
<code>"&="			return B_AND_ASSIGN;
<code>"|="			return B_OR_ASSIGN;
<code>"^="			return B_XOR_ASSIGN;
<code>"<<="			return B_L_SHIFT_ASSIGN;
<code>">>="			return B_R_SHIFT_ASSIGN;
<code>"("			return O_PARENTH;
<code>")"			return C_PARENTH;
<code>"{"			return O_BRACE;
<code>"}"			return C_BRACE;
<code>"["			return O_BRACKET;
<code>"]"			return C_BRACKET;
<code>#include			return DIR_INCLUDE;
<code>{AZ}+{AZ09}*\.h		{ yylval = *yytext - 'a'; return LIBRARY; }
<code>#define			return DIR_DEFINE;
<code>#undef			return DIR_UNDEF;
<code>#if			return DIR_IF;
<code>#ifdef			return DIR_IFDEF;
<code>#ifndef			return DIR_IFNDEF;
<code>#elif			return DIR_ELIF;
<code>#else			return DIR_ELSE;
<code>#endif			return DIR_ENDIF;
<code>#line			return DIR_LINE;
<code>#error			return DIR_ERROR;
<code>#pragma			return DIR_PRAGMA;
<code>#				return DIR_NULL;
<code>\.*[0-9]+(\.|{AZ}|[0-9])*	{/*printf("ERROR %s\n", yytext);*/ return yytext[0]; }
<code>. 			{/*printf("ERROR %s\n", yytext);*/ return yytext[0]; }
%%
