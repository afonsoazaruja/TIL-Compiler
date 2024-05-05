%option c++ prefix="til_scanner_" outfile="til_scanner.cpp"
%option stack noyywrap yylineno 8bit debug
%{ 
// make relevant includes before including the parser's tab file
#include <string>
#include <cdk/ast/sequence_node.h>
#include <cdk/ast/expression_node.h>
#include <cdk/ast/lvalue_node.h>
#include "til_parser.tab.h"

// don't change this
#define yyerror LexerError
%}
%x X_STRING X_COMMENT
%%
  yydebug=1; set_debug(1);

  /*
  / 1 - TIPO DE DADOS
  */

"int"                 return tINT_TYPE;
"double"              return tDOUBLE_TYPE;
"string"              return tSTRING_TYPE;
"void"                return tVOID_TYPE;

  /*
  / 3.3 - COMENTÁRIOS
  */

";".*                 ;                         /* one line comment */

"/*"                  yy_push_state(X_COMMENT);
<X_COMMENT>"/*"       yy_push_state(X_COMMENT); /* nested comments */
<X_COMMENT>"*/"       yy_pop_state(X_COMMENT);
<X_COMMENT>.|\n       ;

  /*
  / 3.8.2 - REAIS EM VÍRGULA FLUTUANTE
  */

  /*
  / 3.8.1 - INTEIROS
  */

  /*
  / 3.8.3 - CADEIAS DE CARACTERES
  */

  /*
  / 4.5 - SÍMBOLOS GLOBAIS
  */

  /*
  / 5 - FUNÇÕES
  */

  /*
  / 6.2 - INSTRUÇÃO CONDICIONAL
  */

  /*
  / 6.3 - INSTRUÇÃO DE ITERAÇÃO
  */

  /*
  / 6.4 - INSTRUÇÃO DE TERMINAÇÃO
  */

  /*
  / 6.5 - INSTRUÇÃO DE CONTINUAÇÃO
  */

  /*
  / 6.6 - ISNTRUÇÃO DE RETORNO
  */

  /*
  / 6.8 - INSTRUÇÃO DE IMPRESSÃO
  */

  /*
  / 7 - OPERADORES DE EXPRESSÃO
  */

  /*
  / ESPECIAL
  */

"null"                    return tNULLPTR;

  /*
  / 3.7 - IDENTIFICADORES
  */

  /*
  / 3.1 - CARACTERES BRANCOS
  */

  /*
  / RESTO
  */

"--".*                 ; /* ignore comments */

">="                   return tGE; 
"<="                   return tLE; 
"=="                   return tEQ; 
"!="                   return tNE; 

"loop"                return tLOOP; 
"if"                   return tIF; 
"else"                 return tELSE; 
"print"                return tPRINT; 
"read"                 return tREAD; 
"begin"                return tBEGIN; 
"end"                  return tEND; 

[A-Za-z][A-Za-z0-9_]*  yylval.s = new std::string(yytext); return tIDENTIFIER;

\'                     yy_push_state(X_STRING); yylval.s = new std::string("");
<X_STRING>\'           yy_pop_state(); return tSTRING;
<X_STRING>\\\'         *yylval.s += yytext + 1;
<X_STRING>.            *yylval.s += yytext;
<X_STRING>\n           yyerror("newline in string");

[0-9]+                 yylval.i = strtol(yytext, nullptr, 10); return tINTEGER;

[-()<>=+*/%;{}.]       return *yytext;

[ \t\n]+               ; /* ignore whitespace */

.                      yyerror("Unknown character");

%%
