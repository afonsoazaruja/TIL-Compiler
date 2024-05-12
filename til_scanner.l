%option c++ prefix="til_scanner_" outfile="til_scanner.cpp"
%option stack noyywrap yylineno 8bit debug
%{ 
// make relevant includes before including the parser's tab file
#include <string>
#include <sstream>
#include <cdk/ast/sequence_node.h>
#include <cdk/ast/expression_node.h>
#include <cdk/ast/lvalue_node.h>
#include "til_parser.tab.h"

// don't change this
#define yyerror LexerError

static std::ostringstream strliteral;
%}

EXP [eE][+-]?[0-9]+

%x X_STRING X_COMMENT X_BACKSLASH X_IGNORE_STRING X_HEXNUM
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
<X_COMMENT>"*/"       yy_pop_state();
<X_COMMENT>.|\n       ;

  /*
  / 3.8.2 - REAIS EM VÍRGULA FLUTUANTE
  */

([0-9]*\.[0-9]+|[0-9]+\.[0-9]*)({EXP})? |
[0-9]+({EXP})                           {
                                          try {
                                            yylval.d = std::stod(yytext);
                                            return tDOUBLE;
                                          } catch (const std::out_of_range &e) {
                                            yyerror("double overflow");
                                          }
                                        }

  /*
  / 3.8.1 - INTEIROS
  */
  
0[0-9]+                 yyerror("Invalid Integer"); /* inteiro que comece por 0 */
[0-9]+                  yylval.i = strtol(yytext, nullptr, 10); return tINTEGER; /* já estava no Simple */

"0x"                    yy_push_state(X_HEXNUM);
<X_HEXNUM>[0-9a-fA-F]+  yylval.i = strtol(yytext, nullptr, 16); return tINTEGER; 

  /*
  / 3.8.3 - CADEIAS DE CARACTERES
  */

\"                          yy_push_state(X_STRING);
<X_STRING>\\                yy_push_state(X_BACKSLASH);
<X_STRING>\"                {
                            yylval.s = new std::string(strliteral.str());
                            strliteral.str("");
                            yy_pop_state();
                            return tSTRING;
                            }

<X_STRING>\0                yyerror("NULL char in String");
<X_STRING>\n                yyerror("Newline in String");
<X_STRING>.                 strliteral << *yytext;

<X_BACKSLASH>t              strliteral << '\t'; yy_pop_state();
<X_BACKSLASH>n              strliteral << '\n'; yy_pop_state();
<X_BACKSLASH>r              strliteral << '\r'; yy_pop_state();
<X_BACKSLASH>\"             strliteral << '\"'; yy_pop_state();
<X_BACKSLASH>\\             strliteral << '\\'; yy_pop_state();

<X_BACKSLASH>0              yy_push_state(X_IGNORE_STRING); /* End of String */

<X_BACKSLASH>[0-7]{1,3}     {
                            unsigned long num = std::stoi(yytext, NULL, 8);
                            if (num > 255) {
                              yyerror("Invalid 8 bit Number");
                            }
                            strliteral << (char)(unsigned char) num;
                            yy_pop_state();
                            }

<X_BACKSLASH>.              strliteral << *yytext; yy_pop_state();

<X_IGNORE_STRING>\"         {
                            yylval.s = new std::string(strliteral.str());
                            strliteral.str("");
                            yy_pop_state(); yy_pop_state(); yy_pop_state(); /* Ignorar resto da string sair dos 3 estados */
                            return tSTRING;
                            }

<X_IGNORE_STRING>\n             yyerror("Newline in String");
<X_IGNORE_STRING>\0             yyerror("NULL char in String");
<X_IGNORE_STRING>.|\\\"|\\\\    strliteral << *yytext; yy_pop_state();

  /*
  / 4.5 - SÍMBOLOS GLOBAIS
  */

"external"              return tEXTERNAL;
"forward"               return tFORWARD;
"public"                return tPUBLIC;
"var"                   return tVAR;

  /*
  / 5 - FUNÇÕES
  */

[@]		                 return *yytext;
"program"              return tPROGRAM;

  /*
  / 6.1 - BLOCOS
  / 6.2 - INSTRUÇÃO CONDICIONAL
  / 6.3 - INSTRUÇÃO DE ITERAÇÃO
  / 6.4 - INSTRUÇÃO DE TERMINAÇÃO
  / 6.5 - INSTRUÇÃO DE CONTINUAÇÃO
  / 6.6 - ISNTRUÇÃO DE RETORNO
  / 6.8 - INSTRUÇÃO DE IMPRESSÃO
  */

"block"                return tBLOCK;
"if"                   return tIF;
"else"		       return tELSE;
"loop"                 return tLOOP;
"stop"                 return tSTOP;
"next"                 return tNEXT;
"return"               return tRETURN;
"print"                return tPRINT;
"println"              return tPRINTLN;
  
  /*
  / 7 - OPERADORES DE EXPRESSÃO
  */

[-+*/%?~;()!<>]     return *yytext;

">="                   return tGE;
"<="                   return tLE;
"=="                   return tEQ;
"!="                   return tNE;
"&&"                   return tAND;
"||"                   return tOR;

  /*
  / 7 - EXPRESSÕES ESPECIAIS
  */

"read"                 return tREAD;
"set"                  return tSET;
"index"                return tINDEX;
"objects"              return tOBJECTS;
"sizeof"               return tSIZEOF;
"function"             return tFUNCTION;

  /*
  / ESPECIAL
  */

"null"                    return tNULLPTR;

  /*
  / 3.7 - IDENTIFICADORES
  */

[A-Za-z][A-Za-z0-9_]*  yylval.s = new std::string(yytext); return tID;

  /*
  / 3.1 - CARACTERES BRANCOS
  */

[ \t\n\r]+               ; /* ignore whitespace */

  /*
  / RESTO
  */

.                      yyerror("Unknown character");

%%
