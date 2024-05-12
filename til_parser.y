%{
//-- don't change *any* of these: if you do, you'll break the compiler.
#include <algorithm>
#include <memory>
#include <cstring>
#include <cdk/compiler.h>
#include <cdk/types/types.h>
#include ".auto/all_nodes.h"
#define LINE                         compiler->scanner()->lineno()
#define yylex()                      compiler->scanner()->scan()
#define yyerror(compiler, s)         compiler->scanner()->error(s)
//-- don't change *any* of these --- END!
%}

%parse-param {std::shared_ptr<cdk::compiler> compiler}

%union {
  //--- don't change *any* of these: if you do, you'll break the compiler.
  YYSTYPE() : type(cdk::primitive_type::create(0, cdk::TYPE_VOID)) {}
  ~YYSTYPE() {}
  YYSTYPE(const YYSTYPE &other) { *this = other; }
  YYSTYPE& operator=(const YYSTYPE &other) { type = other.type; return *this; }

  std::shared_ptr<cdk::basic_type> type;        /* expression type */
  //-- don't change *any* of these --- END!

  int                   i;          /* integer value */
  double                d;          /* double value */
  std::string           *s;          /* symbol name or string literal */
  std::vector<std::shared_ptr<cdk::basic_type>> *types;

  cdk::basic_node       *node;       /* node pointer */
  cdk::sequence_node    *sequence;
  cdk::expression_node  *expression; /* expression nodes */
  cdk::lvalue_node      *lvalue;

  til::block_node       *block; 
};

%token <i> tINTEGER
%token <d> tDOUBLE
%token <s> tID tSTRING
%token tINT_TYPE tDOUBLE_TYPE tSTRING_TYPE tVOID_TYPE 
%token tBLOCK tIF tLOOP tSTOP tNEXT tRETURN tPRINT tPRINTLN
%token tREAD tSET tINDEX tOBJECTS tSIZEOF tFUNCTION
%token tAND tOR tPROGRAM
%token tEXTERNAL tFORWARD tPUBLIC tVAR tPRIVATE
%token tNULLPTR

%nonassoc tIFX
%nonassoc tELSE 

%right '='
%left tGE tLE tEQ tNE '>' '<'
%left '+' '-'
%left '*' '/' '%'
%nonassoc tUNARY

%type <sequence> file global_declarations declarations list exprs
%type <node> program stmt global_declaration declaration
%type <expression> expr literal init global_init opt_init opt_global_init fun_def
%type <lvalue> lval
%type <type> type void_type data_type fun_type return_type void_ptr var
%type <types> types
//%type <block> block

%{
//-- The rules below will be included in yyparse, the main parsing function.
%}
%%

file         : /* empty */                 { compiler->ast($$ = new cdk::sequence_node(LINE)); }
             | global_declarations         { compiler->ast($$ = $1); }
             |                     program { compiler->ast($$ = new cdk::sequence_node(LINE, $1)); }
             | global_declarations program { compiler->ast($$ = new cdk::sequence_node(LINE, $2, $1)); }
             ;

program : tPROGRAM list { $$ = new til::program_node(LINE, $2)); }
        ;

/* GLOBAL DECLARATIONS */

global_declarations : '(' global_declaration ')'                      { $$ = new cdk::sequence_node(LINE, $2); }
                    | global_declarations  '(' global_declaration ')' { $$ = new cdk::sequence_node(LINE, $3, $1); }
                    ;

global_declaration  : tEXTERNAL fun_type tID           { $$ = new til::declaration_node(LINE, tEXTERNAL, $2, *$3, nullptr); delete $3; }
                    | tFORWARD type tID                { $$ = new til::declaration_node(LINE, tFORWARD, $2, *$3, nullptr); delete $3; }
                    | tPUBLIC type tID opt_global_init { $$ = new til::declaration_node(LINE, tPUBLIC, $2, *$3, $4); delete $3; }
                    | tPUBLIC var tID global_init      { $$ = new til::declaration_node(LINE, tPUBLIC, $2, *$3, $4); delete $3; }
                    | type tID opt_global_init         { $$ = new til::declaration_node(LINE, tPRIVATE, $1, *$2, $3); delete $2; }
                    | var tID global_init              { $$ = new til::declaration_node(LINE, tPRIVATE, $1, *$2, $3); delete $2; }
                    ;

/* DECLARATION TYPE */

var : /* empty */                       { $$ = nullptr; }
    | tVAR                              { $$ = cdk::primitive_type::create(0, cdk::TYPE_UNSPEC); }
    ;

types : '(' type ')'                    { $$ = new std::vector<std::shared_ptr<cdk::basic_type>>(); $$->push_back($2); }
      | types '(' type ')'              { $$ = $1; $$->push_back($3); }
      ;

type : data_type                        { $$ = $1; }
     | void_ptr                         { $$ = $1; }
     ;

data_type : tINT_TYPE                   { $$ = cdk::primitive_type::create(4, cdk::TYPE_INT); }
          | tDOUBLE_TYPE                { $$ = cdk::primitive_type::create(8, cdk::TYPE_DOUBLE); }
          | tSTRING_TYPE                { $$ = cdk::primitive_type::create(4, cdk::TYPE_STRING); }
          | fun_type                    { $$ = $1; }
          | data_type '!'               { $$ = cdk::reference_type::create(4, $1); }
          ;

fun_type : return_type '(' types ')'    { $$ = cdk::functional_type::create(*$3, $1); delete $3; }
         | return_type '(' ')'          { $$ = cdk::functional_type::create($1); }
         ;

void_ptr : void_ptr '!'                 { $$ = $1; }
         | void_type '!'                { $$ = cdk::reference_type::create(0, $1); }
         ;

void_type : tVOID_TYPE                  { $$ = cdk::primitive_type::create(0, cdk::TYPE_VOID); }
          ;

return_type : type                      { $$ = $1; }
            | void_type                 { $$ = $1; }
            ;

/* INITIALIZATIONS */

/* se a variável for global deve ser um valor literal ou funcional */
opt_global_init : /* empty */ { $$ = nullptr; }
                | global_init { $$ = $1; }

global_init : literal              { $$ = $1; }
            | '(' fun_def ')'     { $$ = $2; }
            ;

opt_init : {;} // TODO: IMPLEMENTAR
         ;

init : {;} // TODO: IMPLEMENTAR
     ;

fun_def : {;} // TODO: IMPLEMENTAR
         ;

declarations : '(' declaration ')'              { $$ = new cdk::sequence_node(LINE, $2); }
             | declarations '(' declaration ')' { $$ = new cdk::sequence_node(LINE, $3, $1); } 
             ;

declaration  : {;} // TODO: IMPLEMENTAR
             ;

fun_dec : {;} // TODO: IMPLEMENTAR


list : stmt      { $$ = new cdk::sequence_node(LINE, $1); }
     | list stmt { $$ = new cdk::sequence_node(LINE, $2, $1); }
     ;

stmt : expr ';'                         { $$ = new til::evaluation_node(LINE, $1); }
     | tPRINT exprs ';'                 { $$ = new til::print_node(LINE, $2, false); }
     | tPRINTLN exprs ';'               { $$ = new til::print_node(LINE, $2, true); }
     | tREAD ';'                        { $$ = new til::read_node(LINE); }
     | tLOOP '(' expr ')' stmt          { $$ = new til::loop_node(LINE, $3, $5); }
     | tIF '(' expr ')' stmt %prec tIFX { $$ = new til::if_node(LINE, $3, $5); }
     | tIF '(' expr ')' stmt tELSE stmt { $$ = new til::if_else_node(LINE, $3, $5, $7); }
     | '{' list '}'                     { $$ = $2; }
     ;

exprs : {;} // TODO: IMPLEMENTAR
      ;

// ??? FALTAM '(' ')' ???
expr : literal               { $$ = $1; }
     | '-' expr %prec tUNARY { $$ = new cdk::unary_minus_node(LINE, $2); }
     | '+' expr %prec tUNARY { $$ = new cdk::unary_plus_node(LINE, $2); }
     | '+' expr expr         { $$ = new cdk::add_node(LINE, $2, $3); }
     | '-' expr expr         { $$ = new cdk::sub_node(LINE, $2, $3); }
     | '*' expr expr         { $$ = new cdk::mul_node(LINE, $2, $3); }
     | '/' expr expr         { $$ = new cdk::div_node(LINE, $2, $3); }
     | '%' expr expr         { $$ = new cdk::mod_node(LINE, $2, $3); }
     | '<' expr expr         { $$ = new cdk::lt_node(LINE, $2, $3); }
     | '>' expr expr         { $$ = new cdk::gt_node(LINE, $2, $3); }
     | tGE expr expr         { $$ = new cdk::ge_node(LINE, $2, $3); }
     | tLE expr expr         { $$ = new cdk::le_node(LINE, $2, $3); }
     | tNE expr expr         { $$ = new cdk::ne_node(LINE, $2, $3); }
     | tEQ expr expr         { $$ = new cdk::eq_node(LINE, $2, $3); }
     | tAND expr expr        { $$ = new cdk::and_node(LINE, $2, $3); }
     | tOR expr expr         { $$ = new cdk::or_node(LINE, $2, $3); }
     | '(' expr ')'          { $$ = $2; }
     | lval                  { $$ = new cdk::rvalue_node(LINE, $1); }
     | lval '=' expr         { $$ = new cdk::assignment_node(LINE, $1, $3); }
     ;

literal : tINTEGER              { $$ = new cdk::integer_node(LINE, $1); }
        | tDOUBLE               { $$ = new cdk::double_node(LINE, $1); }
        | tSTRING               { $$ = new cdk::string_node(LINE, $1); }
        | tNULLPTR              { $$ = new til::nullptr_node(LINE); }

lval : tID             { $$ = new cdk::variable_node(LINE, $1); }
     ;

%%
