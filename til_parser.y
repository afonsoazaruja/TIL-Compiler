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
%token <d> tDOUBLE
%token <s> tID tSTRING
%token tINT_TYPE tDOUBLE_TYPE tSTRING_TYPE tVOID_TYPE 
%token tBLOCK tIF tLOOP tSTOP tNEXT tRETURN tPRINT tPRINTLN
%token tREAD tSET tINDEX tOBJECTS tSIZEOF tFUNCTION
%token tAND tOR tPROGRAM
%token tEXTERNAL tFORWARD tPUBLIC tVAR tPRIVATE
%token tNULLPTR

%nonassoc tIF

%right '='
%left tGE tLE tEQ tNE '>' '<'
%left '+' '-'
%left '*' '/' '%'
%nonassoc tUNARY

%type <sequence> file declarations declarations list exprs
%type <node> program stmt declaration declaration
%type <expression> expr literal init init opt_init opt_init fun_def
%type <lvalue> lval
%type <type> type void_type data_type fun_type return_type void_ptr var
%type <types> types
//%type <block> block
%type <type> type void_type data_type fun_type return_type void_ptr var
%type <types> types
//%type <block> block

%{
//-- The rules below will be included in yyparse, the main parsing function.
%}
%%

file         : /* empty */                           { compiler->ast($$ = new cdk::sequence_node(LINE)); }
             | global_declarations                   { compiler->ast($$ = $1); }
             |                      program          { compiler->ast($$ = new cdk::sequence_node(LINE, $1)); }
             | global_declarations  program          { compiler->ast($$ = new cdk::sequence_node(LINE, $2, $1)); }
             ;

program : '(' tPROGRAM block ')' { $$ = new til::program_node(LINE, $3); }
        ;

/* GLOBAL DECLARATIONS */



/* DECLARATION TYPE */

global_declarations      : global_declaration                       { $$ = new cdk::sequence_node(LINE, $1); }
                         | global_declarations global_declaration   { $$ = new cdk::sequence_node(LINE, $2, $1); }
                         ;

global_declaration         : '(' tEXTERNAL fun_type tID            ')'     { $$ = new til::declaration_node(LINE, tEXTERNAL, $3, *$4, nullptr); delete $4; }
                           | '(' tFORWARD fun_type tID             ')'     { $$ = new til::declaration_node(LINE, tFORWARD, $3, *$4, nullptr); delete $4; }
                           | '(' tPUBLIC type tID opt_global_init  ')'     { $$ = new til::declaration_node(LINE, tPUBLIC, $3, *$4, $5); delete $4; }
                           | '(' tPUBLIC opt_var tID global_init   ')'     { $$ = new til::declaration_node(LINE, tPUBLIC, $3, *$4, $5); delete $4; }
                           | '(' type tID opt_global_init          ')'     { $$ = new til::declaration_node(LINE, tPRIVATE, $2, *$3, $4); delete $3; }
                           | '(' opt_var tID global_init           ')'     { $$ = new til::declaration_node(LINE, tPRIVATE, $2, *$3, $4); delete $3; }

opt_var : /* empty */                   { $$ = nullptr; }
    | var                               { $$ = $1; }
    ;

var : tVAR                              { $$ = cdk::primitive_type::create(0, cdk::TYPE_UNSPEC); }
      ;

types : type                            { $$ = new std::vector<std::shared_ptr<cdk::basic_type>>(); $$->push_back($1); }
      | types type                      { $$ = $1; $$->push_back($2); }
      ;

type : tINT_TYPE                        { $$ = cdk::primitive_type::create(4, cdk::TYPE_INT); }
     | tDOUBLE_TYPE                     { $$ = cdk::primitive_type::create(8, cdk::TYPE_DOUBLE); }
     | tSTRING_TYPE                     { $$ = cdk::primitive_type::create(4, cdk::TYPE_STRING); }
     | void_type                        { $$ = cdk::primitive_type::create(0, cdk::TYPE_VOID); }
     | type '!'                         { $$ = cdk::reference_type::create(4, $1); }                       
     | fun_type                         { $$ = $1; }
     ;


fun_type : '(' type ')'                 { $$ = cdk::functional_type::create($2); }
         | '(' type '(' types ')' ')'   { $$ = cdk::functional_type::create(*$4, $2); delete $4; }
         ;

block : '(' tBLOCK ')'                              { $$ = til::block_node(LINE, nullptr, nullptr);}
      | '(' tBLOCK declarations ')'                 { $$ = til::block_node(LINE, $3, nullptr);}
      | '(' tBLOCK instructions ')'                 { $$ = til::block_node(LINE, nullptr, $3);}
      | '(' tBLOCK declarations instructions ')'    { $$ = til::block_node(LINE, $3, $4);}
      ;

declarations      : declaration                     { $$ = new cdk::sequence_node(LINE, $1); }
                  | declarations declaration        { $$ = new cdk::sequence_node(LINE, $2, $1); }
                  ;

declaration    : '(' type tID opt_init      ')'     { $$ = new til::declaration_node(LINE, tPRIVATE, $2, *$3, $4); delete $3; }
               | '(' var tID init           ')'     { $$ = new til::declaration_node(LINE, tPRIVATE, $2, *$3, $4); delete $3; }


intructions : instruction                           { $$ = new cdk::sequence_node(LINE, $1); }
            | instructions instruction              { $$ = new cdk::sequence_node(LINE, $2, $1); }
            ;

instruction : expr                                  { $$ = $1; }    
            | '(' tPRINT exprs ')'                  { $$ = til::print_node(LINE, $3, false); }
            | '(' tPRINTLN exprs ')'                { $$ = til::print_node(LINE, $3, true); }     
            | '(' tSTOP opt_int ')'                 { $$ = til::stop_node(LINE, $3); }
            | '(' tNEXT opt_int ')'                 { $$ = til::next_node(LINE, $3); }
            | '(' tRETURN expr ')'                  { $$ = til::return_node(LINE, $3); } 
            | conditional_instruction               { $$ = $1; } 
            | iteration_instruction                 { $$ = $1; }  
            | block                                 { $$ = $1; }
            ;     
             
conditional_instruction : '(' tIF expr instruction ')'            { $$ = til::if_node(LINE,$3, $4);}
                        | '(' tIF expr instruction instruction')' { $$ = til::if_else_node(LINE,$3, $4, $5);}
                        ;

iteration_instruction  : '(' tLOOP expr instruction ')'           { $$ = til::loop_node(LINE,$3, $4);}
                       ;

/* INITIALIZATIONS */

opt_global_init : : /* empty */            { $$ = nullptr; }
                  | global_init            { $$ = $1; }
                  ;

global_init : literal                     { $$ = $1; } 
            | fun                         { $$ = $1; }
            ;

opt_init  : /* empty */            { $$ = nullptr; }
          | init                   { $$ = $1; }
          ;

init : expr                     { $$ = $1; } 
     ;


fun : '(' tFUNCTION '(' type opt_declarations ')' block ')'     { $$ = new til::function_definition_node(LINE, $4, $5, $7);}
    ;

opt_declarations : /* empty */        { $$ = nullptr; }
                 | declarations       {$$ = $1;} 
                 ;  

exprs : expr                 { $$ = new cdk::sequence_node(LINE, $1); }
      | exprs expr           { $$ = new cdk::sequence_node(LINE, $2, $1); }
      ;

expr : literal                   { $$ = $1; }
     | '(' expr ')'              { $$ = $2; }
     | '-' expr                  { $$ = new cdk::unary_minus_node(LINE, $2); }
     | '+' expr                  { $$ = new cdk::unary_plus_node(LINE, $2); }
     | '+' expr expr             { $$ = new cdk::add_node(LINE, $2, $3); }
     | '-' expr expr             { $$ = new cdk::sub_node(LINE, $2, $3); }
     | '?' lval                  { $$ = new til::address_of_node(LINE, $2); }
     | '*' expr expr             { $$ = new cdk::mul_node(LINE, $2, $3); }
     | '/' expr expr             { $$ = new cdk::div_node(LINE, $2, $3); }
     | '%' expr expr             { $$ = new cdk::mod_node(LINE, $2, $3); }
     | '<' expr expr             { $$ = new cdk::lt_node(LINE, $2, $3); }
     | '>' expr expr             { $$ = new cdk::gt_node(LINE, $2, $3); }
     | tGE expr expr             { $$ = new cdk::ge_node(LINE, $2, $3); }
     | tLE expr expr             { $$ = new cdk::le_node(LINE, $2, $3); }
     | tNE expr expr             { $$ = new cdk::ne_node(LINE, $2, $3); }
     | tEQ expr expr             { $$ = new cdk::eq_node(LINE, $2, $3); }
     | '~' expr                  { $$ = new cdl::not_node(LINE, $2);}
     | tAND expr expr            { $$ = new cdk::and_node(LINE, $2, $3); }
     | tOR expr expr             { $$ = new cdk::or_node(LINE, $2, $3); }
     | tSET lval expr            { $$ = new cdk::assignment_node(LINE, $2, $3); }
     | expr '!'                  { $$ = new til::stack_alloc_node(LINE, $1);} 
     | tSIZEOF expr              { $$ = new til::sizeof_node(LINE, $2);}
     | tREAD                     { $$ = new til::read_node(LINE);} 
     | expr '(' opt_exprs ')'    { $$ = new til::function_call_node(LINE, $1, $3);}
     | '@' '(' opt_exprs ')'     { $$ = new til::function_call_node(LINE, nullptr, $3);}
     | fun                       { $$ = $1;}
     | lval                      { $$ = new cdk::rvalue_node(LINE, $1); }
     ;

opt_exprs :  /* empty */           { $$ = nullptr; }
          | exprs                  { $$ = $1; }
          ;

literal : tINTEGER              { $$ = new cdk::integer_node(LINE, $1); }
        | tDOUBLE               { $$ = new cdk::double_node(LINE, $1); }
        | tSTRING               { $$ = new cdk::string_node(LINE, $1); }
        | tNULLPTR              { $$ = new til::nullptr_node(LINE); }
        ;

opt_int : /* empty */           { $$ = nullptr; }
        | tINTEGER              { $$ = new cdk::integer_node(LINE, $1); }
        ;  

lval : tID                      { $$ = new cdk::variable_node(LINE, $1); }
     ;

%%
