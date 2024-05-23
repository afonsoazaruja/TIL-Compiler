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
%token tGE tLE tEQ tNE

%type <sequence> declarations exprs instructions global_declarations 
%type <node> file program declaration instruction global_declaration conditional_instruction iteration_instruction
%type <expression> expr literal init global_init fun
%type <lvalue> lval
%type <type> data_type fun_type void_pointer return_type var_type var
%type <types> var_types
%type <block> block

%{
//-- The rules below will be included in yyparse, the main parsing function.
%}
%%

file : /* empty */                           { compiler->ast($$ = new cdk::sequence_node(LINE)); }
     | global_declarations                   { compiler->ast($$ = $1); }
     |                      program          { compiler->ast($$ = new cdk::sequence_node(LINE, $1)); }
     | global_declarations  program          { compiler->ast($$ = new cdk::sequence_node(LINE, $2, $1)); }
     ;

program : '(' tPROGRAM block ')'    { $$ = new til::program_node(LINE, $3); }
        ;

global_declarations      : global_declaration                           { $$ = new cdk::sequence_node(LINE, $1); }
                         | global_declarations global_declaration       { $$ = new cdk::sequence_node(LINE, $2, $1); }
                         ;

global_declaration         : '(' tEXTERNAL fun_type tID ')'           { $$ = new til::declaration_node(LINE, tEXTERNAL, $3, *$4, nullptr); delete $4; }
                           | '(' tFORWARD fun_type tID ')'            { $$ = new til::declaration_node(LINE, tFORWARD, $3, *$4, nullptr); delete $4; }
                           | '(' tPUBLIC var_type tID global_init ')' { $$ = new til::declaration_node(LINE, tPUBLIC, $3, *$4, $5); delete $4; }
                           | '(' tPUBLIC var_type tID ')'             { $$ = new til::declaration_node(LINE, tPUBLIC, $3, *$4, nullptr); delete $4; }
                           | '(' tPUBLIC var tID global_init ')'      { $$ = new til::declaration_node(LINE, tPUBLIC, $3, *$4, $5); delete $4; }
                           | '(' tPUBLIC tID global_init ')'          { $$ = new til::declaration_node(LINE, tPUBLIC, cdk::primitive_type::create(0, cdk::TYPE_UNSPEC), *$3, $4); delete $3; }
                           | '(' var_type tID global_init ')'         { $$ = new til::declaration_node(LINE, tPRIVATE, $2, *$3, $4); delete $3; }
                           | '(' var_type tID ')'                     { $$ = new til::declaration_node(LINE, tPRIVATE, $2, *$3, nullptr); delete $3; }
                           | '(' var tID global_init ')'              { $$ = new til::declaration_node(LINE, tPRIVATE, $2, *$3, $4); delete $3; }
                           | '(' tID global_init ')'                  { $$ = new til::declaration_node(LINE, tPRIVATE, cdk::primitive_type::create(0, cdk::TYPE_UNSPEC), *$2, $3); delete $2; }

var : tVAR  { $$ = cdk::primitive_type::create(0, cdk::TYPE_UNSPEC); }
    ;

var_types : var_type            { $$ = new std::vector<std::shared_ptr<cdk::basic_type>>(); $$->push_back($1); }
          | var_types var_type      { $$ = $1; $$->push_back($2); }
          ;

data_type : tINT_TYPE         { $$ = cdk::primitive_type::create(4, cdk::TYPE_INT); }
          | tDOUBLE_TYPE      { $$ = cdk::primitive_type::create(8, cdk::TYPE_DOUBLE); }
          | tSTRING_TYPE      { $$ = cdk::primitive_type::create(4, cdk::TYPE_STRING); }
          | fun_type          { $$ = $1; }
          | data_type '!'     { $$ = cdk::reference_type::create(4, $1); }
          ;

var_type : data_type     { $$ = $1; }
         | void_pointer  { $$ = $1; }
         ;

void_pointer : void_pointer '!'    { $$ = $1; }
             | tVOID_TYPE '!'      { $$ = cdk::reference_type::create(4, cdk::primitive_type::create(0, cdk::TYPE_VOID)); }
             ;

fun_type : '(' return_type ')'                    { $$ = cdk::functional_type::create($2); }
         | '(' return_type '(' var_types ')' ')'  { $$ = cdk::functional_type::create(*$4, $2); delete $4; }
         ;

return_type : var_type   { $$ = $1; }
            | tVOID_TYPE { $$ = cdk::primitive_type::create(0, cdk::TYPE_VOID); }
            ;

block : /* empty */                 { $$ = new til::block_node(LINE, nullptr, nullptr);}
      | declarations                { $$ = new til::block_node(LINE, $1, nullptr);}
      | instructions                { $$ = new til::block_node(LINE, nullptr, $1);}
      | declarations instructions   { $$ = new til::block_node(LINE, $1, $2);}
      ;

declarations : declaration                { $$ = new cdk::sequence_node(LINE, $1); }
             | declarations declaration   { $$ = new cdk::sequence_node(LINE, $2, $1); }
             ;

declaration    : '(' var_type tID init ')'   { $$ = new til::declaration_node(LINE, tPRIVATE, $2, *$3, $4); delete $3; }
               | '(' var_type tID ')'        { $$ = new til::declaration_node(LINE, tPRIVATE, $2, *$3, nullptr); delete $3; }
               | '(' var tID init ')'        { $$ = new til::declaration_node(LINE, tPRIVATE, $2, *$3, $4); delete $3; }
               ;

instructions : instruction                { $$ = new cdk::sequence_node(LINE, $1); }
             | instructions instruction   { $$ = new cdk::sequence_node(LINE, $2, $1); }
             ;

instruction : expr                        { $$ = $1; }
            | '(' tPRINT exprs ')'        { $$ = new til::print_node(LINE, $3, false); }
            | '(' tPRINTLN exprs ')'      { $$ = new til::print_node(LINE, $3, true); }     
            | '(' tSTOP tINTEGER ')'      { $$ = new til::stop_node(LINE, $3); }
            | '(' tSTOP ')'               { $$ = new til::stop_node(LINE); }
            | '(' tNEXT tINTEGER ')'      { $$ = new til::next_node(LINE, $3); }
            | '(' tNEXT ')'               { $$ = new til::next_node(LINE); }
            | '(' tRETURN expr ')'        { $$ = new til::return_node(LINE, $3); } 
            | conditional_instruction     { $$ = $1; } 
            | iteration_instruction       { $$ = $1; }  
            | '(' tBLOCK block ')'        { $$ = $3; }
            ;

conditional_instruction : '(' tIF expr instruction ')'            { $$ = new til::if_node(LINE, $3, $4);}
                        | '(' tIF expr instruction instruction')' { $$ = new til::if_else_node(LINE, $3, $4, $5);}
                        ;

iteration_instruction  : '(' tLOOP expr instruction ')'     { $$ = new til::loop_node(LINE, $3, $4);}
                       ;

global_init : literal   { $$ = $1; } 
            | fun       { $$ = $1; }
            ;

init : expr { $$ = $1; } 
     ;


fun : '(' tFUNCTION '(' return_type declarations ')' block ')'     { $$ = new til::function_definition_node(LINE, $4, $5, $7);}
    | '(' tFUNCTION '(' return_type ')' block ')'                  { $$ = new til::function_definition_node(LINE, $4, new cdk::sequence_node(LINE), $6);}
    ;

exprs : expr            { $$ = new cdk::sequence_node(LINE, $1); }
      | exprs expr      { $$ = new cdk::sequence_node(LINE, $2, $1); }
      ;

expr : literal                            { $$ = $1; }
     | '(' '-' expr ')'                   { $$ = new cdk::unary_minus_node(LINE, $3); }
     | '(' '+' expr ')'                   { $$ = new cdk::unary_plus_node(LINE, $3); }
     | '(' '+' expr expr ')'              { $$ = new cdk::add_node(LINE, $3, $4); }
     | '(' '-' expr expr ')'              { $$ = new cdk::sub_node(LINE, $3, $4); }
     | '(' '?' lval ')'                   { $$ = new til::address_of_node(LINE, $3); }
     | '(' '*' expr expr ')'              { $$ = new cdk::mul_node(LINE, $3, $4); }
     | '(' '/' expr expr ')'              { $$ = new cdk::div_node(LINE, $3, $4); }
     | '(' '%' expr expr ')'              { $$ = new cdk::mod_node(LINE, $3, $4); }
     | '(' '<' expr expr ')'              { $$ = new cdk::lt_node(LINE, $3, $4); }
     | '(' '>' expr expr ')'              { $$ = new cdk::gt_node(LINE, $3, $4); }
     | '(' tGE expr expr ')'              { $$ = new cdk::ge_node(LINE, $3, $4); }
     | '(' tLE expr expr ')'              { $$ = new cdk::le_node(LINE, $3, $4); }
     | '(' tNE expr expr ')'              { $$ = new cdk::ne_node(LINE, $3, $4); }
     | '(' tEQ expr expr ')'              { $$ = new cdk::eq_node(LINE, $3, $4); }
     | '(' '~' expr ')'                   { $$ = new cdk::not_node(LINE, $3);}
     | '(' tAND expr expr ')'             { $$ = new cdk::and_node(LINE, $3, $4); }
     | '(' tOR expr expr ')'              { $$ = new cdk::or_node(LINE, $3, $4); }
     | '(' tSET lval expr ')'             { $$ = new cdk::assignment_node(LINE, $3, $4); }
     | '(' tSIZEOF expr ')'               { $$ = new til::sizeof_node(LINE, $3); }
     | '(' tOBJECTS expr ')'              { $$ = new til::stack_alloc_node(LINE, $3); }
     | '(' tREAD ')'                      { $$ = new til::read_node(LINE); }
     | '(' expr exprs ')'                 { $$ = new til::function_call_node(LINE, $2, $3); }
     | '(' expr ')'                       { $$ = new til::function_call_node(LINE, $2, new cdk::sequence_node(LINE)); }
     | '(' '@' exprs ')'                  { $$ = new til::function_call_node(LINE, nullptr, $3); }   
     | '(' '@' ')'                        { $$ = new til::function_call_node(LINE, nullptr, new cdk::sequence_node(LINE)); }
     | fun                                { $$ = $1;}
     | lval                               { $$ = new cdk::rvalue_node(LINE, $1); }
     ;

literal : tINTEGER      { $$ = new cdk::integer_node(LINE, $1); }
        | tDOUBLE       { $$ = new cdk::double_node(LINE, $1); }
        | tSTRING       { $$ = new cdk::string_node(LINE, $1); }
        | tNULLPTR      { $$ = new til::nullptr_node(LINE); }
        ;

lval : tID                          { $$ = new cdk::variable_node(LINE, $1); }
     | '(' tINDEX expr expr ')'     { $$ = new til::index_node(LINE, $3, $4); } 
     ;

%%
