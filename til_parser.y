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
  cdk::basic_node       *node;       /* node pointer */
  cdk::sequence_node    *sequence;
  cdk::expression_node  *expression; /* expression nodes */
  cdk::lvalue_node      *lvalue;
  til::declaration_node *decl;
  std::shared_ptr<cdk::basic_type> vartype;
};

%token <i> tINTEGER
%token <d> tDOUBLE
%token <s> tIDENTIFIER tSTRING
%token tINT_TYPE tDOUBLE_TYPE tSTRING_TYPE tVOID_TYPE 
%token tBLOCK tIF tLOOP tSTOP tNEXT tRETURN tPRINT tPRINTLN
%token tREAD tSET tINDEX tOBJECTS tSIZEOF tFUNCTION
%token tAND tOR tBEGIN tEND
%token tEXTERNAL tFORWARD tPUBLIC tVAR tPRIVATE
%token tNULLPTR

%nonassoc tIFX
%nonassoc tELSE

%right '='
%left tGE tLE tEQ tNE '>' '<'
%left '+' '-'
%left '*' '/' '%'
%nonassoc tUNARY

%type <decl> declaration
%type <node> stmt program
%type <sequence> list file declarations
%type <expression> expr literal
%type <lvalue> lval
%type <vartype> type

%{
//-- The rules below will be included in yyparse, the main parsing function.
%}
%%

file         : /* empty */  { compiler->ast($$ = new cdk::sequence_node(LINE)); }
             | declarations { compiler->ast($$ = $1); }
             ;

declarations : declaration              { $$ = new cdk::sequence_node(LINE, $1); }
             | declarations declaration { $$ = new cdk::sequence_node(LINE, $2, $1); } 
             ;

declaration  : '(' type tIDENTIFIER expr ')' { $$ = new til::declaration_node(LINE, tPRIVATE, $2, *$3, $4); delete $3; }
             ; // FALTAM OUTRAS DECLARAÇÕES

program : tBEGIN list tEND { compiler->ast(new til::program_node(LINE, $2)); }
        ;

// function :


type : tINT_TYPE         { $$ = cdk::primitive_type::create(4, cdk::TYPE_INT); }
     | tDOUBLE_TYPE      { $$ = cdk::primitive_type::create(8, cdk::TYPE_DOUBLE); }
     | tSTRING_TYPE      { $$ = cdk::primitive_type::create(4, cdk::TYPE_STRING); }
     | tVOID_TYPE        { $$ = cdk::reference_type::create(4, cdk::primitive_type::create(4, cdk::TYPE_VOID)); }
     ; // FALTA PONTEIRO???

// types :

// block :

// block_instr :

// if_instr :

// loop_instr :

list : stmt      { $$ = new cdk::sequence_node(LINE, $1); }
     | list stmt { $$ = new cdk::sequence_node(LINE, $2, $1); }
     ;

stmt : expr ';'                         { $$ = new til::evaluation_node(LINE, $1); }
     | tPRINT expr ';'                  { $$ = new til::print_node(LINE, $2, false); }
     | tPRINTLN expr ';'                { $$ = new til::print_node(LINE, $2, true); }
     | tREAD lval ';'                   { $$ = new til::read_node(LINE, $2); }
     | tLOOP '(' expr ')' stmt          { $$ = new til::loop_node(LINE, $3, $5); }
     | tIF '(' expr ')' stmt %prec tIFX { $$ = new til::if_node(LINE, $3, $5); }
     | tIF '(' expr ')' stmt tELSE stmt { $$ = new til::if_else_node(LINE, $3, $5, $7); }
     | '{' list '}'                     { $$ = $2; }
     ;

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

lval : tIDENTIFIER             { $$ = new cdk::variable_node(LINE, $1); }
     ;

%%
