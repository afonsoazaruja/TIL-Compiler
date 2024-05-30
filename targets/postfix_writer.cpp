#include <string>
#include <sstream>
#include "targets/type_checker.h"
#include "targets/postfix_writer.h"
#include ".auto/all_nodes.h"  // all_nodes.h is automatically generated

#include "til_parser.tab.h"

//-------------------------------------EMPTY---------------------------------

void til::postfix_writer::do_nil_node(cdk::nil_node * const node, int lvl) {
  // EMPTY
}
void til::postfix_writer::do_data_node(cdk::data_node * const node, int lvl) {
  // EMPTY
}
//---------------------------------------------------------------------------



void til::postfix_writer::do_double_node(cdk::double_node * const node, int lvl) {
  
}
void til::postfix_writer::do_not_node(cdk::not_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->argument()->accept(this, lvl + 2);
  _pf.INT(0);
  _pf.EQ();
}
void til::postfix_writer::do_and_node(cdk::and_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  const auto lbl = mklbl(++_lbl);
  node->left()->accept(this, lvl + 2);
  _pf.DUP32();
  _pf.JZ(lbl);
  node->right()->accept(this, lvl + 2);
  _pf.AND();
  _pf.ALIGN();
  _pf.LABEL(lbl);

}
void til::postfix_writer::do_or_node(cdk::or_node * const node, int lvl) {
  
}

//---------------------------------------------------------------------------

void til::postfix_writer::do_sequence_node(cdk::sequence_node * const node, int lvl) {
  for (size_t i = 0; i < node->size(); i++) {
    node->node(i)->accept(this, lvl);
  }
}

//---------------------------------------------------------------------------

void til::postfix_writer::do_integer_node(cdk::integer_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  if (_inFunctionBody) {
    _pf.INT(node->value()); // stored during function scope
  }
  else {
    _pf.SINT(node->value()); // stored globally
  }
}

void til::postfix_writer::do_string_node(cdk::string_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  int lbl1;

  /* generate the string */
  _pf.RODATA(); // strings are DATA readonly
  _pf.ALIGN(); // make sure we are aligned
  _pf.LABEL(mklbl(lbl1 = ++_lbl)); // give the string a name
  _pf.SSTRING(node->value()); // output string characters

  /* leave the address on the stack */
  _pf.TEXT(); // return to the TEXT segment
  _pf.ADDR(mklbl(lbl1)); // the string to be printed
}

//---------------------------------------------------------------------------

void til::postfix_writer::do_unary_minus_node(cdk::unary_minus_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->argument()->accept(this, lvl); // determine the value
  _pf.NEG(); // 2-complement
}

void til::postfix_writer::do_unary_plus_node(cdk::unary_plus_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->argument()->accept(this, lvl); // determine the value
}

//---------------------------------------------------------------------------

void til::postfix_writer::do_add_node(cdk::add_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->left()->accept(this, lvl);
  node->right()->accept(this, lvl);
  _pf.ADD();
}
void til::postfix_writer::do_sub_node(cdk::sub_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->left()->accept(this, lvl);
  node->right()->accept(this, lvl);
  _pf.SUB();
}

void til::postfix_writer::processTypeMultiplicative(cdk::binary_operation_node *const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->left()->accept(this, lvl + 2);
  if (node->is_typed(cdk::TYPE_DOUBLE) &&
      !node->left()->is_typed(cdk::TYPE_DOUBLE))
    _pf.I2D();

  node->right()->accept(this, lvl + 2);
  if (node->is_typed(cdk::TYPE_DOUBLE) &&
      !node->right()->is_typed(cdk::TYPE_DOUBLE))
    _pf.I2D();
}

void til::postfix_writer::do_mul_node(cdk::mul_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  processTypeMultiplicative(node, lvl);
  if (node->is_typed(cdk::TYPE_DOUBLE)) {
    _pf.DMUL();
  } else {
    _pf.MUL();
  }
}
void til::postfix_writer::do_div_node(cdk::div_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  processTypeMultiplicative(node, lvl);
  if (node->is_typed(cdk::TYPE_DOUBLE)) {
    _pf.DDIV();
  } else {
    _pf.DIV();
  }
}
void til::postfix_writer::do_mod_node(cdk::mod_node * const node, int lvl) { 
  // only works with integers
  // TODO: verificar
  ASSERT_SAFE_EXPRESSIONS;
  if (node->is_typed(cdk::TYPE_DOUBLE)) {
    error(node->lineno(), "integer expression expected");
  }
  else {
    _pf.MOD();
  }
}

void til::postfix_writer::processLogicalExpression(cdk::binary_operation_node *const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->left()->accept(this, lvl + 2);
  if (!node->left()->is_typed(cdk::TYPE_DOUBLE) && node->right()->is_typed(cdk::TYPE_DOUBLE)) {
    _pf.I2D();
  }
  
  node->right()->accept(this, lvl + 2);
  if (node->left()->is_typed(cdk::TYPE_DOUBLE) && !node->right()->is_typed(cdk::TYPE_DOUBLE)) {
    _pf.I2D();
  }

  if (node->left()->is_typed(cdk::TYPE_DOUBLE) || node->right()->is_typed(cdk::TYPE_DOUBLE)) {
    _pf.DCMP();
    _pf.INT(0);
  }
}

void til::postfix_writer::do_lt_node(cdk::lt_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  processLogicalExpression(node, lvl);
  _pf.LT();
}
void til::postfix_writer::do_le_node(cdk::le_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  processLogicalExpression(node, lvl);
  _pf.LE();
}
void til::postfix_writer::do_ge_node(cdk::ge_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  processLogicalExpression(node, lvl);
  _pf.GE();
}
void til::postfix_writer::do_gt_node(cdk::gt_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  processLogicalExpression(node, lvl);
  _pf.GT();
}
void til::postfix_writer::do_ne_node(cdk::ne_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  processLogicalExpression(node, lvl);
  _pf.NE();
}
void til::postfix_writer::do_eq_node(cdk::eq_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  processLogicalExpression(node, lvl);
  _pf.EQ();
}

//---------------------------------------------------------------------------

void til::postfix_writer::do_variable_node(cdk::variable_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  // simplified generation: all variables are global
  _pf.ADDR(node->name());
}

void til::postfix_writer::do_rvalue_node(cdk::rvalue_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->lvalue()->accept(this, lvl);
  _pf.LDINT(); // depends on type size
}

void til::postfix_writer::do_assignment_node(cdk::assignment_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->rvalue()->accept(this, lvl); // determine the new value
  _pf.DUP32();
  if (new_symbol() == nullptr) {
    node->lvalue()->accept(this, lvl); // where to store the value
  } else {
    _pf.DATA(); // variables are all global and live in DATA
    _pf.ALIGN(); // make sure we are aligned
    _pf.LABEL(new_symbol()->name()); // name variable location
    reset_new_symbol();
    _pf.SINT(0); // initialize it to 0 (zero)
    _pf.TEXT(); // return to the TEXT segment
    node->lvalue()->accept(this, lvl);  //DAVID: bah!
  }
  _pf.STINT(); // store the value at address
}

//----------------------------------TIL--------------------------------------

void til::postfix_writer::do_program_node(til::program_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  // Note that Simple doesn't have functions. Thus, it doesn't need
  // a function node. However, it must start in the main function.
  // The ProgramNode (representing the whole program) doubles as a
  // main function node.
  _inFunctionBody = true;
  // generate the main function (RTS mandates that its name be "_main")
  _pf.BSS();
  _pf.TEXT();
  _pf.ALIGN();
  _pf.GLOBAL("_main", _pf.FUNC());
  _pf.LABEL("_main");
  _pf.ENTER(0);  // Simple doesn't implement local variables

  node->block()->accept(this, lvl + 2); // this will also generate the code to execute the block

  // end the main function
  _pf.INT(0);
  _pf.STFVAL32();
  _pf.LEAVE();
  _pf.RET();
  
  // these are just a few library function imports<
  _pf.EXTERN("readi");
  _pf.EXTERN("printi");
  _pf.EXTERN("printd");
  _pf.EXTERN("prints");
  _pf.EXTERN("println");
}

//---------------------------------------------------------------------------

void til::postfix_writer::do_evaluation_node(til::evaluation_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->argument()->accept(this, lvl); // determine the value
  if (node->argument()->is_typed(cdk::TYPE_INT)) {
    _pf.TRASH(4); // delete the evaluated value
  } else if (node->argument()->is_typed(cdk::TYPE_STRING)) {
    _pf.TRASH(4); // delete the evaluated value's address
  } else {
    std::cerr << "ERROR: CANNOT HAPPEN!" << std::endl;
    exit(1);
  }
}

void til::postfix_writer::do_print_node(til::print_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  for (size_t ix = 0; ix < node->arguments()->size(); ix++) {
    const auto arg =
        dynamic_cast<cdk::expression_node *>(node->arguments()->node(ix));
    arg->accept(this, lvl); // determine the value to print
    if (arg->is_typed(cdk::TYPE_INT)) {
      _pf.CALL("printi");
      _pf.TRASH(4);
    } else if (arg->is_typed(cdk::TYPE_DOUBLE)) {
      _pf.CALL("printd");
      _pf.TRASH(8);
    } else if (arg->is_typed(cdk::TYPE_STRING)) {
      _pf.CALL("prints");
      _pf.TRASH(4);
    } else
      error(node->lineno(), "cannot print expression of unknown type");
  }
  if (node->newLine()) {
    _pf.CALL("println");
  }
}

//---------------------------------------------------------------------------

void til::postfix_writer::do_read_node(til::read_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  _pf.CALL("readi");
  _pf.LDFVAL32();
  _pf.STINT();
}

//---------------------------------------------------------------------------

void til::postfix_writer::do_loop_node(til::loop_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  int lbl1, lbl2;
  _pf.LABEL(mklbl(lbl1 = ++_lbl));
  node->condition()->accept(this, lvl);
  _pf.JZ(mklbl(lbl2 = ++_lbl));
  node->block()->accept(this, lvl + 2);
  _pf.JMP(mklbl(lbl1));
  _pf.LABEL(mklbl(lbl2));
}

//---------------------------------------------------------------------------

void til::postfix_writer::do_if_node(til::if_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  int lbl1;
  node->condition()->accept(this, lvl);
  _pf.JZ(mklbl(lbl1 = ++_lbl));
  node->block()->accept(this, lvl + 2);
  _pf.LABEL(mklbl(lbl1));
}

void til::postfix_writer::do_if_else_node(til::if_else_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  int lbl1, lbl2;
  node->condition()->accept(this, lvl);
  _pf.JZ(mklbl(lbl1 = ++_lbl));
  node->thenblock()->accept(this, lvl + 2);
  _pf.JMP(mklbl(lbl2 = ++_lbl));
  _pf.LABEL(mklbl(lbl1));
  node->elseblock()->accept(this, lvl + 2);
  _pf.LABEL(mklbl(lbl1 = lbl2));
}

//----------------------------------NEW--------------------------------------

void til::postfix_writer::do_declaration_node(til::declaration_node *const node, int lvl) {
  // ASSERT_SAFE_EXPRESSIONS;
  // const auto id = node->identifier();
  // const auto type_size = node->type()->size();
  // int offset = 0;

  // if (_inFunctionArgs) {
  //   offset = _offset;
  //   _offset += type_size;
  // } else if (_inFunctionBody) {
  //   _offset -= type_size;
  //   offset = _offset;
  // }

  // const auto symbol = new_symbol();
  // if (symbol) {
  //   symbol->set_offset(offset);
  //   reset_new_symbol();
  // }

  // // we may still need to initialize the variable
  // if (node->init()) {
  //   if (_inFunctionBody)
  //     processLocalVariableInitialization(symbol, node->init(), lvl);
  //   else
  //     processGlobalVariableInitialization(symbol, node->init(), lvl);
  //   _symbolsToDeclare.erase(symbol->name());
  // } else if (!_inFunctionArgs && !_inFunctionBody)
  //   _symbolsToDeclare.insert(symbol->name());
}

void til::postfix_writer::processLocalVariableInitialization(
    std::shared_ptr<til::symbol> symbol,
    cdk::expression_node *const initializer, int lvl) {
  // initializer->accept(this, lvl);
  // switch (symbol->type()->name()) {
  // case cdk::TYPE_INT:
  // case cdk::TYPE_STRING:
  // case cdk::TYPE_POINTER:
  // case cdk::TYPE_FUNCTIONAL:
  // case cdk::TYPE_UNSPEC: // cases such as `auto x = input;`
  //   _pf.LOCAL(symbol->offset());
  //   _pf.STINT();
  //   break;
  // case cdk::TYPE_DOUBLE:
  //   if (initializer->is_typed(cdk::TYPE_INT))
  //     _pf.I2D();
  //   _pf.LOCAL(symbol->offset());
  //   _pf.STDOUBLE();
  //   break;
  // default:
  //   error(initializer->lineno(), "invalid type for variable initialization");
  // }
}

void til::postfix_writer::processGlobalVariableInitialization(
    std::shared_ptr<til::symbol> symbol,
    cdk::expression_node *const initializer, int lvl) {
  // switch (symbol->type()->name()) {
  //   case cdk::TYPE_INT:
  //   case cdk::TYPE_STRING:
  //   case cdk::TYPE_POINTER:
  //     _pf.DATA(); // Data segment, for global variables
  //     _pf.ALIGN();
  //     _pf.LABEL(symbol->name());
  //     initializer->accept(this, lvl + 2);
  //     break;
  //   case cdk::TYPE_DOUBLE:
  //     _pf.DATA(); // Data segment, for global variables
  //     _pf.ALIGN();
  //     _pf.LABEL(symbol->name());

  //     // the following initializations need to be done outside of the switch
  //     const cdk::integer_node *dclini;
  //     cdk::double_node *ddi;
  //     switch (initializer->type()->name()) {
  //       case cdk::TYPE_INT:
  //         // here, we actually want to initialize the variable with a double
  //         // thus, we need to convert the expression to a double node
  //         // NOTE: I don't like these variable names either, taken from DM
  //         dclini = dynamic_cast<const cdk::integer_node *>(initializer);
  //         ddi = new cdk::double_node(dclini->lineno(), dclini->value());
  //         ddi->accept(this, lvl + 2);
  //         break;
  //       case cdk::TYPE_DOUBLE:
  //         initializer->accept(this, lvl + 2);
  //         break;
  //       default:
  //         error(initializer->lineno(),
  //               "invalid type for double variable initialization");
  //     }
  //     break;
  //   case cdk::TYPE_FUNCTIONAL:
  //     _functions.push_back(symbol);
  //     initializer->accept(this, lvl);
  //     _pf.DATA(); // Data segment, for global variables
  //     _pf.ALIGN();
  //     if (symbol->qualifier() == tPUBLIC)
  //       _pf.GLOBAL(symbol->name(), _pf.OBJ());
  //     _pf.LABEL(symbol->name());
  //     _pf.SADDR(_functionLabels.back());
  //     break;
  //   default:
  //     error(initializer->lineno(), "invalid type for variable initialization");
  // }
}



void til::postfix_writer::do_nullptr_node(til::nullptr_node *const node, int lvl) {}

void til::postfix_writer::do_function_definition_node(til::function_definition_node *const node, int lvl) {}

void til::postfix_writer::do_block_node(til::block_node *const node, int lvl) {
  _symtab.push();
  if (node->declarations())
    node->declarations()->accept(this, lvl + 2);
  if (node->instructions())
    node->instructions()->accept(this, lvl + 2);
  _symtab.pop();
}

void til::postfix_writer::do_function_call_node(til::function_call_node *const node, int lvl) {}

void til::postfix_writer::do_stack_alloc_node(til::stack_alloc_node *const node, int lvl) {}

void til::postfix_writer::do_stop_node(til::stop_node *const node, int lvl) {}

void til::postfix_writer::do_address_of_node(til::address_of_node *const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->lvalue()->accept(this, lvl + 2);
}

void til::postfix_writer::do_next_node(til::next_node *const node, int lvl) {}

void til::postfix_writer::do_return_node(til::return_node *const node, int lvl) {}

void til::postfix_writer::do_index_node(til::index_node *const node, int lvl) {}

void til::postfix_writer::do_sizeof_node(til::sizeof_node *const node, int lvl) {}


