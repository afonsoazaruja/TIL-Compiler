#include <string>
#include "targets/type_checker.h"
#include ".auto/all_nodes.h"  // automatically generated
#include <cdk/types/primitive_type.h>

#define ASSERT_UNSPEC { if (node->type() != nullptr && !node->is_typed(cdk::TYPE_UNSPEC)) return; }

//---------------------------------------------------------------------------

void til::type_checker::do_sequence_node(cdk::sequence_node *const node, int lvl) {
  for (auto child : node->nodes()) {
    child->accept(this, lvl);
  }
}

//-----------------------------------EMPTY----------------------------------
void til::type_checker::do_nil_node(cdk::nil_node *const node, int lvl) {
  // EMPTY
}
void til::type_checker::do_data_node(cdk::data_node *const node, int lvl) {
  // EMPTY
}
void til::type_checker::do_stop_node(til::stop_node *const node, int lvl) {
  // EMPTY
}
void til::type_checker::do_next_node(til::next_node *const node, int lvl) {
  // EMPTY
}
void til::type_checker::do_block_node(til::block_node *const node, int lvl) {
  // EMPTY
}
//---------------------------------------------------------------------------

void til::type_checker::do_integer_node(cdk::integer_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
}

void til::type_checker::do_double_node(cdk::double_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->type(cdk::primitive_type::create(8, cdk::TYPE_DOUBLE));
}

void til::type_checker::do_string_node(cdk::string_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->type(cdk::primitive_type::create(4, cdk::TYPE_STRING));
}

void til::type_checker::do_not_node(cdk::not_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->argument()->accept(this, lvl + 2);
  if (!node->argument()->is_typed(cdk::TYPE_INT)) 
    throw std::string("wrong type in argument of not expression");
  node->type(node->argument()->type());
}

//---------------------------------------------------------------------------
void til::type_checker::processUnaryExpression(cdk::unary_operation_node *const node, int lvl) {
  node->argument()->accept(this, lvl + 2);
  if (!node->argument()->is_typed(cdk::TYPE_INT) && !node->argument()->is_typed(cdk::TYPE_DOUBLE)) 
    throw std::string("wrong type in argument of unary expression");

  node->type(node->argument()->type());
}

void til::type_checker::do_unary_minus_node(cdk::unary_minus_node *const node, int lvl) {
  processUnaryExpression(node, lvl);
}

void til::type_checker::do_unary_plus_node(cdk::unary_plus_node *const node, int lvl) {
  processUnaryExpression(node, lvl);
}

//---------------------------------------------------------------------------

bool til::type_checker::processBinaryExpression(cdk::binary_operation_node *const node, int lvl) {
  node->left()->accept(this, lvl + 2);
  node->right()->accept(this, lvl + 2);

  if (node->left()->is_typed(cdk::TYPE_INT) || node->left()->is_typed(cdk::TYPE_UNSPEC)) {
    if (node->right()->is_typed(cdk::TYPE_INT))
      node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
    else if (node->right()->is_typed(cdk::TYPE_DOUBLE))
      node->type(cdk::primitive_type::create(8, cdk::TYPE_DOUBLE));
    else if (node->right()->is_typed(cdk::TYPE_UNSPEC)) {
      node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
      node->left()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
      node->right()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
    } else {
      return false;
    }
  } else if (node->left()->is_typed(cdk::TYPE_DOUBLE)) {
    if (node->right()->is_typed(cdk::TYPE_DOUBLE) || node->right()->is_typed(cdk::TYPE_INT))
      node->type(cdk::primitive_type::create(8, cdk::TYPE_DOUBLE));
    else if (node->right()->is_typed(cdk::TYPE_UNSPEC)) {
      node->type(cdk::primitive_type::create(8, cdk::TYPE_DOUBLE));
      node->right()->type(cdk::primitive_type::create(8, cdk::TYPE_DOUBLE));
    } else {
      return false;
    }
  } else {
    return false;
  }
  return true;
}

void til::type_checker::processTypeBinaryExpression(cdk::binary_operation_node *const node, int lvl) {
  ASSERT_UNSPEC;
  if (!processBinaryExpression(node, lvl)) 
    throw std::string("wrong types in binary expression");
}

void til::type_checker::processTypeIntegerExpression(cdk::binary_operation_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->left()->accept(this, lvl + 2);
  if (!node->left()->is_typed(cdk::TYPE_INT)) 
    throw std::string("wrong type in left argument of logical expression");
  node->right()->accept(this, lvl + 2);
  if(!node->right()->is_typed(cdk::TYPE_INT)) 
    throw std::string("wrong type in right argument of logical expression");
  node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
}

void til::type_checker::processCompareExpression(cdk::binary_operation_node *const node, int lvl) {
  ASSERT_UNSPEC;
  processTypeBinaryExpression(node, lvl);
  node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
}

bool til::type_checker::checkPointerCompatibleTypes(std::shared_ptr<cdk::basic_type> left, std::shared_ptr<cdk::basic_type> right) {
  auto p1 = left;
  auto p2 = right;
  while (p1->name() == cdk::TYPE_POINTER && p2->name() == cdk::TYPE_POINTER) {
    p1 = cdk::reference_type::cast(p1)->referenced();
    p2 = cdk::reference_type::cast(p2)->referenced();
  } // TODO: VERIFICAR somente p2->name() == cdk::TYPE_UNSPEC???
  return p1->name() == p2->name() || (p1->name() == cdk::TYPE_UNSPEC || p2->name() == cdk::TYPE_UNSPEC);
}

void til::type_checker::processEqualityExpression(cdk::binary_operation_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->left()->accept(this, lvl + 2);
  node->right()->accept(this, lvl + 2);
  if (!processBinaryExpression(node, lvl) || !checkPointerCompatibleTypes(node->left()->type(), node->right()->type()))
    throw std::string("wrong types in binary expression");  
  node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
}

void til::type_checker::processLogicalExpression(cdk::binary_operation_node *const node, int lvl) {
  ASSERT_UNSPEC;
  processTypeIntegerExpression(node, lvl);
  node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
}

void til::type_checker::processAdditiveExpression(cdk::binary_operation_node *const node, int lvl, bool sub) {
  ASSERT_UNSPEC;
  if (processBinaryExpression(node, lvl))
    return;
  else if (node->left()->is_typed(cdk::TYPE_POINTER) && node->right()->is_typed(cdk::TYPE_INT)) {
    node->type(node->left()->type());
  }
  else if (node->left()->is_typed(cdk::TYPE_INT) && node->right()->is_typed(cdk::TYPE_POINTER)) {
    node->type(node->right()->type());
  }
  else {
    if (sub) {
      if (node->left()->is_typed(cdk::TYPE_POINTER) && node->right()->is_typed(cdk::TYPE_POINTER) && checkPointerCompatibleTypes(node->left()->type(), node->right()->type())) {
        node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
        return;
      }
    }
    throw std::string("wrong types in additive expression");
  }
}


void til::type_checker::do_add_node(cdk::add_node *const node, int lvl) {
  processAdditiveExpression(node, lvl, false);
}
void til::type_checker::do_sub_node(cdk::sub_node *const node, int lvl) {
  processAdditiveExpression(node, lvl, true);
}
void til::type_checker::do_mul_node(cdk::mul_node *const node, int lvl) {
  processTypeBinaryExpression(node, lvl);
}
void til::type_checker::do_div_node(cdk::div_node *const node, int lvl) {
  processTypeBinaryExpression(node, lvl);
}
void til::type_checker::do_mod_node(cdk::mod_node *const node, int lvl) {
  processTypeIntegerExpression(node, lvl);
}

//----------------------------COMPARE---------------------------------------

void til::type_checker::do_lt_node(cdk::lt_node *const node, int lvl) {
  processCompareExpression(node, lvl);
}
void til::type_checker::do_le_node(cdk::le_node *const node, int lvl) {
  processCompareExpression(node, lvl);
}
void til::type_checker::do_ge_node(cdk::ge_node *const node, int lvl) {
  processCompareExpression(node, lvl);
}
void til::type_checker::do_gt_node(cdk::gt_node *const node, int lvl) {
  processCompareExpression(node, lvl);
}
void til::type_checker::do_ne_node(cdk::ne_node *const node, int lvl) {
  processEqualityExpression(node, lvl);
}
void til::type_checker::do_eq_node(cdk::eq_node *const node, int lvl) {
  processEqualityExpression(node, lvl);
}

//----------------------------LOGICAL---------------------------------------

void til::type_checker::do_and_node(cdk::and_node *const node, int lvl) {
  processLogicalExpression(node, lvl);
}
void til::type_checker::do_or_node(cdk::or_node *const node, int lvl) {
  processLogicalExpression(node, lvl);
}

//---------------------------------------------------------------------------

void til::type_checker::do_variable_node(cdk::variable_node *const node, int lvl) {
  ASSERT_UNSPEC;
  const std::string &id = node->name();
  std::shared_ptr<til::symbol> symbol = _symtab.find(id);

  if (symbol != nullptr) {
    node->type(symbol->type());
  } else {
    throw id;
  }
}

void til::type_checker::do_rvalue_node(cdk::rvalue_node *const node, int lvl) {
  ASSERT_UNSPEC;
  try {
    node->lvalue()->accept(this, lvl);
    node->type(node->lvalue()->type());
  } catch (const std::string &id) {
    throw "undeclared variable '" + id + "'";
  }
}

void til::type_checker::do_assignment_node(cdk::assignment_node *const node, int lvl) {
  ASSERT_UNSPEC;

  try {
    node->lvalue()->accept(this, lvl);
  } catch (const std::string &id) {
    auto symbol = std::make_shared<til::symbol>(cdk::primitive_type::create(4, cdk::TYPE_INT), id, 0);
    _symtab.insert(id, symbol);
    _parent->set_new_symbol(symbol);  // advise parent that a symbol has been inserted
    node->lvalue()->accept(this, lvl);  //DAVID: bah!
  }

  if (!node->lvalue()->is_typed(cdk::TYPE_INT)) throw std::string("wrong type in left argument of assignment expression");

  node->rvalue()->accept(this, lvl + 2);
  if (!node->rvalue()->is_typed(cdk::TYPE_INT)) throw std::string("wrong type in right argument of assignment expression");

  // in Simple, expressions are always int
  node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
}

//----------------------------------TIL--------------------------------------

void til::type_checker::do_program_node(til::program_node *const node, int lvl) {
  node->block()->accept(this, lvl + 2);
}

void til::type_checker::do_evaluation_node(til::evaluation_node *const node, int lvl) {
  node->argument()->accept(this, lvl + 2);
}

void til::type_checker::do_print_node(til::print_node *const node, int lvl) {
  node->arguments()->accept(this, lvl + 2);
  for (auto *node : node->arguments()->nodes()) {
    const auto &type = dynamic_cast<cdk::expression_node *>(node)->type(); 
    if (!(type->name() == cdk::TYPE_INT || type->name() == cdk::TYPE_DOUBLE || type->name() == cdk::TYPE_STRING))
      throw std::string("wrong type in print statement");
  }
}

//---------------------------------------------------------------------------

void til::type_checker::do_read_node(til::read_node *const node, int lvl) {
  // try {
  //   node->argument()->accept(this, lvl);
  // } catch (const std::string &id) {
  //   throw "undeclared variable '" + id + "'";
  // }
}

//---------------------------------------------------------------------------

void til::type_checker::do_loop_node(til::loop_node *const node, int lvl) {
  node->condition()->accept(this, lvl + 4);
}

//---------------------------------------------------------------------------

void til::type_checker::do_if_node(til::if_node *const node, int lvl) {
  node->condition()->accept(this, lvl + 4);
}

void til::type_checker::do_if_else_node(til::if_else_node *const node, int lvl) {
  node->condition()->accept(this, lvl + 4);
}

//----------------------------------NEW--------------------------------------

void til::type_checker::do_declaration_node(til::declaration_node *const node, int lvl) {}

void til::type_checker::do_nullptr_node(til::nullptr_node *const node, int lvl) {}

void til::type_checker::do_function_definition_node(til::function_definition_node *const node, int lvl) {}


void til::type_checker::do_function_call_node(til::function_call_node *const node, int lvl) {}

void til::type_checker::do_stack_alloc_node(til::stack_alloc_node *const node, int lvl) {}



void til::type_checker::do_address_of_node(til::address_of_node *const node, int lvl) {}


void til::type_checker::do_return_node(til::return_node *const node, int lvl) {}

void til::type_checker::do_index_node(til::index_node *const node, int lvl) {}

void til::type_checker::do_sizeof_node(til::sizeof_node *const node, int lvl) {}


