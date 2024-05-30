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

bool til::type_checker::checkCompatiblePtrTypes(std::shared_ptr<cdk::basic_type> left, std::shared_ptr<cdk::basic_type> right) {
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
  if (!processBinaryExpression(node, lvl) || !checkCompatiblePtrTypes(node->left()->type(), node->right()->type()))
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
      if (node->left()->is_typed(cdk::TYPE_POINTER) && node->right()->is_typed(cdk::TYPE_POINTER) && checkCompatiblePtrTypes(node->left()->type(), node->right()->type())) {
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

  node->lvalue()->accept(this, lvl + 2);
  node->rvalue()->accept(this, lvl + 2);
  
  changeTypeOnMatch(node->lvalue(), node->rvalue());
  const auto lvalType = node->lvalue()->type();
  const auto rvalType = node->rvalue()->type();
  throwIncompatibleTypes(lvalType, rvalType);
  
  node->type(lvalType);
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
  ASSERT_UNSPEC;
  node->type(cdk::primitive_type::create(0, cdk::TYPE_UNSPEC));
}

//---------------------------------------------------------------------------

void til::type_checker::do_loop_node(til::loop_node *const node, int lvl) {
  node->condition()->accept(this, lvl + 4);
}

//---------------------------------------------------------------------------

void til::type_checker::do_if_node(til::if_node *const node, int lvl) {
  node->condition()->accept(this, lvl + 4);
  node->block()->accept(this, lvl + 4);
}

void til::type_checker::do_if_else_node(til::if_else_node *const node, int lvl) {
  node->condition()->accept(this, lvl + 4);
  node->thenblock()->accept(this, lvl + 4);
  node->elseblock()->accept(this, lvl + 4);
}

//--------------------------------CHECK TYPES --------------------------------------

void til::type_checker::changeTypeOnMatch(cdk::typed_node *const lvalue, cdk::typed_node *const rvalue) {
  const auto ltype = lvalue->type();
  const auto rtype = rvalue->type();

  if (ltype->name() == cdk::TYPE_UNSPEC && rtype->name() == cdk::TYPE_UNSPEC) {
    lvalue->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
    rvalue->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
  } else if ((ltype->name() == cdk::TYPE_POINTER && rtype->name() == cdk::TYPE_POINTER && checkCompatiblePtrTypes(ltype, rtype)) ||
             (ltype->name() == cdk::TYPE_FUNCTIONAL && rtype->name() == cdk::TYPE_FUNCTIONAL && checkCompatibleFunTypes(cdk::functional_type::cast(ltype), cdk::functional_type::cast(rtype))) ||
             ((ltype->name() == cdk::TYPE_INT || ltype->name() == cdk::TYPE_DOUBLE) && rtype->name() == cdk::TYPE_UNSPEC)) {
    rvalue->type(ltype);
  }
}

bool til::type_checker::checkCompatibleFunTypes(std::shared_ptr<cdk::functional_type> t1, std::shared_ptr<cdk::functional_type> t2) {
  if ((t1->output_length() > 0 && t2->output_length() > 0) && !checkCompatibleDataTypes(t1->output(0), t2->output(0)))
    return false;

  if (t1->input_length() != t2->input_length())
    return false;

  for (size_t i = 0; i < t1->input_length(); i++)
    if (!checkCompatibleDataTypes(t1->input(i), t2->input(i)))
      return false;
  return true;
}

bool til::type_checker::checkCompatibleDataTypes(std::shared_ptr<cdk::basic_type> t1, std::shared_ptr<cdk::basic_type> t2, bool is_return) {
  const auto t1_name = t1->name();
  const auto t2_name = t2->name();
  switch (t1_name) {
    case cdk::TYPE_INT:
    case cdk::TYPE_DOUBLE:
      if (!(t2_name == cdk::TYPE_DOUBLE || t2_name == cdk::TYPE_INT))
        return false;
      break;
    case cdk::TYPE_STRING:
      if (t2_name != cdk::TYPE_STRING)
        return false;
      break;
    case cdk::TYPE_POINTER:
      if (is_return == (t2_name == cdk::TYPE_POINTER) && !checkCompatiblePtrTypes(t1, t2))
        return false;
      break;
    case cdk::TYPE_FUNCTIONAL:
      if (!((t2_name == cdk::TYPE_FUNCTIONAL &&
            checkCompatibleFunTypes(cdk::functional_type::cast(t1), cdk::functional_type::cast(t2))) ||
            (t2_name == cdk::TYPE_POINTER && cdk::reference_type::cast(t2)->referenced() == nullptr)))
        return false;
      break;
    case cdk::TYPE_UNSPEC:
      if (t2_name == cdk::TYPE_VOID)
        return false;
      break;
    default:
      if (t1_name != t2_name)
        return false;
    }
  return true;
}

void til::type_checker::throwIncompatibleTypes(std::shared_ptr<cdk::basic_type> t1, std::shared_ptr<cdk::basic_type> t2, bool is_return) {
  if (checkCompatibleDataTypes(t1, t2))
    return;

  const std::string field_name = is_return ? "return" : "initialization"; 
  switch (t1->name()) {
    case cdk::TYPE_INT:
    case cdk::TYPE_DOUBLE:
      throw std::string("wrong type in " + field_name + " - expected double or int");
    case cdk::TYPE_STRING:
      throw std::string("wrong type in " + field_name + " - expected string");
    case cdk::TYPE_POINTER:
      throw std::string("wrong type in " + field_name + " - expected pointer");
    case cdk::TYPE_FUNCTIONAL:
      throw std::string("wrong type in " + field_name + " - expected function");
    default:
      throw std::string("unknown type in " + field_name);
  }
}

//------------------------------------------------------------------------------

void til::type_checker::do_declaration_node(til::declaration_node *const node, int lvl) {
  const auto &init = node->init();

  if(init){
    init->accept(this, lvl + 2);
    if(node->type()){
      changeTypeOnMatch(node, init);
      throwIncompatibleTypes(node->type(), init->type());
      if(node->type()->name() == cdk::TYPE_UNSPEC)
        node->type(init->type());
    } else {
      node->type(init->type());
    }
  }
  
  const auto new_sym = til::make_symbol(node->type(), node->identifier(), (bool)node->init(), node->qualifier());
  if (!_symtab.insert(node->identifier(), new_sym)) { // symbol already exists
    const auto prev_sym = _symtab.find_local(node->identifier());
    if (prev_sym->type()->name() != new_sym->type()->name()) // symbol with different type
      throw std::string("redeclaration of " + node->identifier() + " with different type");
    _symtab.replace(node->identifier(), new_sym); // successful redeclaration
  }
  _parent->set_new_symbol(new_sym);
}

void til::type_checker::do_nullptr_node(til::nullptr_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->type(cdk::reference_type::create(4, cdk::primitive_type::create(0, cdk::TYPE_VOID)));
}

void til::type_checker::do_function_definition_node(til::function_definition_node *const node, int lvl) {
  // EMPTY
}


void til::type_checker::do_function_call_node(til::function_call_node *const node, int lvl) {\
  ASSERT_UNSPEC;

  std::vector<std::shared_ptr<cdk::basic_type>> argsTypes;

  if (node->function()) { // regular function call
    node->function()->accept(this, lvl + 2);
    if(!node->function()->is_typed(cdk::TYPE_FUNCTIONAL))
      throw std::string("Incorrect type in function call expression");

    const auto &type = node->function()->type();
    argsTypes = cdk::functional_type::cast(type)->input()->components();
    node->type(cdk::functional_type::cast(type)->output(0));
    
  } else { //recursive function call
    auto sym = _symtab.find("@");
    if(!sym)
      throw std::string("Recursive call not allowed in the current scope");

    const auto &type = sym->type();
    argsTypes = cdk::functional_type::cast(type)->input()->components();
    node->type(cdk::functional_type::cast(type)->output(0));
  }
}

void til::type_checker::do_stack_alloc_node(til::stack_alloc_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->argument()->accept(this, lvl + 2);
  if (node->argument()->is_typed(cdk::TYPE_INT))
    node->type(cdk::reference_type::create(4, cdk::primitive_type::create(0, cdk::TYPE_UNSPEC)));
  else
    throw std::string("wrong type: stack alloc");
}

void til::type_checker::do_address_of_node(til::address_of_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->lvalue()->accept(this, lvl + 2);
  node->type(cdk::reference_type::create(4, node->lvalue()->type()));
}


void til::type_checker::do_return_node(til::return_node *const node, int lvl) {
  const auto function = _symtab.find("@");
  const auto ret_val = node->retval(); // return value
  if (!function) { // we may be in main
    const auto main = _symtab.find("_main");
    if (main) {
      if (!ret_val)
        throw std::string("wrong type of return value in main - int expected");
      
      ret_val->accept(this, lvl + 2);
      
      if (!ret_val->is_typed(cdk::TYPE_INT))
        throw std::string("wrong type of return value in main - int expected");
      return;
    }
    throw std::string("return statement found outside function");
  } else if (!ret_val) {
    return;
  }

  const auto &funSymType = cdk::functional_type::cast(function->type());
  const auto functionOutput = funSymType->output(0);
  const bool hasOutput = funSymType->output() != nullptr;

  if (hasOutput && functionOutput->name() == cdk::TYPE_VOID)
    throw std::string("return with a value in void function");
  else if (!hasOutput)
    throw std::string("unknown return type in function");

  ret_val->accept(this, lvl + 2);
  throwIncompatibleTypes(functionOutput, ret_val->type(), true);
}

void til::type_checker::do_index_node(til::index_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->base()->accept(this, lvl + 2);
  if (!node->base()->is_typed(cdk::TYPE_POINTER))
    throw std::string("wrong type: base of index");
  node->index()->accept(this, lvl + 2);
  if (!node->index()->is_typed(cdk::TYPE_INT))
    throw std::string("wrong type: index of index");
  const auto base_ref = cdk::reference_type::cast(node->base()->type())->referenced();
  node->type(base_ref);
}

void til::type_checker::do_sizeof_node(til::sizeof_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->argument()->accept(this, lvl + 2);
  node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
}


