#include <string>
#include <sstream>
#include "targets/frame_size_calculator.h"
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

//----------------------------------TYPES-------------------------------------

void til::postfix_writer::do_integer_node(cdk::integer_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  if (_inFunctionBody) 
    _pf.INT(node->value()); // stored during function scope
  else 
    _pf.SINT(node->value()); // stored globally
  
}

void til::postfix_writer::do_double_node(cdk::double_node * const node, int lvl) {
  if (_inFunctionBody)
    _pf.DOUBLE(node->value());
  else 
    _pf.SDOUBLE(node->value());
}

void til::postfix_writer::do_string_node(cdk::string_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  const auto lbl = mklbl(++_lbl);

  /* generate the string */
  _pf.RODATA(); // strings are DATA readonly
  _pf.ALIGN(); // make sure we are aligned
  _pf.LABEL(lbl); // give the string a name
  _pf.SSTRING(node->value()); // output string characters
  if (_inFunctionBody) {
    _pf.TEXT(_functionLabels.back()); // return to the TEXT segment
    _pf.ADDR(lbl); // the string to be printed
  }
  else {
    _pf.DATA();
    _pf.SADDR(lbl);
  }
}

//------------------------------------UNARY-----------------------------------

void til::postfix_writer::do_unary_minus_node(cdk::unary_minus_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->argument()->accept(this, lvl); // determine the value
  _pf.NEG(); // 2-complement
}

void til::postfix_writer::do_unary_plus_node(cdk::unary_plus_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->argument()->accept(this, lvl); // determine the value
}

//----------------------------------PROCESS---------------------------------

void til::postfix_writer::processAdditiveExpression(cdk::binary_operation_node *const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->left()->accept(this, lvl + 2);
  if (node->is_typed(cdk::TYPE_DOUBLE) && !node->left()->is_typed(cdk::TYPE_DOUBLE))
    _pf.I2D();
  else if (node->is_typed(cdk::TYPE_POINTER) && !node->left()->is_typed(cdk::TYPE_POINTER)) {
    const auto ref_right = cdk::reference_type::cast(node->right()->type())->referenced();
    _pf.INT(std::max(1, static_cast<int>(ref_right->size())));  // for ptr
    _pf.MUL();
  }

  node->right()->accept(this, lvl + 2);
  if (node->is_typed(cdk::TYPE_DOUBLE) && !node->right()->is_typed(cdk::TYPE_DOUBLE))
    _pf.I2D();
  else if (node->is_typed(cdk::TYPE_POINTER) && !node->right()->is_typed(cdk::TYPE_POINTER)) {
    const auto ref_left = cdk::reference_type::cast(node->left()->type())->referenced();
    _pf.INT(std::max(1, static_cast<int>(ref_left->size()))); // for ptr
    _pf.MUL();
  }
}

void til::postfix_writer::processMultiplicativeExpression(cdk::binary_operation_node *const node, int lvl) {
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

void til::postfix_writer::processLocalVarInit(
    std::shared_ptr<til::symbol> symbol,
    cdk::expression_node *const initializer, int lvl) {
  initializer->accept(this, lvl);
  switch (symbol->type()->name()) {
  case cdk::TYPE_INT:
  case cdk::TYPE_STRING:
  case cdk::TYPE_POINTER:
  case cdk::TYPE_FUNCTIONAL:
  case cdk::TYPE_UNSPEC: // cases such as `auto x = input;`
    _pf.LOCAL(symbol->offset());
    _pf.STINT();
    break;
  case cdk::TYPE_DOUBLE:
    if (initializer->is_typed(cdk::TYPE_INT))
      _pf.I2D();
    _pf.LOCAL(symbol->offset());
    _pf.STDOUBLE();
    break;
  default:
    error(initializer->lineno(), "invalid type: variable initialization");
  }
}

void til::postfix_writer::processGlobalVarInit(std::shared_ptr<til::symbol> symbol, cdk::expression_node *const initializer, int lvl) {
  switch (symbol->type()->name()) {
    case cdk::TYPE_INT:
    case cdk::TYPE_STRING:
    case cdk::TYPE_POINTER:
      _pf.DATA();
      _pf.ALIGN();
      _pf.LABEL(symbol->name());
      initializer->accept(this, lvl + 2);
      break;
    case cdk::TYPE_DOUBLE:
      _pf.DATA(); 
      _pf.ALIGN();
      _pf.LABEL(symbol->name());

      const cdk::integer_node *declareI;
      cdk::double_node *declareDI;
      switch (initializer->type()->name()) {
        case cdk::TYPE_INT:
          declareI = dynamic_cast<const cdk::integer_node *>(initializer);
          declareDI = new cdk::double_node(declareI->lineno(), declareI->value());
          declareDI->accept(this, lvl + 2);
          break;
        case cdk::TYPE_DOUBLE:
          initializer->accept(this, lvl + 2);
          break;
        default:
          error(initializer->lineno(),
                "invalid type for double variable initialization");
      }
      break;
    case cdk::TYPE_FUNCTIONAL:
      _functions.push_back(symbol);
      initializer->accept(this, lvl);
      _pf.DATA();
      _pf.ALIGN();
      if (symbol->qualifier() == tPUBLIC)
        _pf.GLOBAL(symbol->name(), _pf.OBJ());
      _pf.LABEL(symbol->name());
      _pf.SADDR(_functionLabels.back());
      break;
    default:
      error(initializer->lineno(), "invalid type for variable initialization");
  }
}

//----------------------------------OPERATIONS---------------------------------

void til::postfix_writer::do_add_node(cdk::add_node * const node, int lvl) {
  processAdditiveExpression(node, lvl);

  if (node->is_typed(cdk::TYPE_DOUBLE))
    _pf.DADD();
  else
    _pf.ADD();
  }
  
void til::postfix_writer::do_sub_node(cdk::sub_node * const node, int lvl) {
  processAdditiveExpression(node, lvl);

  if (node->is_typed(cdk::TYPE_DOUBLE)) { 
    _pf.DSUB();
  } else {
    _pf.SUB();
    if (node->left()->is_typed(cdk::TYPE_POINTER) && node->right()->is_typed(cdk::TYPE_POINTER) 
      && cdk::reference_type::cast(node->left()->type())->referenced()->name() != cdk::TYPE_VOID){
      _pf.INT(cdk::reference_type::cast(node->left()->type())->referenced()->size());
      _pf.DIV();
    }
  }
}

void til::postfix_writer::do_mul_node(cdk::mul_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  processMultiplicativeExpression(node, lvl);
  if (node->is_typed(cdk::TYPE_DOUBLE)) {
    _pf.DMUL();
  } else {
    _pf.MUL();
  }
}

void til::postfix_writer::do_div_node(cdk::div_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  processMultiplicativeExpression(node, lvl);
  if (node->is_typed(cdk::TYPE_DOUBLE)) {
    _pf.DDIV();
  } else {
    _pf.DIV();
  }
}

void til::postfix_writer::do_mod_node(cdk::mod_node * const node, int lvl) { 
  ASSERT_SAFE_EXPRESSIONS;
  node->left()->accept(this, lvl + 2);
  node->right()->accept(this, lvl + 2);
  if (node->is_typed(cdk::TYPE_DOUBLE)) {
    error(node->lineno(), "integer expression expected");
  }
  else {
    _pf.MOD();
  }
}

//----------------------------COMPARE---------------------------------------

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

//----------------------------LOGICAL---------------------------------------

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
  ASSERT_SAFE_EXPRESSIONS;
  const auto lbl = mklbl(++_lbl);
  node->left()->accept(this, lvl + 2);
  _pf.DUP32();
  _pf.JNZ(lbl);
  node->right()->accept(this, lvl + 2);
  _pf.OR();
  _pf.ALIGN();
  _pf.LABEL(lbl);
}

void til::postfix_writer::do_not_node(cdk::not_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->argument()->accept(this, lvl + 2);
  _pf.INT(0);
  _pf.EQ();
}

//---------------------------------------------------------------------------

void til::postfix_writer::do_sequence_node(cdk::sequence_node * const node, int lvl) {
  for (size_t i = 0; i < node->size(); i++) {
    node->node(i)->accept(this, lvl);
  }
}

void til::postfix_writer::do_variable_node(cdk::variable_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  const auto &id = node->name();
  const auto sym = _symtab.find(id);

  if (sym->qualifier() == tEXTERNAL)
    _currentForwardLabel = sym->name();
  else if (sym->is_global())
    _pf.ADDR(sym->name());
  else {
    _pf.LOCAL(sym->offset());
  }
}

void til::postfix_writer::do_rvalue_node(cdk::rvalue_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->lvalue()->accept(this, lvl);
  if (node->is_typed(cdk::TYPE_DOUBLE)) {
    _pf.LDDOUBLE();
  }
  else {
    if (_currentForwardLabel.empty())
      _pf.LDINT(); 
  }
}

void til::postfix_writer::do_assignment_node(cdk::assignment_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->rvalue()->accept(this, lvl);
  if (node->is_typed(cdk::TYPE_DOUBLE)) {
    if (node->rvalue()->is_typed(cdk::TYPE_INT))
      _pf.I2D();
    _pf.DUP64();
  }
  else {
    _pf.DUP32();
  }

  node->lvalue()->accept(this, lvl + 2);
  if (node->is_typed(cdk::TYPE_DOUBLE))
    _pf.STDOUBLE();
  else
    _pf.STINT();
}

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
      _functionsToDeclare.insert("printi");
      _pf.CALL("printi");
      _pf.TRASH(4);
    } else if (arg->is_typed(cdk::TYPE_DOUBLE)) {
      _functionsToDeclare.insert("printd");
      _pf.CALL("printd");
      _pf.TRASH(8);
    } else if (arg->is_typed(cdk::TYPE_STRING)) {
      _functionsToDeclare.insert("prints");
      _pf.CALL("prints");
      _pf.TRASH(4);
    } else
      error(node->lineno(), "print: unknown type");
  }
  if (node->newLine()) {
    _functionsToDeclare.insert("println");
    _pf.CALL("println");
  }
}

void til::postfix_writer::do_read_node(til::read_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  switch (node->type()->name()) {
    case cdk::TYPE_INT:
    case cdk::TYPE_UNSPEC:
      _functionsToDeclare.insert("readi");
      _pf.CALL("readi");
      _pf.LDFVAL32();
      break;
    case cdk::TYPE_DOUBLE:
      _functionsToDeclare.insert("readd");
      _pf.CALL("readd");
      _pf.LDFVAL64();
      break;
    default:
      error(node->lineno(), "read: unknown type");
  }
}

//------------------------------CONDITIONAL-----------------------------------

void til::postfix_writer::do_loop_node(til::loop_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  int whileCondLbl = ++_lbl;
  int whileEndLbl = ++_lbl;
  
  _loopCond.push_back(whileCondLbl);
  _loopEnd.push_back(whileEndLbl);

  _symtab.push();   

  _pf.ALIGN();                               
  _pf.LABEL(mklbl(whileCondLbl));            
  node->condition()->accept(this, lvl + 2);  
  _pf.JZ(mklbl(whileEndLbl));

  node->block()->accept(this, lvl + 2); 
  _lastBlockInstructionSeen = false;
  _pf.JMP(mklbl(whileCondLbl));  
  _pf.ALIGN();                   
  _pf.LABEL(mklbl(whileEndLbl));

  _symtab.pop();         
  _loopCond.pop_back(); 
  _loopEnd.pop_back();
}

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

void til::postfix_writer::do_stop_node(til::stop_node *const node, int lvl) {
  const auto loopLabels = _loopCond.size();
  if (loopLabels == 0) {
    error(node->lineno(), "stop node found outside a while block");
    return;
  }
  const size_t stopLvl = (size_t)node->level();
  if (stopLvl > loopLabels || stopLvl < 1) {
    error(node->lineno(), "invalid stop level");
    return;
  }
  _lastBlockInstructionSeen = true;
  const auto loopEndLbl = _loopEnd[loopLabels - stopLvl];
  _pf.JMP(mklbl(loopEndLbl));
}

void til::postfix_writer::do_next_node(til::next_node *const node, int lvl) {
  const auto loopLabels = _loopCond.size();
    if (loopLabels == 0) {
      error(node->lineno(), "next node found outside a while block");
      return;
    }
    const size_t nextLvl = (size_t)node->level();
    if (nextLvl > loopLabels || nextLvl < 1) {
      error(node->lineno(), "invalid next level");
      return;
    }
    _lastBlockInstructionSeen = true;
    const auto loopCondLbl = _loopCond[loopLabels - nextLvl];
    _pf.JMP(mklbl(loopCondLbl));
}

//----------------------------------NEW--------------------------------------

void til::postfix_writer::do_declaration_node(til::declaration_node *const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  const auto type_size = node->type()->size();
  int offset = 0;

  if (_inFunctionArgs) {
    offset = _offset;
    _offset += type_size;
  } else if (_inFunctionBody) {
    _offset -= type_size;
    offset = _offset;
  }

  const auto symbol = new_symbol();
  if (symbol) {
    symbol->set_offset(offset);
    reset_new_symbol();
  }

  // we may still need to initialize the variable
  if (node->init()) {
    if (_inFunctionBody)
      processLocalVarInit(symbol, node->init(), lvl);
    else
      processGlobalVarInit(symbol, node->init(), lvl);
    _symbolsToDeclare.erase(symbol->name());
  } else if (!_inFunctionArgs && !_inFunctionBody) {
    _symbolsToDeclare.insert(symbol->name());
  }
}

void til::postfix_writer::do_nullptr_node(til::nullptr_node *const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  if (_inFunctionBody)
    _pf.INT(0);
  else
    _pf.SINT(0);
}

void til::postfix_writer::do_stack_alloc_node(til::stack_alloc_node *const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->argument()->accept(this, lvl);
  _pf.INT(cdk::reference_type::cast(node->type())->referenced()->size()); // type size
  _pf.MUL(); // type size * argument
  _pf.ALLOC();           
  _pf.SP();
}

void til::postfix_writer::do_address_of_node(til::address_of_node *const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->lvalue()->accept(this, lvl + 2);
}

void til::postfix_writer::do_index_node(til::index_node *const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->base()->accept(this, lvl);
  node->index()->accept(this, lvl);
  _pf.INT(node->type()->size());
  _pf.MUL();
  _pf.ADD();
}

void til::postfix_writer::do_sizeof_node(til::sizeof_node *const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  if (_inFunctionBody)
    _pf.INT(node->argument()->type()->size());
  else
    _pf.SINT(node->argument()->type()->size());
}

void til::postfix_writer::do_block_node(til::block_node *const node, int lvl) {
  _symtab.push();
  if (node->declarations())
    node->declarations()->accept(this, lvl + 2);
  if (node->instructions())
    node->instructions()->accept(this, lvl + 2);
  _symtab.pop();
}

//-----------------------------------FUNCTION-------------------------------------

void til::postfix_writer::do_program_node(til::program_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  for (auto symName : _symbolsToDeclare) {
    const auto sym = _symtab.find(symName);
    if (sym->qualifier() == tEXTERNAL){ // it's a function
      _functionsToDeclare.insert(symName);
      continue;
    }

    // declare the symbol
    _pf.BSS();
    _pf.ALIGN();
    _pf.LABEL(symName);
    _pf.SALLOC(sym->type()->size());
  }

  const auto program = til::make_symbol(cdk::functional_type::create(cdk::primitive_type::create(4, cdk::TYPE_INT)), "_main", 0, tPRIVATE);
  _symtab.insert(program->name(), program);
  _functions.push_back(program);
  _functionLabels.push_back("_main");

  _symtab.push();
  _pf.TEXT("_main");
  _pf.ALIGN();
  _pf.GLOBAL("_main", _pf.FUNC());
  _pf.LABEL("_main");

  frame_size_calculator fsc(_compiler, _symtab);
  node->accept(&fsc, lvl);
  _pf.ENTER(fsc.localsize()); // allocate space for local variables

  _inFunctionBody = true;
  node->block()->accept(this, lvl + 2);
  _inFunctionBody = false;

  _symtab.pop();

  _functionLabels.pop_back();
  _functions.pop_back();

  if (!_mainReturnSeen){
    _pf.INT(0);
    _pf.STFVAL32();
  }

  // end the main function
  _pf.LEAVE();
  _pf.RET();
  
  for(auto externFunction : _functionsToDeclare)
    _pf.EXTERN(externFunction);
}

void til::postfix_writer::do_function_definition_node(til::function_definition_node *const node, int lvl) {
  _symtab.push();
  auto function = til::make_symbol(node->type(), "@", 0, tPRIVATE); // for recursion
  if (_symtab.find_local(function->name())) {
    _symtab.replace(function->name(), function);
  } else {
    _symtab.insert(function->name(), function);
  }
  _functions.push_back(function);

  const auto funcLabel = mklbl(++_lbl);
  _functionLabels.push_back(funcLabel);

  const auto prev_offset = _offset;
  _offset = 8; // start at 8

  if (node->arguments()) {
    _inFunctionArgs = true;
    for (size_t arg = 0; arg < node->arguments()->size(); arg++) {
      node->arguments()->node(arg)->accept(this, lvl);
    }
    _inFunctionArgs = false;
  }

  _pf.TEXT(funcLabel);
  _pf.ALIGN();
  _pf.LABEL(funcLabel);

  frame_size_calculator fsc(_compiler, _symtab);
  node->accept(&fsc, lvl);
  _pf.ENTER(fsc.localsize());

  _offset = 0; // reset offset, prepare for local variables
  auto _previouslyInFunctionBody = _inFunctionBody;
  _inFunctionBody = true;
  if (node->block())
    node->block()->accept(this, lvl + 2);
  _inFunctionBody = _previouslyInFunctionBody;
  _symtab.pop(); // leaving args scope
  _offset = prev_offset;

  if (function)
    _functions.pop_back();

  _pf.LEAVE();
  _pf.RET();

  if (_inFunctionBody) {
    _functionLabels.pop_back();
    _pf.TEXT(_functionLabels.back());
    _pf.ADDR(funcLabel);
  }  
}

void til::postfix_writer::do_function_call_node(til::function_call_node *const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  std::vector<std::shared_ptr<cdk::basic_type>> argsTypes;
  const auto func = node->function();
  
  if (func) {
    argsTypes = cdk::functional_type::cast(func->type())->input()->components();
  } else {
    auto deepestRecursiveFunc = _functions.back();
    argsTypes = cdk::functional_type::cast(deepestRecursiveFunc->type())->input()->components();
  }

  size_t argsSize = 0; // bytes (size of all the arguments)

  if (node->arguments()) {
    for (int i = node->arguments()->size() - 1; i >= 0; i--) {
      auto arg = dynamic_cast<cdk::expression_node *>(node->arguments()->node(i));
      arg->accept(this, lvl + 2);

      if (argsTypes[i]->name() == cdk::TYPE_DOUBLE && arg->type()->name() == cdk::TYPE_INT) {
        argsSize += 4;
        _pf.I2D();
      }
      argsSize += arg->type()->size();
    }
  }

  if (func) {
    // non-recursive functions
    _currentForwardLabel.clear();
    func->accept(this, lvl + 2);
    // non-recursive function call
    if (_currentForwardLabel.empty()) 
      _pf.BRANCH();
    else
      _pf.CALL(_currentForwardLabel);
  } else { // recursive function call
      _pf.CALL(_functionLabels.back());
  }

  if (argsSize > 0)
    _pf.TRASH(argsSize);

  switch (node->type()->name()) {
    case cdk::TYPE_VOID:
      break;
    case cdk::TYPE_INT:
      if (_currentForwardLabel.empty()) {
        _pf.LDFVAL64();
        _pf.D2I();
      } else {
        _pf.LDFVAL32();
      }
      break;
    case cdk::TYPE_STRING:
    case cdk::TYPE_POINTER:
    case cdk::TYPE_FUNCTIONAL:
      _pf.LDFVAL32();
      break;
    case cdk::TYPE_DOUBLE:
      _pf.LDFVAL64();
      break;
    default:
      error(node->lineno(), "Cant call expression: unknown type");
  }
  _currentForwardLabel.clear();
}

void til::postfix_writer::do_return_node(til::return_node *const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  const auto current_function_type_name = cdk::functional_type::cast(_functions.back()->type())->output(0)->name();

  if (current_function_type_name != cdk::TYPE_VOID){
    node->retval()->accept(this, lvl + 2);

    switch (current_function_type_name) {
      case cdk::TYPE_INT:
        if (_functions.back()->is_main()) {
          _mainReturnSeen = true;
          _pf.STFVAL32();
        } else {
          _pf.I2D();
          _pf.STFVAL64();
        }
        break;
      case cdk::TYPE_STRING:
      case cdk::TYPE_POINTER:
      case cdk::TYPE_FUNCTIONAL:
        _pf.STFVAL32(); 
        break;
      case cdk::TYPE_DOUBLE:
        if (!node->retval()->is_typed(cdk::TYPE_DOUBLE))
          _pf.I2D();    
        _pf.STFVAL64(); 
        break;
      default:
        error(node->lineno(), "invalid return type");
    }
  }

  _lastBlockInstructionSeen = true;
  _pf.LEAVE();
  _pf.RET();
}
