#ifndef __TIL_TARGETS_POSTFIX_WRITER_H__
#define __TIL_TARGETS_POSTFIX_WRITER_H__

#include "targets/basic_ast_visitor.h"

#include <set>
#include <sstream>
#include <cdk/emitters/basic_postfix_emitter.h>

namespace til {

  //!
  //! Traverse syntax tree and generate the corresponding assembly code.
  //!
  class postfix_writer: public basic_ast_visitor {
    cdk::symbol_table<til::symbol> &_symtab;
    cdk::basic_postfix_emitter &_pf;
  
    std::set<std::string> _symbolsToDeclare;
    std::set<std::string> _functionsToDeclare;

    int _lbl;

    // semantic analysis
    bool _inFunctionBody = false;
    bool _inFunctionArgs = false;
    bool _mainReturnSeen = false;
    bool _lastBlockInstructionSeen = false;

    // while labels -- for break/continue; work like stacks
    std::vector<int> _whileCond, _whileEnd;

    std::vector<std::string> _functionLabels;
    std::string _currentForwardLabel;
    std::vector<std::shared_ptr<til::symbol>> _functions;

    int _offset = 0;    // current frame pointer offset -- 0 means no vars define

  public:
    postfix_writer(std::shared_ptr<cdk::compiler> compiler, cdk::symbol_table<til::symbol> &symtab,
                   cdk::basic_postfix_emitter &pf) :
        basic_ast_visitor(compiler), _symtab(symtab), _pf(pf), _lbl(0) {
    }

  public:
    ~postfix_writer() {
      os().flush();
    }

  protected:
    void processAdditiveExpression(cdk::binary_operation_node *const node, int lvl);
    void processMultiplicativeExpression(cdk::binary_operation_node *const node, int lvl);
    void processLogicalExpression(cdk::binary_operation_node *const node, int lvl);
    void processLocalVarInit(std::shared_ptr<til::symbol> symbol,
    cdk::expression_node *const initializer, int lvl);

    void processGlobalVarInit(std::shared_ptr<til::symbol> symbol,
    cdk::expression_node *const initializer, int lvl);

  private:
    /** Method used to generate sequential labels. */
    inline std::string mklbl(int lbl) {
      std::ostringstream oss;
      if (lbl < 0)
        oss << ".L" << -lbl;
      else
        oss << "_L" << lbl;
      return oss.str();
    }

    /** print error messages. */
    void error(int lineno, std::string e) {
      std::cerr << lineno << ": " << e << std::endl;
    }

  public:
  // do not edit these lines
#define __IN_VISITOR_HEADER__
#include ".auto/visitor_decls.h"       // automatically generated
#undef __IN_VISITOR_HEADER__
  // do not edit these lines: end

  };

} // til

#endif
