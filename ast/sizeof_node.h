#ifndef __TIL_AST_SIZEOF_NODE_H__
#define __TIL_AST_SIZEOF_NODE_H__

#include <cdk/ast/expression_node.h>

namespace til {

  class sizeof_node : public cdk::unary_operation_node {
    cdk::expression_node *_expression;
  public:
    inline sizeof_node(int lineno, cdk::expression_node *argument)
        : cdk::unary_operation_node(lineno, argument), _expression(argument) {}

    cdk::expression_node* expression() {
      return _expression;
    }

    void accept(basic_ast_visitor *sp, int level) {
      sp->do_sizeof_node(this, level);
    }
  };

} // til

#endif
