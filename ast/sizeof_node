#ifndef __TIL_AST_SIZEOF_NODE_H__
#define __TIL_AST_SIZEOF_NODE_H__

#include <cdk/ast/expression_node.h>

namespace til {

  class sizeof_node: public cdk::expression_node {
    cdk::expression_node *_expression;

  public:
    sizeof_node(int lineno, cdk::expression_node *expression = nullptr) :
        cdk::expression_node(lineno), _expression(expression) {
    }

    cdk::expression_node* expression() {
      return _expression;
    }

    void accept(basic_ast_visitor *sp, int level) {
      sp->do_sizeof_node(this, level);
    }

  };

} // til

#endif
