#ifndef __TIL_AST_FUNCTION_CALL_NODE_H__
#define __TIL_AST_FUNCTION_CALL_NODE_H__

#include <string>
#include <cdk/ast/basic_node.h>
#include <cdk/ast/sequence_node.h>
#include <cdk/ast/nil_node.h>
#include <cdk/ast/expression_node.h>

namespace til {

  //!
  //! Class for describing function call nodes.
  //!
  //! If _arguments is null, them the node is either a call to a function with
  //! no arguments (or in which none of the default arguments is present) or
  //! an access to a variable.
  //!
  class function_call_node: public cdk::expression_node {
    cdk::expression_node *_function;
    cdk::sequence_node *_arguments;
  
  public:
    /**
     * Constructor for a function call without arguments.
     * An empty sequence is automatically inserted to represent
     * the missing arguments.
     */
    function_call_node(int lineno, cdk::expression_node *function) :
        cdk::expression_node(lineno), _function(function), _arguments(new cdk::sequence_node(lineno)) {
    }

    /**
     * Constructor for a function call with arguments.
     */
    function_call_node(int lineno, cdk::expression_node *function, cdk::sequence_node *arguments) :
        cdk::expression_node(lineno), _function(function), _arguments(arguments) {
    }

    cdk::expression_node *function() { return _function; }
    
    cdk::sequence_node *arguments() { return _arguments; }
    
    cdk::expression_node *argument(size_t ix) {
      return dynamic_cast<cdk::expression_node*>(_arguments->node(ix));
    }

    void accept(basic_ast_visitor *sp, int level) {
      sp->do_function_call_node(this, level);
    }

  };

} // til

#endif
