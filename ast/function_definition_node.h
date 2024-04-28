#ifndef __TIL_AST_FUNCTION_DEFINITION_NODE_H__
#define __TIL_AST_FUNCTION_DEFINITION_NODE_H__

#include <string>
#include <cdk/ast/expression_node.h>
#include <cdk/ast/sequence_node.h>
#include "ast/block_node.h"

namespace til {

  //!
  //! Class for describing function definitions.
  //! <pre>
  //! declaration: type qualifier id '(' args ')' block
  //!            {
  //!              $$ = new til::function_definition(LINE, $1, $2, $3, $5, $7);
  //!            }
  //! </pre>
  //!
  class function_definition_node: public cdk::expression_node {
    cdk::sequence_node *_arguments;
    til::block_node *_block;

  public:
    inline function_definition_node(int lineno,
                                    std::shared_ptr<cdk::basic_type> outputType,
                                    cdk::sequence_node *arguments,
                                    til::block_node *block)
        : cdk:: expression_node(lineno), _arguments(arguments), _block(block) {
      std::vector<std::shared_ptr<cdk::basic_type>> inputTypes;
      for (auto *node : arguments->nodes())
        inputTypes.push_back(dynamic_cast<cdk::typed_node *>(node)->type());
      type(cdk::functional_type::create(inputTypes, outputType));
    }

    cdk::sequence_node* arguments() {
      return _arguments;
    }
    cdk::typed_node* argument(size_t ax) {
      return dynamic_cast<cdk::typed_node*>(_arguments->node(ax));
    }
    til::block_node* block() {
      return _block;
    }

    void accept(basic_ast_visitor *sp, int level) {
      sp->do_function_definition_node(this, level);
    }

  };

} // til

#endif
