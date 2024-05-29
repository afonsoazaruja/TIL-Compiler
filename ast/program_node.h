#ifndef __TIL_AST_PROGRAM_NODE_H__
#define __TIL_AST_PROGRAM_NODE_H__

#include <cdk/ast/basic_node.h>

namespace til {

  /**
   * Class for describing program nodes.
   */
  class program_node : public cdk::basic_node {
    til::block_node *_block;

  public:
    program_node(int lineno, til::block_node *block) :
        cdk::basic_node(lineno), _block(block) {
    }

    til::block_node *block() { return _block; }

    void accept(basic_ast_visitor *sp, int level) { sp->do_program_node(this, level); }

  };

} // til

#endif
