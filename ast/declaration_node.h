#ifndef __TIL_AST_DECLARATION_NODE_H__
#define __TIL_AST_DECLARATION_NODE_H__

#include <cdk/ast/typed_node.h>
#include <cdk/ast/expression_node.h>
#include <cdk/types/basic_type.h>

namespace til { // AFONSO: needs review

  class declaration_node: public cdk::typed_node {
    int _qualifier;
    std::string _identifier;
    cdk::expression_node *_init;

  public:
    declaration_node(int lineno, int qualifier, 
                    std::shared_ptr<cdk::basic_type> varType, 
                    const std::string &identifier,
                    cdk::expression_node *init) :
      cdk::typed_node(lineno), _qualifier(qualifier), _identifier(identifier), _init(init) {
      type(varType);
    }

    int qualifier() { return _qualifier; }
    
    const std::string& identifier() const { return _identifier; }
    
    cdk::expression_node* init() { return _init; }

    void accept(basic_ast_visitor *sp, int level) {
      sp->do_declaration_node(this, level);
    }
  };

} // til

#endif
