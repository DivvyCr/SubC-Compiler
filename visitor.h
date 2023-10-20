#ifndef _VISITOR_H
#define _VISITOR_H

namespace abstract_syntax {
  class IntAST;
  class FloatAST;
  class BoolAST;
  class VariableAST;
  class AssignmentAST;
  class FunctionCallAST;
  class UnaryExpressionAST;
  class BinaryExpressionAST;
  class CodeBlockAST;
  class IfBlockAST;
  class WhileBlockAST;
  class ReturnAST;
  class PrototypeAST;
  class FunctionAST;
  class ProgramAST;
}

using namespace abstract_syntax;

class Visitor {
  public:
    virtual void* visit(IntAST &node) = 0;
    virtual void* visit(FloatAST &node) = 0;
    virtual void* visit(BoolAST &node) = 0;
    virtual void* visit(VariableAST &node) = 0;
    virtual void* visit(AssignmentAST &node) = 0;
    virtual void* visit(FunctionCallAST &node) = 0;
    virtual void* visit(UnaryExpressionAST &node) = 0;
    virtual void* visit(BinaryExpressionAST &node) = 0;
    virtual void* visit(CodeBlockAST &node) = 0;
    virtual void* visit(IfBlockAST &node) = 0;
    virtual void* visit(WhileBlockAST &node) = 0;
    virtual void* visit(ReturnAST &node) = 0;
    virtual void* visit(PrototypeAST &node) = 0;
    virtual void* visit(FunctionAST &node) = 0;
    virtual void* visit(ProgramAST &node) = 0;
};

#endif // _VISITOR_H
