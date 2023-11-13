#ifndef _VISITOR_H
#define _VISITOR_H

class IntAST;
class FloatAST;
class BoolAST;
class VariableLoadAST;
class AssignmentAST;
class FunctionCallAST;
class UnaryExpressionAST;
class BinaryExpressionAST;

class ExpressionVisitor {
  public:
    virtual void* visit(IntAST &node) = 0;
    virtual void* visit(FloatAST &node) = 0;
    virtual void* visit(BoolAST &node) = 0;
    virtual void* visit(VariableLoadAST &node) = 0;
    virtual void* visit(AssignmentAST &node) = 0;
    virtual void* visit(FunctionCallAST &node) = 0;
    virtual void* visit(UnaryExpressionAST &node) = 0;
    virtual void* visit(BinaryExpressionAST &node) = 0;
};

class VariableDeclarationAST;
class ExpressionStatementAST;
class CodeBlockAST;
class IfBlockAST;
class WhileBlockAST;
class ReturnAST;

class StatementVisitor {
  public:
    virtual void visit(VariableDeclarationAST &node) = 0;
    virtual void visit(ExpressionStatementAST &node) = 0;
    virtual void visit(CodeBlockAST &node) = 0;
    virtual void visit(IfBlockAST &node) = 0;
    virtual void visit(WhileBlockAST &node) = 0;
    virtual void visit(ReturnAST &node) = 0;
};

#endif // _VISITOR_H
