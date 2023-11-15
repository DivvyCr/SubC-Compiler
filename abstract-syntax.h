#ifndef _ABSTRACT_SYNTAX_H
#define _ABSTRACT_SYNTAX_H

#include <memory>
#include <string>
#include <vector>

#include "llvm/Support/raw_ostream.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Value.h"

#include "visitor.h"
#include "lexer.h"

using namespace minic_lexer;

using std::string;
using std::vector;
using std::unique_ptr;
using llvm::Function;
using llvm::Value;

//
// TYPES:
//

enum MiniCType {
  INTEGER = 1,
  FLOAT = 2,
  BOOL = 3,
  VOID = 4,
};

//
// ABSTRACT SYNTAX TREE (AST):
//

// Expressions:
class ExpressionAST;
class IntAST;
class FloatAST;
class BoolAST;
class VariableLoadAST;
class AssignmentAST;
class FunctionCallAST;
class UnaryExpressionAST;
class BinaryExpressionAST;
using PtrExpressionAST = unique_ptr<ExpressionAST>;
using PtrIntAST = unique_ptr<IntAST>;
using PtrFloatAST = unique_ptr<FloatAST>;
using PtrBoolAST = unique_ptr<BoolAST>;
using PtrVariableLoadAST = unique_ptr<VariableLoadAST>;
using PtrAssignmentAST = unique_ptr<AssignmentAST>;
using PtrFunctionCallAST = unique_ptr<FunctionCallAST>;
using PtrUnaryExpressionAST = unique_ptr<UnaryExpressionAST>;
using PtrBinaryExpressionAST = unique_ptr<BinaryExpressionAST>;
// Statements:
class StatementAST;
class VariableDeclarationAST;
class ExpressionStatementAST;
class CodeBlockAST;
class IfBlockAST;
class WhileBlockAST;
class ReturnAST;
using PtrStatementAST = unique_ptr<StatementAST>;
using PtrVariableDeclarationAST = unique_ptr<VariableDeclarationAST>;
using PtrExpressionStatementAST = unique_ptr<ExpressionStatementAST>;
using PtrCodeBlockAST = unique_ptr<CodeBlockAST>;
using PtrIfBlockAST = unique_ptr<IfBlockAST>;
using PtrWhileBlockAST = unique_ptr<WhileBlockAST>;
using PtrReturnAST = unique_ptr<ReturnAST>;
// Top-level:
class PrototypeAST;
class FunctionAST;
class ProgramAST;
using PtrPrototypeAST = unique_ptr<PrototypeAST>;
using PtrFunctionAST = unique_ptr<FunctionAST>;
using PtrProgramAST = unique_ptr<ProgramAST>;

class BaseAST {
  public:
    BaseAST(TOKEN token) : Token(token) {}
    TOKEN getToken() { return Token; }
  private:
    TOKEN Token;
};

// This class represents code constructs that hold some value.
class ExpressionAST : public BaseAST { 
  public:
    ExpressionAST(TOKEN token) : BaseAST(token) {}
    virtual ~ExpressionAST() {}
    virtual void *dispatch(ExpressionVisitor &v) = 0;
};

class IntAST : public ExpressionAST {
  public:
    IntAST(TOKEN token, int value)
      : ExpressionAST(token), Value(value) {}
    void *dispatch(ExpressionVisitor &visitor) override { return visitor.visit(*this); }
    const int getValue() const { return Value; }
  private:
    int Value;
};

class FloatAST : public ExpressionAST {
  public:
    FloatAST(TOKEN token, float value)
      : ExpressionAST(token), Value(value) {}
    void *dispatch(ExpressionVisitor &visitor) override { return visitor.visit(*this); }
    const float getValue() const { return Value; }
  private:
    float Value;
};

class BoolAST : public ExpressionAST {
  public:
    BoolAST(TOKEN token, bool value)
      : ExpressionAST(token), Value(value) {}
    void *dispatch(ExpressionVisitor &visitor) override { return visitor.visit(*this); }
    const bool getValue() const { return Value; }
  private:
    bool Value;
};

class VariableLoadAST : public ExpressionAST {
  public:
    VariableLoadAST(TOKEN token, const string &ident)
      : ExpressionAST(token), Identifier(ident) {}
    void *dispatch(ExpressionVisitor &visitor) override { return visitor.visit(*this); }
    const string getIdentifier() const { return Identifier; }
  private:
    string Identifier;
};

class AssignmentAST : public ExpressionAST {
  public:
    AssignmentAST(TOKEN token, const string &ident,
        PtrExpressionAST assignment)
      : ExpressionAST(token), Identifier(ident),
      Assignment(std::move(assignment)) {}
    void *dispatch(ExpressionVisitor &visitor) override { return visitor.visit(*this); }
    const string getIdentifier() const { return Identifier; }
    PtrExpressionAST &getAssignment() { return Assignment; }
  private:
    string Identifier;
    PtrExpressionAST Assignment;
};

class FunctionCallAST : public ExpressionAST {
  public:
    FunctionCallAST(TOKEN token, const string &ident,
        vector<PtrExpressionAST> arguments)
      : ExpressionAST(token), Identifier(ident),
      Arguments(std::make_move_iterator(arguments.begin()),
      std::make_move_iterator(arguments.end())) {}
    void *dispatch(ExpressionVisitor &visitor) override { return visitor.visit(*this); }
    const string getIdentifier() const { return Identifier; }
    vector<PtrExpressionAST> &getArguments() { return Arguments; }
  private:
    string Identifier;  
    vector<PtrExpressionAST> Arguments;
};

class UnaryExpressionAST : public ExpressionAST {
  public:
    UnaryExpressionAST(TOKEN op, PtrExpressionAST expression)
        : ExpressionAST(op), Expression(std::move(expression)) {}
    void *dispatch(ExpressionVisitor &visitor) override { return visitor.visit(*this); }
    TOKEN getOperator() { return getToken(); }
    PtrExpressionAST &getExpression() { return Expression; }
  private:
    PtrExpressionAST Expression;
};

class BinaryExpressionAST : public ExpressionAST {
  public:
    BinaryExpressionAST(TOKEN op,
        PtrExpressionAST left_expression,
        PtrExpressionAST right_expression)
      : ExpressionAST(op), Left(std::move(left_expression)), Right(std::move(right_expression)) {}
    void *dispatch(ExpressionVisitor &visitor) override { return visitor.visit(*this); }
    TOKEN getOperator() { return getToken(); }
    const PtrExpressionAST &getLeft() const { return Left; }
    const PtrExpressionAST &getRight() const { return Right; }
  private:
    PtrExpressionAST Left, Right;
};

// This class represents code constructs that serve to structure.
class StatementAST {
  public:
    virtual ~StatementAST() {}
    virtual void dispatch(StatementVisitor &v) = 0;
};

class VariableDeclarationAST : public StatementAST {
  public:
    VariableDeclarationAST(PtrVariableLoadAST variable, MiniCType declared_type)
      : Variable(std::move(variable)), Type(declared_type) {}
    void dispatch(StatementVisitor &visitor) override { return visitor.visit(*this); }
    const PtrVariableLoadAST &getVariable() const { return Variable; }
    MiniCType getType() { return Type; }
  private:
    PtrVariableLoadAST Variable;
    MiniCType Type;
};

class ExpressionStatementAST : public StatementAST {
  public:
    ExpressionStatementAST(PtrExpressionAST expression)
      : Expression(std::move(expression)) {}
    void dispatch(StatementVisitor &visitor) override { return visitor.visit(*this); }
    const PtrExpressionAST &getExpression() const { return Expression; }
  private:
    PtrExpressionAST Expression;
};

class CodeBlockAST : public BaseAST, public StatementAST {
  public:
    CodeBlockAST(TOKEN token, 
        vector<PtrVariableDeclarationAST> declarations,
        vector<PtrStatementAST> statements)
      : BaseAST(token),
      Declarations(std::make_move_iterator(declarations.begin()),
          std::make_move_iterator(declarations.end())),
      Statements(std::make_move_iterator(statements.begin()),
          std::make_move_iterator(statements.end())) {}
    void dispatch(StatementVisitor &visitor) override { return visitor.visit(*this); }
    const vector<PtrVariableDeclarationAST> &getDeclarations() const { return Declarations; }
    const vector<PtrStatementAST> &getStatements() const { return Statements; }
  private:
    vector<PtrVariableDeclarationAST> Declarations;
    vector<PtrStatementAST> Statements;
};

class IfBlockAST : public BaseAST, public StatementAST {
  public:
    IfBlockAST(TOKEN token,
        PtrExpressionAST condition,
        PtrCodeBlockAST true_branch)
      : BaseAST(token), Condition(std::move(condition)),
      TrueBranch(std::move(true_branch)) {}
    IfBlockAST(TOKEN token,
        PtrExpressionAST condition,
        PtrCodeBlockAST true_branch,
        PtrCodeBlockAST false_branch)
      : BaseAST(token), Condition(std::move(condition)),
      TrueBranch(std::move(true_branch)),
      FalseBranch(std::move(false_branch)) {}
    void dispatch(StatementVisitor &visitor) override { return visitor.visit(*this); }
    const PtrExpressionAST &getCondition() const { return Condition; }
    const PtrCodeBlockAST &getTrueBranch() const { return TrueBranch; }
    const PtrCodeBlockAST &getFalseBranch() const { return FalseBranch; };
  private:
    PtrExpressionAST Condition;
    PtrCodeBlockAST TrueBranch, FalseBranch;
};

class WhileBlockAST : public BaseAST, public StatementAST {
  public:
    WhileBlockAST(TOKEN token,
        PtrExpressionAST condition, PtrStatementAST body)
      : BaseAST(token), Condition(std::move(condition)), Body(std::move(body)) {}
    void dispatch(StatementVisitor &visitor) override { return visitor.visit(*this); }
    const PtrExpressionAST &getCondition() const { return Condition; }
    const PtrStatementAST &getBody() const { return Body; }
  private:
    PtrExpressionAST Condition;
    PtrStatementAST Body;
};

class ReturnAST : public BaseAST, public StatementAST {
  public:
    ReturnAST(TOKEN token)
      : BaseAST(token) {}
    ReturnAST(TOKEN token, PtrExpressionAST body)
      : BaseAST(token), Body(std::move(body)) {}
    void dispatch(StatementVisitor &visitor) override { return visitor.visit(*this); }
    const PtrExpressionAST &getBody() const { return Body; }
  private:
    PtrExpressionAST Body;
};

class PrototypeAST : public BaseAST {
  public:
    PrototypeAST(TOKEN token, const string &ident, MiniCType return_type,
        vector<PtrVariableDeclarationAST> parameters)
      : BaseAST(token), Identifier(ident), ReturnType(return_type),
      Parameters(std::make_move_iterator(parameters.begin()),
          std::make_move_iterator(parameters.end())) {}
    const string getIdentifier() const { return Identifier; }
    MiniCType getReturnType() { return ReturnType; }
    const vector<PtrVariableDeclarationAST> &getParameters() const { return Parameters; }
  private:
    string Identifier;
    MiniCType ReturnType;
    vector<PtrVariableDeclarationAST> Parameters;
};

class FunctionAST : public BaseAST {
  public:
    FunctionAST(TOKEN token,
        PtrPrototypeAST prototype,
        PtrCodeBlockAST body)
      : BaseAST(token), Prototype(std::move(prototype)), Body(std::move(body)) {}
    const PtrPrototypeAST &getPrototype() const { return Prototype; }
    const PtrCodeBlockAST &getBody() const { return Body; }
  private:
    PtrPrototypeAST Prototype;
    PtrCodeBlockAST Body;
};

class ProgramAST {
  public:
    ProgramAST(vector<PtrVariableDeclarationAST> globals,
        vector<PtrPrototypeAST> externs,
        vector<PtrFunctionAST> functions)
          : Globals(std::make_move_iterator(globals.begin()),
              std::make_move_iterator(globals.end())),
          Externs(std::make_move_iterator(externs.begin()),
              std::make_move_iterator(externs.end())),
          Functions(std::make_move_iterator(functions.begin()),
              std::make_move_iterator(functions.end())) {}
    const vector<PtrVariableDeclarationAST> &getGlobals() const { return Globals; }
    const vector<PtrPrototypeAST> &getExterns() const { return Externs; }
    const vector<PtrFunctionAST> &getFunctions() const { return Functions; }
  private:
    vector<PtrVariableDeclarationAST> Globals;
    vector<PtrPrototypeAST> Externs;
    vector<PtrFunctionAST> Functions;
};

#endif // _ABSTRACT_SYNTAX_H
