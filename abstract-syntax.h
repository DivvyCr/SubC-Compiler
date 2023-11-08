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

  UNKNOWN = -1
};

//
// ABSTRACT SYNTAX TREE (AST):
//

// Declare all classes and pointer aliases:
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
class GlobalVariableAST;
class PrototypeAST;
class FunctionAST;
class ProgramAST;
class ExpressionAST;
class StatementAST;

using PtrIntAST = unique_ptr<IntAST>;
using PtrFloatAST = unique_ptr<FloatAST>;
using PtrBoolAST = unique_ptr<BoolAST>;
using PtrVariableAST = unique_ptr<VariableAST>;
using PtrAssignmentAST = unique_ptr<AssignmentAST>;
using PtrFunctionCallAST = unique_ptr<FunctionCallAST>;
using PtrUnaryExpressionAST = unique_ptr<UnaryExpressionAST>;
using PtrBinaryExpressionAST = unique_ptr<BinaryExpressionAST>;
using PtrExpressionStatementAST = unique_ptr<ExpressionStatementAST>;
using PtrCodeBlockAST = unique_ptr<CodeBlockAST>;
using PtrIfBlockAST = unique_ptr<IfBlockAST>;
using PtrWhileBlockAST = unique_ptr<WhileBlockAST>;
using PtrReturnAST = unique_ptr<ReturnAST>;
using PtrGlobalVariableAST = unique_ptr<GlobalVariableAST>;
using PtrPrototypeAST = unique_ptr<PrototypeAST>;
using PtrFunctionAST = unique_ptr<FunctionAST>;
using PtrProgramAST = unique_ptr<ProgramAST>;
using PtrExpressionAST = unique_ptr<ExpressionAST>;
using PtrStatementAST = unique_ptr<StatementAST>;

// TODO: Export TOKEN to top-level AST classes?

class ExpressionAST {
  // This class represents code constructs that hold some value.
  public:
    virtual ~ExpressionAST() {}
    virtual void *dispatch(ExpressionVisitor &v) = 0;
};

class IntAST : public ExpressionAST {
  public:
    IntAST(TOKEN token, int value)
      : Token(token), Value(value) {}
    void *dispatch(ExpressionVisitor &visitor) override { return visitor.visit(*this); }
    int getValue() const { return Value; }
  private:
    TOKEN Token;
    int Value;
};

class FloatAST : public ExpressionAST {
  public:
    FloatAST(TOKEN token, float value)
      : Token(token), Value(value) {}
    void *dispatch(ExpressionVisitor &visitor) override { return visitor.visit(*this); }
    float getValue() const { return Value; }
  private:
    TOKEN Token;
    float Value;
};

class BoolAST : public ExpressionAST {
  public:
    BoolAST(TOKEN token, bool value)
      : Token(token), Value(value) {}
    void *dispatch(ExpressionVisitor &visitor) override { return visitor.visit(*this); }
    bool getValue() const { return Value; }
  private:
    TOKEN Token;
    bool Value;
};

class VariableAST : public ExpressionAST {
  public:
    VariableAST(TOKEN token, const string &ident)
      : Token(token), Identifier(ident), Type(UNKNOWN) {}
    VariableAST(TOKEN token, const string &ident, MiniCType type)
      : Token(token), Identifier(ident), Type(type) {}
    void *dispatch(ExpressionVisitor &visitor) override { return visitor.visit(*this); }
    string getIdentifier() const { return Identifier; }
    MiniCType getType() { return Type; }
  private:
    TOKEN Token;
    string Identifier;
    MiniCType Type;
};

class AssignmentAST : public ExpressionAST {
  public:
    AssignmentAST(TOKEN token, const string &ident,
        PtrExpressionAST assignment)
      : Token(token), Identifier(ident),
      Assignment(std::move(assignment)) {}
    void *dispatch(ExpressionVisitor &visitor) override { return visitor.visit(*this); }
    string getIdentifier() const { return Identifier; }
    PtrExpressionAST &getAssignment() { return Assignment; }
  private:
    TOKEN Token;
    string Identifier;
    PtrExpressionAST Assignment;
};

class FunctionCallAST : public ExpressionAST {
  public:
    FunctionCallAST(TOKEN token, const string &ident,
        vector<PtrExpressionAST> arguments)
      : Token(token), Identifier(ident),
      Arguments(std::make_move_iterator(arguments.begin()),
      std::make_move_iterator(arguments.end())) {}
    void *dispatch(ExpressionVisitor &visitor) override { return visitor.visit(*this); }
    string getIdentifier() const { return Identifier; }
    vector<PtrExpressionAST> &getArguments() { return Arguments; }
  private:
    TOKEN Token;
    string Identifier;  
    vector<PtrExpressionAST> Arguments;
};

class UnaryExpressionAST : public ExpressionAST {
  public:
    UnaryExpressionAST(TOKEN op, PtrExpressionAST expression)
        : Operator(op), Expression(std::move(expression)) {}
    void *dispatch(ExpressionVisitor &visitor) override { return visitor.visit(*this); }
    TOKEN getOperator() const { return Operator; }
    PtrExpressionAST &getExpression() { return Expression; }
  private:
    TOKEN Operator;
    PtrExpressionAST Expression;
};

class BinaryExpressionAST : public ExpressionAST {
  public:
    BinaryExpressionAST(TOKEN op,
        PtrExpressionAST left_expression,
        PtrExpressionAST right_expression)
      : Operator(op), Left(std::move(left_expression)), Right(std::move(right_expression)) {}
    void *dispatch(ExpressionVisitor &visitor) override { return visitor.visit(*this); }
    TOKEN getOperator() const { return Operator; }
    PtrExpressionAST &getLeft() { return Left; }
    PtrExpressionAST &getRight() { return Right; }
  private:
    TOKEN Operator;
    PtrExpressionAST Left, Right;
};

class StatementAST {
  // This class represents code constructs that serve to structure.
  public:
    virtual ~StatementAST() {}
    virtual void dispatch(StatementVisitor &v) = 0;
};

class ExpressionStatementAST : public StatementAST {
  public:
    ExpressionStatementAST(PtrExpressionAST expression)
      : Expression(std::move(expression)) {}
    void dispatch(StatementVisitor &visitor) override { return visitor.visit(*this); }
    PtrExpressionAST &getExpression() { return Expression; }
  private:
    PtrExpressionAST Expression;
};

class CodeBlockAST : public StatementAST {
  public:
    CodeBlockAST(TOKEN token, 
        vector<PtrVariableAST> declarations,
        vector<PtrStatementAST> statements)
      : Token(token),
      Declarations(std::make_move_iterator(declarations.begin()),
          std::make_move_iterator(declarations.end())),
      Statements(std::make_move_iterator(statements.begin()),
          std::make_move_iterator(statements.end())) {}
    void dispatch(StatementVisitor &visitor) override { return visitor.visit(*this); }
    vector<PtrVariableAST> &getDeclarations() { return Declarations; }
    vector<PtrStatementAST> &getStatements() { return Statements; }
  private:
    TOKEN Token;
    vector<PtrVariableAST> Declarations;
    vector<PtrStatementAST> Statements;
};

class IfBlockAST : public StatementAST {
  public:
    IfBlockAST(TOKEN token,
        PtrExpressionAST condition,
        PtrCodeBlockAST true_branch)
      : Token(token), Condition(std::move(condition)),
      TrueBranch(std::move(true_branch)) {}
    IfBlockAST(TOKEN token,
        PtrExpressionAST condition,
        PtrCodeBlockAST true_branch,
        PtrCodeBlockAST false_branch)
      : Token(token), Condition(std::move(condition)),
      TrueBranch(std::move(true_branch)),
      FalseBranch(std::move(false_branch)) {}
    void dispatch(StatementVisitor &visitor) override { return visitor.visit(*this); }
    PtrExpressionAST &getCondition() { return Condition; }
    PtrCodeBlockAST &getTrueBranch() { return TrueBranch; }
    PtrCodeBlockAST &getFalseBranch() { return FalseBranch; };
  private:
    TOKEN Token;
    PtrExpressionAST Condition;
    PtrCodeBlockAST TrueBranch, FalseBranch;
};

class WhileBlockAST : public StatementAST {
  public:
    WhileBlockAST(TOKEN token,
        PtrExpressionAST condition, PtrStatementAST body)
      : Token(token), Condition(std::move(condition)), Body(std::move(body)) {}
    void dispatch(StatementVisitor &visitor) override { return visitor.visit(*this); }
    PtrExpressionAST &getCondition() { return Condition; }
    PtrStatementAST &getBody() { return Body; }
  private:
    TOKEN Token;
    PtrExpressionAST Condition;
    PtrStatementAST Body;
};

class ReturnAST : public StatementAST {
  public:
    ReturnAST(TOKEN token)
      : Token(token) {}
    ReturnAST(TOKEN token, PtrExpressionAST body)
      : Token(token), Body(std::move(body)) {}
    void dispatch(StatementVisitor &visitor) override { return visitor.visit(*this); }
    PtrExpressionAST &getBody() { return Body; }
  private:
    TOKEN Token;
    PtrExpressionAST Body;
};

class GlobalVariableAST {
  public:
    GlobalVariableAST(PtrVariableAST variable)
      : Variable(std::move(variable)) {}
    const PtrVariableAST &getVariable() const { return Variable; }
  private:
    PtrVariableAST Variable;
};

class PrototypeAST {
  public:
    PrototypeAST(TOKEN token, const string &ident, MiniCType return_type,
        vector<PtrVariableAST> parameters)
      : Token(token), Identifier(ident), ReturnType(return_type),
      Parameters(std::make_move_iterator(parameters.begin()),
          std::make_move_iterator(parameters.end())) {}
    string getIdentifier() const { return Identifier; }
    MiniCType getReturnType() const { return ReturnType; }
    const vector<PtrVariableAST> &getParameters() const { return Parameters; }
    MiniCType getType();
  private:
    TOKEN Token;
    string Identifier;
    MiniCType ReturnType;
    vector<PtrVariableAST> Parameters;
};

class FunctionAST {
  public:
    FunctionAST(TOKEN token,
        PtrPrototypeAST prototype,
        PtrCodeBlockAST body)
      : Token(token), Prototype(std::move(prototype)), Body(std::move(body)) {}
    const PtrPrototypeAST &getPrototype() const { return Prototype; }
    const PtrCodeBlockAST &getBody() const { return Body; }
  private:
    TOKEN Token;
    PtrPrototypeAST Prototype;
    PtrCodeBlockAST Body;
};

class ProgramAST {
  public:
    ProgramAST(vector<PtrPrototypeAST> externs,
        vector<PtrFunctionAST> functions,
        vector<PtrGlobalVariableAST> globals)
          : Externs(std::make_move_iterator(externs.begin()),
              std::make_move_iterator(externs.end())),
          Functions(std::make_move_iterator(functions.begin()),
              std::make_move_iterator(functions.end())),
          Globals(std::make_move_iterator(globals.begin()),
              std::make_move_iterator(globals.end())){}
    const vector<PtrPrototypeAST> &getExterns() const { return Externs; }
    const vector<PtrFunctionAST> &getFunctions() const { return Functions; }
    const vector<PtrGlobalVariableAST> &getGlobals() const { return Globals; }
  private:
    vector<PtrPrototypeAST> Externs;
    vector<PtrFunctionAST> Functions;
    vector<PtrGlobalVariableAST> Globals;
};

#endif // _ABSTRACT_SYNTAX_H
