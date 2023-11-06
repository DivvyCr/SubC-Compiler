#include "abstract-syntax.h"

namespace abstract_syntax {

  IntAST::IntAST(TOKEN token, int value)
    : Token(token), Value(value) {}

  int IntAST::getValue() const { return Value; }

  FloatAST::FloatAST(TOKEN token, float value)
    : Token(token), Value(value) {}

  BoolAST::BoolAST(TOKEN token, bool value)
    : Token(token), Value(value) {}

  AssignmentAST::AssignmentAST(TOKEN token, const string &ident,
      unique_ptr<ExpressionAST> assignment)
    : Token(token), Identifier(ident),
    Assignment(std::move(assignment)) {}

  FunctionCallAST::FunctionCallAST(TOKEN token, const string &ident,
      vector<unique_ptr<ExpressionAST>> arguments)
    : Token(token), Identifier(ident),
    Arguments(std::make_move_iterator(arguments.begin()),
        std::make_move_iterator(arguments.end())) {}

  UnaryExpressionAST::UnaryExpressionAST(TOKEN op, unique_ptr<ExpressionAST> expression)
      : Operator(op), Expression(std::move(expression)) {}

  BinaryExpressionAST::BinaryExpressionAST(TOKEN op,
      unique_ptr<ExpressionAST> left_expression,
      unique_ptr<ExpressionAST> right_expression)
    : Operator(op), Left(std::move(left_expression)), Right(std::move(right_expression)) {}

  CodeBlockAST::CodeBlockAST(TOKEN token, 
      vector<unique_ptr<VariableAST>> declarations,
      vector<unique_ptr<StatementAST>> statements)
    : Token(token),
    Declarations(std::make_move_iterator(declarations.begin()),
        std::make_move_iterator(declarations.end())),
    Statements(std::make_move_iterator(statements.begin()),
        std::make_move_iterator(statements.end())) {}
  
  IfBlockAST::IfBlockAST(TOKEN token,
      unique_ptr<ExpressionAST> condition,
      unique_ptr<CodeBlockAST> true_branch)
    : Token(token), Condition(std::move(condition)),
    TrueBranch(std::move(true_branch)) {}
  IfBlockAST::IfBlockAST(TOKEN token,
      unique_ptr<ExpressionAST> condition,
      unique_ptr<CodeBlockAST> true_branch,
      unique_ptr<CodeBlockAST> false_branch)
    : Token(token), Condition(std::move(condition)),
    TrueBranch(std::move(true_branch)),
    FalseBranch(std::move(false_branch)) {}

  WhileBlockAST::WhileBlockAST(TOKEN token,
      unique_ptr<ExpressionAST> condition, unique_ptr<StatementAST> body)
    : Token(token), Condition(std::move(condition)), Body(std::move(body)) {}

  ReturnAST::ReturnAST(TOKEN token) : Token(token) {}
  ReturnAST::ReturnAST(TOKEN token, unique_ptr<ExpressionAST> body)
    : Token(token), Body(std::move(body)) {}

  GlobalVariableAST::GlobalVariableAST(unique_ptr<VariableAST> variable)
    : Variable(std::move(variable)) {}

  PrototypeAST::PrototypeAST(TOKEN token, const string &ident, MiniCType return_type,
      vector<unique_ptr<VariableAST>> parameters)
    : Token(token), Identifier(ident), ReturnType(return_type),
    Parameters(std::make_move_iterator(parameters.begin()),
        std::make_move_iterator(parameters.end())) {}

  FunctionAST::FunctionAST(TOKEN token,
      unique_ptr<PrototypeAST> prototype,
      unique_ptr<CodeBlockAST> body)
    : Token(token), Prototype(std::move(prototype)), Body(std::move(body)) {}

  ProgramAST::ProgramAST(vector<unique_ptr<PrototypeAST>> externs,
      vector<unique_ptr<FunctionAST>> functions,
      vector<unique_ptr<GlobalVariableAST>> globals)
        : Externs(std::make_move_iterator(externs.begin()),
            std::make_move_iterator(externs.end())),
        Functions(std::make_move_iterator(functions.begin()),
            std::make_move_iterator(functions.end())),
        Globals(std::make_move_iterator(globals.begin()),
            std::make_move_iterator(globals.end())){}

}
