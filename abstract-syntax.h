#ifndef _ABSTRACT_SYNTAX_H
#define _ABSTRACT_SYNTAX_H

#include <memory>
#include <string>
#include <vector>

#include "llvm/Support/raw_ostream.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Value.h"

#include "visitor.h"

using std::string;
using std::vector;
using std::unique_ptr;
using llvm::Function;
using llvm::Value;

namespace abstract_syntax {

  enum TOKEN_TYPE {
    IDENT = -1,        // [a-zA-Z_][a-zA-Z_0-9]*
    ASSIGN = int('='), // '='

    // delimiters
    LBRA = int('{'),  // left brace
    RBRA = int('}'),  // right brace
    LPAR = int('('),  // left parenthesis
    RPAR = int(')'),  // right parenthesis
    SC = int(';'),    // semicolon
    COMMA = int(','), // comma

    // types
    INT_TOK = -2,   // "int"
    VOID_TOK = -3,  // "void"
    FLOAT_TOK = -4, // "float"
    BOOL_TOK = -5,  // "bool"

    // keywords
    EXTERN = -6,  // "extern"
    IF = -7,      // "if"
    ELSE = -8,    // "else"
    WHILE = -9,   // "while"
    RETURN = -10, // "return"

                  // literals
    INT_LIT = -14,   // [0-9]+
    FLOAT_LIT = -15, // [0-9]+.[0-9]+
    BOOL_LIT = -16,  // "true" or "false" key words

    // logical operators
    AND = -17, // "&&"
    OR = -18,  // "||"

    // operators
    PLUS = int('+'),    // addition or unary plus
    MINUS = int('-'),   // substraction or unary negative
    MULT = int('*'), // multiplication
    DIV = int('/'),     // division
    MOD = int('%'),     // modular
    NOT = int('!'),     // unary negation

    // comparison operators
    EQ = -19,      // equal
    NE = -20,      // not equal
    LE = -21,      // less than or equal to
    LT = int('<'), // less than
    GE = -23,      // greater than or equal to
    GT = int('>'), // greater than

    // Special: 
    EOF_TOK = 0, // End of file.
    INVALID = -100
  };

  struct TOKEN {
    int type = INVALID;
    string lexeme;
    int line_num;
    int column_num;
  };

  struct LEXER_DATA {
    TOKEN token;
    string identifier_val; // Filled in if IDENT
    int int_val;           // Filled in if INT_LIT
    bool bool_val;         // Filled in if BOOL_LIT
    float float_val;       // Filled in if FLOAT_LIT
 
    FILE* input_file;
    int line_num;
    int column_num;
  };

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

  // TODO: Export TOKEN to AST, and make universal constructor with it?
  class AST {
    public:
      virtual ~AST() {}
      virtual void *dispatch(Visitor &visitor) = 0;
  };

  class StatementAST : public AST {};
  class ExpressionAST : public StatementAST {};

  class VariableAST : public ExpressionAST {
    public:
      VariableAST(TOKEN token, const string &ident)
        : Token(token), Identifier(ident), Type(UNKNOWN) {}
      VariableAST(TOKEN token, const string &ident, MiniCType type)
        : Token(token), Identifier(ident), Type(type) {}
      void *dispatch(Visitor &visitor) override { return visitor.visit(*this); }
      string getIdentifier() const { return Identifier; }
      MiniCType getType() { return Type; }
    private:
      TOKEN Token;
      string Identifier;
      MiniCType Type;
  };

  class IntAST : public ExpressionAST {
    public:
      IntAST(TOKEN token, int value);
      void *dispatch(Visitor &visitor) override { return visitor.visit(*this); }
      int getValue() const;
    private:
      TOKEN Token;
      int Value;
  };

  class FloatAST : public ExpressionAST {
    public:
      FloatAST(TOKEN token, float value);
      void *dispatch(Visitor &visitor) override { return visitor.visit(*this); }
      float getValue() const { return Value; }
    private:
      TOKEN Token;
      float Value;
  };

  class BoolAST : public ExpressionAST {
    public:
      BoolAST(TOKEN token, bool value);
      void *dispatch(Visitor &visitor) override { return visitor.visit(*this); }
      bool getValue() const { return Value; }
    private:
      TOKEN Token;
      bool Value;
  };

  class AssignmentAST : public ExpressionAST {
    public:
      AssignmentAST(TOKEN token, const string &ident,
          unique_ptr<ExpressionAST> assignment);
      void *dispatch(Visitor &visitor) override { return visitor.visit(*this); }
      string getIdentifier() const { return Identifier; }
      unique_ptr<ExpressionAST> &getAssignment() { return Assignment; }
    private:
      TOKEN Token;
      string Identifier;
      unique_ptr<ExpressionAST> Assignment;
  };

  class FunctionCallAST : public ExpressionAST {
    public:
      FunctionCallAST(TOKEN token, const string &ident,
          vector<unique_ptr<ExpressionAST>> arguments);
      void *dispatch(Visitor &visitor) override { return visitor.visit(*this); }
      string getIdentifier() const { return Identifier; }
      vector<unique_ptr<ExpressionAST>> &getArguments() { return Arguments; }
    private:
      TOKEN Token;
      string Identifier;  
      vector<unique_ptr<ExpressionAST>> Arguments;
  };

  class UnaryExpressionAST : public ExpressionAST {
    public:
      UnaryExpressionAST(TOKEN op,
          unique_ptr<ExpressionAST> expression);
      void *dispatch(Visitor &visitor) override { return visitor.visit(*this); }
      TOKEN getOperator() const { return Operator; }
      unique_ptr<ExpressionAST> &getExpression() { return Expression; }
    private:
      TOKEN Operator;
      unique_ptr<ExpressionAST> Expression;
  };

  class BinaryExpressionAST : public ExpressionAST {
    public:
      BinaryExpressionAST(TOKEN op,
          unique_ptr<ExpressionAST> left_expression,
          unique_ptr<ExpressionAST> right_expression);
      void *dispatch(Visitor &visitor) override { return visitor.visit(*this); }
      TOKEN getOperator() const { return Operator; }
      unique_ptr<ExpressionAST> &getLeft() { return Left; }
      unique_ptr<ExpressionAST> &getRight() { return Right; }
    private:
      TOKEN Operator;
      unique_ptr<ExpressionAST> Left, Right;
  };

  class CodeBlockAST : public StatementAST {
    public:
      CodeBlockAST(TOKEN token, 
          vector<unique_ptr<VariableAST>> declarations,
          vector<unique_ptr<StatementAST>> statements);
      void *dispatch(Visitor &visitor) override { return visitor.visit(*this); }
      vector<unique_ptr<VariableAST>> &getDeclarations() { return Declarations; }
      vector<unique_ptr<StatementAST>> &getStatements() { return Statements; }
    private:
      TOKEN Token;
      vector<unique_ptr<VariableAST>> Declarations;
      vector<unique_ptr<StatementAST>> Statements;
  };

  class IfBlockAST : public StatementAST {
    public:
      IfBlockAST(TOKEN token,
          unique_ptr<ExpressionAST> condition,
          unique_ptr<CodeBlockAST> true_branch);
      IfBlockAST(TOKEN token,
          unique_ptr<ExpressionAST> condition,
          unique_ptr<CodeBlockAST> true_branch,
          unique_ptr<CodeBlockAST> false_branch);
      void *dispatch(Visitor &visitor) override { return visitor.visit(*this); }
      unique_ptr<ExpressionAST> &getCondition() { return Condition; }
      unique_ptr<CodeBlockAST> &getTrueBranch() { return TrueBranch; }
      unique_ptr<CodeBlockAST> &getFalseBranch() { return FalseBranch; };
    private:
      TOKEN Token;
      unique_ptr<ExpressionAST> Condition;
      unique_ptr<CodeBlockAST> TrueBranch, FalseBranch;
  };

  class WhileBlockAST : public StatementAST {
    public:
      WhileBlockAST(TOKEN token,
          unique_ptr<ExpressionAST> condition,
          unique_ptr<StatementAST> body); 
      void *dispatch(Visitor &visitor) override { return visitor.visit(*this); }
      unique_ptr<ExpressionAST> &getCondition() { return Condition; }
      unique_ptr<StatementAST> &getBody() { return Body; }
    private:
      TOKEN Token;
      unique_ptr<ExpressionAST> Condition;
      unique_ptr<StatementAST> Body;
  };

  class ReturnAST : public StatementAST {
    public:
      ReturnAST(TOKEN token);
      ReturnAST(TOKEN token, unique_ptr<ExpressionAST> b);
      void *dispatch(Visitor &visitor) override { return visitor.visit(*this); }
      unique_ptr<ExpressionAST> &getBody() { return Body; }
    private:
      TOKEN Token;
      unique_ptr<ExpressionAST> Body;
  };

  class PrototypeAST : public AST {
    public:
      PrototypeAST(TOKEN token, const string &ident, MiniCType type,
          vector<unique_ptr<VariableAST>> parameters);
      void *dispatch(Visitor &visitor) override { return visitor.visit(*this); }
      string getIdentifier() const { return Identifier; }
      MiniCType getReturnType() const { return ReturnType; }
      vector<unique_ptr<VariableAST>> &getParameters() { return Parameters; }
      MiniCType getType();
    private:
      // TODO: Add return type!
      TOKEN Token;
      string Identifier;
      MiniCType ReturnType;
      vector<unique_ptr<VariableAST>> Parameters;
  };

  class FunctionAST : public AST {
    public:
      FunctionAST(TOKEN token,
          unique_ptr<PrototypeAST> prototype,
          unique_ptr<CodeBlockAST> body);
      void *dispatch(Visitor &visitor) override { return visitor.visit(*this); }
      unique_ptr<PrototypeAST> &getPrototype() { return Prototype; }
      unique_ptr<CodeBlockAST> &getBody() { return Body; }
    private:
      TOKEN Token;
      unique_ptr<PrototypeAST> Prototype;
      unique_ptr<CodeBlockAST> Body;
  };

  class ProgramAST : public AST {
    public:
      ProgramAST(vector<unique_ptr<PrototypeAST>> externs,
          vector<unique_ptr<AST>> declarations);
      void *dispatch(Visitor &visitor) override { return visitor.visit(*this); }
      vector<unique_ptr<PrototypeAST>> &getExterns() { return Externs; }
      vector<unique_ptr<AST>> &getDeclarations() { return Declarations; }
    private:
      vector<unique_ptr<PrototypeAST>> Externs;
      vector<unique_ptr<AST>> Declarations;
  };

  //inline llvm::raw_ostream &operator<<(llvm::raw_ostream &os,
  //    unique_ptr<AST> ast) {
  //  os << ast->toString("");
  //  return os;
  //}

}

#endif // _ABSTRACT_SYNTAX_H
