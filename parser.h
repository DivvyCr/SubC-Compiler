#include <map>

#include "abstract-syntax.h"
#include "lexer.h"

using std::string;
using std::vector;
using std::unique_ptr;
using std::make_unique;

using namespace abstract_syntax;

namespace parser {

  static std::map<int, int> operator_precedence = {
    {OR, 1},
    {AND, 2},
    {EQ, 3}, {NE, 3},
    {LT, 4}, {LE, 4}, {GT, 4}, {GE, 4},
    {PLUS, 5}, {MINUS, 5},
    {MULT, 6}, {DIV, 6}, {MOD, 6}
  };

  static LEXER_DATA lexer_data;
  static TOKEN active_token; 

  unique_ptr<AST> startParse(FILE *input_file);

  // Program:
  static unique_ptr<ProgramAST> parseProgram();
  static unique_ptr<PrototypeAST> parseExtern();

  using DeclPair = std::pair<vector<unique_ptr<FunctionAST>>, vector<unique_ptr<GlobalVariableAST>>>;

  static DeclPair parseDeclList();
  static DeclPair parseDecl(DeclPair decls);
  static DeclPair parseDeclExt(int token_type, DeclPair decls);

  static DeclPair parseFuncSpec(TOKEN token, const string &ident, MiniCType return_type, DeclPair decls);
  static vector<unique_ptr<VariableAST>> parseParameters();
  static unique_ptr<VariableAST> parseParameter();

  // Statements:
  static vector<unique_ptr<StatementAST>> parseStatements();
  static unique_ptr<StatementAST> parseStatement();
  static unique_ptr<IfBlockAST> parseIfBlock();
  static unique_ptr<WhileBlockAST> parseWhileBlock();
  static unique_ptr<ReturnAST> parseReturnStatement();
  static unique_ptr<ExpressionAST> parseExpressionStmt();
  static unique_ptr<CodeBlockAST> parseCodeBlock();
  static vector<unique_ptr<VariableAST>> parseBlockDecls();

  // Expressions:
  static unique_ptr<ExpressionAST> parseExpression();
  static unique_ptr<ExpressionAST> parseOptAssign(TOKEN token, const string &ident);
  static unique_ptr<ExpressionAST> parseExpressionOp(int exprPrec, unique_ptr<ExpressionAST> lhs);
  static unique_ptr<ExpressionAST> parseNegation();
  static unique_ptr<ExpressionAST> parseParens();
  static unique_ptr<ExpressionAST> parseAtomicValue();
  static unique_ptr<VariableAST> parseVariable(TOKEN token, int token_type, const string &ident);
  static unique_ptr<ExpressionAST> parseIdentifier(TOKEN token, const string &ident);
  static vector<unique_ptr<ExpressionAST>> parseArguments();

  // Utilities:
  static TOKEN getNextToken();

  static bool isVarType(int token_type);
  static bool isAnyType(int token_type);
  static bool isLiteral(int token_type);
  static bool isExpression(int token_type);
  static bool isStatement(int token_type);
  static bool isExpressionEnd(int token_type);
  static bool isOperator(int token_type);

  static std::nullptr_t raiseError(const char *msg);

}
