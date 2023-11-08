#include <map>

#include "abstract-syntax.h"
#include "lexer.h"

using std::string;
using std::vector;
using std::unique_ptr;
using std::make_unique;

using namespace minic_lexer;

namespace minic_parser {

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

  PtrProgramAST startParse(FILE *input_file);

  // Program:
  static PtrProgramAST parseProgram();
  static PtrPrototypeAST parseExtern();

  using DeclPair = std::pair<vector<PtrFunctionAST>, vector<PtrGlobalVariableAST>>;

  static DeclPair parseDeclList();
  static DeclPair parseDecl(DeclPair decls);
  static DeclPair parseDeclExt(int token_type, DeclPair decls);

  static DeclPair parseFuncSpec(TOKEN token, const string &ident, MiniCType return_type, DeclPair decls);
  static vector<PtrVariableAST> parseParameters();
  static PtrVariableAST parseParameter();

  // Statements:
  static vector<PtrStatementAST> parseStatements();
  static PtrStatementAST parseStatement();
  static PtrIfBlockAST parseIfBlock();
  static PtrWhileBlockAST parseWhileBlock();
  static PtrReturnAST parseReturnStatement();
  static PtrExpressionStatementAST parseExpressionStmt();
  static PtrCodeBlockAST parseCodeBlock();
  static vector<PtrVariableAST> parseBlockDecls();

  // Expressions:
  static PtrExpressionAST parseExpression();
  static PtrExpressionAST parseOptAssign(TOKEN token, const string &ident);
  static PtrExpressionAST parseExpressionOp(int exprPrec, PtrExpressionAST lhs);
  static PtrExpressionAST parseNegation();
  static PtrExpressionAST parseParens();
  static PtrExpressionAST parseAtomicValue();
  static PtrVariableAST parseVariable(TOKEN token, int token_type, const string &ident);
  static PtrExpressionAST parseIdentifier(TOKEN token, const string &ident);
  static vector<PtrExpressionAST> parseArguments();

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
