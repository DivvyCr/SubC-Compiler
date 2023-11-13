#include <map>
#include <string>
#include <tuple>

#include "abstract-syntax.h"
#include "lexer.h"

using std::string;
using std::vector;
using std::tuple;
using std::unique_ptr;
using std::make_unique;

using namespace minic_lexer;

namespace minic_parser {

  // UTILITIES:
  static TOKEN active_token; 
  static LEXER_DATA lexer_data;

  static std::map<int, int> operator_precedence = {
    {OR, 1},
    {AND, 2},
    {EQ, 3}, {NE, 3},
    {LT, 4}, {LE, 4}, {GT, 4}, {GE, 4},
    {PLUS, 5}, {MINUS, 5},
    {MULT, 6}, {DIV, 6}, {MOD, 6}
  };

  struct DECLARATIONS {
    vector<PtrPrototypeAST> externs;
    vector<PtrFunctionAST> functions;
    vector<PtrVariableDeclarationAST> globals;
  };

  // PARSING:
  PtrProgramAST parseProgram(FILE *input_file);
  static void parseExtern(DECLARATIONS &declarations);
  static void parseDeclaration(DECLARATIONS &declarations);
  static void parseGlobalVariableOrFunction(DECLARATIONS &declarations, int token_type);
  static void parseFunction(DECLARATIONS &declarations, TOKEN token, const string &ident, MiniCType return_type);
  static vector<PtrVariableDeclarationAST> parseParameters();
  static PtrVariableDeclarationAST parseParameter();

  // Statements:
  static vector<PtrStatementAST> parseStatements();
  static PtrStatementAST parseStatement();
  static PtrIfBlockAST parseIfBlock();
  static PtrWhileBlockAST parseWhileBlock();
  static PtrReturnAST parseReturnStatement();
  static PtrExpressionStatementAST parseExpressionStmt();
  static PtrCodeBlockAST parseCodeBlock();
  static vector<PtrVariableDeclarationAST> parseBlockDecls();

  // Expressions:
  static PtrExpressionAST parseExpression();
  static PtrExpressionAST parseOptAssign(TOKEN token, const string &ident);
  static PtrExpressionAST parseExpressionOp(int exprPrec, PtrExpressionAST lhs);
  static PtrExpressionAST parseNegation();
  static PtrExpressionAST parseParens();
  static PtrExpressionAST parseAtomicValue();
  static PtrVariableDeclarationAST parseVariable(TOKEN token, int token_type, const string &ident);
  static PtrExpressionAST parseIdentifier(TOKEN token, const string &ident);
  static vector<PtrExpressionAST> parseArguments();

  // Utilities:
  static TOKEN getNextToken();

  static bool isExtern(int token_type);
  static bool isVarType(int token_type);
  static bool isAnyType(int token_type);
  static bool isLiteral(int token_type);
  static bool isExpression(int token_type);
  static bool isStatement(int token_type);
  static bool isExpressionEnd(int token_type);
  static bool isOperator(int token_type);

  static MiniCType convertType(int token_type);
  static std::nullptr_t throwError(const char *msg);

}
