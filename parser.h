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

  using DECLARATIONS = std::tuple<vector<PtrPrototypeAST>,
        vector<PtrFunctionAST>, vector<PtrGlobalVariableAST>>;

  // PARSING:
  PtrProgramAST parseProgram(FILE *input_file);
  static DECLARATIONS parseExtern(DECLARATIONS declarations);
  static DECLARATIONS parseDeclaration(DECLARATIONS declarations);
  static DECLARATIONS parseGlobalVariableOrFunction(DECLARATIONS declarations, int token_type);
  static DECLARATIONS parseFunction(DECLARATIONS declarations, TOKEN token, const string &ident, MiniCType return_type);
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

  static bool isExtern(int token_type);
  static bool isVarType(int token_type);
  static bool isAnyType(int token_type);
  static bool isLiteral(int token_type);
  static bool isExpression(int token_type);
  static bool isStatement(int token_type);
  static bool isExpressionEnd(int token_type);
  static bool isOperator(int token_type);

  static MiniCType convertType(int token_type);
  static std::nullptr_t raiseError(const char *msg);
  static std::nullptr_t propagateError(const char *msg);

}
