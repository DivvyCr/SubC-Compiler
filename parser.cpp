#include "parser.h"

namespace minic_parser {

  PtrProgramAST parseProgram(FILE *input_file) {
    lexer_data.input_file = input_file;
    lexer_data.line_num = 1;
    lexer_data.column_num = 1;
    getNextToken(); // Consume first token (always INVALID).

    DECLARATIONS declarations;
    vector<PtrGlobalVariableAST> globals;
    vector<PtrPrototypeAST> externs;
    vector<PtrFunctionAST> functions;
    declarations.globals = std::move(globals);
    declarations.externs = std::move(externs);
    declarations.functions = std::move(functions);

    while (isExtern(active_token.type)) parseExtern(declarations);
    while (isAnyType(active_token.type)) parseDeclaration(declarations);

    return make_unique<ProgramAST>(
        std::move(declarations.globals),
        std::move(declarations.externs),
        std::move(declarations.functions));
  }

  static void parseExtern(DECLARATIONS &decls) {
    TOKEN token = active_token;
    getNextToken(); // Consume EXTERN
 
    // RETURN TYPE:
    if (!isAnyType(active_token.type)) {
      throwError("parseExtern: Expected TYPE");
      return;
    }
    MiniCType return_type = convertType(active_token.type);
    getNextToken();

    // IDENTIFIER:
    if (active_token.type != IDENT) {
      throwError("parseExtern: Expected identifier (for an extern)");
      return;
    }
    string identifier = lexer_data.identifier_val;
    getNextToken();

    // PARAMETERS:
    if (active_token.type != LPAR) {
      throwError("parseExtern: Expected '('");
      return;
    }
    getNextToken();

    vector<PtrVariableAST> parameters = parseParameters();

    if (active_token.type != RPAR) {
      throwError("parseExtern: Expected ')'");
      return;
    }
    getNextToken();

    // EOL:
    if (active_token.type != SC) {
      throwError("parseExtern: Expected ';'");
      return;
    }
    getNextToken();

    PtrPrototypeAST ext = make_unique<PrototypeAST>(token, identifier, return_type, std::move(parameters));
    decls.externs.push_back(std::move(ext));
  }

  static void parseDeclaration(DECLARATIONS &decls) {
    int decl_type = active_token.type;
    getNextToken(); // Consume TYPE

    if (isVarType(decl_type)) {
      return parseGlobalVariableOrFunction(decls, decl_type);
    } else if (decl_type == VOID_TOK) {
      if (active_token.type != IDENT) {
        throwError("parseDeclaration: Expected identifier (for function)");
      }
      TOKEN token = active_token;
      string identifier = lexer_data.identifier_val;
      getNextToken(); // Consume IDENT
      return parseFunction(decls, token, identifier, VOID);
    }
    throwError("parseDeclaration: Error");
  }

  static void parseGlobalVariableOrFunction(DECLARATIONS &decls, int token_type) {
    if (active_token.type != IDENT) {
      throwError("parseGlobalVariableOrFunction: Expected identifier (for global variable or function)");
    }
    TOKEN token = active_token;
    string identifier = lexer_data.identifier_val;
    getNextToken(); // Consume IDENT

    if (active_token.type == SC) {
      getNextToken(); // Consume ;
      PtrVariableAST variable = parseVariable(token, token_type, identifier);
      PtrGlobalVariableAST global = std::make_unique<GlobalVariableAST>(std::move(variable));
      decls.globals.push_back(std::move(global));
      return;
    } else if (active_token.type == LPAR) {
      MiniCType return_type = convertType(token_type); // NOTE: We passed isVarType(..) check prior.
      return parseFunction(decls, token, identifier, return_type);
    }
    throwError("parseGlobalVariableOrFunction: Expected ';' or '('");
  }

  static void parseFunction(DECLARATIONS &decls, TOKEN token, const string &identifier, MiniCType return_type) {
    getNextToken(); // Consume (
    vector<PtrVariableAST> parameters = parseParameters();
    if (active_token.type != RPAR) {
      throwError("parseFunction: Expected ')'");
    }
    getNextToken(); // Consume )
    PtrCodeBlockAST body = parseCodeBlock();

    PtrPrototypeAST prototype = make_unique<PrototypeAST>(token, identifier, return_type, std::move(parameters));
    PtrFunctionAST function = make_unique<FunctionAST>(token, std::move(prototype), std::move(body));
    decls.functions.push_back(std::move(function));
  }

  static vector<PtrVariableAST> parseParameters() {
    vector<PtrVariableAST> parameters;

    if (isVarType(active_token.type)) {
      PtrVariableAST first_parameter = parseParameter();
      if (!first_parameter) {
        throwError("parseParameters: Error parsing parameter");
        return parameters;
      }
      parameters.push_back(std::move(first_parameter));

      while (active_token.type == COMMA) {
        getNextToken(); // Consume COMMA
        PtrVariableAST next_parameter = parseParameter();
        if (!next_parameter) {
          throwError("parseParameters: Error parsing parameters");
          return parameters;
        }
        parameters.push_back(std::move(next_parameter));
      }
    } else if (active_token.type == VOID_TOK) {
      getNextToken(); // Consume void
    }

    return parameters;
  }

  static PtrVariableAST parseParameter() {
    if (!isVarType(active_token.type)) {
      return throwError("parseParameter: Expected TYPE");
    }
    int token_type = active_token.type;
    getNextToken(); // Consume TYPE

    if (active_token.type != IDENT) {
      return throwError("parseParameter: Expected identifier (for parameter)");
    }
    TOKEN token = active_token; 
    string identifier = lexer_data.identifier_val;
    getNextToken(); // Consume IDENT

    return parseVariable(token, token_type, identifier);
  }

  static PtrIfBlockAST parseIfBlock() {
    TOKEN token = active_token;
    getNextToken(); // Consume IF
    if (active_token.type != LPAR) {
      return throwError("parseIfBlock: Expected '('");
    }
    getNextToken(); // Consume (

    if (!isExpression(active_token.type)) {
      return throwError("parseIfBlock: Expected EXPR");
    }
    PtrExpressionAST condition = parseExpression();

    if (active_token.type != RPAR) {
      return throwError("parseIfBlock: Expected ')'");
    }
    getNextToken(); // Consume )

    if (!isStatement(active_token.type)) {
      throwError("parseIfBlock: Expected STMT");
    }
    PtrCodeBlockAST true_branch = parseCodeBlock();

    if (active_token.type == ELSE) {
      getNextToken(); // Consume ELSE
      if (active_token.type != LBRA) {
        return throwError("parseElseBlock: Expected '{'");
      }
      PtrCodeBlockAST false_branch = parseCodeBlock();
      return make_unique<IfBlockAST>(token, std::move(condition), std::move(true_branch), std::move(false_branch));
    }

    return make_unique<IfBlockAST>(token, std::move(condition), std::move(true_branch));
  }

  static PtrWhileBlockAST parseWhileBlock() {
    TOKEN token = active_token;
    getNextToken(); // Consume WHILE
    if (active_token.type != LPAR) {
      return throwError("parseWhileBlock: Expected '('");
    }
    getNextToken(); // Consume (

    if (!isExpression(active_token.type)) {
      return throwError("parseWhileBlock: Expected EXPR");
    }
    PtrExpressionAST condition = parseExpression();

    if (active_token.type != RPAR) {
      return throwError("parseWhileBlock: Expected ')'");
    }
    getNextToken(); // Consume )

    if (!isStatement(active_token.type)) {
      return throwError("parseWhileBlock: Expected STMT");
    }
    PtrStatementAST body = parseStatement();
    return make_unique<WhileBlockAST>(token, std::move(condition), std::move(body));
  }

  static PtrReturnAST parseReturnStatement() {
    TOKEN token = active_token;
    getNextToken(); // consume RETURN
    if (active_token.type == SC) {
      return make_unique<ReturnAST>(token);
    } else if (isExpression(active_token.type)) {
      PtrExpressionAST body = parseExpression();
      if (active_token.type != SC) {
        return throwError("parseReturnStatement: Expected ';'");
      }
      getNextToken(); // Consume ;
      return make_unique<ReturnAST>(token, std::move(body));
    }
    return throwError("parseReturnStatement: Expected ';' or an expression");
  }

  static PtrCodeBlockAST parseCodeBlock() {
    TOKEN token = active_token;
    getNextToken(); // Consume {
    vector<PtrVariableAST> declarations = parseBlockDecls();
    vector<PtrStatementAST> statements = parseStatements();
    if (active_token.type != RBRA) {
      return throwError("parseCodeBlock: Expected '}'");
    }
    getNextToken(); // Consume }

    return make_unique<CodeBlockAST>(token, std::move(declarations), std::move(statements));
  }

  static vector<PtrVariableAST> parseBlockDecls() {
    vector<PtrVariableAST> declarations;

    while (isVarType(active_token.type)) {
      int token_type = active_token.type;
      getNextToken(); // Consume TYPE
      if (active_token.type != IDENT) {
        throwError("parseBlockDecls: Expected identifier (for code block declaration)");
        return declarations;
      }
      TOKEN token = active_token;
      string identifier = lexer_data.identifier_val;
      getNextToken(); // Consume IDENT

      if (active_token.type != SC) {
        throwError("parseBlockDecls: Expected ';'");
        return declarations;
      }
      getNextToken(); // Consume ;
      PtrVariableAST decl = parseVariable(token, token_type, identifier);
      declarations.push_back(std::move(decl));
    }

    if (isStatement(active_token.type) ||
        active_token.type == RBRA) {
      return declarations;
    }

    throwError("parseBlockDecls: Expected a variable declaration or a statement.");
    return vector<PtrVariableAST>();
  }

  static vector<PtrStatementAST> parseStatements() {
    vector<PtrStatementAST> statements;

    while (isStatement(active_token.type)) {
      if (active_token.type == SC) {
        getNextToken(); // Consume ; (Empty expression.)
        continue;
      }
      PtrStatementAST statement = parseStatement();
      statements.push_back(std::move(statement));
    }

    return statements;
  }

  static PtrStatementAST parseStatement() {
    if (isExpression(active_token.type)) {
      return parseExpressionStmt();
    } else if (active_token.type == LBRA) {
      return parseCodeBlock();
    } else if (active_token.type == IF) {
      return parseIfBlock();
    } else if (active_token.type == WHILE) {
      return parseWhileBlock();
    } else if (active_token.type == RETURN) {
      return parseReturnStatement();
    }

    return throwError("parseStatement: Expected an expression, a code block, or a statement");
  }

  static PtrExpressionStatementAST parseExpressionStmt() {
    if (isExpression(active_token.type)) {
      PtrExpressionAST e = parseExpression();
      if (active_token.type != SC) {
        return throwError("parseExpressionStmt: Expected ';'");
      }
      getNextToken(); // Consume ;
      return make_unique<ExpressionStatementAST>(std::move(e));
    }
    return throwError("parseExpressionStmt: Expected an expression ending in ';'");
  }

  static PtrExpressionAST parseExpression() {
    if (active_token.type == IDENT) {
      TOKEN token = active_token;
      string identifier = lexer_data.identifier_val;
      getNextToken(); // Consume IDENT
      return parseOptAssign(token, identifier);
    } else if (isLiteral(active_token.type) ||
        active_token.type == MINUS ||
        active_token.type == NOT ||
        active_token.type == LPAR) {
      PtrExpressionAST first_operand = parseNegation();
      if (isOperator(active_token.type) || active_token.type == LPAR) {
        return parseExpressionOp(0, std::move(first_operand));
      } else if (isExpressionEnd(active_token.type)) {
        return first_operand;
      }
    }
    return throwError("parseExpression: Expected a numeric/boolean expression, an assignment, or ';'");
  }

  static PtrExpressionAST parseOptAssign(TOKEN token, const string &ident) {
    if (active_token.type == ASSIGN) {
      getNextToken(); // Consume =
      PtrExpressionAST assignment = parseExpression();
      return make_unique<AssignmentAST>(token, ident, std::move(assignment));
    } else if (isExpressionEnd(active_token.type) ||
        isOperator(active_token.type) || active_token.type == LPAR) {
      PtrExpressionAST first_operand = parseIdentifier(token, ident);
      return parseExpressionOp(0, std::move(first_operand));
    }

    return throwError("parseOptAssign: Expected a numeric/boolean expression or an assignment (following an identifier)");
  }

  static PtrExpressionAST parseExpressionOp(int exprPrec, PtrExpressionAST lhs) {
    while (true) {
      int token_prec = operator_precedence[active_token.type];

      if (token_prec < exprPrec || !isOperator(active_token.type)) {
        return lhs;
      }

      TOKEN op = active_token;
      getNextToken(); // Consume OPERATOR

      PtrExpressionAST rhs = parseNegation(); // ParsePrimary() in LLVM tutorial implies atomic value
      int next_prec = operator_precedence[active_token.type];
      if (token_prec < next_prec) {
        rhs = parseExpressionOp(token_prec + 1, std::move(rhs));
      }

      lhs = make_unique<BinaryExpressionAST>(op, std::move(lhs), std::move(rhs));
    }
    return lhs;
  }

  static PtrExpressionAST parseNegation() {
    TOKEN op = active_token;
    if (op.type == MINUS || op.type == NOT) {
      getNextToken(); // Consume OPERATOR
      PtrExpressionAST operand = parseNegation();
      return make_unique<UnaryExpressionAST>(op, std::move(operand));
    } else if (isLiteral(op.type) ||
        op.type == IDENT ||
        op.type == LPAR) {
      return parseParens();
    }
    return throwError("parseNegation: Expected unary operand or expression");
  }

  static PtrExpressionAST parseParens() {
    if (active_token.type == LPAR) {
      getNextToken(); // Consume (
      PtrExpressionAST expr = parseExpression();
      if (active_token.type == RPAR) {
        getNextToken(); // Consume )
        return expr;
      }
    } else if (isLiteral(active_token.type) ||
        active_token.type == IDENT) {
      return parseAtomicValue();
    }
    return throwError("parseParens: Expected parentheses or expression");
  }

  static PtrExpressionAST parseAtomicValue() {
    TOKEN token = active_token;
    if (active_token.type == IDENT) {
      string ident = lexer_data.identifier_val;
      getNextToken(); // Consume IDENT
      return parseIdentifier(token, ident);
    } else if (isLiteral(active_token.type)) {
      getNextToken(); // Consume VALUE
      switch (token.type) {
        case INT_LIT:
          return make_unique<IntAST>(token, lexer_data.int_val);
        case FLOAT_LIT:
          return make_unique<FloatAST>(token, lexer_data.float_val);
        case BOOL_LIT:
          return make_unique<BoolAST>(token, lexer_data.bool_val);
        default:
          break;
      }
    }
    return throwError("parseAtomicValue: Expected identifier or literal");
  }

  static PtrExpressionAST parseIdentifier(TOKEN token, const string &ident) {
    if (active_token.type == LPAR) {
      getNextToken(); // Consume (
      vector<PtrExpressionAST> arguments = parseArguments();
      if (active_token.type != RPAR) {
        return throwError("parseIdentifier: Expected ')'");
      }
      getNextToken(); // Consume )
      return make_unique<FunctionCallAST>(token, ident, std::move(arguments));
    } else if (isExpressionEnd(active_token.type) || isOperator(active_token.type)) {
      return make_unique<VariableLoadAST>(token, ident);
    }
    return throwError("parseIdentifier: Expected variable or function call");
  }

  static vector<PtrExpressionAST> parseArguments() {
    vector<PtrExpressionAST> arguments;

    if (isExpression(active_token.type)) {
      PtrExpressionAST first_argument = parseExpression();
      arguments.push_back(std::move(first_argument));

      while (active_token.type == COMMA) {
        getNextToken(); // Consume COMMA
        PtrExpressionAST next_argument = parseExpression();
        arguments.push_back(std::move(next_argument));
      }
    }

    return arguments;
  }

  static PtrVariableAST parseVariable(TOKEN token, int token_type, const string &ident) {
    MiniCType variable_type;
    switch (token_type) {
      case INT_TOK:
        variable_type = INTEGER;
        break;
      case FLOAT_TOK:
        variable_type = FLOAT;
        break;
      case BOOL_TOK:
        variable_type = BOOL;
        break;
      default:
        return throwError("parseVariable: Expected variable type (non-VOID).");
    }
    return make_unique<VariableAST>(make_unique<VariableLoadAST>(token, ident), variable_type);
  }

  //
  // Utilities:
  //
  
  static TOKEN getNextToken() {
    lexer_data = minic_lexer::updateData(lexer_data);
    return active_token = lexer_data.token;
  }

  static std::nullptr_t throwError(const char *msg) {
    fprintf(stderr, "Error: %s [%d:%d]\n", msg, active_token.line_num, active_token.column_num);
    exit(1);
    return nullptr;
  }

  static MiniCType convertType(int token_type) {
    MiniCType ret;
    switch (token_type) {
      case INT_TOK:
        ret = INTEGER;
        break;
      case FLOAT_TOK:
        ret = FLOAT;
        break;
      case BOOL_TOK:
        ret = BOOL;
        break;
      case VOID_TOK:
        ret = VOID;
        break;
      default:
        break;
    }
    return ret;
  }

  static bool isExtern(int token_type) {
    return (token_type == EXTERN);
  }

  static bool isVarType(int token_type) {
    return (token_type == INT_TOK || token_type == FLOAT_TOK || token_type == BOOL_TOK);
  }

  static bool isAnyType(int token_type) {
    return (isVarType(token_type) || token_type == VOID_TOK);
  }

  static bool isLiteral(int token_type) {
    return (token_type == INT_LIT || token_type == FLOAT_LIT || token_type == BOOL_LIT);
  }

  static bool isExpression(int token_type) {
    return (isLiteral(token_type) || token_type == SC ||
        token_type == MINUS || token_type == NOT ||
        token_type == LPAR || token_type == IDENT);
  }

  static bool isStatement(int token_type) {
    return (isExpression(token_type) || token_type == LBRA ||
        token_type == IF || token_type == WHILE || token_type == RETURN);
  }

  static bool isOperator(int token_type) {
    return (token_type == OR || token_type == AND ||
        token_type == EQ || token_type == NE ||
        token_type == LE || token_type == LT ||
        token_type == GE || token_type == GT ||
        token_type == PLUS || token_type == MINUS ||
        token_type == MULT || token_type == DIV || token_type == MOD);
  }

  static bool isExpressionEnd(int token_type) {
    return (token_type == COMMA || token_type == RPAR || token_type == SC);
  }
}
