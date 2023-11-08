#include "parser.h"

namespace minic_parser {

  PtrProgramAST startParse(FILE *input_file) {
    lexer_data.input_file = input_file;
    lexer_data.line_num = 1;
    lexer_data.column_num = 1;
        
    return parseProgram();
  }

  static PtrProgramAST parseProgram() {
    getNextToken(); // Consume first token (always INVALID).
    
    vector<PtrPrototypeAST> externs;
    while (active_token.type == EXTERN) {
      PtrPrototypeAST first_extern = parseExtern();
      externs.push_back(std::move(first_extern));
    }

    std::pair<vector<PtrFunctionAST>, vector<PtrGlobalVariableAST>> ret;
    if (isAnyType(active_token.type)) {
      ret = parseDeclList();
    }

    return make_unique<ProgramAST>(std::move(externs), std::move(ret.first), std::move(ret.second));
  }

  static PtrPrototypeAST parseExtern() {
    TOKEN t = active_token;
    getNextToken(); // Consume EXTERN
 
    if (!isAnyType(active_token.type)) {
      return raiseError("parseExtern: Expected TYPE");
    }
    MiniCType return_type;
    switch (active_token.type) {
      case INT_TOK:
        return_type = INTEGER;
        break;
      case FLOAT_TOK:
        return_type = FLOAT;
        break;
      case BOOL_TOK:
        return_type = BOOL;
        break;
      case VOID_TOK:
        return_type = VOID;
        break;
      default:
        break; // Unreachable due to check above.
    }
    getNextToken();

    if (active_token.type != IDENT) {
      return raiseError("parseExtern: Expected IDENT");
    }
    string ident = lexer_data.identifier_val;
    getNextToken();

    if (active_token.type != LPAR) {
      return raiseError("parseExtern: Expected '('");
    }
    getNextToken();

    vector<PtrVariableAST> parameters = parseParameters();

    if (active_token.type != RPAR) {
      return raiseError("parseExtern: Expected ')'");
    }
    getNextToken();

    if (active_token.type != SC) {
      return raiseError("parseExtern: Expected ';'");
    }
    getNextToken();

    return make_unique<PrototypeAST>(t, ident, return_type, std::move(parameters));
  }

  static DeclPair parseDeclList() {
    vector<PtrFunctionAST> functions;
    vector<PtrGlobalVariableAST> globals;

    DeclPair ret = std::make_pair(std::move(functions), std::move(globals));

    while (isAnyType(active_token.type)) {
      ret = parseDecl(std::move(ret));
    }

    if (active_token.type == EOF_TOK) {
      return ret;
    }

    raiseError("parseDeclList");
    return std::make_pair(vector<PtrFunctionAST>(), vector<PtrGlobalVariableAST>());
  }

  static DeclPair parseDecl(DeclPair decls) {
    auto cur_type = active_token.type;
    getNextToken(); // Consume TYPE

    if (isVarType(cur_type)) {
      return parseDeclExt(cur_type, std::move(decls));
    } else if (cur_type == VOID_TOK) {
      TOKEN t = active_token;
      auto func_ident = lexer_data.identifier_val;
      getNextToken(); // Consume IDENT
      return parseFuncSpec(t, func_ident, VOID, std::move(decls));
    }

    return std::make_pair(vector<PtrFunctionAST>(), vector<PtrGlobalVariableAST>());
  }

  static DeclPair parseDeclExt(int token_type, DeclPair decls) {
    string ident = lexer_data.identifier_val;
    TOKEN t = active_token;
    getNextToken(); // Consume IDENT

    if (active_token.type == SC) {
      getNextToken(); // Consume ;
      PtrVariableAST v = parseVariable(t, token_type, ident);
      PtrGlobalVariableAST g = std::make_unique<GlobalVariableAST>(std::move(v));
      std::move(decls.second).push_back(std::move(g));
      return decls;
    } else if (active_token.type == LPAR) {
      MiniCType return_type;
      switch (token_type) {
        case INT_TOK:
          return_type = INTEGER;
          break;
        case FLOAT_TOK:
          return_type = FLOAT;
          break;
        case BOOL_TOK:
          return_type = BOOL;
          break;
        case VOID_TOK:
          return_type = VOID;
          break;
        default:
          break; // Unreachable due to check above.
      }
      return parseFuncSpec(t, ident, return_type, std::move(decls));
    }

    return std::make_pair(vector<PtrFunctionAST>(), vector<PtrGlobalVariableAST>());
  }

  static DeclPair parseFuncSpec(TOKEN token, const string &ident, MiniCType return_type, DeclPair decls) {
    getNextToken(); // Consume (
    vector<PtrVariableAST> parameters = parseParameters();
    getNextToken(); // Consume )
    PtrCodeBlockAST body = parseCodeBlock();

    PtrPrototypeAST proto = make_unique<PrototypeAST>(token, ident, return_type, std::move(parameters));
    PtrFunctionAST f = make_unique<FunctionAST>(token, std::move(proto), std::move(body));
    std::move(decls.first).push_back(std::move(f));
    return decls;
  }

  static vector<PtrVariableAST> parseParameters() {
    vector<PtrVariableAST> parameters;

    if (isVarType(active_token.type)) {
      PtrVariableAST first_parameter = parseParameter();
      parameters.push_back(std::move(first_parameter));

      while (active_token.type == COMMA) {
        getNextToken(); // Consume COMMA
        PtrVariableAST parameter = parseParameter();
        parameters.push_back(std::move(parameter));
      }
    } else if (active_token.type == VOID_TOK) {
      getNextToken(); // Consume void
    }

    return parameters;
  }

  static PtrVariableAST parseParameter() {
    auto parameter_type = active_token.type;
    getNextToken(); // Consume TYPE
    TOKEN t = active_token; 
    auto parameter_ident = lexer_data.identifier_val;
    getNextToken(); // Consume IDENT

    return parseVariable(t, parameter_type, parameter_ident);
  }

  static PtrIfBlockAST parseIfBlock() {
    TOKEN t = active_token;
    getNextToken(); // Consume IF
    if (active_token.type != LPAR) {
      return raiseError("parseIfBlock: Expected '('");
    }
    getNextToken(); // Consume (

    if (!isExpression(active_token.type)) {
      return raiseError("parseIfBlock: Expected EXPR");
    }
    PtrExpressionAST condition = parseExpression();

    if (active_token.type != RPAR) {
      return raiseError("parseIfBlock: Expected ')'");
    }
    getNextToken(); // Consume )

    if (!isStatement(active_token.type)) {
      raiseError("parseIfBlock: Expected STMT");
    }
    PtrCodeBlockAST true_branch = parseCodeBlock();

    if (active_token.type == ELSE) {
      getNextToken(); // Consume ELSE
      if (active_token.type != LBRA) {
        return raiseError("parseElseBlock: Expected '{'");
      }
      PtrCodeBlockAST false_branch = parseCodeBlock();
      return make_unique<IfBlockAST>(t, std::move(condition), std::move(true_branch), std::move(false_branch));
    }

    return make_unique<IfBlockAST>(t, std::move(condition), std::move(true_branch));
  }

  static PtrWhileBlockAST parseWhileBlock() {
    TOKEN t = active_token;
    getNextToken(); // Consume WHILE
    if (active_token.type != LPAR) {
      return raiseError("parseWhileBlock: Expected '('");
    }
    getNextToken(); // Consume (

    if (!isExpression(active_token.type)) {
      return raiseError("parseWhileBlock: Expected EXPR");
    }
    PtrExpressionAST condition = parseExpression();

    if (active_token.type != RPAR) {
      return raiseError("parseWhileBlock: Expected ')'");
    }
    getNextToken(); // Consume )

    if (!isStatement(active_token.type)) {
      return raiseError("parseWhileBlock: Expected STMT");
    }
    PtrStatementAST body = parseStatement();
    return make_unique<WhileBlockAST>(t, std::move(condition), std::move(body));
  }

  static PtrReturnAST parseReturnStatement() {
    TOKEN t = active_token;
    getNextToken(); // consume RETURN
    if (active_token.type == SC) {
      return make_unique<ReturnAST>(t);
    } else if (isExpression(active_token.type)) {
      PtrExpressionAST body = parseExpression();

      if (active_token.type != SC) {
        return raiseError("parseReturnStatement: Expected ';'");
      }
      getNextToken(); // Consume ;
      return make_unique<ReturnAST>(t, std::move(body));
    }
    return raiseError("parseReturnStatement");
  }

  static PtrCodeBlockAST parseCodeBlock() {
    TOKEN t = active_token;
    getNextToken(); // Consume {
    vector<PtrVariableAST> declarations = parseBlockDecls();
    vector<PtrStatementAST> statements = parseStatements();
    if (active_token.type != RBRA) {
      return raiseError("parseCodeBlock: Expected '}'");
    }
    getNextToken(); // Consume }

  return make_unique<CodeBlockAST>(t, std::move(declarations), std::move(statements));
  }

  static vector<PtrVariableAST> parseBlockDecls() {
    vector<PtrVariableAST> declarations;

    while (isVarType(active_token.type)) {
      auto type = active_token.type;
      getNextToken(); // Consume TYPE
      if (active_token.type == IDENT) {
        TOKEN token = active_token;
        auto ident = lexer_data.identifier_val;
        getNextToken(); // Consume IDENT
        if (active_token.type == SC) {
          getNextToken(); // Consume ;
          PtrVariableAST decl = parseVariable(token, type, ident);
          declarations.push_back(std::move(decl));
        }
      }
    }

    if (isStatement(active_token.type) ||
        active_token.type == RBRA) {
      return declarations;
    }

    raiseError("parseBlockDecls");
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

    return raiseError("parseStatement");
  }

  static PtrExpressionStatementAST parseExpressionStmt() {
    if (isExpression(active_token.type)) {
      PtrExpressionAST e = parseExpression();
      if (active_token.type == SC) {
        getNextToken(); // Consume ;
        return make_unique<ExpressionStatementAST>(std::move(e));
      }
    }
    return raiseError("parseExpressionStmt");
  }

  static PtrExpressionAST parseExpression() {
    if (active_token.type == IDENT) {
      TOKEN t = active_token;
      auto ident = lexer_data.identifier_val;
      getNextToken(); // Consume IDENT
      return parseOptAssign(t, ident);
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
    return raiseError("parseExpression");
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

    return raiseError("parseOptAssign");
  }

  static PtrExpressionAST parseExpressionOp(int exprPrec, PtrExpressionAST lhs) {
    while (true) {
      int token_prec = operator_precedence[active_token.type];

      if (token_prec < exprPrec ||
          !isOperator(active_token.type)) {
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
    return raiseError("parseNegation");
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
    return raiseError("parseParens");
  }

  static PtrExpressionAST parseAtomicValue() {
    if (active_token.type == IDENT) {
      TOKEN t = active_token;
      string ident = lexer_data.identifier_val;
      getNextToken(); // Consume IDENT
      return parseIdentifier(t, ident);
    } else if (isLiteral(active_token.type)) {
      TOKEN t = active_token;
      getNextToken(); // Consume VALUE
      switch (t.type) {
        case INT_LIT:
          return make_unique<IntAST>(t, lexer_data.int_val);
        case FLOAT_LIT:
          return make_unique<FloatAST>(t, lexer_data.float_val);
        case BOOL_LIT:
          return make_unique<BoolAST>(t, lexer_data.bool_val);
        default:
          break;
      }
    }
    return raiseError("parseAtomicValue: Expected identifier or literal");
  }

  static PtrExpressionAST parseIdentifier(TOKEN token, const string &ident) {
    if (active_token.type == LPAR) {
      getNextToken(); // Consume (
      auto arguments = parseArguments();
      if (active_token.type != RPAR) {
        return raiseError("parseIdentifier: Expected ')'");
      }
      getNextToken(); // Consume )
      return make_unique<FunctionCallAST>(token, ident, std::move(arguments));
    } else if (isExpressionEnd(active_token.type) || isOperator(active_token.type)) {
      return make_unique<VariableAST>(token, ident, UNKNOWN);
    }
    return raiseError("parseIdentifier: Expected variable or function call");
  }

  static vector<PtrExpressionAST> parseArguments() {
    vector<PtrExpressionAST> arguments;

    if (isExpression(active_token.type)) {
      PtrExpressionAST first_argument = parseExpression();
      arguments.push_back(std::move(first_argument));

      while (active_token.type == COMMA) {
        getNextToken(); // Consume COMMA
        PtrExpressionAST argument = parseExpression();
        arguments.push_back(std::move(argument));
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
        return raiseError("parseVariable: Expected variable.");
    }
    return make_unique<VariableAST>(token, ident, variable_type);
  }

  //
  // Utilities:
  //
  
  static TOKEN getNextToken() {
    lexer_data = minic_lexer::updateData(lexer_data);
    return active_token = lexer_data.token;
  }

  static std::nullptr_t raiseError(const char *msg) {
    fprintf(stderr, "Error: %s [%d:%d]\n", msg, active_token.line_num, active_token.column_num);
    return nullptr;
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
