#include "lexer.h"

namespace lexer {

  LEXER_DATA updateData(LEXER_DATA data) {
    static int prev_char = ' ';
    static int next_char = ' ';

    // Skip any whitespace.
    while (isspace(prev_char)) {
      if (prev_char == '\n' || prev_char == '\r') {
        data.line_num++;
        data.column_num = 1;
      }
      prev_char = getc(data.input_file);
      data.column_num++;
    }

    if (isalpha(prev_char) || (prev_char == '_')) { // identifier: [a-zA-Z_][a-zA-Z_0-9]*
      data.identifier_val = prev_char;
      data.column_num++;

      while (isalnum((prev_char = getc(data.input_file))) || (prev_char == '_')) {
        data.identifier_val += prev_char;
        data.column_num++;
      }

      if (data.identifier_val == "int")
        return getData(data, "int", INT_TOK);
      if (data.identifier_val == "bool")
        return getData(data, "bool", BOOL_TOK);
      if (data.identifier_val == "float")
        return getData(data, "float", FLOAT_TOK);
      if (data.identifier_val == "void")
        return getData(data, "void", VOID_TOK);
      if (data.identifier_val == "bool")
        return getData(data, "bool", BOOL_TOK);
      if (data.identifier_val == "extern")
        return getData(data, "extern", EXTERN);
      if (data.identifier_val == "if")
        return getData(data, "if", IF);
      if (data.identifier_val == "else")
        return getData(data, "else", ELSE);
      if (data.identifier_val == "while")
        return getData(data, "while", WHILE);
      if (data.identifier_val == "return")
        return getData(data, "return", RETURN);
      if (data.identifier_val == "true") {
        data.bool_val = true;
        return getData(data, "true", BOOL_LIT);
      }
      if (data.identifier_val == "false") {
        data.bool_val = false;
        return getData(data, "false", BOOL_LIT);
      }

      return getData(data, data.identifier_val.c_str(), IDENT);
    }

    if (prev_char == '=') {
      next_char = getc(data.input_file);
      if (next_char == '=') { // EQ: ==
        prev_char = getc(data.input_file);
        data.column_num += 2;
        return getData(data, "==", EQ);
      } else {
        prev_char = next_char;
        data.column_num++;
        return getData(data, "=", ASSIGN);
      }
    }

    if (prev_char == '{') {
      prev_char = getc(data.input_file);
      data.column_num++;
      return getData(data, "{", LBRA);
    }
    if (prev_char == '}') {
      prev_char = getc(data.input_file);
      data.column_num++;
      return getData(data, "}", RBRA);
    }
    if (prev_char == '(') {
      prev_char = getc(data.input_file);
      data.column_num++;
      return getData(data, "(", LPAR);
    }
    if (prev_char == ')') {
      prev_char = getc(data.input_file);
      data.column_num++;
      return getData(data, ")", RPAR);
    }
    if (prev_char == ';') {
      prev_char = getc(data.input_file);
      data.column_num++;
      return getData(data, ";", SC);
    }
    if (prev_char == ',') {
      prev_char = getc(data.input_file);
      data.column_num++;
      return getData(data, ",", COMMA);
    }

    if (isdigit(prev_char) || prev_char == '.') { // Number: [0-9]+.
      string NumStr;

      if (prev_char == '.') { // Floatingpoint Number: .[0-9]+
        do {
          NumStr += prev_char;
          prev_char = getc(data.input_file);
          data.column_num++;
        } while (isdigit(prev_char));

        data.float_val = strtof(NumStr.c_str(), nullptr);
        return getData(data, NumStr, FLOAT_LIT);
      } else {
        do { // Start of Number: [0-9]+
          NumStr += prev_char;
          prev_char = getc(data.input_file);
          data.column_num++;
        } while (isdigit(prev_char));

        if (prev_char == '.') { // Floatingpoint Number: [0-9]+.[0-9]+)
          do {
            NumStr += prev_char;
            prev_char = getc(data.input_file);
            data.column_num++;
          } while (isdigit(prev_char));

          data.float_val = strtof(NumStr.c_str(), nullptr);
          return getData(data, NumStr, FLOAT_LIT);
        } else { // Integer : [0-9]+
          data.int_val = strtod(NumStr.c_str(), nullptr);
          return getData(data, NumStr, INT_LIT);
        }
      }
    }

    if (prev_char == '&') {
      next_char = getc(data.input_file);
      if (next_char == '&') { // AND: &&
        prev_char = getc(data.input_file);
        data.column_num += 2;
        return getData(data, "&&", AND);
      } else {
        prev_char = next_char;
        data.column_num++;
        return getData(data, "&", int('&'));
      }
    }

    if (prev_char == '|') {
      next_char = getc(data.input_file);
      if (next_char == '|') { // OR: ||
        prev_char = getc(data.input_file);
        data.column_num += 2;
        return getData(data, "||", OR);
      } else {
        prev_char = next_char;
        data.column_num++;
        return getData(data, "|", int('|'));
      }
    }

    if (prev_char == '!') {
      next_char = getc(data.input_file);
      if (next_char == '=') { // NE: !=
        prev_char = getc(data.input_file);
        data.column_num += 2;
        return getData(data, "!=", NE);
      } else {
        prev_char = next_char;
        data.column_num++;
        return getData(data, "!", NOT);
        ;
      }
    }

    if (prev_char == '<') {
      next_char = getc(data.input_file);
      if (next_char == '=') { // LE: <=
        prev_char = getc(data.input_file);
        data.column_num += 2;
        return getData(data, "<=", LE);
      } else {
        prev_char = next_char;
        data.column_num++;
        return getData(data, "<", LT);
      }
    }

    if (prev_char == '>') {
      next_char = getc(data.input_file);
      if (next_char == '=') { // GE: >=
        prev_char = getc(data.input_file);
        data.column_num += 2;
        return getData(data, ">=", GE);
      } else {
        prev_char = next_char;
        data.column_num++;
        return getData(data, ">", GT);
      }
    }

    if (prev_char == '/') { // could be division or could be the start of a comment
      prev_char = getc(data.input_file);
      data.column_num++;
      if (prev_char == '/') { // definitely a comment
        do {
          prev_char = getc(data.input_file);
          data.column_num++;
        } while (prev_char != EOF && prev_char != '\n' && prev_char != '\r');

        if (prev_char != EOF)
          return updateData(data);
      } else
        return getData(data, "/", DIV);
    }

    // Check for end of file.  Don't eat the EOF.
    if (prev_char == EOF) {
      data.column_num++;
      return getData(data, "0", EOF_TOK);
    }

    // Otherwise, just return the character as its ascii value.
    int cur_char = prev_char;
    string s(1, cur_char);
    prev_char = getc(data.input_file);
    data.column_num++;
    return getData(data, s, int(cur_char));
  }

  static LEXER_DATA getData(LEXER_DATA data, const string &lex_val, int token_type) {
    TOKEN return_tok;
    return_tok.lexeme = lex_val;
    return_tok.type = token_type;
    return_tok.line_num = data.line_num;
    return_tok.column_num = data.column_num - lex_val.length() - 1;

    data.token = return_tok;
    return data;
  }

}
