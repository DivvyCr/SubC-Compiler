#ifndef _LEXER_H
#define _LEXER_H

#include <map>
#include <string>
#include <tuple>
#include <vector>

using std::string;

namespace minic_lexer {

  enum TOKEN_TYPE {
    // Special 
    EOF_TOK = 0,
    INVALID = -100,
    IDENT = -1,        // [a-zA-Z_][a-zA-Z_0-9]*
    ASSIGN = int('='),
    // Delimiters
    LBRA = int('{'),
    RBRA = int('}'),
    LPAR = int('('),
    RPAR = int(')'),
    SC = int(';'),
    COMMA = int(','),
    // Types
    INT_TOK = -2,
    VOID_TOK = -3,
    FLOAT_TOK = -4,
    BOOL_TOK = -5,
    // Keywords
    EXTERN = -6,
    IF = -7,
    ELSE = -8,
    WHILE = -9,
    RETURN = -10,
    // Literals
    INT_LIT = -14,   // [0-9]+
    FLOAT_LIT = -15, // [0-9]+.[0-9]+
    BOOL_LIT = -16,  // "true" or "false" key words
    // Operators
    AND = -17, // &&
    OR = -18,  // ||
    PLUS = int('+'),    // addition or unary plus
    MINUS = int('-'),   // substraction or unary negative
    MULT = int('*'),    // multiplication
    DIV = int('/'),     // division
    MOD = int('%'),     // modular
    NOT = int('!'),     // unary negation
    EQ = -19,      // ==
    NE = -20,      // !=
    LE = -21,      // <=
    LT = int('<'), // <
    GE = -23,      // >=
    GT = int('>')  // >
  };

  static std::map<string, TOKEN_TYPE> keyword_map = {
	{"int", INT_TOK},
	{"float", FLOAT_TOK},
	{"bool", BOOL_TOK},
	{"void", VOID_TOK},
	{"extern", EXTERN},
	{"if", IF},
	{"else", ELSE},
	{"while", WHILE},
	{"return", RETURN},
	{"true", BOOL_LIT},
	{"false", BOOL_LIT},
  };

  static std::map<char, TOKEN_TYPE> special_char_map = {
	{'{', LBRA},
	{'}', RBRA},
	{'(', LPAR},
	{')', RPAR},
	{';', SC},
	{',', COMMA},
  };

  static std::vector<std::tuple<string, int, int>> multi_char_array = {
	std::make_tuple("==", ASSIGN, EQ),
	std::make_tuple("&&", int('&'), AND),
	std::make_tuple("||", int('|'), OR ),
	std::make_tuple("!=", NOT, NE),
	std::make_tuple("<=", LT,  LE),
	std::make_tuple(">=", GT,  GE),
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

  LEXER_DATA updateData(LEXER_DATA data);
  static LEXER_DATA getData(LEXER_DATA data, const string &lex_val, int token_type);

}

#endif // _LEXER_H
