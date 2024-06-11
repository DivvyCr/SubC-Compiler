#include "lexer.h"

#include <optional>

namespace minic_lexer {

  LEXER_DATA updateData(LEXER_DATA data) {
    static int prev_char = ' ';
    static int next_char = ' ';

    auto fmtRetTok = [&](const string &lex_val, int token_type) {
      TOKEN return_tok;
      return_tok.lexeme = lex_val;
      return_tok.type = token_type;
      return_tok.line_num = data.line_num;
      return_tok.column_num = data.column_num - lex_val.length() - 1;

      data.token = return_tok;
      return data;
    };

    //
    // Whitespace:
    //

    while (isspace(prev_char)) {
      if (prev_char == '\n' || prev_char == '\r') {
        data.line_num++;
        data.column_num = 1;
      }
      prev_char = getc(data.input_file);
      data.column_num++;
    }

    //
    // Identifiers:
    //

    if (isalpha(prev_char) || (prev_char == '_')) {
      data.identifier_val = prev_char;
      data.column_num++;

      while (isalnum((prev_char = getc(data.input_file))) || (prev_char == '_')) {
        data.identifier_val += prev_char;
        data.column_num++;
      }

      auto keyword_iter = keyword_map.find(data.identifier_val);
      if (keyword_iter != keyword_map.end()) {
        if (keyword_iter->first == "true")  { data.bool_val = true;  }
        if (keyword_iter->first == "false") { data.bool_val = false; }

        // Keyword found, return its token:
        return fmtRetTok(keyword_iter->first, keyword_iter->second);
      }

      // No keyword found, return identifier:
      return fmtRetTok(data.identifier_val.c_str(), IDENT);
    }

    //
    // Special Chars:
    //

    auto char_iter = special_char_map.find(prev_char);
    if (char_iter != special_char_map.end()) {
      prev_char = getc(data.input_file);
      data.column_num++;
      return fmtRetTok({char_iter->first}, char_iter->second); // NOTE: Curly braces to convert char to string
    }

    //
    // Numbers:
    //

    if (isdigit(prev_char) || prev_char == '.') {
      string num_str;

      auto consumeDigit = [&]() {
        num_str += prev_char;
        prev_char = getc(data.input_file);
        data.column_num++;
      };

      // Consume integer digits:
      while (isdigit(prev_char)) consumeDigit();

      if (prev_char == '.') {
        // Consume decimal point, followed by decimal digits:
        consumeDigit();
        while (isdigit(prev_char)) consumeDigit();

        data.float_val = strtof(num_str.c_str(), nullptr);
        return fmtRetTok(num_str, FLOAT_LIT);
      } else {
        data.int_val = strtod(num_str.c_str(), nullptr);
        return fmtRetTok(num_str, INT_LIT);
      }
    }

    //
    // Special Multi-Chars:
    //

    auto lexMultiChar = [&](const string &to_match, int matched_one, int matched_two) {
      if (prev_char == to_match[0]) {
        next_char = getc(data.input_file);
        if (next_char == to_match[1]) {
          prev_char = getc(data.input_file);
          data.column_num += 2;
          return std::optional<LEXER_DATA>{fmtRetTok(to_match, matched_two)};
        } else {
          prev_char = next_char;
          data.column_num++;
          return std::optional<LEXER_DATA>{fmtRetTok(to_match.substr(0, 1), matched_one)};
        }
      }
      return std::optional<LEXER_DATA>{};
    };

    for (auto &args : multi_char_array) {
      if (auto ret = std::apply(lexMultiChar, args)) return *ret;
    }

    // Line-Comment or Division, where comment contents must be skipped:
    if (prev_char == '/') {
      prev_char = getc(data.input_file);
      data.column_num++;
      if (prev_char == '/') {
        do {
          prev_char = getc(data.input_file);
          data.column_num++;
        } while (prev_char != EOF && prev_char != '\n' && prev_char != '\r');

        if (prev_char != EOF)
          return updateData(data);
      } else
        return fmtRetTok("/", DIV);
    }

    //
    // Otherwise:
    //

    // EOF:
    if (prev_char == EOF) {
      data.column_num++;
      return fmtRetTok("0", EOF_TOK);
    }

    // Plain Char:
    int cur_char = prev_char;
    string s(1, cur_char);
    prev_char = getc(data.input_file);
    data.column_num++;
    return fmtRetTok(s, int(cur_char));
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
