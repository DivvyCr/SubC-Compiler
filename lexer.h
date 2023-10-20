#ifndef _LEXER_H
#define _LEXER_H

#include "abstract-syntax.h"

using namespace abstract_syntax;

namespace lexer {

  LEXER_DATA updateData(LEXER_DATA data);
  static LEXER_DATA getData(LEXER_DATA data, const string &lex_val, int token_type);

}

#endif // _LEXER_H
