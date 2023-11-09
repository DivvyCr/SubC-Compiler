#ifndef _CODEGEN_H
#define _CODEGEN_H

#include "abstract-syntax.h"

namespace minic_code_generator {
  void generate(const ProgramAST &node);
  std::nullptr_t errorExit(const char* msg);
}

#endif // _CODEGEN_H
