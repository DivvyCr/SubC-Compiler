#ifndef _PRINTER_H
#define _PRINTER_H

#include <iostream> 

#include "abstract-syntax.h"

namespace minic_printer {
  std::ostream &operator<<(std::ostream &out, const GlobalVariableAST &node);
  std::ostream &operator<<(std::ostream &out, const PrototypeAST &node);
  std::ostream &operator<<(std::ostream &out, const FunctionAST &node);
  std::ostream &operator<<(std::ostream &out, const ProgramAST &node);
}

#endif // _PRINTER_H
