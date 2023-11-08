#ifndef _CODEGEN_H
#define _CODEGEN_H

#include <memory>

#include "llvm/IR/Function.h"
#include "llvm/IR/Instructions.h"

using namespace llvm;

namespace minic_code_generator {
  void generate(const ProgramAST &node);

  AllocaInst *createEntryBlockAlloca(Function *function, Type *variable_type, StringRef variable_name);
  std::nullptr_t raiseError(const char* msg);
}

#endif
