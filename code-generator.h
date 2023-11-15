#ifndef _CODEGEN_H
#define _CODEGEN_H

#include <memory>

#include "llvm/IR/Module.h"
#include "llvm/IR/LLVMContext.h"

#include "abstract-syntax.h"

namespace minic_code_generator {
  std::unique_ptr<llvm::Module> generate(llvm::LLVMContext *llvmContext, const ProgramAST &node);
  std::nullptr_t throwError(TOKEN error_token, string const &error_message);
}

#endif // _CODEGEN_H
