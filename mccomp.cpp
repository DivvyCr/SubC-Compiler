#include "llvm/Support/FileSystem.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/Support/raw_ostream.h"
#include <iostream>
#include <stdio.h>

#include "lexer.h"
#include "parser.h"
#include "abstract-syntax.h"
#include "printer.h"
#include "code-generator.h"

using minic_parser::parseProgram;
using minic_printer::operator<<;
using minic_code_generator::generate;

int main(int argc, char **argv) {
  if (argc != 2) {
    std::cout << "Usage: ./code InputFile\n";
    return 1;
  }

  // Initialise:
  FILE *input_file = fopen(argv[1], "r");
  if (input_file == NULL) perror("File error");

  // Run parser, then print the AST:
  PtrProgramAST root = parseProgram(input_file);
  if (!root) {
    fprintf(stderr, "Parsing error!\n");
    exit(1);
  }
  std::cout << *root << "\n";

  // Generate the IR:
  llvm::LLVMContext llvm_context;
  std::unique_ptr<llvm::Module> out_module(generate(&llvm_context, *root));

  // Output the IR to output.ll:
  string output_file = "output.ll";
  std::error_code output_error_code;
  llvm::raw_fd_ostream output_ll(output_file, output_error_code, llvm::sys::fs::OF_None);
  if (output_error_code) {
    llvm::errs() << "Could not open file: " << output_error_code.message();
    exit(1);
  }
  out_module->print(output_ll, nullptr);

  // Terminate:
  fclose(input_file);
  return 0;
}