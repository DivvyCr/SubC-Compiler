#include "llvm/Support/FileSystem.h"
#include "llvm/Support/raw_ostream.h"
#include <iostream>
#include <stdio.h>

#include "lexer.h"
#include "parser.h"
#include "abstract-syntax.h"
#include "printer.h"
#include "code-generator.cpp"

using minic_printer::operator<<;
using minic_code_generator::CodeGenerator;

int main(int argc, char **argv) {
  if (argc != 2) {
    std::cout << "Usage: ./code InputFile\n";
    return 1;
  }

  // Initialise:
  FILE *input_file = fopen(argv[1], "r");
  if (input_file == NULL) {
    perror("Error opening file");
  }

  // Run parser:
  unique_ptr<AST> root = parser::startParse(input_file);
  // Print AST:
  std::cout << *root << "\n";
  // Generate code:
  CodeGenerator().generateCode(*root);

  // Terminate:
  fclose(input_file);
  return 0;
}
