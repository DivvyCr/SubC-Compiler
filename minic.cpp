#include "llvm/Support/FileSystem.h"
#include "llvm/Support/raw_ostream.h"
#include <iostream>
#include <stdio.h>

#include "lexer.h"
#include "parser.h"
#include "abstract-syntax.h"
#include "printer.h"
#include "code-generator.h"

using minic_printer::operator<<;

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
  unique_ptr<ProgramAST> root = minic_parser::startParse(input_file);
  // Print AST:
  std::cout << *root << "\n";
  // Generate code:
  minic_code_generator::generate(*root);

  // Terminate:
  fclose(input_file);
  return 0;
}
