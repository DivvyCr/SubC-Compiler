#include "llvm/Support/FileSystem.h"
#include "llvm/Support/raw_ostream.h"
#include <iostream>
#include <stdio.h>

#include "lexer.h"
#include "parser.h"
#include "abstract-syntax.h"
#include "printer.h"
#include "code-generator.h"

using std::cout;

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

  // Run parser, print the AST, and generate the IR:
  PtrProgramAST root = parseProgram(input_file);
  if (root) {
    // cout << *root << "\n";
    generate(*root);
  } else {
    fprintf(stderr, "Parsing error!\n");
  }

  // Terminate:
  fclose(input_file);
  return 0;
}
