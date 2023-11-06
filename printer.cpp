#include "printer.h"

#include <stdio.h>
#include <string>

#include "visitor.h"

using std::string;

namespace minic_printer {

  static const string TREE_INDENT = "  ";

  class Printer : public Visitor {
    std::ostream &Out;
    string CurrentPrefix;

    public:
      Printer(std::ostream &out) : Out(out) {}

    void print(const AST &node) {
      // New line, unless first print.
      if (!CurrentPrefix.empty()) Out << "\n";
      // Indent:
      CurrentPrefix = CurrentPrefix + TREE_INDENT;
      // Visit node:
      const_cast<AST &>(node).dispatch(*this);
      // Unindent:
      CurrentPrefix.erase(0, TREE_INDENT.size()); // NOTE: Erases characters from the string START.
    }

    void* visit(IntAST &node) override {
      Out << CurrentPrefix << "Int: " << std::to_string(node.getValue());
      return nullptr;
    }
    void* visit(FloatAST &node) override {
      Out << CurrentPrefix << "Float: " << std::to_string(node.getValue());
      return nullptr;
    }
    void* visit(BoolAST &node) override {
      Out << CurrentPrefix << "Bool: " << std::to_string(node.getValue());
      return nullptr;
    }
    void* visit(VariableAST &node) override {
      Out << CurrentPrefix << "Variable: (";
      switch (node.getType()) {
        case INTEGER:
          Out << "int) ";
          break;
        case FLOAT:
          Out << "float) ";
          break;
        case BOOL:
          Out << "bool) ";
          break;
        case UNKNOWN:
          Out << "ANY) ";
          break;
        default:
          break;
      }
      Out << node.getIdentifier();
      return nullptr;
    }
    void* visit(AssignmentAST &node) override {
      Out << CurrentPrefix << "ASSIGN: " << node.getIdentifier();
      print(*node.getAssignment());
      return nullptr;
    }
    void* visit(FunctionCallAST &node) override {
      Out << CurrentPrefix << "CALL: " << node.getIdentifier();
      for (auto &arg : node.getArguments()) print(*arg);
      return nullptr;
    }
    void* visit(UnaryExpressionAST &node) override {
      Out << CurrentPrefix << node.getOperator().lexeme.c_str();
      print(*node.getExpression());
      return nullptr;
    }
    void* visit(BinaryExpressionAST &node) override {
      Out << CurrentPrefix << node.getOperator().lexeme.c_str();
      print(*node.getLeft());
      print(*node.getRight());
      return nullptr;
    }
    void* visit(CodeBlockAST &node) override {
      Out << CurrentPrefix << "Block:";
      for (auto &decl : node.getDeclarations()) print(*decl);
      for (auto &stmt : node.getStatements()) print(*stmt);
      return nullptr;
    }
    void* visit(IfBlockAST &node) override {
      Out << CurrentPrefix << "IF:";
      print(*node.getCondition());
      print(*node.getTrueBranch());
      if (node.getFalseBranch()) {
        print(*node.getFalseBranch());
      }
      return nullptr;
    }
    void* visit(WhileBlockAST &node) override {
      Out << CurrentPrefix << "WHILE:";
      print(*node.getCondition());
      print(*node.getBody());
      return nullptr;
    }
    void* visit(ReturnAST &node) override {
      Out << CurrentPrefix << "RETURN";
      if (node.getBody()) {
        Out << ":";
        print(*node.getBody());
      }
      return nullptr;
    }
    void* visit(GlobalVariableAST &node) override {
      print(*node.getVariable());
      return nullptr;
    }
    void* visit(PrototypeAST &node) override {
      Out << CurrentPrefix << "Prototype: " << node.getIdentifier();
      for (auto &param : node.getParameters()) print(*param);
      return nullptr;
    }
    void* visit(FunctionAST &node) override {
      Out << CurrentPrefix << "Function:";
      print(*node.getPrototype());
      print(*node.getBody());
      return nullptr;
    }
    void* visit(ProgramAST &node) override {
      Out << CurrentPrefix << "Program:";
      for (auto &ex : node.getExterns()) print(*ex);
      for (auto &f : node.getFunctions()) print(*f);
      return nullptr;
    }

  };

  std::ostream &operator<<(std::ostream &out, const AST &node) {
    Printer(out).print(node); // TODO: Why doesn't this create an object on every invocation?
    return out;
  }

}
