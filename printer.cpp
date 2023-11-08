#include "printer.h"

#include <stdio.h>
#include <string>

#include "visitor.h"

namespace minic_printer {

  static const string TREE_INDENT = "  ";

  class Printer : public ExpressionVisitor, public StatementVisitor {
    std::string CurrentPrefix = "";
    std::ostream &Out;

    public:
      Printer(std::ostream &out) : Out(out) {}

      void print(const ProgramAST &node) {
        for (auto &e : node.getExterns()) print(*e);
        for (auto &g : node.getGlobals()) print(*g);
        for (auto &f : node.getFunctions()) print(*f);
      }

      void print(const ExpressionAST &node, bool deepen = true) {
        if (deepen) {
          Out << "\n";
          CurrentPrefix = CurrentPrefix + TREE_INDENT;
        }
        // Visit node:
        const_cast<ExpressionAST &>(node).dispatch(*this);
        // Unindent:
        if (deepen) {
          CurrentPrefix.erase(0, TREE_INDENT.size()); // NOTE: Erases characters from the string START.
        }
      }

      void print(const StatementAST &node, bool deepen = true) {
        if (deepen) {
          Out << "\n";
          CurrentPrefix = CurrentPrefix + TREE_INDENT;
        }
        // Visit node:
        const_cast<StatementAST &>(node).dispatch(*this);
        // Unindent:
        if (deepen) {
          CurrentPrefix.erase(0, TREE_INDENT.size()); // NOTE: Erases characters from the string START.
        }
      }

    private:
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
      void visit(ExpressionStatementAST &node) override {
        print(*node.getExpression(), false);
      }
      void visit(CodeBlockAST &node) override {
        Out << CurrentPrefix << "Block:";
        for (auto &decl : node.getDeclarations()) print(*decl);
        for (auto &stmt : node.getStatements()) print(*stmt);
      }
      void visit(IfBlockAST &node) override {
        Out << CurrentPrefix << "IF:";
        print(*node.getCondition());
        print(*node.getTrueBranch());
        if (node.getFalseBranch()) {
          print(*node.getFalseBranch());
        }
      }
      void visit(WhileBlockAST &node) override {
        Out << CurrentPrefix << "WHILE:";
        print(*node.getCondition());
        print(*node.getBody());
      }
      void visit(ReturnAST &node) override {
        Out << CurrentPrefix << "RETURN";
        if (node.getBody()) {
          Out << ":";
          print(*node.getBody());
        }
      }

      // Non-visitor:
      void print(const GlobalVariableAST &node) {
        Out << "\nGlobal ";
        print(*node.getVariable(), false);
      }
      void print(const PrototypeAST &node) {
        Out << "\n" << CurrentPrefix << "Prototype: " << node.getIdentifier();
        for (auto &param : node.getParameters()) print(*param);
      }
      void print(const FunctionAST &node) {
        Out << "\n" << CurrentPrefix << "Function:";
        CurrentPrefix = CurrentPrefix + TREE_INDENT;
        print(*node.getPrototype());
        CurrentPrefix.erase(0, TREE_INDENT.size()); // NOTE: Erases characters from the string START.
        print(*node.getBody());
      }
  };

  std::ostream &operator<<(std::ostream &out, const ProgramAST &node) {
    Printer(out).print(node);
    return out << "\n";
  }

}
