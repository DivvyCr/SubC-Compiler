#include <map>
#include <memory>
#include <string>
#include <vector>

#include <iostream>
#include <stdio.h>

#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/APInt.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Value.h"
#include "llvm/IR/Verifier.h"

#include "abstract-syntax.h"
#include "visitor.h"

using std::string;
using std::vector;
using std::unique_ptr;

using namespace llvm;
using namespace abstract_syntax;

namespace minic_code_generator {

  class CodeGenerator : public Visitor {
    unique_ptr<LLVMContext> MiniCContext;
    unique_ptr<Module> MiniCModule;
    unique_ptr<IRBuilder<>> MiniCBuilder;

    std::map<string, AllocaInst *> named_values;
    std::map<string, unique_ptr<PrototypeAST>> known_functions;

    public:
      CodeGenerator() {
        MiniCContext = std::make_unique<LLVMContext>();
        MiniCModule = std::make_unique<Module>("MiniC", *MiniCContext);
        MiniCBuilder = std::make_unique<IRBuilder<>>(*MiniCContext);
      }

      Value *generateCode(const AST &node) {
        return reinterpret_cast<Value *>(const_cast<AST &>(node).dispatch(*this));
      }

      void* visit(IntAST &node) {
        return ConstantInt::get(*MiniCContext, APInt(32, node.getValue(), true));
      }
      void* visit(FloatAST &node) {
        return ConstantFP::get(*MiniCContext, APFloat(node.getValue()));
      }
      void* visit(BoolAST &node) {
        return ConstantInt::get(*MiniCContext, APInt(1, node.getValue(), false));
      }
      void* visit(VariableAST &node) {
        AllocaInst *variable_alloca;
        GlobalVariable *global_variable;
        llvm::Type *variable_type;
        string tmp = "Unknown variable: ";
        switch (node.getType()) {
          case INTEGER:
            variable_type = Type::getInt32Ty(*MiniCContext);
            break;
          case FLOAT:
            variable_type = Type::getFloatTy(*MiniCContext);
            break;
          case BOOL:
            variable_type = Type::getInt1Ty(*MiniCContext);
            break;
          case UNKNOWN:
            variable_alloca = named_values[node.getIdentifier()];
            global_variable = MiniCModule->getNamedGlobal(node.getIdentifier());
            if (variable_alloca) {
              return MiniCBuilder->CreateLoad(variable_alloca->getAllocatedType(), variable_alloca, node.getIdentifier().c_str());
            } else if (global_variable) {
              return MiniCBuilder->CreateLoad(global_variable->getType(), global_variable);
            }
            tmp += node.getIdentifier();
            return raiseError(tmp.c_str());
          default:
            // Impossible to reach here due to parser.
            return nullptr;
        }

        Function *llvm_parent_function = MiniCBuilder->GetInsertBlock()->getParent();
        variable_alloca = createEntryBlockAlloca(llvm_parent_function, variable_type, node.getIdentifier());
        named_values[node.getIdentifier()] = variable_alloca;
        return variable_alloca;
      }
      void* visit(AssignmentAST &node) {
        AllocaInst *assignee = named_values[node.getIdentifier()];
        GlobalVariable *global_variable = MiniCModule->getNamedGlobal(node.getIdentifier());

        Value *expression = generateCode(*node.getAssignment());
        if (!expression) return nullptr;

        if (assignee) {
          if (assignee->getAllocatedType() != expression->getType()) {
            return raiseError("Mismatched types on assignment.");
          }
          MiniCBuilder->CreateStore(expression, assignee);
        } else if (global_variable) {
          if (global_variable->getValueType() != expression->getType()) {
            return raiseError("Mismatched types on assignment.");
          }
          MiniCBuilder->CreateStore(expression, global_variable);
        } else {
          return raiseError("Unknown variable for assignment.");
        }

        return expression;
      }
      void* visit(FunctionCallAST &node) {
        Function *llvm_call = getPrototypeFor(node.getIdentifier());
        if (!llvm_call) return raiseError("Unknown function called.");

        if (node.getArguments().size() != llvm_call->arg_size()) {
          return raiseError("Bad function call (Arguments).");
        }

        vector<Value *> llvm_arguments;
        for (int i = 0; i < node.getArguments().size(); i++) {
          Value *arg = generateCode(*node.getArguments()[i]);
          if (!arg) return nullptr;
          llvm_arguments.push_back(arg);
        }
        return MiniCBuilder->CreateCall(llvm_call, llvm_arguments);
      }
      void* visit(UnaryExpressionAST &node) {
        Value *expression = generateCode(*node.getExpression());
        if (!expression) return nullptr;

        switch (node.getOperator().type) {
          case MINUS:
            return MiniCBuilder->CreateFNeg(expression);
          default:
            break;
        }
        return raiseError("Bad expression (Unary).");
      }
      void* visit(BinaryExpressionAST &node) {
        Value *left_expression = generateCode(*node.getLeft());
        if (!left_expression) return nullptr;
        Value *right_expression = generateCode(*node.getRight());
        if (!right_expression) return nullptr;

        bool is_float_op = false;
        Type *left_type = left_expression->getType();
        Type *right_type = right_expression->getType();

        if (left_type->isFloatTy() && right_type->isFloatTy()) {
          is_float_op = true;
        } else if (left_type->isFloatTy() && right_type->isIntegerTy()) {
          right_expression = MiniCBuilder->CreateSIToFP(right_expression, Type::getFloatTy(*MiniCContext), "conv");
          is_float_op = true;
        } else if (left_type->isIntegerTy() && right_type->isFloatTy()) {
          left_expression = MiniCBuilder->CreateSIToFP(left_expression, Type::getFloatTy(*MiniCContext), "conv");
          is_float_op = true;
        } else if (left_expression->getType() != right_expression->getType()) {
          return raiseError("Mismatched expression types.");
        }

        // NOTE: These operations take the FIRST ARGUMENT (ie. left-hand side)
        // to determine the TYPE of the whole expression.
        switch (node.getOperator().type) {
          case PLUS:
            return is_float_op ? MiniCBuilder->CreateFAdd(left_expression, right_expression)
              : MiniCBuilder->CreateAdd(left_expression, right_expression);
          case MULT:
            return MiniCBuilder->CreateFMul(left_expression, right_expression);
          default:
            break;
        }
        return raiseError("Bad expression.");
      }
      void* visit(CodeBlockAST &node) {
        for (int i = 0; i < node.getDeclarations().size(); i++) { // Always VariableAST
          Value *declaration = generateCode(*node.getDeclarations()[i]);
          if (!declaration) return nullptr;
        }
        for (int i = 0; i < node.getStatements().size(); i++) {
          Value *status = generateCode(*node.getStatements()[i]);
          if (!status) return nullptr;
        }

        // Return non-nullptr Value to indicate success:
        return ConstantInt::get(*MiniCContext, APInt(1, 1, false));
      }
      void* visit(IfBlockAST &node) {return nullptr;}
      void* visit(WhileBlockAST &node) {return nullptr;}
      void* visit(ReturnAST &node) {
        Value *expression = generateCode(*node.getBody());
        if (!expression) return raiseError("Bad return.");
        MiniCBuilder->CreateRet(expression);

        // Return non-nullptr Value to indicate success:
        return ConstantInt::get(*MiniCContext, APInt(1, 1, false));
      }
      void* visit(GlobalVariableAST &node) {
        unique_ptr<VariableAST> v = std::move(node.getVariable());
        Type *variable_type;
        switch (v->getType()) {
          case INTEGER:
            variable_type = Type::getInt32Ty(*MiniCContext);
            break;
          case FLOAT:
            variable_type = Type::getFloatTy(*MiniCContext);
            break;
          case BOOL:
            variable_type = Type::getInt1Ty(*MiniCContext);
            break;
          default:
            return raiseError("GlobalVariable without type");
        }
        return MiniCModule->getOrInsertGlobal(v->getIdentifier(), variable_type);
      }
      void* visit(PrototypeAST &node) {
        vector<Type *> parameter_types;
        for (int i = 0; i < node.getParameters().size(); i++) {
          MiniCType parameter_minictype = node.getParameters()[i]->getType();
          Type *parameter_type;
          switch (parameter_minictype) {
            case INTEGER:
              parameter_type = Type::getInt32Ty(*MiniCContext);
              break;
            case FLOAT:
              parameter_type = Type::getFloatTy(*MiniCContext);
              break;
            case BOOL:
              parameter_type = Type::getInt1Ty(*MiniCContext);
              break;
            default:
              // Impossible to reach here due to parser.
              return nullptr;
          }
          parameter_types.push_back(parameter_type);    
        }

        Type *return_type;
        switch (node.getReturnType()) {
          case INTEGER:
            return_type = Type::getInt32Ty(*MiniCContext);
            break;
          case FLOAT:
            return_type = Type::getFloatTy(*MiniCContext);
            break;
          case BOOL:
            return_type = Type::getInt1Ty(*MiniCContext);
            break;
          case VOID:
            return_type = Type::getVoidTy(*MiniCContext);
            break;
          default:
            // Impossible to reach here due to parser.
            return nullptr;
        }

        FunctionType *function_type = FunctionType::get(return_type, parameter_types, false);

        Function *function = Function::Create(function_type, Function::ExternalLinkage, node.getIdentifier(), MiniCModule.get());
        // Necessary to name arguments, in order to make it easier to use them inside function body.
        int i = 0; 
        for (auto &arg : function->args()) {
          arg.setName(node.getParameters()[i++]->getIdentifier());
        }

        return function;
      }
      void* visit(FunctionAST &node) {
        auto &prototype = *node.getPrototype();
        string function_name = node.getPrototype()->getIdentifier();
        known_functions[function_name] = std::move(node.getPrototype());
        Function *llvm_function = getPrototypeFor(function_name);

        if (!llvm_function) return raiseError("Function code generator error.");

        BasicBlock *llvm_block = BasicBlock::Create(*MiniCContext, "entry", llvm_function);
        MiniCBuilder->SetInsertPoint(llvm_block);

        named_values.clear();
        for (auto &llvm_argument : llvm_function->args()) {
          AllocaInst *argument_alloca = createEntryBlockAlloca(llvm_function, llvm_argument.getType(), llvm_argument.getName());
          MiniCBuilder->CreateStore(&llvm_argument, argument_alloca);
          named_values[string(llvm_argument.getName())] = argument_alloca;
        }

        Value *status = generateCode(*node.getBody());
        if (status) {
          llvm::verifyFunction(*llvm_function);
          return llvm_function;
        }

        return raiseError("Function code generator error (body).");
      }
      void* visit(ProgramAST &node) {
        for (int i = 0; i < node.getExterns().size(); i++) {
          generateCode(*node.getExterns()[i]);
          known_functions[node.getExterns()[i]->getIdentifier()] = std::move(node.getExterns()[i]);
        }
        for (int i = 0; i < node.getGlobals().size(); i++) {
          generateCode(*node.getGlobals()[i]);
        }
        for (int i = 0; i < node.getFunctions().size(); i++) {
          generateCode(*node.getFunctions()[i]);
        }
        MiniCModule->print(llvm::outs(), nullptr);
        return ConstantInt::get(*MiniCContext, APInt());
      }

    private:
      std::nullptr_t raiseError(const char* msg) {
        fprintf(stderr, "Error: %s\n", msg);
        return nullptr;
      }

      AllocaInst *createEntryBlockAlloca(Function *function,
          Type *variable_type, StringRef variable_name) {
        IRBuilder<> tmp_builder(&function->getEntryBlock(), function->getEntryBlock().begin());
        AllocaInst *variable_alloca = tmp_builder.CreateAlloca(variable_type, nullptr, variable_name);
        return variable_alloca;
      }

      Function *getPrototypeFor(const string &identifier) {
        Function *function = MiniCModule->getFunction(identifier);
        if (function) { return function; }

        auto prototype_iterator = known_functions.find(identifier);
        if (prototype_iterator != known_functions.end()) {
          // Cannot return directly from generateCode(..):
          generateCode(*prototype_iterator->second);
          return MiniCModule->getFunction(identifier);
        }

        return raiseError("Function not known.");
      }
  };

}
