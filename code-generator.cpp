#include <map>
#include <memory>
#include <string>
#include <vector>

#include <functional>
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
#include "code-generator.h"
#include "visitor.h"

using std::string;
using std::vector;
using std::unique_ptr;

using namespace std::placeholders;
using namespace llvm;

using SymbolTable = std::map<string, AllocaInst *>;
using FunctionTable = std::map<string, PrototypeAST *>;

namespace minic_code_generator {

  // typedef std::function<Value* (Value *, Value *, const Twine &)> IOpFunc;
  typedef Value* (IRBuilder<>::*IOpFunc)(Value *, Value *, const Twine &);
  typedef Value* (IRBuilder<>::*FOpFunc)(Value *, Value *, const Twine &, MDNode *);

  class BaseCodeGenerator {
    public:
      BaseCodeGenerator(LLVMContext *llvmContext, Module *llvmModule, IRBuilder<> *irBuilder, SymbolTable symbols, FunctionTable functions)
        : TheContext(llvmContext), TheModule(llvmModule), TheBuilder(irBuilder), Symbols(symbols), Functions(functions) {}
      LLVMContext *getContext() { return TheContext; }
      Module *getModule() { return TheModule; }
      IRBuilder<> *getBuilder() { return TheBuilder; }
    protected:
      SymbolTable Symbols;
      FunctionTable Functions;
    private:
      LLVMContext *TheContext;
      Module *TheModule;
      IRBuilder<> *TheBuilder;
  };

  class ExpressionCodeGenerator : public ExpressionVisitor, public BaseCodeGenerator {
    std::map<int, IOpFunc> iOps;
    std::map<int, FOpFunc> fOps;

    public:
      ExpressionCodeGenerator(LLVMContext *llvmContext, Module *llvmModule, IRBuilder<> *irBuilder, SymbolTable symbols, FunctionTable functions)
        : BaseCodeGenerator(llvmContext, llvmModule, irBuilder, symbols, functions) {
        iOps[PLUS] = &IRBuilder<>::CreateNSWAdd;
        iOps[MINUS] = &IRBuilder<>::CreateNSWSub;
        iOps[MULT] = &IRBuilder<>::CreateNSWMul;
        // iOps[DIV] = &IRBuilder<>::CreateSDiv; // Signed Division
        iOps[MOD] = &IRBuilder<>::CreateSRem; // Signed Remainder
        // iOps[NOT] = ONLY FLOAT NEGATION EXISTS?
        iOps[EQ] = &IRBuilder<>::CreateICmpEQ;
        iOps[NE] = &IRBuilder<>::CreateICmpNE;
        iOps[GT] = &IRBuilder<>::CreateICmpSGT;
        iOps[GE] = &IRBuilder<>::CreateICmpSGE;
        iOps[LT] = &IRBuilder<>::CreateICmpSLT;
        iOps[LE] = &IRBuilder<>::CreateICmpSLE;

        fOps[PLUS] = &IRBuilder<>::CreateFAdd;
        fOps[MINUS] = &IRBuilder<>::CreateFSub;
        fOps[MULT] = &IRBuilder<>::CreateFMul;
        fOps[DIV] = &IRBuilder<>::CreateFDiv;
        fOps[MOD] = &IRBuilder<>::CreateFRem;
        // fOps[NOT] = &IRBuilder<>::CreateFNeg;
        fOps[EQ] = &IRBuilder<>::CreateFCmpOEQ;
        fOps[NE] = &IRBuilder<>::CreateFCmpONE;
        fOps[GT] = &IRBuilder<>::CreateFCmpOGT;
        fOps[GE] = &IRBuilder<>::CreateFCmpOGE;
        fOps[LT] = &IRBuilder<>::CreateFCmpOLT;
        fOps[LE] = &IRBuilder<>::CreateFCmpOLE;
      }

      Value* generateCode(const ExpressionAST &node) {
        return reinterpret_cast<Value *>(const_cast<ExpressionAST &>(node).dispatch(*this));
      }

      void* visit(IntAST &node) {
        return ConstantInt::get(*getContext(), APInt(32, node.getValue(), true));
      }
      void* visit(FloatAST &node) {
        return ConstantFP::get(*getContext(), APFloat(node.getValue()));
      }
      void* visit(BoolAST &node) {
        return ConstantInt::get(*getContext(), APInt(1, node.getValue(), false));
      }
      void* visit(VariableAST &node) {
        AllocaInst *variable_alloca;
        llvm::Type *variable_type;
        string tmp = "Unknown variable: ";
        switch (node.getType()) {
          case INTEGER:
            variable_type = Type::getInt32Ty(*getContext());
            break;
          case FLOAT:
            variable_type = Type::getFloatTy(*getContext());
            break;
          case BOOL:
            variable_type = Type::getInt1Ty(*getContext());
            break;
          case UNKNOWN:
            variable_alloca = Symbols[node.getIdentifier()];
            if (variable_alloca) {
              return getBuilder()->CreateLoad(variable_alloca->getAllocatedType(), variable_alloca, node.getIdentifier().c_str());
            }
            tmp += node.getIdentifier();
            return raiseError(tmp.c_str());
          default:
            // Impossible to reach here due to parser.
            return nullptr;
        }

        Function *llvm_parent_function = getBuilder()->GetInsertBlock()->getParent();
        variable_alloca = createEntryBlockAlloca(llvm_parent_function, variable_type, node.getIdentifier());
        Symbols[node.getIdentifier()] = variable_alloca;
        return variable_alloca;
      }
      void* visit(AssignmentAST &node) {
        AllocaInst *assignee = Symbols[node.getIdentifier()];
        if (!assignee) return raiseError("Unknown variable for assignment.");

        Value *expression = generateCode(*node.getAssignment());
        if (!expression) return nullptr;

        if (assignee->getAllocatedType() != expression->getType()) {
          return raiseError("Mismatched types on assignment.");
        }

        getBuilder()->CreateStore(expression, assignee);
        return expression;
      }
      void* visit(FunctionCallAST &node) {
        Function *llvm_call = getModule()->getFunction(node.getIdentifier());
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
        return getBuilder()->CreateCall(llvm_call, llvm_arguments);
      }
      void* visit(UnaryExpressionAST &node) {
        Value *expression = generateCode(*node.getExpression());
        if (!expression) return nullptr;

        switch (node.getOperator().type) {
          case MINUS:
            return getBuilder()->CreateFNeg(expression);
          default:
            break;
        }
        return raiseError("Bad expression (Unary).");
      }
      void* visit(BinaryExpressionAST &node) {
        // Generate code for LHS and RHS:
        Value *left = generateCode(*node.getLeft());
        if (!left) return nullptr;
        Value *right = generateCode(*node.getRight());
        if (!right) return nullptr;

        // Determine whether it is an INTEGER or a FLOATING-POINT operation,
        // and convert integers to floating-point if necessary:
        bool is_float = false;
        Type *left_type = left->getType();
        Type *right_type = right->getType();

        if (left_type->isFloatTy() || right_type->isFloatTy()) {
          is_float = true;

          if (left_type->isIntegerTy()) {
            left = getBuilder()->CreateSIToFP(left, Type::getFloatTy(*getContext()), "convert");
          }
          if (right_type->isIntegerTy()) {
            right = getBuilder()->CreateSIToFP(right, Type::getFloatTy(*getContext()), "convert");
          }
        }

        if (left->getType() != right->getType()) return raiseError("Mismatched expression types.");

        // Generate the appropriate operation instruction:
        return is_float
          ? (*getBuilder().*(fOps[node.getOperator().type]))(left, right, "", nullptr)
          : (*getBuilder().*(iOps[node.getOperator().type]))(left, right, "");
      }
  };

  class StatementCodeGenerator : public StatementVisitor, public BaseCodeGenerator {
    ExpressionCodeGenerator ExpressionGenerator;

    public:
      StatementCodeGenerator(LLVMContext *llvmContext, Module *llvmModule, IRBuilder<> *irBuilder, SymbolTable symbols, FunctionTable functions)
        : BaseCodeGenerator(llvmContext, llvmModule, irBuilder, symbols, functions),
        ExpressionGenerator(llvmContext, llvmModule, irBuilder, symbols, functions) {}

      void generateCode(const StatementAST &node) {
        const_cast<StatementAST &>(node).dispatch(*this);
      }

      void visit(ExpressionStatementAST &node) {
        ExpressionGenerator.generateCode(*node.getExpression());
      }
      void visit(CodeBlockAST &node) {
        for (int i = 0; i < node.getDeclarations().size(); i++) { // Always VariableAST
          Value *status = ExpressionGenerator.generateCode(*node.getDeclarations()[i]);
          // if (!status) return raiseError("Malformed declaration.");
        }

        for (int i = 0; i < node.getStatements().size(); i++) generateCode(*node.getStatements()[i]);
      }
      void visit(IfBlockAST &node) {
        Function *parentFunction = getBuilder()->GetInsertBlock()->getParent();
        BasicBlock *trueBlock = BasicBlock::Create(*getContext(), "if.true", parentFunction);
        BasicBlock *falseBlock = BasicBlock::Create(*getContext(), "if.false", parentFunction);
        BasicBlock *afterBlock = BasicBlock::Create(*getContext(), "if.after", parentFunction);

        // Generate code for the condition:
        Value *condition = ExpressionGenerator.generateCode(*node.getCondition());
        if (!condition) return;
        getBuilder()->CreateCondBr(condition, trueBlock, falseBlock);

        // Generate code for the True branch:
        getBuilder()->SetInsertPoint(trueBlock);
        /*Value *trueExpression = */generateCode(*node.getTrueBranch());
        // if (!trueExpression) return raiseError("Malformed true branch.");
        getBuilder()->CreateBr(afterBlock);

        // Generate code for the False branch:
        getBuilder()->SetInsertPoint(falseBlock);
        /*Value *falseExpression = */generateCode(*node.getFalseBranch());
        // if (!falseExpression) return raiseError("Malformed false branch.");
        getBuilder()->CreateBr(afterBlock);

        // Place subsequent insertion to after block:
        getBuilder()->SetInsertPoint(afterBlock);
      }
      void visit(WhileBlockAST &node) {
        Function *parentFunction = getBuilder()->GetInsertBlock()->getParent();
        BasicBlock *condBlock = BasicBlock::Create(*getContext(), "while.cond", parentFunction);
        BasicBlock *loopBlock = BasicBlock::Create(*getContext(), "while.body", parentFunction);
        BasicBlock *afterBlock= BasicBlock::Create(*getContext(), "while.after", parentFunction);

        // Unconditionally enter the condition upon first encountering the while loop:
        getBuilder()->CreateBr(condBlock);
 
        // Generate code for the condition:
        getBuilder()->SetInsertPoint(condBlock);
        Value *condition = ExpressionGenerator.generateCode(*node.getCondition());
        if (!condition) return;
        getBuilder()->CreateCondBr(condition, loopBlock, afterBlock);

        // Generate code for the body:
        getBuilder()->SetInsertPoint(loopBlock);
        /*Value *bodyValue = */generateCode(*node.getBody());
        //if (!bodyValue) return raiseError("Malformed while body.");
        getBuilder()->CreateBr(condBlock);
        loopBlock = getBuilder()->GetInsertBlock();

        // Place subsequent insertion to after block:
        getBuilder()->SetInsertPoint(afterBlock);
      }
      void visit(ReturnAST &node) {
        Value *expression = ExpressionGenerator.generateCode(*node.getBody());
        if (!expression) return;
        getBuilder()->CreateRet(expression);
      }
  };

  class ProgramCodeGenerator : public BaseCodeGenerator {
    StatementCodeGenerator StatementGenerator;

    public:
      ProgramCodeGenerator(LLVMContext *llvmContext, Module *llvmModule, IRBuilder<> *irBuilder, SymbolTable symbols, FunctionTable functions)
        : BaseCodeGenerator(llvmContext, llvmModule, irBuilder, symbols, functions),
        StatementGenerator(llvmContext, llvmModule, irBuilder, symbols, functions) {}

      void generatePrototype(PrototypeAST &node) {
        vector<Type *> parameter_types;
        for (int i = 0; i < node.getParameters().size(); i++) {
          MiniCType parameter_minictype = node.getParameters()[i]->getType();
          Type *parameter_type;
          switch (parameter_minictype) {
            case INTEGER:
              parameter_type = Type::getInt32Ty(*getContext());
              break;
            case FLOAT:
              parameter_type = Type::getFloatTy(*getContext());
              break;
            case BOOL:
              parameter_type = Type::getInt1Ty(*getContext());
              break;
            default:
              // Impossible to reach here due to parser.
              return;
          }
          parameter_types.push_back(parameter_type);    
        }

        Type *return_type;
        switch (node.getReturnType()) {
          case INTEGER:
            return_type = Type::getInt32Ty(*getContext());
            break;
          case FLOAT:
            return_type = Type::getFloatTy(*getContext());
            break;
          case BOOL:
            return_type = Type::getInt1Ty(*getContext());
            break;
          case VOID:
            return_type = Type::getVoidTy(*getContext());
            break;
          default:
            // Impossible to reach here due to parser.
            return;
        }

        FunctionType *function_type = FunctionType::get(return_type, parameter_types, false);

        Function *function = Function::Create(function_type, Function::ExternalLinkage, node.getIdentifier(), getModule());
        // Necessary to name arguments, in order to make it easier to use them inside function body.
        int i = 0; 
        for (auto &arg : function->args()) {
          arg.setName(node.getParameters()[i++]->getIdentifier());
        }

        Functions[node.getIdentifier()] = &node;
        return;
      }

      void generateFunction(FunctionAST &node) {
        auto &prototype = *node.getPrototype();
        string function_name = node.getPrototype()->getIdentifier();
        Functions[function_name] = &*node.getPrototype();
        Function *llvm_function = getPrototypeFor(function_name);

        if (!llvm_function) return;

        BasicBlock *llvm_block = BasicBlock::Create(*getContext(), "entry", llvm_function);
        getBuilder()->SetInsertPoint(llvm_block);

        Symbols.clear();
        for (auto &llvm_argument : llvm_function->args()) {
          AllocaInst *argument_alloca = createEntryBlockAlloca(llvm_function, llvm_argument.getType(), llvm_argument.getName());
          getBuilder()->CreateStore(&llvm_argument, argument_alloca);
          Symbols[string(llvm_argument.getName())] = argument_alloca;
        }

        StatementGenerator.generateCode(*node.getBody());
        llvm::verifyFunction(*llvm_function);
        return;
      }

      void generateProgram(const ProgramAST &node) {
        for (auto &e : node.getExterns()) generatePrototype(*e);
        // for (auto &g : node.getGlobals()) generateGlobal(*g);
        for (auto &f : node.getFunctions()) generateFunction(*f);
        getModule()->print(llvm::outs(), nullptr);
      }

    private:
      Function *getPrototypeFor(const string &identifier) {
        Function *function = getModule()->getFunction(identifier);
        if (function) { return function; }

        auto prototype_iterator = Functions.find(identifier);
        if (prototype_iterator != Functions.end()) {
          // Cannot return directly from generateCode(..):
          generatePrototype(*prototype_iterator->second);
          return getModule()->getFunction(identifier);
        }

        return raiseError("Function not known.");
      }
  };

  void generate(const ProgramAST &node) {
    LLVMContext MiniCContext;
    unique_ptr<Module> MiniCModule = std::make_unique<Module>("MiniC", *&MiniCContext);
    IRBuilder<> MiniCBuilder(MiniCContext);

    SymbolTable ss;
    FunctionTable fs;

    ProgramCodeGenerator(&MiniCContext, MiniCModule.get(), &MiniCBuilder, ss, fs).generateProgram(node);
  }

  AllocaInst *createEntryBlockAlloca(Function *function, Type *variable_type, StringRef variable_name) {
    IRBuilder<> tmp_builder(&function->getEntryBlock(), function->getEntryBlock().begin());
    AllocaInst *variable_alloca = tmp_builder.CreateAlloca(variable_type, nullptr, variable_name);
    return variable_alloca;
  }

  std::nullptr_t raiseError(const char* msg) {
    fprintf(stderr, "Error: %s\n", msg);
    return nullptr;
  }

}

