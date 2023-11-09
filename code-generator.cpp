#include "code-generator.h"

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
#include "visitor.h"

using std::string;
using std::vector;
using std::unique_ptr;

using namespace std::placeholders;
using namespace llvm;

using SymbolTable = std::map<string, AllocaInst *>;
using FunctionTable = std::map<string, PrototypeAST *>;

namespace minic_code_generator {

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
    std::map<int, Instruction::BinaryOps> iOps;
    std::map<int, CmpInst::Predicate> iCmpOps;
    std::map<int, Instruction::BinaryOps> fOps;
    std::map<int, CmpInst::Predicate> fCmpOps;

    public:
      ExpressionCodeGenerator(LLVMContext *llvmContext, Module *llvmModule, IRBuilder<> *irBuilder, SymbolTable symbols, FunctionTable functions)
        : BaseCodeGenerator(llvmContext, llvmModule, irBuilder, symbols, functions) {
        // LOGIC:
        iOps[AND] = Instruction::BinaryOps::And;
        iOps[OR] = Instruction::BinaryOps::Or;

        // INTEGER:
        iOps[PLUS] = Instruction::BinaryOps::Add;
        iOps[MINUS] = Instruction::BinaryOps::Sub;
        iOps[MULT] = Instruction::BinaryOps::Mul;
        iOps[DIV] = Instruction::BinaryOps::SDiv;
        iOps[MOD] = Instruction::BinaryOps::SRem;
        // iOps[NOT] = ONLY FLOAT NEGATION EXISTS?
        iCmpOps[EQ] = CmpInst::Predicate::ICMP_EQ;
        iCmpOps[NE] = CmpInst::Predicate::ICMP_NE;
        iCmpOps[GT] = CmpInst::Predicate::ICMP_SGT;
        iCmpOps[GE] = CmpInst::Predicate::ICMP_SGE;
        iCmpOps[GE] = CmpInst::Predicate::ICMP_SGE;
        iCmpOps[LT] = CmpInst::Predicate::ICMP_SLT;
        iCmpOps[LE] = CmpInst::Predicate::ICMP_SLE;

        // FLOAT:
        fOps[PLUS] = Instruction::BinaryOps::FAdd;
        fOps[MINUS] = Instruction::BinaryOps::FSub;
        fOps[MULT] = Instruction::BinaryOps::FMul;
        fOps[DIV] = Instruction::BinaryOps::FDiv;
        fOps[MOD] = Instruction::BinaryOps::FRem;
        // fOps[NOT] = &IRBuilder<>::CreateFNeg;
        fCmpOps[EQ] = CmpInst::Predicate::FCMP_OEQ;
        fCmpOps[NE] = CmpInst::Predicate::FCMP_ONE;
        fCmpOps[GT] = CmpInst::Predicate::FCMP_OGT;
        fCmpOps[GE] = CmpInst::Predicate::FCMP_OGE;
        fCmpOps[GE] = CmpInst::Predicate::FCMP_OGE;
        fCmpOps[LT] = CmpInst::Predicate::FCMP_OLT;
        fCmpOps[LE] = CmpInst::Predicate::FCMP_OLE;
      }

      Value* generateCode(const ExpressionAST &node) {
        return reinterpret_cast<Value *>(const_cast<ExpressionAST &>(node).dispatch(*this));
      }

    private:
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
        GlobalVariable *global_variable;
        Type *variable_type;
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
            global_variable = getModule()->getNamedGlobal(node.getIdentifier());
            if (variable_alloca) {
              return getBuilder()->CreateLoad(variable_alloca->getAllocatedType(), variable_alloca, node.getIdentifier().c_str());
            } else if (global_variable) {
              return getBuilder()->CreateLoad(global_variable->getValueType(), global_variable);
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
        GlobalVariable *global_variable = getModule()->getNamedGlobal(node.getIdentifier());

        Value *expression = generateCode(*node.getAssignment());
        if (!expression) return nullptr;
        
        if (assignee) {
          if (assignee->getAllocatedType() != expression->getType()) {
            return raiseError("Mismatched types on assignment.");
          }
          getBuilder()->CreateStore(expression, assignee);
        } else if (global_variable) {
          if (global_variable->getValueType() != expression->getType()) {
            return raiseError("Mismatched types on assignment.");
          }
          getBuilder()->CreateStore(expression, global_variable);
        } else {
          return raiseError("Unknown variable for assignment.");
        }

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
        Value *left = generateCode(*node.getLeft());
        if (!left) return nullptr;
        Type *left_type = left->getType();

        Value *right = generateCode(*node.getRight());
        if (!right) return nullptr;
        Type *right_type = right->getType();

        int op_type = node.getOperator().type;
        if (isLogicOp(op_type)) {
          if (left_type->isIntegerTy(1) && right_type->isIntegerTy(1)) {
            return getBuilder()->CreateBinOp(iOps[op_type], left, right);
          }
          return raiseError("Mismatched expression types (for logic operation)");
        }

        // Determine whether it is an INTEGER or a FLOATING-POINT operation,
        // and convert integers to floating-point if necessary:
        bool is_float = false;

        if (left_type->isFloatTy() || right_type->isFloatTy()) {
          is_float = true;

          if (left_type->isIntegerTy()) {
            left = getBuilder()->CreateSIToFP(left, Type::getFloatTy(*getContext()), "convert");
          }
          if (right_type->isIntegerTy()) {
            right = getBuilder()->CreateSIToFP(right, Type::getFloatTy(*getContext()), "convert");
          }
        }

        if (left->getType() != right->getType()) return raiseError("Mismatched expression types (for arithmetic operation)"); 

        // Generate the appropriate operation instruction:
        // TODO: Create struct to hold is_float and op_type together, then use a map on it?
        if (is_float) {
          if (isMathOp(op_type)) return getBuilder()->CreateBinOp(fOps[op_type], left, right);
          if (isCmpOp(op_type)) return getBuilder()->CreateCmp(fCmpOps[op_type], left, right);
        } else {
          if (isMathOp(op_type)) return getBuilder()->CreateBinOp(iOps[op_type], left, right);
          if (isCmpOp(op_type)) return getBuilder()->CreateCmp(iCmpOps[op_type], left, right);
        }
        return raiseError("Unknown operator");
      }

      bool isMathOp(int operator_type) {
        return (operator_type == PLUS || operator_type == MINUS ||
            operator_type == MULT || operator_type == DIV ||
            operator_type == MOD);
      }
      bool isCmpOp(int operator_type) {
        return (operator_type == EQ || operator_type == NE ||
            operator_type == LE || operator_type == LT ||
            operator_type == GE || operator_type == GT);
      }
      bool isLogicOp(int operator_type) {
        return (operator_type == AND || operator_type == OR || operator_type == NOT);
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
        // TODO: Check that the function returns appropriate TYPE.
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

      void generateGlobal(GlobalVariableAST &node) {
        Type *variable_type;
        switch (node.getVariable()->getType()) {
          case INTEGER:
            variable_type = Type::getInt32Ty(*getContext());
            break;
          case FLOAT:
            variable_type = Type::getFloatTy(*getContext());
            break;
          case BOOL:
            variable_type = Type::getInt1Ty(*getContext());
            break;
          default:
            return;
        }
        getModule()->getOrInsertGlobal(node.getVariable()->getIdentifier(), variable_type);
      }

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
        verifyFunction(*llvm_function);
        return;
      }

      void generateProgram(const ProgramAST &node) {
        for (auto &e : node.getExterns()) generatePrototype(*e);
        for (auto &g : node.getGlobals()) generateGlobal(*g);
        for (auto &f : node.getFunctions()) generateFunction(*f);
        getModule()->print(outs(), nullptr);
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

