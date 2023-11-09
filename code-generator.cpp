#include "code-generator.h"

#include <map>
#include <memory>
#include <string>
#include <utility>
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
      BaseCodeGenerator(LLVMContext *llvmContext, Module *llvmModule, IRBuilder<> *irBuilder, SymbolTable *symbols, FunctionTable *functions)
        : TheContext(llvmContext), TheModule(llvmModule), TheBuilder(irBuilder), LocalVariableTable(symbols), Functions(functions) {}

      LLVMContext *getContext() { return TheContext; }
      Module *getModule() { return TheModule; }
      IRBuilder<> *getBuilder() { return TheBuilder; }

      Type *getIntType() { return Type::getInt32Ty(*getContext()); }
      Type *getFloatType() { return Type::getFloatTy(*getContext()); }
      Type *getBoolType() { return Type::getInt1Ty(*getContext()); }
      Type *getVoidType() { return Type::getVoidTy(*getContext()); }

      Type *convertNonVoidType(MiniCType minic_type) {
        switch (minic_type) {
          case INTEGER: return getIntType();
          case FLOAT: return getFloatType();
          case BOOL: return getBoolType();
          default: break;
        }
        return nullptr;
      }
    protected:
      SymbolTable *LocalVariableTable;
      FunctionTable *Functions;

      AllocaInst *createEntryBlockAlloca(Function *function, Type *variable_type, StringRef variable_name) {
        IRBuilder<> tmp_builder(&function->getEntryBlock(), function->getEntryBlock().begin());
        AllocaInst *variable_alloca = tmp_builder.CreateAlloca(variable_type, nullptr, variable_name);
        return variable_alloca;
      }
    private:
      LLVMContext *TheContext;
      Module *TheModule;
      IRBuilder<> *TheBuilder;
  };

  class ExpressionCodeGenerator : public ExpressionVisitor, public BaseCodeGenerator {
    std::map<std::pair<int, MiniCType>, Instruction::BinaryOps> arithmetic_instructions = {
      {{AND, INTEGER}, Instruction::BinaryOps::And},
      {{OR, INTEGER}, Instruction::BinaryOps::Or},
      {{PLUS, INTEGER}, Instruction::BinaryOps::Add},
      {{MINUS, INTEGER}, Instruction::BinaryOps::Sub},
      {{MULT, INTEGER}, Instruction::BinaryOps::Mul},
      {{DIV, INTEGER}, Instruction::BinaryOps::SDiv},
      {{MOD, INTEGER}, Instruction::BinaryOps::SRem},
      {{AND, FLOAT}, Instruction::BinaryOps::And},
      {{OR, FLOAT}, Instruction::BinaryOps::Or},
      {{PLUS, FLOAT}, Instruction::BinaryOps::Add},
      {{MINUS, FLOAT}, Instruction::BinaryOps::Sub},
      {{MULT, FLOAT}, Instruction::BinaryOps::Mul},
      {{DIV, FLOAT}, Instruction::BinaryOps::SDiv},
      {{MOD, FLOAT}, Instruction::BinaryOps::SRem}
    };

    std::map<std::pair<int, MiniCType>, CmpInst::Predicate> comparison_instructions = {
      {{EQ, INTEGER}, CmpInst::Predicate::ICMP_EQ},
      {{NE, INTEGER}, CmpInst::Predicate::ICMP_NE},
      {{GT, INTEGER}, CmpInst::Predicate::ICMP_SGT},
      {{GE, INTEGER}, CmpInst::Predicate::ICMP_SGE},
      {{LT, INTEGER}, CmpInst::Predicate::ICMP_SLT},
      {{LE, INTEGER}, CmpInst::Predicate::ICMP_SLE},
      {{EQ, FLOAT}, CmpInst::Predicate::FCMP_OEQ},
      {{NE, FLOAT}, CmpInst::Predicate::FCMP_ONE},
      {{GT, FLOAT}, CmpInst::Predicate::FCMP_OGT},
      {{GE, FLOAT}, CmpInst::Predicate::FCMP_OGE},
      {{LT, FLOAT}, CmpInst::Predicate::FCMP_OLT},
      {{LE, FLOAT}, CmpInst::Predicate::FCMP_OLE}
    };

    public:
      ExpressionCodeGenerator(LLVMContext *llvmContext, Module *llvmModule, IRBuilder<> *irBuilder, SymbolTable *symbols, FunctionTable *functions)
        : BaseCodeGenerator(llvmContext, llvmModule, irBuilder, symbols, functions) {}

      Value* generateCode(const ExpressionAST &node) {
        return reinterpret_cast<Value *>(const_cast<ExpressionAST &>(node).dispatch(*this));
      }

    private:
      void* visit(IntAST &node) {
        return ConstantInt::get(getIntType(), node.getValue(), /*isSigned:*/ true);
      }
      void* visit(FloatAST &node) {
        return ConstantFP::get(getFloatType(), node.getValue());
      }
      void* visit(BoolAST &node) {
        return ConstantInt::get(getBoolType(), node.getValue(), /*isSigned:*/ false);
      }
      void* visit(VariableAST &node) {
        string variable_name = node.getIdentifier();
        Type *variable_type = convertNonVoidType(node.getType());
        AllocaInst *variable_alloca = (*LocalVariableTable)[variable_name];

        if (variable_alloca && variable_type) {
          return raiseError(("Redeclaration of local variable: " + variable_name).c_str());
        }

        if (variable_alloca && !variable_type) {
          // Use of declared variable, so load it:
          // NOTE: This effectively shadows global variables, by virtue of being checked earlier
          return getBuilder()->CreateLoad(variable_alloca->getAllocatedType(), variable_alloca, variable_name);
        }

        if (!variable_alloca && variable_type) {
          // Declaration of a new variable:
          // NOTE: This will shadow global variables, see above
          Function *parent_function = getBuilder()->GetInsertBlock()->getParent();
          AllocaInst *new_variable_alloca = createEntryBlockAlloca(parent_function, variable_type, variable_name);
          (*LocalVariableTable)[node.getIdentifier()] = new_variable_alloca;
          return new_variable_alloca;
        }

        // By this point, we must have (!variable_alloca && !variable_type)
        // Use of undeclared variable, so check whether can use global or throw error:
        GlobalVariable *global_variable = getModule()->getNamedGlobal(variable_name);
        return (global_variable) 
          ? getBuilder()->CreateLoad(global_variable->getValueType(), global_variable)
          : raiseError(("Use of undeclared variable: " + variable_name).c_str());
      }
      void* visit(AssignmentAST &node) {
        string assignee_name = node.getIdentifier();
        AllocaInst *assignee_alloca = (*LocalVariableTable)[assignee_name];
        GlobalVariable *global_variable = getModule()->getNamedGlobal(assignee_name);

        Value *expression = generateCode(*node.getAssignment());
        if (!expression) return raiseError("Malformed assignment expression");

        // Try to assign local variable first, to shadow any global variables:
        if (assignee_alloca) return storeAssignment(assignee_alloca, expression, assignee_alloca->getAllocatedType());
        if (global_variable) return storeAssignment(global_variable, expression, global_variable->getValueType());
        return raiseError("Unknown variable for assignment");
      }
      void* visit(FunctionCallAST &node) {
        Function *called_function = getModule()->getFunction(node.getIdentifier());
        if (!called_function) return raiseError("Unknown function called");

        int num_expected_arguments = called_function->arg_size();
        int num_supplied_arguments = node.getArguments().size();
        if (num_supplied_arguments != num_expected_arguments) {
          return raiseError("Invalid number of arguments in function call");
        }

        vector<Value *> arguments;
        for (PtrExpressionAST &arg_expression : node.getArguments()) {
          Value *arg = generateCode(*arg_expression);
          if (!arg) return raiseError("Malformed argument in function call");
          arguments.push_back(arg);
        }
        return getBuilder()->CreateCall(called_function, arguments);
      }
      void* visit(UnaryExpressionAST &node) {
        Value *expression = generateCode(*node.getExpression());
        if (!expression) return raiseError("Malformed expression (for unary operand)");

        Type *expression_type = expression->getType();
        switch (node.getOperator().type) {
          case MINUS:
            if (expression_type->isFloatTy()) {
              return getBuilder()->CreateFNeg(expression); // Flips sign bit
            }
            if (expression_type->isIntegerTy(32)) {
              return getBuilder()->CreateNeg(expression); // Subtracts from 0, potential overflow
            }
            return raiseError("Cannot negate non-arithmetic expression");
          case NOT:
            if (expression_type->isIntegerTy(1)) {
              return getBuilder()->CreateNot(expression); // Applies (1 XOR expression)
            }
            return raiseError("Cannot apply NOT to non-boolean expression");
          default:
            break;
        }
        return raiseError("Unknown or inappropriate unary operator");
      }
      void* visit(BinaryExpressionAST &node) {
        // Initialise LHS and RHS:
        Value *left = generateCode(*node.getLeft());
        if (!left) return raiseError("Malformed expression (for binary operand)");
        Type *left_type = left->getType();

        Value *right = generateCode(*node.getRight());
        if (!right) return raiseError("Malformed expression (for binary operand)");
        Type *right_type = right->getType();

        // Type-check:
        int operator_type = node.getOperator().type;
        MiniCType operands_type = INTEGER;
        if (left_type->isFloatTy() || right_type->isFloatTy()) {
          convertToFloatIfInt(&left, left_type);
          convertToFloatIfInt(&right, right_type);
          operands_type = FLOAT;
        } else if (left_type->isIntegerTy(1) && right_type->isIntegerTy(1)) {
          operands_type = BOOL;
        }
        if (left->getType() != right->getType()) {
          return raiseError("Mismatched expression types (for arithmetic operation)"); 
        }

        // Generate appropriate instruction:
        if ((isArithmeticOp(operator_type) && (operands_type == INTEGER || operands_type == FLOAT)) ||
            (isLogicOp(operator_type) && operands_type == BOOL)) {
          if (operands_type == BOOL) operands_type = INTEGER; // Necessary to match with instructions map
          return getBuilder()->CreateBinOp(arithmetic_instructions[{operator_type, operands_type}], left, right);
        }
        if ((isComparisonOp(operator_type) && (operands_type == INTEGER || operands_type == FLOAT)) ||
            (operator_type == EQ && operator_type == NE && operands_type == BOOL)) {
          if (operands_type == BOOL) operands_type = INTEGER; // Necessary to match with instructions map
          return getBuilder()->CreateCmp(comparison_instructions[{operator_type, operands_type}], left, right);
        }

        return raiseError("Unknown or inappropriate binary operator");
      }

      Value* storeAssignment(Value *assignee, Value *assignment, Type *assignee_type) {
        Type *assignment_type = assignment->getType();
        if (assignee_type->isIntegerTy(32) && assignment_type->isFloatTy()) {
          return raiseError("Cannot assign float value to integer variable, due to loss of precision");
        }
        if (assignee_type->isFloatTy() && assignment_type->isIntegerTy(32)) {
          convertToFloatIfInt(&assignment, assignment_type);
        }
        // NOTE: Important to explicitly check assignment type again (possible type mutation)
        if (assignee_type == assignment->getType()) {
          getBuilder()->CreateStore(assignment, assignee);
          return assignment;
        }

        return raiseError("Mismatched types on assignment");
      }

      void convertToFloatIfInt(Value **value, Type *value_type) {
        if (value_type->isIntegerTy(32)) {
          *value = getBuilder()->CreateSIToFP(*value, getFloatType(), "conv");
          (*value)->mutateType(getFloatType());
        }
      }

      bool isArithmeticOp(int operator_type) {
        return (operator_type == PLUS || operator_type == MINUS ||
            operator_type == MULT || operator_type == DIV ||
            operator_type == MOD);
      }
      bool isComparisonOp(int operator_type) {
        return (operator_type == EQ || operator_type == NE ||
            operator_type == LE || operator_type == LT ||
            operator_type == GE || operator_type == GT);
      }
      bool isLogicOp(int operator_type) {
        return (operator_type == AND || operator_type == OR ||
            operator_type == NOT);
      }
  };

  class StatementCodeGenerator : public StatementVisitor, public BaseCodeGenerator {
    ExpressionCodeGenerator ExpressionGenerator;

    public:
      StatementCodeGenerator(LLVMContext *llvmContext, Module *llvmModule, IRBuilder<> *irBuilder, SymbolTable *symbols, FunctionTable *functions)
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
        if (node.getBody()) {
          Value *expression = ExpressionGenerator.generateCode(*node.getBody());
          if (!expression) return;

          getBuilder()->CreateRet(expression);
          return;
        }
        getBuilder()->CreateRetVoid();
      }
  };

  class ProgramCodeGenerator : public BaseCodeGenerator {
    StatementCodeGenerator StatementGenerator;

    public:
      ProgramCodeGenerator(LLVMContext *llvmContext, Module *llvmModule, IRBuilder<> *irBuilder, SymbolTable *symbols, FunctionTable *functions)
        : BaseCodeGenerator(llvmContext, llvmModule, irBuilder, symbols, functions),
        StatementGenerator(llvmContext, llvmModule, irBuilder, symbols, functions) {}

      void generateGlobal(GlobalVariableAST &node) {
        Type *variable_type = convertNonVoidType(node.getVariable()->getType());
        if (!variable_type) return;

        getModule()->getOrInsertGlobal(node.getVariable()->getIdentifier(), variable_type);
      }

      void generatePrototype(PrototypeAST &node) {
        vector<Type *> parameter_types;
        for (int i = 0; i < node.getParameters().size(); i++) {
          MiniCType parameter_minictype = node.getParameters()[i]->getType();
          Type *parameter_type = convertNonVoidType(parameter_minictype);
          if (!parameter_type) { // Shouldn't be able to enter here, due to parser
            raiseError("Malformed parameter type");
            return;
          }
          parameter_types.push_back(parameter_type);    
        }

        MiniCType return_minictype = node.getReturnType();
        Type *return_type = (return_minictype == VOID) ? getVoidType() : convertNonVoidType(return_minictype);
        if (!return_type) { // Shouldn't be able to enter here, due to parser
          raiseError("Malformed return type");
          return;
        }

        FunctionType *function_type = FunctionType::get(return_type, parameter_types, false);

        Function *function = Function::Create(function_type, Function::ExternalLinkage, node.getIdentifier(), getModule());
        // Necessary to name arguments, in order to make it easier to use them inside function body.
        int i = 0; 
        for (auto &arg : function->args()) {
          arg.setName(node.getParameters()[i++]->getIdentifier());
        }

        (*Functions)[node.getIdentifier()] = &node;
        return;
      }

      void generateFunction(FunctionAST &node) {
        auto &prototype = *node.getPrototype();
        string function_name = node.getPrototype()->getIdentifier();
        (*Functions)[function_name] = &*node.getPrototype();

        Function *llvm_function = getModule()->getFunction(function_name);
        if (!llvm_function) {
          PrototypeAST *proto = (*Functions)[function_name];
          if (!proto) {
            raiseError("Function not known");
            return;
          }
          generatePrototype(*proto);
          llvm_function = getModule()->getFunction(function_name);
        }
        if (!llvm_function) return;

        BasicBlock *llvm_block = BasicBlock::Create(*getContext(), "entry", llvm_function);
        getBuilder()->SetInsertPoint(llvm_block);

        LocalVariableTable->clear();
        for (auto &llvm_argument : llvm_function->args()) {
          AllocaInst *argument_alloca = createEntryBlockAlloca(llvm_function, llvm_argument.getType(), llvm_argument.getName());
          getBuilder()->CreateStore(&llvm_argument, argument_alloca);
          (*LocalVariableTable)[string(llvm_argument.getName())] = argument_alloca;
        }

        StatementGenerator.generateCode(*node.getBody());
        verifyFunction(*llvm_function);
        return; // TODO: Handle missing return statements.
      }

      void generateProgram(const ProgramAST &node) {
        for (auto &e : node.getExterns()) generatePrototype(*e);
        for (auto &g : node.getGlobals()) generateGlobal(*g);
        for (auto &f : node.getFunctions()) generateFunction(*f);
        getModule()->print(outs(), nullptr);
      }
  };

  void generate(const ProgramAST &node) {
    LLVMContext MiniCContext;
    unique_ptr<Module> MiniCModule = std::make_unique<Module>("MiniC", *&MiniCContext);
    IRBuilder<> MiniCBuilder(MiniCContext);

    SymbolTable ss;
    FunctionTable fs;
    
    ProgramCodeGenerator(&MiniCContext, MiniCModule.get(), &MiniCBuilder, &ss, &fs).generateProgram(node);
  }

  std::nullptr_t raiseError(const char* msg) {
    fprintf(stderr, "Error: %s\n", msg);
    return nullptr;
  }

}

