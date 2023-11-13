#include "code-generator.h"

#include <map>
#include <memory>
#include <string>
#include <utility>
#include <vector>
#include <set>
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

#include "visitor.h"

using namespace llvm;

using std::string;
using std::vector;

using SymbolTable = std::map<string, AllocaInst *>;
using FunctionTable = std::map<string, PrototypeAST *>;

namespace minic_code_generator {

  class BaseCodeGenerator {
    public:
      BaseCodeGenerator(LLVMContext *llvmContext, Module *llvmModule, IRBuilder<> *irBuilder)
        : TheContext(llvmContext), TheModule(llvmModule), TheBuilder(irBuilder) {}

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
    vector<SymbolTable *> *ScopedLocalVariables;
    FunctionTable *Functions;

    std::map<std::pair<int, MiniCType>, Instruction::BinaryOps> arithmetic_instructions = {
      {{AND, INTEGER}, Instruction::BinaryOps::And},
      {{OR, INTEGER}, Instruction::BinaryOps::Or},
      {{PLUS, INTEGER}, Instruction::BinaryOps::Add},
      {{MINUS, INTEGER}, Instruction::BinaryOps::Sub},
      {{MULT, INTEGER}, Instruction::BinaryOps::Mul},
      {{DIV, INTEGER}, Instruction::BinaryOps::SDiv},
      {{MOD, INTEGER}, Instruction::BinaryOps::SRem},
      {{PLUS, FLOAT}, Instruction::BinaryOps::FAdd},
      {{MINUS, FLOAT}, Instruction::BinaryOps::FSub},
      {{MULT, FLOAT}, Instruction::BinaryOps::FMul},
      {{DIV, FLOAT}, Instruction::BinaryOps::FDiv},
      {{MOD, FLOAT}, Instruction::BinaryOps::FRem}
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
      ExpressionCodeGenerator(LLVMContext *llvmContext, Module *llvmModule, IRBuilder<> *irBuilder, vector<SymbolTable *> *symbols, FunctionTable *functions)
        : BaseCodeGenerator(llvmContext, llvmModule, irBuilder), ScopedLocalVariables(symbols), Functions(functions) {}

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
      void* visit(VariableLoadAST &node) {
        string variable_name = node.getIdentifier();

        AllocaInst *variable_alloca = searchScopes(variable_name);
        if (variable_alloca) {
          return getBuilder()->CreateLoad(variable_alloca->getAllocatedType(), variable_alloca, variable_name);
        }

        GlobalVariable *global_variable = getModule()->getNamedGlobal(variable_name);
        if (global_variable) {
          return getBuilder()->CreateLoad(global_variable->getValueType(), global_variable);
        }

        return throwError(("Cannot reference undeclared variable: " + variable_name).c_str());
      }
      void* visit(AssignmentAST &node) {
        // Generate assignment:
        Value *expression = generateCode(*node.getAssignment());

        // Try to assign local variable first, to shadow any global variables:
        string assignee_name = node.getIdentifier();
        AllocaInst *assignee_alloca = searchScopes(assignee_name);
        if (assignee_alloca) return storeAssignment(assignee_alloca, expression, assignee_alloca->getAllocatedType());
        GlobalVariable *global_variable = getModule()->getNamedGlobal(assignee_name);
        if (global_variable) return storeAssignment(global_variable, expression, global_variable->getValueType());

        return throwError("Unknown variable for assignment");
      }
      void* visit(FunctionCallAST &node) {
        fprintf(stderr, "Call\n");
        Function *called_function = getModule()->getFunction(node.getIdentifier());
        if (!called_function) return throwError(node.getToken(), "Unknown function called");

        int num_expected_arguments = called_function->arg_size();
        int num_supplied_arguments = node.getArguments().size();
        if (num_supplied_arguments != num_expected_arguments) {
          return throwError("Invalid number of arguments in function call");
        }

        vector<Value *> arguments;
        for (PtrExpressionAST &arg_expression : node.getArguments()) {
          Value *arg = generateCode(*arg_expression);
          if (!arg) return throwError("Malformed argument in function call");
          arguments.push_back(arg);
        }
        return getBuilder()->CreateCall(called_function, arguments);
      }
      void* visit(UnaryExpressionAST &node) {
        Value *expression = generateCode(*node.getExpression());
        Type *expression_type = expression->getType();
        switch (node.getOperator().type) {
          case MINUS:
            if (expression_type->isFloatTy()) {
              return getBuilder()->CreateFNeg(expression); // Flips sign bit
            }
            if (expression_type->isIntegerTy(32)) {
              return getBuilder()->CreateNeg(expression); // Subtracts from 0, potential overflow
            }
            return throwError("Cannot negate non-arithmetic expression");
          case NOT:
            if (expression_type->isIntegerTy(1)) {
              return getBuilder()->CreateNot(expression); // Applies (1 XOR expression)
            }
            return throwError("Cannot apply NOT to non-boolean expression");
          default:
            break;
        }
        return throwError(node.getToken(), "Unknown unary operator or inappropriate operand types");
      }
      void* visit(BinaryExpressionAST &node) {
        // Initialise LHS and RHS:
        Value *left = generateCode(*node.getLeft());
        Type *left_type = left->getType();
        Value *right = generateCode(*node.getRight());
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
          return throwError("Mismatched expression types (for arithmetic operation)"); 
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

        return throwError(node.getToken(), "Unknown binary operator or inappropriate operand types");
      }

      AllocaInst* searchScopes(string identifier) {
        AllocaInst *found_alloca;
        for (int i = ScopedLocalVariables->size()-1; i >= 0; i--) {
          found_alloca = (*ScopedLocalVariables->at(i))[identifier];
          if (found_alloca) break;
        }
        return found_alloca;
      }

      Value* storeAssignment(Value *assignee, Value *assignment, Type *assignee_type) {
        Type *assignment_type = assignment->getType();
        if (assignee_type->isIntegerTy(32) && assignment_type->isFloatTy()) {
          throwError("Cannot assign float value to integer variable, due to loss of precision");
          exit(1);
        }
        if (assignee_type->isFloatTy() && assignment_type->isIntegerTy(32)) {
          convertToFloatIfInt(&assignment, assignment_type);
        }
        // NOTE: Important to explicitly check assignment type again (possible type mutation)
        if (assignee_type == assignment->getType()) {
          getBuilder()->CreateStore(assignment, assignee);
          return assignment;
        }

        return throwError("Mismatched types on assignment");
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
    vector<SymbolTable *> *ScopedLocalVariables;
    FunctionTable *Functions;

    public:
      StatementCodeGenerator(LLVMContext *llvmContext, Module *llvmModule, IRBuilder<> *irBuilder, vector<SymbolTable *> *symbols, FunctionTable *functions)
        : BaseCodeGenerator(llvmContext, llvmModule, irBuilder), ScopedLocalVariables(symbols), Functions(functions),
        ExpressionGenerator(llvmContext, llvmModule, irBuilder, symbols, functions) {}

      void generateCode(const StatementAST &node) {
        const_cast<StatementAST &>(node).dispatch(*this);
      }

      void visit(VariableDeclarationAST &node) {
        string variable_name = node.getVariable()->getIdentifier();
        Type *variable_type = convertNonVoidType(node.getType());
        AllocaInst *variable_alloca = (*ScopedLocalVariables->back())[variable_name];

        if (variable_alloca) {
          throwError(("Cannot redeclare local variable: " + variable_name).c_str());
        }

        Function *parent_function = getBuilder()->GetInsertBlock()->getParent();
        AllocaInst *new_variable_alloca = createEntryBlockAlloca(parent_function, variable_type, variable_name);
        (*ScopedLocalVariables->back())[node.getVariable()->getIdentifier()] = new_variable_alloca;
      }
      void visit(ExpressionStatementAST &node) {
        ExpressionGenerator.generateCode(*node.getExpression());
      }
      void visit(CodeBlockAST &node) {
        SymbolTable new_scope;
        ScopedLocalVariables->push_back(&new_scope);
        for (const PtrVariableDeclarationAST &var : node.getDeclarations()) generateCode(*var);
        for (const PtrStatementAST &stmt : node.getStatements()) generateCode(*stmt);
        ScopedLocalVariables->pop_back();
      }
      void visit(IfBlockAST &node) {
        Function *parent_function = getBuilder()->GetInsertBlock()->getParent();
        BasicBlock *true_block = BasicBlock::Create(*getContext(), "if.true", parent_function);
        BasicBlock *false_block = BasicBlock::Create(*getContext(), "if.false", parent_function);
        BasicBlock *after_block = BasicBlock::Create(*getContext(), "if.after", parent_function);

        // Generate code for the condition:
        Value *condition = ExpressionGenerator.generateCode(*node.getCondition());
        getBuilder()->CreateCondBr(condition, true_block, false_block);

        // Generate code for the True branch:
        getBuilder()->SetInsertPoint(true_block);
        generateCode(*node.getTrueBranch());
        getBuilder()->CreateBr(after_block);

        // Generate code for the False branch:
        getBuilder()->SetInsertPoint(false_block);
        generateCode(*node.getFalseBranch());
        getBuilder()->CreateBr(after_block);

        // Place subsequent insertion to after block:
        getBuilder()->SetInsertPoint(after_block);
      }
      void visit(WhileBlockAST &node) {
        Function *parent_function = getBuilder()->GetInsertBlock()->getParent();
        BasicBlock *cond_block = BasicBlock::Create(*getContext(), "while.cond", parent_function);
        BasicBlock *loop_block = BasicBlock::Create(*getContext(), "while.body", parent_function);
        BasicBlock *after_block= BasicBlock::Create(*getContext(), "while.after", parent_function);

        // Unconditionally enter the condition upon first encountering the while loop:
        getBuilder()->CreateBr(cond_block);
 
        // Generate code for the condition:
        getBuilder()->SetInsertPoint(cond_block);
        Value *condition = ExpressionGenerator.generateCode(*node.getCondition());
        getBuilder()->CreateCondBr(condition, loop_block, after_block);

        // Generate code for the body:
        getBuilder()->SetInsertPoint(loop_block);
        generateCode(*node.getBody());
        getBuilder()->CreateBr(cond_block);
        loop_block = getBuilder()->GetInsertBlock();

        // Place subsequent insertion to after block:
        getBuilder()->SetInsertPoint(after_block);
      }
      void visit(ReturnAST &node) {
        Function *parent_function = getBuilder()->GetInsertBlock()->getParent();
        Value *return_body = (node.getBody())
          ? ExpressionGenerator.generateCode(*node.getBody()) : nullptr;

        // Type-check:
        if ((return_body && parent_function->getReturnType() != return_body->getType()) ||
            (!return_body && parent_function->getReturnType() != getVoidType())) {
          throwError("Mismatched function return type and return statement");
        }

        // Generate return statement:
        if (node.getBody()) {
          getBuilder()->CreateRet(return_body);
        } else {
          getBuilder()->CreateRetVoid();
        }
      }
  };

  class ProgramCodeGenerator : public BaseCodeGenerator {
    FunctionTable *Functions;
    IRBuilder<> MiniCBuilder;

    public:
      ProgramCodeGenerator(LLVMContext *llvmContext, Module *llvmModule, FunctionTable *functions)
        : BaseCodeGenerator(llvmContext, llvmModule, &MiniCBuilder),
        Functions(functions), MiniCBuilder(*llvmContext) {}

      void generateGlobal(VariableDeclarationAST &node) {
        Type *variable_type = convertNonVoidType(node.getType());
        if (!variable_type) throwError("Malformed global variable type");

        getModule()->getOrInsertGlobal(node.getVariable()->getIdentifier(), variable_type);
      }

      void generatePrototype(PrototypeAST &node) {
        // Convert parsed parameter types to LLVM Types:
        std::set<string> parameter_names;
        vector<Type *> parameter_types;
        for (const PtrVariableDeclarationAST &var : node.getParameters()) {
          if (parameter_names.count(var->getVariable()->getIdentifier()) > 0) {
            throwError("Cannot redeclare parameter");
          } else {
            parameter_names.insert(var->getVariable()->getIdentifier());
          }
          Type *parameter_type = convertNonVoidType(var->getType());
          if (!parameter_type) throwError("Malformed parameter type");
          parameter_types.push_back(parameter_type);    
        }
        // Convert parsed type to LLVM Type:
        MiniCType return_minictype = node.getReturnType();
        Type *return_type = (return_minictype == VOID) ? getVoidType() : convertNonVoidType(return_minictype);
        if (!return_type) throwError("Malformed return type");
        // Generate LLVM function signature:
        FunctionType *function_signature = FunctionType::get(return_type, parameter_types, false);
        
        // Check whether the same function exists:
        Function *existing_function = getModule()->getFunction(node.getIdentifier());
        if (existing_function && existing_function->getFunctionType() == function_signature) {
          throwError("Cannot redeclare function"); // NOTE: Relying on lazy evaluation above
        }

        // Declare LLVM Function:
        Function *function = Function::Create(function_signature, Function::ExternalLinkage, node.getIdentifier(), getModule());
        int i = 0; // Name the arguments in order to make it easier to use them inside function body:
        for (auto &arg : function->args()) arg.setName(node.getParameters()[i++]->getVariable()->getIdentifier());

        // Add PrototypeAST to known functions:
        (*Functions)[node.getIdentifier()] = &node;
      }

      void generateFunction(FunctionAST &node) {
        // Generate function signature and declare function:
        generatePrototype(*node.getPrototype());
        Function *function = getModule()->getFunction(node.getPrototype()->getIdentifier());
        if (!function) throwError("Unknown function");

        // Generate block for function body:
        BasicBlock *functionBodyBlock = BasicBlock::Create(*getContext(), "entry", function);
        getBuilder()->SetInsertPoint(functionBodyBlock);

        // Initialise local variables with function parameters:
        SymbolTable function_scope;
        for (auto &arg : function->args()) {
          AllocaInst *argument_alloca = createEntryBlockAlloca(function, arg.getType(), arg.getName());
          getBuilder()->CreateStore(&arg, argument_alloca);
          function_scope[string(arg.getName())] = argument_alloca;
        }

        // Generate code for the function body:
        vector<SymbolTable *> symbols;
        symbols.push_back(&function_scope);
        StatementCodeGenerator(getContext(), getModule(), getBuilder(), &symbols, Functions).generateCode(*node.getBody());
        // Ensure function has a return statement:
        if (!getBuilder()->GetInsertBlock()->getTerminator()) {
          if (function->getReturnType()->isVoidTy()) {
            getBuilder()->CreateRetVoid();
          } else {
            throwError("Non-void function must have a return statement");
          }
        }
        // LLVM-supplied function to catch consistency bugs:
        verifyFunction(*function);
      }

      void generateProgram(const ProgramAST &node) {
        for (const PtrVariableDeclarationAST &g : node.getGlobals()) generateGlobal(*g);
        for (const PtrPrototypeAST &e : node.getExterns()) generatePrototype(*e);
        for (const PtrFunctionAST &f : node.getFunctions()) generateFunction(*f);
        getModule()->print(outs(), nullptr);
      }
  };

  void generate(const ProgramAST &node) {
    LLVMContext MiniCContext;
    std::unique_ptr<Module> MiniCModule = std::make_unique<Module>("MiniC", *&MiniCContext);

    FunctionTable fs;
    ProgramCodeGenerator(&MiniCContext, MiniCModule.get(), &fs).generateProgram(node);
  }

  std::nullptr_t throwError(const char* msg) {
    // TODO: Use tokens from AST nodes to pin-point line and column of error.
    fprintf(stderr, "Error: %s\n", msg);
    exit(1);
    return nullptr;
  }

  std::nullptr_t throwError(TOKEN t, const char *msg) {
    fprintf(stderr, "Error: %s [%d:%d]\n", msg, t.line_num, t.column_num);
    exit(1);
    return nullptr;
  }

}

