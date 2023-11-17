#include "code-generator.h"

#include <map>
#include <memory>
#include <set>
#include <stdio.h>
#include <string>
#include <vector>

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
using FunctionTable = std::map<string, vector<Function *>>;

namespace minic_code_generator {

  class BaseCodeGenerator {
    public:
      BaseCodeGenerator(LLVMContext *llvmContext, Module *llvmModule, IRBuilder<> *irBuilder)
        : TheContext(llvmContext), TheModule(llvmModule), TheBuilder(irBuilder) {}
    protected:
      LLVMContext *getContext() { return TheContext; }
      Module *getModule() { return TheModule; }
      IRBuilder<> *getBuilder() { return TheBuilder; }

      Type *getIntType() { return Type::getInt32Ty(*getContext()); }
      Type *getFloatType() { return Type::getFloatTy(*getContext()); }
      Type *getBoolType() { return Type::getInt1Ty(*getContext()); }
      Type *getVoidType() { return Type::getVoidTy(*getContext()); }

      bool isIntType(Type *t) { return t->isIntegerTy(32); }
      bool isFloatType(Type *t) { return t->isFloatTy(); }
      bool isBoolType(Type *t) { return t->isIntegerTy(1); }
      bool isVoidType(Type *t) { return t->isVoidTy(); }

      Type *convertNonVoidType(TOKEN error_token, const string &error_substring, MiniCType minic_type) {
        switch (minic_type) {
          case INTEGER: return getIntType();
          case FLOAT: return getFloatType();
          case BOOL: return getBoolType();
          default: break;
        }
        return throwError(error_token, "Invalid " + error_substring + " type");
      }

      void convertToBool(Value **value) {
        if (isIntType((*value)->getType())) {
          *value = getBuilder()->CreateCmp(CmpInst::Predicate::ICMP_NE, *value, Constant::getNullValue(getIntType()), "tobool");
        } else if (isFloatType((*value)->getType())) {
          *value = getBuilder()->CreateCmp(CmpInst::Predicate::FCMP_UNE, *value, Constant::getNullValue(getFloatType()), "tobool");
        }
      }
      void convertToInt(Value **value) {
        if (isBoolType((*value)->getType())) {
          *value = getBuilder()->CreateZExt(*value, getIntType(), "toint");
        } else if (isFloatType((*value)->getType())) {
          *value = getBuilder()->CreateFPToSI(*value, getIntType(), "toint");
        }
      }
      void convertToFloat(Value **value) {
        if (isBoolType((*value)->getType())) {
          *value = getBuilder()->CreateUIToFP(*value, getFloatType(), "tofloat");
        } else if (isIntType((*value)->getType())) {
          *value = getBuilder()->CreateSIToFP(*value, getFloatType(), "tofloat");
        }
      }
      void implicitCastToMatch(TOKEN error_token, const string &error_substring, Type *expected_type, Value **value) {
        if (isBoolType(expected_type)) { convertToBool(value); }
        else if (isIntType(expected_type)) { convertToInt(value); }
        else if (isFloatType(expected_type)) { convertToFloat(value); }
      }
      void implicitCastToWiden(TOKEN error_token, const string &error_substring, Type *expected_type, Value **value) {
        Type *value_type = (*value)->getType();
        if (isFloatType(expected_type)) {
          // Both int and bool can be widened to float:
          convertToFloat(value);
        } else if (isIntType(expected_type)) {
          // Only bool can be widened to int:
          if (isFloatType(value_type)) {
            throwError(error_token, "Mismatched expression type (float) with " + error_substring + " type (int)");
          }
          convertToInt(value);
        } else if (isBoolType(expected_type)) {
          // Nothing can be widened to bool:
          if (isFloatType(value_type) || isIntType(value_type)) {
            throwError(error_token, "Mismatched expression type (int/float) with " + error_substring + " type (bool)");
          }
        }
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
      {{EQ, FLOAT}, CmpInst::Predicate::FCMP_UEQ},
      {{NE, FLOAT}, CmpInst::Predicate::FCMP_UNE},
      {{GT, FLOAT}, CmpInst::Predicate::FCMP_UGT},
      {{GE, FLOAT}, CmpInst::Predicate::FCMP_UGE},
      {{LT, FLOAT}, CmpInst::Predicate::FCMP_ULT},
      {{LE, FLOAT}, CmpInst::Predicate::FCMP_ULE}
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

        GlobalVariable *global_variable = getModule()->getGlobalVariable(variable_name);
        if (global_variable) {
          return getBuilder()->CreateLoad(global_variable->getValueType(), global_variable, variable_name);
        }

        string err_str = "Cannot reference undeclared variable: " + variable_name;
        return throwError(node.getToken(), err_str);
      }
      void* visit(AssignmentAST &node) {
        // Generate assignment:
        Value *expression = generateCode(*node.getAssignment());
        string assignee_name = node.getIdentifier();

        // Try to assign local variable first, to shadow any global variables:
        AllocaInst *assignee_alloca = searchScopes(assignee_name);
        if (assignee_alloca) return storeAssignment(node.getToken(), assignee_alloca, expression, assignee_alloca->getAllocatedType());

        GlobalVariable *global_variable = getModule()->getGlobalVariable(assignee_name);
        if (global_variable) return storeAssignment(node.getToken(), global_variable, expression, global_variable->getValueType());

        string err_str = "Unknown variable for assignment: " + assignee_name;
        return throwError(node.getToken(), err_str);
      }
      void* visit(FunctionCallAST &node) {
        string called_name = node.getIdentifier();
        int num_supplied_arguments = node.getArguments().size();

        // Look for an appropriate function in the function table:
        // (appropriate means having the same name and number of arguments)
        Function *called_function;
        vector<Function *> potential_functions = (*Functions)[called_name];
        for (auto &f : potential_functions) {
          int num_expected_arguments = f->arg_size();
          if (num_supplied_arguments == num_expected_arguments) {
            called_function = f;
            break;
          }
        }

        if (!called_function) {
          string err_str = "Unknown function called: " + called_name
            + " with " + std::to_string(num_supplied_arguments) + " arguments";
          return throwError(node.getToken(), err_str);
        }

        // Type-check each argument:
        vector<Value *> arguments;
        for (int idx = 0; idx < num_supplied_arguments; idx++) {
          Value *argument = generateCode(*node.getArguments()[idx]);

          Type *expected_type = called_function->getArg(idx)->getType();
          implicitCastToMatch(node.getArguments()[idx]->getToken(), "argument", expected_type, &argument);
          /* implicitCastToWiden(node.getArguments()[idx]->getToken(), "argument", expected_type, &argument); */

          arguments.push_back(argument);
        }
        return getBuilder()->CreateCall(called_function, arguments);
      }
      void* visit(UnaryExpressionAST &node) {
        Value *expression = generateCode(*node.getExpression());
        Type *expression_type = expression->getType();
        switch (node.getOperator().type) {
          case MINUS:
            Value *negated;
            if (isFloatType(expression_type)) {
              negated = getBuilder()->CreateFNeg(expression); // Flips sign bit
            } else {
              convertToInt(&expression); // Widens bool to int, if needed
              negated = getBuilder()->CreateNeg(expression); // Subtracts from 0, potential overflow
            }
            return negated;
          case NOT:
            convertToBool(&expression); // Numeric types treated as boolean in Standard C
            return getBuilder()->CreateNot(expression); // Applies (1 XOR expression)
          default:
            break;
        }
        string err_str = "Unknown unary operator: " + node.getOperator().lexeme;
        return throwError(node.getToken(), err_str);
      }
      void* visit(BinaryExpressionAST &node) {
        int operator_type = node.getOperator().type;

        // Lazily handle AND and OR:
        if (isLogicOp(operator_type)) {
          // In-order traversal of the binary expression,
          // flattening the boolean clauses into `logic_subexprs` and `logic_operators`,
          // which are used for easier lazy evaluation:

          logic_expr_depth++;

          if (isLogicOp(node.getLeft()->getToken().type)) {
            generateCode(*node.getLeft());
          } else {
            logic_subexprs.push_back(&node.getLeft());
          }

          logic_operators.push_back(node.getOperator());

          if (isLogicOp(node.getRight()->getToken().type)) {
            generateCode(*node.getRight());
          } else {
            logic_subexprs.push_back(&node.getRight());
          }

          logic_expr_depth--;

          // Generate the full boolean expression:
          if (logic_expr_depth < 1) return generateLazyBooleanExpression();
          return nullptr; // This is only returned from the two generateCode(..) calls above.
        }

        // Initialise LHS and RHS for non-lazy evaluation:
        Value *left = generateCode(*node.getLeft());
        Type *left_type = left->getType();
        Value *right = generateCode(*node.getRight());
        Type *right_type = right->getType();

        // Type-check; if mismatched, only implicit cast to widen:
        MiniCType operands_type;
        if (isFloatType(left_type) || isFloatType(right_type)) {
          convertToFloat(&left);
          convertToFloat(&right);
          operands_type = FLOAT;
        } else if (isIntType(left_type) || isIntType(right_type)) {
          convertToInt(&left);
          convertToInt(&right);
          operands_type = INTEGER;
        } else if (isBoolType(left_type) && isBoolType(right_type)) {
          operands_type = BOOL;
        } else { // Should be unreachable:
          string err_str = "Mismatched operand types for " + node.getOperator().lexeme + " operator";
          return throwError(node.getToken(), err_str); 
        }

        // Generate appropriate instruction:
        if (isArithmeticOp(operator_type)) {
          if (operands_type != INTEGER && operands_type != FLOAT) {
            return throwError(node.getToken(), "Cannot perform arithmetic on non-numeric operands");
          }
          return getBuilder()->CreateBinOp(arithmetic_instructions[{operator_type, operands_type}], left, right);
        }
        if (isComparisonOp(operator_type)) {
          if (operands_type == BOOL && (operator_type != EQ && operator_type != NE)) {
            return throwError(node.getToken(), "Cannot perform non-equality comparison on boolean operands");
          }
          if (operands_type != INTEGER && operands_type != FLOAT) {
            return throwError(node.getToken(), "Cannot perform comparison on non-numeric operands");
          }
          if (operands_type == BOOL) operands_type = INTEGER; // Necessary to match with instructions map
          return getBuilder()->CreateCmp(comparison_instructions[{operator_type, operands_type}], left, right);
        }

        string err_str = "Unknown binary operator: " + node.getOperator().lexeme;
        return throwError(node.getToken(), err_str);
      }

      int logic_expr_depth = 0;
      vector<const PtrExpressionAST *> logic_subexprs;
      vector<TOKEN> logic_operators;

      PHINode* generateLazyBooleanExpression() {
        BasicBlock *entry_block = getBuilder()->GetInsertBlock();
        Function *parent_function = entry_block->getParent();
  
        // NOTE: Boolean expressions in C are in Disjunctive Normal Form.
        // That is, clauses consist of &&, and they are separated by ||.

        // If expression contains at least one || operator,
        // there are multiple clauses that will be merged via a PHINode in 'lor_merge_block':
        BasicBlock *lor_merge_block = nullptr;
        PHINode *lor_phi;
        for (TOKEN &t : logic_operators) {
          if (t.type == OR) {
            lor_merge_block = BasicBlock::Create(*getContext(), "lor.end", parent_function);
            getBuilder()->SetInsertPoint(lor_merge_block);
            lor_phi = getBuilder()->CreatePHI(getBoolType(), 0);
            break;
          }
        }

        int idx = logic_operators.size()-1;
        BasicBlock *next_clause;

        BasicBlock *land_merge_block = nullptr;
        PHINode *land_phi;
        TOKEN rhs_operator = logic_operators.back();
        if (rhs_operator.type == AND) {
          land_merge_block = BasicBlock::Create(*getContext(), "land.end", parent_function);
          next_clause = land_merge_block;
          getBuilder()->SetInsertPoint(land_merge_block);
          land_phi = getBuilder()->CreatePHI(getBoolType(), 0);
          if (lor_merge_block) getBuilder()->CreateBr(lor_merge_block);

          generateLogicRHS(parent_function, land_merge_block, land_phi, idx);
          if (lor_merge_block) lor_phi->addIncoming(land_phi, land_merge_block);
        } else if (rhs_operator.type == OR) {
          generateLogicRHS(parent_function, lor_merge_block, lor_phi, idx);
        } else { // Should be unreachable due to parser:
          throwError(rhs_operator, "Invalid operator in logical expression: " + rhs_operator.lexeme);
        }

        TOKEN cur_operator;
        Value *cur_operand;
        BasicBlock *lhs_block;
        BasicBlock *rhs_block;
        while (idx >= 0) {
          cur_operator = logic_operators[idx];
          cur_operand = generateLogicLHS(parent_function, &rhs_block, &lhs_block, entry_block, idx);
          if (cur_operator.type == OR) {
            next_clause = rhs_block;
            getBuilder()->CreateCondBr(cur_operand, lor_merge_block, rhs_block);
            lor_phi->addIncoming(getBuilder()->getTrue(), lhs_block);
          } else if (cur_operator.type == AND) {
            getBuilder()->CreateCondBr(cur_operand, rhs_block, next_clause);
            if (land_merge_block && (idx != 0 || next_clause == land_merge_block)) {
              land_phi->addIncoming(getBuilder()->getFalse(), lhs_block);
            }
          } else { // Should be unreachable due to parser:
            throwError(rhs_operator, "Invalid operator in logical expression: " + cur_operator.lexeme);
          }
          idx--;
        }

        if (lor_merge_block) {
          getBuilder()->SetInsertPoint(lor_merge_block);
          return lor_phi;
        } else if (land_merge_block) {
          getBuilder()->SetInsertPoint(land_merge_block);
          return land_phi;
        }
        return nullptr; // Should be unreachable?
      }

      void generateLogicRHS(Function *parent_function, BasicBlock *merge_block, PHINode *phi, int idx) {
        // This is for the right-most operand only!
        BasicBlock *rhs_block = BasicBlock::Create(*getContext(), "logic.rhs", parent_function);
        getBuilder()->SetInsertPoint(rhs_block);
        Value *last_operand = generateCode(**logic_subexprs[idx+1]);
        if (!isBoolType(last_operand->getType())) convertToBool(&last_operand); // Numeric types treated as boolean in Standard C
        getBuilder()->CreateBr(merge_block);
        phi->addIncoming(last_operand, rhs_block);
      }

      Value* generateLogicLHS(Function *parent_function, BasicBlock **rhs_block, BasicBlock **lhs_block, BasicBlock *entry_block, int idx) {
        // This is for the generic logic operand, as part of lazy generation.
        *rhs_block = getBuilder()->GetInsertBlock();
        *lhs_block = (idx == 0) ? entry_block : BasicBlock::Create(*getContext(), "logic.lhs", parent_function);
        getBuilder()->SetInsertPoint(*lhs_block);
        Value *cur_operand = generateCode(**logic_subexprs[idx]);
        if (!isBoolType(cur_operand->getType())) convertToBool(&cur_operand); // Numeric types treated as boolean in Standard C
        return cur_operand;
      }

      AllocaInst* searchScopes(string identifier) {
        AllocaInst *found_alloca;
        for (int i = ScopedLocalVariables->size()-1; i >= 0; i--) {
          found_alloca = (*ScopedLocalVariables->at(i))[identifier];
          if (found_alloca) break;
        }
        return found_alloca;
      }

      Value* storeAssignment(TOKEN error_token, Value *assignee, Value *assignment, Type *assignee_type) {
        Type *assignment_type = assignment->getType();

        if (assignee_type != assignment_type) {
          implicitCastToMatch(error_token, "assignment", assignee_type, &assignment);
        }
        getBuilder()->CreateStore(assignment, assignee);
        return assignment;
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
        return (operator_type == AND || operator_type == OR);
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
        Type *variable_type = convertNonVoidType(node.getToken(), "variable", node.getType());
        AllocaInst *variable_alloca = (*ScopedLocalVariables->back())[variable_name];

        if (variable_alloca) {
          string err_str = "Cannot redeclare variable: " + variable_name;
          throwError(node.getToken(), err_str);
        }

        Function *parent_function = getBuilder()->GetInsertBlock()->getParent();
        AllocaInst *new_variable_alloca = getBuilder()->CreateAlloca(variable_type, nullptr, variable_name);
        (*ScopedLocalVariables->back())[node.getVariable()->getIdentifier()] = new_variable_alloca;
      }
      void visit(ExpressionStatementAST &node) {
        ExpressionGenerator.generateCode(*node.getExpression());
      }
      void visit(CodeBlockAST &node) {
        SymbolTable new_scope;
        ScopedLocalVariables->push_back(&new_scope);
        for (const PtrVariableDeclarationAST &var : node.getDeclarations()) generateCode(*var);
        for (const PtrStatementAST &stmt : node.getStatements()) {
          if (getBuilder()->GetInsertBlock()->getTerminator()) {
            break; // Stopping code generation for this block after a branch or a return instruction
          }
          generateCode(*stmt);
        }
        ScopedLocalVariables->pop_back();
      }
      void visit(IfBlockAST &node) {
        Function *parent_function = getBuilder()->GetInsertBlock()->getParent();
        BasicBlock *true_block = BasicBlock::Create(*getContext(), "if.true", parent_function);
        BasicBlock *false_block = BasicBlock::Create(*getContext(), "if.false", parent_function);
        BasicBlock *after_block = BasicBlock::Create(*getContext(), "if.after", parent_function);

        bool is_true_block_terminated = false;
        bool is_false_block_terminated = false;

        // Generate code for the condition:
        Value *condition = ExpressionGenerator.generateCode(*node.getCondition());
        if (!isBoolType(condition->getType())) convertToBool(&condition);
        // Handle optional False branch; note the conditional branches:
        if (node.getFalseBranch()) {
          getBuilder()->CreateCondBr(condition, true_block, false_block);

          // Generate code for the False branch:
          getBuilder()->SetInsertPoint(false_block);
          generateCode(*node.getFalseBranch());
          // Handle premature terminators (ie. return statements):
          if (!getBuilder()->GetInsertBlock()->getTerminator()) {
            getBuilder()->CreateBr(after_block);
          } else {
            is_false_block_terminated = true;
          }
        } else {
          getBuilder()->CreateCondBr(condition, true_block, after_block);
          // No False branch, so false_block is unreachable:
          false_block->eraseFromParent();
        }

        // Generate code for the True branch:
        getBuilder()->SetInsertPoint(true_block);
        generateCode(*node.getTrueBranch());
        // Handle premature terminators (ie. return statements):
        if (!getBuilder()->GetInsertBlock()->getTerminator()) {
          getBuilder()->CreateBr(after_block);
        } else {
          is_true_block_terminated = true;
        }

        if (is_true_block_terminated && is_false_block_terminated) {
          // Both blocks were prematurely terminated, so after_block is unreachable:
          after_block->eraseFromParent();
        } else {
          // Place subsequent insertion to after block:
          getBuilder()->SetInsertPoint(after_block);
        }
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
        if (!isBoolType(condition->getType())) convertToBool(&condition);
        getBuilder()->CreateCondBr(condition, loop_block, after_block);

        // Generate code for the body:
        getBuilder()->SetInsertPoint(loop_block);
        generateCode(*node.getBody());
        // Handle premature terminators (ie. return statements):
        if (!getBuilder()->GetInsertBlock()->getTerminator()) {
          getBuilder()->CreateBr(cond_block);
        }

        // Place subsequent insertion to after block:
        getBuilder()->SetInsertPoint(after_block);
      }
      void visit(ReturnAST &node) {
        Function *parent_function = getBuilder()->GetInsertBlock()->getParent();
        Value *return_body = (node.getBody())
          ? ExpressionGenerator.generateCode(*node.getBody()) : nullptr;

        // Type-check; if mismatched, only implicit cast to widen:
        Type *expected_type = parent_function->getReturnType();
        if (return_body) {
          implicitCastToWiden(node.getToken(), "function return", expected_type, &return_body);
          getBuilder()->CreateRet(return_body);
        } else {
          if (expected_type != getVoidType()) {
            throwError(node.getToken(), "Mismatched expression type with function return type (void)");
          }
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
        string global_name = node.getVariable()->getIdentifier();

        GlobalVariable *global_variable = getModule()->getGlobalVariable(global_name);
        if (global_variable) {
          string err_str = "Cannot redeclare global variable: " + global_name;
          throwError(node.getToken(), err_str);
        }

        // NOTE: This allocated memory, which is not manually freed:
        Type *variable_type = convertNonVoidType(node.getToken(), "global variable", node.getType());
        new GlobalVariable(*getModule(), variable_type, false, GlobalValue::CommonLinkage, Constant::getNullValue(variable_type), global_name);
      }

      Function* generatePrototype(PrototypeAST &node) {
        // Convert parsed parameter types to LLVM Types:
        std::set<string> parameter_names;
        vector<Type *> parameter_types;
        for (const PtrVariableDeclarationAST &par : node.getParameters()) {
          string parameter_name = par->getVariable()->getIdentifier();
          if (parameter_names.count(parameter_name) > 0) {
            return throwError(par->getToken(), "Cannot redeclare parameter: " + parameter_name);
          } else {
            parameter_names.insert(parameter_name);
          }
          Type *parameter_type = convertNonVoidType(par->getToken(), "parameter", par->getType());
          parameter_types.push_back(parameter_type);    
        }
        // Convert parsed type to LLVM Type:
        MiniCType return_minictype = node.getReturnType();
        Type *return_type = (return_minictype == VOID) ? getVoidType() : convertNonVoidType(node.getToken(), "function return", return_minictype);
        if (!return_type) return throwError(node.getToken(), "Invalid function return type");
        // Generate LLVM function signature:
        FunctionType *function_signature = FunctionType::get(return_type, parameter_types, false);
        
        // Check whether the same function exists (overloading is not permitted):
        vector<Function *> existing_functions = (*Functions)[node.getIdentifier()];
        for (auto &f : existing_functions) {
          int num_args = node.getParameters().size();
          if (f->arg_size() == num_args) {
            string err_str = "Cannot redeclare function: " + node.getIdentifier() + " with " + std::to_string(num_args) + " arguments";
            return throwError(node.getToken(), err_str);
          }
        }

        // Declare LLVM Function:
        Function *function = Function::Create(function_signature, Function::ExternalLinkage, node.getIdentifier(), getModule());
        int i = 0; // Name the arguments in order to make it easier to use them inside function body:
        for (auto &arg : function->args()) arg.setName(node.getParameters()[i++]->getVariable()->getIdentifier());

        // Add PrototypeAST to known functions:
        (*Functions)[node.getIdentifier()].push_back(function);

        return function;
      }

      void generateFunction(FunctionAST &node) {
        // Generate function signature and declare function:
        Function *function = generatePrototype(*node.getPrototype());
        if (!function) throwError(node.getToken(), "Unknown function: " + node.getPrototype()->getIdentifier());

        // Generate block for function body:
        BasicBlock *functionBodyBlock = BasicBlock::Create(*getContext(), "entry", function);
        getBuilder()->SetInsertPoint(functionBodyBlock);

        // Initialise local variables with function parameters:
        SymbolTable function_scope;
        for (auto &arg : function->args()) {
          AllocaInst *argument_alloca = getBuilder()->CreateAlloca(arg.getType(), nullptr, arg.getName());
          getBuilder()->CreateStore(&arg, argument_alloca);
          function_scope[string(arg.getName())] = argument_alloca;
        }

        // Generate code for the function body:
        vector<SymbolTable *> symbols;
        symbols.push_back(&function_scope);
        StatementCodeGenerator(getContext(), getModule(), getBuilder(), &symbols, Functions).generateCode(*node.getBody());
        // Ensure all function paths are terminated by a branch or a return statement:
        for (auto block_iterator = function->begin(); block_iterator != function->end(); ++block_iterator) {
          if (!block_iterator->getTerminator()) {
            if (isVoidType(function->getReturnType())) {
              getBuilder()->CreateRetVoid();
            } else {
              throwError(node.getToken(), "Non-void function must have a return statement");
            }
          }
        }

        // LLVM-supplied function to catch consistency bugs:
        verifyFunction(*function);
      }

      void generateProgram(const ProgramAST &node) {
        for (const PtrVariableDeclarationAST &g : node.getGlobals()) generateGlobal(*g);
        for (const PtrPrototypeAST &e : node.getExterns()) generatePrototype(*e);
        for (const PtrFunctionAST &f : node.getFunctions()) generateFunction(*f);
      }
  };

  std::unique_ptr<Module> generate(LLVMContext *llvm_context, const ProgramAST &node) {
    std::unique_ptr<Module> llvm_module = std::make_unique<Module>("MiniC", *llvm_context);

    FunctionTable fs;
    ProgramCodeGenerator(llvm_context, llvm_module.get(), &fs).generateProgram(node);
    return std::move(llvm_module);
  }

  std::nullptr_t throwError(TOKEN error_token, string const &error_message) {
    fprintf(stderr, "Error: %s [%d:%d]\n", error_message.c_str(), error_token.line_num, error_token.column_num);
    exit(1);
    return nullptr;
  }

}

