/* irgen.cc -  LLVM IR generator
 *
 * You can implement any LLVM related functions here.
 */

#include "irgen.h"
llvm::Type *IRGenerator::GetType(Type* astTy) const
{
  llvm::Type *ty = NULL;
  if ( astTy == Type::intType ) {
    ty = IRGenerator::GetIntType();
  } 
  else if ( astTy == Type::boolType ) {
    ty = IRGenerator::GetBoolType();
  } 
  else if ( astTy == Type::floatType ) {
    ty = IRGenerator::GetFloatType();
  }
  else if (astTy == Type::vec2Type ){
    ty = llvm::VectorType::get(llvm::Type::getFloatTy(*context), 2);
  }
  else if (astTy == Type::vec3Type ){
    ty = llvm::VectorType::get(llvm::Type::getFloatTy(*context), 3);
  }
  else if (astTy == Type::vec4Type ){
    ty = llvm::VectorType::get(llvm::Type::getFloatTy(*context), 4);
  }
  else if (astTy == Type::voidType ){
    ty = IRGenerator::GetVoidType();
  }
  else if (dynamic_cast<ArrayType*>(astTy) != NULL ){
    //get (Type *ElementType, uint64_t NumElements)
    ArrayType* arrayTy = dynamic_cast<ArrayType*>(astTy);
    ty = llvm::ArrayType::get(GetType(arrayTy->GetElemType()), arrayTy->GetElemCount());
  }
  return ty;
}


IRGenerator::IRGenerator() :
    context(NULL),
    module(NULL),
    currentFunc(NULL),
    currentBB(NULL)
{
  footBlocks = new std::stack<llvm::BasicBlock*>;
  loopFootBlocks = new std::stack<llvm::BasicBlock*>;
  continueBlocks = new std::stack<llvm::BasicBlock*>;
}

IRGenerator::~IRGenerator() {
}

llvm::Module *IRGenerator::GetOrCreateModule(const char *moduleID)
{
   if ( module == NULL ) {
     context = new llvm::LLVMContext();
     module  = new llvm::Module(moduleID, *context);
     module->setTargetTriple(TargetTriple);
     module->setDataLayout(TargetLayout);
   }
   return module;
}

void IRGenerator::SetFunction(llvm::Function *func) {
   currentFunc = func;
}

llvm::Function *IRGenerator::GetFunction() const {
   return currentFunc;
}

void IRGenerator::SetBasicBlock(llvm::BasicBlock *bb) {
   if(currentBB != NULL)
     if(pred_begin(currentBB) == pred_end(currentBB) && currentBB->getTerminator() ==NULL)
       new llvm::UnreachableInst(*context, currentBB);
   currentBB = bb;
}

llvm::BasicBlock *IRGenerator::GetBasicBlock() const {
   return currentBB;
}

llvm::Type *IRGenerator::GetIntType() const {
   llvm::Type *ty = llvm::Type::getInt32Ty(*context);
   return ty;
}

llvm::Type *IRGenerator::GetBoolType() const {
   llvm::Type *ty = llvm::Type::getInt1Ty(*context);
   return ty;
}

llvm::Type *IRGenerator::GetFloatType() const {
   llvm::Type *ty = llvm::Type::getFloatTy(*context);
   return ty;
}

llvm::Type *IRGenerator::GetVoidType() const {
   llvm::Type *ty = llvm::Type::getVoidTy(*context);
   return ty;
}

const char *IRGenerator::TargetLayout = "e-p:64:64:64-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f32:32:32-f64:64:64-v64:64:64-v128:128:128-a0:0:64-s0:64:64-f80:128:128-n8:16:32:64-S128";

const char *IRGenerator::TargetTriple = "x86_64-redhat-linux-gnu";

