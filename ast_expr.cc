/* File: ast_expr.cc
 * -----------------
 * Implementation of expression node classes.
 */

#include <string.h>
#include "ast_expr.h"
#include "ast_type.h"
#include "ast_decl.h"
#include "symtable.h"


llvm::Value* ArithmeticExpr::Emit(){
  Operator *op = this->op;

  //pre inc
  if( this->left == NULL && this->right != NULL){
      //getting the value at the pointer
      llvm::Value *oldVal = this->right->Emit();

      //getting the pointer of the value
      llvm::LoadInst *l = llvm::cast<llvm::LoadInst>(oldVal);
      llvm::Value *location = l->getPointerOperand();

      //getting the basic block
      llvm::BasicBlock *bb = irgen->GetBasicBlock();

      llvm::Type *type = NULL;
      llvm::Value *one = NULL;
      if( oldVal->getType() == irgen->GetType(Type::intType) ){
        type = irgen->GetIntType();
        one = llvm::ConstantInt::get(type,1);
      }
      else if( oldVal->getType() == irgen->GetType(Type::floatType) ){
        type = irgen->GetFloatType();
        one = llvm::ConstantFP::get(type,1);
      }

      if( op->IsOp("++") ){
        //adding one
        llvm::Value *inc = llvm::BinaryOperator::CreateAdd(oldVal, one, "", bb);
        llvm::Value* sInst = new llvm::StoreInst(inc, location, bb);
        return inc;
      }
      if( op->IsOp("--") ){
        //subtracting one
        llvm::Value *dec = llvm::BinaryOperator::CreateSub(oldVal, one, "", bb);
        llvm::Value* sInst = new llvm::StoreInst(dec, location, bb);
        return dec;
      }
  }

  llvm::Value *lhs = this->left->Emit();
  llvm::Value *rhs = this->right->Emit();
  llvm::BasicBlock *bb = irgen->GetBasicBlock();

  //dealing with arithmetic between floats and 
  if( lhs->getType() == irgen->GetType(Type::floatType) &&
      rhs->getType() == irgen->GetType(Type::floatType) ){
    if( op->IsOp("+") ){
      return llvm::BinaryOperator::CreateFAdd(lhs, rhs, "", bb);
    }
    else if( op->IsOp("-") ){
      return llvm::BinaryOperator::CreateFSub(lhs, rhs, "", bb);
    }
    else if( op->IsOp("*") ){
      return llvm::BinaryOperator::CreateFMul(lhs, rhs, "", bb);
    }
    else if( op->IsOp("/") ){
      return llvm::BinaryOperator::CreateFDiv(lhs, rhs, "", bb);
    }
    else{
      return NULL;
    }      
  }
  //dealing with ints only
  else if( lhs->getType() == irgen->GetType(Type::intType) && 
           rhs->getType() == irgen->GetType(Type::intType) ){
    if( op->IsOp("+") ){
      return llvm::BinaryOperator::CreateAdd(lhs, rhs, "", bb);
    }
    else if( op->IsOp("-") ){
      return llvm::BinaryOperator::CreateSub(lhs, rhs, "", bb);
    }
    else if( op->IsOp("*") ){
      return llvm::BinaryOperator::CreateMul(lhs, rhs, "", bb);
    }
    else if( op->IsOp("/") ){
      return llvm::BinaryOperator::CreateSDiv(lhs, rhs, "", bb);
    }
    else{
      return NULL;
    }
  }
  
  //arithmetic between vector and scalar
  else if( (lhs->getType() == irgen->GetType(Type::floatType)) ||
           (rhs->getType() == irgen->GetType(Type::floatType)) ){
    llvm::Value* vec = NULL;
    llvm::Value* constFP = NULL;
    llvm::Type* ty = NULL;
    int vecNum = 0;

    if(lhs->getType() == irgen->GetType(Type::vec2Type)){
      vecNum = 2;
      vec = lhs;
      constFP = rhs;
      ty = irgen->GetType(Type::vec2Type);
    }
    else if(rhs->getType() == irgen->GetType(Type::vec2Type)){
      vecNum = 2;
      vec = rhs;
      constFP = lhs;
      ty = irgen->GetType(Type::vec2Type);
    }
    else if(lhs->getType() == irgen->GetType(Type::vec3Type)){
      vecNum = 3;
      vec = lhs;
      constFP = rhs;
      ty = irgen->GetType(Type::vec3Type);
    }
    else if(rhs->getType() == irgen->GetType(Type::vec3Type)){
      vecNum = 3;
      vec = rhs;
      constFP = lhs;
      ty = irgen->GetType(Type::vec3Type);
    }
    else if(lhs->getType() == irgen->GetType(Type::vec4Type)){
      vecNum = 4;
      vec = lhs;
      constFP = rhs;
      ty = irgen->GetType(Type::vec4Type);
    }
    else if(rhs->getType() == irgen->GetType(Type::vec4Type)){
      vecNum = 4;
      vec = rhs;
      constFP = lhs;
      ty = irgen->GetType(Type::vec4Type);
    }
    llvm::Value *emptyVec = llvm::Constant::getNullValue(ty); //getting empty vec2Type 

    //loading vector and making it a vector type
    llvm::VectorType* vector = dynamic_cast<llvm::VectorType*>(vec);

    //looping through elements in vector
    for(int i = 0; i < vecNum; i++){
      llvm::Value* num = llvm::ConstantInt::get(irgen->GetIntType(), i);
      //inserting element into the empty vector
      emptyVec = llvm::InsertElementInst::Create(emptyVec, constFP, num, "emptyVec", irgen->GetBasicBlock());
    }
    llvm::Value* load = vec;

    //adding between vector and scalar
    if( op->IsOp("+") ){
      return llvm::BinaryOperator::CreateFAdd(emptyVec, load, "", bb);
    }
    //subtracting between vector and scalar
    else if( op->IsOp("-") ){
      return llvm::BinaryOperator::CreateFSub(emptyVec, load, "", bb);
    }
    //multiplying between vector and scalar
    else if( op->IsOp("*") ){
      return llvm::BinaryOperator::CreateFMul(emptyVec, load, "", bb);
    }
    //dividing between vector and scalar
    else if( op->IsOp("/") ){
      return llvm::BinaryOperator::CreateFDiv(emptyVec, load, "", bb);
    }
    else{
      return NULL;
    }
  }
  return NULL;
}

llvm::Value* RelationalExpr::Emit(){
  llvm::Value *lhs = this->left->Emit();
  llvm::Value *rhs = this->right->Emit();
  llvm::BasicBlock *bb = irgen->GetBasicBlock();
  Operator *op = this->op;

  //comparing float types
  if( (lhs->getType() == irgen->GetType(Type::floatType)) && (rhs->getType() == irgen->GetType(Type::floatType)) ){
    if( op->IsOp(">") ){
      return llvm::CmpInst::Create(llvm::CmpInst::FCmp, llvm::FCmpInst::FCMP_OGT, lhs, rhs, "", bb);
    }
    if( op->IsOp("<") ){
      return llvm::CmpInst::Create(llvm::CmpInst::FCmp, llvm::FCmpInst::FCMP_OLT, lhs, rhs, "", bb);
    }
    if( op->IsOp(">=") ){
      return llvm::CmpInst::Create(llvm::CmpInst::FCmp, llvm::FCmpInst::FCMP_OGE, lhs, rhs, "", bb);
    }
    if( op->IsOp("<=") ){
      return llvm::CmpInst::Create(llvm::CmpInst::FCmp, llvm::FCmpInst::FCMP_OLE, lhs, rhs, "", bb);
    }
    if( op->IsOp("==") ){
      return llvm::CmpInst::Create(llvm::CmpInst::FCmp, llvm::FCmpInst::FCMP_OEQ, lhs, rhs, "", bb);
    }
    if( op->IsOp("!=") ){
      return llvm::CmpInst::Create(llvm::CmpInst::FCmp, llvm::FCmpInst::FCMP_ONE, lhs, rhs, "", bb);
    }
  }

  //comparing int types
  if( (lhs->getType() == irgen->GetType(Type::intType)) && (rhs->getType() == irgen->GetType(Type::intType)) ){
    if( op->IsOp(">") ){
      return llvm::CmpInst::Create(llvm::CmpInst::ICmp, llvm::ICmpInst::ICMP_SGT, lhs, rhs, "", bb);
    }
    if( op->IsOp("<") ){
      return llvm::CmpInst::Create(llvm::CmpInst::ICmp, llvm::ICmpInst::ICMP_SLT, lhs, rhs, "", bb);
    }
    if( op->IsOp(">=") ){
      return llvm::CmpInst::Create(llvm::CmpInst::ICmp, llvm::ICmpInst::ICMP_SGE, lhs, rhs, "", bb);
    }
    if( op->IsOp("<=") ){
      return llvm::CmpInst::Create(llvm::CmpInst::ICmp, llvm::ICmpInst::ICMP_SLE, lhs, rhs, "", bb);
    }
    if( op->IsOp("==") ){
      return llvm::CmpInst::Create(llvm::CmpInst::ICmp, llvm::ICmpInst::ICMP_EQ, lhs, rhs, "", bb);
    }
    if( op->IsOp("!=") ){
      return llvm::CmpInst::Create(llvm::CmpInst::ICmp, llvm::ICmpInst::ICMP_NE, lhs, rhs, "", bb);
    }
  }
  return NULL;
}

llvm::Value* PostfixExpr::Emit(){
    //getting the value at the pointer
    llvm::Value *oldVal = this->left->Emit(); //"loading from pointer"

    //getting the pointer of the value
    llvm::LoadInst *l = llvm::cast<llvm::LoadInst>(oldVal);
    llvm::Value *location = l->getPointerOperand(); //getting pointer?

    //getting the basic block
    llvm::BasicBlock *bb = irgen->GetBasicBlock();

    //Get a useful '1'
    llvm::Type *intTy = irgen->GetIntType();
    llvm::Value *one = llvm::ConstantInt::get(intTy, 1);


    Operator *op = this->op;
    //Int Post fix
    if( oldVal->getType() == irgen->GetType(Type::intType)){
        //post dec
        if( op->IsOp("--") ){
            //creating binary op
            llvm::Value *dec = llvm::BinaryOperator::CreateSub(oldVal, one, "", bb);
            //storing new value
            llvm::Value* sInst = new llvm::StoreInst(dec, location, bb);
        }
        if( op->IsOp("++") ){
            //creating binary op
            llvm::Value *dec = llvm::BinaryOperator::CreateAdd(oldVal, one, "", bb);
            //storing new value
            llvm::Value* sInst = new llvm::StoreInst(dec, location, bb);
        }
    }
    //Float Post fix
    else if( oldVal->getType() == irgen->GetType(Type::floatType)){
        //post dec
        if( op->IsOp("--") ){
            //creating binary op
            llvm::Value *dec = llvm::BinaryOperator::CreateFSub(oldVal, one, "", bb);
            //storing new value
            llvm::Value* sInst = new llvm::StoreInst(dec, location, bb);
        }
        if( op->IsOp("++") ){
            //creating binary op
            llvm::Value *dec = llvm::BinaryOperator::CreateFAdd(oldVal, one, "", bb);
            //storing new value
            llvm::Value* sInst = new llvm::StoreInst(dec, location, bb);
        }
    }
    //TODO: Why?
    return oldVal;
}

llvm::Value* AssignExpr::Emit(){
    Operator *op = this->op;
    llvm::Value *lVal = this->left->Emit();
    llvm::LoadInst* leftLocation = llvm::cast<llvm::LoadInst>(lVal);
    llvm::Value *rVal = this->right->Emit();

    //Regular assignment
    if(op->IsOp("=")){
        llvm::Value* sInst = new llvm::StoreInst(rVal, leftLocation->getPointerOperand(), irgen->GetBasicBlock());
    }
    //Float assignments
    else if( ((lVal->getType() == irgen->GetType(Type::floatType)) && (rVal->getType() == irgen->GetType(Type::floatType))) /*||
      ((lVal->getType() == irgen->GetType(Type::vec2Type)) && (rVal->getType() == irgen->GetType(Type::vec2Type)))*/ ){
      if( op->IsOp("*=") ){
        llvm::Value *mul = llvm::BinaryOperator::CreateFMul(lVal, rVal, "mulequal", irgen->GetBasicBlock());
        llvm::Value *sInst = new llvm::StoreInst(mul, leftLocation->getPointerOperand(), irgen->GetBasicBlock());
      }
      else if( op->IsOp("+=") ){
        llvm::Value *add = llvm::BinaryOperator::CreateFAdd(lVal, rVal, "plusequal", irgen->GetBasicBlock());
        llvm::Value *sInst = new llvm::StoreInst(add, leftLocation->getPointerOperand(), irgen->GetBasicBlock());
      }
      else if( op->IsOp("-=") ){
        llvm::Value *min = llvm::BinaryOperator::CreateFSub(lVal, rVal, "minusequal", irgen->GetBasicBlock());
        llvm::Value *sInst = new llvm::StoreInst(min, leftLocation->getPointerOperand(), irgen->GetBasicBlock());
      }
      else if( op->IsOp("/=") ){
        llvm::Value *div = llvm::BinaryOperator::CreateFDiv(lVal, rVal, "divequal", irgen->GetBasicBlock());
        llvm::Value *sInst = new llvm::StoreInst(div, leftLocation->getPointerOperand(), irgen->GetBasicBlock());
      }
    }
    //Int assignments
    else if( (lVal->getType() == irgen->GetType(Type::intType)) && (rVal->getType() == irgen->GetType(Type::intType)) ){
      if( op->IsOp("*=") ){
        llvm::Value *mul = llvm::BinaryOperator::CreateMul(lVal, rVal, "mulequal", irgen->GetBasicBlock());
        llvm::Value *sInst = new llvm::StoreInst(mul, leftLocation->getPointerOperand(), irgen->GetBasicBlock());
      }
      else if( op->IsOp("+=") ){
        llvm::Value *add = llvm::BinaryOperator::CreateAdd(lVal, rVal, "plusequal", irgen->GetBasicBlock());
        llvm::Value *sInst = new llvm::StoreInst(add, leftLocation->getPointerOperand(), irgen->GetBasicBlock());
      }
      else if( op->IsOp("-=") ){
        llvm::Value *min = llvm::BinaryOperator::CreateSub(lVal, rVal, "minusequal", irgen->GetBasicBlock());
        llvm::Value *sInst = new llvm::StoreInst(min, leftLocation->getPointerOperand(), irgen->GetBasicBlock());
      }
      else if( op->IsOp("/=") ){
        llvm::Value *div = llvm::BinaryOperator::CreateSDiv(lVal, rVal, "divequal", irgen->GetBasicBlock());
        llvm::Value *sInst = new llvm::StoreInst(div, leftLocation->getPointerOperand(), irgen->GetBasicBlock());
      }
    }
    return leftLocation;
}

//Array Access
llvm::Value* ArrayAccess::Emit(){
  //llvm::GetElementPtrInst::Create(Value *Ptr, ArrayRef<Value*> IdxList, const Twine &NameStr, BasicBlock *InsertAtEnd);
  //llvm::GetElementPtrInst::Create(
  return NULL;
}

//Field Acess for Functions
llvm::Value* FieldAccess::Emit(){
  if( this->base != NULL){
    llvm::Value* base = this->base->Emit();
    llvm::BasicBlock* bb = irgen->GetBasicBlock();
    //llvm::LoadInst *l = llvm::cast<llvm::LoadInst>(base);
    //llvm::Value *location = l->getPointerOperand();

    llvm::Value *idx = NULL;
    string x = "x", y = "y", z = "z", w = "w";
    //for
    if(this->field->GetName() == x){
      idx = llvm::ConstantInt::get(irgen->GetIntType(), 0);
    }
    else if(this->field->GetName() == y){
      idx = llvm::ConstantInt::get(irgen->GetIntType(), 1);
    }
    else if(this->field->GetName() == z){
      idx = llvm::ConstantInt::get(irgen->GetIntType(), 2);
    }
    else if(this->field->GetName() == w){
      idx = llvm::ConstantInt::get(irgen->GetIntType(), 3);
    }
    //if (idx == NULL){
    //  cout << "fnjdls" << endl;
    //}
    
    llvm::Value *v = llvm::ExtractElementInst::Create(base, idx, "", bb);
    return v;
  }
  return NULL;
}

llvm::Value* IntConstant::Emit(){
    llvm::Type *intTy = irgen->GetIntType();
    return llvm::ConstantInt::get(intTy, this->value);
}

llvm::Value* FloatConstant::Emit(){
    llvm::Type *fTy = irgen->GetFloatType();
    return llvm::ConstantFP::get(fTy, this->value);
}

llvm::Value* BoolConstant::Emit(){
    llvm::Type *bTy = irgen->GetBoolType();

    // true == 1, false == 0
    return llvm::ConstantInt::get(bTy, this->value); 
}

llvm::Value* VarExpr::Emit(){
    string exprName = this->GetIdentifier()->GetName();
    llvm::Value* v = symtable->lookup(exprName);
    llvm::Value* lInst = new llvm::LoadInst( v, exprName, irgen->GetBasicBlock() );
    return lInst;
}

IntConstant::IntConstant(yyltype loc, int val) : Expr(loc) {
    value = val;
}
void IntConstant::PrintChildren(int indentLevel) { 
    printf("%d", value);
}

FloatConstant::FloatConstant(yyltype loc, double val) : Expr(loc) {
    value = val;
}
void FloatConstant::PrintChildren(int indentLevel) { 
    printf("%g", value);
}

BoolConstant::BoolConstant(yyltype loc, bool val) : Expr(loc) {
    value = val;
}
void BoolConstant::PrintChildren(int indentLevel) { 
    printf("%s", value ? "true" : "false");
}

VarExpr::VarExpr(yyltype loc, Identifier *ident) : Expr(loc) {
    Assert(ident != NULL);
    this->id = ident;
}

void VarExpr::PrintChildren(int indentLevel) {
    id->Print(indentLevel+1);
}

Operator::Operator(yyltype loc, const char *tok) : Node(loc) {
    Assert(tok != NULL);
    strncpy(tokenString, tok, sizeof(tokenString));
}

void Operator::PrintChildren(int indentLevel) {
    printf("%s",tokenString);
}

bool Operator::IsOp(const char *op) const {
    return strcmp(tokenString, op) == 0;
}

CompoundExpr::CompoundExpr(Expr *l, Operator *o, Expr *r) 
  : Expr(Join(l->GetLocation(), r->GetLocation())) {
    Assert(l != NULL && o != NULL && r != NULL);
    (op=o)->SetParent(this);
    (left=l)->SetParent(this); 
    (right=r)->SetParent(this);
}

CompoundExpr::CompoundExpr(Operator *o, Expr *r) 
  : Expr(Join(o->GetLocation(), r->GetLocation())) {
    Assert(o != NULL && r != NULL);
    left = NULL; 
    (op=o)->SetParent(this);
    (right=r)->SetParent(this);
}

CompoundExpr::CompoundExpr(Expr *l, Operator *o) 
  : Expr(Join(l->GetLocation(), o->GetLocation())) {
    Assert(l != NULL && o != NULL);
    (left=l)->SetParent(this);
    (op=o)->SetParent(this);
}

void CompoundExpr::PrintChildren(int indentLevel) {
   if (left) left->Print(indentLevel+1);
   op->Print(indentLevel+1);
   if (right) right->Print(indentLevel+1);
}
   
ConditionalExpr::ConditionalExpr(Expr *c, Expr *t, Expr *f)
  : Expr(Join(c->GetLocation(), f->GetLocation())) {
    Assert(c != NULL && t != NULL && f != NULL);
    (cond=c)->SetParent(this);
    (trueExpr=t)->SetParent(this);
    (falseExpr=f)->SetParent(this);
}

void ConditionalExpr::PrintChildren(int indentLevel) {
    cond->Print(indentLevel+1, "(cond) ");
    trueExpr->Print(indentLevel+1, "(true) ");
    falseExpr->Print(indentLevel+1, "(false) ");
}
ArrayAccess::ArrayAccess(yyltype loc, Expr *b, Expr *s) : LValue(loc) {
    (base=b)->SetParent(this); 
    (subscript=s)->SetParent(this);
}

void ArrayAccess::PrintChildren(int indentLevel) {
    base->Print(indentLevel+1);
    subscript->Print(indentLevel+1, "(subscript) ");
}
     
FieldAccess::FieldAccess(Expr *b, Identifier *f) 
  : LValue(b? Join(b->GetLocation(), f->GetLocation()) : *f->GetLocation()) {
    Assert(f != NULL); // b can be be NULL (just means no explicit base)
    base = b; 
    if (base) base->SetParent(this); 
    (field=f)->SetParent(this);
}


void FieldAccess::PrintChildren(int indentLevel) {
    if (base) base->Print(indentLevel+1);
    field->Print(indentLevel+1);
}

Call::Call(yyltype loc, Expr *b, Identifier *f, List<Expr*> *a) : Expr(loc)  {
    Assert(f != NULL && a != NULL); // b can be be NULL (just means no explicit base)
    base = b;
    if (base) base->SetParent(this);
    (field=f)->SetParent(this);
    (actuals=a)->SetParentAll(this);
}

void Call::PrintChildren(int indentLevel) {
   if (base) base->Print(indentLevel+1);
   if (field) field->Print(indentLevel+1);
   if (actuals) actuals->PrintAll(indentLevel+1, "(actuals) ");
}

