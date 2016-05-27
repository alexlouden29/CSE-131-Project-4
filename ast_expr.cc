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
      llvm::Value *oldVal = this->right->Emit(); //"loading from pointer"

      //getting the pointer of the value
      llvm::LoadInst *l = llvm::cast<llvm::LoadInst>(oldVal);
      llvm::Value *location = l->getPointerOperand(); //getting pointer?

      //getting the basic block
      llvm::BasicBlock *bb = irgen->GetBasicBlock();

      llvm::Type *intTy = irgen->GetIntType();
      llvm::Value *one = llvm::ConstantInt::get(intTy, 1);

      if( op->IsOp("++") ){
        //adding one
        llvm::Value *inc = llvm::BinaryOperator::CreateAdd(oldVal, one, "", bb);
        llvm::Value* sInst = new llvm::StoreInst(inc, location, bb);
        return inc;
      }
      if( op->IsOp("--") ){
        //adding one
        llvm::Value *inc = llvm::BinaryOperator::CreateSub(oldVal, one, "", bb);
        llvm::Value* sInst = new llvm::StoreInst(inc, location, bb);
        return inc;
      }
    }
    if( op->IsOp("+") ){
      //adding float and scalar value
      //if(){
      //  llvm::Value *v = llvm::Constant::getNullValue(ty); //getting NULL vec2Type
      //
      //}
      //else{
        llvm::BasicBlock *bb = irgen->GetBasicBlock();
        llvm::Value *rhs = this->right->Emit();
        llvm::Value *lhs = this->left->Emit();

        llvm::Value *sum = llvm::BinaryOperator::CreateAdd(lhs, rhs, "", bb);
        return sum;
      //}
    }
    else if( op->IsOp("*") ){
      //multiplying float and scalar value
      llvm::Value *lhs = this->left->Emit();
      llvm::Value *rhs = this->right->Emit();
      llvm::Value* vec;
      llvm::Value* constFP;
      if(lhs->getType() == irgen->GetType(Type::vec2Type)){
        vec = lhs;
        constFP = rhs;
      }
      else if(rhs->getType() == irgen->GetType(Type::vec2Type)){
        vec = rhs;
        constFP = lhs;
      }
      else{
        //cout << "BOOGERS" << endl;
        vec = NULL;
        constFP = NULL;
      }
      if( vec != NULL ) {
        
        llvm::Type *ty = irgen->GetType(Type::vec2Type);
        llvm::Value *v = llvm::Constant::getNullValue(ty); //getting empty vec2Type  
        //loading vector and making it a vector type
        llvm::LoadInst* lInst = llvm::cast<llvm::LoadInst>(vec);
        llvm::VectorType* vector = llvm::cast<llvm::VectorType>(lInst);
        
        //looping through elements in vector
        for(int i = 0; i < vector->getNumElements(); i++){
          llvm::Value* num = llvm::ConstantInt::get(irgen->GetIntType(), i);

          //getting the element at index
          llvm::Value* elem = llvm::ExtractElementInst::Create(vec, num);

          //multiplying element with constFP
          llvm::Value* mult = llvm::BinaryOperator::CreateFMul(elem, constFP);

          //inserting element into the empty vector
          llvm::InsertElementInst::Create(v, mult, num, "", irgen->GetBasicBlock());
        }
      
        llvm::Value* ptr = lInst->getPointerOperand();

        //storing new vector to where the old vector was I think
        llvm::Value* sInst = new llvm::StoreInst(v, ptr, irgen->GetBasicBlock());
        return v;
/*
        for(int i = 0; i < vector->getNumElements(); i++){
          llvm::Value *index = llvm::ConstantInt::get(irgen->GetIntType(), i);
          llvm::InsertElementInst::Create(v, constFP, index, "", irgen->GetBasicBlock());
        }
        llvm::Value * mult = llvm::BinaryOperator::CreateMul(v, vec);
        llvm::Value* sInst = new llvm::StoreInst(v, vec, irgen->GetBasicBlock());
*/
      }
      else{
        llvm::BasicBlock *bb = irgen->GetBasicBlock();
        llvm::Value *rhs = this->right->Emit();
        llvm::Value *lhs = this->left->Emit();
        llvm::Value *mul = llvm::BinaryOperator::CreateMul(lhs, rhs, "", bb);
        return mul;
      }
    }
        return NULL;
}

llvm::Value* RelationalExpr::Emit(){
    llvm::Value *lhs = this->left->Emit();
    llvm::Value *rhs = this->right->Emit();
    llvm::BasicBlock *bb = irgen->GetBasicBlock();
    Operator *op = this->op;
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
    else if( (lVal->getType() == irgen->GetType(Type::floatType)) && (rVal->getType() == irgen->GetType(Type::floatType)) ){
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
        llvm::LoadInst *l = llvm::cast<llvm::LoadInst>(base);
        llvm::Value *location = l->getPointerOperand();

        llvm::Value *idx;
        string x = "x";
        int i;
        if(this->field->GetName() == x){
            idx = llvm::ConstantInt::get(irgen->GetIntType(), 0);
        }
        else{
            idx = llvm::ConstantInt::get(irgen->GetIntType(), 1);
        }
        llvm::Value *v = llvm::ExtractElementInst::Create(base, idx, "", irgen->GetBasicBlock());
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

