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
        if( op->IsOp("++") ){
            //getting the value at the pointer
            llvm::Value *oldVal = this->right->Emit(); //"loading from pointer"

            //getting the pointer of the value
            llvm::LoadInst *l = llvm::cast<llvm::LoadInst>(oldVal);
            llvm::Value *location = l->getPointerOperand(); //getting pointer?

            //getting the basic block
            llvm::BasicBlock *bb = irgen->GetBasicBlock();

            llvm::Type *intTy = irgen->GetIntType();
            llvm::Value *one = llvm::ConstantInt::get(intTy, 1);

            //adding one
            llvm::Value *inc = llvm::BinaryOperator::CreateAdd(oldVal, one, "", bb);
            llvm::Value* sInst = new llvm::StoreInst(inc, location, bb);
            
            return inc;
        }
    }
    if( op->IsOp("+") ){
        llvm::BasicBlock *bb = irgen->GetBasicBlock();
        llvm::Value *rhs = this->right->Emit();
        llvm::Value *lhs = this->left->Emit();

        llvm::Value *sum = llvm::BinaryOperator::CreateAdd(lhs, rhs, "", bb);
        return sum;
    }
    return NULL;
}

llvm::Value* RelationalExpr::Emit(){
    llvm::Value *lhs = this->left->Emit();
    llvm::Value *rhs = this->right->Emit();
    llvm::BasicBlock *bb = irgen->GetBasicBlock();
    if( (lhs->getType() == irgen->GetType(Type::floatType)) && (rhs->getType() == irgen->GetType(Type::floatType)) ){
        Operator *op = this->op;
        if( op->IsOp(">") ){
            return llvm::CmpInst::Create(llvm::CmpInst::FCmp, llvm::FCmpInst::FCMP_OGT, lhs, rhs, "", bb);
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

    llvm::Type *intTy = irgen->GetIntType();
    llvm::Value *one = llvm::ConstantInt::get(intTy, 1);


    Operator *op = this->op;

    if( oldVal->getType() == irgen->GetType(Type::intType)){
        //post dec
        if( op->IsOp("--") ){
            //creating binary op
            llvm::Value *dec = llvm::BinaryOperator::CreateSub(oldVal, one, "", bb);
            //storing new value
            llvm::Value* sInst = new llvm::StoreInst(dec, location, bb);
        }
    }

    return oldVal;
    //llvm::Value *oldVal = new llvm::LoadInst( lhs, this->GetIdentifier()->GetName(), irgen->GetBasicBlock() );
}

llvm::Value* AssignExpr::Emit(){
    Operator *op = this->op;
    llvm::Value *lVal = this->left->Emit();
    llvm::LoadInst* leftLocation = llvm::cast<llvm::LoadInst>(lVal);
    llvm::Value *rVal = this->right->Emit();

    if(op->IsOp("=")){
        llvm::Value* sInst = new llvm::StoreInst(rVal, leftLocation->getPointerOperand(), irgen->GetBasicBlock());
    }
    else if( op->IsOp("*=") ){
        //dealing with floats
        if( rVal->getType() == irgen->GetFloatType() ){
            llvm::Value *mul = llvm::BinaryOperator::CreateFMul(lVal, rVal);
            llvm::Value *sInst = new llvm::StoreInst(mul, leftLocation->getPointerOperand(), irgen->GetBasicBlock());
        }
    }
    else if( op->IsOp("+=") ){
        if( rVal->getType() == irgen->GetIntType() ){
            llvm::Value *add = llvm::BinaryOperator::CreateAdd(lVal, rVal);
            llvm::Value *sInst = new llvm::StoreInst(add, leftLocation->getPointerOperand(), irgen->GetBasicBlock());
        }
    }
    return leftLocation;
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

