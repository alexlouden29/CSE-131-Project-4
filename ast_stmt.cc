/* File: ast_stmt.cc
 * -----------------
 * Implementation of statement node classes.
 */
#include "ast_stmt.h"
#include "ast_type.h"
#include "ast_decl.h"
#include "ast_expr.h"
#include "symtable.h"

#include "irgen.h"
#include "llvm/Bitcode/ReaderWriter.h"
#include "llvm/Support/raw_ostream.h"                                                   



Program::Program(List<Decl*> *d) {
    Assert(d != NULL);
    (decls=d)->SetParentAll(this);
}

void Program::PrintChildren(int indentLevel) {
    decls->PrintAll(indentLevel+1);
    printf("\n");
}

llvm::Value* Program::Emit() {
    // TODO:
    // This is just a reference for you to get started
    //
    // You can use this as a template and create Emit() function
    // for individual node to fill in the module structure and instructions.
    //
    symtable->globalScope = true;
    llvm::Module *mod = irgen->GetOrCreateModule("mod.bc");
    for(int x = 0; x < decls->NumElements(); x++){
        decls->Nth(x)->Emit();
        
    }
    llvm::WriteBitcodeToFile(mod, llvm::outs());

    return NULL;
    /*
    IRGenerator irgen;
    llvm::Module *mod = irgen.GetOrCreateModule("Name_the_Module.bc");

    // create a function signature
    std::vector<llvm::Type *> argTypes;
    llvm::Type *intTy = irgen.GetIntType();
    argTypes.push_back(intTy);
    llvm::ArrayRef<llvm::Type *> argArray(argTypes);
    llvm::FunctionType *funcTy = llvm::FunctionType::get(intTy, argArray, false);

    // llvm::Function *f = llvm::cast<llvm::Function>(mod->getOrInsertFunction("foo", intTy, intTy, (Type *)0));
    llvm::Function *f = llvm::cast<llvm::Function>(mod->getOrInsertFunction("Name_the_function", funcTy));
    llvm::Argument *arg = f->arg_begin();
    arg->setName("x");

    // insert a block into the runction
    llvm::LLVMContext *context = irgen.GetContext();
    llvm::BasicBlock *bb = llvm::BasicBlock::Create(*context, "entry", f);

    // create a return instruction
    llvm::Value *val = llvm::ConstantInt::get(intTy, 1);
    llvm::Value *sum = llvm::BinaryOperator::CreateAdd(arg, val, "", bb);
    llvm::ReturnInst::Create(*context, sum, bb);

    // write the BC into standard output
    llvm::WriteBitcodeToFile(mod, llvm::outs());
    */
}


llvm::Value* ReturnStmt::Emit(){
    Expr *e = this -> expr; 
    llvm::BasicBlock *bb = irgen->GetBasicBlock();
    llvm::LLVMContext *context = irgen->GetContext();
    llvm::Value *returnExpr = e->Emit();
    return llvm::ReturnInst::Create( *context, returnExpr, bb);
}

llvm::Value* StmtBlock::Emit(){
    llvm::Value* v = NULL;
    for(int x = 0; x < decls->NumElements(); x++ ){
      VarDecl* decl = decls->Nth(x);
      decl->Emit();
    }
    for(int x = 0; x < stmts->NumElements(); x++ ){
      Stmt* stmt = stmts->Nth(x);
      v = stmt->Emit();
      //if(v
    }
    return NULL;
}



llvm::Value* DeclStmt::Emit(){
    llvm::Value* v = this->GetDecl()->Emit();
    return v;

}

llvm::Value* ConditionalStmt::Emit(){
    return NULL;

}

llvm::Value* LoopStmt::Emit(){
    return NULL;

}

llvm::Value* ForStmt::Emit(){
    symtable->globalScope = false;
    scope s;
    symtable->pushScope(&s);
    llvm::Function *f = irgen->GetFunction();
    llvm::LLVMContext *context = irgen->GetContext();

    //creating basic blocks
    llvm::BasicBlock *footerBB = llvm::BasicBlock::Create(*context, "footer", f);
    llvm::BasicBlock *stepBB = llvm::BasicBlock::Create(*context, "step", f);
    llvm::BasicBlock *bodyBB = llvm::BasicBlock::Create(*context, "body", f);
    llvm::BasicBlock *headerBB = llvm::BasicBlock::Create(*context, "header", f);
  
    //Emit Initialization
    llvm::Value *init = this->init->Emit();

    //creating branch inst to terminate currentBB
    llvm::BranchInst::Create(headerBB, irgen->GetBasicBlock());
    irgen->SetBasicBlock(headerBB);

    //llvm::BranchInst::Create(BasicBlock *IfTrue, BasicBlock *IfFalse, Value *Cond, BasicBlock *InsertAtEnd) 
    llvm::Value *test = this->test->Emit();
    llvm::BranchInst::Create(footerBB, bodyBB, test, headerBB);

    irgen->SetBasicBlock(bodyBB);
   
    /*if(irgen->GetBasicBlock()->getSingleSuccessor() == bodyBB){
      irgen->SetBasicBlock(bodyBB);
      llvm::Value *body = this->body->Emit();
    }
    else if(irgen->GetBasicBlock()->getSingleSuccessor() == footerBB){
      llvm::BranchInst::Create(footerBB, irgen->GetBasicBlock());
      irgen->SetBasicBlock(footerBB);
      return bodyBB->getTerminator();
    }*/

    if(bodyBB->getTerminator() != NULL){
      llvm::BranchInst::Create(footerBB, bodyBB);
      irgen->SetBasicBlock(footerBB);
      return bodyBB->getTerminator();
    }

    llvm::BranchInst::Create(stepBB, bodyBB);
    irgen->SetBasicBlock(stepBB);
    llvm::Value *step = this->step->Emit();
    llvm::BranchInst::Create(headerBB, stepBB);
    irgen->SetBasicBlock(headerBB);
    return NULL;   
}

llvm::Value* WhileStmt::Emit(){
    return NULL;
}

llvm::Value* IfStmt::Emit(){
     //test, body, elsebody, thenbody = body
    //llvm::BasicBlock *footerBB = llvm::BasicBlock::Create(*context, "footer", f);
    //llvm::BasicBlock *stepBB = llvm::BasicBlock::Create(*context, "step", f);
    //llvm::BasicBlock *bodyBB = llvm::BasicBlock::Create(*context, "body", f);
    symtable->globalScope = false;
    scope s;
    symtable->pushScope(&s);
    llvm::Function *f = irgen->GetFunction();
    llvm::LLVMContext *context = irgen->GetContext();

    llvm::Value* cond = test->Emit();
    llvm::BasicBlock* footBB = llvm::BasicBlock::Create(*context, "footer", f);

    llvm::BasicBlock* elseBB;
    if( elseBody != NULL ){
      elseBB = llvm::BasicBlock::Create(*context, "else", f);
    }
    
    llvm::BasicBlock* thenBB = llvm::BasicBlock::Create(*context, "then", f);
    llvm::BranchInst::Create(thenBB, elseBody ? elseBB:footBB, cond, irgen->GetBasicBlock());
    thenBB->moveAfter(irgen->GetBasicBlock());
    irgen->SetBasicBlock(thenBB);
    body->Emit();
    elseBB->moveAfter(thenBB);
    llvm::BranchInst::Create(footBB, thenBB);
    irgen->SetBasicBlock(elseBB);
    elseBody->Emit();
    footBB->moveAfter(elseBB);
    llvm::BranchInst::Create(footBB, elseBB);
    irgen->SetBasicBlock(footBB);
    
    return NULL;
}

llvm::Value* BreakStmt::Emit(){
    return NULL;
}

llvm::Value* ContinueStmt::Emit(){
    return NULL;
}

llvm::Value* SwitchLabel::Emit(){
    return NULL;
}

llvm::Value* Case::Emit(){
    return NULL;
}

llvm::Value* Default::Emit(){
    return NULL;
}

llvm::Value* SwitchStmt::Emit(){
    return NULL;
}




StmtBlock::StmtBlock(List<VarDecl*> *d, List<Stmt*> *s) {
    Assert(d != NULL && s != NULL);
    (decls=d)->SetParentAll(this);
    (stmts=s)->SetParentAll(this);
}

void StmtBlock::PrintChildren(int indentLevel) {
    decls->PrintAll(indentLevel+1);
    stmts->PrintAll(indentLevel+1);
}

DeclStmt::DeclStmt(Decl *d) {
    Assert(d != NULL);
    (decl=d)->SetParent(this);
}

void DeclStmt::PrintChildren(int indentLevel) {
    decl->Print(indentLevel+1);
}

ConditionalStmt::ConditionalStmt(Expr *t, Stmt *b) { 
    Assert(t != NULL && b != NULL);
    (test=t)->SetParent(this); 
    (body=b)->SetParent(this);
}

ForStmt::ForStmt(Expr *i, Expr *t, Expr *s, Stmt *b): LoopStmt(t, b) { 
    Assert(i != NULL && t != NULL && b != NULL);
    (init=i)->SetParent(this);
    step = s;
    if ( s )
      (step=s)->SetParent(this);
}

void ForStmt::PrintChildren(int indentLevel) {
    init->Print(indentLevel+1, "(init) ");
    test->Print(indentLevel+1, "(test) ");
    if ( step )
      step->Print(indentLevel+1, "(step) ");
    body->Print(indentLevel+1, "(body) ");
}

void WhileStmt::PrintChildren(int indentLevel) {
    test->Print(indentLevel+1, "(test) ");
    body->Print(indentLevel+1, "(body) ");
}

IfStmt::IfStmt(Expr *t, Stmt *tb, Stmt *eb): ConditionalStmt(t, tb) { 
    Assert(t != NULL && tb != NULL); // else can be NULL
    elseBody = eb;
    if (elseBody) elseBody->SetParent(this);
}

void IfStmt::PrintChildren(int indentLevel) {
    if (test) test->Print(indentLevel+1, "(test) ");
    if (body) body->Print(indentLevel+1, "(then) ");
    if (elseBody) elseBody->Print(indentLevel+1, "(else) ");
}


ReturnStmt::ReturnStmt(yyltype loc, Expr *e) : Stmt(loc) { 
    expr = e;
    if (e != NULL) expr->SetParent(this);
}

void ReturnStmt::PrintChildren(int indentLevel) {
    if ( expr ) 
      expr->Print(indentLevel+1);
}

SwitchLabel::SwitchLabel(Expr *l, Stmt *s) {
    Assert(l != NULL && s != NULL);
    (label=l)->SetParent(this);
    (stmt=s)->SetParent(this);
}

SwitchLabel::SwitchLabel(Stmt *s) {
    Assert(s != NULL);
    label = NULL;
    (stmt=s)->SetParent(this);
}

void SwitchLabel::PrintChildren(int indentLevel) {
    if (label) label->Print(indentLevel+1);
    if (stmt)  stmt->Print(indentLevel+1);
}

SwitchStmt::SwitchStmt(Expr *e, List<Stmt *> *c, Default *d) {
    Assert(e != NULL && c != NULL && c->NumElements() != 0 );
    (expr=e)->SetParent(this);
    (cases=c)->SetParentAll(this);
    def = d;
    if (def) def->SetParent(this);
}

void SwitchStmt::PrintChildren(int indentLevel) {
    if (expr) expr->Print(indentLevel+1);
    if (cases) cases->PrintAll(indentLevel+1);
    if (def) def->Print(indentLevel+1);
}

