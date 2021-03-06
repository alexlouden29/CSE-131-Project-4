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
    //Set global scope for first chunk
    symtable->globalScope = true;

    //Create Module
    llvm::Module *mod = irgen->GetOrCreateModule("mod.bc");

    //Emit all declarations
    for(int x = 0; x < decls->NumElements(); x++){
        decls->Nth(x)->Emit();
        
    }
    if(irgen->GetBasicBlock() != NULL)
      if(irgen->GetBasicBlock()->getTerminator() == NULL && pred_begin(irgen->GetBasicBlock()) == pred_begin(irgen->GetBasicBlock()))
        new llvm::UnreachableInst(*irgen->GetContext(), irgen->GetBasicBlock());

    //Write to bc file
    //mod->dump();
    llvm::WriteBitcodeToFile(mod, llvm::outs());

    return NULL;
}

/*******  Return Statement Emit *******/
llvm::Value* ReturnStmt::Emit(){
    Expr *e = this -> expr; 
    llvm::BasicBlock *bb = irgen->GetBasicBlock();
    llvm::LLVMContext *context = irgen->GetContext();
    llvm::Function *func = irgen->GetFunction();
    if(func->getReturnType() == irgen->GetVoidType()){
        llvm::ReturnInst* r = llvm::ReturnInst::Create( *context, bb );
        irgen->SetBasicBlock(llvm::BasicBlock::Create(*irgen->GetContext(), "dead", irgen->GetFunction()));
        return r;
    }
    llvm::Value *returnExpr = e->Emit();
    llvm::ReturnInst* r = llvm::ReturnInst::Create( *context, returnExpr, bb);
    irgen->SetBasicBlock(llvm::BasicBlock::Create(*irgen->GetContext(), "dead", irgen->GetFunction()));
    return r;
}

/*******  Stamtement Block Emit *******/
llvm::Value* StmtBlock::Emit(){
    llvm::Value* v = NULL;
    for(int x = 0; x < decls->NumElements(); x++ ){
      VarDecl* decl = decls->Nth(x);
      decl->Emit();
    }
    for(int x = 0; x < stmts->NumElements(); x++ ){
      Stmt* stmt = stmts->Nth(x);
      v = stmt->Emit();
    }
    return NULL;
}

/*****  Decl Statement *****/
llvm::Value* DeclStmt::Emit(){
    VarDecl* varD = (VarDecl*)this->GetDecl();
    llvm::Value* v = varD->Emit();
    if(varD->GetAssignTo() != NULL){
      new llvm::StoreInst (varD->GetAssignTo()->Emit(), v, irgen->GetBasicBlock());
    }
    return v;

}

llvm::Value* ConditionalStmt::Emit(){
    return NULL;
}

//Unecessary?
llvm::Value* LoopStmt::Emit(){
    return NULL;

}

/*********  For Loop IRGen *********/
llvm::Value* ForStmt::Emit(){
    //Push a scope
    symtable->globalScope = false;
    scope s;
    symtable->pushScope(&s);
   
    //llvm Setup
    llvm::Function *f = irgen->GetFunction();
    llvm::LLVMContext *context = irgen->GetContext();

    //creating basic blocks
    llvm::BasicBlock *footerBB = llvm::BasicBlock::Create(*context, "footer", f);
    llvm::BasicBlock *stepBB = llvm::BasicBlock::Create(*context, "step", f);
    llvm::BasicBlock *bodyBB = llvm::BasicBlock::Create(*context, "body", f);
    llvm::BasicBlock *headerBB = llvm::BasicBlock::Create(*context, "header", f);
  
    //Emit Initialization
    init->Emit();

    //creating branch inst to terminate currentBB
    llvm::BranchInst::Create(headerBB, irgen->GetBasicBlock());
    headerBB->moveAfter(irgen->GetBasicBlock());
    irgen->SetBasicBlock(headerBB);

    //Emit for loop test and create conditional loop
    llvm::Value *test = this->test->Emit();
    llvm::BranchInst::Create(bodyBB, footerBB, test, headerBB);

    //Emit body, set branch after body
    irgen->SetBasicBlock(bodyBB);

    //if there's a break or continue in the for loop
    irgen->loopFootBlocks->push(footerBB);
    irgen->footBlocks->push(footerBB);
    irgen->continueBlocks->push(stepBB);

    body->Emit();
    if(bodyBB->getTerminator() == NULL){
      llvm::BranchInst::Create(stepBB, bodyBB);
    }
   
    //Organize step and footer
    stepBB->moveAfter(bodyBB);
    irgen->SetBasicBlock(stepBB);
    step->Emit();
    llvm::BranchInst::Create(headerBB, stepBB);
    footerBB->moveAfter(stepBB);
    
    //Check if footer is empty
    if( pred_begin(footerBB) == pred_end(footerBB)) {
      new llvm::UnreachableInst(*context, footerBB);
    }
    else {
      irgen->SetBasicBlock(footerBB);
      llvm::BasicBlock* pfootBB = irgen->footBlocks->top();
      if(irgen->footBlocks->size() != 0){
        if(pfootBB != footerBB){
          irgen->footBlocks->pop();
          if(pfootBB->getTerminator() == NULL){
            llvm::BranchInst::Create(stepBB, pfootBB);
          }
        }
      }
    }

    //Pop scope
    irgen->loopFootBlocks->pop();
    irgen->footBlocks->pop();
    irgen->continueBlocks->pop();
    symtable->popScope();
    return NULL;   
}

/******** While Loop IRGen **********/
llvm::Value* WhileStmt::Emit(){
  //Add Scope
  symtable->globalScope = false;
  scope s;
  symtable->pushScope(&s);

  //llvm Setup
  llvm::Function *f = irgen->GetFunction();
  llvm::LLVMContext *context = irgen->GetContext();

  //creating basic blocks
  llvm::BasicBlock *footerBB = llvm::BasicBlock::Create(*context, "footer", f);
  llvm::BasicBlock *bodyBB = llvm::BasicBlock::Create(*context, "body", f);
  llvm::BasicBlock *headerBB = llvm::BasicBlock::Create(*context, "header", f);

  //if there's a break or continue in the while loop
  irgen->loopFootBlocks->push(footerBB);
  irgen->footBlocks->push(footerBB);
  irgen->continueBlocks->push(headerBB);

  //creating branch inst to terminate currentBB
  llvm::BranchInst::Create(headerBB, irgen->GetBasicBlock());
  headerBB->moveAfter(irgen->GetBasicBlock());
  irgen->SetBasicBlock(headerBB);

  //Emit for loop test and create conditional loop
  llvm::Value *test = this->test->Emit();
  llvm::BranchInst::Create(bodyBB, footerBB, test, headerBB);

  //Emit body, set branch after body
  irgen->SetBasicBlock(bodyBB);

  body->Emit();
  if(bodyBB->getTerminator() == NULL){
    llvm::BranchInst::Create(headerBB, bodyBB);
  }
   
  //Move footer and check if empty
  footerBB->moveAfter(bodyBB);
  if( pred_begin(footerBB) == pred_end(footerBB)) {
    //Mark empty footer Unreachable
    new llvm::UnreachableInst(*context, footerBB);
  }
  else {
    irgen->SetBasicBlock(footerBB);
    llvm::BasicBlock* pfootBB = irgen->footBlocks->top();
    if(irgen->footBlocks->size() != 0){
      if(pfootBB != footerBB){
        irgen->footBlocks->pop();
        if(pfootBB->getTerminator() == NULL){
          llvm::BranchInst::Create(headerBB, pfootBB);
        }
      }
    }
  }

  //Pop Scope
  irgen->loopFootBlocks->pop();
  irgen->footBlocks->pop();
  irgen->continueBlocks->pop();
  symtable->popScope();
  return NULL;
}

/********* If Statement IRGen ********/
llvm::Value* IfStmt::Emit(){
    //Push Scope
    symtable->globalScope = false;
    scope s;
    symtable->pushScope(&s);
    //Prep
    llvm::Function *f = irgen->GetFunction();
    llvm::LLVMContext *context = irgen->GetContext();

    //Emit conditional test code
    llvm::Value* cond = test->Emit();

    //Create footerBB
    llvm::BasicBlock* footBB = llvm::BasicBlock::Create(*context, "footer", f);
    irgen->footBlocks->push(footBB);
    //Create elseBB
    llvm::BasicBlock* elseBB = llvm::BasicBlock::Create(*context, "else", f);
    
    //Organize the shit show of branches and blocks
    llvm::BasicBlock* thenBB = llvm::BasicBlock::Create(*context, "then", f);
    llvm::BranchInst::Create(thenBB, elseBody ? elseBB:footBB, cond, irgen->GetBasicBlock());
    thenBB->moveAfter(irgen->GetBasicBlock());
    irgen->SetBasicBlock(thenBB);
    body->Emit();
    elseBB->moveAfter(thenBB);

    //Create the terminator if none present
    if( thenBB->getTerminator() == NULL ){
        llvm::BranchInst::Create(footBB, thenBB);
    }
     
    if( elseBody != NULL ){
      irgen->SetBasicBlock(elseBB);
      elseBody->Emit(); 
    }
    footBB->moveAfter(elseBB);

    //check if elseBody is not yet terminated
    if( elseBody != NULL && elseBB -> getTerminator() == NULL ){
      llvm::BranchInst::Create(footBB, elseBB);
    }
    else if( pred_begin(elseBB) == pred_end(elseBB) ){
      new llvm::UnreachableInst(*context, elseBB);
    }

    //Mark empty footer Unreachable
    if( pred_begin(footBB) == pred_end(footBB)) {
      new llvm::UnreachableInst(*context, footBB);
    }
    else {
      irgen->SetBasicBlock(footBB);
      llvm::BasicBlock* pfootBB = irgen->footBlocks->top();
      if(irgen->footBlocks->size() != 0){
        if(pfootBB != footBB){
          irgen->footBlocks->pop();
          if(pfootBB->getTerminator() == NULL){
            llvm::BranchInst::Create(footBB, pfootBB);
          }
          if(irgen->footBlocks->size() == 1)
            irgen->footBlocks->pop();
        }
      }
    }
    
    //Pop scope
    symtable->popScope();
    return NULL;
}

/****** Break Statement *******/
llvm::Value* BreakStmt::Emit(){
  llvm::BasicBlock *currBB = irgen->GetBasicBlock();
  llvm::BranchInst::Create( irgen->loopFootBlocks->top(), currBB );
  irgen->SetBasicBlock(llvm::BasicBlock::Create(*irgen->GetContext(), "dead", irgen->GetFunction()));
  return NULL;
}

/******* Continue Statement Emit *********/
llvm::Value* ContinueStmt::Emit(){
  llvm::BasicBlock *currBB = irgen->GetBasicBlock();
  llvm::BranchInst::Create( irgen->continueBlocks->top(), currBB );
  irgen->SetBasicBlock(llvm::BasicBlock::Create(*irgen->GetContext(), "dead", irgen->GetFunction()));
  return NULL;
}

//Unecessary I think
llvm::Value* SwitchLabel::Emit(){
    return NULL;
}

/******  Switch Case Emit  *******/
llvm::Value* Case::Emit(){
  //Prep
  llvm::Function *f = irgen->GetFunction();
  llvm::LLVMContext *context = irgen->GetContext();
  //Emit
  stmt->Emit();
  return NULL;
}

/******* Switch Default emit ********/
llvm::Value* Default::Emit(){
  //Prep
  llvm::Function *f = irgen->GetFunction();
  llvm::LLVMContext *context = irgen->GetContext();
  //Emit
  stmt->Emit();
  return NULL;
}

/************ Switch Statment IRGen ***********/
llvm::Value* SwitchStmt::Emit(){
  //Push Scope
  symtable->globalScope = false;
  scope s;
  symtable->pushScope(&s);

  //Prep
  llvm::Function *f = irgen->GetFunction();
  llvm::LLVMContext *context = irgen->GetContext();
  
  //Emit Switch Value
  llvm::Value* exp = expr->Emit();

 
  List<llvm::BasicBlock*>* BBList = new List<llvm::BasicBlock*>;
  //List<Stmt*>* caseList = new List<Stmt*>;
  llvm::BasicBlock* defaultBB = llvm::BasicBlock::Create(*context, "default", f);

  //Loop through cases, add BB for each and save both to lists
  for(int x = 0; x < cases->NumElements(); x++){
    if( dynamic_cast<Case*>(cases->Nth(x)) != NULL ){
      llvm::BasicBlock* caseBB = llvm::BasicBlock::Create(*context, "case", f);
      BBList->Append(caseBB);
      //caseList->Append(cases->Nth(x));
      if(dynamic_cast<BreakStmt*>(cases->Nth(x)) != NULL){
        //caseList->Append(cases->Nth(x));
      }
    }
    else if( dynamic_cast<Default*>(cases->Nth(x)) != NULL ){
      BBList->Append(defaultBB);
      //caseList->Append(cases->Nth(x));
    }
  }
  
  //Make basic blocks for cases and default
  llvm::BasicBlock* footBB = llvm::BasicBlock::Create(*context, "footer", f);
  //symtable->breakBlock = footBB;
  irgen->loopFootBlocks->push(footBB);
  irgen->footBlocks->push(footBB);

  //Make Switch Statement
  llvm::SwitchInst* swInst = llvm::SwitchInst::Create(exp, footBB, cases->NumElements(), irgen->GetBasicBlock());


  //Loop through cases and emit
  Stmt* stmt = NULL;
  int BBCount = 0;
  for(int x = 0; x < cases->NumElements(); x++){
    llvm::BasicBlock* currBB = NULL;
    if(BBCount < BBList->NumElements() ){
      currBB = BBList->Nth(BBCount);
    }
    Stmt* s = cases->Nth(x);
    //Emit if case
    if(dynamic_cast<Case*>(s) != NULL){
      irgen->SetBasicBlock(currBB);
      Case* c;
      c = dynamic_cast<Case*>(s);
      llvm::Value* labelVal = c->getLabel()->Emit();
      if(currBB != NULL){
        swInst->addCase(llvm::cast<llvm::ConstantInt>(labelVal), currBB);
      }
      //Prep scope and emit case
      scope s;
      symtable->pushScope(&s);
      c->Emit(); 
      //Ridiculous loop to catch all of the statements in a case
      for(int y = x; y < cases->NumElements(); y++){
        //Catch all of the cases's statements
        if(y+1 < cases->NumElements()){
          if(dynamic_cast<Case*>(cases->Nth(y+1)) == NULL &&
             dynamic_cast<Default*>(cases->Nth(y+1)) == NULL){
            stmt = cases->Nth(y+1);
            if( stmt != NULL){
              stmt->Emit();
            }
          }
          else{
            y = cases->NumElements();
          }
        }
      }
      //Mark empty footer Unreachable
      symtable->popScope();
      BBCount++;
    }
    //Emit if default
    else if( dynamic_cast<Default*>(cases->Nth(x)) != NULL ){
      irgen->SetBasicBlock(currBB);
      //Create default, push scope and emit
      swInst->setDefaultDest(defaultBB);
      scope s;
      symtable->pushScope(&s);
      cases->Nth(x)->Emit();

      //Ridiculous loop to catch all of the statements in a case
      for(int y = x; y < cases->NumElements(); y++){
        //Catch all of the cases's statements
        if(y+1 < cases->NumElements()){
          if(dynamic_cast<Case*>(cases->Nth(y+1)) == NULL &&
             dynamic_cast<Default*>(cases->Nth(y+1)) == NULL){
            stmt = cases->Nth(y+1);
            if( stmt != NULL){
              stmt->Emit();
            }
          }
          else{
            y = cases->NumElements();
            break;
          }
        }
      }
      //Pop scope and increment BB count
      symtable->popScope();
      BBCount++;
    }
    if(currBB != NULL){
      if(currBB->getTerminator() == NULL && dynamic_cast<Case*>(cases->Nth(x)) != NULL){
        llvm::BranchInst::Create(BBList->Nth(BBCount), currBB);
      }
    }
  }

  //Deal with possible default issues, like non-existant/no terminator
  if( pred_begin(defaultBB) == pred_end(defaultBB) ){
    new llvm::UnreachableInst(*context, defaultBB);
  }
  else if( succ_begin(defaultBB) == succ_end(defaultBB) ){
    llvm::BranchInst::Create(footBB, defaultBB);
  }
  
  //Check if footer is Unreachable
  if( pred_begin(footBB) == pred_end(footBB)) {
    new llvm::UnreachableInst(*context, footBB);
  }
  else {
    irgen->SetBasicBlock(footBB);
    if(irgen->footBlocks->size() != 0){
      llvm::BasicBlock* pfootBB = irgen->footBlocks->top();
      if(pfootBB != footBB && pfootBB != NULL){
        irgen->footBlocks->pop();
        if(pfootBB->getTerminator() == NULL){
          llvm::BranchInst::Create(footBB, pfootBB);
        }
        if(irgen->footBlocks->size() == 1)
          irgen->footBlocks->pop();
      }
    }
  }
  //Pop scope
  irgen->loopFootBlocks->pop();
  symtable->popScope();
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

