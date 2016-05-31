/* File: ast_expr.cc
 * -----------------
 * Implementation of expression node classes.
 */

#include <string.h>
#include "ast_expr.h"
#include "ast_type.h"
#include "ast_decl.h"
#include "symtable.h"


/********** Arithmetic Expr Emit **********/
llvm::Value* ArithmeticExpr::Emit(){
  Operator *op = this->op;

/*********** PRE INCREMENT/DECREMENT ************/
  if( this->left == NULL && this->right != NULL){
    //getting the value at the pointer
    llvm::Value *oldVal = this->right->Emit();

    //getting the pointer of the value
    llvm::LoadInst *l = llvm::cast<llvm::LoadInst>(oldVal);
    llvm::Value *location = l->getPointerOperand();

    //getting the basic block
    llvm::BasicBlock *bb = irgen->GetBasicBlock();

    //INT CASE pre inc/dec
    if( oldVal->getType() == irgen->GetIntType() ){
      llvm::Type* type = irgen->GetIntType();
      llvm::Constant* one = llvm::ConstantInt::get(type,1);
      llvm::Constant* negOne = llvm::ConstantInt::get(type,1, true);

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
      if( op->IsOp("+") ){
        return oldVal;
      }
      if( op->IsOp("-") ){
        //handling negatives
        return llvm::BinaryOperator::CreateMul(oldVal, negOne, "", bb);
      }
    }

    //FLOAT CASE pre inc/dec
    else if( oldVal->getType() == irgen->GetFloatType() ){
      llvm::Type* type = irgen->GetFloatType();
      llvm::Constant* one = llvm::ConstantFP::get(type,1.0);
      llvm::Constant* negOne = llvm::ConstantFP::get(type,-1.0);

      if( op->IsOp("++") ){
        //adding one
        llvm::Value *inc = llvm::BinaryOperator::CreateFAdd(oldVal, one, "", bb);
        llvm::Value* sInst = new llvm::StoreInst(inc, location, bb);
        return inc;
      }
      if( op->IsOp("--") ){
        //subtracting one
        llvm::Value *dec = llvm::BinaryOperator::CreateFSub(oldVal, one, "", bb);
        llvm::Value* sInst = new llvm::StoreInst(dec, location, bb);
        return dec;
      }
      if( op->IsOp("+") ){
        return oldVal;
      }
      if( op->IsOp("-") ){
        return llvm::BinaryOperator::CreateFMul(oldVal, negOne, "", bb);
      }
    }

    //VECTOR CASE pre inc/dec
    else if(oldVal->getType() == irgen->GetType(Type::vec2Type) ||
            oldVal->getType() == irgen->GetType(Type::vec3Type) ||
            oldVal->getType() == irgen->GetType(Type::vec4Type) ){

      llvm::Type* ty = oldVal->getType();
      llvm::Value* vec = llvm::Constant::getNullValue(ty);
      llvm::Value* one = llvm::ConstantFP::get(irgen->GetFloatType(), 1.0);
      int vecNum = 0;

      //getting num of possible elements in the vector
      if( oldVal->getType() == irgen->GetType(Type::vec2Type) ){
        vecNum = 2;
      }
      else if( oldVal->getType() == irgen->GetType(Type::vec3Type) ){
        vecNum = 3;
      }
      else if( oldVal->getType() == irgen->GetType(Type::vec4Type) ){
        vecNum = 4;
      }

      //inserting 1.0 into empty vector "vec"
      for(int i = 0; i < vecNum; i++){
        llvm::Value* num = llvm::ConstantInt::get(irgen->GetIntType(), i);
        vec = llvm::InsertElementInst::Create(vec, one, num, "", bb);
      }

      //adding or subtracting elements in oldVal by 1.0
      llvm::Value* toStore = NULL;
      if(this->op->IsOp("--"))
        toStore = llvm::BinaryOperator::CreateFSub( oldVal, vec, "", bb );
      if(this->op->IsOp("++"))
        toStore = llvm::BinaryOperator::CreateFAdd( oldVal, vec, "", bb );

      //storing vector
      new llvm::StoreInst( toStore, location, bb );

      return toStore;
    }
  }

/************* Standard two piece arithmetic expressions ************/
  //Setup
  llvm::Value *lhs = this->left->Emit();
  llvm::Value *rhs = this->right->Emit();
  llvm::BasicBlock *bb = irgen->GetBasicBlock();

  //dealing with arithmetic between floats 
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
  }
  //dealing with arithmetic between ints only
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
  }
  
  //arithmetic between vector and scalar
  else if( (lhs->getType() == irgen->GetType(Type::floatType)) ||
           (rhs->getType() == irgen->GetType(Type::floatType)) ){
    llvm::Value* vec = NULL;
    llvm::Value* constFP = NULL;
    llvm::Type* ty = NULL;
    int vecNum = 0;
    bool opp = false; //bool to check if its lhs/rhs or rhs/lhs
                      //used in subtraction and division

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
      if( (this->op->IsOp("-") == true) || (this->op->IsOp("/")) )
        opp = true;
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
      if( (this->op->IsOp("-") == true) || (this->op->IsOp("/")) )
        opp = true;
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
      if( (this->op->IsOp("-") == true) || (this->op->IsOp("/")) )      
        opp = true;
    }

    llvm::Value *emptyVec = llvm::Constant::getNullValue(ty); //getting empty vec 

    //looping through elements in vector
    for(int i = 0; i < vecNum; i++){
      llvm::Value* num = llvm::ConstantInt::get(irgen->GetIntType(), i);

      //inserting const element into the empty vector
      emptyVec = llvm::InsertElementInst::Create(emptyVec, constFP, num, "emptyVec", irgen->GetBasicBlock());
    }

    //setting load to vec
    llvm::Value* load = vec;

    //adding between vector and scalar
    if( op->IsOp("+") ){
      return llvm::BinaryOperator::CreateFAdd(emptyVec, load, "", bb);
    }
    //subtracting between vector and scalar
    else if( op->IsOp("-") ){
      if( opp == true){
        opp = false;
        return llvm::BinaryOperator::CreateFSub(emptyVec, load, "", bb);
      }
      return llvm::BinaryOperator::CreateFSub(load, emptyVec, "", bb);
    }
    //multiplying between vector and scalar
    else if( op->IsOp("*") ){
      return llvm::BinaryOperator::CreateFMul(emptyVec, load, "", bb);
    }
    //dividing between vector and scalar
    else if( op->IsOp("/") ){
      if( opp == true){
        opp = false;
        return llvm::BinaryOperator::CreateFDiv(emptyVec, load, "", bb);
      }
      return llvm::BinaryOperator::CreateFDiv(load, emptyVec, "", bb);
    }
  }

  //arithmetic between vectors only
  else if( ((lhs->getType() == irgen->GetType(Type::vec2Type)) &&
           (rhs->getType() == irgen->GetType(Type::vec2Type))) ||
           ((lhs->getType() == irgen->GetType(Type::vec3Type)) &&
           (rhs->getType() == irgen->GetType(Type::vec3Type))) ||
           ((lhs->getType() == irgen->GetType(Type::vec4Type)) &&
           (rhs->getType() == irgen->GetType(Type::vec4Type))) ){
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
  }
  return NULL;
}

/******** Equality Expr Emit **********/
llvm::Value* EqualityExpr::Emit(){
  //Set up
  llvm::Value *lhs = this->left->Emit();
  llvm::Value *rhs = this->right->Emit();
  llvm::BasicBlock *bb = irgen->GetBasicBlock();
 
  //Check float types
  if( (lhs->getType() == irgen->GetType(Type::floatType)) && (rhs->getType() == irgen->GetType(Type::floatType)) ){
    if( this->op->IsOp("==") ){
      return llvm::CmpInst::Create(llvm::CmpInst::FCmp, llvm::FCmpInst::FCMP_OEQ, lhs, rhs, "", bb);
    }
    if( this->op->IsOp("!=") ){
      return llvm::CmpInst::Create(llvm::CmpInst::FCmp, llvm::FCmpInst::FCMP_ONE, lhs, rhs, "", bb);
    }
  }
  
  //Check int types
  if( (lhs->getType() == irgen->GetType(Type::intType)) && (rhs->getType() == irgen->GetType(Type::intType)) ){
    if( this->op->IsOp("==") ){
      return llvm::CmpInst::Create(llvm::CmpInst::ICmp, llvm::ICmpInst::ICMP_EQ, lhs, rhs, "", bb);
    }
    if( this->op->IsOp("!=") ){
      return llvm::CmpInst::Create(llvm::CmpInst::ICmp, llvm::ICmpInst::ICMP_NE, lhs, rhs, "", bb);
    }
  }
  return NULL;
}
  
/********* Relational Expr Emit **********/
llvm::Value* RelationalExpr::Emit(){
  llvm::Value *lhs = this->left->Emit();
  llvm::Value *rhs = this->right->Emit();
  llvm::BasicBlock *bb = irgen->GetBasicBlock();

  //comparing float types
  if( (lhs->getType() == irgen->GetType(Type::floatType)) && (rhs->getType() == irgen->GetType(Type::floatType)) ){
    if( this->op->IsOp(">") ){
      return llvm::CmpInst::Create(llvm::CmpInst::FCmp, llvm::FCmpInst::FCMP_OGT, lhs, rhs, "", bb);
    }
    if( this->op->IsOp("<") ){
      return llvm::CmpInst::Create(llvm::CmpInst::FCmp, llvm::FCmpInst::FCMP_OLT, lhs, rhs, "", bb);
    }
    if( this->op->IsOp(">=") ){
      return llvm::CmpInst::Create(llvm::CmpInst::FCmp, llvm::FCmpInst::FCMP_OGE, lhs, rhs, "", bb);
    }
    if( this->op->IsOp("<=") ){
      return llvm::CmpInst::Create(llvm::CmpInst::FCmp, llvm::FCmpInst::FCMP_OLE, lhs, rhs, "", bb);
    }
  }

  //comparing int types
  if( (lhs->getType() == irgen->GetType(Type::intType)) && (rhs->getType() == irgen->GetType(Type::intType)) ){
    if( this->op->IsOp(">") ){
      return llvm::CmpInst::Create(llvm::CmpInst::ICmp, llvm::ICmpInst::ICMP_SGT, lhs, rhs, "", bb);
    }
    if( this->op->IsOp("<") ){
      return llvm::CmpInst::Create(llvm::CmpInst::ICmp, llvm::ICmpInst::ICMP_SLT, lhs, rhs, "", bb);
    }
    if( this->op->IsOp(">=") ){
      return llvm::CmpInst::Create(llvm::CmpInst::ICmp, llvm::ICmpInst::ICMP_SGE, lhs, rhs, "", bb);
    }
    if( this->op->IsOp("<=") ){
      return llvm::CmpInst::Create(llvm::CmpInst::ICmp, llvm::ICmpInst::ICMP_SLE, lhs, rhs, "", bb);
    }
  }
  return NULL;
}


/********** Conditional Expr Emit *********/
llvm::Value* ConditionalExpr::Emit(){
  //llvm::SelectInst::Create(Value *condition, Value *trueValue, Value *falseValue, const Twine &NameStr, BasicBlock *InsertAtEnd );
  llvm::BasicBlock *bb = irgen->GetBasicBlock();
  //IfStmt* ifstmt = new Ifstmt(this->cond, this->trueExpr, this->falseExpr);
  llvm::Value* cond = this->cond->Emit();
  llvm::Value* trueVal = this->trueExpr->Emit();
  llvm::Value* falseVal = this->falseExpr->Emit();

  //@1524

  return llvm::SelectInst::Create(cond, trueVal, falseVal, "Selection", bb);
}


/********** Logical Expr Emit ***********/
llvm::Value* LogicalExpr::Emit(){
  //llvm::BinaryOperator::CreateAnd(Value *S1, Value *S2, const Twine &Name, BasicBlock *InsertAtEnd)
  //llvm::BinaryOperator::CreateOr(Value *S1, Value *S2, const Twine &Name, BasicBlock *InsertAtEnd)
  //Setup
  llvm::Value *lhs = this->left->Emit();
  llvm::Value *rhs = this->right->Emit();
  llvm::BasicBlock *bb = irgen->GetBasicBlock();

  if( this->op->IsOp("&&") ){
    return llvm::BinaryOperator::CreateAnd(lhs, rhs, "LogicalAnd", bb);
  }
  if( this->op->IsOp("||") ){
    return llvm::BinaryOperator::CreateOr(lhs, rhs, "LogicalOr", bb);
  }
  return NULL;
}


/********* Postfix Expr Emit *********/
llvm::Value* PostfixExpr::Emit(){
  //getting the value at the pointer
  llvm::Value *oldVal = this->left->Emit(); //"loading from pointer"

  //getting the pointer of the value
  llvm::LoadInst *l = llvm::cast<llvm::LoadInst>(oldVal);
  llvm::Value *location = l->getPointerOperand(); //getting pointer?

  //getting the basic block
  llvm::BasicBlock *bb = irgen->GetBasicBlock();

  //INT Post fix
  if( oldVal->getType() == irgen->GetType(Type::intType)){
    //Get a useful '1'
    llvm::Type *intTy = irgen->GetIntType();
    llvm::Value *one = llvm::ConstantInt::get(intTy, 1);
    //post dec
    if( this->op->IsOp("--") ){
      //creating binary op
      llvm::Value *dec = llvm::BinaryOperator::CreateSub(oldVal, one, "", bb);
      //storing new value
      llvm::Value* sInst = new llvm::StoreInst(dec, location, bb);
    }
    if( this->op->IsOp("++") ){
      //creating binary op
      llvm::Value *inc = llvm::BinaryOperator::CreateAdd(oldVal, one, "", bb);
      //storing new value
      llvm::Value* sInst = new llvm::StoreInst(inc, location, bb);
    }
  }
  //FLOAT Post fix
  else if( oldVal->getType() == irgen->GetType(Type::floatType)){
    //Get a useful '1'
    llvm::Type* type = irgen->GetFloatType();
    llvm::Constant* one = llvm::ConstantFP::get(type,1.0);
    //post dec
    if( this->op->IsOp("--") ){
      //creating binary op
      llvm::Value *dec = llvm::BinaryOperator::CreateFSub(oldVal, one, "", bb);
      //storing new value
      llvm::Value* sInst = new llvm::StoreInst(dec, location, bb);
    }
    //Post inc
    if( this->op->IsOp("++") ){
      //creating binary op
      llvm::Value *inc = llvm::BinaryOperator::CreateFAdd(oldVal, one, "", bb);
      //storing new value
      llvm::Value* sInst = new llvm::StoreInst(inc, location, bb);
    }
  }

  //VECTOR Post fix
  else if(oldVal->getType() == irgen->GetType(Type::vec2Type) ||
          oldVal->getType() == irgen->GetType(Type::vec3Type) ||
          oldVal->getType() == irgen->GetType(Type::vec4Type) ){
    llvm::Type* ty = oldVal->getType();
    llvm::Value* vec = llvm::Constant::getNullValue(ty);
    llvm::Value*one = llvm::ConstantFP::get(irgen->GetFloatType(), 1.0);
    int vecNum = 0;

    //getting number of possible elements in vector
    if( oldVal->getType() == irgen->GetType(Type::vec2Type) ){
      vecNum = 2;
    }
    else if( oldVal->getType() == irgen->GetType(Type::vec3Type) ){
      vecNum = 3;
    }
    else if( oldVal->getType() == irgen->GetType(Type::vec4Type) ){
      vecNum = 4;
    }

    //inserting const elmt into empty vector
    for(int i = 0; i < vecNum; i++){
      llvm::Value* num = llvm::ConstantInt::get(irgen->GetIntType(), i);
      vec = llvm::InsertElementInst::Create(vec, one, num, "", bb);
    }

    //adding or subtracting 1.0 with vector
    llvm::Value* toStore = NULL;
    if( this->op->IsOp("--"))
      toStore = llvm::BinaryOperator::CreateFSub( oldVal, vec, "", bb );
    if(this->op->IsOp("++"))
      toStore = llvm::BinaryOperator::CreateFAdd( oldVal, vec, "", bb );
    new llvm::StoreInst( toStore, location, bb );
  }
  return oldVal;
}

/********** Assign Expr Emit **********/
llvm::Value* AssignExpr::Emit(){
  Operator *op = this->op;
  llvm::Value *rVal = this->right->Emit();
  llvm::Value *lVal = this->left->Emit();
  llvm::LoadInst* leftLocation = llvm::cast<llvm::LoadInst>(lVal);

  llvm::ShuffleVectorInst *shuffleLHS = dynamic_cast<llvm::ShuffleVectorInst*>(lVal);
  llvm::ShuffleVectorInst *shuffleRHS = dynamic_cast<llvm::ShuffleVectorInst*>(rVal);

  /********** REGULAR ASSIGNMENT "+" ********/
  if(this->op->IsOp("=")){
    //assignment between two field accesses with more than 1 swizzle
    if( (shuffleLHS != NULL) && (shuffleRHS != NULL) ){
      FieldAccess *f = dynamic_cast<FieldAccess*>(this->left);
      llvm::Value* base = f->getBase()->Emit();
      llvm::LoadInst* l = llvm::cast<llvm::LoadInst>(base);
      char*field = f->getField()->GetName();

      llvm::Value* vec= base;
      int count = 0;
      llvm::Value* leftIdx = NULL;
      llvm::Value* rightIdx = NULL;
      for(char*it = field; *it; ++it){
        if(*it == 'x'){
          leftIdx = llvm::ConstantInt::get(irgen->GetIntType(), 0);
          rightIdx = llvm::ConstantInt::get(irgen->GetIntType(), count);
          llvm::Value* newElmt = llvm::ExtractElementInst::Create( rVal, rightIdx, "", irgen->GetBasicBlock() );
          vec = llvm::InsertElementInst::Create( vec, newElmt, leftIdx, "", irgen->GetBasicBlock() );
        }
        else if(*it == 'y'){
          leftIdx = llvm::ConstantInt::get(irgen->GetIntType(), 1);
          rightIdx = llvm::ConstantInt::get(irgen->GetIntType(), count);
          llvm::Value* newElmt = llvm::ExtractElementInst::Create( rVal, rightIdx, "", irgen->GetBasicBlock() );
          vec = llvm::InsertElementInst::Create( vec, newElmt, leftIdx, "", irgen->GetBasicBlock() );
        }
        else if(*it == 'z'){
          leftIdx = llvm::ConstantInt::get(irgen->GetIntType(), 2);
          rightIdx = llvm::ConstantInt::get(irgen->GetIntType(), count);
          llvm::Value* newElmt = llvm::ExtractElementInst::Create( rVal, rightIdx, "", irgen->GetBasicBlock() );
          vec = llvm::InsertElementInst::Create( vec, newElmt, leftIdx, "", irgen->GetBasicBlock() );
        }
        else if(*it == 'w'){
          leftIdx = llvm::ConstantInt::get(irgen->GetIntType(), 3);
          rightIdx = llvm::ConstantInt::get(irgen->GetIntType(), count);
          llvm::Value* newElmt = llvm::ExtractElementInst::Create( rVal, rightIdx, "", irgen->GetBasicBlock() );
          vec = llvm::InsertElementInst::Create( vec, newElmt, leftIdx, "", irgen->GetBasicBlock() );
        }
        count++;
      }
      new llvm::StoreInst(vec, l->getPointerOperand(), irgen->GetBasicBlock());
    }
    else if( (shuffleLHS != NULL) && 
             ((rVal->getType() == irgen->GetType(Type::vec2Type)) || 
              (rVal->getType() == irgen->GetType(Type::vec3Type)) ||
              (rVal->getType() == irgen->GetType(Type::vec4Type)) )){
      //rhs is a vector
      FieldAccess *f = dynamic_cast<FieldAccess*>(this->left);
      llvm::Value* base = f->getBase()->Emit();
      llvm::LoadInst* l = llvm::cast<llvm::LoadInst>(base);
      char*field = f->getField()->GetName();

      llvm::Value* vec= base;
      int count = 0;
      llvm::Value* leftIdx = NULL;
      llvm::Value* rightIdx = NULL;
      for(char*it = field; *it; ++it){
        if(*it == 'x'){
          leftIdx = llvm::ConstantInt::get(irgen->GetIntType(), 0);
          rightIdx = llvm::ConstantInt::get(irgen->GetIntType(), count);
          llvm::Value* newElmt = llvm::ExtractElementInst::Create( rVal, rightIdx, "", irgen->GetBasicBlock() );
          vec = llvm::InsertElementInst::Create( vec, newElmt, leftIdx, "", irgen->GetBasicBlock() );
        }
        else if(*it == 'y'){
          leftIdx = llvm::ConstantInt::get(irgen->GetIntType(), 1);
          rightIdx = llvm::ConstantInt::get(irgen->GetIntType(), count);
          llvm::Value* newElmt = llvm::ExtractElementInst::Create( rVal, rightIdx, "", irgen->GetBasicBlock() );
          vec = llvm::InsertElementInst::Create( vec, newElmt, leftIdx, "", irgen->GetBasicBlock() );
        }
        else if(*it == 'z'){
          leftIdx = llvm::ConstantInt::get(irgen->GetIntType(), 2);
          rightIdx = llvm::ConstantInt::get(irgen->GetIntType(), count);
          llvm::Value* newElmt = llvm::ExtractElementInst::Create( rVal, rightIdx, "", irgen->GetBasicBlock() );
          vec = llvm::InsertElementInst::Create( vec, newElmt, leftIdx, "", irgen->GetBasicBlock() );
        }
        else if(*it == 'w'){
          leftIdx = llvm::ConstantInt::get(irgen->GetIntType(), 3);
          rightIdx = llvm::ConstantInt::get(irgen->GetIntType(), count);
          llvm::Value* newElmt = llvm::ExtractElementInst::Create( rVal, rightIdx, "", irgen->GetBasicBlock() );
          vec = llvm::InsertElementInst::Create( vec, newElmt, leftIdx, "", irgen->GetBasicBlock() );
        }
        count++;
      }
      new llvm::StoreInst(vec, l->getPointerOperand(), irgen->GetBasicBlock());
    }
    /*else if( dynamic_cast<llvm::ExtractElementInst*>(rVal) != NULL){
         //cout << "WHAT" << endl;
      }*/
    else if( dynamic_cast<llvm::ExtractElementInst*>(lVal) != NULL ){
      //assigning to single swizzle
      FieldAccess *f = dynamic_cast<FieldAccess*>(this->left);
      llvm::Value* base = f->getBase()->Emit();
      llvm::LoadInst* l = llvm::cast<llvm::LoadInst>(base);
      char*field = f->getField()->GetName();
      llvm::Value* vec= base;

      int count = 0;
      llvm::Value* leftIdx = NULL;
      char* it = f->getField()->GetName();
      if(*it == 'x'){
        leftIdx = llvm::ConstantInt::get(irgen->GetIntType(), 0);
        vec = llvm::InsertElementInst::Create( vec,rVal, leftIdx, "", irgen->GetBasicBlock() );
      }
      else if(*it == 'y'){
        leftIdx = llvm::ConstantInt::get(irgen->GetIntType(), 1);
        vec = llvm::InsertElementInst::Create( vec, rVal, leftIdx, "", irgen->GetBasicBlock() );
      }
      else if(*it == 'z'){
        leftIdx = llvm::ConstantInt::get(irgen->GetIntType(), 2);
        vec = llvm::InsertElementInst::Create( vec, rVal, leftIdx, "", irgen->GetBasicBlock() );
      }
      else if(*it == 'w'){
        leftIdx = llvm::ConstantInt::get(irgen->GetIntType(), 3);
        vec = llvm::InsertElementInst::Create( vec, rVal, leftIdx, "", irgen->GetBasicBlock() );
      }
      new llvm::StoreInst(vec, l->getPointerOperand(), irgen->GetBasicBlock());
    }
    else{
      new llvm::StoreInst(rVal, leftLocation->getPointerOperand(), irgen->GetBasicBlock());
    }
    return rVal;
  }
  else if( shuffleLHS != NULL ){
    FieldAccess *f = dynamic_cast<FieldAccess*>(this->left);
    llvm::Value* base = f->getBase()->Emit();
    llvm::LoadInst* l = llvm::cast<llvm::LoadInst>(base);
    char*field = f->getField()->GetName();
    llvm::Value* vec= base;
    int count = 0;
    llvm::Value* leftIdx = NULL;
    llvm::Value* rightIdx = NULL;
    for(char*it = field; *it; ++it){
      llvm::Value* newElmt = NULL;
      if(*it == 'x'){
        leftIdx = llvm::ConstantInt::get(irgen->GetIntType(), 0);
        rightIdx = llvm::ConstantInt::get(irgen->GetIntType(), count);
        newElmt = llvm::ExtractElementInst::Create( lVal, leftIdx, "", irgen->GetBasicBlock() );
      }
      else if(*it == 'y'){
        leftIdx = llvm::ConstantInt::get(irgen->GetIntType(), 1);
        rightIdx = llvm::ConstantInt::get(irgen->GetIntType(), count);
        newElmt = llvm::ExtractElementInst::Create( lVal, leftIdx, "", irgen->GetBasicBlock() );
      }
      else if(*it == 'z'){
        leftIdx = llvm::ConstantInt::get(irgen->GetIntType(), 2);
        rightIdx = llvm::ConstantInt::get(irgen->GetIntType(), count);
        newElmt = llvm::ExtractElementInst::Create( lVal, leftIdx, "", irgen->GetBasicBlock() );
      }
      else if(*it == 'w'){
        leftIdx = llvm::ConstantInt::get(irgen->GetIntType(), 3);
        rightIdx = llvm::ConstantInt::get(irgen->GetIntType(), count);
        newElmt = llvm::ExtractElementInst::Create( lVal, leftIdx, "", irgen->GetBasicBlock() );
      }
      if(this->op->IsOp("*=")){
        llvm::Value* mul = llvm::BinaryOperator::CreateFMul(newElmt, rVal, "", irgen->GetBasicBlock());
        vec = llvm::InsertElementInst::Create(vec, mul, leftIdx, "", irgen->GetBasicBlock() );
      }
      else if(this->op->IsOp("+=")){
        llvm::Value* add = llvm::BinaryOperator::CreateFAdd(newElmt, rVal, "", irgen->GetBasicBlock());
        vec = llvm::InsertElementInst::Create(vec, add, leftIdx, "", irgen->GetBasicBlock());
      }
      else if(this->op->IsOp("-=")){
        llvm::Value* min = llvm::BinaryOperator::CreateFSub(newElmt, rVal, "", irgen->GetBasicBlock());
        vec = llvm::InsertElementInst::Create(vec, min, leftIdx, "", irgen->GetBasicBlock());
      }
      else if(this->op->IsOp("/=")){
        llvm::Value* div = llvm::BinaryOperator::CreateFDiv(newElmt, rVal, "", irgen->GetBasicBlock());
        vec = llvm::InsertElementInst::Create(vec, div, leftIdx, "", irgen->GetBasicBlock());
      }
      count++;
    }
    new llvm::StoreInst(vec, l->getPointerOperand(), irgen->GetBasicBlock());
  }

  //Float assignments
  else if( ((lVal->getType() == irgen->GetType(Type::floatType)) && (rVal->getType() == irgen->GetType(Type::floatType))) ||
           ((lVal->getType() == irgen->GetType(Type::vec2Type)) && (rVal->getType() == irgen->GetType(Type::vec2Type))) ||
           ((lVal->getType() == irgen->GetType(Type::vec3Type)) && (rVal->getType() == irgen->GetType(Type::vec3Type))) ||
           ((lVal->getType() == irgen->GetType(Type::vec4Type)) && (rVal->getType() == irgen->GetType(Type::vec4Type))) ){   

    if( this->op->IsOp("*=") ){
      llvm::Value *mul = llvm::BinaryOperator::CreateFMul(lVal, rVal, "mulequal", irgen->GetBasicBlock());
      if( (dynamic_cast<llvm::ExtractElementInst*>(lVal)!= NULL)){
        FieldAccess *f = dynamic_cast<FieldAccess*>(this->left);
        llvm::Value* base = f->getBase()->Emit();
        llvm::LoadInst* l = llvm::cast<llvm::LoadInst>(base);
        llvm::Value *sInst = new llvm::StoreInst(mul, l->getPointerOperand(), irgen->GetBasicBlock());
      }
      else{
        llvm::Value *sInst = new llvm::StoreInst(mul, leftLocation->getPointerOperand(), irgen->GetBasicBlock());
      }
      return mul;
    }
    else if( this->op->IsOp("+=") ){
      llvm::Value *add = llvm::BinaryOperator::CreateFAdd(lVal, rVal, "plusequal", irgen->GetBasicBlock());
      if( (dynamic_cast<llvm::ExtractElementInst*>(lVal)!= NULL)){
        FieldAccess *f = dynamic_cast<FieldAccess*>(this->left);
        llvm::Value* base = f->getBase()->Emit();
        llvm::LoadInst* l = llvm::cast<llvm::LoadInst>(base);

        char*field = f->getField()->GetName();
        llvm::Value* vec= base;

        int count = 0;
        llvm::Value* leftIdx = NULL;
        char* it = f->getField()->GetName();
        if(*it == 'x'){
          leftIdx = llvm::ConstantInt::get(irgen->GetIntType(), 0);
          vec = llvm::InsertElementInst::Create( vec, add, leftIdx, "", irgen->GetBasicBlock() );
        }
        else if(*it == 'y'){
          leftIdx = llvm::ConstantInt::get(irgen->GetIntType(), 1);
          vec = llvm::InsertElementInst::Create( vec, add, leftIdx, "", irgen->GetBasicBlock() );
        }
        else if(*it == 'z'){
          leftIdx = llvm::ConstantInt::get(irgen->GetIntType(), 2);
          vec = llvm::InsertElementInst::Create( vec, add, leftIdx, "", irgen->GetBasicBlock() );
        }
        else if(*it == 'w'){
          leftIdx = llvm::ConstantInt::get(irgen->GetIntType(), 3);
          vec = llvm::InsertElementInst::Create( vec, add, leftIdx, "", irgen->GetBasicBlock() );
        }
        new llvm::StoreInst(vec, l->getPointerOperand(), irgen->GetBasicBlock());
      }
      else{
        llvm::Value *sInst = new llvm::StoreInst(add, leftLocation->getPointerOperand(), irgen->GetBasicBlock());
      }
      return add;
    }
    else if( this->op->IsOp("-=") ){
      llvm::Value *min = llvm::BinaryOperator::CreateFSub(lVal, rVal, "minusequal", irgen->GetBasicBlock());
      if( (dynamic_cast<llvm::ExtractElementInst*>(lVal)!= NULL) ){
        FieldAccess *f = dynamic_cast<FieldAccess*>(this->left);
        llvm::Value* base = f->getBase()->Emit();
        llvm::LoadInst* l = llvm::cast<llvm::LoadInst>(base);
        char*field = f->getField()->GetName();
        llvm::Value* vec= base;

        int count = 0;
        llvm::Value* leftIdx = NULL;
        char* it = f->getField()->GetName();
        if(*it == 'x'){
          leftIdx = llvm::ConstantInt::get(irgen->GetIntType(), 0);
          vec = llvm::InsertElementInst::Create( vec, min, leftIdx, "", irgen->GetBasicBlock() );
        }
        else if(*it == 'y'){
          leftIdx = llvm::ConstantInt::get(irgen->GetIntType(), 1);
          vec = llvm::InsertElementInst::Create( vec, min, leftIdx, "", irgen->GetBasicBlock() );
        }
        else if(*it == 'z'){
          leftIdx = llvm::ConstantInt::get(irgen->GetIntType(), 2);
          vec = llvm::InsertElementInst::Create( vec, min, leftIdx, "", irgen->GetBasicBlock() );
        }
        else if(*it == 'w'){
          leftIdx = llvm::ConstantInt::get(irgen->GetIntType(), 3);
          vec = llvm::InsertElementInst::Create( vec, min, leftIdx, "", irgen->GetBasicBlock() );
        }
        new llvm::StoreInst(vec, l->getPointerOperand(), irgen->GetBasicBlock());
      }
      else{
        llvm::Value *sInst = new llvm::StoreInst(min, leftLocation->getPointerOperand(), irgen->GetBasicBlock());
      }
      return min;
    }
    else if( this->op->IsOp("/=") ){
      llvm::Value *div = llvm::BinaryOperator::CreateFDiv(lVal, rVal, "divequal", irgen->GetBasicBlock());
      if( (dynamic_cast<llvm::ExtractElementInst*>(lVal)!= NULL) ){
        FieldAccess *f = dynamic_cast<FieldAccess*>(this->left);
        llvm::Value* base = f->getBase()->Emit();
        llvm::LoadInst* l = llvm::cast<llvm::LoadInst>(base);
        char*field = f->getField()->GetName();
        llvm::Value* vec= base;

        int count = 0;
        llvm::Value* leftIdx = NULL;
        char* it = f->getField()->GetName();
        if(*it == 'x'){
          leftIdx = llvm::ConstantInt::get(irgen->GetIntType(), 0);
          vec = llvm::InsertElementInst::Create( vec, div, leftIdx, "", irgen->GetBasicBlock() );
        }
        else if(*it == 'y'){
          leftIdx = llvm::ConstantInt::get(irgen->GetIntType(), 1);
          vec = llvm::InsertElementInst::Create( vec, div, leftIdx, "", irgen->GetBasicBlock() );
        }
        else if(*it == 'z'){
          leftIdx = llvm::ConstantInt::get(irgen->GetIntType(), 2);
          vec = llvm::InsertElementInst::Create( vec, div, leftIdx, "", irgen->GetBasicBlock() );
        }
        else if(*it == 'w'){
          leftIdx = llvm::ConstantInt::get(irgen->GetIntType(), 3);
          vec = llvm::InsertElementInst::Create( vec, div, leftIdx, "", irgen->GetBasicBlock() );
        }
        new llvm::StoreInst(vec, l->getPointerOperand(), irgen->GetBasicBlock());
      }
      else{
        llvm::Value *sInst = new llvm::StoreInst(div, leftLocation->getPointerOperand(), irgen->GetBasicBlock());
      }
      return div;
    }
  }
  //Int assignments
  else if( (lVal->getType() == irgen->GetType(Type::intType)) && (rVal->getType() == irgen->GetType(Type::intType)) ){
    if( this->op->IsOp("*=") ){
      llvm::Value *mul = llvm::BinaryOperator::CreateMul(lVal, rVal, "mulequal", irgen->GetBasicBlock());
      llvm::Value *sInst = new llvm::StoreInst(mul, leftLocation->getPointerOperand(), irgen->GetBasicBlock());
      return mul;
    }

    else if( this->op->IsOp("+=") ){
      llvm::Value *add = llvm::BinaryOperator::CreateAdd(lVal, rVal, "plusequal", irgen->GetBasicBlock());
      llvm::Value *sInst = new llvm::StoreInst(add, leftLocation->getPointerOperand(), irgen->GetBasicBlock());
      return add;
    }
    else if( this->op->IsOp("-=") ){
      llvm::Value *min = llvm::BinaryOperator::CreateSub(lVal, rVal, "minusequal", irgen->GetBasicBlock());
      llvm::Value *sInst = new llvm::StoreInst(min, leftLocation->getPointerOperand(), irgen->GetBasicBlock());
      return min;
    }
    else if( this->op->IsOp("/=") ){
      llvm::Value *div = llvm::BinaryOperator::CreateSDiv(lVal, rVal, "divequal", irgen->GetBasicBlock());
      llvm::Value *sInst = new llvm::StoreInst(div, leftLocation->getPointerOperand(), irgen->GetBasicBlock());
      return div;
    }
  }
  else if( (lVal->getType() == irgen->GetType(Type::floatType)) ||
           (rVal->getType() == irgen->GetType(Type::floatType)) ){
    llvm::Value* vec = NULL;
    llvm::Value* constFP = NULL;
    llvm::Type* ty = NULL;
    int vecNum = 0;
    bool opp = false;

    if(lVal->getType() == irgen->GetType(Type::vec2Type)){
      vecNum = 2;
      vec = lVal;
      constFP = rVal;
      ty = irgen->GetType(Type::vec2Type);
    }
    else if(rVal->getType() == irgen->GetType(Type::vec2Type)){
      vecNum = 2;
      vec = rVal;
      constFP = lVal;
      ty = irgen->GetType(Type::vec2Type);
      if( (this->op->IsOp("-=") == true) || (this->op->IsOp("/=")) )
        opp = true;
    }
    else if(lVal->getType() == irgen->GetType(Type::vec3Type)){
      vecNum = 3;
      vec = lVal;
      constFP = rVal;
      ty = irgen->GetType(Type::vec3Type);
    }
    else if(rVal->getType() == irgen->GetType(Type::vec3Type)){
      vecNum = 3;
      vec = rVal;
      constFP = lVal;
      ty = irgen->GetType(Type::vec3Type);
      if( (this->op->IsOp("-=") == true) || (this->op->IsOp("/=")) )
        opp = true;
    }
    else if(lVal->getType() == irgen->GetType(Type::vec4Type)){
      vecNum = 4;
      vec = lVal;
      constFP = rVal;
      ty = irgen->GetType(Type::vec4Type);
    }
    else if(rVal->getType() == irgen->GetType(Type::vec4Type)){
      vecNum = 4;
      vec = rVal;
      constFP = lVal;
      ty = irgen->GetType(Type::vec4Type);
      if( (this->op->IsOp("-=") == true) || (this->op->IsOp("/=")) )
        opp = true;
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
    if( this->op->IsOp("*=") ){
      llvm::Value *mul = llvm::BinaryOperator::CreateFMul(emptyVec, load, "mulequal", irgen->GetBasicBlock());
      llvm::Value *sInst = new llvm::StoreInst(mul, leftLocation->getPointerOperand(), irgen->GetBasicBlock());
      return mul;
    }
    else if( this->op->IsOp("+=") ){
      llvm::Value *add = llvm::BinaryOperator::CreateFAdd(emptyVec, load, "plusequal", irgen->GetBasicBlock());
      llvm::Value *sInst = new llvm::StoreInst(add, leftLocation->getPointerOperand(), irgen->GetBasicBlock());
      return add;
    }
    else if( this->op->IsOp("-=") ){
      llvm::Value*min;
      if( opp == true){
        opp = false;
        min = llvm::BinaryOperator::CreateFSub(emptyVec, load, "minusequal", irgen->GetBasicBlock() );
      }
      else{
        min = llvm::BinaryOperator::CreateFSub(load, emptyVec, "minusequal", irgen->GetBasicBlock());
      }
      llvm::Value *sInst = new llvm::StoreInst(min, leftLocation->getPointerOperand(), irgen->GetBasicBlock());
      return min;
    }
    else if( this->op->IsOp("/=") ){
      llvm::Value *div;
      if( opp == true){
        opp = false;
        div = llvm::BinaryOperator::CreateFDiv(emptyVec, load, "divequal", irgen->GetBasicBlock() );
      }
      div = llvm::BinaryOperator::CreateFDiv(load, emptyVec, "divequal", irgen->GetBasicBlock());
      llvm::Value *sInst = new llvm::StoreInst(div, leftLocation->getPointerOperand(), irgen->GetBasicBlock());
      return div;
    }
  }

  //assign arith on single swizzle single swizzle
  else if( (dynamic_cast<llvm::ExtractElementInst*>(lVal)!= NULL) && 
           (dynamic_cast<llvm::ExtractElementInst*>(rVal)!=NULL) ){
    if( this->op->IsOp("*=") ){
      llvm::Value *mul = llvm::BinaryOperator::CreateFMul(shuffleLHS, shuffleRHS, "mulequal", irgen->GetBasicBlock());
      llvm::Value *sInst = new llvm::StoreInst(mul, leftLocation->getPointerOperand(), irgen->GetBasicBlock());
      return mul;
    }
    else if( this->op->IsOp("+=") ){
      llvm::Value *add = llvm::BinaryOperator::CreateFAdd(shuffleLHS, shuffleRHS, "plusequal", irgen->GetBasicBlock());
      llvm::Value *sInst = new llvm::StoreInst(add, leftLocation->getPointerOperand(), irgen->GetBasicBlock());
      return add;
    }
    else if( this->op->IsOp("-=") ){
      llvm::Value *min = llvm::BinaryOperator::CreateFSub(shuffleLHS, shuffleRHS, "minusequal", irgen->GetBasicBlock());
      llvm::Value *sInst = new llvm::StoreInst(min, leftLocation->getPointerOperand(), irgen->GetBasicBlock());
      return min;
    }
    else if( this->op->IsOp("/=") ){
      llvm::Value *div = llvm::BinaryOperator::CreateFDiv(shuffleLHS, shuffleRHS, "divequal", irgen->GetBasicBlock());
      llvm::Value *sInst = new llvm::StoreInst(div, leftLocation->getPointerOperand(), irgen->GetBasicBlock());
      return div;
    }
  }
  return NULL;
}

/********** ArrayAccess Emit ***********/
llvm::Value* ArrayAccess::Emit(){
  //llvm::GetElementPtrInst::Create(Value *Ptr, ArrayRef<Value*> IdxList, const Twine &NameStr, BasicBlock *InsertAtEnd);
  //idx = llvm::ConstantInt::get(irgen->GetIntType(), 0);
  std::vector<llvm::Value*> arrayBase;
  arrayBase.push_back(llvm::ConstantInt::get(irgen->GetIntType(), 0));
  arrayBase.push_back(subscript->Emit());
  llvm::Value* arrayElem = llvm::GetElementPtrInst::Create(dynamic_cast<llvm::LoadInst*>(base->Emit())->getPointerOperand(), arrayBase, "", irgen->GetBasicBlock());
  //llvm::Value* lInst = new llvm::LoadInst( v, exprName, irgen->GetBasicBlock() );
  return new llvm::LoadInst(arrayElem, "", irgen->GetBasicBlock());
}

/********* Field Access Emit **********/
llvm::Value* FieldAccess::Emit(){
  if( this->base != NULL ){
    llvm::Value* base = this->base->Emit();
    llvm::BasicBlock* bb = irgen->GetBasicBlock();
    
    std::vector<llvm::Constant*> swizzles;
    if( this->field != NULL ){
      char* f = this->field->GetName();
      llvm::Constant* idx;
      for(char* it = f; *it; ++it){
        if(*it == 'x')
          idx = llvm::ConstantInt::get(irgen->GetIntType(), 0);
        else if(*it == 'y')
          idx = llvm::ConstantInt::get(irgen->GetIntType(), 1);
        else if(*it == 'z')
          idx = llvm::ConstantInt::get(irgen->GetIntType(), 2);
        else if(*it == 'w')
          idx = llvm::ConstantInt::get(irgen->GetIntType(), 3);
        else
          idx = llvm::ConstantInt::get(irgen->GetIntType(), 100);//impossible
        swizzles.push_back(idx);
      }
      if(strlen(f) < 2){
        return llvm::ExtractElementInst::Create(base, idx, "", bb);
      }
      llvm::ArrayRef<llvm::Constant*> swizzleArrayRef(swizzles);
      llvm::Constant* mask = llvm::ConstantVector::get(swizzleArrayRef);
      llvm::Value* newVec = new llvm::ShuffleVectorInst(base, base, mask, "", bb);
      
      return newVec;
    }
  }
  return NULL;
}

/*********** Call Emit ***********/
llvm::Value* Call::Emit(){
  //Used for "Call" expression
  // Func should be the address of the function
  // Args is an ArrayRef<Value*> of the Actuals LLVM::Value
  //llvm::CallInst::Create( Value *Func, ArrayRef<Value*> Args, const Twine &NameStr, BasicBlock *InsertAtEnd );
  //llvm::ArrayRef<llvm::Constant*> swizzleArrayRef(swizzles);
  //idx = llvm::ConstantInt::get(irgen->GetIntType(), 100);
  //std::vector<llvm::Constant*> swizzles;
  llvm::Function* func = (llvm::Function*)symtable->lookup(field->GetName());
  std::vector<llvm::Value*> valActuals;
  for(int x = 0; x < actuals->NumElements(); x++){
    valActuals.push_back(actuals->Nth(x)->Emit());
  }
  
  return llvm::CallInst::Create(func, valActuals, "Call", irgen->GetBasicBlock());
}

/********* Emits for int, float, bool, any Var ***********/
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

