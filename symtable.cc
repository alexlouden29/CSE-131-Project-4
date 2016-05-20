/*
 * Symbol table implementation
 *
 */
#include "symtable.h"
#include "ast.h"
#include "ast_type.h"
#include "ast_decl.h"

SymbolTable::SymbolTable(){

  map<string, llvm::Value*> s1;

  vector<scope> scopess;
  scopes = scopess;
  scopes.push_back(s1);

  ifFlag = false;
  elseFlag = false;
  elifFlag = false;
  whileFlag = false;
  forFlag = false;
  globalScope = false;
  funcFlag = false;
  doWhileFlag = false;
  switchFlag = false;
  breakFlag = false;
  returnFlag = false;
  caseFlag = false;
  defaultFlag = false;
  Type *returnType = NULL;
}

void SymbolTable::pushScope(scope *s){
  scopes.push_back(*s);
}

void SymbolTable::popScope(){
  scopes.pop_back();
}

void SymbolTable::addSymbol(string key, llvm::Value* val){
  scope* m = &scopes.back();
  m->insert(pair<string,llvm::Value*>(key,val));
}

llvm::Value* SymbolTable::lookup(string key){
  llvm::Value* v = NULL;
  for(vector<scope>::reverse_iterator vectorIt = scopes.rbegin(); vectorIt != scopes.rend(); ++vectorIt){
    scope* s = &(*vectorIt);
    v = lookupInScope(key, s);
    if(v != NULL){
      break;
    }
  }
  return v;
}

llvm::Value* SymbolTable::lookupInScope(string key, scope *s){
  int i = s->count(key);
  if(i > 0){
    return s->at(key);
  }
  return NULL;
}

scope* SymbolTable::currScope(){
  if (&scopes.back() == NULL){
  }
  return &(scopes.back());
}

int SymbolTable::size(){
  return scopes.size();
}
