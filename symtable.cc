/*
 * Symbol table implementation
 *
 */
#include "symtable.h"
#include "ast.h"
#include "ast_type.h"
#include "ast_decl.h"

SymbolTable::SymbolTable(){

  llvm::Value* llvmValue = NULL;

  map<string, Decl*> s1;

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

void SymbolTable::addSymbol(string key, Decl* decl){
  scope* m = &scopes.back();
  m->insert(pair<string,Decl*>(key,decl));
}

Decl* SymbolTable::lookup(string key){
  Decl* d = NULL;
  for(vector<scope>::reverse_iterator vectorIt = scopes.rbegin(); vectorIt != scopes.rend(); ++vectorIt){
    scope* s = &(*vectorIt);
    d = lookupInScope(key, s);
    if(d != NULL){
      break;
    }
  }
  return d;
}

Decl* SymbolTable::lookupInScope(string key, scope *s){
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
