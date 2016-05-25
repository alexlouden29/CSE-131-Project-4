/**
 * File: symtable.h
 * ----------- 
 *  Header file for Symbol table implementation.
 */

#ifndef _H_symtable
#define _H_symtable

#include <vector>
#include <map>
#include <string>
#include "ast_decl.h"

#include "ast.h"
#include "llvm/IR/Value.h"

#include "irgen.h"

using namespace std;
typedef map<string, llvm::Value*> scope;

class SymbolTable {
  protected:
    vector<scope> scopes;

  public:
    SymbolTable(); //constructor

    llvm::BasicBlock* breakBlock;

    bool ifFlag, elseFlag, elifFlag;
    bool whileFlag, forFlag;
    bool globalScope;
    bool funcFlag;
    bool doWhileFlag; 
    bool switchFlag, caseFlag, defaultFlag;
    bool breakFlag;
    bool returnFlag;
    Type *returnType;

    void pushScope(scope *s);
    void popScope();
    void addSymbol(string key, llvm::Value* val);
    llvm::Value* lookup(string key);
    scope* currScope();
    llvm::Value* lookupInScope(string key, scope *s);
    int size();
   
};


#endif

