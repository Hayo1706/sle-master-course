module Resolve
extend lang::std::Id;

import Syntax;
import IO;
/*
 * Name resolution for QL
 */ 


// modeling declaring occurrences of names
alias Def = rel[str name, loc def];

// modeling use occurrences of names
alias Use = rel[loc use, str name];

alias UseDef = rel[loc use, loc def];

// the reference graph
alias RefGraph = tuple[
  Use uses, 
  Def defs, 
  UseDef useDef
]; 

RefGraph resolve(start[Form] f) = <us, ds, us o ds> when Use us := uses(f), Def ds := defs(f);

Use uses(start[Form] f) {
  Use use = {};
  visit(f){
    case (Expr) `<Expr e>`:
      if (e is var){
         use = use + <e.name.src, "<e.name>">;
      }

  }
  return use;
}


Def defs(start[Form] f) {
  Def def = {};
  visit(f){
    case (Question) `<Question q>`:
      if (q is answerable || q is computed){
         def = def + <"<q.name>", q.name.src>;
      }

  }
  return def;
}

// import Syntax;
// import ParseTree;
// start[Form] a = parse(#start[Form], |project://sle-master-course/examples/tax.myql|);
//resolve(a)
// start[Tests] a = parse(#start[Tests], |project://sle-master-course/src/checktests.testql|);
// start[Tests] b = parse(#start[Tests], |project://sle-master-course/src/evaltests.testql|);