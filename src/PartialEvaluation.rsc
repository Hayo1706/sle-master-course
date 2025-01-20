module PartialEvaluation

import Syntax;
import Eval;
import Flatten;
import String;
import IO;
import Boolean;
import util::Math;
 /*
  * Partial evaluation:
  *  evaluate a questionnaire to another questionnaire with partial knowledge 
  *  as represented by the VEnv argument.
  *   - if conditions evaluate to true or false eliminate if-constructs
  *   - if variables are in VEnv evaluate them to values
  *   - expressions with value operands should be evaluated 
  *  Use eval where needed. 
  */


start[Form] peval(start[Form] form, VEnv venv) = unflatten(peval(flatten(form), venv), form);

list[Question] peval(list[Question] qs, VEnv venv) {
    return ([] | it + peval(q, venv) | q <- qs);
} 


// NB: this function returns a list of questions
// so that if conditionals disappear you can return the empty list.
list[Question] peval(Question q, VEnv venv) {
    list[Question] qs = [];
    switch(q){
        case (Question)`if ( <Expr cond> ) <Question then>`:{
            if (q.then is computed)
                q.then.expr = peval(q.then.expr, venv);

            q.cond = peval(q.cond, venv);
            if("<q.cond>" == "true")
                qs += q.then;
            else if ("<q.cond>" != "false")
                qs += q;
        }
        default:
            qs += q;
    }
    return qs;

}

// Expr peval(e:(Expr)`<Id x>`, VEnv venv){ 
//     if ("<x>" in venv){
//         Value v = venv["<x>"];
//         return value2expr(v);
//     }
//     else{
//         return e;
//     }
// }
Expr peval(e:(Expr)`<Id x>`, VEnv venv) = value2expr(venv["<x>"]) when "<x>" in venv;

//Expr peval((Expr)`<Id x>`, VEnv venv) = iexp when "<x>" in venv, Value v := venv["<x>"], Expr iexp := [Expr]"<v>";

// Expr peval(e:[Expr]"true", VEnv venv) {
//     return e;
// }
// // Expr peval(e:(Expr)`false`, VEnv venv) = e;
// //Expr peval(e:(Expr)`<Int i>`, VEnv venv) = e;

Expr peval((Expr)`<Expr lhs> + <Expr rhs>`, VEnv venv) = iexp when (Expr)`<Int i1>` := peval(lhs, venv), (Expr)`<Int i2>` := peval(rhs, venv), int i := toInt("<i1>") + toInt("<i2>"), Expr iexp := [Expr]"<i>";
Expr peval((Expr)`<Expr lhs> - <Expr rhs>`, VEnv venv) = iexp when (Expr)`<Int i1>` := peval(lhs, venv), (Expr)`<Int i2>` := peval(rhs, venv), int i := toInt("<i1>") - toInt("<i2>"), Expr iexp := [Expr]"<i>";
Expr peval((Expr)`<Expr lhs> * <Expr rhs>`, VEnv venv) = iexp when (Expr)`<Int i1>` := peval(lhs, venv), (Expr)`<Int i2>` := peval(rhs, venv), int i := toInt("<i1>") * toInt("<i2>"), Expr iexp := [Expr]"<i>";
Expr peval((Expr)`<Expr lhs> / <Expr rhs>`, VEnv venv) = iexp when (Expr)`<Int i1>` := peval(lhs, venv), (Expr)`<Int i2>` := peval(rhs, venv), int i := toInt("<i1>") / toInt("<i2>"), Expr iexp := [Expr]"<i>";
Expr peval((Expr)`<Expr lhs> ^ <Expr rhs>`, VEnv venv) = iexp when (Expr)`<Int i1>` := peval(lhs, venv), (Expr)`<Int i2>` := peval(rhs, venv), real i := pow(toInt("<i1>"), toInt("<i2>")), Expr iexp := [Expr]"<i>";


Expr peval((Expr)`<Expr lhs> && <Expr rhs>`, VEnv venv) = iexp when (Expr)`<Bool i1>` := peval(lhs, venv), (Expr)`<Bool i2>` := peval(rhs, venv), bool i := fromString("<i1>") && fromString("<i2>"), Expr iexp := [Expr]"<i>";
Expr peval((Expr)`<Expr lhs> || <Expr rhs>`, VEnv venv) = iexp when (Expr)`<Bool i1>` := peval(lhs, venv), (Expr)`<Bool i2>` := peval(rhs, venv), bool i := (fromString("<i1>") || fromString("<i2>")), Expr iexp := [Expr]"<i>";
Expr peval((Expr)`<Expr lhs> == <Expr rhs>`, VEnv venv) = iexp when (Expr)`<Bool i1>` := peval(lhs, venv), (Expr)`<Bool i2>` := peval(rhs, venv), bool i := fromString("<i1>") == fromString("<i2>"), Expr iexp := [Expr]"<i>";
Expr peval((Expr)`<Expr lhs> != <Expr rhs>`, VEnv venv) = iexp when (Expr)`<Bool i1>` := peval(lhs, venv), (Expr)`<Bool i2>` := peval(rhs, venv), bool i := fromString("<i1>") != fromString("<i2>"), Expr iexp := [Expr]"<i>";

Expr peval((Expr)`<Expr lhs> \> <Expr rhs>`, VEnv venv) = iexp when (Expr)`<Bool i1>` := peval(lhs, venv), (Expr)`<Bool i2>` := peval(rhs, venv), bool i := fromString("<i1>") > fromString("<i2>"), Expr iexp := [Expr]"<i>";
Expr peval((Expr)`<Expr lhs> \< <Expr rhs>`, VEnv venv) = iexp when (Expr)`<Bool i1>` := peval(lhs, venv), (Expr)`<Bool i2>` := peval(rhs, venv), bool i := fromString("<i1>") < fromString("<i2>"), Expr iexp := [Expr]"<i>";
Expr peval((Expr)`<Expr lhs> \>= <Expr rhs>`, VEnv venv) = iexp when (Expr)`<Bool i1>` := peval(lhs, venv), (Expr)`<Bool i2>` := peval(rhs, venv), bool i := fromString("<i1>") >= fromString("<i2>"), Expr iexp := [Expr]"<i>";
Expr peval((Expr)`<Expr lhs> \>= <Expr rhs>`, VEnv venv) = iexp when (Expr)`<Bool i1>` := peval(lhs, venv), (Expr)`<Bool i2>` := peval(rhs, venv), bool i := fromString("<i1>") <= fromString("<i2>"), Expr iexp := [Expr]"<i>";
Expr peval((Expr)`! <Expr operand>`, VEnv venv) = iexp when (Expr)`<Bool rhs>` := peval(operand, venv), bool i := !fromString("<rhs>"), Expr iexp := [Expr]"<i>";



// Expr peval((Expr)`<Expr lhs> + <Expr rhs>`, VEnv venv) = (Expr)`<Expr e1> + <Expr e2>` when Expr e1 := peval(lhs, venv), Expr e2 := peval(rhs, venv);

Expr peval(Expr e, VEnv venv) = e;

