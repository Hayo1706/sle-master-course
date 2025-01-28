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
    q = bottom-up visit(q){
        case (Expr) e => peval(e, venv)       
    }

    if (q is ifThen ){
        if("<q.cond>" == "true")
            return [q.then];
        else if ("<q.cond>" != "false")
            return [q];
    }
    else
        return [q];
    return [];
}

Expr peval(e:(Expr)`<Id x>`, VEnv venv) = value2expr(venv["<x>"]) when "<x>" in venv;

Expr peval((Expr)`<Expr lhs> + <Expr rhs>`, VEnv venv) = iexp when (Expr)`<Int i1>` := peval(lhs, venv), (Expr)`<Int i2>` := peval(rhs, venv), int i := toInt("<i1>") + toInt("<i2>"), Expr iexp := [Expr]"<i>";
Expr peval((Expr)`<Expr lhs> - <Expr rhs>`, VEnv venv) = iexp when (Expr)`<Int i1>` := peval(lhs, venv), (Expr)`<Int i2>` := peval(rhs, venv), int i := toInt("<i1>") - toInt("<i2>"), Expr iexp := [Expr]"<i>";
Expr peval((Expr)`<Expr lhs> * <Expr rhs>`, VEnv venv) = iexp when (Expr)`<Int i1>` := peval(lhs, venv), (Expr)`<Int i2>` := peval(rhs, venv), int i := toInt("<i1>") * toInt("<i2>"), Expr iexp := [Expr]"<i>";
Expr peval((Expr)`<Expr lhs> / <Expr rhs>`, VEnv venv) = iexp when (Expr)`<Int i1>` := peval(lhs, venv), (Expr)`<Int i2>` := peval(rhs, venv), int i := toInt("<i1>") / toInt("<i2>"), Expr iexp := [Expr]"<i>";
Expr peval((Expr)`<Expr lhs> ^ <Expr rhs>`, VEnv venv) = iexp when (Expr)`<Int i1>` := peval(lhs, venv), (Expr)`<Int i2>` := peval(rhs, venv), real i := pow(toInt("<i1>"), toInt("<i2>")), Expr iexp := [Expr]"<i>";


Expr peval((Expr)`<Expr lhs> && <Expr rhs>`, VEnv venv) = iexp when (Expr)`<Bool i1>` := peval(lhs, venv), (Expr)`<Bool i2>` := peval(rhs, venv),Expr iexp := [Expr]"<fromString("<i1>") && fromString("<i2>")>";
Expr peval((Expr)`<Expr lhs> || <Expr rhs>`, VEnv venv) = iexp when (Expr)`<Bool i1>` := peval(lhs, venv), (Expr)`<Bool i2>` := peval(rhs, venv),Expr iexp := [Expr]"<fromString("<i1>") || fromString("<i2>")>";
Expr peval((Expr)`<Expr lhs> == <Expr rhs>`, VEnv venv) = iexp when (Expr)`<Bool i1>` := peval(lhs, venv), (Expr)`<Bool i2>` := peval(rhs, venv),Expr iexp := [Expr]"<fromString("<i1>") == fromString("<i2>")>";
Expr peval((Expr)`<Expr lhs> != <Expr rhs>`, VEnv venv) = iexp when (Expr)`<Bool i1>` := peval(lhs, venv), (Expr)`<Bool i2>` := peval(rhs, venv),Expr iexp := [Expr]"<fromString("<i1>") != fromString("<i2>")>";

Expr peval((Expr)`<Expr lhs> \> <Expr rhs>`, VEnv venv) = iexp when (Expr)`<Bool i1>` := peval(lhs, venv), (Expr)`<Bool i2>` := peval(rhs, venv),Expr iexp := [Expr]"<fromString("<i1>") > fromString("<i2>")>";
Expr peval((Expr)`<Expr lhs> \< <Expr rhs>`, VEnv venv) = iexp when (Expr)`<Bool i1>` := peval(lhs, venv), (Expr)`<Bool i2>` := peval(rhs, venv), Expr iexp := [Expr]"<fromString("<i1>") < fromString("<i2>")>";
Expr peval((Expr)`<Expr lhs> \>= <Expr rhs>`, VEnv venv) = iexp when (Expr)`<Bool i1>` := peval(lhs, venv), (Expr)`<Bool i2>` := peval(rhs, venv),Expr iexp := [Expr]"<fromString("<i1>") >= fromString("<i2>")>";
Expr peval((Expr)`<Expr lhs> \>= <Expr rhs>`, VEnv venv) = iexp when (Expr)`<Bool i1>` := peval(lhs, venv), (Expr)`<Bool i2>` := peval(rhs, venv),Expr iexp := [Expr]"<fromString("<i1>") <= fromString("<i2>")>";
Expr peval((Expr)`! <Expr operand>`, VEnv venv) = iexp when (Expr)`<Bool rhs>` := peval(operand, venv),Expr iexp := [Expr]"<!fromString("<rhs>")>";


Expr peval((Expr)`(<Expr e>)`, VEnv venv) = e;
Expr peval(Expr e, VEnv venv) = e;

