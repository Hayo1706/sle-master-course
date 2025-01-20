module Flatten

import Syntax;
//import IO;
/* Normalization:
 *  wrt to the semantics of QL the following
 *     q0: "" int; 
 *     if (a) { 
 *        if (b) { 
 *          q1: "" int; 
 *        } 
 *        q2: "" int; 
 *      }
 *
 *  is equivalent to
 *     if (true) q0: "" int;
 *     if (true && a && b) q1: "" int;
 *     if (true && a) q2: "" int;
 *
 * Write a transformation that performs this flattening transformation.
 *
 */

list[Question] flatten(start[Form] f) = flatten(f.top);

list[Question] flatten(Form f) = ([] |it + flatten(q, [Expr]"true")| Question q <- f.questions);

list[Question] flatten(Question q, Expr conditions) {
    list[Question] result = [];
    switch (q) {
        case (Question)`if ( <Expr cond> ) <Question then>`:{
            result += flatten(then, (Expr)`<Expr conditions> && <Expr cond>`);
        }
        case (Question)`if (<Expr cond>) <Question then> else <Question elsethen>`:{
            result += flatten(then, (Expr)`<Expr conditions> && <Expr cond>`);
            result += flatten(elsethen, (Expr)`<Expr conditions> && !<Expr cond>`);
        }
        case (Question)`{ <Question* questions> }`:
            result += ([] | it + flatten(q, conditions) | Question q <- questions);
        default:
            result += (Question)`if (<Expr conditions>) <Question q>`;
    }
    return result;
}

// helper function to go back to a proper questionnaire term.
start[Form] unflatten(list[Question] qs, start[Form] org) {
    Str title = org.top.title;
    Form f = (Form)`form <Str title> {}`;
    for (Question q <- qs, (Form)`form <Str t> {<Question* qqs>}` := f) {
        f = (Form)`form <Str t> {
                  '  <Question* qqs>
                  '  <Question q>
                  '}`;
    }
    return org[top=f];
}
