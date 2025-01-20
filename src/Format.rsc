module Format

import util::SimpleBox;
import Syntax;

/*
 * Formatting: transforming QL forms to Box 
 */

str formatQL(start[Form] form) = format(form2box(form));

Box form2box(start[Form] form) = V(H("form", form.top.title, hs=1),"{", I([form2box(q) | q <- form.top.questions]),"}");

Box form2box((Question)`if ( <Expr cond> ) <Question then>`) = V(H("if", "(", cond, ")"), V(form2box(then)));

Box form2box((Question)`if ( <Expr cond> ) <Question then> else <Question elsethen>`) = V(H("if", "(", cond, ")"), V(form2box(then)), "else", V(form2box(elsethen)));

Box form2box((Question)`<Str question> <Id name> : <Type anstype>`) = V(question, I(H(name, ":", anstype)));

Box form2box((Question)`{ <Question* questions> }`) = V("{", I([form2box(q) | q <- questions]), "}");

Box form2box((Question)`<Str text> <Id name> : <Type anstype> = <Expr expr>`) = V(text, I(H(name, ":", anstype, "=", expr)));


