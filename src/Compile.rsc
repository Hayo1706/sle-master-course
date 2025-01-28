module Compile

import Syntax;
import Eval;
import IO;
import ParseTree;

import lang::html::AST; // modeling HTML docs
import lang::html::IO; // reading/writing HTML


void compile(start[Form] form) {
  loc h = form.src[extension="html"];
  loc j = form.src[extension="js"].top;
  str js = compile2js(form);
  HTMLElement ht = compile2html(form);
  writeFile(j, js);
  writeHTMLFile(h, ht, escapeMode=extendedMode());
}

str type2default((Type)`integer`) = "0";
str type2default((Type)`string`) = "";
str type2default((Type)`boolean`) = "false";

data Jenv = output(str venvinit, str calculations, str setContent, int ifs);


str compile2js(start[Form] form) {
  Jenv jenv = output("venv = new Map([", "function calculate(){", "function setContent(){", 0);
  
  visit(form.top.questions){
    case (Question) `<Question q>`:
        jenv = compile2js(q, jenv);
  }

  jenv.venvinit += "\n]);\n\n";
  jenv.calculations += "\n}\n\n";
  jenv.setContent += "\n}\n\n";

  js = "";
  js += jenv.venvinit;
  js += "const mapsAreEqual = (m1, m2) =\> m1.size === m2.size && Array.from(m1.keys()).every((key) =\> m1.get(key) === m2.get(key));\n\n";
  js += "window.onload = update();\n\n";
  js += "function evaluateInput(value, id) {
  \tvenv.set(id,value);
  \tupdate();
}\n\n";
  js += "function update(){
  solve();
  setContent();
}\n\n";
  js += jenv.calculations;
  js += jenv.setContent;
  js += "function solve(){
  do {
    temp = venv;
    calculate();
  } while (!mapsAreEqual(temp,venv));
}";
  return js;
}

default Jenv compile2js(Question q, Jenv jenv) = jenv;

Jenv compile2js((Question)`if (<Expr cond>) <Question then>`, Jenv jenv) {
  jenv.venvinit += "\n\t[\"ifcondition<jenv.ifs>\",false],";
  jenv.calculations += "\n\tvenv.set(\"ifcondition<jenv.ifs>\", <compileExpr(cond)>);";
  jenv.setContent += "\n\tdocument.getElementsByName(\"ifcondition<jenv.ifs>\")[0].style.visibility = <compileExpr(cond)> ? \"visible\" : \"hidden\";";
  jenv.setContent += "\n\tArray.prototype.forEach.call(document.getElementsByName(\"ifcondition<jenv.ifs>\")[0].getElementsByTagName(\"input\"),target =\> target.disabled = !<compileExpr(cond)>);";
  return jenv;
}

Jenv compile2js((Question)`if (<Expr cond>) <Question then> else <Question elsethen>`, Jenv jenv) {
  jenv.venvinit += "\n\t[\"ifcondition<jenv.ifs>\",false],";
  jenv.venvinit += "\n\t[\"ifcondition<jenv.ifs>-else\",true],";
  jenv.calculations += "\n\tvenv.set(ifcondition<jenv.ifs>, <compileExpr(cond)>);";
  jenv.setContent += "\n\tif (<cond>){
    document.getElementsByName(\"ifcondition<jenv.ifs>\")[0].style.visibility = \"visible\";
    document.getElementsByName(\"ifcondition<jenv.ifs>-else\")[0].style.visibility = \"hidden\";
    }
    else{
      document.getElementsByName(\"ifcondition<jenv.ifs>\")[0].style.visibility = \"hidden\";
      document.getElementsByName(\"ifcondition<jenv.ifs>-else\")[0].style.visibility = \"visible\";
    }";
  jenv.ifs += 1;  
  return jenv;
}

Jenv compile2js((Question)`<Str question> <Id name> : <Type anstype>`, Jenv jenv) {
  jenv.venvinit += "\n\t[\"<name>\",<type2default(anstype)>],";
  return jenv;
}

Jenv compile2js((Question)`<Str question> <Id name> : <Type anstype> = <Expr expr>`, Jenv jenv) {
  jenv.venvinit += "\n\t[\"<name>\",<type2default(anstype)>],";
  jenv.calculations += "\n\tvenv.set(\"<name>\", <compileExpr(expr)>);";
  jenv.setContent += "\n\tdocument.getElementsByName(\"<name>\")[0].textContent = venv.get(\"<name>\");";
  return jenv;
}

str compileExpr(Expr e) {
  switch (e) {
      case (Expr)`<Id x>`: return "venv.get(\"<x>\")";
      case (Expr)`<Str x>`: return "<x>";
      case (Expr)`<Bool x>`: return "<x>" == "true" ? "true" : "false";
      case (Expr)`<Int x>`: return "<x>";
      case (Expr)`<Expr left> + <Expr right>`: return "(Number(<compileExpr(left)>) + Number(<compileExpr(right)>))";
      case (Expr)`<Expr left> - <Expr right>`: return "(Number(<compileExpr(left)>) - Number(<compileExpr(right)>))";
      case (Expr)`<Expr left> * <Expr right>`: return "(Number(<compileExpr(left)>) * Number(<compileExpr(right)>))";
      case (Expr)`<Expr left> / <Expr right>`: return "(Number(<compileExpr(left)>) / Number(<compileExpr(right)>))";
      case (Expr)`<Expr left> ^ <Expr right>`: return "Math.pow(Number(<compileExpr(left)>), Number(<compileExpr(right)>))";
      case (Expr)`<Expr left> && <Expr right>`: return "(<compileExpr(left)> && <compileExpr(right)>)";
      case (Expr)`<Expr left> || <Expr right>`: return "(<compileExpr(left)> || <compileExpr(right)>)";
      case (Expr)`<Expr left> == <Expr right>`: return "(<compileExpr(left)> == <compileExpr(right)>)";
      case (Expr)`<Expr left> != <Expr right>`: return "(<compileExpr(left)> != <compileExpr(right)>)";
      case (Expr)`<Expr left> \> <Expr right>`: return "(<compileExpr(left)> \> <compileExpr(right)>)";
      case (Expr)`<Expr left> \< <Expr right>`: return "(<compileExpr(left)> \< <compileExpr(right)>)";
      case (Expr)`<Expr left> \>= <Expr right>`: return "(<compileExpr(left)> \>= <compileExpr(right)>)";
      case (Expr)`<Expr left> \<= <Expr right>`: return "(<compileExpr(left)> \<= <compileExpr(right)>)";

      case (Expr)`! <Expr operand>`: return "(!<compileExpr(operand)>)";
  }
  return "";
}
int H_ifs = 0;

HTMLElement compile2html(start[Form] f) {
  H_ifs = 0;
  list[HTMLElement] elems = [];

  for (Question q <- f.top.questions){
    elems += compile2html(q);
  }
  HTMLElement submit = input();
  submit.\type = "submit";
  submit.\value = "Submit";

  elems += submit;
  // Return the complete HTML structure wrapped in a form element
  HTMLElement questions = form(elems);

  HTMLElement lin = link();
  lin.\rel = "stylesheet";
  lin.href = "https://cdn.simplecss.org/simple.min.css";

  HTMLElement scr = script([]);
  scr.src = "<f.src[extension="js"].file>";
  scr.defer = "true";
  

  HTMLElement tit = title([text("<f.top.title>"[1..-1])]);

  return html([tit,lin,scr, h3([text("<f.top.title>"[1..-1])]), questions]);
}



default list[HTMLElement] compile2html(Question q) = [];

list[HTMLElement] compile2html((Question)`if (<Expr cond>) <Question then>`) {
  list[HTMLElement] ifthenqs = compile2html(then); // Recursively process conditional questions
  HTMLElement ifdiv = div(ifthenqs);
  ifdiv.name = "ifcondition<H_ifs>";
  H_ifs += 1;
  return [ifdiv];
}

list[HTMLElement] compile2html((Question)`if (<Expr cond>) <Question then> else <Question elsethen>`) {
  list[HTMLElement] ifthenqs = compile2html(then); // Recursively process then branch
  list[HTMLElement] ifelseqs = compile2html(elsethen); // Recursively process else branch
  HTMLElement ifdiv = div(ifthenqs);
  ifdiv.name = "ifcondition<H_ifs>";
  HTMLElement ifelsediv = div(ifelseqs);
  ifelsediv.name = "ifcondition<H_ifs>-else";
  H_ifs += 1;
  return [ifdiv, ifelsediv];
}

list[HTMLElement] compile2html((Question)`<Str question> <Id name> : <Type anstype>`){
    // Create label and input for answerable questions
    HTMLElement lbl = \label([text("<question>"[1..-1])]);
    HTMLElement inp = input();
    inp.\type = typeForAnswerable(anstype); // Determines the input type based on question type
    inp.\name = "<name>"; // Adding the name attribute
    inp.onchange = "evaluateInput(<inp.\type == "checkbox" ? "this.checked" : "this.value">, this.name)";
    return [div([lbl, inp])];
}

list[HTMLElement] compile2html((Question)`{ <Question* questions> }`) = ([] | it + compile2html(q) | q <- questions);

list[HTMLElement] compile2html((Question)`<Str question> <Id name> : <Type anstype> = <Expr expr>`) {
    // Create label and input for computed questions
    HTMLElement lbl = label([text("<question>"[1..-1])]);
    HTMLElement inp = span([]);
    inp.\type = "text"; // Computed fields are read-only
    inp.\name = "<name>"; // Adding the name attribute
    inp.\readonly = "true"; // Computed fields should not be editable
    inp.\value = "1"; // Placeholder for computed values (updated dynamically in JS)
    return [div([lbl, inp])];
}

str typeForAnswerable(Type t) {
  switch (t) {
    case (Type)`boolean`: return "checkbox";
    case (Type)`integer`: return "number";
    case (Type)`string`: return "text";
    default: return "text"; // Fallback for unknown types
  }
}