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


str compile2js(start[Form] form) {
  str venvinit = "venv = new Map([";
  str calculations = "function calculate(){";
  str setContent = "function setContent(){";

  int ifs = 0;
  
  visit(form.top.questions){
    case (Question) `<Question q>`:{
      if (q is answerable){
        venvinit += "\n\t[\"<q.name>\",<type2default(q.anstype)>],";
      }
      if (q is computed){
        venvinit += "\n\t[\"<q.name>\",<type2default(q.anstype)>],";
        calculations += "\n\tvenv.set(\"<q.name>\", <compileExpr(q.expr)>);";
        setContent += "\n\tdocument.getElementsByName(\"<q.name>\")[0].textContent = venv.get(\"<q.name>\");";
      }
      if(q is ifThen){
        venvinit += "\n\t[\"ifcondition<ifs>\",false],";
        calculations += "\n\tvenv.set(\"ifcondition<ifs>\", <compileExpr(q.cond)>);";
        setContent += "\n\tdocument.getElementsByName(\"ifcondition<ifs>\")[0].style.visibility = 
                          <compileExpr(q.cond)> ? \"visible\" : \"hidden\";";

        ifs += 1;
      }
      if (q is ifThenElse){
        venvinit += "\n\t[\"ifcondition<ifs>\",false],";
        venvinit += "\n\t[\"ifcondition<ifs>-else\",<type2default(q.anstype)>],";
        calculations += "\n\tvenv.set(ifcondition<ifs>, <compileExpr(q.cond)>);";
        setContent += "\n\tif (<q.cond>){
    document.getElementsByName(\"ifcondition<ifs>\")[0].style.visibility = \"visible\";
    document.getElementsByName(\"ifcondition<ifs>-else\")[0].style.visibility = \"hidden\";
    }
  else{
    \tdocument.getElementsByName(\"ifcondition<ifs>\")[0].style.visibility = \"hidden\";
    \tdocument.getElementsByName(\"ifcondition<ifs>-else\")[0].style.visibility = \"visible\";
  }";
        ifs += 1;
      }
    }
  }
  venvinit += "\n]);\n\n";
  calculations += "\n}\n\n";
  setContent += "\n}\n\n";

  js = "";
  js += venvinit;
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
  js += calculations;
  js += setContent;
  js += "function solve(){
  do {
    temp = venv;
    calculate();
  } while (!mapsAreEqual(temp,venv));
}";
  return js;
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

HTMLElement compile2html(start[Form] f) {
  list[HTMLElement] elems = [];

  for (Question q <- f.top.questions){
    elems += compile2html(q);
  }

  // Return the complete HTML structure wrapped in a form element
  HTMLElement questions = form(elems);

  HTMLElement lin = link();
  lin.\rel = "stylesheet";
  lin.href = "https://cdn.simplecss.org/simple.min.css";

  HTMLElement scr = script([]);
  scr.src = "<f.src[extension="js"].file>";
  scr.defer = "true";
  

  HTMLElement tit = title([text("<f.top.title>")]);

  return html([tit,lin,scr, h3([text("<f.top.title>")]), questions]);
}

int ifs = 0;

list[HTMLElement] compile2html(Question q){
    list[HTMLElement] elems = [];
    if (q is answerable) {
          // Create label and input for answerable questions
          HTMLElement lbl = \label([text("<q.question>")]);
          HTMLElement inp = input();
          inp.\type = typeForAnswerable(q); // Determines the input type based on question type
          inp.\name = "<q.name>"; // Adding the name attribute
          inp.onchange = "evaluateInput(<inp.\type == "checkbox" ? "this.checked" : "this.value">, this.name)";
          elems += div([lbl, inp]);
      }
      else if (q is computed) {
        // Create label and input for computed questions
        HTMLElement lbl = label([text("<q.question>")]);
        HTMLElement inp = span([]);
        inp.\type = "text"; // Computed fields are read-only
        inp.\name = "<q.name>"; // Adding the name attribute
        inp.\readonly = "true"; // Computed fields should not be editable
        inp.\value = "1"; // Placeholder for computed values (updated dynamically in JS)
        elems += div([lbl, inp]);
      }
      else if (q is ifThen) {
        list[HTMLElement] ifthenqs = compile2html(q.then); // Recursively process conditional questions
        HTMLElement ifdiv = div(ifthenqs);
        ifdiv.name = "ifcondition<ifs>";
        ifs += 1;
        elems += ifdiv;
        
      }
      else if (q is ifThenElse) {
        list[HTMLElement] ifthenqs = compile2html(q.then); // Recursively process then branch
        list[HTMLElement] ifelseqs = compile2html(q.elsethen); // Recursively process else branch
        HTMLElement ifdiv = div(ifthenqs);
        ifdiv.name = "ifcondition<ifs>";
        elems += ifdiv;
        HTMLElement ifelsediv = div(ifelseqs);
        ifelsediv.name = "ifcondition<ifs>-else";
        ifs += 1;
        elems += ifelsediv;
      }
      else if (q is block) {
        // Process blocks of questions
        for (Question subQ <- q.questions) {
          elems += compile2html(subQ);
        }
      }
      return elems;
}

str typeForAnswerable(Question q) {
  switch (q.anstype) {
    case (Type)`boolean`: return "checkbox";
    case (Type)`integer`: return "number";
    case (Type)`string`: return "text";
    default: return "text"; // Fallback for unknown types
  }
}