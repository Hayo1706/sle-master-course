module CompileWithValidation

extend Compile;
import Syntax;
import Eval;

import lang::html::AST; // modeling HTML docs
import lang::html::IO; // reading/writing HTML


list[HTMLElement] compile2html((Question)`<Str question> <Id name> : <Type anstype> <ValidationRule rule>`){
    // Create label and input for answerable questions
    HTMLElement lbl = \label([text("<question>"[1..-1])]);
    HTMLElement inp = input();
    inp.\type = typeForAnswerable(anstype); // Determines the input type based on question type
    inp.\name = "<name>"; // Adding the name attribute
    switch(rule){
        case (ValidationRule)`[required]` : {
            inp.required = "true";
        }
        case (ValidationRule)`<Regex regex>` : {
            inp.pattern = "<regex>"[2..-2];
        }
    }
    inp.onchange = "evaluateInput(<inp.\type == "checkbox" ? "this.checked" : "this.value">, this.name)";
    return [div([lbl, inp])];

}

Jenv compile2js((Question)`<Str question> <Id name> : <Type anstype> <ValidationRule rule>`, Jenv jenv) {
    switch(rule){
        case (ValidationRule)`[<Expr min> .. <Expr max> ]` : {
            jenv.setContent += "\n\tdocument.getElementsByName(\"<name>\")[0].max = <compileExpr(max)>";
            jenv.setContent += "\n\tdocument.getElementsByName(\"<name>\")[0].min = <compileExpr(min)>";
        }
    }
    jenv.venvinit += "\n\t[\"<name>\",<type2default(anstype)>],";
    return jenv;

}