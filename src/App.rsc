module App

import salix::HTML;
import salix::App;
import salix::Core;
import salix::Index;


import Eval;
import Syntax;
import IO;

import String;

// The salix application model is a tuple
// containing the questionnaire and its current run-time state (env).
alias Model = tuple[start[Form] form, VEnv env];

App[Model] runQL(start[Form] ql) = webApp(qlApp(ql), |project://sle-master-course/src|);

SalixApp[Model] qlApp(start[Form] ql, str id="root") 
  = makeApp(id, 
        Model() { return <ql, initialEnv(ql)>; }, 
        withIndex("<ql.top.title>"[1..-1], id, view, css=["https://cdn.simplecss.org/simple.min.css"]), 
        update);


// The salix Msg type defines the application events.
data Msg
  = updateInt(str name, str n)
  | updateBool(str name, bool b)
  | updateStr(str name, str s)
  ;

// We map messages to Input values 
// to be able to reuse the interpreter defined in Eval.
Input msg2input(updateInt(str q, str n)) = user(q, vint(toInt(n)));
Input msg2input(updateBool(str q, bool b)) = user(q, vbool(b));
Input msg2input(updateStr(str q, str s)) = user(q, vstr(s));

// The Salix model update function simply evaluates the user input
// to obtain the new state. 
Model update(Msg msg, Model model) = model[env=eval(model.form, msg2input(msg), model.env)];

// Salix view rendering works by "drawing" on an implicit HTML canvas.
// Look at the Salix demo folder to learn how html elements are drawn, and how element nesting is achieved with
// nesting of void-closures.
void view(Model model) {
    h3("<model.form.top.title>"[1..-1]);
    ul(() {
        // Render each enabled question
        list[Question] enabledQuestions = render(model.form, model.env);
        for (Question q <- enabledQuestions) {
            li(() { viewQuestion(q, model); });
        }
    });
}
Msg(str) updateInt(str name) = Msg(str n) { return updateInt(name, n);};

// fill in: question rendering, but only if they are enabled.
void viewQuestion((Question)`<Str question> <Id name> <Type anstype>`, Model model) {
    // Render input fields for answerable questions
    label("<question>"[1..-1]);
    switch (anstype) {
        case (Type)`boolean`:{
            input(
                \type("checkbox"),
                \checked(model.env["<name>"] == vbool(true)),
                \onClick(updateBool("<name>", !model.env["<name>"].b))
            );
            
        }
        case (Type)`integer`:
            input(
                \type("number"),
                \value("<model.env["<name>"].n>"),
                \onChange(updateInt("<name>"))
            );
        case (Type)`string`:
            input(
                \type("text"),
                \value("<model.env["<name>"].s>")
            );
        }
}

void viewQuestion((Question)`<Str question> <Id name> : <Type anstype> <Expr expr>`, Model model) {
    // Render read-only fields for computed questions
    label("<text>"[1..-1]);
    str val = "";
    switch (anstype) {
        case (Type)`boolean`:{
            val = "<eval(expr, model.env).b>";
        }
        case (Type)`integer`:
            val = "<eval(expr, model.env).n>";
        case (Type)`string`:
            val = "<eval(expr, model.env).s>";
        }
    span(val);
}

void viewQuestion((Question)`{ <Question* questions>}`, Model model) {
    // Recursively render sub-questions in a block
    ul(() {
        for (Question sub <- questions) {
            li(() { viewQuestion(sub, model); });
        }
    });
}