module AppWithValidation
extend App;
import Eval;
import Syntax;
import IO;

import String;

import salix::HTML;
import salix::App;
import salix::Core;
import salix::Index;

void viewQuestion((Question)`<Str question> <Id name> : <Type anstype> <ValidationRule rule>`, Model model) {
    // Render input fields for answerable questions
    label("<question>"[1..-1]);
    switch (anstype) {
        case (Type)`boolean`:{
            input(
                \type("checkbox"),
                \checked(model.env["<name>"] == vbool(true)),
                \onClick(updateBool("<name>", !model.env["<name>"].b)),
                \required(rule is required)
            );
            println("required");
        }
        case (Type)`integer`:{
            input(
                \type("number"),
                \value("<model.env["<name>"].n>"),
                \onChange(updateInt("<name>")),
                \min(rule is range ? "<eval(rule.min, model.env).n>" : ""),
                \max(rule is range ? "<eval(rule.max, model.env).n>" : ""),
                \required(rule is required)
            );
        }
        case (Type)`string`:{
            input(
                \type("text"),
                \value("<model.env["<name>"].s>"),
                \required(rule is required),
                \pattern(rule is regex ? "<rule.reg>"[2..-2] : "")
            );
        }
    }
}

