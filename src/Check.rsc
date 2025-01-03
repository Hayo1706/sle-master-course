module Check

import Message;
import IO;
import ParseTree;
import List;

extend Syntax;

// internal type to represent unknown 
syntax Type = "*unknown*";

// type environment maps question names to types
// (NB: it's not a map, because the form can contain errors!)
alias TEnv = lrel[str, Type];

// build a Type Environment (TEnv) for a questionnaire.
TEnv collect(Form f) {
  TEnv env = [];
  visit(f) {
    case (Question) `<Question q>`:
      if (q is answerable || q is computed){
        env = env + <"<q.name>", q.anstype>;
      }
  }
  return env;
}


/*
 * typeOf: compute the type of expressions
 */

// the fall back type is *unknown*
default Type typeOf(Expr _, TEnv env) = (Type)`*unknown*`;

// a reference has the type of its declaration
Type typeOf((Expr)`<Id x>`, TEnv env) = t
    when <"<x>", Type t> <- env;

/*
 * Checking forms
 */

set[Message] check(start[Form] form) = check(form.top);

set[Message] check(Form form) 
  = { *check(q, env) | Question q <- form.questions }
  + checkDuplicates(form)
  + checkCycles(form)
  when TEnv env := collect(form);

set[Message] checkCycles(Form form) {
    set[Message] messages = {};
    list[str] defined = []; // Tracks the list of questions defined so far.

    visit(form) {
        // Handle normal questions
        case (Question)`<Str _> <Id name> : <Type _>`:
            defined += "<name>";

        // Handle computed questions.
        case (Question)`<Str _> <Id name> : <Type _> = <Expr _>`:
        {
            defined += "<name>";
        }

        // Handle expressions
        case (Expr)`<Id ref>`:
            if ("<ref>" notin defined) {
                messages += error("Reference to undefined or later question: <ref>", ref.src);
            }
    }

    return messages;
}

set[Message] checkDuplicates(Form form) {
    set[Message] messages = {};
    list[str] defined = []; // Tracks the list of questions defined so far.
    visit(form) {
        // Handle normal questions
        case (Question)`<Str _> <Id name> : <Type _>`:
        {
            if ("<name>" in defined)
                messages += error("Duplicate question: <name>", name.src);
            defined += "<name>";
        }

        // Handle computed questions.
        case (Question)`<Str _> <Id name> : <Type _> = <Expr _>`:
        {
            if ("<name>" in defined)
                messages += error("Duplicate question: <name>", name.src);
            defined += "<name>";
        }
    }
    return messages;
}

/*
 * Checking questions
 */

// by default, there are no errors or warnings
default set[Message] check(Question _, TEnv _) = {};


/*
 * Checking expressions
 */


// when the other cases fail, there are no errors
default set[Message] check(Expr _, TEnv env) = {};

set[Message] check(e:(Expr)`<Id x>`, TEnv env) = {error("undefined question", x.src)}
    when "<x>" notin env<0>;

set[Message] check((Expr)`(<Expr e>)`, TEnv env) = check(e, env);


void printTEnv(TEnv tenv) {
    for (<str x, Type t> <- tenv) {
        println("<x>: <t>");
    }
}
 
