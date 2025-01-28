module Check

import Message;
import IO;
import ParseTree;
import List;
import String;
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

Type typeOf((Expr)`(<Expr e>)`, TEnv env) = typeOf(e, env);
Type typeOf((Expr)`<Int _>`, TEnv env) = (Type)`integer`;
Type typeOf((Expr)`<Bool _>`, TEnv env) = (Type)`boolean`;
Type typeOf((Expr)`<Str _>`, TEnv env) = (Type)`string`;
Type typeOf((Expr)`<Expr _> \< <Expr _>`, TEnv env) = (Type)`boolean`;
Type typeOf((Expr)`<Expr _> \> <Expr _>`, TEnv env) = (Type)`boolean`;
Type typeOf((Expr)`<Expr _> \>= <Expr _>`, TEnv env) = (Type)`boolean`;
Type typeOf((Expr)`<Expr _> \<= <Expr _>`, TEnv env) = (Type)`boolean`;
Type typeOf((Expr)`<Expr _> == <Expr _>`, TEnv env) = (Type)`boolean`;
Type typeOf((Expr)`<Expr _> != <Expr _>`, TEnv env) = (Type)`boolean`;
Type typeOf((Expr)`<Expr _> || <Expr _>`, TEnv env) = (Type)`boolean`;
Type typeOf((Expr)`<Expr _> && <Expr _>`, TEnv env) = (Type)`boolean`;
Type typeOf((Expr)`<Expr _> + <Expr _>`, TEnv env) = (Type)`integer`;
Type typeOf((Expr)`<Expr _> * <Expr _>`, TEnv env) = (Type)`integer`;
Type typeOf((Expr)`<Expr _> / <Expr _>`, TEnv env) = (Type)`integer`;
Type typeOf((Expr)`<Expr _> - <Expr _>`, TEnv env) = (Type)`integer`;




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
    rel[str, str, loc] dependencies = {};

    void addIfDependencies(Expr cond, Question then){
        visit(cond){
            case (Id) `<Id x>`:{
                visit(then){
                    case q2:(Question) `<Str _> <Id _>: <Type _> = <Expr _>`:
                        dependencies += <"<q2.name>", "<x>", q2.src>;
                    case q2:(Question) `<Str _> <Id _>: <Type _>`:
                        dependencies += <"<q2.name>", "<x>", q2.src>;
                }
            }
        }
    }

    // Add dependencies for each question
    visit(form) {
        case Question q:{
            if (q is computed){
                visit(q.expr){
                    case (Id) `<Id x>`:{
                        dependencies += <"<q.name>", "<x>", q.src>;
                    }
                }
            }
            if (q is ifThen){
                addIfDependencies(q.cond, q.then);
            }
            if (q is ifThenElse){
                addIfDependencies(q.cond, q.then);
                addIfDependencies(q.cond, q.elsethen);
            }
        }
    }

    for (<str x, x> <- dependencies<0,1>+) { // Iterate over transitive dependencies
        for (<_, x, loc a> <- dependencies) { // Match where the dependency ends with the same `x`
            messages += {error("cyclic dependency", a)};
        }
    }
    
    return messages;
}

set[Message] checkDuplicates(Form form) {
    set[Message] messages = {};
    list[Question] defined = []; // Tracks the list of questions defined so far.
    top-down visit(form) {
        case Question q:{
            if (q is answerable || q is computed){
                for (Question q2 <- defined){
                    if ("<q2.name>" == "<q.name>"){
                        if ("<q2.anstype>" != "<q.anstype>")
                                messages += error("Redeclared question name with different type: <q.name>", q.src);                        
                        if ("<q2.question>" != "<q.question>")
                                messages += warning("Redeclared question name: <q.name>", q.src);
                    }
                    else if ("<q2.question>" == "<q.question>"){
                        messages += warning("Redeclared question text: <q.name>", q.src);
                    }
                }
                defined += q;
                if ("<q.question>" == "<(Str)`""`>"){
                    messages += warning("Empty question text", q.src);
                }
                
            }
        }
    }
    return messages;
}

/*
 * Checking questions
 */

// by default, there are no errors or warnings
default set[Message] check(Question _, TEnv _) = {};

set[Message] check((Question)`<Str s> <Id i>: <Type t> = <Expr e>`, TEnv env)
    = { error("incompatible types", e.src) | t !:= typeOf(e, env) }
    + check(e, env);

set[Message] ifcondition(Expr cond, Question then, TEnv env) {
    set[Message] messages = {};
    messages += { error("condition must be boolean", cond.src) | (Type)`boolean` !:= typeOf(cond, env) };
    messages += { warning("question is unreachable", then.src) | (Expr)`false` := cond};
    messages += { warning("if is always true", cond.src) | (Expr)`true` := cond};
    messages += {warning("question is empty", then.src) | (Question)`{}` := then};
    messages += check(cond, env);
    messages += check(then, env);
    return messages;
}

set[Message] check((Question)`if (<Expr cond>) <Question then>`, TEnv env)
    = ifcondition(cond, then, env);

set[Message] check((Question)`if (<Expr cond>) <Question then> else <Question elsethen>`, TEnv env) {
    set[Message] messages = {};
    messages += ifcondition(cond, then, env);
    messages += { warning("empty else", elsethen.src) | (Question)`{}` := elsethen};
    return messages;
}

set[Message] check((Question)`{ <Question* questions> }`, TEnv env) {
    set[Message] messages = {};
    for (Question q <- questions) {
        messages += check(q, env);
    }
    return messages;
}



/*
 * Checking expressions
 */


// when the other cases fail, there are no errors
default set[Message] check(Expr _, TEnv env) = {};

set[Message] check(e:(Expr)`<Id x>`, TEnv env) = {error("undefined question", x.src)}
    when "<x>" notin env<0>;

set[Message] check((Expr)`(<Expr e>)`, TEnv env) = check(e, env);


// Helper function for logical operators
set[Message] validateLogicalOperands(Expr left, Expr right, TEnv env) {
    set[Message] messages = {};
    messages += { error("Invalid operand for logical operation", left.src) | (Type)`boolean` !:= typeOf(left, env) };
    messages += { error("Invalid operand for logical operation", right.src) | (Type)`boolean` !:= typeOf(right, env) };
    messages += check(left, env);
    messages += check(right, env);
    return messages;
}

set[Message] check((Expr)`<Expr left> && <Expr right>`, TEnv env) = validateLogicalOperands(left, right, env);
set[Message] check((Expr)`<Expr left> || <Expr right>`, TEnv env) = validateLogicalOperands(left, right, env);


// Helper function for equality/inequality operators
set[Message] validateEqualityOperands(Expr expr, Expr left, Expr right, TEnv env) {
    return { error("Operands must have the same type for equality/inequality checks", expr.src)
    | Type leftType := typeOf(left, env), leftType !:= typeOf(right, env) } + check(left, env) + check(right, env);
}

set[Message] check((Expr)`<Expr left> == <Expr right>`, TEnv env) = validateEqualityOperands((Expr)`<Expr left> == <Expr right>`, left, right, env);
set[Message] check((Expr)`<Expr left> != <Expr right>`, TEnv env) = validateEqualityOperands((Expr)`<Expr left> != <Expr right>`, left, right, env);


// Helper function for comparison operations
set[Message] validateComparisonOperands(Expr left, Expr right, TEnv env) {
    set[Message] messages = {}; 
    messages += { error("Operands must be integers for comparison", left.src) | (Type)`integer` !:= typeOf(left, env) };
    messages += { error("Operands must be integers for comparison", right.src) | (Type)`integer` !:= typeOf(right, env) };
    messages += check(left, env);
    messages += check(right, env);
    return messages;
}

set[Message] check((Expr)`<Expr left> \< <Expr right>`, TEnv env) = validateComparisonOperands(left, right, env);
set[Message] check((Expr)`<Expr left> \<= <Expr right>`, TEnv env) = validateComparisonOperands(left, right, env);

set[Message] check((Expr)`<Expr left> \> <Expr right>`, TEnv env) = validateComparisonOperands(left, right, env);
set[Message] check((Expr)`<Expr left> \>= <Expr right>`, TEnv env) = validateComparisonOperands(left, right, env);

// Helper function to validate arithmetic expressions
set[Message] validateArithmeticOperands(Expr left, Expr right, TEnv env) {
    set[Message] messages = {}; 
    messages += { error("Invalid operand for arithmetic operation", left.src) | (Type)`integer` !:= typeOf(left, env) };
    messages += { error("Invalid operand for arithmetic operation", right.src) | (Type)`integer` !:= typeOf(right, env) };
    messages += check(left, env);
    messages += check(right, env);
    return messages;
}

set[Message] check((Expr)`<Expr left> + <Expr right>`, TEnv env) = validateArithmeticOperands(left, right, env);
set[Message] check((Expr)`<Expr left> - <Expr right>`, TEnv env) = validateArithmeticOperands(left, right, env);
set[Message] check((Expr)`<Expr left> * <Expr right>`, TEnv env) = validateArithmeticOperands(left, right, env);
set[Message] check((Expr)`<Expr left> / <Expr right>`, TEnv env) = validateArithmeticOperands(left, right, env);


void printTEnv(TEnv tenv) {
    for (<str x, Type t> <- tenv) {
        println("<x>: <t>");
    }
}
 
