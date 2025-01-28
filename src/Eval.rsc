module Eval

import Syntax;
import String;
import ParseTree;
import IO;
import util::Math;

/*
 * Big-step semantics for QL
 */
 
// NB: Eval assumes the form is type- and name-correct.

// Semantic domain for expressions (values)
data Value
  = vint(int n)
  | vbool(bool b)
  | vstr(str s)
  ;

// The value environment, mapping question names to values.
alias VEnv = map[str name, Value \value];

// Modeling user input
data Input = user(str question, Value \value);
  

Value type2default((Type)`integer`) = vint(0);
Value type2default((Type)`string`) = vstr("");
Value type2default((Type)`boolean`) = vbool(false);


// produce an environment which for each question has a default value
// using the function type2default function defined above.
// observe how visit traverses the form and match on normal questions and computed questions.
VEnv initialEnv(start[Form] f) = initialEnv(f.top);

VEnv initialEnv(Form f) {
  VEnv env = ();
  visit(f) {
    case (Question) `<Question q>`:
      if (q is answerable || q is computed){
        env["<q.name>"] = type2default(q.anstype);
      }
  }
  return eval(f,user("",vint(0)), env);
}

// Expression evaluation (complete for all expressions)

Value eval((Expr)`<Id x>`, VEnv venv) = venv["<x>"];

VEnv eval(start[Form] f, Input inp, VEnv venv) = eval(f.top, inp, venv);

Value eval((Expr)`<Str x>`, VEnv venv) = vstr("<x>"[1..-1]);      // String literal

Value eval((Expr)`<Bool x>`, VEnv venv) = vbool("<x>" == "true");  // Boolean literal

Value eval((Expr)`<Int x>`, VEnv venv) = vint(toInt("<x>"));  // Integer literal


Value eval((Expr)`(<Expr e>)`, VEnv venv) = eval(e, venv);

// Exponentiation (right-associative)
Value eval((Expr)`<Expr left> ^ <Expr right>`, VEnv venv) = vint(toInt(pow(eval(left, venv).n, eval(right, venv).n)));

// Multiplication and Division (left-associative)
Value eval((Expr)`<Expr left> * <Expr right>`, VEnv venv) = vint(eval(left, venv).n * eval(right, venv).n);

Value eval((Expr)`<Expr left> / <Expr right>`, VEnv venv) = vint(eval(right, venv).n != 0 ? eval(left, venv).n / eval(right, venv).n : 0);

// Addition and Subtraction (left-associative)
Value eval((Expr)`<Expr left> + <Expr right>`, VEnv venv) = vint(eval(left, venv).n + eval(right, venv).n);

Value eval((Expr)`<Expr e1> - <Expr e2>`, VEnv venv) = vint(eval(e1, venv).n - eval(e2, venv).n);

// Comparison operations (>, <, <=, >=)
Value eval((Expr)`<Expr left> \> <Expr right>`, VEnv venv) = vbool(eval(left, venv).n > eval(right, venv).n);

Value eval((Expr)`<Expr left> \< <Expr right>`, VEnv venv) = vbool(eval(left, venv).n < eval(right, venv).n);

Value eval((Expr)`<Expr left> \<= <Expr right>`, VEnv venv) = vbool(eval(left, venv).n <= eval(right, venv).n);

Value eval((Expr)`<Expr left> \>= <Expr right>`, VEnv venv) = vbool(eval(left, venv).n >= eval(right, venv).n);

// Equality and inequality (==, !=)
Value eval((Expr)`<Expr left> == <Expr right>`, VEnv venv) = vbool(eval(left, venv).n == eval(right, venv).n);

Value eval((Expr)`<Expr left> != <Expr right>`, VEnv venv) = vbool(eval(left, venv).n != eval(right, venv).n);

// Logical NOT
Value eval((Expr)`! <Expr operand>`, VEnv venv) = vbool(!eval(operand, venv).b);

// Logical AND (&&)
Value eval((Expr)`<Expr left> && <Expr right>`, VEnv venv) = vbool(eval(left, venv).b && eval(right, venv).b);

// Logical OR (||)
Value eval((Expr)`<Expr left> || <Expr right>`, VEnv venv) = vbool(eval(left, venv).b || eval(right, venv).b);



// Because of out-of-order use and declaration of questions
// we use the solve primitive in Rascal to find the fixpoint of venv.
VEnv eval(Form f, Input inp, VEnv venv) {
  return solve (venv) {
    venv = evalOnce(f, inp, venv);
  }
}

// evaluate the questionnaire in one round 
VEnv evalOnce(Form f, Input inp, VEnv venv)
  = ( venv | eval(q, inp, it) | Question q <- f.questions );


VEnv eval(Question q, Input inp, VEnv venv) {
    switch (q) {
      case answerable(_, Id name, _, _): {
        if (inp.question == "<name>") {
          venv["<name>"] = inp.\value;
        }
        return venv;
      }

      case computed(Str text, Id name, Type anstype, Expr expr): {
        Value computedValue = eval(expr, venv);
        venv["<name>"] = computedValue;
        return venv;
      }

      case ifThen (Expr cond, Question then): {
        Value condValue = eval(cond, venv);
        if (condValue is vbool && condValue == vbool(true)) {
          return eval(then, inp, venv);
        }
        return venv;
      }

      case ifThenElse(Expr cond, Question then, Question elsethen):{
        Value condValue = eval(cond, venv);
        if (condValue is vbool && condValue == vbool(true)) {
          return eval(then, inp, venv);
        } else {
          return eval(elsethen, inp, venv);
        }
      }
      
      case block(Question* questions):{
        for (Question subQuestion <- questions) {
          venv = eval(subQuestion, inp, venv);
        }
        return venv;
      }
  }
  return venv;
}

/*
 * Rendering UIs: use questions as widgets
 */

list[Question] render(start[Form] form, VEnv venv) = render(form.top, venv);

list[Question] render(Form form, VEnv venv) {
  list[Question] renderedQuestions = [];
  for (Question q <- form.questions) {
    renderedQuestions += renderQ(q, venv);
  }
  return renderedQuestions;
}

list[Question] renderQ(Question q, VEnv venv) {
    switch (q) {
      case answerable(_, Id name, _, _):
        return [q];
      

      case computed(Str text, Id name, Type anstype, Expr expr):{
        Expr computedValue = value2expr(eval(expr, venv));
        return [(Question)`<Str text> <Id name>: <Type anstype> = <Expr computedValue>`];
      }

      case ifThen (Expr cond, Question then): {
        Value condValue = eval(cond, venv);
        if (condValue is vbool && condValue.b) {
          return renderQ(then, venv);
        }
        return [];
      }

      case ifThenElse(Expr cond, Question then, Question elsethen):{
        Value condValue = eval(cond, venv);
        if (condValue is vbool && condValue.b) {
          return renderQ(then, venv);
        }
        return renderQ(elsethen, venv);
      }
      
      case block(Question* questions):{
        list[Question] renderedSubQuestions = [];
        for (Question subQ <- questions) {
          renderedSubQuestions += renderQ(subQ, venv);        
        }
        return renderedSubQuestions;
      }
  }
  return [];
}

Expr value2expr(vbool(bool b)) = [Expr]"<b>";
Expr value2expr(vstr(str s)) = [Expr]"\"<s>\"";
Expr value2expr(vint(int i)) = [Expr]"<i>";

void printUI(list[Question] ui) {
  for (Question q <- ui) {
    println(q);
  }
}


void evalSnippets() {
  start[Form] pt = parse(#start[Form], |project://sle-master-course/examples/tax.myql|);

  env = initialEnv(pt);
  env2 = eval(pt, user("hasSoldHouse", vbool(true)), env);
  env3 = eval(pt, user("sellingPrice", vint(1000)), env2);
  env4 = eval(pt, user("privateDebt", vint(500)), env3);

  for (Input u <- [user("hasSoldHouse", vbool(true)), user("sellingPrice", vint(1000)), user("privateDebt", vint(500))]) {
    env = eval(pt, u, env);
    println(env);
    printUI(render(pt,env));
  }
}