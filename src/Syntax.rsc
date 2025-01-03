module Syntax

extend lang::std::Layout;
extend lang::std::Id;

start syntax Form 
  = form: "form" Str title "{" Question* questions "}"; 

lexical Str = [\"]![\"]* [\"];

lexical Bool = "true" | "false";

lexical Int = [\-]* [0-9]+; 

// boolean, integer, string
syntax Type   
  = boolean: "boolean"
  | integer: "integer"
  | string: "string";


// TODO: answerable question, computed question, block, if-then-else
syntax Question 
  = ifThen: "if" "(" Expr cond ")" Question then () !>> "else" 
  | ifThenElse: "if" "(" Expr cond ")" Question then () "else" Question else
  | answerable: Str question Id name ":" Type anstype
  | block: "{" Question* questions "}"
  | computed: Str text Id name ":" Type anstype "=" Expr expr
  ;

// TODO: +, -, *, /, &&, ||, !, >, <, <=, >=, ==, !=, literals (bool, int, str)
// Think about disambiguation using priorities and associativity
// and use C/Java style precedence rules (look it up on the internet)
syntax Expr
  = var: Id name \ "true" \"false"
  | Str | Bool | Int |
  | bracket "(" Expr ")"       
    > right Expr "^" Expr
    > left ( Expr "*" Expr  
           | Expr "/" Expr
           )
    > left ( Expr "+" Expr
           | Expr "-" Expr
           )
    > left ( Expr "\>" Expr
           | Expr "\<" Expr
           | Expr "\<=" Expr
           | Expr "\>=" Expr
           )
    > left ( Expr "==" Expr
           | Expr "!=" Expr
           )
    > right "!" Expr
    > left Expr "&&" Expr
    > left Expr "||" Expr
  ;

