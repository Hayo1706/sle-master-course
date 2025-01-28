module Syntax

extend lang::std::Layout;
extend lang::std::Id;

start syntax Form 
  = form: "form" Str title "{" Question* questions "}"; 

lexical Str = [\"]![\"]* [\"];

lexical Bool = "true" | "false";

lexical Int = [\-]?[0-9]+; 

lexical Regex = "[\\" ![\n]+ "\\]";
// boolean, integer, string
syntax Type   
  = boolean: "boolean"
  | integer: "integer"
  | string: "string";

syntax ValidationRule
  = required : "[required]"
  | range : "[" Expr min ".." Expr max "]"
  | regex : Regex reg;


// TODO: answerable question, computed question, block, if-then-else
syntax Question 
  = ifThen: "if" "(" Expr cond ")" Question then !>> "else" 
  | ifThenElse: "if" "(" Expr cond ")" Question then "else" Question elsethen
  | answerable: Str question Id name ":" Type anstype ValidationRule? rule
  | block: "{" Question* questions "}"
  | computed: Str question Id name ":" Type anstype "=" Expr expr
  ;

// TODO: +, -, *, /, &&, ||, !, >, <, <=, >=, ==, !=, literals (bool, int, str)
// Think about disambiguation using priorities and associativity
// and use C/Java style precedence rules (look it up on the internet)
syntax Expr
  = bracket "(" Expr ")"
  | "!" Expr
  > Int | Bool | Str 
  > var: Id name \ "true" \"false" \ "required" \ "range" \ "regex" \ "form" \ "if" \ "else"      
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
          | Expr "==" Expr
          | Expr "!=" Expr
          )
  > left Expr "&&" Expr
  > left Expr "||" Expr
  ;

