module tutorial::Series2

import ParseTree;
import IO;
import String;

/*
 * Syntax definition
 * - define a grammar for JSON (https://json.org/)
 */
 
start syntax JSON
  = Object;
  
syntax Object
  = "{" {Element ","}* "}"
  ;
  
syntax Element
  = String ":" Value; // Fill in  
  
syntax Value
  = String
  | Number
  | Array
  | Object
  | Boolean
  | Null
  ;

syntax Null
  = "null";
  
syntax Boolean
  = "true"
  | "false"
  ;  
  
syntax Array
  = "[" {Value ","}* "]"
  ;  
  
lexical String
  = [\"] ![\"]* [\"]; // slightly simplified
  
lexical Number
  = [\-]? [0-9]+; // Fill in. Hint; think of the pattern for numbers in regular expressions. How do you accept a number in a regex?  

layout Whitespace = [\ \t\n]* !>> [\ \t\n];  
    

start[JSON] example() 
  = parse(#start[JSON],"{\"age\": 42, \"name\": \"Joe\", \"address\": {\"street\": \"Wallstreet\", \"number\": 102}}");    
  
start[JSON] example2() 
  = parse(#start[JSON],"{\"age\": 42, \"name\": \"Joe\", \"address\": 102}");    


// use visit/deep match to find all element names
// - use concrete pattern matching
// - use "<x>" to convert a String x to str
set[str] propNames(start[JSON] json) {
  set[str] names = {};
  visit(json){
    case (Element) `<String s>: <Value v>`:
      names = names + unquote("<s>");

  }
  return names;
}

// define a recursive transformation mapping JSON to map[str,value] 
// - every Value constructor alternative needs a 'transformation' function
// - define a data type for representing null;

map[str, value] json2map(start[JSON] json) = json2map(json.top);

map[str, value] json2map((JSON)`<Object obj>`)  = json2map(obj);
map[str, value] json2map((Object)`{<{Element ","}* elems>}`) = (
  (unquote("<key>") : json2value(val)  | (Element)`<String key>: <Value val>` <- elems)
);

str unquote(str s) = s[1..-1];

value json2value((Value)`<String s>`)    = unquote("<s>"); // This is an example how to transform the String literal to a value
value json2value((Value)`<Number n>`)    = toInt("<n>");
value json2value((Value)`<Boolean b>`)   = "<b>" == "true";
value json2value((Value)`<Null null>`)   = "";
map[str, value] json2value((Value)`<Object object>`) = json2map(object);


// The other alternatives are missing. You need to add them.

default value json2map(Value v) { throw "No tranformation function for `<v>` defined"; }

test bool example2map() = json2map(example()) == (
  "age": 42,
  "name": "Joe",
  "address" : (
     "street" : "Wallstreet",
     "number" : 102
  )
);

