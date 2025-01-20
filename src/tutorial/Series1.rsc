module tutorial::Series1

import List;
import IO;

/*
 * Documentation: https://www.rascal-mpl.org/docs/GettingStarted/
 */

/*
 * Hello world
 *
 * - Import IO, write a function that prints out Hello World!
 * - open the console (click "Import in new Rascal Terminal")
 * - import this module and invoke helloWorld.
 */
 
void helloWorld() {
  println("Hello SLE 2024/2025");
} 

/*
 * FizzBuzz (https://en.wikipedia.org/wiki/Fizz_buzz)
 * - To practice some basic programming constructs in Rascal, let us implement two versions of Fizz_buzz
 * - an implementation that solves it imperatively and prints them (fizzBuzz)
 * - another implemention as a list-returning function (fizzBuzzList)
 */
 
void fizzBuzz() {
  println(intercalate(",",[x % 3 == 0 ? "Fizz" : "<x>" | x <- [1..100]]));
}

list[str] fizzBuzzList() {
  return [x % 3 == 0 ? "Fizz" : "<x>" | x <- [1..100]];
}

// We can test for fizzBuzzList as follows. Just run from console using :test
// (see https://www.rascal-mpl.org/docs/Rascal/Tests/)
list[str] fbls = ["1","2","Fizz","4","5","Fizz","7","8","Fizz","10","11","Fizz","13","14","Fizz","16","17","Fizz","19","20","Fizz","22","23","Fizz","25","26","Fizz","28","29","Fizz","31","32","Fizz","34","35","Fizz","37","38","Fizz","40","41","Fizz","43","44","Fizz","46","47","Fizz","49","50","Fizz","52","53","Fizz","55","56","Fizz","58","59","Fizz","61","62","Fizz","64","65","Fizz","67","68","Fizz","70","71","Fizz","73","74","Fizz","76","77","Fizz","79","80","Fizz","82","83","Fizz","85","86","Fizz","88","89","Fizz","91","92","Fizz","94","95","Fizz","97","98","Fizz"];
test bool testfizzBuzzList() = fizzBuzzList() == fbls;


/*
 * Factorial
 * - Let us practice some more writing different implementations for the factorial function
 * - first using ordinary structured programming and recursion (fact1)
 * - second using pattern-based dispatch (fact2)
 * - third using switch (fact3) (see https://www.rascal-mpl.org/docs/Rascal/Statements/Switch/)
 */
 
int fact1(int n) {
  if (n == 0)
    return 1;
  return n * fact1(n-1);
}

int fact2(0) = 1;
int fact2(1) = 1;
default int fact2(int n) = n * fact2(n-1);

int fact3(int n) {
  switch (n) {
    case 0: return 1;
    default: return n * fact3(n-1);
  }
  
}

// Now that we have three implementations, let us write a test let us write a test so they 
// can serve as each other's oracle. For example:
test bool testfactorial0() = fact1(0) == fact2(0);
test bool testfactorial1() = fact1(1) == fact2(1);
test bool testfactorial15() = fact1(15) == fact3(15);
test bool testfactorial100() = fact2(100) == fact3(100);
test bool testfactorial(int n) = n >= 0 && n < 20 ? fact2(n) == fact3(n) : true;


/*
 * Rascal also has comprehensions for generating values.
 * (https://www.rascal-mpl.org/docs/Recipes/BasicProgramming/Comprehensions/)
 * Let us write some examples in the function beloww, you can use println to test the result
 */
 
void comprehensions() {

  // construct a list of squares of integer from 0 to 9 (use range [0..10])
  println([x * x | x <- [0..10]]);
  
  // same, but construct a set
  println({x * x | x <- [0..10]});

  // same, but construct a map
  println((x: x * x | x <- [0..10]));

  // construct a list of factorials from 0 to 9
  println([fact1(x) | x <- [0..10]]);
  
  // same, but now only for even numbers  
  println([fact1(x) | x <- [0..10], x % 2 == 0]);
}
 

/*
 * Pattern matching
 * - fill in the blanks below with pattern match expressions (using :=)
 */
 

void patternMatching() {
  str hello = "Hello World!";
  
  // print all splits of list
  // look at the examples here: https://www.rascal-mpl.org/docs/Rascal/Patterns/List/
  list[int] aList = [1,2,3,4,5];
  for ([*A, *B] := aList) {
    println("<A>,<B>");
  }
  
  // print all partitions of a set
  // loo at th eexamples here: https://www.rascal-mpl.org/docs/Rascal/Patterns/Set/
  set[int] aSet = {1,2,3,4,5};
  for ({*A, *B} := aSet) {
    println("<A>,<B>");
  } 

}  
 
 
 
/*
 * Trees
 * - complete the data type ColoredTree below to represent 
 *   a colored binary tree where each node is either a leaf, a red node, 
 *   or a black node
 * - use the exampleTree() to test your data type in the console
 */
 
 
data ColoredTree
  = leaf(int n) | red(ColoredTree left, ColoredTree right) | black(ColoredTree left, ColoredTree right);


ColoredTree exampleTree()
  =  red(black(leaf(1), red(leaf(2), leaf(3))),
        black(leaf(4), leaf(5)));  
  
  
// write a recursive function summing the leaves
// (use pattern-based dispatch)

int sumLeaves(leaf(n)) = n;
int sumLeaves(red(ColoredTree left, ColoredTree right)) = sumLeaves(left) + sumLeaves(right); // sum for red nodes
int sumLeaves(black(ColoredTree left, ColoredTree right)) = sumLeaves(left) + sumLeaves(right); // sum for black nodes

int sumLeavesWithVisit(ColoredTree t) {
  int c = 0;
  visit(t){
    case leaf(n): c = c + n;
  }
  return c;
}

// same, but now with a for loop and deep match
int sumLeavesWithFor(ColoredTree t) {
  int c = 0;
  for (leaf(int n) := t){
    c = c + n;
  }
  return c;
}

// Below you can find another implementation that uses a reducer and deep match.
// The implementation shows a reducer. Reducers in Rascal resemble the fold function found in 
// most functional languages.
// https://www.rascal-mpl.org/docs/Rascal/Expressions/Reducer/
// It has the following syntax: Reducer = ( <initial value> | <some expression with `it` | <generators> )
int sumLeavesWithReducer(ColoredTree t) = ( 0 | it + i | /leaf(int i) := t );


// Complete the function below that adds 1 to all leaves; use visit + =>
ColoredTree inc1(ColoredTree t) {
  return visit(t){
    case leaf(n) => leaf(n + 1)
  }
}

// Write a test for inc1, run from console using :test
test bool testInc1() = false;

// Define a property for inc1 in the function isInc1, that returns a boolean
// this function should checks if one tree is inc1 of the other
// (without using inc1).
// Use switch on the tupling of t1 and t2 (`<t1, t2>`)
// or pattern based dispatch.
// Hint! The tree also needs to have the same shape!
bool isInc1(ColoredTree t1, ColoredTree t2) {
  switch (<t1, t2>) {
    case <leaf(int n1), leaf(int n2)>:
      return n2 == n1 + 1 || n1 == n2 + 1; // Check if leaf value in t2 is n1 + 1
    case <red(ColoredTree l1, ColoredTree r1), red(ColoredTree l2, ColoredTree r2)>:
      return isInc1(l1, l2) && isInc1(r1, r2); // Check for red nodes
    case <black(ColoredTree l1, ColoredTree r1), black(ColoredTree l2, ColoredTree r2)>:
      return isInc1(l1, l2) && isInc1(r1, r2); // Check for black nodes
    default:
      return false; // Trees do not match in shape or type
  }
}

 
// Write a randomized test for inc1 using the property
// again, execute using :test
test bool testInc1Randomized(ColoredTree t1) = true;