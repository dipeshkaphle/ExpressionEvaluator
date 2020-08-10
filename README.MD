# Expression Evaluator

* This is capable of evaluating Arithmetic as well as Logical (Boolean) expressions
* Type is inferred by the program on the basic of first operation
* Using parentheses is preferred so as to ensure the correct order of evaluation
* The evaluation can result in either a Double value or a Boolean value. Or it will tell us it cant evaluate the expression if its invalid (like `"False + True"` )
* Capable of command line evaluation (Example: `eval "True & False ` will print `False`)
* Running the program without providing any arguments will start up a `REPL`

## Demo
![Images](images/repl.png)
* REPL

![Images](images/LogicalEvaluationsDemo.png)
* Logical Expression evaluation

![Images](images/ArithmeticEvaluations.png) 
* Arithmetic Expression evaluation

![Images](images/comparision.png)
* Comparisions 

## Operations
* `+`
* `-`
* `*`
* `/`
* `%`  -> will do float mod
* `^`  -> power
* `&` -> Logical And
* `|` -> Logical Or
* `~` -> Logical Not
* `-->` -> Logical Implies
* `<-->` -> Logical Double Implies
* `^^` -> Logical Xor
* `<` 
* `<=`
* `>` 
* `>=`
* `==`
* `!=`
* `Sin`
* `Cos`
* `Tan`
* `Log` followed by logbase and and number
* `Ln` -> Natural Log


## Note
+ You cant assign boolean values to variables 
+ The comparision operators on Boolean values do not work properly (Will be fixed soon)



## Requirements to run the program
* `Glasgow Haskell Compiler` or `ghc` must be installed 

## Compilation
* Run `ghc eval.hs` 
* This will produce a binary `eval` and you can run it locally with `./eval` 
