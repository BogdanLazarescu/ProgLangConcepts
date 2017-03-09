
module SS = Set.Make(String);;
type literal =
	  Int 	of int
	| Bool 	of bool
	| Char 	of char
	| String of string
	| Set of SS.t
	| Stream of literal list;;


type assignment =
	  StandardAssign;;

type binary_operation =
	  Plus
 	| Minus
 	| Divide
 	| Times;;


type expression =
 	  Literal 				of literal
 	| Identifier 			of string
 	| StreamAccess 			of string * int
 	| Application 			of string * expression list
 	| ScopedApplication 	of string * string * expression list
 	| BinaryOperation 		of binary_operation * expression * expression
 	| Assignment 			of assignment * string * expression
 	| StreamConstruction 	of expression list
	| SetConstruction 	of expression list;;

type test_type =
	Equality
	|	LessThan
	| GreaterThan;;

type test =
	  Test of test_type * expression * expression;;

type condition = UnaryCondition of test;;

type statement =
	  Expression 	of expression
	| Skip 			of int * string
	| Output 		of expression
	| If 			of condition * statement list * statement list;;

type program =
	  Program of string list * statement list * statement list;;
