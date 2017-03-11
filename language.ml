
module SS = Set.Make(String);;
type literal =
	  Int 	of int
	| Bool 	of bool
	| Char 	of char
	| String of string
	| Set of SS.t;;


type assignment =
	  StandardAssign;;

type binary_operation =
	  Plus
 	| Minus
 	| Divide
 	| Times;;

	type set_operation =
		Union;;

type expression =
 		Literal 				of literal
 	| Identifier 			of string
 	| BinaryOperation 		of binary_operation * expression * expression
 	| Assignment 			of assignment * string * expression
	| SetConstruction 	of expression list
	| SetOperation 	of set_operation * expression * expression;;

type test_type =
	Equality
	|	LessThan
	| GreaterThan;;

type test =
	  Test of test_type * expression * expression;;

type condition = UnaryCondition of test;;

type statement =
	  Expression 	of expression
	| Output 		of expression
	| If 			of condition * statement list * statement list;;

type program =
	  Program of string list * statement list * statement list;;
