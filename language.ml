
type literal =
	  Int 	of int
	| Bool 	of bool
	| Char 	of char
	| String of string
	| Stream of literal list;;

type assignment =
	  StandardAssign;;

type binary_operation =
	  Plus
 	| Minus
 	| Divide
 	| Times;;

type unary_operation =
	  UnaryMinus;;

type expression =
 	  Literal 				of literal
 	| Identifier 			of string
 	| StreamAccess 			of string * int
 	| Application 			of string * expression list
 	| ScopedApplication 	of string * string * expression list
 	| BinaryOperation 		of binary_operation * expression * expression
 	| UnaryOperation 		of unary_operation * expression
 	| Assignment 			of assignment * string * expression
 	| StreamConstruction 	of expression list;;

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
