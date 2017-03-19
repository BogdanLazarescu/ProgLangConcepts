%{

open Language
open Errors

%}

%token <int> INT
%token <bool> BOOL
%token <char> CHAR
%token <string> IDENT
%token <string> STRING

%token SETUNION SETDIFF SETINTER SETCONCAT
%token EOF EOL
%token TRUE FALSE
%token PLUS MINUS TIMES DIVIDE
%token LPAREN RPAREN
%token LCBRACKET RCBRACKET
%token LESSTHAN GREATERTHAN EQUAL
%token IF THEN ELSE ENDIF
%token USE BEGIN LOOP OUT
%token ASSIGN
%token COMMA

%right ASSIGN
%left SETUNION SETDIFF SETINTER SETCONCAT
%left EQUAL
%left GREATERTHAN	LESSTHAN
%left PLUS MINUS
%left TIMES DIVIDE

%start main
%type <Language.program> main

%%

main:
		USE identifier_list BEGIN statement_list LOOP int_statement statement_list EOF 	{ Program ($2, $4, $7, $6) }
	| USE identifier_list LOOP int_statement statement_list EOF 				{ Program ($2, [], $5, $4) }
	| USE identifier_list BEGIN statement_list EOF 						{ Program ($2, $4, [], Literal(Int 0)) }
	| error {
			parse_err "Malformed program structure, 'sets' is required, 'begin' and 'loop' are optional but must have statements.";
			Program ([], [], [],  Literal(Int 0))
		}
;

identifier_list:
	  IDENT 						{ [ $1 ] }
	| IDENT COMMA identifier_list 	{ $1 :: $3 }
	| error {
			parse_err "Your 'with' list is malformed. Expected another list element here.";
			[]
		}
;

statement_list:
	  statement 				{ [ $1 ] }
	| statement statement_list 	{ $1 :: $2 }
	| error {
			parse_err "Not expecting program block here. Statement list expected after begin or loop.";
			[]
		}
;

statement:
	  expression EOL 		{ Expression $1 }
	| control_statement EOL { $1 }
	| flow_statement 		{ $1 }
	| error EOL {
			parse_err "This statement is malformed.";
			Expression (Literal (Int 0))
		}
;

control_statement:
	| OUT expression 	{ Output $2 }
;

flow_statement:
	  IF condition THEN statement_list ELSE statement_list ENDIF 	{ If ($2, $4, $6) }
	| IF condition THEN statement_list ENDIF 						{ If ($2, $4, []) }
;

expression_list:
	  expression 						{ [ $1 ] }
	| expression COMMA expression_list 	{ $1 :: $3 }
;

int_statement:
	INT 										{ Literal(Int $1) }
| IDENT 									{ Identifier $1 }
| error {
	parse_err "This expression is malformed.";
	Literal(Int 0)
}
;

expression:
	  literal 										{ Literal $1 }
	| IDENT 										{ Identifier $1 }
	| LPAREN expression RPAREN						{ $2 }
	| assignment 									{ $1 }
	| set_operation 							{ $1 }
	| binary_operation 								{ $1 }
	| set_construction 							{ $1 }
	| error {
			parse_err "This expression is malformed.";
			Literal (Int 0)
		}
;

set_construction:
	LCBRACKET expression_list RCBRACKET { SetConstruction $2 }
	| LCBRACKET RCBRACKET 				{ SetConstruction [] }
	;

assignment:
	  IDENT ASSIGN expression 		{ Assignment (StandardAssign, $1, $3) }
;

binary_operation:
	  expression PLUS expression 	{ BinaryOperation (Plus, $1, $3)  }
	| expression MINUS expression 	{ BinaryOperation (Minus, $1, $3) }
	| expression TIMES expression 	{ BinaryOperation (Times, $1, $3) }
	| expression DIVIDE expression 	{ BinaryOperation (Divide, $1, $3) }
;

set_operation:
		expression SETUNION expression { SetOperation (Union, $1, $3) }
	| expression SETINTER expression { SetOperation (Intersection, $1, $3)}
	| expression SETDIFF expression {SetOperation (Difference, $1, $3)}
	| expression SETCONCAT expression {SetOperation (Concatenation, $1, $3)}
	;

condition:
	  test 						{ UnaryCondition $1 }
;

test:
		expression EQUAL expression 	{ Test (Equality, $1, $3) }
	| expression LESSTHAN expression 	{ Test (LessThan, $1, $3) }
	| expression GREATERTHAN expression 	{ Test (GreaterThan, $1, $3) }
	| error {
			parse_err "This test is malformed.";
			Test (Equality, Literal (Int 0), Literal (Int 0))
		}
;

literal:
	  INT 		{ Int $1 }
	| CHAR 		{ Char $1 }
	| BOOL 		{ Bool $1 }
	| STRING 	{ String $1 }
;
