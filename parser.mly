%{

open Language
open Errors

%}

%token <int> INT
%token <bool> BOOL
%token <char> CHAR
%token <string> IDENT
%token <string> STRING

%token EOF EOL
%token TRUE FALSE
%token PLUS MINUS TIMES DIVIDE
%token LPAREN RPAREN
%token LBRACKET RBRACKET CURRENT
%token LCBRACKET RCBRACKET
%token LESSTHAN GREATERTHAN EQ
%token IF THEN ELSE ENDIF
%token USING BEGIN LOOP SKIP IN OUT
%token ASSIGN
%token COMMA

%right ASSIGN
%left EQ
%left GREATERTHAN	LESSTHAN
%left PLUS MINUS
%left TIMES DIVIDE

%start main
%type <Language.program> main

%%

main:
	  USING identifier_list BEGIN statement_list LOOP statement_list EOF 	{ Program ($2, $4, $6) }
	| USING identifier_list LOOP statement_list EOF 						{ Program ($2, [], $4) }
	| USING identifier_list BEGIN statement_list EOF 						{ Program ($2, $4, []) }
	| error {
			parse_err "Malformed program structure, 'with' is required, 'begin' and 'loop' are optional but must have statements.";
			Program ([], [], [])
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
	  SKIP				{ Skip (1, "") }
	| SKIP IN IDENT 	{ Skip (1, $3) }
	| SKIP INT			{ Skip ($2, "") }
	| SKIP INT IN IDENT { Skip ($2, $4) }
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

expression:
	  literal 										{ Literal $1 }
	| IDENT 										{ Identifier $1 }
	| LPAREN expression RPAREN						{ $2 }
	| assignment 									{ $1 }
	| binary_operation 								{ $1 }
	| stream_construction 							{ $1 }
	| IDENT LPAREN expression_list RPAREN 			{ Application ($1, $3) }
	| IDENT LPAREN RPAREN 							{ Application ($1, []) }
	| IDENT LBRACKET INT RBRACKET 					{ StreamAccess ($1, $3) }
	| IDENT CURRENT 								{ StreamAccess ($1, 0) }
	| IDENT shift_list 								{ StreamAccess ($1, $2) }
	| error {
			parse_err "This expression is malformed.";
			Literal (Int 0)
		}
;

shift_list:
	  GREATERTHAN 			{ 1 }
	| shift_list GREATERTHAN { $1 + 1 }
	| error {
			parse_err "Invalid shift operation. Only expecting > here.";
			0
		}
;

stream_construction:
	  LBRACKET expression_list RBRACKET { StreamConstruction $2 }
	| LBRACKET RBRACKET 				{ StreamConstruction [] }
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

condition:
	  test 						{ UnaryCondition $1 }
;

test:
		expression EQ expression 	{ Test (Equality, $1, $3) }
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
