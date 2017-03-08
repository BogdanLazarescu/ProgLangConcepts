{
	open Parser
	open Lexing
	open Errors
}

let white_space = [' ' '\t']
let digit = ['0'-'9']
let int = digit +
let float = (int '.' int) | (int ['f' 'F'])
let char = ''' [^ '\n'] '''
let bool = "true" | "false"
let alphanum = ['a'-'z' 'A'-'Z' '0'-'9' '_']*

rule token = parse

	(* Lines & Whitespace *)

	  white_space 			{ token lexbuf }
	| '\n' 					{ new_line lexbuf; token lexbuf }
	| eof 					{ EOF }
	| ';' 					{ EOL }

	(* Literals *)

	| int 					{ INT(int_of_string (lexeme lexbuf)) }
	| char 					{ CHAR(String.get (lexeme lexbuf) 1) }
	| bool 					{ BOOL(bool_of_string (lexeme lexbuf)) }
	| "true" 				{ TRUE }
	| "false" 				{ FALSE }
	| '"' [^ '"']* '"'		{ let s = lexeme lexbuf in STRING(String.sub s 1 ((String.length s) - 2)) }

	(* Math Operators *)

	| '+' 					{ PLUS }
	| '-' 					{ MINUS }
	| '*' 					{ TIMES }
	| '/' 					{ DIVIDE }

	(* Expression Grouping *)

	| '(' 					{ LPAREN }
	| ')' 					{ RPAREN }
	| '{'						{ LCBRACKET}
	| '}'						{	RCBRACKET}

	(* Stream Access *)

	| '[' 					{ LBRACKET }
	| ']' 					{ RBRACKET }
	| '~' 					{ CURRENT }

	(* Conditionals *)

	| "==" 					{ EQ }
	| '<' 					{ LESSTHAN }
	| '>' 					{ GREATERTHAN }

	(* Control Structures *)

	| "if" 					{ IF }
	| "then" 				{ THEN }
	| "else" 				{ ELSE }
	| "endif" 				{ ENDIF }

	(* Keywords *)

	| "with"				{ USING }
	| "begin"				{ BEGIN }
	| "loop" 				{ LOOP }
	| "skip"        { SKIP }
	| "in"					{ IN }
	| "out" 				{ OUT }

	| '=' 					{ ASSIGN }

	(* Identifiers *)

	| ['a'-'z' 'A'-'Z' '_'] alphanum* 	{ IDENT(lexeme lexbuf) }

	(* Function Application Expression Lists *)

	| ',' 					{ COMMA }

	(* Scoped Function Application *)

	| '.' 					{ DOT }

	(* Error Reporting *)

	| _ 					{ lexing_error ("Unrecognized character: " ^ (lexeme lexbuf)) (lexeme_start_p lexbuf) (lexeme_end_p lexbuf) }
