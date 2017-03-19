{
	open Parser
	open Lexing
	open Errors
}

let white_space = [' ' '\t']
let digit = ['0'-'9']
let int = digit +
let char = ''' [^ '\n'] '''
let bool = "true" | "false"
let alphanum = ['a'-'z' 'A'-'Z' '0'-'9' '_']*

rule token = parse

	  white_space 			{ token lexbuf }
	| '\n' 					{ new_line lexbuf; token lexbuf }
	| eof 					{ EOF }
	| ';' 					{ EOL }

	| int 					{ INT(int_of_string (lexeme lexbuf)) }
	| char 					{ CHAR(String.get (lexeme lexbuf) 1) }
	| bool 					{ BOOL(bool_of_string (lexeme lexbuf)) }
	| "true" 				{ TRUE }
	| "false" 				{ FALSE }
	| '"' [^ '"']* '"'		{ let s = lexeme lexbuf in STRING(String.sub s 1 ((String.length s) - 2)) }

	| '+' 					{ PLUS }
	| '-' 					{ MINUS }
	| '*' 					{ TIMES }
	| '/' 					{ DIVIDE }

	| '(' 					{ LPAREN }
	| ')' 					{ RPAREN }
	| '{'						{ LCBRACKET}
	| '}'						{	RCBRACKET}

	| "SUnion" 					{SETUNION}
	| "SInter"					{SETINTER}
	| "SDiff"						{SETDIFF}
	| "SConcat"					{SETCONCAT}

	| "==" 					{ EQUAL }
	| '<' 					{ LESSTHAN }
	| '>' 					{ GREATERTHAN }

	| "if" 					{ IF }
	| "then" 				{ THEN }
	| "else" 				{ ELSE }
	| "endif" 				{ ENDIF }

	| "sets"				{ USE}
	| "begin"				{ BEGIN }
	| "loop" 				{ LOOP }
	| "out" 				{ OUT }

	| '=' 					{ ASSIGN }


	| ['a'-'z' 'A'-'Z' '_'] alphanum* 	{ IDENT(lexeme lexbuf) }

	(* Function Application Expression Lists *)

	| ',' 					{ COMMA }

	(* Error Reporting *)

	| _ 					{ lexing_error ("Unrecognized character: " ^ (lexeme lexbuf)) (lexeme_start_p lexbuf) (lexeme_end_p lexbuf) }
