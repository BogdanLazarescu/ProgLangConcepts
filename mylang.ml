
open Language
open Interpreter
open Input
open Err

let _ =
	try
		let srcfile = Sys.argv.(1) in
			try
				(*Read the input file:*)
				let input = Lexing.from_channel (open_in srcfile) in
				(*Read the user program:*)
				let program = Parser.main Lexer.token input in
				(*Call the interpreter to interpret the program and output the result*)
				let interpreter = new Interpreter.interpreter in
					interpreter#run program (Input.parse stdin);
					(*print the output resulted after the program execution*)
					print_endline interpreter#get_output
			with
				| Invalid_argument e ->
					prerr_endline ("Invalid arg: " ^ e)
				| Sys_error e ->
					prerr_endline ("Source file unavailable " ^ e)
				| Parsing.Parse_error ->
					prerr_endline "Syntax error"
				| Input.Input_format_error e ->
					prerr_endline ("Input format error: " ^ e)
				| Err.Lexing_error e ->	prerr_endline e
				| Err.Parse_error e ->	prerr_endline e
				| Err.Fatal e -> prerr_endline ("Fatal error: " ^ e)
				| Err.Undeclared_identifier i ->
					prerr_endline ("Undeclared identifier used " ^ i)
	with
		Invalid_argument e ->
			prerr_endline "No input specified.\n"
