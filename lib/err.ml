
open Lexing
open Parsing

exception Fatal of string;;
exception Undeclared_identifier of string;;

exception Lexing_error of string;;
exception Parse_error of string;;

let get_source_line line =
	let file = open_in Sys.argv.(1) in
		let rec internal current =
			let sourceline = input_line file in
				if line = current then
					sourceline
				else
					internal (current + 1)
		in
			internal 1

let generate_error errortype message x =
	let line = Str.global_replace (Str.regexp "\t") " " (get_source_line x.pos_lnum) in
	let loc = Printf.sprintf " [Line %d] -> " x.pos_lnum in

		errortype ^ " Error\n" ^ loc ^ line ^ "\n" ^ " " ^ message ^ "\n"

let parse_err message =
	raise (Parse_error (
		generate_error
			"Syntax"
			message
			(rhs_start_pos 1)))

let lexing_error token start=
	raise (Lexing_error (generate_error "Lexing" token start))
