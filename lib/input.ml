
open Str
open Language

exception Input_format_error of string;;

let string_of_char str =
	if String.length str = 1
	then
		String.get str 0
	else
		raise (Failure "character not in correct format - expected single character")

let string_of_quoted_char str =
	if String.get str 0 = '\''
		&& String.get str ((String.length str) - 1) = '\''
		then
			String.get str 1
		else
			raise (Failure "character not in correct format - expected 'char'")

let string_of_braced_set str =
	if String.get str 0 = '{'
		&& String.get str ((String.length str) - 1) = '}'
		then
			String.trim (String.sub str 1 ((String.length str) - 2))
		else
			raise (Failure "character not in correct format - expected 'char'")

let list_of_braced_set str =
		Str.split (Str.regexp "[\t \t]*,[ \t]*") (string_of_braced_set str)

let repalce_with_emptyword s =
		if(s = ":") then ""
		else s

let replace_with_emptyword_in_list l =
		List.map repalce_with_emptyword l

let literal_of_string str =
	try
		Int (int_of_string str)
	with Failure bad_format ->
	try
		Bool (bool_of_string str)
	with Invalid_argument invalid_argument ->
	try
		Char (string_of_char str)
	with Failure bad_format ->
	try
		Char (string_of_quoted_char str)
	with Failure bad_format ->
	try
		Set (SS.of_list
			(replace_with_emptyword_in_list (list_of_braced_set str)))
	with Failure bad_format ->
		raise (Failure ("Unable to parse " ^ str))

let parse channel =
	try
		let literals = ref [] in
			try

			 	while true do
			 		(* Get line, trim it, split it on space & convert to ints *)
					let trimmed = String.trim (input_line channel) in
					let inp =literal_of_string trimmed in
			 				begin
			 					literals := inp :: !literals;
							end
			 	done;
			 	[]
			with
				| Failure e ->
					raise (Input_format_error (
						"Input data " ^
						(string_of_int ((List.length !literals) + 1)) ^
						" contains an invalid element (non-integer)"))

				| End_of_file -> List.rev !literals

	with
		| End_of_file ->
			raise (Input_format_error
				"Not enough data in input, only one line defined")
