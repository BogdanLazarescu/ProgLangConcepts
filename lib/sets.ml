
open Language
open Errors

let rec string_of_set set=
 	let rec internal = function
	| [] -> ""
	| [x] -> x
	| [x;y] -> x ^ ", " ^y
	| values :: rest -> values ^ ", " ^ internal rest
in internal (SS.elements set)

	let rec string_of_set_elements set=
		let rec internal = function
		| [] -> ""
		| [x] -> x
		| [x;y] -> x ^ " " ^y
		| values :: rest -> values ^ " " ^ internal rest
		in internal (SS.elements set)

let rec string_of_literal = function
		| Int n -> string_of_int n
		| Bool b -> string_of_bool b
		| Char c -> String.make 1 c
		| Set s -> ("{" ^ string_of_set s ^ "}" )
		| String s -> s

let rec unbraced_string_of_set s =
		 string_of_set s

let out set value =
		Set (
			match value with
			| Set s -> SS.add (string_of_literal (Set(s))) set
			| value -> SS.add (string_of_literal value) set
		)

let print_set s =
			print_string "{";
     print_string (string_of_set s);
		 print_string "}"

let rec skipSet number set =
	match number with
		 | 0 -> Set set
		 | n ->
		 		try
		 			skipSet (n - 1) (SS.remove (SS.min_elt set) set)
		 		with
		 			Failure e -> raise End_of_stream

let rec kleenStar str number =
	match number with
			| 0 -> ""
			| n ->
				str ^ (kleenStar str (n-1))

let rec kleenStarSet str number=
	match number with
			| 0 -> SS.empty
			| n -> (SS.add (kleenStar str n) (kleenStarSet str (n-1)))

let rec set_of_first_nth set n =
	if(n >= (SS.cardinal set)) then
		set
	else
			set_of_first_nth (SS.remove (SS.max_elt set) set) n

	let rec print_list = function
	[] -> ()
	| e::l -> print_string e ; print_string " " ; print_list l

	let int_of_literal x  =
		match x with
		| Int x -> x
		|_ -> raise (Invalid_argument "your k parameter should be of type int")
