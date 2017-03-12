
open Language
open Errors

let rec string_of_set = function
	| values :: rest -> values ^ ", " ^ string_of_set rest
	| [x;y] -> x ^ ", " ^y
	| [] -> ""

let rec string_of_literal = function
		| Int n -> string_of_int n
		| Bool b -> string_of_bool b
		| Char c -> String.make 1 c
		| Set s -> string_of_set (SS.elements s)
		| String s -> s

let out set value =
		Set (
			match value with
			| Set s -> SS.union set s
			| value -> SS.add (string_of_literal value) set
		)

let print_set s =
			print_string "{";
     print_string (string_of_set (SS.elements s));
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
