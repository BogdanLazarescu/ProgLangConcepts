
open Language
open Err

let replace_emptyword s =
	if(s = "") then ":"
	else s

let rec string_of_set set=
 	let rec internal = function
	| [] -> ""
	| [x] -> replace_emptyword x
	| [x;y] -> replace_emptyword x ^ ", " ^y
	| values :: rest -> replace_emptyword values ^ ", " ^ internal rest
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
		| Set s -> ("{" ^ string_of_set s ^ "}" )
		| String s -> s

let rec unbraced_string_of_set s =
		 string_of_set s

let out value =
			match value with
			| Set s -> s
			| value -> SS.add (string_of_literal value) SS.empty

(*return a set containing just first n elements of it*)
let rec set_of_first_nth n set =
	if(n >= (SS.cardinal set)) then
		Set(set)
	else
			set_of_first_nth n (SS.remove (SS.max_elt set) set)

  let rec string_of_set_list = function
    [] ->""
    | e::l -> (string_of_literal e)^ "\n" ^ (string_of_set_list l)

let string_of_set_list set n =
	let lst= (List.map (set_of_first_nth n) set) in
    string_of_set_list lst

	let int_of_literal x  =
		match x with
		| Int x -> x
		|_ -> raise (Invalid_argument "your parameter should be of type int")

let set_of_list list =
	let set = SS.empty in
    List.fold_right SS.add list set
