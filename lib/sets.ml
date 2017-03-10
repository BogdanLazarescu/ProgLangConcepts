
open Language
open Errors

	let out stream value =
		Set (
			match value with
			| Set s -> SS.union stream s
			| value -> SS.add (Streams.string_of_literal value) stream
		)

		let print_space_el s =
			print_string s;
			print_string ", "

		let print_set s =
			print_string "{";
     SS.iter print_space_el s;
		 print_string "\b\b}"

		 let rec skipSet number set =
		 	match number with
		 		| 0 -> Set set
		 		| n ->
		 			try
		 				skipSet (n - 1) (SS.remove (SS.min_elt set) set)
		 			with
		 				Failure e -> raise End_of_stream
