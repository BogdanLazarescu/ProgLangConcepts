
open Language

let union x y =
	match x, y with
	| Set x, Set y -> SS.union x y
	| _, _ -> raise (Invalid_argument "The arguments of SUnion must be of Set type.")

let intersection x y =
	match x, y with
	| Set x, Set y -> SS.inter x y
	|_, _ -> raise (Invalid_argument "The arguments of SInter must be of Set type.")

let difference x y =
	match x, y with
	| Set x, Set y -> SS.diff x y
	|_, _ -> raise (Invalid_argument "The arguments of SDiff must be of Set type.")

let strCC e1 e2 = e1 ^ e2

let concat s el =
	SS.of_list (List.map (strCC el) (SS.elements s))

let rec lst_union = function
	| [] -> SS.empty
	| [x] -> x
	| s :: rest ->SS.union s (lst_union rest)

let concatenation x y =
	match x, y with
	| Set x, Set y ->lst_union (List.map (concat y) (SS.elements x))
	|_, _ -> raise (Invalid_argument "The arguments of SConcat must be of Set type.")

let plus x y =
	match x, y with
	| Int x, Int y -> Int (x + y)
	| _, _ -> raise (Invalid_argument "The arguments of '+' must have numeric types.")

let minus x y =
	match x, y with
	| Int x, Int y -> Int (x - y)
	| _, _ -> raise (Invalid_argument "The arguments of '-' must have numeric types.")

let times x y =
	match x, y with
	| Int x, Int y -> Int (x * y)
	| _, _ -> raise (Invalid_argument "The arguments of '*' must have numeric types.")

let divide x y =
	match x, y with
	| Int x, Int y -> Int (x / y)
	| _, _ -> raise (Invalid_argument "The arguments of '/' must have numeric types.")

(*Math comparisons between different types*)
	let equal x y =
		match x, y with
		(* Comapre int *)
		| Int x, Int y -> x = y

		(* Compare bool *)
		| Bool x, Bool y -> x = y
		| Bool x, Int y -> if x then y <> 0 else y = 0
		| Int x, Bool y -> if y then x <> 0 else x = 0

		(* Compare String*)
		| String x, String y -> x = y
		| String x, Int y -> x = (string_of_int y)
		| Int x, String y -> (string_of_int x) = y
		| String x, Bool y -> if y then (String.length x) <> 0 else (String.length x) = 0
		| Bool x, String y -> if x then (String.length y) <> 0 else (String.length y) = 0

		(* Compare Set*)
		| Set x, Set y -> SS.equal x y
		| _, _ -> false

		let greater_than x y =
			match x, y with
			(* Compare int*)
			| Int x, Int y -> x > y

			(* Compare bool*)
			| Bool x, Bool y -> if x then not y else false
			| Bool x, Int y -> if x then y > 1 else y > 0
			| Int x, Bool y -> if y then x > 1 else x > 0

			(* Compare string *)
			| String x, String y -> x > y
			| String x, Int y -> x > (string_of_int y)
			| Int x, String y -> (string_of_int x) > y
			| String x, Bool y -> if y then false else (String.length x) > 0
			| Bool x, String y -> if x then (String.length y) = 0 else false

			(* Compare set*)
			| Set x, Set y -> (SS.compare x y) > 0
			| _, _ -> false

	let less_than x y =
		match x, y with
		(* Compare int*)
		| Int x, Int y -> x < y

		(* Compare bool *)
		| Bool x, Bool y -> if x then false else y
		| Bool x, Int y -> if x then y < 1 else y < 0
		| Int x, Bool y -> if y then x < 1 else x < 0

		(* Compare string *)
		| String x, String y -> x < y
		| String x, Int y -> x < (string_of_int y)
		| Int x, String y -> (string_of_int x) < y
		| String x, Bool y -> if y then (String.length x) = 0 else false
		| Bool x, String y -> if x then false else (String.length y) > 0

		(* Compare Set *)
		| Set x, Set y -> (SS.compare x y) < 0
		| _, _ -> false
