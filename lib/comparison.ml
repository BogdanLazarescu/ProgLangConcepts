
open Language

let equal x y =
	match x, y with
	(* Comapre int *)
	| Int x, Int y -> x = y

	(* Compare char*)
	| Char x, Char y -> x = y
	| Int x, Char y -> x = (Char.code y)
	| Char x, Int y -> (Char.code x) = y

	(* Compare bool *)
	| Bool x, Bool y -> x = y
	| Bool x, Int y -> if x then y <> 0 else y = 0
	| Int x, Bool y -> if y then x <> 0 else x = 0
	| Bool x, Char y -> if x then (Char.code y) <> 0 else (Char.code y) = 0
	| Char x, Bool y -> if y then (Char.code x) <> 0 else (Char.code x) = 0

	(* Compare String*)
	| String x, String y -> x = y
	| String x, Int y -> x = (string_of_int y)
	| Int x, String y -> (string_of_int x) = y
	| String x, Char y -> x = (String.make 1 y)
	| Char x, String y -> (String.make 1 x) = y
	| String x, Bool y -> if y then (String.length x) <> 0 else (String.length x) = 0
	| Bool x, String y -> if x then (String.length y) <> 0 else (String.length y) = 0

	(* Compare Set*)
	| Set x, Set y -> SS.equal x y
	| _, _ -> false

	let greater_than x y =
		match x, y with
		(* Compare int*)
		| Int x, Int y -> x > y

		(* Comapre char *)
		| Char x, Char y -> x > y
		| Int x, Char y -> x > (Char.code y)
		| Char x, Int y -> (Char.code x) > y

		(* Compare bool*)
		| Bool x, Bool y -> if x then not y else false
		| Bool x, Int y -> if x then y > 1 else y > 0
		| Int x, Bool y -> if y then x > 1 else x > 0
		| Bool x, Char y -> if x then (Char.code y) > 1 else (Char.code y) > 0
		| Char x, Bool y -> if y then (Char.code x) > 1 else (Char.code x) > 0

		(* Compare string *)
		| String x, String y -> x > y
		| String x, Int y -> x > (string_of_int y)
		| Int x, String y -> (string_of_int x) > y
		| String x, Char y -> x > (String.make 1 y)
		| Char x, String y -> (String.make 1 x) > y
		| String x, Bool y -> if y then false else (String.length x) > 0
		| Bool x, String y -> if x then (String.length y) = 0 else false

		(* Compare set*)
		| Set x, Set y -> (SS.compare x y) > 0
		| _, _ -> false

let less_than x y =
	match x, y with
	(* Compare int*)
	| Int x, Int y -> x < y

	(* Compare char *)
	| Char x, Char y -> x < y
	| Int x, Char y -> x < (Char.code y)
	| Char x, Int y -> (Char.code x) < y


	(* Compare bool *)
	| Bool x, Bool y -> if x then false else y
	| Bool x, Int y -> if x then y < 1 else y < 0
	| Int x, Bool y -> if y then x < 1 else x < 0
	| Bool x, Char y -> if x then (Char.code y) < 1 else (Char.code y) < 0
	| Char x, Bool y -> if y then (Char.code x) < 1 else (Char.code x) < 0

	(* Compare string *)
	| String x, String y -> x < y
	| String x, Int y -> x < (string_of_int y)
	| Int x, String y -> (string_of_int x) < y
	| String x, Char y -> x < (String.make 1 y)
	| Char x, String y -> (String.make 1 x) < y
	| String x, Bool y -> if y then (String.length x) = 0 else false
	| Bool x, String y -> if x then false else (String.length y) > 0

	(* Comapre Set *)
	| Set x, Set y -> (SS.compare x y) < 0
	| _, _ -> false
