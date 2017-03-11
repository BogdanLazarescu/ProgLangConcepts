
open Language

let equal x y =
	match x, y with
	(* Numerics *)
	| Int x, Int y -> x = y

	(* Characters *)
	| Char x, Char y -> x = y
	| Int x, Char y -> x = (Char.code y)
	| Char x, Int y -> (Char.code x) = y

	(* Booleans *)
	| Bool x, Bool y -> x = y
	| Bool x, Int y -> if x then y <> 0 else y = 0
	| Int x, Bool y -> if y then x <> 0 else x = 0
	| Bool x, Char y -> if x then (Char.code y) <> 0 else (Char.code y) = 0
	| Char x, Bool y -> if y then (Char.code x) <> 0 else (Char.code x) = 0

	(* Strings *)
	| String x, String y -> x = y
	| String x, Int y -> x = (string_of_int y)
	| Int x, String y -> (string_of_int x) = y
	| String x, Char y -> x = (String.make 1 y)
	| Char x, String y -> (String.make 1 x) = y
	| String x, Bool y -> if y then (String.length x) <> 0 else (String.length x) = 0
	| Bool x, String y -> if x then (String.length y) <> 0 else (String.length y) = 0

	(* Sets *)
	| Set x, Set y -> SS.equal x y
	| _, _ -> false

let less_than x y =
	match x, y with
	(* Numerics *)
	| Int x, Int y -> x < y


	(* Characters *)
	| Char x, Char y -> x < y
	| Int x, Char y -> x < (Char.code y)
	| Char x, Int y -> (Char.code x) < y


	(* Booleans *)
	| Bool x, Bool y -> if x then false else y
	| Bool x, Int y -> if x then y < 1 else y < 0
	| Int x, Bool y -> if y then x < 1 else x < 0
	| Bool x, Char y -> if x then (Char.code y) < 1 else (Char.code y) < 0
	| Char x, Bool y -> if y then (Char.code x) < 1 else (Char.code x) < 0

	(* Strings *)
	| String x, String y -> x < y
	| String x, Int y -> x < (string_of_int y)
	| Int x, String y -> (string_of_int x) < y
	| String x, Char y -> x < (String.make 1 y)
	| Char x, String y -> (String.make 1 x) < y
	| String x, Bool y -> if y then (String.length x) = 0 else false
	| Bool x, String y -> if x then false else (String.length y) > 0

	(* Sets *)
	| Set x, Set y -> (SS.compare x y) < 0
	| _, _ -> false

let greater_than x y =
	match x, y with
	(* Numerics *)
	| Int x, Int y -> x > y

	(* Characters *)
	| Char x, Char y -> x > y
	| Int x, Char y -> x > (Char.code y)
	| Char x, Int y -> (Char.code x) > y

	(* Booleans *)
	| Bool x, Bool y -> if x then not y else false
	| Bool x, Int y -> if x then y > 1 else y > 0
	| Int x, Bool y -> if y then x > 1 else x > 0
	| Bool x, Char y -> if x then (Char.code y) > 1 else (Char.code y) > 0
	| Char x, Bool y -> if y then (Char.code x) > 1 else (Char.code x) > 0

	(* Strings *)
	| String x, String y -> x > y
	| String x, Int y -> x > (string_of_int y)
	| Int x, String y -> (string_of_int x) > y
	| String x, Char y -> x > (String.make 1 y)
	| Char x, String y -> (String.make 1 x) > y
	| String x, Bool y -> if y then false else (String.length x) > 0
	| Bool x, String y -> if x then (String.length y) = 0 else false

	(* Sets *)
	| Set x, Set y -> (SS.compare x y) > 0
	| _, _ -> false
