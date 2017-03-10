
open Language
open Comparison
let union x y =
	match x, y with
	| Set x, Set y -> SS.union x y
	| _, _ -> raise (Invalid_argument "you may only perform math operations on numeric types.")


let plus x y =
	match x, y with
	| Int x, Int y -> Int (x + y)
	| _, _ -> raise (Invalid_argument "you may only perform math operations on numeric types.")

let minus x y =
	match x, y with
	| Int x, Int y -> Int (x - y)
	| _, _ -> raise (Invalid_argument "you may only perform math operations on numeric types.")

let times x y =
	match x, y with
	| Int x, Int y -> Int (x * y)
	| _, _ -> raise (Invalid_argument "you may only perform math operations on numeric types.")

let divide x y =
	match x, y with
	| Int x, Int y -> Int (x / y)
	| _, _ -> raise (Invalid_argument "you may only perform math operations on numeric types.")


let min x y =
	if (Comparison.less_than x y) then x else y

let max x y =
	if (Comparison.greater_than x y) then x else y

let unary_minus x =
	match x with
	| Int x -> Int (0 - x)
	| _ -> raise (Invalid_argument "you may only perform math operations on numeric types.")

let rec average values =
	divide (List.fold_left plus (Int 0) values) (Int (List.length values))

let round = function
	| Int n -> Int n
	| _ -> raise (Invalid_argument "You can only round numeric types")

let floor = function
	| _ -> raise (Invalid_argument "You can only floor numeric types")

let ceil = function
	| _ -> raise (Invalid_argument "You can only ceil numeric types")
