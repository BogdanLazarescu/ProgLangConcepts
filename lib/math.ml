
open Language
open Comparison

let union x y =
	match x, y with
	| Set x, Set y -> SS.union x y
	| _, _ -> raise (Invalid_argument "you may only perform SUnion on sets of Set type.")

let intersection x y =
	match x, y with
	| Set x, Set y -> SS.inter x y
	|_, _ -> raise (Invalid_argument "you may only perform SInter on sets of Set type.")

let difference x y =
	match x, y with
	| Set x, Set y -> SS.diff x y
	|_, _ -> raise (Invalid_argument "you may only perform SDiff on sets of Set type.")

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
	|_, _ -> raise (Invalid_argument "you may only perform SConcat on sets of Set type.")

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

let unary_minus x =
	match x with
	| Int x -> Int (0 - x)
	| _ -> raise (Invalid_argument "you may only perform math operations on numeric types.")
