open Language
open Errors
open Math
open Comparison
open Input
open Sets

class interpreter =
	object (this)

		val mutable bindings = ([] : (string * literal) list)
		val mutable inputs = ([] : string list)
		val mutable outputList = ([] : SS.t list);

		method get_output =
			Sets.string_of_set_list outputList  (Sets.int_of_literal (this#read_bind "k"))

			method define_output =
				outputList <- []

		method read_bind ident =
			try
				List.assoc ident bindings
			with
				Not_found ->
					raise (Undeclared_identifier ident)

		method update_binding ident value =
			bindings <- (ident, value) :: List.remove_assoc ident bindings;
			value

		method update_output set =
			outputList <- outputList@[set];

		method define_sets ident_list literals_list =
			try
				match ident_list with
					| ident :: rest ->
						bindings <- (ident, (List.nth literals_list (List.length bindings))) :: bindings;
						inputs <- ident :: inputs;
						this#define_sets rest literals_list
					| [] -> ()
			with
				| Failure e -> raise (Fatal "'sets' declaration from program does not match the number of input literals")

		(* ///////////////////////////  Interpreter  /////////////////////*)

		method run prog sets_list =
			match prog with
				| Program (using, start, loop, loopInt) ->

					(*Match the input literals with the one declared in the user  program*)
					this#define_sets using sets_list;

					(*Initialize the output list*)
					this#define_output;

					(*Execute the BEGIN part of the code*)
					this#run_statement_list start;

					(*Execute the LOOP part of the code*)
					try
						(*get the loop statement parameter*)
						let k = this#evaluate_intStatement loopInt in
						for i = 1 to k do
							this#run_statement_list loop;
						 done
					with
						| Not_found ->
								()

		method run_statement_list = function
			| statement :: rest ->
				this#run_statement statement;
				this#run_statement_list rest
			|			[] ->()

		method run_statement = function
			| Expression (expression) ->
				this#eval_expr expression; ()

			| Output (expression) ->
				begin
					this#update_output
										(Sets.out (this#eval_expr expression))
				end;
				()
			| If (condition, true_list, false_list) ->
				if this#evaluate_condition condition then
					this#run_statement_list true_list
				else
					this#run_statement_list false_list

	(*Evaluate a given Expression*)
	method eval_expr expression =
		match expression with
			| Literal (literal) ->
				literal
			| Identifier (identifier) ->
				this#read_bind identifier
			| BinaryOperation (operation, left, right) ->
				this#run_binary_operation operation left right
			| SetOperation (operation, left, right) ->
				this#run_set_operation operation left right
			| Assignment (optype, identifier, value) ->
				this#run_assignment optype identifier value
			| SetConstruction (expressions) ->
				this#construct_set expressions

		(*Evaluate an int statement for the loop parameter*)
		method evaluate_intStatement expression =
			match expression with
				| Literal (literal) ->
					Sets.int_of_literal literal
				| Identifier (identifier) ->
					Sets.int_of_literal (this#read_bind identifier)

		method evaluate_condition condition =
			match condition with
				| UnaryCondition test ->
					begin
						match test with
							| Test (optype, left, right) -> this#evaluate_test optype left right
					end

		method evaluate_test test left right =
			let l = this#eval_expr left in
			let r = this#eval_expr right in
				match test with
					| Equality 		-> Comparison.equal l r
					| GreaterThan -> Comparison.greater_than l r
					| LessThan 		-> Comparison.less_than l r

		method run_binary_operation operation left right =
			let l = this#eval_expr left in
			let r = this#eval_expr right in
				match operation with
					| Plus 		-> Math.plus l r
					| Minus 	-> Math.minus l r
					| Divide 	-> Math.divide l r
					| Times 	-> Math.times l r

			method run_set_operation operation left right =
				let l = this#eval_expr left in
				let r = this#eval_expr right in
					match operation with
								| Union		-> Set(Math.union l r)
								| Intersection ->	Set(Math.intersection l r)
								| Difference -> Set(Math.difference l r)
								| Concatenation ->Set(Math.concatenation l r)

		method run_assignment optype identifier expression =
			let evaluated = this#eval_expr expression in
					this#update_binding identifier evaluated;
					evaluated

			method construct_set expr_list =
			Set ( SS.of_list(
				let rec internal = function
					| exp :: rest -> (Sets.string_of_literal (this#eval_expr exp)) :: (internal rest)
					| [] -> []
					in
					internal expr_list)
			)
	end;;
