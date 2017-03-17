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

		(* Bindings read/write *)

		method read_binding identifier =
			try
				List.assoc identifier bindings
			with
				Not_found ->
					raise (Undeclared_identifier identifier)

		method update_binding identifier value =
			bindings <- (identifier, value) :: List.remove_assoc identifier bindings;
			value

		method update_output set =
			outputList <- outputList@[set];

		(* Stream Bindings *)

		method get_default_stream_identifier =
			if List.length inputs == 1 then
				List.hd inputs
			else
				raise (Fatal "Omission of stream identifier in skip is forbidden when there is more than one stream defined")

		method define_sets identifier_list stream_list =
			try
				match identifier_list with
					| identifier :: rest ->
						bindings <- (identifier, (List.nth stream_list (List.length bindings))) :: bindings;
						inputs <- identifier :: inputs;
						this#define_sets rest stream_list
					| [] -> ()
			with
				| Failure e -> raise (Fatal "'sets' declaration from written program does not match the number of input streams")

		(* Output *)

		method define_output =
			outputList <- []

		method get_output =
			Sets.string_of_set_list outputList  (Sets.int_of_literal (this#read_binding "k"))

		(* Stream continuation operations *)

		method next_all =
			this#next inputs

		method next = function
			| identifier :: rest ->
				begin
					match this#read_binding identifier with
					| Set s ->
						this#update_binding identifier (Sets.skipSet 1 s);
						()
					| _ -> ()
				end;
				this#next rest
			| [] -> ()

		(* Interpreter *)

		method run program sets_list =
			match program with
				| Program (using, start, loop) ->

					this#define_sets using sets_list;
					this#define_output;
					this#run_statement_list start;
					try

 						(* Main loop of the program, execute the loop body & advance the streams *)
						let k = (Sets.int_of_literal (this#read_binding "k")) in
						(*while k>0 do
							this#run_statement_list loop;
							(*this#next_all;*)

						*)
						for i = 1 to k do
							this#run_statement_list loop;
						 done
					with
						| End_of_stream ->
								 ()
						| Not_found ->
								()

		method run_statement_list = function
			| statement :: rest ->
				this#run_statement statement;
				this#run_statement_list rest
			|			[] ->()

		method run_statement = function
			| Expression (expression) ->
				this#evaluate_expression expression; ()

			| Output (expression) ->
				begin
					this#update_output
										(Sets.out (this#evaluate_expression expression))
				end;
				()
			| If (condition, true_list, false_list) ->
				if this#evaluate_condition condition then
					this#run_statement_list true_list
				else
					this#run_statement_list false_list

		method evaluate_expression expression =
			match expression with
				| Literal (literal) ->
					literal
				| Identifier (identifier) ->
					this#read_binding identifier
				| BinaryOperation (operation, left, right) ->
					this#run_binary_operation operation left right
				| SetOperation (operation, left, right) ->
					this#run_set_operation operation left right
				| Assignment (optype, identifier, value) ->
					this#run_assignment optype identifier value
				| SetConstruction (expressions) ->
					this#construct_set expressions

		method evaluate_condition condition =
			match condition with
				| UnaryCondition test ->
					begin
						match test with
							| Test (optype, left, right) -> this#evaluate_test optype left right
					end

		method evaluate_test test left right =
			let x = this#evaluate_expression left in
			let y = this#evaluate_expression right in
				match test with
					| Equality 				-> Comparison.equal x y
					| LessThan 				-> Comparison.less_than x y
					| GreaterThan 			-> Comparison.greater_than x y

		method run_binary_operation operation left right =
			let x = this#evaluate_expression left in
			let y = this#evaluate_expression right in
				match operation with
					| Plus 		-> Math.plus x y
					| Minus 	-> Math.minus x y
					| Times 	-> Math.times x y
					| Divide 	-> Math.divide x y

			method run_set_operation operation left right =
				let x = this#evaluate_expression left in
				let y = this#evaluate_expression right in
					match operation with
								| Union		-> Set(Math.union x y)
								| Intersection ->	Set(Math.intersection x y)
								| Difference -> Set(Math.difference x y)
								| Concatenation ->Set(Math.concatenation x y)

		method run_assignment optype identifier expression =
			let evaluated = this#evaluate_expression expression in
			match optype with
				| StandardAssign ->
					this#update_binding identifier evaluated;
					evaluated

				(* StandardAssign & operation assigns need to be seperated,
				   we want to check if the variable is assigned for operation assigns,
				   but we need to allow new variables for standard assigns *)

				| _ -> evaluated

			method construct_set expression_list =
			Set ( SS.of_list(
				let rec internal = function
					| expression :: rest -> (Sets.string_of_literal (this#evaluate_expression expression)) :: (internal rest)
					| [] -> []
					in
					internal expression_list)
			)
	end;;
