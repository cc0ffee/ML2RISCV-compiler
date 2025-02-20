val string_of_pos : Lexing.position -> string
val string_of_typ : Iit_ast.typ -> string

val get_bop : Iit_ast.bop -> string * int * bool
val string_of_unop : Iit_ast.unop -> string
val unop_lvl : int

(* Print an integer from the interpreter, formatted correctly for
 * the given type. *)
val pprint_as_type : Format.formatter -> int -> Iit_ast.typ -> unit
val pprint_const : Format.formatter -> Iit_ast.const -> unit
val pprint_exp : Format.formatter -> 'a Iit_ast.exp -> unit
val pprint_string : Format.formatter -> string -> unit
val pprint_stmt_list : Format.formatter -> 'a Iit_ast.stmt list -> unit
val pprint_stmt : Format.formatter -> 'a Iit_ast.stmt -> unit
