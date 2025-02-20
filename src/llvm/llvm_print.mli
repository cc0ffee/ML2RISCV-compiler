val string_of_typ : Llvm_ast.typ -> string
val string_of_var : Llvm_ast.var -> string
val var_of_string : string -> Llvm_ast.var

val print_var : Format.formatter -> Llvm_ast.var -> unit
val print_value : Format.formatter -> Llvm_ast.var Llvm_ast.value_ -> unit
val print_inst : Format.formatter -> Llvm_ast.var Llvm_ast.inst_ -> unit
val print_func : Format.formatter -> Llvm_ast.var Llvm_ast.func_ -> unit

val print_typdefs :
  Format.formatter -> Llvm_ast.typ list Varmap.t -> unit Varmap.t

val print_prog :
  Format.formatter ->
  Llvm_ast.var Llvm_ast.func_ list * Llvm_ast.typ list Varmap.t -> unit
