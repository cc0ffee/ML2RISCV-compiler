val print_tab : int -> unit
val flush : 'a -> unit
val commit : 'a -> unit
val new_line : 'a -> unit
val force_new_line : 'a -> unit
val indent : 'a -> unit
val unindent : 'a -> unit
val space : 'a -> unit
val print_commas : bool -> ('a -> unit) -> 'a list -> unit
val escape_string : string -> string
val string_of_typ : C_ast.typ -> string
val print_base_type : C_ast.typ -> unit
val print_fields : string -> (string * C_ast.typ) list -> unit
val print_enum : string -> (string * 'a C_ast.exp) list -> unit
val get_base_type : C_ast.typ -> C_ast.typ
val print_pointer : C_ast.typ -> unit
val print_array : C_ast.typ -> unit
val print_type : (unit -> unit) -> C_ast.typ -> unit
val print_onlytype : C_ast.typ -> unit
val print_single_name : C_ast.typ * string -> unit
val print_params : (C_ast.typ * string) list -> unit
val get_operator : 'a C_ast.exp -> string * int
val print_comma_exps : 'a C_ast.exp list -> unit
val print_expression : 'a C_ast.exp -> int -> unit
val print_constant : C_ast.const -> unit
val print_statement : 'a C_ast.stmt -> unit
val print_gnu_asm_arg : string * string * 'a C_ast.exp -> unit
val print_substatement : 'a C_ast.stmt -> unit
val print_defs : 'a C_ast.def list -> unit
val print_def : 'a C_ast.def -> unit
val print : out_channel -> 'a C_ast.file -> unit
val set_tab : int -> unit
val set_width : int -> unit
