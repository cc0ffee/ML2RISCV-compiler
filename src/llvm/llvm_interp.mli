type mem = int array
type loc = string * int
exception RuntimeError of string * loc

val cost : int ref

val initstack : unit -> int array * int
val initheap : unit -> int array * int
val interp :
  Llvm_ast.prog * Llvm_ast.typdefs ->
  Llvm_ast.value Varmap.t ->
  Llvm_ast.value Varmap.t * (int array * int) * (int array * int) * int
