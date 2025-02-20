
(* The type of tokens. *)

type token = Iit_tokens.token

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val stmt: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Iit_ast.p_stmt)

val prog: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Iit_ast.p_stmt list)
