open Iit_ast
open Format

let string_of_pos p =
	Printf.sprintf "%s:%d:%d"
	  p.Lexing.pos_fname
	  p.Lexing.pos_lnum
	  (p.Lexing.pos_cnum - p.Lexing.pos_bol)

let string_of_typ = function
  | TLogical -> "LOGICAL"
  | TCharacter -> "CHARACTER"
  | TInteger -> "INTEGER"

let get_bop = function
  | BAdd -> ("+", 5, false)
  | BSub -> ("-", 5, false)
  | BMul -> ("*", 6, false)
  | BDiv -> ("/", 6, false)
  | BAnd -> ("AND", 3, false)
  | BOr -> ("OR", 2, false)
  | BGt -> (">", 4, false)
  | BGe -> (">=", 4, false)
  | BLt -> ("<", 4, false)
  | BLe -> ("<=", 4, false)
  | BNe -> ("#", 4, false)
  | BEq -> ("=", 4, false)

let string_of_unop = function
  | UNeg -> "-"
  | UNot -> "NOT"
  | UChar -> "CHAR"
  | ULog -> "LG"
  | UInt -> "INT"

let pprint_as_type f v t =
  match t with
  | TLogical -> fprintf f "%d" (if v > 0 then 1 else 0)
  | TInteger -> fprintf f "%d" v
  | TCharacter -> fprintf f "%c" (Char.chr v)
          
let pprint_const f c =
  match c with
  | CChar c -> fprintf f "%c" c
  | CInt i -> fprintf f "%d" i

let unop_lvl = 8

let rec pprint_exp lvl f e =
  match e.edesc with
  | EConst c -> pprint_const f c
  | EVar s -> fprintf f "%s" (String.uppercase_ascii s)
  | EBinop (b, e1, e2) ->
     let (s, lvl', rassoc) = get_bop b in
     fprintf f "@[%s%a %s %a%s@]"
       (if lvl > lvl' then "(" else "")
       (pprint_exp (if rassoc then lvl' + 1 else lvl')) e1
       s
       (pprint_exp (if rassoc then lvl' else lvl' + 1)) e2
       (if lvl > lvl' then ")" else "")
  | EAssign (e1, e2) ->
     fprintf f "@[%s%a -> %a%s@]"
       (if lvl > 1 then "(" else "")
       (pprint_exp 2) e1
       (pprint_exp 1) e2
       (if lvl > 1 then ")" else "")
  | EUnop (u, e) ->
     fprintf f "@[%s%s %a%s@]"
       (if lvl > unop_lvl then "(" else "")
       (string_of_unop u)
       (pprint_exp unop_lvl) e
       (if lvl > unop_lvl then "(" else "")

let pprint_exp f e = pprint_exp 0 f e
    
let pprint_string f s = fprintf f "%s" s

let rec seq p sep f l =
  match l with
  | [] -> ()
  | [x] -> fprintf f "@[%a@]" p x
  | x::r -> fprintf f "%a@[%s%a@]"
              p x
              sep
              (seq p sep) r

let rec pprint_stmt_list f l =
  match l with
  | [] -> ()
  | [s] -> pprint_stmt f s
  | s::ss -> fprintf f "@[<v>%a@;%a@]"
               pprint_stmt s
               pprint_stmt_list ss
and pprint_stmt f s =
  match s.sdesc with
  | SDecl (t, ss) -> fprintf f "@[%s %a@]"
                       (string_of_typ t)
                       (seq pprint_string ", ") ss
  | SDo [s] -> fprintf f "@[<hv 2>DO@ %a@ END@]"
                 pprint_stmt s
  | SDo ss -> fprintf f "@[<v>@[<v 2>DO@ %a@]@ END@]"
                pprint_stmt_list ss
  | SExp e -> pprint_exp f e
  | SIf (e, s1, Some s2) ->
     fprintf f "@[<hv>IF@;<1 2>%a@ %a@ ELSE@ %a@]"
       pprint_exp e
       pprint_stmt s1
       pprint_stmt s2
  | SIf (e, s1, None) ->
     fprintf f "@[<hv>IF@;<1 2>%a@ %a@]"
       pprint_exp e
       pprint_stmt s1
  | SWhile (e, s) ->
     fprintf f "@[<hv>WHILE@;<1 2>%a@ %a@]"
       pprint_exp e
       pprint_stmt s
  | SStop -> fprintf f "STOP"
