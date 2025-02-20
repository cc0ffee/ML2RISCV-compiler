type loc = Lexing.position * Lexing.position
type var = string

type const = CNum of int
           | CBool of bool
           | CTriv (* () *)
           | CNil (* [] *)

type typ = TInt
         | TBool
         | TUnit
         | TList of typ
         | TArrow of typ * typ
         | TProd of typ * typ

type bop = BAdd
         | BSub
         | BMul
         | BDiv
         | BAnd
         | BOr
         | BGt (* > *)
         | BGe (* >= *)
         | BLt (* < *)
         | BLe (* <= *)
         | BNe (* != *)
         | BEq (* = *)

type unop = UNot | UNeg

(* Expressions *)
type 'a exp_ =
  | EVar of var
  | EConst of const
  | EBinop of bop * 'a exp * 'a exp
  | EUnop of unop * 'a exp
  | EFun of var * typ * 'a exp
  | EIf of 'a exp * 'a exp * 'a exp (* if e1 then e2 else e3 *)
  | ELet of var * typ option * 'a exp * 'a exp (* let x (: t) = e1 in e2 *)
  (* let (rec?) f (x :t1) (:t2) = e1 in e2 *)
  | ELetFun of bool * var * var * typ * typ option * 'a exp * 'a exp
  | ELetPair of var * var * 'a exp * 'a exp (* let (x, y) = e1 in e2 *)
  | EApp of 'a exp * 'a exp
  (* match e1 with | [] -> e2 | h::t -> e3 *)
  | EMatchList of 'a exp * 'a exp * var * var * 'a exp
  | EPair of 'a exp * 'a exp
  | ECons of 'a exp * 'a exp
  | EAnnot of 'a exp * typ (* Annotations (e : t) *)

and 'a exp = { edesc : 'a exp_;
               eloc : loc;
               einfo : 'a }

let mk_t_exp e loc t = { edesc = e;
                         eloc = loc;
                         einfo = t }

let mk_exp e loc = { edesc = e;
                     eloc = loc;
                     einfo = () }

type p_exp = unit exp
type t_exp = typ exp

type t_prog = t_exp
type p_prog = p_exp

let ctr = ref (-1)
let new_var () =
  ctr := !ctr + 1;
  "__ctemp" ^ (string_of_int (!ctr))
let new_mangle s =
  ctr := !ctr + 1;
  s ^ (string_of_int (!ctr))

let syn_err e s (spos, epos) =
      let string_of_pos p =
	Lexing.(Printf.sprintf "%s:%d:%d"
	          p.pos_fname
	          p.pos_lnum
	          (p.pos_cnum - p.pos_bol))
      in
      Printf.printf "%s--%s: Syntax Error: %s\n"
		    (string_of_pos spos)
		    (string_of_pos epos)
		    s;
      raise e
