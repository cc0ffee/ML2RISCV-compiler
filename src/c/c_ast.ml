(** CS443 - C AST **)
(** Stefan Muller - 2022 **)


type loc = string * int
type var = string

         
type typ = TVoid
         | TBool
         | TChar
         | TInt
         | TArray of typ
         | TStruct of string (* * (string * typ) list *)
         | TFunction of typ * (typ * string) list (* return, args *)

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
         | BEq (* == *)

type unop = UNeg
          | UNot

type const = CChar of char
           | CInt of int

type var_scope = Local | Global
                   
type 'a exp_ = EConst of const
             | EVar of var * var_scope
             | EBinop of bop * 'a exp * 'a exp
             | EAssign of 'a lhs * 'a exp
             | ENewStruct of string
             | ENewArray of typ * int
             | EUnop of unop * 'a exp
             | ECall of 'a exp * 'a exp list
             | EArrIndex of 'a exp * 'a exp
             | EField of 'a exp * string (* e.s *)
             | ECast of 'a exp * typ

and 'a lhs_ = LHVar of var
            | LHArr of var * 'a exp
            (* x.s *)
            (* 'a component is the type of the structure, e.g. TStruct s *)
            | LHField of var * 'a * string

and 'a exp = { edesc : 'a exp_;
               eloc : loc;
               einfo : 'a }

and 'a lhs = { ldesc : 'a lhs_;
               lloc : loc;
               linfo : 'a }
           
let mk_exp e loc = { edesc = e;
                     eloc = loc;
                     einfo = () }

let mk_t_exp e loc t = { edesc = e;
                         eloc = loc;
                         einfo = t }

let mk_t_lhs e loc t = { ldesc = e;
                         lloc = loc;
                         linfo = t }

let lhs_to_exp loc info l =
  let mk e = {edesc = e; eloc = loc; einfo = info}
  in
  mk
    (match l.ldesc with
     | LHVar v -> EVar (v, Local)
     | LHArr (v, e) -> EArrIndex (mk (EVar (v, Local)), e)
     | LHField (v, _, s) -> EField (mk (EVar (v, Local)), s)
    )

let exp_to_lhs e =
  match e.edesc with
  | EVar (v, Local) -> Some (mk_t_lhs (LHVar v) e.eloc e.einfo)
  | EArrIndex ({edesc = EVar (v, Local); _}, e) ->
     Some (mk_t_lhs (LHArr (v, e)) e.eloc e.einfo)
  | EField (({edesc = EVar (v, Local); _} as s), f) ->
     Some (mk_t_lhs (LHField (v, s.einfo, f)) e.eloc e.einfo)
  | _ -> None

           
type 'a stmt_ = SDecl of var * typ * 'a exp option
              | SBlock of 'a stmt list
              | SExp of 'a exp
              | SIf of 'a exp * 'a stmt * 'a stmt
              (* for (e1; e2; e3) s2 *)
              | SFor of 'a exp * 'a exp * 'a exp * 'a stmt
              | SBreak
              | SContinue
              | SReturn of 'a exp option

and 'a stmt = { sdesc : 'a stmt_;
                sloc : loc }

and 'a def_ = DFun of string * typ * 'a stmt
            | DName of (string * typ * 'a exp option) list
            | DTypeDef of (string * typ) list
            | DStructDef of string * (string * typ) list

and 'a def = { ddesc : 'a def_;
               dloc : loc }

let mk_stmt s loc = {sdesc = s;
                     sloc = loc}

let mk_def d loc = {ddesc = d; dloc = loc}

type 'a file = 'a def list

type p_stmt = unit stmt
type p_exp = unit exp
type p_lhs = unit lhs
type p_def = unit def

type t_stmt = typ stmt
type t_lhs = typ lhs
type t_exp = typ exp
type t_def = typ def

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
