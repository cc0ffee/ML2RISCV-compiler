type loc = Lexing.position * Lexing.position
type var = string

type typ = TLogical
         | TCharacter
         | TInteger

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
         | BNe (* # *)
         | BEq (* = *)

type unop = UNeg
          | UNot
          (* type conversions *)
          | UChar
          | ULog
          | UInt

type const = CChar of char
           | CInt of int
         
type 'a exp_ = EConst of const
             | EVar of var
             | EBinop of bop * 'a exp * 'a exp
             | EAssign of 'a exp * 'a exp
             | EUnop of unop * 'a exp
and 'a exp = { edesc : 'a exp_;
               eloc : loc;
               einfo : 'a }

let mk_exp e loc = { edesc = e;
                     eloc = loc;
                     einfo = () }

let mk_t_exp e loc t = { edesc = e;
                         eloc = loc;
                         einfo = t }

type 'a stmt_ = SDecl of typ * var list
              | SDo of 'a stmt list
              | SExp of 'a exp
              | SIf of 'a exp * 'a stmt * (* ELSE *) 'a stmt option
              | SWhile of 'a exp * 'a stmt
              | SStop
and 'a stmt = { sdesc : 'a stmt_;
                sloc : loc }

type p_stmt = unit stmt
type p_exp = unit exp

type t_stmt = typ stmt
type t_exp = typ exp
            
let mk_stmt loc s = {sdesc = s; sloc = loc}

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
