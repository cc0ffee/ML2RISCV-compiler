(** CS443 - LLVM AST **)
(** Stefan Muller - 2022 **)

type var = Local of string
         | Global of string

let compare_var (v1: var) (v2: var) =
  match (v1, v2) with
  | (Local s1, Local s2) -> String.compare s1 s2
  | (Global s1, Global s2) -> String.compare s1 s2
  | (Local _, Global _) -> -1
  | (Global _, Local _) -> 1

let string_of_var = function Local s | Global s -> s
                   
type label = string

type typ = TInteger of int (* size in bits *)
         | TPointer of typ
         | TFunction of typ * (typ list)
         | TVoid
         | TArray of typ
         | TStruct of string

type typdefs = typ list Varmap.t

type bop = BAdd
         | BSub
         | BMul
         | BDiv
         | BAnd
         | BOr
         | BXor

type cmp = CEq
         | CNe
         | CSGt
         | CSGe
         | CSLt
         | CSLe

type cast = CBitcast
          | CZext
          | CSext
          | CPtrtoint
          | CInttoptr
          | CTrunc
         
type 'var value_ = Const of int
                | Var of 'var
         
type 'var inst_ =
  ILabel of label
| ISet of 'var * typ * 'var value_
(* dest, type, operands*)
| IBinop of 'var * bop * typ * 'var value_ * 'var value_
(* dest, cmp, type, ops *)
| ICmp of 'var * cmp * typ * 'var value_ * 'var value_
(* dest, cast type, init type, op, dest type *)
| ICast of 'var * cast * typ * 'var value_ * typ
| IBr of label
(* condition as i1, iftrue, iffalse *)
| ICondBr of 'var value_ * label * label
| IRet of (typ * 'var value_) option
(* dest, return type, function ptr, args *)
| ICall of 'var * typ * 'var * (typ * 'var value_) list
(* dest, struct type, struct ptr, indices *)
| IGetElementPtr of 'var * typ * 'var * (typ * 'var value_) list
| IAlloca of 'var * typ * int
(* dest, type of value (not pointer), pointer *)
| ILoad of 'var * typ * 'var
(* type of value, value to store, pointer *)
| IStore of typ * 'var value_ * 'var
| IPhi of 'var * typ * (label * 'var value_) list
        
type 'var func_ = { f_name   : string;
                  f_ret    : typ;
                  f_args   : (typ * string) list;
                  f_body   : 'var inst_ array;
                  f_labels : int Varmap.t
                 }

type value = var value_
type inst = var inst_
type func = var func_

type prog = func list

let make_func (name: string) (ty: typ) (args: (typ * string) list)
      (body: inst list)
  =
  let labels : int Varmap.t ref = ref Varmap.empty in
  let body = Array.of_list body in
  let _ =
    Array.iteri
                 (fun i inst ->
                   match inst with
                   | ILabel l -> labels := Varmap.add l i (!labels)
                   | _ -> ())
                 body
  in
  { f_name = name;
    f_ret = ty;
    f_args = args;
    f_body = body;
    f_labels = !labels
  }

              
let rec sizeof ctx t =
  match t with
  | TInteger _ | TPointer _ -> 1
  | TVoid -> 0
  | TArray _ | TFunction _ -> raise (Invalid_argument "LLVM.Ast.sizeof")
  | TStruct s ->
     List.fold_left (fun s t -> s + (sizeof ctx t)) 0 (Varmap.find s ctx)

let ctr = ref 0
let new_temp () =
  ctr := !ctr + 1;
  Local ("temp" ^ (string_of_int !ctr))

let lctr = ref 0
let new_label () =
  lctr := !lctr + 1;
  "label" ^ (string_of_int !lctr)

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
