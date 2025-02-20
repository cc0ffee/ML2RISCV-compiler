(* MiniIITRAN to LLVM Compiler *)
(* IIT CS 443, Fall 2022 *)
(* Project 2 *)

open IITRAN.Ast
module L = LLVM.Ast

exception CompileError of string * loc

let result_var = L.Local "result"
let itype = L.TInteger 64
let btype = L.TInteger 1

let compile_typ =
  function TInteger | TCharacter -> itype
           | TLogical -> btype

let ctr = ref 0
let new_temp () =
  ctr := !ctr + 1;
  L.Local ("temp" ^ (string_of_int !ctr))

let lctr = ref 0
let new_label () =
  lctr := !lctr + 1;
  "label" ^ (string_of_int !lctr)

let compile_var s = L.Var (L.Local s)

let move (dest: L.var) (typ: L.typ) (value: L.value) =
  L.ISet (dest, typ, value)

exception Unimplemented

(* Mutually recursive *)
let rec compile_exp (dest: L.var) (e: t_exp) : L.inst list =
  match e.edesc with
  | EConst (CInt n) -> [L.ISet (dest, itype, L.Const n)]
  | EConst (CChar c) -> [L.ISet (dest, itype, L.Const (Char.code c))]
  | EVar var -> [L.ISet (dest, compile_typ e.einfo, compile_var var)]
  | EBinop (op, e1, e2) -> compile_binop dest op e1 e2
  | EAssign ({edesc = EVar var; _}, e) -> (compile_exp (L.Local var) e) @ [L.ISet (dest, (compile_typ e.einfo), compile_var var)] 
  | EAssign (_, _) -> raise (CompileError ("Left side of assignment not a variable", e.eloc))
  | EUnop (op, e) -> compile_unop dest op e

and compile_binop (dest: L.var) (b: bop) (e1: t_exp) (e2: t_exp) : L.inst list =
  (* Match for short-circuit first *)
  match b with
  | BAnd -> 
    let label1 = new_label() in     
    let label2 = new_label() in     (* A single false goes to 0 for AND *)
    let ldone = new_label() in
    (compile_branch_exp e1 label1 label2) @
    [L.ILabel label2] @ [L.ISet (dest, btype, L.Const 0)] @ [L.IBr ldone] @
    [L.ILabel label1] @ (compile_exp dest e2) @ [L.IBr ldone] @ [L.ILabel ldone]
  | BOr -> 
    let label1 = new_label() in     (* A single true goes to 1 for OR *)
    let label2 = new_label() in
    let ldone = new_label() in
    (compile_branch_exp e1 label1 label2) @
    [L.ILabel label1] @ [L.ISet (dest, btype, L.Const 1)] @ [L.IBr ldone] @
    [L.ILabel label2] @ (compile_exp dest e2) @ [L.IBr ldone] @ [L.ILabel ldone]
  | _ ->
  let dest1 = new_temp() in 
  let dest2 = new_temp() in
  (compile_exp dest1 e1) @
  (compile_exp dest2 e2) @
  match b with
    | BAdd -> [L.IBinop (dest, L.BAdd, itype, L.Var dest1, L.Var dest2)]
    | BSub -> [L.IBinop (dest, L.BSub, itype, L.Var dest1, L.Var dest2)]
    | BMul -> [L.IBinop (dest, L.BMul, itype, L.Var dest1, L.Var dest2)]
    | BDiv -> [L.IBinop (dest, L.BDiv, itype, L.Var dest1, L.Var dest2)]
    | BAnd -> [L.IBinop (dest, L.BAnd, btype, L.Var dest1, L.Var dest2)]
    | BOr -> [L.IBinop (dest, L.BOr, btype, L.Var dest1, L.Var dest2)]
    | BEq -> [L.ICmp (dest, L.CEq, itype, L.Var dest1, L.Var dest2)]
    | BNe -> [L.ICmp (dest, L.CNe, itype, L.Var dest1, L.Var dest2)]
    | BGt -> [L.ICmp (dest, L.CSGt, itype, L.Var dest1, L.Var dest2)]
    | BGe -> [L.ICmp (dest, L.CSGe, itype, L.Var dest1, L.Var dest2)]
    | BLt -> [L.ICmp (dest, L.CSLt, itype, L.Var dest1, L.Var dest2)]
    | BLe -> [L.ICmp (dest, L.CSLe, itype, L.Var dest1, L.Var dest2)]

and compile_unop (dest: L.var) (u: unop) (e: t_exp) : L.inst list =
  let dest1 = new_temp () in
  (compile_exp dest1 e) @
  match u with
    | UNeg -> [L.IBinop (dest, L.BSub, itype, L.Const 0, L.Var dest1)]
    | UNot -> [L.IBinop (dest, L.BXor, btype, L.Const 1, L.Var dest1)]
    | UChar -> [L.ISet (dest, itype, L.Var dest1)]
    | ULog -> [L.ICmp (dest, L.CSGt, itype, L.Var dest1, L.Const 0)]
    | UInt -> match e.einfo with
              | TCharacter | TInteger -> [L.ISet (dest, itype, L.Var dest1)]
              | TLogical -> [L.ICast (dest, L.CSext, btype, L.Var dest1, itype)]

and compile_branch_exp (e: t_exp) (tlabel: L.label) (flabel: L.label) : L.inst list = 
  let dest1 = new_temp () in
  (compile_exp dest1 e) @ [L.ICondBr (L.Var dest1, tlabel, flabel)]

let rec compile_stmt (s: t_stmt) : L.inst list =
  match s.sdesc with
  | SDecl _ -> []
  | SDo ss -> List.concat (List.map compile_stmt ss)
  | SStop -> [L.IRet (Some (itype, L.Var result_var)); L.ILabel (new_label ())]
  | SExp exp -> compile_exp (new_temp ()) exp
  | SIf (exp, s1, s2o) ->
      let label1 = new_label() in     (* True *)
      let label2 = new_label() in     (* False *)
      let label3 = new_label() in     (* Exit *)
      compile_branch_exp exp label1 label2 @
      [L.ILabel (label1)] @
      compile_stmt s1 @
      [L.IBr (label3)] @
      [L.ILabel (label2)] @
      (match s2o with
      | Some s2 -> compile_stmt s2
      | None -> []) @
      [L.IBr (label3)] @
      [L.ILabel (label3)]
    | SWhile (e, s) ->
      let testl = new_label() in 
      let bodyl = new_label() in
      let donel = new_label() in
      [L.IBr (testl)] @
      [L.ILabel (testl)] @
      compile_branch_exp e bodyl donel @
      [L.ILabel (bodyl)] @
      compile_stmt s @
      [L.IBr (testl)] @
      [L.ILabel (donel)]
    
let compile_prog (p: t_stmt list) : L.prog =
  [L.make_func "main" itype []
     ((L.ILabel (Config.entry_label_of_fun "main"))::
        (List.concat (List.map compile_stmt p))
      @ [L.IRet (Some (itype, L.Var result_var))]
  )]
