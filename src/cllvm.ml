(* IIT CS 443 - Fall 2022 *)
(* C to LLVM Compiler *)
(* Project 3 *)

open C.Ast
module L = LLVM.Ast
open C.Typecheck

exception CompileError of string * loc

let new_temp = L.new_temp
let new_label = L.new_label

let compile_var (s, scope) =
  match scope with
  | Local -> L.Var (L.Local s)
  | Global -> L.Var (L.Global s)

let btype = L.TInteger 1
let ctype = L.TInteger 8
let itype = L.TInteger 32

(* Will probably be useful *)
let word_size = Config.word_size
let malloc = L.Global "malloc"

let rec compile_typ ctx t =
  match t with
  | TVoid -> L.TVoid
  | TBool -> btype
  | TChar -> ctype
  | TInt -> itype
  | TArray t -> L.TPointer (compile_typ ctx t)
  | TStruct s -> L.TPointer (L.TStruct s)
  | TFunction (rt, args) ->
     L.TPointer (L.TFunction
                   (compile_typ ctx rt,
                    List.map (fun (t, _) -> compile_typ ctx t) args))
  
let move (dest: L.var) (typ: L.typ) (value: L.value) =
  L.ISet (dest, typ, value)

let compile_cast (dest: L.var) (in_typ: L.typ) (exp: L.value) (out_typ : L.typ)
  =
  let ct =
    match (in_typ, out_typ) with
    | (L.TInteger n1, L.TInteger n2) ->
       if n1 > n2 then L.CTrunc
       else if n1 = n2 then L.CBitcast
       else L.CZext
    | (L.TPointer _, L.TPointer _) -> L.CBitcast
    | (L.TInteger _, L.TPointer _) -> L.CInttoptr
    | (L.TPointer _, L.TInteger _) -> L.CPtrtoint
    | _ -> failwith "invalid cast"
  in
  [L.ICast (dest, ct, in_typ, exp, out_typ)]

exception Unimplemented

let rec compile_exp (ctx: ctx) (dest: L.var) (e: t_exp) : L.inst list =
  match e.edesc with
  | EConst (CInt n) -> [L.ISet (dest, itype, L.Const n)]
  | EConst (CChar c) -> [L.ISet (dest, ctype, L.Const (Char.code c))]
  | EVar (v, s) -> [L.ISet (dest, compile_typ ctx e.einfo, compile_var(v, s))]
  | EBinop (op, e1, e2) -> compile_binop ctx dest op e1 e2
  | EAssign ({ldesc = LHVar var; _}, e1) -> (compile_exp ctx (L.Local var) e1) @ [L.ISet (dest, (compile_typ ctx e1.einfo), L.Var (L.Local var))]
  | EAssign ({ldesc = LHArr (var, e1); _}, e2) -> let dest1 = new_temp() in
                                                  let dest2 = new_temp() in 
                                                  let ptr = new_temp() in
                                                  compile_exp ctx dest1 e1 @
                                                  compile_exp ctx dest2 e2 @
                                                  [L.IGetElementPtr (ptr, compile_typ ctx e2.einfo, L.Local var, [compile_typ ctx e1.einfo, L.Var dest1])] @
                                                  [L.IStore (compile_typ ctx e2.einfo, L.Var dest2, ptr)] @
                                                  [L.ILoad (dest, compile_typ ctx e2.einfo, ptr)] 
  | EAssign ({ldesc = LHField (v, TStruct s, str); _}, e1) -> let dest1 = new_temp() in
                                                              let ptr = new_temp() in
                                                              let (field_index, field_type) = match get_field_i_and_typ ctx s str with  (* get struct field's index and type *)
                                                              | Some (idx, ftyp) -> (idx, ftyp)
                                                              | None -> raise (CompileError ("Field not found in struct", e1.eloc)) in
                                                              compile_exp ctx dest1 e1 @
                                                              [L.IGetElementPtr (ptr, L.TStruct s, L.Local v, [(itype, L.Const 0); (itype, L.Const field_index)])] @
                                                              [L.IStore (compile_typ ctx e1.einfo, L.Var dest1, ptr)] @
                                                              [L.ILoad (dest, compile_typ ctx e1.einfo, ptr)]
  | EAssign (_, _) -> raise (CompileError ("Left side of assignment not a variable", e.eloc))
  | EUnop (op, e) -> compile_unop ctx dest op e
  | ENewStruct s -> let tds = Varmap.map (List.map (compile_typ ctx)) (get_typedefs ctx) in (* account typedef mem size for allocating struct *)
                    let ptr = new_temp() in
                    [L.ICall (ptr, L.TPointer ctype, malloc, [(itype, L.Const (word_size * (L.sizeof tds (TStruct s))))])] @ (* ctype is size 8 so... ig we use it *)
                    [L.ICast (dest, L.CBitcast, L.TPointer ctype, L.Var ptr, compile_typ ctx e.einfo)]

  | ENewArray (t, size) -> let tds = Varmap.map (List.map (compile_typ ctx)) (get_typedefs ctx) in  (* account typedef mem size for allocating array *)
                           let ptr = new_temp() in
                           [L.ICall (ptr, L.TPointer ctype, malloc, [(itype, L.Const ( (L.sizeof tds (compile_typ ctx t)) * word_size * size ))])] @
                           [L.ICast (dest, L.CBitcast, L.TPointer ctype, L.Var ptr, compile_typ ctx e.einfo)]
  | ECall (e1, lst) -> let ptr = new_temp() in
                      let typ_lst = List.map (fun arg -> compile_typ ctx arg.einfo) lst in                            (* get types for all arguments *)
                      let val_lst = List.map (fun arg -> new_temp()) lst in                                           (* get values for all arguments *)
                      let typ_val = List.map2 (fun arg_typ arg_val -> (arg_typ, L.Var arg_val)) typ_lst val_lst in    (* map to get (typ, value) needed for call func *)
                      compile_exp ctx ptr e1 @  
                      List.concat (List.map2 (fun dest e2 -> compile_exp ctx dest e2) val_lst lst) @      (* compiles argument ie n - 2, concat does it for all args *)
                      [L.ICall (dest, compile_typ ctx e.einfo, ptr, typ_val)]
  | EArrIndex (e1, e2) -> let dest1 = new_temp() in
                          let dest2 = new_temp() in 
                          let ptr = new_temp() in
                          compile_exp ctx dest1 e1 @                            (* Compiles to array pointer *)
                          compile_exp ctx dest2 e2 @                            (* Compiles to index *)
                          [L.IGetElementPtr (ptr, compile_typ ctx e1.einfo, dest1, [compile_typ ctx e2.einfo, L.Var dest2])] @   
                          [L.ILoad (dest, compile_typ ctx e1.einfo, ptr)] 
  | EField (e1, fid) -> let dest1 = new_temp() in                              (* Compiles to struct pointer *)
                       let ptr = new_temp() in                                 (* Becomes pointer to individual element being accessed *)
                       let struct_name = match e1.einfo with                         
                          | TStruct s -> s
                          | _ -> raise (CompileError ("Expected a struct type", e1.eloc)) in
                       let deref_ptr = match compile_typ ctx e1.einfo with      (* Dereference ptr as in LLVM if you don't it passes ex i32* *)
                       | L.TPointer typ -> typ                                  (* in LLVM AST L.TPointer holds typ which is what we need so returns typ *)
                       | _ -> raise (CompileError ("Can't deref pointer for struct", e.eloc)) in
                       let (field_index, field_type) = match get_field_i_and_typ ctx struct_name fid with     (* get struct field's index and type *)
                                                      | Some (idx, ftyp) -> (idx, ftyp)
                                                      | None -> raise (CompileError ("Field not found in struct", e.eloc)) in
                       compile_exp ctx dest1 e1 @
                       [L.IGetElementPtr (ptr, deref_ptr, dest1, [(itype, L.Const 0); (itype, L.Const field_index)])] @
                       [L.ILoad (dest, compile_typ ctx field_type, ptr)]
  | ECast (e1, t) -> let dest1 = new_temp() in
                    (* pass arguments to the predefined compile_cast*)
                    compile_exp ctx dest1 e1 @
                    compile_cast (dest) (compile_typ ctx e1.einfo) (L.Var dest1) (compile_typ ctx t)

and compile_binop (ctx: ctx) (dest: L.var) (b: bop) (e1: t_exp) (e2: t_exp) : L.inst list =
  match b with
  | BAnd -> 
    let label1 = new_label() in     
    let label2 = new_label() in     (* A single false goes to 0 for AND *)
    let ldone = new_label() in
    (compile_branch_exp ctx e1 label1 label2) @
    [L.ILabel label2] @ [L.ISet (dest, btype, L.Const 0)] @ [L.IBr ldone] @
    [L.ILabel label1] @ (compile_exp ctx dest e2) @ [L.IBr ldone] @ [L.ILabel ldone]
  | BOr -> 
    let label1 = new_label() in     (* A single true goes to 1 for OR *)
    let label2 = new_label() in
    let ldone = new_label() in
    (compile_branch_exp ctx e1 label1 label2) @
    [L.ILabel label1] @ [L.ISet (dest, btype, L.Const 1)] @ [L.IBr ldone] @
    [L.ILabel label2] @ (compile_exp ctx dest e2) @ [L.IBr ldone] @ [L.ILabel ldone]
  | _ ->
  let dest1 = new_temp() in 
  let dest2 = new_temp() in
  (compile_exp ctx dest1 e1) @
  (compile_exp ctx dest2 e2) @
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

and compile_unop (ctx: ctx) (dest: L.var) (u: unop) (e: t_exp) : L.inst list =
  let dest1 = new_temp () in
  (compile_exp ctx dest1 e) @
  match u with
    | UNeg -> [L.IBinop (dest, L.BSub, itype, L.Const 0, L.Var dest1)]
    | UNot -> [L.IBinop (dest, L.BXor, btype, L.Const 1, L.Var dest1)]

and compile_branch_exp (ctx: ctx) (e: t_exp) (tlabel: L.label) (flabel: L.label) : L.inst list = 
  let dest1 = new_temp () in
  (compile_exp ctx dest1 e) @ [L.ICondBr (L.Var dest1, tlabel, flabel)]
  
let rec compile_stmt
      (ctx: ctx)
      (tds: L.typdefs)
      (break_lbl: L.label option)
      (cont_lbl: L.label option)
      (s: t_stmt): L.inst list =
  match s.sdesc with
  | SDecl (v, _, Some e) ->
     (* Compile e, store to local variable v *)
     compile_exp ctx (L.Local v) e
  | SDecl _ -> []
  | SBlock ss ->
     List.concat (List.map (compile_stmt ctx tds break_lbl cont_lbl) ss)
  | SBreak -> (match break_lbl with
     | None -> raise (CompileError ("break not found", s.sloc))
     | Some break -> [L.IBr break])
  | SContinue -> (match cont_lbl with
     | None -> raise (CompileError ("continue not found", s.sloc))
     | Some cont -> [L.IBr cont])
  | SExp exp -> compile_exp ctx (new_temp ()) exp
  | SIf (exp, s1, s2o) -> 
    let label1 = new_label() in     (* True *)
    let label2 = new_label() in     (* False *)
    let label3 = new_label() in     (* Exit *)
    compile_branch_exp ctx exp label1 label2 @
    [L.ILabel (label1)] @
    compile_stmt ctx tds break_lbl cont_lbl s1 @
    [L.IBr (label3)] @
    [L.ILabel (label2)] @
    compile_stmt ctx tds break_lbl cont_lbl s2o @
    [L.IBr (label3)] @
    [L.ILabel (label3)]
  | SFor (e1, e2, e3, s) ->
    let label1 = new_label() in     (* Label for loop expression, evaluated till `e2` is false *)
    let label2 = new_label() in     (* Label for loop body, evaluates body `s` each run *)
    let label3 = new_label() in     (* Exit label when `e2` evaluates to false *)
    let dest = new_temp() in
    compile_exp ctx dest e1 @ (* Evaluate expression `e1` just once at start*)
    [L.IBr label1] @
    [L.ILabel label1] @
    compile_branch_exp ctx e2 label2 label3 @ (* Jump to label2 when expression true, evaluate body; exit loop otherwise *)
    [L.ILabel label2] @
    compile_stmt ctx tds (Some label3) (Some label1) s @
    compile_exp ctx dest e3 @
    [L.IBr label1] @ (* Re-evaluate e2 after body `s` is run and `e3` is evaluated *)
    [L.ILabel label3]
  | SReturn ret -> (match ret with
                | None -> let label1 = new_label() in [L.IRet None] @ [L.ILabel label1]
                | Some e -> let dest1 = new_temp() in
                        let label1 = new_label() in
                        compile_exp ctx dest1 e @ [L.IRet (Some (compile_typ ctx e.einfo, L.Var dest1))] @ [L.ILabel label1])

let compile_func ctx tds (name, t, body) : L.func =
  match t with
  | TFunction (tret, args) ->
     let tret = compile_typ ctx tret in
     let targs = List.map (fun (t, s) -> (compile_typ ctx t, s)) args in
     L.make_func name tret targs
       ((L.ILabel (Config.entry_label_of_fun name))::(compile_stmt ctx tds None None body)
        @ [match tret with
           | L.TVoid -> L.IRet None
           | _ -> L.IRet (Some (tret, Const 0))])
  | _ -> raise (CompileError ("not a function type", ("", 0)))

let rec compile_def ctx tds d : L.func list =
  match d.ddesc with
  | DFun (s, t, b) -> [compile_func ctx tds (s, t, b)]
  | _ -> []

let compile_prog (ctx, ds) : L.prog * L.typdefs =
  let tds = Varmap.map (List.map (compile_typ ctx)) (get_typedefs ctx) in
  (List.concat (List.map (compile_def ctx tds) ds), tds)