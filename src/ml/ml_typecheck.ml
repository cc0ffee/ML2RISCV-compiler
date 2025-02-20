open Ml_ast

module P = Ml_print
   
exception TypeError of string * loc

let t_error expected got l =
  raise (TypeError (Printf.sprintf "Expected expr of type %s; got %s"
                           (P.string_of_typ expected)
                           (P.string_of_typ got),
                    l))

let t_error_s expected got l =
  raise (TypeError (Printf.sprintf "Expected expr of type %s; got %s"
                           (P.string_of_typ expected)
                           got,
                    l))
  
type ctx = typ Varmap.t

let add_var (c: ctx) (v: var) (t: typ) =
  Varmap.add v t c

let lookup_var (c: ctx) (v: var) =
  Varmap.find v c

let rec teq t1 t2 =
  match (t1, t2) with
  | (TInt, TInt) -> true
  | (TBool, TBool) -> true
  | (TUnit, TUnit) -> true
  | (TList t1, TList t2) -> teq t1 t2
  | (TArrow (t1a, t1b), TArrow (t2a, t2b)) -> teq t1a t2a && teq t1b t2b
  | (TProd (t1a, t1b), TProd (t2a, t2b)) -> teq t1a t2a && teq t1b t2b
  | _ -> false
  
let infer_const (c: const) : typ =
  match c with
  | CNum _ -> TInt
  | CBool _ -> TBool
  | CTriv -> TUnit
  | CNil -> failwith "Shouldn't get here"

let type_of_bop (o: bop) : typ * typ * typ =
  match o with
  | BAdd | BSub | BMul | BDiv -> (TInt, TInt, TInt)
  | BLt | BLe | BGt | BGe | BEq | BNe -> (TInt, TInt, TBool)
  | BAnd | BOr -> (TBool, TBool, TBool)

let rec check_exp (c: ctx) (e: p_exp) (t: typ) : t_exp =
  match e.edesc with
  | EConst CNil ->
     (match t with
      | TList _ -> mk_t_exp (EConst CNil) e.eloc t
      | _ -> t_error t (TList TUnit) e.eloc)
    (*
  | EConst c ->
     let t' = infer_const c in
     if teq t t' then
       mk_t_exp (EConst c) e.eloc t
     else t_error t t' e.eloc
  | EBinop (b, e1, e2) ->
     let (t1, t2, t3) = type_of_bop b in
     let e1' = check_exp c e1 t1 in
     let e2' = check_exp c e2 t2 in
     if teq t t3 then
       mk_t_exp (EBinop (b, e1', e2')) e.eloc t3
     else
       t_error t t3 e.eloc
  | EUnop (UNot, e0) ->
     let e0' = check_exp c e0 TBool in
     if teq t TBool then
       mk_t_exp (EUnop (UNot, e0')) e.eloc TBool
     else
       t_error t TBool e.eloc
  | EUnop (UNeg, e0) ->
     let e0' = check_exp c e0 TInt in
     if teq t TInt then
       mk_t_exp (EUnop (UNot, e0')) e.eloc TInt
     else
       t_error t TInt e.eloc
  | EFun (x, targ, body) ->
     let c' = add_var c x targ in
     let body' = infer_exp c' body in
     if teq t (TFunction (targ, body.einfo)) then
       mk_t_exp (EFun (x, targ, body')) e.eloc t
     else
       t_error t (TFunction (targ, body.einfo)) e.eloc
  | EIf (e1, e2, e3) ->
     let e1' = check_exp c e1 TBool in
     let e2' = check_exp c e2 t in
     let e3' = check_exp c e3 t in
     mk_t_exp (EIf (e1', e2', e3')) e.eloc t
  | EMatchList (e1, e2, h, tl, e3) ->
     let e1' = infer_exp c e1 in
     (match e1'.einfo with
      | TList t1 ->
         let e2' = check_exp c e2 t in
         let c' = add_var (add_var c h t1) tl (TList t1) in
         let e3' = check_exp c e3 t in
         mk_t_exp (EMatchList (e1', e2', h, tl, e3')) t e.eloc
      | _ -> raise (TError ("Expected list", e1.eloc))
     )
     *)
  | EPair (e1, e2) ->
     (match t with
      | TProd (t1, t2) ->
         let e1' = check_exp c e1 t1 in
         let e2' = check_exp c e2 t2 in
         mk_t_exp (EPair (e1', e2')) e.eloc t
      | _ -> t_error_s t "'a * 'b" e.eloc
     )
  | ECons (e1, e2) ->
     (match t with
      | TList t' ->
         let e1' = check_exp c e1 t' in
         let e2' = check_exp c e2 t in
         mk_t_exp (ECons (e1', e2')) e.eloc t
      | _ -> t_error_s t "'a list" e.eloc
     )
  | EAnnot (e0, t') ->
     if teq t t' then
       let e0' = check_exp c e0 t in
       mk_t_exp (EAnnot (e0', t')) e.eloc t
     else
       t_error t t' e.eloc
  | _ ->
     let e' = infer_exp c e in
     if teq t e'.einfo then
       e'
     else
       t_error t e'.einfo e.eloc

and infer_exp (c: ctx) (e: p_exp) : t_exp =
  match e.edesc with
  | EVar v ->
     (try
        let t = lookup_var c v in
        mk_t_exp (EVar v) e.eloc t
      with Not_found -> raise (TypeError ("Unbound variable " ^ v, e.eloc))
     )
  | EConst CNil -> raise (TypeError ("Cannot infer type of []", e.eloc))
  | EConst c ->
     mk_t_exp (EConst c) e.eloc (infer_const c)
  | EBinop (b, e1, e2) ->
     let (t1, t2, t3) = type_of_bop b in
     let e1' = check_exp c e1 t1 in
     let e2' = check_exp c e2 t2 in
     mk_t_exp (EBinop (b, e1', e2')) e.eloc t3
  | EUnop (UNot, e1) ->
     let e1' = check_exp c e1 TBool in
     mk_t_exp (EUnop (UNot, e1')) e.eloc TBool
  | EUnop (UNeg, e1) ->
     let e1' = check_exp c e1 TInt in
     mk_t_exp (EUnop (UNeg, e1')) e.eloc TInt
  | EFun (x, t, body) ->
     let c' = add_var c x t in
     let body' = infer_exp c' body in
     mk_t_exp (EFun (x, t, body')) e.eloc (TArrow (t, body'.einfo))
  | EIf (e1, e2, e3) ->
     let e1' = check_exp c e1 TBool in
     let e2' = infer_exp c e2 in
     let e3' = check_exp c e3 e2'.einfo in
     mk_t_exp (EIf (e1', e2', e3')) e.eloc e2'.einfo
  | ELet (x, tx, e1, e2) ->
     let e1' =
       match tx with
       | Some tx -> check_exp c e1 tx
       | None -> infer_exp c e1
     in
     let c' = add_var c x e1'.einfo in
     let e2' = infer_exp c' e2 in
     mk_t_exp (ELet (x, tx, e1', e2')) e.eloc e2'.einfo
  | ELetFun (is_rec, f, x, tx, tr, body, e2) ->
     let c' = add_var c x tx in
     let c' =
       if is_rec then
         (match tr with
          | Some tr ->
             add_var c' f (TArrow (tx, tr))
          | None -> raise (TypeError ("Recursive functions must have result type annotated.", e.eloc))
         )
       else c'
     in
     let body' =
       match tr with
       | Some tr -> check_exp c' body tr
       | None -> infer_exp c' body
     in
     let c' = add_var c f (TArrow (tx, body'.einfo)) in
     let e2' = infer_exp c' e2 in
     mk_t_exp (ELetFun (is_rec, f, x, tx, tr, body', e2')) e.eloc e2'.einfo
  | ELetPair (x, y, e1, e2) ->
     let e1' = infer_exp c e1 in
     (match e1'.einfo with
      | TProd (t1, t2) ->
         let c' = add_var (add_var c x t1) y t2 in
         let e2' = infer_exp c' e2 in
         mk_t_exp (ELetPair (x, y, e1', e2')) e.eloc e2'.einfo
      | _ -> raise (TypeError ("Expected pair", e1.eloc))
     )
  | EApp (e1, e2) ->
     let e1' = infer_exp c e1 in
     (match e1'.einfo with
      | TArrow (ta, tr) ->
         let e2' = check_exp c e2 ta in
         mk_t_exp (EApp (e1', e2')) e.eloc tr
      | _ -> raise (TypeError ("Expected function", e1.eloc))
     )
  | EMatchList (e1, e2, h, tl, e3) ->
     let e1' = infer_exp c e1 in
     (match e1'.einfo with
      | TList t1 ->
         let e2' = infer_exp c e2 in
         let c' = add_var (add_var c h t1) tl (TList t1) in
         let e3' = check_exp c' e3 e2'.einfo in
         mk_t_exp (EMatchList (e1', e2', h, tl, e3')) e.eloc e2'.einfo
      | _ -> raise (TypeError ("Expected list", e1.eloc))
     )
  | EPair (e1, e2) ->
     let e1' = infer_exp c e1 in
     let e2' = infer_exp c e2 in
     mk_t_exp (EPair (e1', e2')) e.eloc (TProd (e1'.einfo, e2'.einfo))
  | ECons (e1, e2) ->
     let e1' = infer_exp c e1 in
     let e2' = check_exp c e2 (TList e1'.einfo) in
     mk_t_exp (ECons (e1', e2')) e.eloc e2'.einfo
  | EAnnot (e0, t) ->
     let e0' = check_exp c e0 t in
     mk_t_exp (EAnnot (e0', t)) e.eloc t

     (*
let rec infer_decl (c: ctx) (d: p_decl) : ctx * t_decl =
  match d.ddesc with
  | DVal (x, t, e) ->
     let e' =
       match t with
       | Some t -> check_exp c e t
       | None -> infer_exp c e
     in
     (add_var c x e'.einfo,
      mk_t_decl (DVal (x, t, e')) d.dloc e'.einfo)
  | DFun (is_rec, f, x, tx, tr, body) ->
     let c' = add_var c x tx in
     let c' =
       if is_rec then
         (match tr with
          | Some tr ->
             add_var c' f (TArrow (tx, tr))
          | None -> raise (TypeError ("Recursive functions must have result type annotated.", d.dloc))
         )
       else c'
     in
     let body' =
       match tr with
       | Some tr -> check_exp c' body tr
       | None -> infer_exp c' body
     in
     (add_var c f (TArrow (tx, body'.einfo)),
      mk_t_decl (DFun (is_rec, f, x, tx, tr, body'))
        d.dloc
        (TArrow (tx, body'.einfo))
     )
  | DExp e ->
     let e' = infer_exp c e in
     (c, mk_t_decl (DExp e') d.dloc e'.einfo)

let infer_prog (p: p_prog) : t_prog =
  let (_, p') =
    List.fold_left
      (fun (c, p) d ->
        let (c', d') = infer_decl c d in
        (c', d'::p)
      )
      (Varmap.empty, [])
      p
  in
  List.rev p'
      *)
