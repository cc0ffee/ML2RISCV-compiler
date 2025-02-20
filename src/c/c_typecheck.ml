(** CS443 - C Type-checking **)
(** Stefan Muller - 2022 **)

open C_ast

(* TODO: Stronger types (e.g. only bools for conditionals), unique typing for
 * constants. Function return values. *)

module P = Cprint

module VMap = Varmap

(* variable context, fields context *)
type ctx = (typ * var_scope) VMap.t * (string * typ) list VMap.t 
let ectx = (VMap.empty, VMap.empty)

exception TypeError of string * loc

let t_error expected got (f, l) =
  raise (TypeError (Printf.sprintf "Expected expr of type %s; got %s"
                           (P.string_of_typ expected)
                           (P.string_of_typ got),
                    (f, l)))

let t_error_s expected got (f, l) =
  raise (TypeError (Printf.sprintf "Expected %s; got %s"
                           expected
                           (P.string_of_typ got),
                    (f, l)))

let e_scalar got loc =
  raise (TypeError (Printf.sprintf "Expected scalar type; got %s"
                      "" (* (P.string_of_typ got) *),
                         loc))
                     
let lookup_var ((vctx, _): ctx) (v: var) =
  fst (VMap.find v vctx)

let lookup_var_full ((vctx, _): ctx) (v: var) =
  VMap.find v vctx

let var_scope ((vctx, _) : ctx) (v: var) =
  snd (VMap.find v vctx)

let get_typedefs ((_, fctx): ctx) =
  VMap.map (fun ts -> List.map snd ts) fctx
  
let get_fields ((_, fctx): ctx) (s: string) =
  (*
  Printf.printf "fields: ";
  VMap.iter (fun k _ -> Printf.printf "%s " k) fctx;
  Printf.printf "\n";
   *)
  VMap.find s fctx

  
  
let get_field_i_and_typ (ctx: ctx) (str: string) (fld: string) =
  let rec get_i_and_typ l x i =
    match l with
    | [] -> None
    | (a, b)::t -> if a = x then Some (i, b)
                   else get_i_and_typ t x (i + 1)
  in
  get_i_and_typ (get_fields ctx str) fld 0

let get_field_i ctx str fld =
  match get_field_i_and_typ ctx str fld with
  | Some (i, _) -> Some i
  | None -> None

            (*
let lookup_typ ((_, tctx): ctx) (s: string) =
  VMap.find s tctx
             *)
  
            (*
let rec resolve_typ ctx t =
  match t with
  | TNamedType s ->
     (try resolve_typ ctx (lookup_typ ctx s)
      with Not_found ->
            raise (TypeError ("Couldn't resolve type " ^ s, ("", 0)))
     )
  | _ -> t
             *)

let is_scalar t =
  match t with
  | TBool | TChar | TInt -> true
  | _ -> false

let is_int t =
  match t with
  | TInt -> true
  | _ -> false
  
let rec teq t1 t2 =
  match (t1, t2) with
  | (TVoid, TVoid) -> true
  | (TBool, TBool) -> true
  | (TChar, TChar) -> true
  | (TInt, TInt) -> true
  | (TArray t1, TArray t2) -> teq t1 t2
  | (TStruct s1, TStruct s2) -> s1 = s2
  | (TFunction (tr1, args1), TFunction (tr2, args2)) ->
     teq tr1 tr2
     &&
       (try
          List.fold_left2 (fun b (t1, _) (t2, _) -> b && teq t1 t2)
            true
            args1
            args2
        with Invalid_argument _ -> false)
  | _ -> false

let typecheck_const (c: const) : typ =
  match c with
  | CChar _ -> TChar
  | CInt _ -> TInt

let rec typecheck_lhs (ctx: ctx) (l: p_lhs) : t_lhs =
  match l.ldesc with
  | LHVar v ->
     (try mk_t_lhs (LHVar v) l.lloc (lookup_var ctx v)
      with Not_found -> raise (TypeError ("Undeclared variable " ^ v, l.lloc)))
  | LHArr (v, e2) ->
     (try
       (match lookup_var ctx v with
        | TArray arrt ->
           let e2' = typecheck_exp ctx e2 in
            if is_int (e2'.einfo) then
              mk_t_lhs (LHArr (v, e2'))
                l.lloc
                arrt
            else
              t_error TInt e2'.einfo e2'.eloc
        | _ -> raise (TypeError ("Indexing a non-array", l.lloc))
       )
      with Not_found -> raise (TypeError ("Undeclared variable " ^ v, l.lloc))
     )
  | LHField (v, _, s) ->
   (try
      (match lookup_var ctx v with
       | TStruct st ->
          let fields =
            try get_fields ctx st
            with Not_found ->
              raise (TypeError ("Couldn't resolve type " ^ st, l.lloc))
          in
          (*
          let _ =
            Printf.printf "Fields: ";
            List.iter (fun (s, _) -> Printf.printf "%s " s) fields;
            Printf.printf "\n"
          in
           *)
          let t = try List.assoc s fields
                  with Not_found ->
                        raise (TypeError ("Struct has no field " ^ s, l.lloc))
          in mk_t_lhs (LHField (v, TStruct st, s)) l.lloc t
       | _ -> raise (TypeError ("Accessing a field of a non-struct", l.lloc))
      )
    with Not_found -> raise (TypeError ("Undeclared variable " ^ v, l.lloc))
   )
       
and typecheck_exp (ctx: ctx) (e: p_exp) : t_exp =
  match e.edesc with
  | EConst c -> mk_t_exp (EConst c) e.eloc (typecheck_const c)
  | EVar (v, _) ->
     (try
        let (t, scope) = lookup_var_full ctx v
        in
        mk_t_exp (EVar (v, scope)) e.eloc t
      with Not_found -> raise (TypeError ("Undeclared variable " ^ v, e.eloc)))
  | EBinop (b, e1, e2) ->
     let e1' = typecheck_exp ctx e1 in
     let e2' = typecheck_exp ctx e2 in
     let (etype, rtype) =
       (match b with
        | BAdd | BSub | BMul | BDiv ->
           (TInt, TInt)
        | BGt | BGe | BLt | BLe | BNe | BEq ->
           (TInt, TBool)
        | BAnd | BOr ->
           (TBool, TBool)
       )
     in
     if teq e1'.einfo e2'.einfo then
       if teq e1'.einfo etype then
         mk_t_exp (EBinop (b, e1', e2')) e.eloc rtype
       else t_error etype e1'.einfo e1'.eloc
     else t_error e1'.einfo e2'.einfo e2.eloc

       
  | EUnop (u, e1) ->
     let e1' = typecheck_exp ctx e1 in
     let (etype, rtype) =
       (match u with
        | UNeg -> (TInt, TInt)
        | UNot -> (TBool, TBool))
     in
     if teq e1'.einfo etype then
       mk_t_exp (EUnop (u, e1')) e.eloc e1'.einfo
     else t_error etype e1'.einfo e1'.eloc

  | EAssign (e1, e2) ->
     let e1' = typecheck_lhs ctx e1 in
     let e2' = typecheck_exp ctx e2 in
     if teq e1'.linfo e2'.einfo then
       mk_t_exp (EAssign (e1', e2')) e.eloc e1'.linfo
     else t_error e1'.linfo e2'.einfo e2'.eloc

  | ENewArray (t, n) -> mk_t_exp (ENewArray (t, n)) e.eloc (TArray t)
  | ENewStruct s -> mk_t_exp (ENewStruct s) e.eloc (TStruct s)
  | ECall (ef, args) ->
     let ef' = typecheck_exp ctx ef in
     (match ef'.einfo with
      | TFunction (ret, argts) ->
         let args' =
           try
             List.fold_left2
               (fun rargs arg (argt, _) ->
                 let arg = typecheck_exp ctx arg in
                 if teq arg.einfo argt then
                   arg::rargs
                 else
                   t_error argt arg.einfo arg.eloc
               )
               []
               args
               argts
           with Invalid_argument _ ->
             raise (TypeError ("Wrong number of arguments", e.eloc))
         in
         let args' = List.rev args' in
         mk_t_exp (ECall (ef', args')) e.eloc ret
      | _ -> raise (TypeError ("Applying a non-function", e.eloc))
     )
  | EArrIndex (e1, e2) ->
     let e1' = typecheck_exp ctx e1 in
     (match e1'.einfo with
      | TArray arrt ->
         let e2' = typecheck_exp ctx e2 in
         if is_int (e2'.einfo) then
           mk_t_exp (EArrIndex (e1', e2')) e.eloc arrt
         else
           t_error TInt e2'.einfo e2'.eloc
      | _ -> raise (TypeError ("Indexing a non-array", e.eloc))
     )

  | EField (e1, s) ->
     let e1' = typecheck_exp ctx e1 in
     (match e1'.einfo with
      | TStruct sname ->
         (try
            let fields = get_fields ctx sname
            in
            mk_t_exp (EField (e1', s)) e.eloc (List.assoc s fields)
          with Not_found -> raise (TypeError ("Struct has no field " ^ s,
                                              e.eloc))
         )
      | _ -> (t_error_s "struct" (e1'.einfo) e.eloc)
     )
  | ECast (e0, t) ->
     let e0' = typecheck_exp ctx e0 in
     if t = TVoid || e0'.einfo = TVoid then
       raise (TypeError ("Cast to/from void not allowed", e.eloc))
     else
       mk_t_exp (ECast (e0', t)) e.eloc t


let add_local_var (vctx, tctx) (t, s) =
  (VMap.add s (t, Local) vctx, tctx)
     
let add_global_var (vctx, tctx) (t, s) =
  (VMap.add s (t, Global) vctx, tctx)

let add_typ (vctx, tctx) (t1, t2) =
  (vctx, VMap.add t1 t2 tctx)
          
let rec typecheck_def (ctx: ctx) (d: p_def) : ctx * t_def =
  match d.ddesc with
  | DFun (s, t, b) ->
     (match t with
      | TFunction (ret, argts) ->
         let ctx' =
           List.fold_left add_local_var ctx argts
         in
         let ctx' = add_global_var ctx' (t, s) in
         let (ctx'', b') = typecheck_stmt ret ctx' b in
         (*
         Printf.printf "Adding %s with type " s;
         Cprint.print_type (fun () -> ()) t;
         Cprint.force_new_line ();
          *)
         (add_global_var ctx (t, s), mk_def (DFun (s, t, b')) d.dloc)
      | _ -> raise (TypeError ("Not a function type", d.dloc))
     )
  | DName defs ->
     let (ctx', defs') =
       List.fold_left
         (fun (ctx, defs) (s, t, e) ->
           match e with
           | Some e ->
              let e' = typecheck_exp ctx e
              in
              if teq t e'.einfo then
                (add_global_var ctx (t, s), (s, t, Some e')::defs)
              else t_error t e'.einfo e'.eloc
           | None -> (add_global_var ctx (t, s), (s, t, None)::defs)
         )
         (ctx, [])
         defs
     in
     (ctx', mk_def (DName (List.rev defs')) d.dloc)
  | DTypeDef defs -> (ctx, mk_def (DTypeDef defs) d.dloc)
                       (*
     (List.fold_left add_typ ctx defs, mk_def (DTypeDef defs) d.dloc)
                        *)
  | DStructDef (n, f) ->
     (add_typ ctx (n, f), mk_def (DStructDef (n, f)) d.dloc)

and typecheck_stmt (rtype: typ) (ctx: ctx) (s: p_stmt) : ctx * t_stmt =
  let typecheck_stmt = typecheck_stmt rtype in
  match s.sdesc with
  | SDecl (v, t, None) -> (add_local_var ctx (t, v),
                           mk_stmt (SDecl (v, t, None)) s.sloc)
  | SDecl (v, t, Some e) ->
     let ctx' = add_local_var ctx (t, v) in
     let e' = typecheck_exp ctx e in
     (ctx',
      if teq t e'.einfo then
        mk_stmt (SDecl (v, t, Some e')) s.sloc
      else
        t_error t e'.einfo e'.eloc
     )
    
  | SBlock ss ->
     let (c', ss') = List.fold_left
                       (fun (c, ss') s ->
                         let (c', s') = typecheck_stmt c s in
                         (c', s'::ss'))
                       (ctx, [])
                       ss
     in
     (c', mk_stmt (SBlock (List.rev ss')) s.sloc)
  | SExp e ->
     (ctx, mk_stmt (SExp (typecheck_exp ctx e)) s.sloc )
  | SIf (e, if_branch, else_branch) ->
     let e' = typecheck_exp ctx e in
     if teq e'.einfo TBool then
       let (_, if') = typecheck_stmt ctx if_branch in
       let (_, else') = typecheck_stmt ctx else_branch in
       (ctx, mk_stmt (SIf (e', if', else')) s.sloc)
     else
       t_error TBool e'.einfo e'.eloc
  | SFor (init, cond, post, body) ->
     let init' = typecheck_exp ctx init in
     let cond' = typecheck_exp ctx cond in
     if teq cond'.einfo TBool then
       let post' = typecheck_exp ctx post in
       let (_, body') = typecheck_stmt ctx body in
       (ctx, mk_stmt (SFor (init', cond', post', body')) s.sloc)
     else
       t_error TBool cond'.einfo cond'.eloc
  | SBreak -> (ctx, mk_stmt SBreak s.sloc)
  | SContinue -> (ctx, mk_stmt SContinue s.sloc)
  | SReturn None ->
     if teq rtype TVoid then
       (ctx, mk_stmt (SReturn None) s.sloc)
     else
       t_error rtype TVoid s.sloc
  | SReturn (Some e) ->
     let e' = typecheck_exp ctx e in
     if teq rtype e'.einfo then
       (ctx, mk_stmt (SReturn (Some e')) s.sloc)
     else
       t_error rtype e'.einfo e'.eloc

let typecheck_prog (p, flds) : ctx * (t_def list) =
  (*
  Printf.printf "fields: ";
  VMap.iter (fun k _ -> Printf.printf "%s " k) flds;
  Printf.printf "\n";
   *)
  let (c, ds) =
    List.fold_left
      (fun (c, ds) d ->
        let (c', d') = typecheck_def c d in
        (c', d'::ds)
      )
      ((VMap.empty, flds), [])
      p
  in
  (c, List.rev ds)
