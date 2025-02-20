open Iit_ast

module P = Iit_print

module VMap = Varmap

type ctx = typ VMap.t

exception TypeError of string * loc

let t_error expected got loc =
  raise (TypeError (Printf.sprintf "Expected expr of type %s; got %s"
                           (P.string_of_typ expected)
                           (P.string_of_typ got),
                         loc))

let lookup (ctx: ctx) (v: var) =
  VMap.find v ctx
         
let rec typecheck_const (c: const) : typ =
  match c with
  | CChar _ -> TCharacter
  | CInt _ -> TInteger

let rec typecheck_exp (ctx: ctx) (e: p_exp) : t_exp  =
  match e.edesc with
  | EConst c -> mk_t_exp (EConst c) e.eloc (typecheck_const c)
  | EVar v ->
     (try
        mk_t_exp (EVar v) e.eloc (lookup ctx v)
      with Not_found -> raise (TypeError ("Undeclared variable " ^ v, e.eloc))
     )
  | EBinop (b, e1, e2) ->
     let e1' = typecheck_exp ctx e1 in
     let e2' = typecheck_exp ctx e2 in
     let e1t = e1'.einfo in
     let e2t = e2'.einfo in
     let (etype, rtype) =
       (match b with
        | BAdd | BSub | BMul | BDiv ->
           (TInteger, TInteger)
        | BGt | BGe | BLt | BLe | BNe | BEq ->
           (TInteger, TLogical)
        | BAnd | BOr ->
           (TLogical, TLogical)
       )
     in
     (if e1t <> e2t then t_error e1t e2t e2.eloc
      else
        if e1t = etype then ()
        else t_error etype e1t e1.eloc
     );
     mk_t_exp (EBinop (b, e1', e2')) e.eloc rtype
  | EAssign (e1, e2) ->
     let e1' = typecheck_exp ctx e1 in
     let e2' = typecheck_exp ctx e2 in
     let e1t = e1'.einfo in
     let e2t = e2'.einfo in
     (if e1t <> e2t then t_error e1t e2t e2.eloc
      else ()
     );
     mk_t_exp (EAssign (e1', e2')) e.eloc e1t
  | EUnop (u, e1) ->
     let e1' = typecheck_exp ctx e1 in
     let e1t = e1'.einfo in
     let (etype, rtype) =
       (match u with
        | UNeg -> (TInteger, TInteger)
        | UNot -> (TLogical, TLogical)
        | UChar -> (e1t, TCharacter)
        | ULog -> (e1t, TLogical)
        | UInt -> (e1t, TInteger)
       )
     in
     (if e1t = etype then ()
      else t_error etype e1t e1.eloc
     );
     mk_t_exp (EUnop (u, e1')) e.eloc rtype
       
let rec typecheck_stmt (ctx: ctx) (s: p_stmt) : ctx * t_stmt =
  let add_to_ctx t ctx v =
    VMap.add v t ctx
  in
  match s.sdesc with
  | SDecl (t, vars) ->
     let ctx' = List.fold_left (add_to_ctx t) ctx vars in
     (ctx', mk_stmt s.sloc(SDecl (t, vars)))
  | SDo ss ->
     let (c', ss') = List.fold_right
                       (fun s (c, ss') ->
                         let (c', s') = typecheck_stmt c s in
                         (c', s'::ss'))
                       ss
                       (ctx, [])
     in
     (c', mk_stmt s.sloc (SDo ss'))
  | SExp e -> (ctx, mk_stmt s.sloc (SExp (typecheck_exp ctx e)))
  | SIf (e, if_branch, maybe_else_branch) ->
     let e' = typecheck_exp ctx e in
     (if e'.einfo <> TLogical then t_error (TLogical) e'.einfo e'.eloc);
     let (_, if') = typecheck_stmt ctx if_branch in
     let (_, else') =
       (match maybe_else_branch with
        | Some se -> (ctx, Some (snd (typecheck_stmt ctx se)))
        | None -> (ctx, None))
     in
     (ctx, mk_stmt s.sloc (SIf (e', if', else')))
  | SWhile (e, body) ->
     let e' = typecheck_exp ctx e in
     (if e'.einfo <> TLogical then t_error (TLogical) e'.einfo e'.eloc);
     let (_, body') = typecheck_stmt ctx body in
     (ctx, mk_stmt s.sloc (SWhile (e', body')) )
  | SStop -> (ctx, mk_stmt s.sloc SStop)

let typecheck_prog (p: p_stmt list) : t_stmt list =
  let (_, ss) =
    List.fold_left
      (fun (c, ss) s ->
        let (c', s') = typecheck_stmt c s in
        (c', s'::ss)
      )
      (VMap.empty, [])
      p
  in
  List.rev ss
