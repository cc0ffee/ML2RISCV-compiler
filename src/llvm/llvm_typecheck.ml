(** CS443 - LLVM Type-checker **)
(** Stefan Muller - 2022 **)

open Llvm_ast

module LLVarmap =
  Map.Make(struct type t = var
                  let compare = compare_var
           end)

exception TypeError of string * inst

let rec teq t1 t2 =
  match (t1, t2) with
  | (TInteger s1, TInteger s2) -> s1 = s2
  | (TPointer t1, TPointer t2) -> teq t1 t2
  | (TFunction (t1r, targs1), TFunction (t2r, targs2)) ->
     teq t1r t2r &&
       (try
          List.fold_left2 (fun b t1 t2 -> b && teq t1 t2) true targs1 targs2
        with Invalid_argument _ -> false)
  | (TVoid, TVoid) -> true
  | (TArray t1, TArray t2) -> teq t1 t2
  | (TStruct s1, TStruct s2) -> s1 = s2
  | _ -> false

let deferred_ts : typ LLVarmap.t ref = ref LLVarmap.empty
let rec typecheck_inst ?(enforce_ssa = false) 
          (ret_type: typ) (tds: typdefs) (ts: typ LLVarmap.t) (i: inst)
        : typ LLVarmap.t =
  let sv = Llvm_print.string_of_var in
  let st = Llvm_print.string_of_typ in
  let add_type v t : typ LLVarmap.t =
    if LLVarmap.mem v ts then
      if enforce_ssa then
        raise (TypeError ((sv v) ^ " defined twice", i))
      else
        if teq t (LLVarmap.find v ts) then ts
        else raise (TypeError ((sv v) ^ " is defined with type"
                               ^ (st (LLVarmap.find v ts))
                               ^ " but then given type "
                               ^ (st t),
                               i))
    else
      if LLVarmap.mem v (!deferred_ts) then
        if teq t (LLVarmap.find v (!deferred_ts)) then
          (deferred_ts := LLVarmap.remove v (!deferred_ts);
           LLVarmap.add v t ts)
        else raise (TypeError ((sv v) ^ " is defined with type"
                               ^ (st (LLVarmap.find v ts))
                               ^ " but then given type "
                               ^ (st t),
                               i))
      else
        LLVarmap.add v t ts
  in
  let v_not_found v =
    raise (TypeError ("Undeclared variable " ^ (sv v), i))
  in
  let compat_with_type (t: typ) (v: value) =
    match (t, v) with
    | (TInteger _, Const _) -> ()
    | (TPointer _, Const _) -> ()
    | (_, Const _) ->
       raise (TypeError ("Constant given non-integer type", i))
    | (_, Var v) ->
       (try
          if teq t (LLVarmap.find v ts) then ()
          else raise (TypeError ((sv v) ^ " doesn't have type " ^ (st t), i))
        with Not_found -> deferred_ts := LLVarmap.add v t (!deferred_ts)
       )
  in
  match i with
  | ILabel _ -> ts
  | ISet (d, t, v) ->
     (compat_with_type t v;
      add_type d t)
  | IBinop (d, _, ((TInteger _) as t), v1, v2) ->
     (compat_with_type t v1;
      compat_with_type t v2;
      add_type d t)
  | IBinop _ -> raise (TypeError ("Binops only defined for ints", i))
  | ICmp (d, _, ((TInteger _) as t), v1, v2) ->
     (compat_with_type t v1;
      compat_with_type t v2;
      add_type d (TInteger 1))
  | ICmp _ -> raise (TypeError ("Comparisons only defined for ints", i))
  | ICast (d, CBitcast, ((TInteger n1) as it), v, ((TInteger n2) as ot))
       when n1 = n2 ->
     (compat_with_type it v;
      add_type d ot)
  | ICast (d, CBitcast, ((TPointer _) as it), v, ((TPointer _) as ot))
    | ICast (d, CPtrtoint, ((TPointer _) as it), v, ((TInteger _) as ot))
    | ICast (d, CInttoptr, ((TInteger _) as it), v, ((TPointer _) as ot)) ->
     (compat_with_type it v;
      add_type d ot)
  | ICast (d, CZext, TInteger s1, v, TInteger s2)
    | ICast (d, CSext, TInteger s1, v, TInteger s2) ->
     if s2 > s1 then
       (compat_with_type (TInteger s1) v;
        add_type d (TInteger s2))
     else
       raise (TypeError ("dest type must be larger than src type", i))
  | ICast (d, CTrunc, TInteger s1, v, TInteger s2) ->
     if s2 < s1 then
       (compat_with_type (TInteger s1) v;
        add_type d (TInteger s2))
     else
       raise (TypeError ("dest type must be smaller than src type", i))
  | ICast _ -> raise (TypeError ("Invalid cast", i))
  | IBr _ -> ts
  | ICondBr (v, _, _) ->
     compat_with_type (TInteger 1) v;
     ts
  | IRet (Some (t, v)) ->
     if teq t ret_type then
       (compat_with_type t v;
        ts)
     else raise (TypeError ("Expected return type is " ^ (st ret_type), i))
  | IRet None -> ts
  | ICall (d, t, v, args) ->
     (try
        (match LLVarmap.find v ts with
         | TPointer (TFunction (rt, targs)) ->
            if teq t rt then
              (List.iter2
                 (fun t1 (t2, v) ->
                   if teq t1 t2 then
                     compat_with_type t2 v
                   else raise (TypeError ("Arg has wrong type", i))
                 )
                 targs
                 args;
               add_type d rt
              )
            else raise (TypeError ("Wrong return type", i))
         | _ -> raise (TypeError ("Var doesn't have function ptr type", i))
        )
      with Not_found -> v_not_found v
         | Invalid_argument _ ->
            raise (TypeError ("Wrong number of arguments", i))
     )
  | IGetElementPtr (d, t, v, inds) ->
     let rec do_gep t inds =
       match (t, inds) with
       | (_, []) -> t
       | (TPointer t, (((TInteger _) as ti), v)::inds) ->
          (compat_with_type ti v;
           do_gep t inds)
       | (TPointer t, _) -> raise (TypeError ("Indices must have int type", i))
       | (TStruct st, (TInteger 32, Const n)::inds) ->
          (try
             do_gep (List.nth (Varmap.find st tds) n) inds
           with Not_found ->
             raise (TypeError ("Unrecognized type %%" ^ st, i))
          )
       | _ -> raise (TypeError ("Invalid type to getelementptr", i))
     in
     add_type d (TPointer (do_gep (TPointer t) inds))
  | IAlloca (d, t, n) ->
     add_type d (TPointer t)
  | ILoad (d, t, v) ->
     (compat_with_type (TPointer t) (Var v);
      add_type d t)
  | IStore (t, vl, vr) ->
     (compat_with_type (TPointer t) (Var vr);
      compat_with_type t vl;
      ts)
  | IPhi (d, t, preds) ->
     (List.iter
        (fun (_, v) -> compat_with_type t v)
        preds);
     add_type d t
       

let typecheck_fun ?(enforce_ssa = false) tds ts f =
  let ts_with_args =
    List.fold_left
      (fun ts (t, n) ->
        LLVarmap.add (Local n) t ts
      )
      ts
      f.f_args
  in
  Array.fold_left
    (typecheck_inst ~enforce_ssa:enforce_ssa f.f_ret tds)
    ts_with_args
    f.f_body

let lib_ts =
  LLVarmap.singleton
    (Global "malloc")
    (TPointer (TFunction ((TPointer (TInteger 8)), [(TInteger 32)])))
  
let typecheck_prog ?(enforce_ssa = false) (p, tds) =
  let funs =
    List.fold_left
      (fun ts f ->
        LLVarmap.add (Global f.f_name)
          (TPointer (TFunction (f.f_ret, List.map fst f.f_args)))
          ts
      )
      lib_ts
      p
  in
  List.fold_left
    (typecheck_fun ~enforce_ssa:enforce_ssa tds)
    funs
    p
