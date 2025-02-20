(** CS443 - Conversion to SSA Form **)
(** Stefan Muller - 2022 **)

open Llvm_bbcfg

module L = Llvm_ast
open L
module VMap = Map.Make
                (struct
                  type t = L.var
                  let compare = L.compare_var
                end)

module SMap = Varmap

module NSet = Set.Make
                (struct
                  type t = G.node
                  let compare = G.cmp_node
                end)
module SSet = Set.Make
                (struct
                  type t = string
                  let compare = String.compare
                end)         

let def_block is = List.concat (List.map Llvm_utils.def_inst is)

let remove_unreachable cfg bbs entry =
  let all_bbs = NSet.of_list (G.nodes cfg) in
  let rec reachable rset n =
    if NSet.mem n rset then
      (
       rset)
    else
      let rset' = NSet.add n rset in
      let next = succs cfg n in
      List.fold_left reachable rset' next
  in
  let rset = reachable NSet.empty entry in
  let r_names =
    NSet.fold (fun n s -> SSet.add (G.get_data n) s) rset SSet.empty
  in
  let unr = NSet.diff all_bbs rset in
  (NSet.fold
    (fun n s -> G.rem_node s n)
    unr
    cfg,
   SMap.fold
     (fun n is m -> if SSet.mem n r_names then SMap.add n is m else m)
     bbs
     SMap.empty
  )

let base_var v =
  let base_name s =
    try
      String.sub s 0 (String.rindex s '$')
    with Not_found -> s
  in
  match v with
  | Local s -> Local (base_name s)
  | Global s -> Global (base_name s)

let convert_block_to_ssa (ts: L.typ Llvm_typecheck.LLVarmap.t)
      (name: string) (params: L.var list)
      (insts: L.inst list) =
  let (cfg, bbs, entry) = bb_cfg name insts in
  let (cfg, bbs) = remove_unreachable cfg bbs entry in
  let (dt, df) = G.dom_tree_and_front cfg entry in
  (*
  let _ =
    List.iter
      (fun n -> Printf.printf "%s is the predecessor of %s\n"
                  (G.get_data n)
                  (String.concat "," (List.map G.get_data (G.succs cfg n))))
      (G.nodes cfg)
  in
   *)
  let adddefsite n defsites v =
    let ds =
      try VMap.find v defsites with Not_found -> []
    in
    let ds' = if not (List.mem n ds) then n::ds else ds
    in
    VMap.add v ds' defsites
  in
  let param_defsites =
    List.fold_left
      (fun ds p ->
        adddefsite entry ds p
      )
      VMap.empty
      params
  in
  let orig_defsites =
    List.fold_left
      (fun ds n ->
        List.fold_left 
          (adddefsite n)
          ds
          (def_block (SMap.find (G.get_data n) bbs))
      )
      param_defsites
      (G.nodes cfg)
  in
  let rec compute_phis v phis defsites =
    match defsites with
    | [] -> phis
    | n::defsites ->
       (*
       let _ = Printf.printf "Checking defsite %s of %s\n"
                 (G.get_data n)
                 (L.string_of_var v)
       in
       let _ = Printf.printf "Dominance frontier of %s is %s\n"
                 (G.get_data n)
                 (String.concat " " (List.map G.get_data (G.succs df n)))
       in
        *)
       let (phis', ds') =
         List.fold_left
           (fun (phis', ds') y ->
             (if List.mem y phis' then
                (phis', ds')
              else
                (y::phis',
                 if not (List.mem y (VMap.find v orig_defsites))
                 then y::ds'
                 else ds')
             )
           )
           (phis, defsites)
           (G.succs df n)
       in
       compute_phis v phis' ds'
  in
  let phis =
    VMap.mapi
      (fun v ds -> compute_phis v [] (entry::ds))
      orig_defsites
  in
  let find_typ v =
    try Llvm_typecheck.LLVarmap.find v ts
    with Not_found -> Llvm_typecheck.LLVarmap.find (base_var v) ts
  in
  let bbs =
    VMap.fold
      (fun v phis bbs ->
        (*Printf.printf "Adding phis for %s to %s\n"
          (L.string_of_var v)
          (String.concat " " (List.map G.get_data phis)); *)
        List.fold_left
          (fun bbs n ->
            match SMap.find (G.get_data n) bbs with
            | (ILabel l)::insts ->
               SMap.add
                 (G.get_data n)
                 ((ILabel l)
                  ::(IPhi (v, find_typ v, []))
                  ::insts)
                 bbs
            | _ -> failwith "block should start with label"
          )
          bbs
          phis
      )
      phis
      bbs
  in
  let ctr = ref 0 in
  let new_var v =
    ctr := !ctr + 1;
    (match v with
     | Local s -> Local (s ^ "$" ^ (string_of_int (!ctr)))
     | Global s -> Global (s ^ "$" ^ (string_of_int (!ctr)))
    )
  in
  let rename_rvar stacks v =
    match v with
    | Global _ -> v
    | Local _ ->
       (try
          (match VMap.find v stacks with
           | Var (Local v) -> Local v
           | _ -> failwith ("shouldn't happen: weird val: " ^
                                      (L.string_of_var v))
          )
        with Not_found -> failwith ("shouldn't happen: not found: " ^
                                      (L.string_of_var v))
       )
  in
  let rename_val stacks v =
    match v with
    | Var (Local v) -> (try VMap.find (Local v) stacks
                        with Not_found -> failwith ("shouldn't happen: not found: " ^
                                      v))
    | _ -> v
  in
  let rename_lvar stacks v =
    let v' = new_var v in
    (VMap.add v (Var v') stacks, v')
  in
  let rename_i stacks i =
    match i with
    | ISet (d, t, v) ->
       let v' = rename_val stacks v in
       let (s', d') = rename_lvar stacks d in
       (s', ISet (d', t, v'))
    | IBinop (d, b, t, v1, v2) ->
       let v1' = rename_val stacks v1 in
       let v2' = rename_val stacks v2 in
       let (s', d') = rename_lvar stacks d in
       (s', IBinop (d', b, t, v1', v2'))
    | ICmp (d, c, t, v1, v2) ->
       let v1' = rename_val stacks v1 in
       let v2' = rename_val stacks v2 in
       let (s', d') = rename_lvar stacks d in
       (s', ICmp (d', c, t, v1', v2'))
    | ICast (d, ct, t1, v, t2) ->
       let v' = rename_val stacks v in
       let (s', d') = rename_lvar stacks d in
       (s', ICast (d', ct, t1, v', t2))
    | ICondBr (v, l1, l2) ->
       (stacks, ICondBr (rename_val stacks v, l1, l2))
    | IRet (Some (t, v)) ->
       (stacks, IRet (Some (t, rename_val stacks v)))
    | ICall (d, t, v1, args) ->
        let args' = List.map (fun (t, v) -> (t, rename_val stacks v)) args in
        let v1' = rename_rvar stacks v1 in
        let (s', d') = rename_lvar stacks d in
        (s', ICall (d', t, v1', args'))
    | IGetElementPtr (d, t, v, inds) ->
        let inds' = List.map (fun (t, v) -> (t, rename_val stacks v)) inds in
        let v' = rename_rvar stacks v in
        let (s', d') = rename_lvar stacks d in
        (s', IGetElementPtr (d', t, v', inds'))
    | IAlloca (d, t, n) ->
       let (s', d') = rename_lvar stacks d in
       (s', IAlloca (d', t, n))
    | ILoad (d, t, v) ->
       let v' = rename_rvar stacks v in
       let (s', d') = rename_lvar stacks d in
       (s', ILoad (d', t, v'))
    | IStore (t, vl, vr) ->
       let vl' = rename_val stacks vl in
       let vr' = rename_rvar stacks vr in
       (stacks, IStore (t, vl', vr'))
    | IPhi (d, t, preds) ->
       let (s', d') = rename_lvar stacks d in
       (s', IPhi (d', t, preds))
    | _ -> (stacks, i)
  in
  let rename_block stacks l =
    let (stacks', revl) =
      List.fold_left
        (fun (stacks, l') i ->
          let (stacks', i') = rename_i stacks i in
          (stacks', i'::l'))
        (stacks, [])
        l
    in
    (stacks', List.rev revl)
  in
  let rec add_phis_to_block stacks pred l =
    match l with
    | (ILabel la)::l' -> (ILabel la)::(add_phis_to_block stacks pred l')
    | (IPhi (v, t, preds))::l' ->
       (IPhi (v, t, (G.get_data pred, (rename_val stacks (Var (base_var v))))::preds))::
         (add_phis_to_block stacks pred l')
    | _ -> l
  in
  let rec handle_node stacks bbs n =
    let l = G.get_data n in
    let block = SMap.find l bbs in
    let (stacks', block') = rename_block stacks block in
    let bbs' = SMap.add l block' bbs in
    let bbs' =
      List.fold_left
        (fun bbs s ->
          (*
          let _ = Printf.printf "Adding phis to %s\n" (G.get_data s) in
           *)
          let block = SMap.find (G.get_data s) bbs in
          SMap.add (G.get_data s) (add_phis_to_block stacks' n block) bbs)
        bbs'
        (G.succs cfg n)
    in
    let bbs' =
      List.fold_left
        (fun bbs n -> handle_node stacks' bbs n)
        bbs'
        (G.succs dt n)
    in
    bbs'
  in
  let init_stacks =
    VMap.mapi (fun v _ -> Const 0) orig_defsites
  in
  let init_stacks =
    List.fold_left (fun s v -> VMap.add v (Var v) s) init_stacks params
  in
  let bbs = handle_node init_stacks bbs entry in
  (* Make sure the entry block gets output first. *)
  let entryinsts = SMap.find (G.get_data entry) bbs in
  let bbs = SMap.remove (G.get_data entry) bbs in
  entryinsts @
    (SMap.fold
       (fun _ block insts -> insts @ block)
       bbs
       [])

let convert_func_to_ssa ts (f: func) =
  make_func
    f.f_name
    f.f_ret
    f.f_args
    (convert_block_to_ssa
       ts
       f.f_name
       (List.map (fun (_, s) -> L.Local s) f.f_args)
       (Array.to_list f.f_body))

let convert_prog_to_ssa ts (p, tds) =
  (List.map (convert_func_to_ssa ts) p, tds)

let unssa_func f =
  let rec collect_phis i =
    if i >= (Array.length f.f_body) - 1 then []
    else
      match f.f_body.(i + 1) with
      | IPhi (d, t, preds) -> (d, t, preds)::(collect_phis (i + 1))
      | _ -> []
  in
  let do_phi my_label (d, t, preds) =
    try
      ISet (d, t, List.assoc my_label preds)
    with Not_found -> failwith ("pred " ^ my_label ^ " isn't found in phi")
  in
  let rec unssa_body l insts =
    match insts with
    | [] -> []
    | (ILabel l')::insts -> (ILabel l')::(unssa_body l' insts)
    | (IPhi _ )::insts -> unssa_body l insts
    | (IBr l')::insts ->
       let phis = collect_phis (SMap.find l' f.f_labels) in
       (List.map (do_phi l) phis) @ (IBr l')::(unssa_body l' insts)
    | (ICondBr (v, l1, l2))::insts ->
       let phis1 = collect_phis (SMap.find l1 f.f_labels) in
       let phis2 = collect_phis (SMap.find l2 f.f_labels) in
       (List.map (do_phi l) phis1) @ (List.map (do_phi l) phis2)
       @ (ICondBr (v, l1, l2))::(unssa_body l insts)
    | i::insts -> i::(unssa_body l insts)
  in
  make_func
    f.f_name
    f.f_ret
    f.f_args
    (unssa_body (Config.entry_label_of_fun f.f_name) (Array.to_list f.f_body))
       
let unssa_prog (p, tds) = (List.map unssa_func p, tds)
