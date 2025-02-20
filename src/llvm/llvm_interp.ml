(** CS443 - LLVM Interpreter **)
(** Stefan Muller - 2022 **)

open Llvm_ast

type mem = int array
type loc = string * int
         
exception RuntimeError of string * loc

let initstack () =
  (Array.make (1024 * 1024) 0, 0)

let initheap () = (Array.make (1024 * 1024) 0, 1024 * 1024)

let cost = ref 0

let rec interp ((p, ctx): prog * typdefs) (globs: value Varmap.t) =
  let _ = cost := 0 in
  let get_func_by_name fname =
    try List.find (fun f' -> f'.f_name = fname) p
    with Not_found ->
      raise (RuntimeError ("Invalid function: " ^ fname, ("", 0)))
  in

  let get_func_by_num n =
    try List.nth p n
    with _ -> raise (RuntimeError ("Invalid function: " ^
                                     (string_of_int n), ("", 0)))
  in
  
  let rec interp_int
            (currfunc: func)
            (prevlabel: label)
            (pc: int)
            (regs: value Varmap.t)
            (globs: value Varmap.t)
            (stack: mem * int)
            (heap: mem * int) =

    let _ = cost :=
              (!cost) +
                (match currfunc.f_body.(pc) with
                 | ILabel _ | IPhi _ -> 0
                 | IBinop (_, BMul, _, _, _) 
                   | IBinop (_, BDiv, _, _, _) -> 3
                 | ISet _ | IBinop _ | ICmp _ | ICast _
                   | IGetElementPtr _ | IAlloca _ -> 1
                 | IBr _ | ICondBr _ -> 5
                 | ILoad _ | IStore _ -> 10
                 | ICall _ | IRet _ -> 15
                )
    in
    
    (* let _ = Printf.printf "%s\n" currfunc.f_name in *)
    let loc = (currfunc.f_name, pc) in
    let lookup (v: var)  =
      try
        match v with
        | Local s -> Varmap.find s regs
        | Global s -> Varmap.find s globs
      with Not_found -> Const 0
                              (* raise (RuntimeError ("Invalid var: " ^ (string_of_var v), loc)) *)
    in

    let update (v: var) (n: value) =
      match v with
      | Local v -> (Varmap.add v n regs, globs)
      | Global v -> (regs, Varmap.add v n globs)
    in

    let interp_raw_value v =
      match v with
      | Var v -> lookup v
      | _ -> v
    in
    
    let interp_value v =
      match v with
      | Const n -> n
      | Var v -> (match lookup v with
                  | Const n -> n
                  | Var _ -> raise (RuntimeError ("unexpectedv", loc))
                 )
    in

    let interp_binop d b v1 v2 =
      let (n1, n2) = (interp_value v1, interp_value v2) in
      let f = match b with
        | BAdd -> (+)
        | BSub -> (-)
        | BMul -> ( * )
        | BDiv -> (/)
        | BAnd -> (land)
        | BOr -> (lor)
        | BXor -> (lxor)
      in
      update d (Const (f n1 n2))
    in

    let interp_cmp d c v1 v2 =
      let (n1, n2) = (interp_value v1, interp_value v2) in
      let f a b =
        let f' = match c with
          | CEq -> (=)
          | CNe -> (<>)
          | CSGt -> (>)
          | CSGe -> (>=)
          | CSLt -> (<)
          | CSLe -> (<=)
        in
        if f' a b then 1 else 0
      in
      update d (Const (f n1 n2))
    in

    let rec eatphis regs globs pc =
      let lookup (v: var)  =
        try
          match v with
          | Local s -> Varmap.find s regs
          | Global s -> Varmap.find s globs
        with Not_found -> Const 0
      in
      let update (v: var) (n: value) =
        match v with
        | Local v -> (Varmap.add v n regs, globs)
        | Global v -> (regs, Varmap.add v n globs)
      in
      let interp_raw_value v =
        match v with
        | Var v -> lookup v
        | _ -> v
      in
      match currfunc.f_body.(pc) with
      | IPhi (d, _, labels) ->
         let v =
           (* Printf.printf "Using val of %s for %s\n"
              prevlabel
              (Llvm_print.string_of_var d); *)
           (try List.assoc prevlabel labels
            with Not_found ->
              raise (RuntimeError ("No value for predecessor " ^ prevlabel,
                                   (currfunc.f_name, pc + 1)))
           )
         in
         let (regs, globs) = update d (interp_raw_value v) in
         eatphis regs globs (pc + 1)
      | _ -> (regs, globs, pc)
    in
    
    let do_branch l =
      let pc = try Varmap.find l currfunc.f_labels
          with Not_found -> raise (RuntimeError ("Invalid label: " ^ l,
                                                 loc))
      in
      let (regs, globs, pc) = eatphis regs globs (pc + 1) in
      interp_int currfunc l pc regs globs stack heap
    in

    let do_getelementptr t base inds =
      let rec size_of_first i ts a =
        match ts with
        | [] -> raise (RuntimeError ("Index out of bounds", loc))
        | t::ts ->
           if i = 0 then (a, t)
           else (size_of_first (i - 1) ts (a + (sizeof ctx t)))
      in
      let rec iter_gep t base inds =
        match (inds, t) with
        | ([], _) -> base
        | (_, TInteger _) | (_, TPointer _) | (_, TVoid) | (_, TFunction _) ->
           raise (RuntimeError ("Type cannot be indexed", loc))
        | ((_, v)::inds, TArray t') ->
           let i = interp_value v in
           iter_gep t' (base + i * (sizeof ctx t')) inds
        | ((_, v)::inds, TStruct ts) ->
           let i = interp_value v in
           let (sizebefore, t') = size_of_first i (Varmap.find ts ctx) 0
           in
           iter_gep t' (base + sizebefore) inds
      in
      match inds with
      | [] -> base
      | (_, v)::inds ->
         let i = interp_value v in
         iter_gep t (base + i * (sizeof ctx t)) inds
    in
    (*
    let _ = Llvm_print.print_inst Format.std_formatter currfunc.f_body.(pc)
    in
    let _ = Format.print_newline ()
    in
     *)


    match currfunc.f_body.(pc) with
    | ILabel newlabel ->
       if prevlabel = "" then
         (* At the beginning of a function *)
         interp_int currfunc newlabel (pc + 1) regs globs stack heap
       else
         raise (RuntimeError ("Unexpected label " ^ newlabel, loc))
    | ISet (d, _, v) ->
       let (regs, globs) = update d (interp_raw_value v) in
       interp_int currfunc prevlabel (pc + 1) regs globs stack heap
    | ICast (d, CTrunc, TInteger n, v, _)
      | ICast (d, CZext, TInteger n, v, _) ->
       (* Since we're representing everything as a sign-extended int,
        * we actually need to mask to do a trunc OR zext... *)
       let [@warning "-8"] Const raw_v = interp_raw_value v in
       let mask = -1 lsr (Sys.int_size - n) in
       let v' = raw_v land mask in
       let (regs, globs) = update d (Const v') in
       interp_int currfunc prevlabel (pc + 1) regs globs stack heap
    | ICast (d, _, _, v, _) ->
       (* but other casts are no-ops *)
       let (regs, globs) = update d (interp_raw_value v) in
       interp_int currfunc prevlabel (pc + 1) regs globs stack heap
    | IBinop (d, b, _, v1, v2) ->
       let (regs, globs) = interp_binop d b v1 v2 in
       interp_int currfunc prevlabel (pc + 1) regs globs stack heap
    | ICmp (d, c, _, v1, v2) ->
       let (regs, globs) = interp_cmp d c v1 v2 in
       interp_int currfunc prevlabel (pc + 1) regs globs stack heap
    | IBr l -> do_branch l
    | ICondBr (v, ifl, elsel) ->
       if interp_value v > 0 then do_branch ifl
       else do_branch elsel
    | IRet None -> (globs, heap, stack, 0)
    | IRet (Some (_, v)) ->
       (globs, heap, stack, interp_value v)
    | ICall (d, _, f, args) ->
       (match (f, args) with
        | (Global "malloc", [(_, v)]) ->
           let n = interp_value v in
           let (m, heapptr) = heap in
           let (regs, globs) = update d (Const heapptr) in
           interp_int currfunc prevlabel (pc + 1) regs globs stack
             (m, heapptr + n / 4)
        | _ ->
           let fnum = interp_value (Var f) in
           let func = get_func_by_num fnum in
           let newregs =
             (try
                List.fold_left2
                  (fun regs (_, name) (_, arg) ->
                    Varmap.add name (interp_raw_value arg) regs)
                  Varmap.empty
                  func.f_args
                  args
              with Invalid_argument _ ->
                raise (RuntimeError ("Wrong number of arguments", loc))
             )
           in
           let (_, sp) = stack in
           let (globs, heap, (newm, _), res) =
             interp_int func "" 0 newregs globs stack heap
           in
           let (regs, globs) = update d (Const res) in
           interp_int currfunc prevlabel (pc + 1) regs globs (newm, sp) heap
       )
    | IGetElementPtr (d, t, p, inds) ->
       let p = interp_value (Var p) in
       let v = do_getelementptr t p inds
       in
       let (regs, globs) = update d (Const v)
       in
       interp_int currfunc prevlabel (pc + 1) regs globs stack heap
    | IAlloca (d, t, n) ->
       let (m, stackptr) = stack in
       let stack = (m, stackptr + n * (sizeof ctx t)) in
       let (regs, globs) = update d (Const stackptr) in
       interp_int currfunc prevlabel (pc + 1) regs globs stack heap
    | ILoad (d, _, v) ->
       let addr = interp_value (Var v) in
       let (s, _) = stack in
       let (h, _) = heap in
       let vl =
         if addr >= Array.length s then
           h.(addr - (Array.length s))
         else
           s.(addr)
       in
       let (regs, globs) = update d (Const vl) in
       interp_int currfunc prevlabel (pc + 1) regs globs stack heap
    | IStore (_, v, ptr) ->
       let addr = interp_value (Var ptr) in
       let (s, _) = stack in
       let (h, _) = heap in
       (if addr >= Array.length s then
          h.(addr - (Array.length s)) <- (interp_value v)
        else
          s.(addr) <- interp_value v);
       interp_int currfunc prevlabel (pc + 1) regs globs stack heap
    | IPhi _ -> raise (RuntimeError ("Phi not at beginning of block", loc))

    in
    let func = get_func_by_name "main" in
    interp_int func "" 0 (Varmap.empty) globs (initstack ()) (initheap ())
      
    
    
              
                           
                                       
       
