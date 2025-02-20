(* CS 443 Fall 2022 *)
(* Project 6 *)
(* Your name(s) here *)

open Allocutil
module R = Riscv.Ast
open LLVM.Ast

exception Unimplemented

module IG = Graph.Make
              (struct
                type data = varorreg
                let to_string =
                  function
                    Variable v -> string_of_var v
                  | Register r -> Riscv.Print.string_of_reg r
              end)

module FSet = Live.FSet

module Dfg = Live.DFG

type alloc_res = InReg of R.reg
               | OnStack of int (* stack position *)
               | InMem of R.symbol (* Risc-V global symbol *)

let vr_to_string =
    function Register r -> Riscv.Print.string_of_reg r
           | Variable v -> string_of_var v

(* Take an existing allocation and a node.
 * If the node is already allocated a register r, return Some r.
 * If the node may be allocated register r, return Some r.
 * Otherwise, return None.
*)
let get_reg (igraph: IG.t) (colors: alloc_res VRMap.t) (n: IG.node) :
      R.reg option =
  let node_color n =
    try
      (match VRMap.find (IG.get_data n) colors with
       | InReg r -> [r]
       | _ -> [])
    with Not_found -> []
  in
  let nbr_colors n =
    List.concat (List.map node_color (IG.succs igraph n))
  in
  match node_color n with
  | r::_ -> (* It already has a color! *) Some r
  | _ ->
     let used = nbr_colors n in
     try
       Some (List.find (fun r -> not (List.mem r used)) R.general_purpose)
     with Not_found -> None

(* Build interference graph from liveness analysis result and a dataflow graph.
 *)
let build_intf_graph (live_in, live_out) ((cfg, n): Dfg.t) : IG.t =
  let vr_nodes : IG.node VRMap.t ref = ref VRMap.empty in
  let node_of_vr vr =
    try
      VRMap.find vr (!vr_nodes)
    with Not_found ->
      let node = IG.new_node vr in
      vr_nodes := VRMap.add vr node (!vr_nodes);
      node
  in
  (* Build interference graph *)
  let intf_edges i live_out =
    let edges =
      match i with
      | ISet (d, _, Var v)
        | ICast (d, _, _, Var v, _) ->
         FSet.fold
           (fun v edges -> (d, v)::edges)
           (FSet.diff live_out (FSet.singleton v))
           []
      | ICall (d, _, f, args) ->
         List.concat
           (List.map
              (fun d ->
                FSet.fold
                  (fun v edges -> (d, v)::edges)
                  live_out
                  []
              )
              ((LLVM.Utils.def_inst i)
               @ (List.map (fun r -> Register r) R.caller_saved)
           ))
      | _ ->
         List.concat
           (List.map
              (fun d ->
                FSet.fold
                  (fun v edges -> (d, v)::edges)
                  live_out
                  []
              )
              (LLVM.Utils.def_inst i))
    in
    List.map (fun (s, d) -> (node_of_vr s, node_of_vr d)) edges
  in
  let add_undir_edge g (a, b) =
    match (IG.get_data a, IG.get_data b) with
    | (Variable (Global _), _)
      | (_, Variable (Global _)) ->
       (* Don't add global vars to interference graph *)
       g
    | (avr, bvr) ->
       let g = IG.add_node (IG.add_node g a) b in
       IG.add_edge (IG.add_edge g (a, b)) (b,a)
  in
  let add_intf_edges (g: IG.t) n =
    List.fold_left
      add_undir_edge
      g
      (intf_edges (Dfg.G.get_data n) (Live.NodeMap.find n live_out))
  in
  List.fold_left
    add_intf_edges
    IG.empty
    (Dfg.G.nodes cfg)


(* Perform register allocation on a dataflow graph for a program,
 * using a greedy algorithm (not quite linear scan, as it doesn't necessarily
 * go through vars in order of definition.
 * glob_alloc contains global variables, mapped to InMem of their symbols.
 * Return (list of spilled variables, allocation map)
 * Returned map should include glob_alloc. *)
let greedy (glob_alloc: alloc_res VRMap.t) ((cfg, n): Dfg.t) :
      var list * alloc_res VRMap.t =
  let li_info = liveness cfg in
  let igraph = build_intf_graph li_info (cfg, n) in

  let nodes = IG.nodes igraph in
  let rec color (spills, colors) n =
    match get_reg igraph colors n with
    | Some r -> (spills, VRMap.add (IG.get_data n) (InReg r) colors)
    | None ->
       let spills' =
         match IG.get_data n with
         | Register _ -> failwith "can't spill a register!"
         | Variable v -> v::spills
       in
       (spills',
        VRMap.add (IG.get_data n) (OnStack (List.length spills)) colors)
  in
  let init_colors =
      (* Pre-color the registers *)
      List.fold_left
        (fun colors r ->
          VRMap.add (Register r) (InReg r) colors)
        glob_alloc
        R.all_regs
    in
  let (spills, alloc) = List.fold_left color ([], init_colors) nodes in
  (List.rev spills, alloc)

      
(* Perform register allocation on a dataflow graph for a program,
 * using a graph-coloring-based algorithm as described in class
 * (with or without coalescing).
 * glob_alloc contains global variables, mapped to InMem of their symbols.
 * Return (list of spilled variables, allocation map)
 * Returned map should include glob_alloc. *)
 let grcolor (glob_alloc: alloc_res VRMap.t) ((cfg, n): Dfg.t) :
  var list * alloc_res VRMap.t =
  (* Do liveness analysis *)
  let li_info = liveness cfg in
  let igraph = build_intf_graph li_info (cfg, n) in

  let rec color igraph =
  (* init stack & list of spilled nodes *)
  let stack = Stack.create () in
  let spills = ref [] in
  let rec simplify graph =
  let nodes = IG.nodes graph in
  (* match first node who 1. stores a variable and 2. has less neighbors than available registers *)
  match List.find_opt (fun node -> 
                          (match (IG.get_data node) with
                            | Variable _ -> true
                            | Register _ -> false)
                            && List.length (IG.succs graph node) < List.length R.general_purpose) nodes with 
    (* if node matches condition, remove from graph & add to stack (fulfilling step 1) *)
    | Some node ->
      Stack.push node stack;
      simplify (IG.rem_node graph node) (* Recurse on graph that doesn't contain removed node *)

    (* step 3: spill node, recurse simplify function till entire graph has Registers *)
    | None -> 
        match nodes with
        (* done simplifying when the graph is "empty"/only contains register values *)
        | [] -> graph
        | _ -> 
          (* identify node with highest degree to spill *)
          let node_to_spill = 
            List.fold_left
              (fun max_node n -> 
                if List.length (IG.succs graph n) > List.length (IG.succs graph max_node) then
                  n else max_node)
              (List.hd nodes)
              nodes
          in
          (* add spills node to spills list and remove from graph  *)
          spills := (IG.get_data node_to_spill) :: !spills;
          simplify (IG.rem_node graph node_to_spill)
    in
    let simplified_graph = simplify igraph in
    let rec color (spills, colors) n =
    match get_reg igraph colors n with
    | Some r -> (spills, VRMap.add (IG.get_data n) (InReg r) colors)
    | None ->
        let spills' =
          match IG.get_data n with
          | Register _ -> failwith "can't spill a register!"
          | Variable v -> v::spills
        in
        (spills',
        VRMap.add (IG.get_data n) (OnStack (List.length spills)) colors)
    in
    let init_colors =
      (* Pre-color the registers *)
      List.fold_left
        (fun colors r ->
          VRMap.add (Register r) (InReg r) colors)
        glob_alloc
        R.all_regs

    in
    let nodes = IG.nodes igraph in
    let (spills, alloc) = List.fold_left color ([], init_colors) nodes in
    (List.rev spills, alloc)
            
    in
    color igraph

let rec codegen_body
          (ctx: typdefs)
          (insts: ainst list)
          (insts_so_far: R.label R.inst list)
          (frame_offset: int)
          (alloc: alloc_res VRMap.t) =
  let is_immed n = n > -2049 && n < 2048 in
  let is_noop_cast =
    function CBitcast | CInttoptr | CPtrtoint -> true
             | CTrunc | CZext | CSext -> false
  in
  (* Return the loading code and register the data is in.
   * If is_fst, use X5 as a temp, otherwise X6 *)
  let get_reg_r is_fst v : R.label R.inst list * R.reg =
    let temp_reg = if is_fst then R.X5 else R.X6 in
    try
      match VRMap.find v alloc with
      | InReg r -> ([], r)
      | OnStack i ->
         ([R.I (R.Lw, temp_reg, R.fp, 0 - i * Config.word_size - frame_offset)],
          temp_reg)
      | InMem s ->
         ([R.LoadLabel (temp_reg, s)],
          temp_reg)
    with Not_found -> failwith ("Not found: " ^ (vr_to_string v))
  in
  let get_reg_r_val is_fst v : R.label R.inst list * R.reg =
    let temp_reg = if is_fst then R.X5 else R.X6 in
    match v with
    | Const n ->
       if is_immed n then
         ([R.I (R.Addi, temp_reg, R.zero, n)], temp_reg)
       else
         let n = Int32.of_int n in
         let upper = Int32.shift_right_logical n 12 in
         let lower = Int32.logxor n (Int32.shift_left upper 12) in
         let lower = Int32.to_int lower in
         let (lower, upper) =
           if lower > 2047
           then (lower - 4096, (Int32.to_int upper) + 1)
           else (lower, Int32.to_int upper)
         in
         ([R.Lui (temp_reg, upper);
           R.I (R.Addi, temp_reg, temp_reg, lower)],
          temp_reg)
    | Var v ->
       (try
          (match VRMap.find v alloc with
           | InReg r -> ([], r)
           | OnStack i ->
              ([R.I (R.Lw, temp_reg, R.fp, 0 - i * Config.word_size - frame_offset)],
               temp_reg)
           | InMem s ->
              ([R.LoadLabel (temp_reg, s)],
               temp_reg)
          )
        with Not_found -> failwith ("Not found: " ^ (vr_to_string v))
       )
  in
  (* Return the  storing code, and register to write to.
   * Use X7 as a temp if necessary. *)
  let get_reg_w v : R.label R.inst list * R.reg =
    let temp_reg = R.X7 in
    try
      (match VRMap.find v alloc with
       | InReg r -> ([], r)
       | OnStack i ->
          ([R.Sw (R.fp, temp_reg, 0 - i * Config.word_size - frame_offset)],
           temp_reg)
       | InMem s ->
          ([R.StoreLabel (temp_reg, s)],
           temp_reg)
      )
    with Not_found -> failwith ("Not found: " ^ (vr_to_string v))
  in
  let iop_of_binop =
    function BAdd -> R.Addi | BXor -> R.Xori | BOr -> R.Ori | BAnd -> R.Andi
             | _ -> raise (Invalid_argument "iop_of_binop")
  in
  let rop_of_binop =
    function BAdd -> R.Add | BSub -> R.Sub | BMul -> R.Mul | BDiv -> R.Div
             | BAnd -> R.And | BOr -> R.Or | BXor -> R.Xor
  in
  let (riscops, rest) =
    match insts with
    | [] -> ([], [])

    | (ICast (d, CInttoptr, _, Const n, _))::t
      | (ICast (d, CPtrtoint, _, Const n, _))::t
      | (ICast (d, CBitcast, _, Const n, _))::t when is_immed n ->
       let (write_d, rd) = get_reg_w d in
       ([R.I (R.Addi, rd, R.zero, n)] @ write_d,
        t)
    | (ICast (d, CInttoptr, _, v, _))::t
      | (ICast (d, CPtrtoint, _, v, _))::t
      | (ICast (d, CBitcast, _, v, _))::t ->
       let (read_v, rs) = get_reg_r_val true v in
       let (write_d, rd) = get_reg_w d in
       (* Opt: don't emit a mv r <- r *)
       if rd = rs then ([], t)
       else
         (read_v @ [R.I (R.Addi, rd, rs, 0)] @ write_d,
          t)
    | ICast (d, CTrunc, _, Const n, TInteger n2)::t
      | ICast (d, CZext, TInteger n2, Const n, _)::t
         when is_immed n ->
       let (write_d, rd) = get_reg_w d in
       ([R.I (R.Addi, rd, R.zero, n);
         R.I (R.Slli, rd, rd, 32 - n2);
         R.I (R.Srli, rd, rd, 32 - n2)]
        @ write_d,
        t)
    | ICast (d, CTrunc, _, v, TInteger n2)::t
      | ICast (d, CZext, TInteger n2, v, _)::t ->
       let (read_v, rs) = get_reg_r_val true v in
       let (write_d, rd) = get_reg_w d in
       (read_v @
          [R.R (R.Add, rd, R.zero, rs);
           R.I (R.Slli, rd, rd, 32 - n2);
           R.I (R.Srli, rd, rd, 32 - n2)]
          @ write_d,
        t)

    | (ICmp (d, c, _, v1, v2))::t ->
       let (read1, rs1) = get_reg_r_val true v1 in
       let (read2, rs2) = get_reg_r_val false v2 in
       let (write_d, rd) = get_reg_w d in
       let rec get_code c =
         match c with
         (* Some bit hacks here *)
         | CEq -> [R.R (R.Sub, rd, rs1, rs2);
                   R.I (R.Sltiu, rd, rd, 1)]
         | CSLt -> [R.R (R.Slt, rd, rs1, rs2)]
         | CSGt -> [R.R (R.Slt, rd, rs2, rs1)]
         | CNe -> (get_code CEq) @ [R.I (R.Xori, rd, rd, 1)]
         | CSLe -> (get_code CSGt) @ [R.I (R.Xori, rd, rd, 1)]
         | CSGe -> (get_code CSLt) @ [R.I (R.Xori, rd, rd, 1)]
       in
       (read1 @ read2 @ get_code c @ write_d,
        t)
    | (IRet _)::t ->
       (* We've already moved the return value into a0 in to_a_func *)
       ([], t)
    | (ICall (d, _, f, args))::t ->
       let rec place_args args arg_regs =
         match (args, arg_regs) with
         | ([], _) -> []
         | ((_, a1)::args, r1::arg_regs) ->
            (* We've already moved these arguments to registers in to_a_func *)
            place_args args arg_regs
         | (args, []) ->
            (* Put the rest of the arguments on the stack. *)
            (R.I (R.Addi, R.sp, R.sp, 0 - (Config.word_size * (List.length args))))
            ::(fst
                 (List.fold_left
                    (fun (is, i) (_, a) ->
                      let (read, rs) = get_reg_r_val true a in
                      (read @ [R.Sw (R.sp, rs, Config.word_size * i)] @ is, i + 1))
                    ([], 0)
                    args)
              )
       in
       let (readf, rf) = get_reg_r true f in
       let (writed, rd) = get_reg_w d in
       ((place_args args R.args)
        @
          readf
        @
          [R.I (R.Jalr, R.ra, rf, 0)]
        @
          (if List.length args > List.length R.args then
             [
               (* Pop the args off the stack *)
               R.I (R.Addi, R.sp, R.sp,
                    Config.word_size *
                      ((List.length args) - (List.length R.args)));
             ]
           else []
          )
        @
           (* Write the return value into rd *)
           [R.I (R.Addi, rd, R.retval, 0)]
        @ writed,
        t)
    | (IGetElementPtr (d, TStruct ts, p, [(_, v1); (_, v2)]))
      ::rest ->
       let (readp, rp) = get_reg_r true p in
       let (read1, rs1) = get_reg_r_val true v1 in
       let (read2, rs2) = get_reg_r_val false v2 in
       let (writed, rd) = get_reg_w d in
       (
         (* If we're really unlucky, rp and rd might both be X7, so we have
          * to be careful. *)
         readp
         @ (* We have to use rp before we load v1 *)
           [R.I (R.Addi, rd, rp, 0)]
         (* OK, now we're done with rp. *)
         @ read1
         @ [(* We haven't loaded v2 yet, so we can use x6 *)
             R.I (R.Addi, R.X6, R.zero, Config.word_size * (sizeof ctx (TStruct ts)));
             R.R (R.Mul, rs1, rs1, R.X6);
             R.R (R.Add, rd, rd, rs1)]
         @ read2
         @ [(* We're done with v1, so we can use x5 *)
             R.I (R.Addi, R.X5, R.zero, Config.word_size);
             R.R (R.Mul, rs2, rs2, R.X5);
             R.R (R.Add, rd, rd, rs2)]
         @ writed
       , rest)
    | (IGetElementPtr (d, t, p, [(_, v1)]))::rest ->
       let (readp, rp) = get_reg_r true p in
       let (read1, rs1) = get_reg_r_val true v1 in
       let (writed, rd) = get_reg_w d in
       (
         (* If we're really unlucky, rp and rd might both be X7, so we have
          * to be careful. *)
         readp
         @ (* We have to use rp before we load v1 *)
           [R.I (R.Addi, rd, rp, 0)]
         (* OK, now we're done with rp. *)
         @ read1
         @ [(* There's no v2, so we can use x6 *)
             R.I (R.Addi, R.X6, R.zero, Config.word_size * (sizeof ctx t));
             R.R (R.Mul, rs1, rs1, R.X6);
             R.R (R.Add, rd, rd, rs1)]
         @ writed
         , rest)
    | (IPhi _)::t ->
       failwith "shouldn't have phis"

    (* TODO: Add your cases here *)

    (* ILabel case *)
    (* lbl: *)
    | ILabel lbl :: t -> ([R.Label lbl], t) 

    (* IBr case *)
    (* jal x0, lbl *)
    (* x0 so the return address is not necessary *)
    | IBr lbl :: t -> ([R.Jal (R.zero, lbl)], t)

    (* IBinop case *)
    (* add rd, rs1, rs2 *)
    | IBinop (dest, op, typ, v1, v2) :: t ->
      let (readdest1, rs1) = get_reg_r_val true v1 in
      let (readdest2, rs2) = get_reg_r_val false v2 in
      let (writedest, rd) = get_reg_w dest in
      (readdest1 @ readdest2 @ [R.R (rop_of_binop op, rd, rs1, rs2)] @ writedest, t)

    (* ISet case *)
    (* add x5, x6, x0 *)
    (* Adds 0 so it "sets" it as the same value to the register *)
    | ISet (dest, typ, value) :: t -> 
        let (writedest, rd) = get_reg_w dest in
        let (read1, rs) = get_reg_r_val true value in
        let set_insts = [R.R (R.Add, rd,  rs, R.zero)] in
        (read1 @ set_insts @ writedest, t)

    (* CondBr case *)
    (* beq x2, x0, lbl_false *)
    (* jal x0 lbl_true *)
    | (ICondBr (value, lbl1, lbl2)) :: t -> 
      let (read1, rs1) = get_reg_r_val true value in
      (read1 @ 
      [R.B (R.Beq, rs1, R.zero, lbl2)] @ 
      [R.Jal (R.zero, lbl1)], t)

    (* Alloca case *)
    (* addi sp, sp, -16 *)
    (* add x4, x0, sp *)
    | (IAlloca (dest, typ, size)) :: t -> 
      let (writedest, rd) = get_reg_w dest in
      let mem_space = (size * Config.word_size * (sizeof ctx typ)) in
      ([R.I (R.Addi, R.sp, R.sp, 0 - mem_space)] @ 
      [R.R (R.Add, rd, R.zero, R.sp)] @ 
      writedest, t)

    (* Load case *)
    (* lw x3, 0(x1) *)
    | (ILoad (dest, _, ptr)) :: t ->
        let (read1, rs1) = get_reg_r_val true (Var ptr) in
        let (write1, rd) = get_reg_w dest in
        let load_insts = [R.I (R.Lw, rd, rs1, 0)] in
        (read1 @ load_insts @ write1, t)

    (* Store case *)
    (* sw x2, 0(x4) *)
    | (IStore (_, v1, dest)) :: t ->
        let (read1, rs1) = get_reg_r_val true v1 in
        let (write1, rd) = get_reg_w dest in
        let load_insts = [R.Sw (rd, rs1, 0)] in
        (read1 @ load_insts @ write1, t)

    | _ -> raise Unimplemented
  in
  match rest with
  | [] -> insts_so_far @ riscops
  | _ ->
     codegen_body
       ctx
       rest
       (insts_so_far @ riscops)
       frame_offset
       alloc

(* Select which register allocation strategy we want to use *)
let regalloc_strategy = (* greedy *) grcolor
let print_alloc = ref false

let regalloc (glob_alloc: alloc_res VRMap.t) (cfg: Dfg.t) =
  let s_of_storage =
    function InReg r -> Printf.sprintf "Reg \t%s" (Riscv.Print.string_of_reg r)
           | OnStack n -> Printf.sprintf "Stk \t%d" n
           | InMem s -> Printf.sprintf "Heap\t%s" s
  in
  let (spilled, alloc) = regalloc_strategy glob_alloc cfg in
  if (!print_alloc) then
    (Printf.printf "\n%-30s\tLoc\tInfo\n" "Variable";
     Printf.printf "------------------------------------------------------------------------------\n";
     ignore
       (VRMap.mapi
          (fun vr s ->
            match vr with
            | Variable v -> Printf.printf "%-30s\t%s\n"
                              (LLVM.Print.string_of_var v)
                              (s_of_storage s)
            | Register _ -> ()
          )
          alloc);
     (spilled, alloc))
  else
    (spilled, alloc)

let codegen_func ctx glob_alloc (f: func) : R.label R.inst list =
  let f = to_a_func f in
  let insts = Array.to_list f.f_body in
  let rec alloc_args alloc args arg_regs =
    match (args, arg_regs) with
    | ([], _) -> alloc
    | ((_, v1)::args, r1::arg_regs) ->
       (* Argument v1 is in r1 *)
       (* We've already copied these in to_a_func *)
         (alloc_args alloc args arg_regs)
    | (args, []) ->
       (* Argument |arg_regs| + i is at fp - i - 1 *)
       fst
         (List.fold_left
            (fun (alloc, i) (_, v) ->
              (VRMap.add (Variable (Local v)) (OnStack (-1 - i)) alloc,
               i + 1)
            )
            (alloc, 0)
            args
         )
  in
  let alloc = alloc_args glob_alloc f.f_args R.args in
  let (spilled, alloc) =
    regalloc alloc (fst (Dfg.cfg_of_insts f.f_name insts))
  in

  let ret_code =
    [(* Pop most of stack frame *)
      R.I (R.Addi, R.sp, R.fp, 0 - Config.word_size);
      (* Restore fp *)
      R.I (R.Lw, R.fp, R.sp, Config.word_size);
      (* Restore return address *)
      R.I (R.Lw, R.ra, R.sp, 0);
      (* Pop rest of stack frame *)
      R.I (R.Addi, R.sp, R.sp, Config.word_size * 2);
      (* Jump *)
      R.I (R.Jalr, R.zero, R.ra, 0)]
  in
  let code = codegen_body ctx insts [] (Config.word_size * 2) alloc
  in
  [R.Label ("__" ^ f.f_name);
   (* Save old frame pointer *)
   R.I (R.Addi, R.sp, R.sp, 0 - Config.word_size * 2);
   R.Sw (R.sp, R.fp, Config.word_size);
   (* Save old return address *)
   R.Sw (R.sp, R.ra, 0);
   (* Set frame pointer *)
   R.I (R.Addi, R.fp, R.sp, Config.word_size);
   (* Make stack space for spilled locals *)
   R.I (R.Addi, R.sp, R.sp, 0 - Config.word_size * (List.length spilled));
  ]
  @
    code
  @
    ret_code

let heapoffset = Config.word_size
let heapsize_val = Config.word_size * 1024 * 1024
let heapsize = "heapsize"
let heapend = "heapend"
let heapptr = "heapptr"
let heapstart = "heapstart"
  
let lib =
  let t0 = R.X5 in
  let t1 = R.X6 in
  let t2 = R.X7 in
  let a0 = List.hd R.args in
  R.
  [Label "__malloc";
   LoadLabel (t0, heapptr);       (* t0 <- old alloc ptr *)
   LoadLabel (t2, heapend);       (* t2 <- heap limit *)
   R (Add, t1, t0, a0);           (* t1 <- new alloc ptr *)
   B (Blt, t2, t1, "__eom");      (* limit check *)
   StoreLabel (t1, heapptr);      (* store new alloc ptr *)
   I (Addi, a0, t0, 0);           (* a0 <- old alloc ptr *)
   I (Jalr, zero, ra, 0);         (* Return *)
   Label "__eom";
   R (Xor, a0, a0, a0);           (* a0 <- 0 *)
   I (Jalr, zero, ra, 0);         (* Return *)
  ]

let lib_funcs =
  ["__malloc"]

let lib_globals =
   [("malloc", 0);
    (heapptr, 0);
    (heapsize, heapsize_val);
    (heapend, 0);
    (heapstart, 0)]
  
let codegen_prog ((p, ctx): prog * typdefs) :
         (R.label R.inst list) * (R.symbol list) * ((R.symbol * int) list) =
  let t0 = R.X5 in
  let t1 = R.X6 in
  let t2 = R.X7 in
  let (_, glob_alloc, sgis, funcs, globs) =
    List.fold_left
      (fun (i, a, is, fs, gs) f ->
        (i + 1,
         VRMap.add (Variable (Global f.f_name))
           (InMem f.f_name)
           a,
         [R.LoadAddress (t0, "__" ^ f.f_name);
          R.StoreLabel (t0, f.f_name)] @ is,
         ("__" ^ f.f_name)::fs,
         (f.f_name, 0)::gs
        )
      )
      (0, VRMap.singleton (Variable (Global "malloc")) (InMem "malloc"),
       [R.LoadAddress (t0, "__malloc");
        R.StoreLabel (t0, "malloc")],
       [],
       [])
      p
  in
  (
    sgis
    @
      [R.LoadAddress (t0, heapstart);
       R.StoreLabel (t0, heapptr);
       R.LoadLabel (t1, heapsize);
       R.R (R.Add, t2, t0, t1);
       R.StoreLabel (t2, heapend);
       R.Jal (R.ra, "__main");
       R.Jal (R.ra, "__halt")]
    @
      List.concat (List.map (codegen_func ctx glob_alloc) p)
    @
      lib
    @
      [R.Label "__halt"]
  ,
    funcs @ lib_funcs,
  globs @ lib_globals
  )
