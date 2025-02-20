(** CS443 - Control Flow Graphs **)
(** Stefan Muller - 2022 **)

module L = Llvm_ast
module VMap = Varmap

module G = Graph.Make
             (struct
               type data = string
               let to_string x = x end)

type t = G.t * (L.inst list VMap.t) * G.node (* graph, insts in each bb, entry *)
let succs = G.succs
let exists_edge = G.exists_edge


let bb_cfg (fname: string) (insts: L.inst list) : t =
  let bbs =
    List.fold_left
      (fun bbs i ->
        match (i, bbs) with
        | (L.ILabel s, _) -> [i]::bbs
        | (_, bb::bbs) -> (i::bb)::bbs
        | _ -> failwith "Function should start with label"
      )
      []
      insts
  in
  let bbmap =
    List.fold_left
      (fun bbmap ls ->
        let ls = List.rev ls in
        match ls with
        | (L.ILabel s)::_ -> VMap.add s ls bbmap
        | _ -> failwith "BB should start with label"
      )
      VMap.empty
      bbs
  in
  let label_nodes =
    VMap.mapi (fun s _ -> G.new_node s) bbmap
  in
  let inst_out_edges =
    function 
    | L.IBr l -> [VMap.find l label_nodes]
    | L.ICondBr (_, l1, l2) ->
       [VMap.find l1 label_nodes;
        VMap.find l2 label_nodes]
    | _ -> []
  in
  let out_edges il = List.concat (List.map inst_out_edges il) in
  let add_edges s n g =
    let insts = VMap.find s bbmap in
    let out_edges = List.map (fun n' -> (n, n')) (out_edges insts) in
    List.fold_left G.add_edge g out_edges
  in
  (VMap.fold add_edges label_nodes G.empty,
   bbmap,
   VMap.find (Config.entry_label_of_fun fname) label_nodes)
