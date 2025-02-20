module L = LLVM.Ast
 
module Make (I: sig type t end) =
  struct
    module G = Graph.Make(struct type data = I.t L.inst_
                                 let to_string _ = ""
                          end)
       
type t = G.t * G.node (* graph, entry *)
let succs = G.succs
let exists_edge = G.exists_edge

module VMap = Varmap

(* Returns a pair of the CFG and a list of nodes corresponding to
 * the list of instructions *)
let cfg_of_insts (fname: string) insts : t * G.node list =
  let entrylabel = Config.entry_label_of_fun fname in
  let inodes = List.map (fun i -> G.new_node i) insts in
  let label_nodes =
    List.fold_left
      (fun map n ->
        match G.get_data n with
        | L.ILabel l -> VMap.add l n map
        | _ -> map)
      VMap.empty
      inodes
  in
  let entry =
    try VMap.find entrylabel label_nodes
    with Not_found -> failwith "entry BB should be labeled"
  in
  let rec add_edges g nodes =
    match nodes with
    | [] -> g
    | [x] -> g
    | n1::n2::t ->
       let new_edges =
         match G.get_data n1 with
          | L.IBr l -> [(n1, VMap.find l label_nodes)]
          | L.ICondBr (_, l1, l2) ->
             [(n1, VMap.find l1 label_nodes);
              (n1, VMap.find l2 label_nodes)]
          | L.IRet _ -> []
                 (*
          | L.ICall (_, _, f, _) ->
             let (pos_entries, pos_exits) =
               let all_func_nodes () =
                 (VMap.fold
                    (fun _ (n, x) (ns, xs) -> (n::ns, x::xs))
                    func_nodes
                    ([], [])
                 )
               in
               (match f with
                | L.Global f ->
                   (try let (n, x) = VMap.find f func_nodes in ([n], [x])
                    with Not_found ->
                          (* Could be any function, add all the edges *)
                          all_func_nodes ())
                | L.Local _ -> all_func_nodes ())
             in
             (List.map (fun ent -> (n1, ent)) pos_entries)
             @ (List.map (fun ex -> (ex, n2)) pos_exits)
                  *)
          | _ -> [(n1, n2)]
       in
       add_edges (List.fold_left G.add_edge g new_edges) (n2::t)
  in
  ((add_edges (G.empty) (entry::inodes), entry),
   inodes)
  end
