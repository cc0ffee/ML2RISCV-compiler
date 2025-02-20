(** CS 443 Graph Library **)
(** (c) Stefan Muller, 2022 **)

module type NodeData =
  sig
    type data
    val to_string: data -> string
  end

module type GRAPH =
  sig
    type node
    type nodedata

    type t

    val cmp_node: node -> node -> int

    val new_node: nodedata -> node
    val get_data: node -> nodedata

    val empty : t
    val nodes: t -> node list
    val add_edge: t -> node * node -> t
    val add_node: t -> node -> t
    val rem_edge: t -> node * node -> t
    val rem_node: t -> node -> t
      
    val succs: t -> node -> node list
    val preds: t -> node -> node list
    val exists_edge: t -> node -> node -> bool

    val dom_tree_and_front: t -> node -> t * t
      
  end

module Make (A: NodeData) : GRAPH with type nodedata = A.data
  =
  struct

    type nodedata = A.data
    type node = int * nodedata

    let cmp_node (a, _) (b, _) = a - b

    module NodeMap = Map.Make
                       (struct
                         type t = node
                         let compare = cmp_node
                       end)
             
    (* Forward adjacency list, reverse adjacency list *)
    type t = node list NodeMap.t * node list NodeMap.t

    let ctr = ref 0
    let new_node d = ctr := !ctr + 1; (!ctr, d)
    let get_data (_, d) = d

    let in_nbrs ((a, _): node) (l: node list) =
      List.exists (fun (b, _) -> a = b) l

    let empty = (NodeMap.empty, NodeMap.empty)

    let nodes ((f, r): t) =
      List.map fst (NodeMap.bindings
                      (NodeMap.merge
                         (fun _ a b ->
                           match (a, b) with
                           | (None, None) -> None
                           | _ -> Some ())
                         f r))

    let succs ((f, _): t) (src: node) =
      try
        NodeMap.find src f
      with Not_found -> []

    let preds ((_, r): t) (src: node) =
      try
        NodeMap.find src r
      with Not_found -> []

    let from_adj_map f =
      (f,
       NodeMap.fold
         (fun src ds r ->
           List.fold_left
             (fun r d ->
               NodeMap.add d (src::(try NodeMap.find d r with Not_found -> []))
                 r)
             r
             ds
         )
         NodeMap.empty
         f)

    let add_edge ((f, r): t) ((src, dest) : node * node) =
      let ssrc = succs (f, r) src in
      let pdest = preds (f, r) dest in
      let ssrc' =
        if in_nbrs dest ssrc then ssrc else dest::ssrc
      in
      let pdest' =
        if in_nbrs src pdest then pdest else src::pdest
      in
      (NodeMap.add src ssrc' f, NodeMap.add dest pdest' r)

    let add_node ((f, r): t) (n: node) =
      let f' = if NodeMap.mem n f then f else NodeMap.add n [] f in
      let r' = if NodeMap.mem n r then r else NodeMap.add n [] r in
      (f', r')
      
    let rem_edge ((f, r): t) ((src, dest) : node * node) =
      let ssrc = succs (f, r) src in
      let pdest = preds (f, r) dest in
      let ssrc' = List.filter (fun (b, _) -> b <> fst dest) ssrc in
      let pdest' = List.filter (fun (b, _) -> b <> fst src) pdest in
      (NodeMap.add src ssrc' f, NodeMap.add dest pdest' r)

    let rem_node ((f, r): t) (n: node) =
      (NodeMap.remove n
         (NodeMap.map (List.filter (fun n' -> n <> n')) f),
       NodeMap.remove n
         (NodeMap.map (List.filter (fun n' -> n <> n')) r))
      
    let exists_edge (g: t) (src: node) (dest: node) =
      in_nbrs dest (succs g src)

    (* Find the dominator tree of a graph.
     * This algorithm is simple but quite inefficient. *)
    let domtree_and_doms ((cfg, ps): t) (s0: node)
        : t * (node list NodeMap.t)  =
      let nodes = nodes (cfg, ps) in
      (* Initialize dominators of s0 to {s0}, all others to all nodes *)
      let init_doms =
        NodeMap.mapi
          (fun (n, _) _ -> if n = fst s0 then [s0] else nodes)
          cfg
      in
      let intersect l1 l2 =
        List.filter (fun x -> List.mem x l2) l1
      in
      (* Iteratively solve simultaneous equations for dominators *)
      let compute_doms (doms: node list NodeMap.t) n =
        n::(
          match List.map (fun p -> NodeMap.find p doms) (preds (cfg, ps) n)
          with
            [] -> []
          | hl::tl -> List.fold_left intersect hl tl
        )
      in
      let rec iter_doms doms =
        let (new_doms, changed) =
          List.fold_left
            (fun (new_doms, changed) n ->
              let new_dom = compute_doms doms n in
              let old_dom =
                try
                  NodeMap.find n doms
                with Not_found -> nodes
              in
              (NodeMap.add n new_dom new_doms,
               changed || List.length new_dom < List.length old_dom)
            )
            (doms, false)
            nodes
        in
        if changed then iter_doms new_doms
        else new_doms
      in
      let doms = iter_doms init_doms in
      let is_dom d n = List.mem d (NodeMap.find n doms) in
      (* Compute immediate dominators in an especially naive, inefficient
       * way by filtering out the dominator for each node from doms that isn't
       * a dominator of the others. *)
      let idoms =
        NodeMap.mapi
          (fun n doms ->
            (*
            Printf.printf "%s has as dominators: %s\n"
              (A.to_string (snd n))
              (String.concat " " (List.map (fun (_, d) -> A.to_string d) doms));
             *)
            let ids =
              List.filter (fun d -> fst n <> fst d &&
                                      List.for_all
                                        (fun d' -> fst d = fst d'
                                                   || fst d' = fst n
                                                   || not (is_dom d d'))
                                        doms)
                doms
            in
            (*
            Printf.printf "%s has as imm. dominators: %s\n"
              (A.to_string (snd n))
              (String.concat " " (List.map (fun (_, d) -> A.to_string d) ids));
             *)
            ids
          )
          doms
      in
      (NodeMap.fold
         (fun n ids idt ->
           match ids with
           | [] -> idt
           | id::_ -> add_edge idt (id, n))
         idoms
         empty
      , doms)

    let dom_tree_and_front (cfg: t) (s0: node) : t * t =
      let (dt, doms) = domtree_and_doms cfg s0 in
      let rec comp_df df n =
        let dflocal =
          List.filter
            (fun y -> not (List.mem y (succs dt n)))
            (succs cfg n)
        in
        let (df, my_df) =
          List.fold_left
            (fun (df, ss) c ->
              let df' = comp_df df c in
              let dfc = NodeMap.find c df' in
              (df',
               ss
               @ (List.filter
                    (fun w -> not (List.mem n (NodeMap.find w doms)))
                    dfc)
              )
            )
            (df, dflocal)
            (succs dt n)
        in
        NodeMap.add n my_df df
      in
      (dt, from_adj_map (comp_df (NodeMap.empty) s0))
            
      
  end
