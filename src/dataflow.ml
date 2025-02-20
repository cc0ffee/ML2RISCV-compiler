open LLVM.Ast

module Make (I: sig type t end) (F: Map.OrderedType) =
  struct
    module DFG = Dfg.Make(struct type t = I.t end)
    open DFG
    module NodeMap = Map.Make (struct type t = G.node
                                      let compare = G.cmp_node
                               end)

    module FSet = Set.Make (F)
                   
    type df = FSet.t NodeMap.t * FSet.t NodeMap.t
          
    let compute
          (cfg: G.t)
          (gen: I.t inst_ -> FSet.t)
          (kill: I.t inst_ -> FSet.t -> FSet.t)
          (fwd: bool)
          (must: bool)
        : df
      =
      let init =
        List.fold_left
          (fun m n -> NodeMap.add n FSet.empty m)
          NodeMap.empty
          (G.nodes cfg)
      in
      let nodes = G.nodes cfg in
      
      let rec iter in_m out_m =
        let (changed, in_m, out_m) =
          List.fold_left
            (fun (changed, in_m, out_m) n ->
              let ngen = gen (G.get_data n) in
              let f = if must then FSet.inter else FSet.union in
              let mapred f g l =
                match l with
                | [] -> FSet.empty
                | h::t -> List.fold_left
                            (fun r x -> f r (g x))
                            (g h)
                            t
              in
              let in' =
                if fwd then
                  mapred f (fun n -> NodeMap.find n out_m) (G.preds cfg n)
                else
                  FSet.union ngen (kill (G.get_data n) (NodeMap.find n out_m))
              in
              let out' =
                if fwd then
                  FSet.union ngen (kill (G.get_data n) (NodeMap.find n in_m))
                else
                  mapred f (fun n -> NodeMap.find n in_m) (G.succs cfg n)
              in
              let changed' =
                changed || not (FSet.equal in' (NodeMap.find n in_m))
                || not (FSet.equal out' (NodeMap.find n out_m))
              in
              (changed',
               NodeMap.add n in' in_m,
               NodeMap.add n out' out_m)
            )
            (false, in_m, out_m)
            nodes
        in
        if changed then iter in_m out_m
        else (in_m, out_m)
      in
      iter init init
  end
