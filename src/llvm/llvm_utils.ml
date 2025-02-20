(** CS443 - Various LLVM Utilities **)
(** Stefan Muller - 2022 **)

open Llvm_ast

let def_inst =
  function
  | ISet (d, _, _)
    | IBinop (d, _, _, _, _)
    | ICmp (d, _, _, _, _)
    | ICast (d, _, _, _, _)
    | ICall (d, _, _, _)
    | IGetElementPtr (d, _, _, _)
    | IAlloca (d, _, _)
    | ILoad (d, _, _)
    | IPhi (d, _, _) -> [d]
  | _ -> []

let use_val =
  function Var v -> [v]
         | _ -> []

let use_inst =
  function
  | ISet (_, _, v)
    | ICast (_, _, _, v, _)
    | ICondBr (v, _, _)
    | IRet (Some (_, v)) -> use_val v
  | IBinop (_, _, _, v1, v2)
    | ICmp (_, _, _, v1, v2) -> (use_val v1) @ (use_val v2)
  | ICall (_, _, v, args)
    | IGetElementPtr (_, _, v, args) ->
     v::(List.concat (List.map (fun (t, v) -> use_val v) args))
  | ILoad (_, _, v) -> [v]
  | IStore (_, vl, vr) -> vr::(use_val vl)
  | IPhi (_, _, preds) ->
     List.concat (List.map (fun (_, v) -> use_val v) preds)
  | ILabel _ | IBr _ | IRet None | IAlloca _ -> []
