(* Definitions and utilities for register allocation *)
(* CS 443 - Stefan Muller *)

open LLVM.Ast
open Dataflow

module R = Riscv.Ast

type varorreg = Variable of var
              | Register of R.reg

let compare_vr a b =
  match (a, b) with
  | (Variable a, Variable b) -> compare_var a b
  | (Register a, Register b) -> (R.reg_no a) - (R.reg_no b)
  | (Variable _, Register _) -> -1
  | (Register _, Variable _) -> 1

type avalue = varorreg value_
type ainst = varorreg inst_
type afunc = varorreg func_
type aprog = afunc list

let to_a_value (v: value) : avalue =
  match v with
  | Const n -> Const n
  | Var v -> Var (Variable v)

let to_a_var v = Variable v

let to_a_inst fname i =
  match i with
  | ILabel l -> [ILabel l]
  | ISet (d, t, v) -> [ISet (to_a_var d, t, to_a_value v)]
  | IBinop (d, b, t, v1, v2) ->
     [IBinop (to_a_var d, b, t, to_a_value v1, to_a_value v2)]
  | ICmp (d, c, t, v1, v2) ->
     [ICmp (to_a_var d, c, t, to_a_value v1, to_a_value v2)]
  | ICast (d, ct, t1, v, t2) ->
     [ICast (to_a_var d, ct, t1, to_a_value v, t2)]
  | IBr l -> [IBr l]
  | ICondBr (v, l1, l2) ->
     [ICondBr (to_a_value v, l1, l2)]
  | IRet (Some (t, v)) ->
     [ ISet (Register R.retval, t, to_a_value v);
       IBr (Config.exit_label_of_fun fname)]
  | IRet None -> [IBr (Config.exit_label_of_fun fname)]
  | ICall (d, t, v, args) ->
     let rec copy_args args arg_regs =
       match (args, arg_regs) with
       | ([], _) -> []
       | ((t, v1)::args, r1::arg_regs) ->
          (ISet (Register r1, t, v1))
          ::(copy_args args arg_regs)
       | (_, []) ->
       (* Args on the stack will be moved later *)
          []
     in
     let args = List.map (fun (t, v) -> (t, to_a_value v)) args
     in 
     (copy_args args R.args)
     @
       [ICall (to_a_var d, t, to_a_var v, args)]
  | IGetElementPtr (d, t, v, inds) ->
     [IGetElementPtr (to_a_var d, t, to_a_var v,
                     List.map (fun (t, v) -> (t, to_a_value v)) inds)]
  | IAlloca (d, t, n) -> [IAlloca (to_a_var d, t, n)]
  | ILoad (d, t, v) ->
     [ILoad (to_a_var d, t, to_a_var v)]
  | IStore (t, vl, vr) ->
     [IStore (t, to_a_value vl, to_a_var vr)]
  | IPhi _ -> failwith "shouldn't have phis"
            
let to_a_func f =
  let temps = List.map (fun _ -> new_temp ()) R.callee_saved in
  let save_regs_code =
    List.map2
      (fun r t -> ISet (Variable t, TInteger 32, Var (Register r)))
      R.callee_saved
      temps
  in
  let rec copy_args args arg_regs =
    match (args, arg_regs) with
    | ([], _) -> []
    | ((t, a1)::args, r1::arg_regs) ->
       (ISet (Variable (Local a1), t, Var (Register r1)))
       ::(copy_args args arg_regs)
    | (_, []) ->
       (* Args on the stack will be moved later *)
       []
  in
  let copy_args_code = copy_args f.f_args R.args in
  let restore_regs_code =
    List.map2
      (fun r t -> ISet (Register r, TInteger 32, Var (Variable t)))
      R.callee_saved
      temps
  in
  (* Will need to move callee-saved regs into temps *)
  { f_name = f.f_name;
    f_ret  = f.f_ret;
    f_args = f.f_args;
    f_body = Array.of_list
               (copy_args_code
                @ save_regs_code
                @ (List.concat (List.map (to_a_inst f.f_name)
                                  (Array.to_list f.f_body)))
                @ [ILabel (Config.exit_label_of_fun f.f_name)]
                @ restore_regs_code
                @ [IRet None]);
    f_labels = f.f_labels
  }
           
module VRMap = Map.Make
                 (struct
                   type t = varorreg
                   let compare = compare_vr
                 end
                 )

module Live = Dataflow.Make
                (struct type t = varorreg end)
                (struct
                  type t = varorreg
                  let compare = compare_vr
                end
                )

let liveness cfg =
  Live.compute
    cfg
    (fun i -> Live.FSet.of_list (LLVM.Utils.use_inst i))
    (fun i s -> Live.FSet.diff s (Live.FSet.of_list (LLVM.Utils.def_inst i)))
    false
    false
