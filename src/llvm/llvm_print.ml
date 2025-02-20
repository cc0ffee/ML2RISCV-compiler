(** CS443 - LLVM Pretty-printer **)
(** Stefan Muller - 2022 **)

open Llvm_ast
open Format

let rec string_of_typ t =
  match t with
  | TInteger n -> "i" ^ (string_of_int n)
  | TPointer t -> (string_of_typ t) ^"*"
  | TVoid -> "void"
  | TArray t -> "[0 x " ^ (string_of_typ t) ^ "]"
  | TStruct s -> "%" ^ s
     (*
     ts ->
     "{" ^ (String.concat ", " (List.map string_of_typ ts)) ^ "}"
      *)
  | TFunction (rt, argts) ->
     (string_of_typ rt) ^ " (" ^
       (String.concat ", " (List.map string_of_typ argts)) ^ ")"

let string_of_var v =
  match v with
  | Local s -> "%" ^ s
  | Global s -> "@" ^ s

let var_of_string s =
  match s.[0] with
  | '%' -> Local (String.sub s 1 ((String.length s) - 1))
  | '@' -> Global (String.sub s 1 ((String.length s) - 1))
  | _ -> raise (Invalid_argument "var_of_string")
    
let print_var f v =
  fprintf f "%s" (string_of_var v)
    
let print_value f v =
  match v with
  | Const n -> fprintf f "%d" n
  | Var v -> fprintf f "%a" print_var v

let print_ptr_value f v =
  match v with
  | Const n -> if n = 0 then fprintf f "null"
               else fprintf f "%d" n
  | Var v -> fprintf f "%a" print_var v

let is_ptr_typ =
  function
  | TInteger _ | TFunction _ | TVoid | TArray _ | TStruct _ -> false
  | TPointer _ -> true
           
let print_inst f i =
  match i with
  | ILabel l -> fprintf f "%s:" l
  | ISet (d, t, v) ->
     (* Thanks https://stackoverflow.com/questions/31091804/what-is-a-move-opcode-in-llvm-ir
      * for this hack *)
     fprintf f "  %a = bitcast %s %a to %s"
       print_var d
       (string_of_typ t)
       print_value v
       (string_of_typ t)
  | IBinop (d, b, t, v1, v2) ->
     fprintf f "  %a = %s %s %a, %a"
       print_var d
       (match b with
        | BAdd -> "add" | BSub -> "sub" | BMul -> "mul"
        | BDiv -> "sdiv" | BAnd -> "and" | BOr -> "or" | BXor -> "xor")
       (string_of_typ t)
       print_value v1
       print_value v2
  | ICmp (d, c, t, v1, v2) ->
     fprintf f "  %a = icmp %s %s %a, %a"
       print_var d
       (match c with
        | CEq -> "eq" | CNe -> "ne" | CSGt -> "sgt" | CSGe -> "sge"
        | CSLt -> "slt" | CSLe -> "sle"
       )
       (string_of_typ t)
       print_value v1
       print_value v2
  | ICast (d, ct, it, v, ot) ->
     fprintf f "  %a = %s %s %a to %s"
       print_var d
       (match ct with
        | CZext -> "zext" | CSext -> "sext"
        | CBitcast -> "bitcast" | CTrunc -> "trunc"
        | CPtrtoint -> "ptrtoint" | CInttoptr -> "inttoptr")
       (string_of_typ it)
       print_value v
       (string_of_typ ot)
  | IBr l -> fprintf f "  br label %%%s" l
  | ICondBr (v, ifl, elsel) ->
     fprintf f "  br i1 %a, label %%%s, label %%%s"
       print_value v
       ifl
       elsel
  | IRet (Some (t, v)) ->
     fprintf f "  ret %s %a"
       (string_of_typ t)
       (if is_ptr_typ t then print_ptr_value else print_value) v
  | IRet None -> fprintf f "  ret"
  | ICall (d, t, fname, args) ->
     fprintf f "  %a = call %s %a(@[%a@])"
       print_var d
       (string_of_typ t)
       print_var fname
       (pp_print_list ~pp_sep:(fun f () -> fprintf f ", ")
          (fun f (t, v) -> fprintf f "%s %a"
                              (string_of_typ t)
                              print_value v
       ))
       args
  | IGetElementPtr (d, t, v, inds) ->
     fprintf f "  %a = getelementptr %s, %s %a, %a"
       print_var d
       (string_of_typ t)
       (string_of_typ (TPointer t))
       print_var v
       (pp_print_list ~pp_sep:(fun f () -> fprintf f ", ")
          (fun f (t, v) -> fprintf f "%s %a"
                              (string_of_typ t)
                              print_value v
       ))
       inds
  | IAlloca (d, t, n) ->
     fprintf f "  %a = alloca %s, i32 %d"
       print_var d
       (string_of_typ t)
       n
  | ILoad (d, t, v) ->
     fprintf f "  %a = load %s, %s %a"
       print_var d
       (string_of_typ t)
       (string_of_typ (TPointer t))
       print_var v
  | IStore (t, va, vr) ->
     fprintf f "  store %s %a, %s %a"
       (string_of_typ t)
       print_value va
       (string_of_typ (TPointer t))
       print_var vr
  | IPhi (d, t, preds) ->
     fprintf f "  %a = phi %s %a"
       print_var d
       (string_of_typ t)
       (pp_print_list ~pp_sep:(fun f () -> fprintf f ", ")
          (fun f (l, v) -> fprintf f "[%a, %%%s]"
                             (if is_ptr_typ t then print_ptr_value
                              else print_value) v
                           l
       ))
       preds
       
let print_func f func =
  fprintf f "define %s %@%s(@[%a@]) {@ %a@ }@."
    (string_of_typ func.f_ret)
    func.f_name
    (pp_print_list ~pp_sep:(fun f () -> fprintf f ", ")
          (fun f (t, s) -> fprintf f "%s %%%s"
                           (string_of_typ t)
                           s
       ))
    func.f_args
    (pp_print_list ~pp_sep:(fun f () -> fprintf f "@.")
       print_inst)
    (Array.to_list func.f_body)

let print_typdefs f tds =
  Varmap.mapi
    (fun s ts ->
      fprintf f "%%%s = type {@ %s@ }@."
        s
        (String.concat ", " (List.map string_of_typ ts))
    )
    tds

let print_prog f (prog, tds) =
  let _ = print_typdefs f tds in
  fprintf f "declare i8* @@malloc(i32)@.";
  pp_print_list ~pp_sep:(fun f () -> fprintf f "@.")
    print_func
    f
    prog
