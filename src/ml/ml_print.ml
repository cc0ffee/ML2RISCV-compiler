open Ml_ast
open Format

let rec seq p sep f l =
  match l with
  | [] -> ()
  | [x] -> fprintf f "@[%a@]" p x
  | x::r -> fprintf f "%a@[%s%a@]"
              p x
              sep
              (seq p sep) r

let rec string_of_typ t =
  match t with
  | TInt -> "int"
  | TBool -> "bool"
  | TUnit -> "unit"
  | TList t -> (string_of_typ t) ^ " list"
  | TArrow (t1, t2) ->
     (string_of_typ t1) ^ " -> " ^ (string_of_typ t2)
  | TProd (t1, t2) ->
     "(" ^ (string_of_typ t1) ^ " * " ^ (string_of_typ t2) ^ ")"

let string_of_op = function
  | BAdd -> "+"
  | BSub -> "-"
  | BMul -> "*"
  | BDiv -> "/"
  | BLt -> "<"
  | BLe -> "<="
  | BGt -> ">"
  | BGe -> ">="
  | BEq -> "="
  | BNe -> "<>"
  | BAnd -> "&&"
  | BOr -> "||"
             
let rec pprint_expr f e =
  let ppe = pprint_expr in
  match e.edesc with
  | EVar v -> fprintf f "%s" v
  | EConst (CNum n) -> fprintf f "%d" n
  | EConst (CBool true) -> fprintf f "true"
  | EConst (CBool false) -> fprintf f "false"
  | EConst CTriv -> fprintf f "()"
  | EConst CNil -> fprintf f "[]"
  | EBinop (o, e1, e2) -> fprintf f "@[<1>@[%a@]@ %s @[%a@]@]"
                              ppe e1
                              (string_of_op o)
                              ppe e2
  | EUnop (UNot, e) -> fprintf f "not@[<2>@[%a@]@]" ppe e
  | EUnop (UNeg, e) -> fprintf f "-@[<2>@[%a@]@]" ppe e
  | EFun (x, t, ebody) ->
     fprintf f "@[<2>fun@ (%s:@ %s)@ ->@ @[%a@]@]"
       x
       (string_of_typ t)
       ppe ebody
  | EIf (e1, e2, e3) ->
     fprintf f "@[<2>if@ @[%a@]@ then@ @[%a@]@ else@ @[%a@]@]"
       ppe e1
       ppe e2
       ppe e3
  | ELet (x, t, e1, e2) ->
     fprintf f "@[<2>let %s@ : %s@ =@ @[%a@]@ in@ @[%a@]@]"
       x
       (match t with
        | Some t -> string_of_typ t
        | None -> string_of_typ e1.einfo)
       ppe e1
       ppe e2
  | ELetFun (is_rec, fname, x, tx, tr, ebody, e2) ->
     fprintf f "@[<2>let%s@ %s@ (%s:@ %s)@ :@ %s@=@ %a@ in@ %a@]"
       (if is_rec then " rec" else "")
       fname
       x
       (string_of_typ tx)
       (match tr with
        | Some t -> string_of_typ t
        | None -> string_of_typ ebody.einfo)
       ppe ebody
       ppe e2
  | ELetPair (x, y, e1, e2) ->
     fprintf f "@[<2>let (%s, %s) =@ @[%a@]@ in@ @[%a@]@]"
       x
       y
       ppe e1
       ppe e2
  | EApp (e1, e2) ->
     fprintf f "@[<1>@[(%a)@ @[(%a)@]@]"
       ppe e1
       ppe e2
  | EMatchList (econd, enil, h, t, econs) ->
     fprintf f "@[<2>match@ @[%a@]@ with@, [] ->@ @[%a@]@,| %s::%s ->@ @[%a@]@]"
       ppe econd
       ppe enil
       h
       t
       ppe econs
  | EPair (e1, e2) ->
     fprintf f "@[(%a,@ %a)@]"
       ppe e1
       ppe e2
  | ECons (e1, e2) ->
     fprintf f "@[(%a)@,::(%a)@]"
       ppe e1
       ppe e2
  | EAnnot (e, _) -> ppe f e

                         (*
let rec pprint_decl f d =
  let ppe = pprint_expr in
  match d.ddesc with
  | DVal (x, _, e) ->
     fprintf f "@[<1>let@ %s:@ @[%s@] =@ @[%a@]@]"
       x
       (string_of_typ e.einfo)
       ppe e
  | DFun (is_rec, fname, x, tx, tr, ebody) ->
     fprintf f "@[<1>let %s%s(%s:@ %s)@ :@ %s@ =@ @[%a@]@]"
       (if is_rec then "rec " else "")
       fname
       x
       (string_of_typ tx)
       (match tr with
        | Some t -> string_of_typ t
        | None -> string_of_typ ebody.einfo)
       ppe ebody
  | DExp e -> ppe f e
                          *)
