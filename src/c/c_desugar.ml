(** CS443 - Desugar Parsed C **)
(** Stefan Muller - 2022 **)

open Cabs
module C = C_ast

exception Unsupported of string * C.loc
exception DesugarError of string * C.loc

let collect_names name_group_list =
  List.concat (List.map (fun (_, _, nl) -> nl) name_group_list)

module VMap = Varmap
  
let flds : (string * C.typ) list VMap.t ref = ref VMap.empty
let aliases : C.typ VMap.t ref = ref VMap.empty

let rec desugar_base_type loc t =
  let uns s =
    raise (Unsupported (s, loc))
  in
  let dbt = desugar_base_type loc
  in
  match t with
  | NO_TYPE -> uns "declaration with no type"
  | VOID -> C.TVoid
  | BOOL -> C.TBool
  | CHAR UNSIGNED -> uns "unsigned types"
  | CHAR _ -> TChar
  | INT (_, UNSIGNED) -> uns "unsigned types"
  | INT (NO_SIZE, _) -> C.TInt
  | INT (_, _) -> uns "size modifiers"
  | BITFIELD _ -> uns "bitfields"
  | FLOAT _ | DOUBLE _ | COMPLEX_FLOAT | COMPLEX_DOUBLE
    | COMPLEX_LONG_DOUBLE -> uns "floating-point"
  | PTR (PROTO (r, l, _)) ->
     C.TFunction (dbt r,
                  List.map (fun (_, _, (n, t, _, _)) -> (dbt t, n)) l)
  | PTR t -> uns "pointer types" (* TPtr (dbt t) *)
  | RESTRICT_PTR _ -> uns "restricted pointers"
  | ARRAY (t, e) -> C.TArray (dbt t)
                            (*
     let t = dbt t in
     (match (desugar_expression loc e).C.edesc with
      | C.EConst (C.CInt n) -> C.TArray (t, n)
      | _ -> uns "variable-size arrays"
     )
                             *)
  | STRUCT (s, ngl) -> uns "'struct'; structs must be defined at top level"
  | UNION _ -> uns "union types"
  | PROTO (r, l, _) ->
     C.TFunction (dbt r,
                  List.map (fun (_, _, (n, t, _, _)) -> (dbt t, n)) l)
  | OLD_PROTO _ -> uns "old-style function prototypes"
  | NAMED_TYPE s -> C.TStruct s
     (*
     (try VMap.find s (!aliases)
      with Not_found ->
        raise (DesugarError ("Could not resolve type " ^ s, loc))
     ) *)
  | ENUM _ -> uns "enums"
  | CONST _ | VOLATILE _ | GNU_TYPE _ -> uns "type modifiers"
  | BUILTIN_TYPE _ -> uns "built-in types"
  | TYPE_LINE (fl, ln, t) -> desugar_base_type  (fl, ln) t

and desugar_definition loc d =
  let uns s =
    raise (Unsupported (s, loc))
  in
  C.mk_def
    (match d with
     | FUNDEF ((_, _, (s, t, _, _)), b) ->
        C.DFun (s, desugar_base_type loc t, desugar_body loc b)
     | OLDFUNDEF _ -> uns "old-style function definitions"
     | DECDEF (_, _, ns) ->
        C.DName (List.map
                   (function
                      (s, t, _, NOTHING) -> (s, desugar_base_type loc t, None)
                    | (s, t, _, e) ->
                       (s, desugar_base_type loc t,
                        Some (desugar_expression loc e)))
                   ns)
     | TYPEDEF ((_, _, ns), _)
       | ONLYTYPEDEF (_, _, ns) ->
        uns "typedef"
            (*
        List.iter
          (fun (s, t, _, _) ->
            aliases := VMap.add s (desugar_base_type loc t) (!aliases))
          ns;
        C.DTypeDef (List.map (fun (s, t, _, _) ->
                        (s, desugar_base_type loc t))
                      ns)
             *)
     | STRUCTDEF (s, ngl) ->
        let fields =
          List.map (fun (n, t, _, _) -> (n, desugar_base_type loc t))
            (collect_names ngl)
        in
        (*
        Printf.printf "Adding %s with %d fields\n" s (List.length fields);
         *)
        flds := VMap.add s fields (!flds);
        (*
        Printf.printf "fields: ";
        VMap.iter (fun k _ -> Printf.printf "%s " k) (!flds);
        Printf.printf "\n";
         *)
        C.DStructDef (s, fields)
    )
    loc

and desugar_local_decl loc d =
  let uns s =
    raise (Unsupported (s, loc))
  in
  let ms s = C.mk_stmt s loc in
  match d with
    DECDEF (_, _, ns) ->
     List.map
       (function
          (s, t, _, NOTHING) ->
           ms (C.SDecl (s, desugar_base_type loc t, None))
        (* uns "declarations without initialization" *)
        | (s, t, _, e) -> (ms (C.SDecl (s, desugar_base_type loc t,
                                       Some (desugar_expression loc e))))
       )
       ns
  | _ -> uns "this as local declaration"
  
and desugar_body loc (ds, s) : C.p_stmt =
  C.mk_stmt
    (C.SBlock (
         (List.concat (List.map (desugar_local_decl loc) ds))
         @ (desugar_statement loc s)
    ))
  loc

and desugar_statement loc s =
  let uns s =
    raise (Unsupported (s, loc))
  in
  let ds = desugar_statement loc in
  let ms s = C.mk_stmt s loc in
  let dsb s = ms (C.SBlock (ds s)) in
  match s with
  | NOP -> [ms (C.SBlock [])]
  | DEFINITION d -> desugar_local_decl loc d
  | COMPUTATION e -> [ms (C.SExp (desugar_expression loc e))]
  | BLOCK b -> [desugar_body loc b]
  | SEQUENCE (s1, s2) -> (ds s1) @ (ds s2)
  | IF (e, s1, s2) -> [ms (C.SIf (desugar_expression loc e,
                                  dsb s1,
                                  dsb s2))]
  | WHILE (e, s) -> [ms (C.SFor (C.mk_exp (C.EConst (C.CInt 0)) loc,
                                 desugar_expression loc e,
                                 C.mk_exp (C.EConst (C.CInt 0)) loc,
                                 dsb s))]
  | DOWHILE (e, s) ->
     let s' = ds s in
     s'
     @ [ms (C.SFor (C.mk_exp (C.EConst (C.CInt 0)) loc,
                    desugar_expression loc e,
                    C.mk_exp (C.EConst (C.CInt 0)) loc,
                    ms (C.SBlock s')))]
  | FOR (e1, e2, e3, s) ->
     [ms (C.SFor (desugar_expression loc e1,
                  desugar_expression loc e2,
                  desugar_expression loc e3,
                  ms (C.SBlock (ds s))))]
  | BREAK -> [ms C.SBreak]
  | CONTINUE -> [ms C.SContinue]
  | RETURN NOTHING -> [ms (C.SReturn None)]
  | RETURN e -> [ms (C.SReturn (Some (desugar_expression loc e)))]
  | SWITCH (e, s) -> uns "switch" 
  | CASE _ -> uns "case"
  | DEFAULT _ -> uns "default"
  | LABEL (_, s) -> ds s
  (* ewwww, goto *)
  | GOTO _ -> uns "goto"
  | ASM _ | GNU_ASM _-> uns "embedded assembly"
  | STAT_LINE (s, fl, ln) -> desugar_statement (fl, ln) s

and ds_as_statement loc s =
  C.mk_stmt (C.SBlock (desugar_statement loc s)) loc

and desugar_binop loc b e1 e2 =
  let uns s =
    raise (Unsupported (s, loc))
  in
  let me e = C.mk_exp e loc in
  let e1 = desugar_expression loc e1 in
  let e2 = desugar_expression loc e2 in
  match b with
  | ASSIGN | ADD_ASSIGN | SUB_ASSIGN | MUL_ASSIGN | DIV_ASSIGN
    | MOD_ASSIGN | BAND_ASSIGN | BOR_ASSIGN | XOR_ASSIGN | SHL_ASSIGN
    | SHR_ASSIGN ->
     me (C.EAssign ((match C.exp_to_lhs e1 with
                     | Some l -> l
                     | None -> uns "lhs"
                    )
                  ,
                    match b with
                    | ASSIGN -> e2
                    | ADD_ASSIGN -> me (C.EBinop (C.BAdd, e1, e2))
                    | SUB_ASSIGN -> me (C.EBinop (C.BSub, e1, e2))
                    | MUL_ASSIGN -> me (C.EBinop (C.BMul, e1, e2))
                    | DIV_ASSIGN -> me (C.EBinop (C.BDiv, e1, e2))
                    | MOD_ASSIGN -> uns "mod"
                    | BAND_ASSIGN -> uns "&="
                    | BOR_ASSIGN -> uns "|="
                    | XOR_ASSIGN -> uns "^="
                    | SHL_ASSIGN | SHR_ASSIGN -> uns "bitshifts"
                    | _ -> uns "shouldn't happen"
           )
       )
  | _ ->
     me
       (C.EBinop (
            (match b with
             | ADD -> C.BAdd
            | SUB -> C.BSub
            | MUL -> C.BMul
            | DIV -> C.BDiv
            | MOD -> uns "mod"
            | AND -> C.BAnd
            | OR -> C.BOr
            | BAND -> uns "&"
            | BOR -> uns "|"
            | XOR -> uns "^"
            | SHL | SHR -> uns "bitshifts"
            | EQ -> C.BEq
            | NE -> C.BNe
            | LT -> C.BLt
            | GT -> C.BGt
            | LE -> C.BLe
            | GE -> C.BGe
            | _ -> uns "shouldn't happen"
            )
          ,
            e1, e2))

and desugar_unop loc u e =
  let uns s =
    raise (Unsupported (s, loc))
  in
  let e = desugar_expression loc e in
  let const_1 = C.mk_exp (C.EConst (C.CInt 1)) loc in
  let e_lhs () =
    match C.exp_to_lhs e with
      Some l -> l
    | None -> uns "lhs"
  in
  C.mk_exp
    (match u with
     | MINUS -> C.EUnop (C.UNeg, e)
     | PLUS -> uns "unary +"
     | NOT -> C.EUnop (C.UNot, e)
     | BNOT -> uns "~"
     | MEMOF | ADDROF -> uns "explicit pointers" 
     | PREINCR ->
        C.EAssign (e_lhs (), C.mk_exp (C.EBinop (C.BAdd, e, const_1)) loc)
     | PREDECR ->
        C.EAssign (e_lhs (), C.mk_exp (C.EBinop (C.BSub, e, const_1)) loc)
     | POSINCR ->
        C.EBinop
          (C.BSub,
           C.mk_exp
             (C.EAssign (e_lhs (), C.mk_exp (C.EBinop (C.BAdd, e, const_1)) loc))
             loc,
           const_1)
     | POSDECR ->
        C.EBinop
          (C.BAdd,
           C.mk_exp
             (C.EAssign (e_lhs (), C.mk_exp (C.EBinop (C.BSub, e, const_1)) loc))
             loc,
           const_1)
    )
    loc
                           
  
and desugar_expression loc e =
  let uns s =
    raise (Unsupported (s, loc))
  in
  let de = desugar_expression loc in
  match e with
  | NOTHING -> C.mk_exp (C.EConst (C.CInt 0)) loc
  | UNARY (u, e) -> desugar_unop loc u e
  | BINARY (b, e1, e2) -> desugar_binop loc b e1 e2
  | QUESTION (e1, e2, e3) -> uns "ternary operator"
  | CAST (t, e) -> C.mk_exp (C.ECast (de e, desugar_base_type loc t)) loc
  | CALL (e, es) ->
     C.mk_exp (C.ECall (de e, List.map de es)) loc
  | COMMA _ -> uns "comma expressions"
  | CONSTANT c -> C.mk_exp (C.EConst (desugar_constant loc c)) loc
                           (* Make all variables local for now---typechecking
                            * will fix this *)
  | VARIABLE s -> C.mk_exp (C.EVar (s, Local)) loc
  | EXPR_SIZEOF _ | TYPE_SIZEOF _ -> uns "sizeof"
  | NEW_TYPE (ARRAY (t, CONSTANT (CONST_INT n))) ->
     let t = desugar_base_type loc t in 
     C.mk_exp (C.ENewArray (t, int_of_string n)) loc
  | NEW_TYPE (STRUCT (s, _))
    | NEW_TYPE (NAMED_TYPE s) ->
     C.mk_exp (C.ENewStruct s) loc
  | NEW_TYPE _ -> uns "new other than array and struct"
  | INDEX (e1, e2) ->
     C.mk_exp (C.EArrIndex (de e1, de e2)) loc
  | MEMBEROF (e, s) | MEMBEROFPTR (e, s) ->
     C.mk_exp (C.EField (de e, s)) loc
  | GNU_BODY _ -> uns "GNU bodies"
  | DESIGNATED _ -> uns "designated initialization"
  | EXPR_LINE (e, fl, ln) -> desugar_expression (fl, ln) e
                                                                   
and desugar_constant loc c =
  let uns s =
    raise (Unsupported (s, loc))
  in
  match c with
  | CONST_INT s -> C.CInt (int_of_string s)
  | CONST_FLOAT _ -> uns "floating-point"
  | CONST_CHAR s -> C.CChar s.[0]
  | CONST_STRING _ -> uns "strings"
  | CONST_COMPOUND _ -> uns "compound constants"

let desugar_file fname (ds: file) : unit C.file * (string * C.typ) list VMap.t =
  let prog = List.map (desugar_definition (fname, 0)) ds
  in
  (prog, !flds)
                                                                     
