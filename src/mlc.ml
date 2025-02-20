(** IIT CS443 **)
(** ML to C Compiler **)
(** Project 4 **)

open ML.Ast

module Ca = C.Ast

type env_record = (var * int) list

type cfunction = { cname : string;
                     cret : Ca.typ;
                     cparam : string * Ca.typ;
                     cbody: Ca.p_stmt }

let cloc_of_mlloc (stpos, _) = (stpos.Lexing.pos_fname, stpos.Lexing.pos_lnum)
let dummy_loc = ("", 0)
let mk_exp e = { Ca.edesc = e;
                 Ca.eloc = dummy_loc;
                 Ca.einfo = ()
               }
let mk_lhs e = { Ca.ldesc = e;
                 Ca.lloc = dummy_loc;
                 Ca.linfo = ()
               }
let mk_stmt s = { Ca.sdesc = s;
                  Ca.sloc = dummy_loc;
                }

(* Convenience functions *)
(* I HIGHLY suggest that you read through the signatures of all of these
 * functions before starting the project. *)

(* The default type for values of unknown type *)
let def_typ = Ca.TInt
(* The default type of function pointers *)
let fptr_typ = Ca.TFunction (Ca.TInt, [])


(* Produce code to declare and initialize a struct with given values for fields
 *)
let init_struct (stype: string) (fields : (string * Ca.p_exp) list) 
: Ca.p_stmt list * Ca.p_exp * cfunction list =
  let svar = new_var () in
  ((mk_stmt (Ca.SDecl (svar, Ca.TStruct stype, Some (mk_exp (ENewStruct stype)))))
   ::
     (List.fold_left
        (fun ss (f, v) ->
          (mk_stmt (Ca.SExp (mk_exp (Ca.EAssign (mk_lhs (LHField (svar, (), f)),
                                               v)))))
          ::ss
        )
        []
        fields)
  ,
    mk_exp (EVar (svar, Local)),
  []
  )

(* Produce a C assignment expression of e to v *)
let mk_assign (v: Ca.var) (e: Ca.p_exp) : Ca.p_exp =
  mk_exp (Ca.EAssign (mk_lhs (Ca.LHVar v), e))

(* Produce a C assignment statement of e to v *)
let mk_assign_s (v: Ca.var) (e: Ca.p_exp) : Ca.p_stmt =
  mk_stmt (Ca.SExp (mk_assign v e))

(* Produce a C expression casting e (C expression) to t *)
let mk_cast_e (e: Ca.p_exp) (t: Ca.typ) =
  mk_exp (Ca.ECast (e, t))

(* Produce a C expression casting e (C expression_desc) to t *)
let mk_cast (e: unit Ca.exp_) (t: Ca.typ) = mk_cast_e (mk_exp e) t

(* Definitions for compiling lists *)
let list_struct : string = "__list"
let list_hd : string = "list_hd"
let list_tl : string = "list_tl"
let list_fields : (string * Ca.typ) list =
  [(list_hd, def_typ); (list_tl, Ca.TStruct list_struct)]
let compile_nil : Ca.p_exp =
  mk_exp (Ca.ECast (mk_exp (Ca.EConst (Ca.CInt 0)), Ca.TStruct list_struct))
let compile_cons (h: Ca.p_exp) (t: Ca.p_exp)
    : Ca.p_stmt list * Ca.p_exp * cfunction list =
  init_struct
    list_struct
    [(list_hd, h); (list_tl, t)]

(* Definitions for compiling pairs *)
let pair_struct : string = "__pair"
let pair_fst : string = "pair_fst"
let pair_snd : string = "pair_snd"
let pair_fields = [(pair_fst, def_typ); (pair_snd, def_typ)]

(* Definitions for compiling environments *)
let env_var : string = "__env"
let env_type : Ca.typ = Ca.TStruct list_struct
let lookup_fun = "__lookup"

(* Produce code to get the ith value in the environment, cast to type t *)
let lookup_in_env (i: int) (t: Ca.typ) =
  ([],
   mk_cast (Ca.ECall (mk_exp (Ca.EVar (lookup_fun, Ca.Global)),
                      [mk_exp (Ca.EConst (Ca.CInt i));
                       mk_exp (Ca.EVar (env_var, Ca.Local))])) t,
   [])

    (* The C code for the "lookup" function.
     * Don't confuse this with lookup_in_env, which is what you will call
     * from your code, to generate a call to lookup.
     * This isn't a function, it's just a pile of compiled C code *)
let lookup =
  let ivar = "i" in
  let evar = env_var in
  let dummy_exp = mk_exp (Ca.EConst (Ca.CInt 0)) in
  Ca.mk_def
    (Ca.DFun
    (lookup_fun,
     Ca.TFunction (def_typ, [(Ca.TInt, ivar); (env_type, env_var)]),
     mk_stmt
       (Ca.SBlock
          [mk_stmt (Ca.SFor (dummy_exp,
                             mk_exp (Ca.EBinop (Ca.BGt, mk_exp (Ca.EVar (ivar, Ca.Local)),
                                                mk_exp (Ca.EConst (Ca.CInt 0)))),
                             dummy_exp,
                               mk_stmt
                                 (Ca.SBlock
                                    [mk_assign_s
                                       evar
                                       (mk_exp (Ca.EField (mk_exp (Ca.EVar (evar, Ca.Local)),
                                                           list_tl)));
                                     mk_assign_s
                                       ivar
                                       (mk_exp
                                          (Ca.EBinop (Ca.BSub,
                                                      mk_exp (Ca.EVar (ivar, Ca.Local)),
                                                      mk_exp (Ca.EConst (Ca.CInt 1)))))]
                                 )
             ));
           mk_stmt (Ca.SReturn
                      (Some
                         (mk_exp
                            (Ca.EField (mk_exp (Ca.EVar (evar, Ca.Local)), list_hd)))))
       ])
    )
    )
    dummy_loc

(* Generate code to extend the environment with variable x, bound to
 * value v, and add it to the environment record. *)
(* Returns the C code to extend the env, and the new environment record *)
let extend_env (env_record: env_record) (x: Ca.var) (v: Ca.p_exp)
  : (Ca.p_stmt list * Ca.p_exp * cfunction list) * (string * int) list =
  let (is, e, c) =
    compile_cons (mk_cast_e v def_typ) (mk_exp (Ca.EVar (env_var, Ca.Local)))
  in
  ((is @ [mk_assign_s env_var e], mk_exp (Ca.EVar (env_var, Ca.Local)), c),
   (x, 0)::(List.map (fun (x, n) -> (x, n + 1)) env_record)
  )

(* Add a placeholder for variable x to the environment record. *)
let extend_with_placeholder (env_record: env_record) (x: Ca.var) =
  (x, 0)::(List.map (fun (x, n) -> (x, n + 1)) env_record)

(* Pop the first entry from both the environment and environment record
* when we leave a scope. Returns a pair of the C code to do the pop, and the
* new environment record. *)
let pop_env (env_record: env_record) =
  match env_record with
  | _::t ->
     (([mk_assign_s env_var
          (mk_exp (Ca.EField (mk_exp (Ca.EVar (env_var, Ca.Local)), list_tl)))],
       [],
       []),
     List.map (fun (x, n) -> (x, n - 1)) t)
  | _ -> failwith "empty env"

(* Definitions for compiling closures *)
let clos_struct : string = "__clos"
let clos_env : string = "clos_env"
let clos_fun : string = "clos_fun"
let clos_fields = [(clos_env, env_type); (clos_fun, fptr_typ)]


let rec compile_typ (t: typ) : Ca.typ =
  match t with
  | TInt -> Ca.TInt
  | TBool -> Ca.TBool
  | TUnit -> Ca.TInt
  | TList _ -> Ca.TStruct list_struct
  | TArrow (t1, t2) -> Ca.TStruct clos_struct
  | TProd _ -> Ca.TStruct pair_struct

let compile_const (c: const) : Ca.p_stmt list * Ca.p_exp * cfunction list =
  match c with
  | CNum n -> ([], mk_exp (Ca.EConst (Ca.CInt n)), [])
  | CBool b ->
     ([], mk_cast (Ca.EConst (Ca.CInt (if b then 1 else 0))) Ca.TBool, [])
  | CTriv -> ([], mk_exp (Ca.EConst (Ca.CInt 0)), [])
  | CNil -> ([], compile_nil, [])

let compile_bop =
  function BAdd -> Ca.BAdd | BSub -> Ca.BSub | BMul -> Ca.BMul
           | BDiv -> Ca.BDiv | BAnd -> Ca.BAnd | BOr -> Ca.BOr
           | BGt -> Ca.BGt | BGe -> Ca.BGe | BLt -> Ca.BLt
           | BLe -> Ca.BLe | BNe -> Ca.BNe | BEq -> Ca.BEq

(* added compile_unop for cleaner code *)
let compile_unop =
    function UNot -> Ca.UNot | UNeg -> Ca.UNeg

exception Unimplemented

(* Compile the body of a function *)
let rec compile_body (env_record: env_record) (name: string) (x: var) (tx: typ) (body: t_exp)
        : Ca.p_stmt list * Ca.p_exp * cfunction list =
        (* From lecture we need to extend_env. Also from proj writeup that all EVar are to be Local *)
        let ((s1, e1, c1), env') = extend_env env_record x (mk_exp (Ca.EVar (x, Ca.Local))) in
        let (s2, e2, c2) = compile_exp env' body in
        (* cfunction template needed for body *)
        let cfunc_body = {cname = name; cret = compile_typ body.einfo; cparam = x, compile_typ tx; cbody = mk_stmt (Ca.SBlock (s1 @ s2 @ [mk_stmt (Ca.SReturn (Some e2))]))} in 
        (* in lecture 11 there is closure struct we use to represent this *)
        (* writeup says clos_fun is ptr to function and its already defined as fptr_typ *)
        let (s3, e3, c3) = init_struct clos_struct [(clos_env, mk_exp(Ca.EVar (env_var, Ca.Local))); (clos_fun, mk_cast (Ca.EVar (name, Ca.Local)) fptr_typ)] in
        s3, e3, cfunc_body :: c1 @ c2 @ c3

(* Compile an expression *)
and compile_exp (env_record: env_record) (e: t_exp)
          : Ca.p_stmt list * Ca.p_exp * cfunction list =
    match e.edesc with
                    (* "function lookup_in_env i t returns ... looks up ith index in env and casts type "*)
    | EVar (var) -> lookup_in_env (List.assoc var env_record) (compile_typ e.einfo)
    | EConst (const) -> compile_const const
                            (* compile each expression to the env *)
    | EBinop (b, e1, e2) -> let (s1, e1', c1) = compile_exp env_record e1 in
                            let (s2, e2', c2) = compile_exp env_record e2 in
                            (* p_stmt_list * p_exp * c_func list is the format, if there's multiple concat them*)
                            s1 @ s2, mk_exp(Ca.EBinop (compile_bop b, e1', e2')), c1 @ c2
    | EUnop (u, e) -> let (s1, e1, c1) = 
                      compile_exp env_record e in
                      s1, mk_exp(Ca.EUnop (compile_unop u, e1)), c1
    (* mangle makes the func var name unique so whenever it is called its the right one *)
    | EFun (v, t, e) -> let name = new_mangle "__fun" in compile_body env_record name v t e
    | EIf (e1, e2, e3) -> let (s1, e1', c1) = compile_exp env_record e1 in
                          let (s2, e2', c2) = compile_exp env_record e2 in
                          let (s3, e3', c3) = compile_exp env_record e3 in
                          let temp_var = new_var() in
                          (* reference the lecture slide for if else *)
                          (* Block from lecture where it assigns temp var to exp *)
                          let true_stmt = mk_stmt(Ca.SBlock(s2 @ [mk_assign_s temp_var e2'])) in
                          let false_stmt = mk_stmt(Ca.SBlock (s3 @ [mk_assign_s temp_var e3'])) in
                          (* int temp1 = Decl of temp_var as a statement *)
                          s1 @ [mk_stmt (Ca.SDecl(temp_var, compile_typ e2.einfo, None))] @ 
                          [mk_stmt(Ca.SIf(e1', true_stmt, false_stmt))], 
                          mk_exp(Ca.EVar (temp_var, Ca.Local)), 
                          c1 @ c2 @ c3
    | ELet (v, _, e1, e2) -> (* from lecture, follows compile, extend, compile, pop *)
                             (* statements for e1
                                env = __extend_env(env, “x”, e1_exp);
                                statements for e2
                                env = __pop_env(env);*)
                             let (s1, e1', c1) = compile_exp env_record e1 in
                             let ((s2, e2', c2), env_record1) = extend_env env_record v e1' in
                             let (s3, e3', c3) = compile_exp env_record1 e2 in
                             let((s4, e4', c4), env_record2) = pop_env env_record1 in 
                             let temp_var = new_var() in
                             (* Declaration of let value and apply it to value *)
                             s1 @ s2 @ s3 @ [mk_stmt (Ca.SDecl (temp_var, compile_typ e2.einfo, Some e3'))] @ s4, mk_exp(Ca.EVar (temp_var, Ca.Local)), c1 @ c2 @ c3 @ c4
    | ELetFun (b, v1, v2, t1, t2, e1, e2) -> (* Following lec12 recursive func steps *)
                                             (* Extend environment, environment record with placeholder*)
                                             let (env_record1) = extend_with_placeholder env_record v1 in
                                             (* Compile function with extended env. record *)
                                             let name = new_mangle v1 in
                                             let (s1, e1', c1) = compile_body env_record1 name v2 t1 e1 in
                                             (* Make closure with placeholder-extended environement *)
                                             let ((s2, e2', c2), env_record2) = extend_env env_record1 v1 e1' in
                                             let (s3, e3', c3) = compile_exp env_record1 e2 in
                                             let ((s4, e4', c4), env_record3) = pop_env env_record1 in 
                                             let temp_var = new_var() in
                                             (* Backpatch environment in closure to point back to closure *)
                                             (* We essentially have to make a new exp assigning the func body and closure env as fields to the variable in the env *)
                                             (* idk how this mess works genuinely but it works *)
                                             s1 @ s2 @ [mk_stmt (Ca.SExp(mk_exp(Ca.EAssign(mk_lhs(Ca.LHField((match e1'.edesc with | Ca.EVar (var, Ca.Local) -> var | _ -> raise Unimplemented), (), clos_env)), mk_exp(Ca.EVar(env_var, Ca.Local))))))] @ s3 @ [mk_stmt (Ca.SDecl (temp_var, compile_typ e2.einfo, Some e3'))] @ s4, mk_exp(Ca.EVar (temp_var, Ca.Local)), c1 @ c2 @ c3 @ c4
    | ELetPair (v1, v2, e1, e2) -> let temp_var = new_var() in
                                   (* Since env follows a stack, x, y is push to the stack individually *)
                                   let (s1, e1', c1) = compile_exp env_record e1 in
                                   (* extending environment allows the expression of the pair *)
                                   (* Make struct field for pair in C which would be like (x,y) = pair_fst: x, pair_snd: y *)
                                   let ((s2, e2', c2), env_record1) = extend_env env_record v1 (mk_exp (Ca.EField (mk_exp (Ca.EVar (temp_var, Ca.Local)), pair_fst))) in
                                   let ((s3, e3', c3), env_record1) = extend_env env_record1 v2 (mk_exp (Ca.EField (mk_exp (Ca.EVar (temp_var, Ca.Local)), pair_snd))) in
                                   let (s4, e4', c4) = compile_exp env_record1 e2 in
                                   (* pop follows what was in the lec10 for environment *)
                                   let ((s5, e5', c5), env_record1) = pop_env env_record1 in
                                   let ((s6, e6', c6), env_record1) = pop_env env_record1 in
                                   let temp_var1 = new_var() in
                                   (*declaration of pairs x, y individually for C*)
                                   s1 @ [mk_stmt (Ca.SDecl (temp_var, compile_typ e1.einfo, Some e1'))] @ s2 @ s3 @ s4 @ [mk_stmt(Ca.SDecl (temp_var1, compile_typ e2.einfo, Some e4'))] @ s5 @ s6, mk_exp(Ca.EVar (temp_var1, Ca.Local)), c1 @ c2 @ c3 @ c4 @ c5 @ c6 
    | EApp (e1, e2) -> let (s1, e1', c1) = compile_exp env_record e1 in
                       let (s2, e2', c2) = compile_exp env_record e2 in
                       (* exp_e1.clos_fun(exp_e2, exp_e1.clos_env) *)
                       (* ((ret_ty(e2_ty))exp_e1.clos_fun) (exp_e2, exp_e1.clos_env)*)
                       let temp_var = new_var() in 
                       (* EField same as exp.field in C. we have to cast this to ret_ty(e2_ty) and seems like the mk_cast wrapper does that *)
                       let (e3') = mk_exp (Ca.ECall (mk_cast (Ca.EField (e1', clos_fun)) (Ca.TFunction (compile_typ e.einfo, [(compile_typ e2.einfo, ""); (env_type, env_var)])), [e2'; mk_exp (Ca.EField (e1', clos_env))])) in
                       s1 @ s2, e3', c1 @ c2
    | EMatchList (e1, e2, v1, v2, e3) -> let (s1, e1', c1) = compile_exp env_record e1 in
                                         let (s2, e2', c2) = compile_exp env_record e2 in
                                         let temp_var = new_var () in
                                         let temp_var1 = new_var () in
                                         (* instructions for obtaining list head & tail *)
                                         (* Need to cast for head as shown for ECons and project writeup *)
                                         let head_exp = (mk_cast (Ca.EField (mk_exp (Ca.EVar (temp_var1, Ca.Local)), list_hd)) (compile_typ (match e1.einfo with| TList a -> a | _ -> raise Unimplemented))) in
                                         let tail_exp = mk_exp (Ca.EField (mk_exp(Ca.EVar (temp_var1, Ca.Local)), list_tl)) in
                                         let ((s3, e3', c3), env_record1) = extend_env env_record v1 (head_exp) in
                                         let ((s4, e4', c4), env_record1) = extend_env env_record1 v2 (tail_exp) in
                                         let (s5, e5', c5) = compile_exp env_record1 e3 in
                                         (* pop envs after compiling e3 *)
                                         let ((s6, e6', c6), env_record1) = pop_env env_record1 in
                                         let ((s7, e6', c7), env_record1) = pop_env env_record1 in
                                        (* instructions for each case *)
                                         let nil_branch = mk_stmt (Ca.SBlock (s2 @ [mk_assign_s temp_var e2'])) in
                                         let cons_branch = mk_stmt (Ca.SBlock (s3 @ s4 @ s5 @ [mk_assign_s temp_var e5'] @ s6 @ s7)) in
                                         s1
                                         (* Declare match for Some and None *)
                                         @ [mk_stmt (Ca.SDecl (temp_var1, compile_typ e1.einfo, Some e1'))]
                                         @ [mk_stmt (Ca.SDecl (temp_var, compile_typ e2.einfo, None))]
                                         (* run nill branch if first expression evaluates to nil, cons branch otherwise *)
                                         @ [mk_stmt (Ca.SIf (mk_exp (Ca.EBinop (Ca.BEq, mk_cast_e e1' Ca.TInt, mk_exp (Ca.EConst (Ca.CInt 0)))), nil_branch, cons_branch))],
                                         mk_exp (Ca.EVar (temp_var, Ca.Local)),
                                         c1 @ c2 @ c3 @ c4 @ c5 @ c6 @ c7
    | EPair (e1, e2) -> (* struct pair includes pair_fst pair_snd. we init pair with those two of type int *)
                        let (s1, e1', c1) = compile_exp env_record e1 in
                        let (s2, e2', c2) = compile_exp env_record e2 in
                                                                    (* ECast seems to work for int var ? im not entirely sure *)
                        let (s3, e3', c3) = init_struct pair_struct [(pair_fst, mk_exp(Ca.ECast (e1', Ca.TInt))); (pair_snd, mk_exp(Ca.ECast (e2', Ca.TInt)))] in
                        s1 @ s2 @ s3, e3', c1 @ c2 @ c3
    | ECons (e1, e2) -> (* same idea as pair, but list_tl is type __list and list_hd is int *)
                        let (s1, e1', c1) = compile_exp env_record e1 in
                        let (s2, e2', c2) = compile_exp env_record e2 in
                        let (s3, e3', c3) = init_struct list_struct [(list_hd, mk_exp(Ca.ECast(e1', Ca.TInt))); (list_tl, e2')] in
                        s1 @ s2 @ s3, e3', c1 @ c2 @ c3
    | EAnnot (e, _) -> compile_exp env_record e
let lib_structs =
  [(Ca.DStructDef (list_struct, list_fields));
   (Ca.DStructDef (pair_struct, pair_fields));
   (Ca.DStructDef (clos_struct, clos_fields))
  ]
let lib_structs = List.map (fun d -> Ca.mk_def d dummy_loc) lib_structs
                     
let compile_prog p =
  let (mainis, maine, c) = compile_exp [] p in
  let mainis =
    (Ca.mk_stmt
       (Ca.SDecl (env_var, Ca.TStruct list_struct, Some (compile_nil)))
       dummy_loc)
    ::mainis
  in
  let adddefs (sds, fds) c =
    let tfun = Ca.TFunction (c.cret,
                            [(snd c.cparam, fst c.cparam);
                             (env_type, env_var)])
    in
    (sds,
     (Ca.mk_def (Ca.DFun (c.cname,
                          tfun,
                          c.cbody))
        dummy_loc)::fds
    )
  in
  let main =
    Ca.mk_def
      (Ca.DFun ("main",
               Ca.TFunction (compile_typ p.einfo, []),
               mk_stmt (Ca.SBlock (mainis @ [mk_stmt
                                              (Ca.SReturn (Some maine))]))
      ))
      dummy_loc
  in

  let (sds, fds) = List.fold_left adddefs (lib_structs, []) c in
  sds @ [lookup] @ fds @ [main]
