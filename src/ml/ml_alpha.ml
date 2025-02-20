open Ml_ast

exception UnboundVar of string * loc
   
let rec alpha (env: string Varmap.t) (e: 'a exp) =
  {e with
    edesc =
      match e.edesc with
      | EVar v ->
         (try EVar (Varmap.find v env)
          with Not_found -> raise (UnboundVar (v, e.eloc)))
      | EConst c -> EConst c
      | EBinop (b, e1, e2) ->
         EBinop (b, alpha env e1, alpha env e2)
      | EUnop (u, e) -> EUnop (u, alpha env e)
      | EFun (x, t, e) ->
         let newx = new_mangle x in
         let env' = Varmap.add x newx env in
         EFun (newx, t, alpha env' e)
      | EIf (e1, e2, e3) -> EIf (alpha env e1, alpha env e2, alpha env e3)
      | ELet (x, t, e1, e2) ->
         let newx = new_mangle x in
         let env' = Varmap.add x newx env in
         ELet (newx, t, alpha env e1, alpha env' e2)
      | ELetFun (is_rec, f, x, rt, tx, e1, e2) ->
         let newf = new_mangle f in
         let newx = new_mangle x in
         ELetFun (is_rec, newf, newx, rt, tx,
                  alpha (Varmap.add x newx (if is_rec then Varmap.add f newf env
                                            else env))
                    e1,
                  alpha (Varmap.add f newf env) e2)
      | ELetPair (x, y, e1, e2) ->
         let newx = new_mangle x in
         let newy = new_mangle y in
         let env' = Varmap.add x newx (Varmap.add y newy env) in
         ELetPair (newx, newy, alpha env e1, alpha env' e2)
      | EApp (e1, e2) -> EApp (alpha env e1, alpha env e2)
      | EMatchList (e1, e2, h, t, e3) ->
         let newh = new_mangle h in
         let newt = new_mangle t in
         let env' = Varmap.add h newh (Varmap.add t newt env) in
         EMatchList (alpha env e1, alpha env e2, newh, newt, alpha env' e3)
      | EPair (e1, e2) -> EPair (alpha env e1, alpha env e2)
      | ECons (e1, e2) -> ECons (alpha env e1, alpha env e2)
      | EAnnot (e, t) -> EAnnot (alpha env e, t)
  }
       
let alpha_prog e = alpha Varmap.empty e
