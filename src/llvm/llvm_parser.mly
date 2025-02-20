%{
    open Llvm_ast
    open Lexing
    open Llvm_lexer

    exception SyntaxError

    let syn_err = Llvm_ast.syn_err SyntaxError

    let mk_name s = "p" ^ s
		
    let mk_args loc ops =
      List.map
	(function (t, Var (Local v)) -> (t, v)
		| (_, _) ->
		   syn_err "Argument must be local var" loc
	)
        ops

    let bop_of_s loc =
      function
      | "add" -> BAdd
      | "sub" -> BSub
      | "mul" -> BMul
      | "sdiv" -> BDiv
      | "and" -> BAnd
      | "or" -> BOr
      | "xor" -> BXor
      | _ -> syn_err "Invalid opcode" loc

    let cmp_of_s loc =
      function
      | "eq" -> CEq
      | "ne" -> CNe
      | "sgt" -> CSGt
      | "sge" -> CSGe
      | "slt" -> CSLt
      | "sle" -> CSLe
      | _ -> syn_err "Invalid comparison" loc

    let cast_of_s loc =
      function
      | "bitcast" -> CBitcast
      | "zext" -> CZext
      | "ptrtoint" -> CPtrtoint
      | "inttoptr" -> CInttoptr
      | _ -> syn_err "Invalid opcode" loc
%}



%start prog
%type <Llvm_ast.prog> prog
%type <Llvm_ast.func option> func
%type <Llvm_ast.inst> inst
%type <Llvm_ast.inst list> insts
%type <Llvm_ast.value> value
%type <Llvm_ast.typ * Llvm_ast.value> op
%type <(Llvm_ast.typ * Llvm_ast.value) list> ops
%type <(Llvm_ast.typ * Llvm_ast.value) list> maybeops
%type <Llvm_ast.label * Llvm_ast.value> pred
%type <(Llvm_ast.label * Llvm_ast.value) list> brackops
%type <Llvm_ast.var> var
%type <Llvm_ast.typ> typ
%type <Llvm_ast.typ list> typs
%type <Llvm_ast.typ> unchecked_typ
%type <Llvm_ast.typ list> unchecked_typs
%type <Llvm_ast.bop> binop
%type <Llvm_ast.cast> cast
%type <unit> typedef

%%

typedef:
  | t=LVAR; EQUAL; TYP; LBRACE; fs=unchecked_typs; RBRACE
  { structs := Varmap.add t fs (!structs) }
;

prog:
  | EOF { [] }
  | typedef; p=prog
    { p }
  | f=func; p=prog { (match f with Some f -> f::p | None -> p) }
  | error  { syn_err "Invalid declaration/definition" $loc }
;

func:
  | DECLARE; rt=typ; f=GVAR; LPAREN; typs; RPAREN { None }
  | DECLARE error { syn_err "Invalid function declaration" $loc }
  | DEFINE; rt=typ; f=GVAR; LPAREN; args=maybeops; RPAREN; LBRACE; b=insts; RBRACE
    { Some (make_func f rt (mk_args $loc args) b)  }
;

binop:
  | ADD { BAdd}
  | SUB { BSub }
  | MUL { BMul }
  | DIV { BDiv }
  | AND { BAnd }
  | OR { BOr }
  | XOR { BXor }
;

cast:
  | BITCAST { CBitcast }
  | PTRTOINT { CPtrtoint }
  | INTTOPTR { CInttoptr }
  | ZEXT { CZext }
  | TRUNC { CTrunc }
;

inst:
  | l=KEYWORD; COLON { ILabel l }
  | d=var; EQUAL; b=binop; t=typ; v1=value; COMMA; v2=value
    { IBinop (d, b, t, v1, v2)
    }
  | STORE; t=typ; va=value; COMMA; typ; vr=var
    { IStore (t, va, vr)
    }
  | BR; LABEL; l=LVAR { IBr l }
  | BR; typ; v=value; COMMA; LABEL; l1=LVAR; COMMA; LABEL; l2=LVAR
    { ICondBr (v, l1, l2)
    }
  | RET; t=typ; v=value
    { IRet (Some (t, v)) }
  | RET { IRet None }
  | d=var; EQUAL; ICMP; c=KEYWORD; t=typ; v1=value; COMMA; v2=value
    { ICmp (d, cmp_of_s $loc c, t, v1, v2)
    }
  | d=var; EQUAL; c=cast; t1=typ; v=value; TO; t=typ
    { ICast (d, c, t1, v, t)
    }
  | d=var; EQUAL; CALL; t=typ; f=var; LPAREN; args=maybeops; RPAREN
    { ICall (d, t, f, args) }
  | d=var; EQUAL; GETELEMENTPTR; t=typ; COMMA; args=ops
    { match args with
      | (_, Var v)::args ->
	 IGetElementPtr (d, t, v, args)
      | _ -> syn_err "Invalid instruction" $loc
    }
  | d=var; EQUAL; ALLOCA; t=typ
    { IAlloca (d, t, 1) }
  | d=var; EQUAL; ALLOCA; t=typ; COMMA; typ; v=value;
    { match v with
      | Const n -> IAlloca (d, t, n)
      | _ -> syn_err "Invalid instruction" $loc
    }
  | d=var; EQUAL; LOAD; t=typ; COMMA; typ; v=var
    { ILoad (d, t, v)
    }
  | d=var; EQUAL; PHI; t=typ; preds=brackops
    { should_be_ssa := true; IPhi (d, t, preds) }
  | error { syn_err "Invalid instruction" $loc }
;

insts:
  |                      { [] }
  | i=inst; is=insts { i::is }
;

var:
  | v=LVAR { Local (mk_name v) }
  | v=GVAR { Global v }
;

value:
  | v=var     { Var v }
  | n=CONST   { Const n }
  | NULL      { Const 0 }
;

op:
  | t=typ; v=value     { (t, v) }

;

ops:
  | o=op                { [o] }
  | o=op; COMMA; os=ops { o::os }
;

maybeops:
  |        { [] }
  | o=ops  { o }
;

pred:
  | LSQ; v=value; COMMA; l=LVAR; RSQ { (l, v) }
;

brackops:
  | p=pred                    { [p] }
  | p=pred; COMMA; ps=brackops { p::ps }
;

typ:
  | rt=typ; LPAREN; args=typs; RPAREN
    { TFunction (rt, args) }
  | t=typ; STAR
    { TPointer t }
  | TVOID { TVoid }
  | n=TINT { TInteger n }
  | t=TSTRUCT { TStruct t }
;

unchecked_typ:
  | rt=unchecked_typ; LPAREN; args=unchecked_typs; RPAREN
    { TFunction (rt, args) }
  | t=unchecked_typ; STAR
    { TPointer t }
  | TVOID { TVoid }
  | n=TINT { TInteger n }
  | t=TSTRUCT { TStruct t }
  | t=LVAR { TStruct t }
;


typs:
  |                       { [] }
  | t=typ                 { [t] }
  | t=typ; COMMA; ts=typs { t::ts }
;

unchecked_typs:
  |                       { [] }
  | t=unchecked_typ       { [t] }
  | t=unchecked_typ; COMMA; ts=unchecked_typs { t::ts }
;
