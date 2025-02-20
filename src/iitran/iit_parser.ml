
module MenhirBasics = struct
  
  exception Error
  
  let _eRR =
    fun _s ->
      raise Error
  
  type token = Iit_tokens.token
  
end

include MenhirBasics

# 1 "src/iitran/iit_parser.mly"
  
    open Iit_ast
    open Lexing
    open Iit_lexer

    let syn_err = Iit_ast.syn_err SyntaxError

# 25 "src/iitran/iit_parser.ml"

type ('s, 'r) _menhir_state = 
  | MenhirState000 : ('s, _menhir_box_prog) _menhir_state
    (** State 000.
        Stack shape : .
        Start symbol: prog. *)

  | MenhirState004 : (('s, _menhir_box_prog) _menhir_cell1_typ, _menhir_box_prog) _menhir_state
    (** State 004.
        Stack shape : typ.
        Start symbol: prog. *)

  | MenhirState006 : (('s, _menhir_box_prog) _menhir_cell1_IDENT, _menhir_box_prog) _menhir_state
    (** State 006.
        Stack shape : IDENT.
        Start symbol: prog. *)

  | MenhirState010 : (('s, _menhir_box_prog) _menhir_cell1_decllist, _menhir_box_prog) _menhir_state
    (** State 010.
        Stack shape : decllist.
        Start symbol: prog. *)

  | MenhirState011 : (('s, 'r) _menhir_cell1_WHILE, 'r) _menhir_state
    (** State 011.
        Stack shape : WHILE.
        Start symbol: <undetermined>. *)

  | MenhirState012 : (('s, 'r) _menhir_cell1_NOT, 'r) _menhir_state
    (** State 012.
        Stack shape : NOT.
        Start symbol: <undetermined>. *)

  | MenhirState013 : (('s, 'r) _menhir_cell1_NEG, 'r) _menhir_state
    (** State 013.
        Stack shape : NEG.
        Start symbol: <undetermined>. *)

  | MenhirState014 : (('s, 'r) _menhir_cell1_LPAREN, 'r) _menhir_state
    (** State 014.
        Stack shape : LPAREN.
        Start symbol: <undetermined>. *)

  | MenhirState017 : (('s, 'r) _menhir_cell1_CLG, 'r) _menhir_state
    (** State 017.
        Stack shape : CLG.
        Start symbol: <undetermined>. *)

  | MenhirState018 : (('s, 'r) _menhir_cell1_CINT, 'r) _menhir_state
    (** State 018.
        Stack shape : CINT.
        Start symbol: <undetermined>. *)

  | MenhirState020 : (('s, 'r) _menhir_cell1_CCHAR, 'r) _menhir_state
    (** State 020.
        Stack shape : CCHAR.
        Start symbol: <undetermined>. *)

  | MenhirState026 : ((('s, 'r) _menhir_cell1_LPAREN, 'r) _menhir_cell1_expr, 'r) _menhir_state
    (** State 026.
        Stack shape : LPAREN expr.
        Start symbol: <undetermined>. *)

  | MenhirState027 : ((('s, 'r) _menhir_cell1_expr, 'r) _menhir_cell1_TIMES, 'r) _menhir_state
    (** State 027.
        Stack shape : expr TIMES.
        Start symbol: <undetermined>. *)

  | MenhirState030 : ((('s, 'r) _menhir_cell1_expr, 'r) _menhir_cell1_PLUS, 'r) _menhir_state
    (** State 030.
        Stack shape : expr PLUS.
        Start symbol: <undetermined>. *)

  | MenhirState031 : (((('s, 'r) _menhir_cell1_expr, 'r) _menhir_cell1_PLUS, 'r) _menhir_cell1_expr, 'r) _menhir_state
    (** State 031.
        Stack shape : expr PLUS expr.
        Start symbol: <undetermined>. *)

  | MenhirState032 : ((('s, 'r) _menhir_cell1_expr, 'r) _menhir_cell1_DIV, 'r) _menhir_state
    (** State 032.
        Stack shape : expr DIV.
        Start symbol: <undetermined>. *)

  | MenhirState034 : ((('s, 'r) _menhir_cell1_expr, 'r) _menhir_cell1_OR, 'r) _menhir_state
    (** State 034.
        Stack shape : expr OR.
        Start symbol: <undetermined>. *)

  | MenhirState035 : (((('s, 'r) _menhir_cell1_expr, 'r) _menhir_cell1_OR, 'r) _menhir_cell1_expr, 'r) _menhir_state
    (** State 035.
        Stack shape : expr OR expr.
        Start symbol: <undetermined>. *)

  | MenhirState036 : ((('s, 'r) _menhir_cell1_expr, 'r) _menhir_cell1_NE, 'r) _menhir_state
    (** State 036.
        Stack shape : expr NE.
        Start symbol: <undetermined>. *)

  | MenhirState037 : (((('s, 'r) _menhir_cell1_expr, 'r) _menhir_cell1_NE, 'r) _menhir_cell1_expr, 'r) _menhir_state
    (** State 037.
        Stack shape : expr NE expr.
        Start symbol: <undetermined>. *)

  | MenhirState038 : ((('s, 'r) _menhir_cell1_expr, 'r) _menhir_cell1_MINUS, 'r) _menhir_state
    (** State 038.
        Stack shape : expr MINUS.
        Start symbol: <undetermined>. *)

  | MenhirState039 : (((('s, 'r) _menhir_cell1_expr, 'r) _menhir_cell1_MINUS, 'r) _menhir_cell1_expr, 'r) _menhir_state
    (** State 039.
        Stack shape : expr MINUS expr.
        Start symbol: <undetermined>. *)

  | MenhirState040 : ((('s, 'r) _menhir_cell1_expr, 'r) _menhir_cell1_LT, 'r) _menhir_state
    (** State 040.
        Stack shape : expr LT.
        Start symbol: <undetermined>. *)

  | MenhirState041 : (((('s, 'r) _menhir_cell1_expr, 'r) _menhir_cell1_LT, 'r) _menhir_cell1_expr, 'r) _menhir_state
    (** State 041.
        Stack shape : expr LT expr.
        Start symbol: <undetermined>. *)

  | MenhirState042 : ((('s, 'r) _menhir_cell1_expr, 'r) _menhir_cell1_LE, 'r) _menhir_state
    (** State 042.
        Stack shape : expr LE.
        Start symbol: <undetermined>. *)

  | MenhirState043 : (((('s, 'r) _menhir_cell1_expr, 'r) _menhir_cell1_LE, 'r) _menhir_cell1_expr, 'r) _menhir_state
    (** State 043.
        Stack shape : expr LE expr.
        Start symbol: <undetermined>. *)

  | MenhirState044 : ((('s, 'r) _menhir_cell1_expr, 'r) _menhir_cell1_GT, 'r) _menhir_state
    (** State 044.
        Stack shape : expr GT.
        Start symbol: <undetermined>. *)

  | MenhirState045 : (((('s, 'r) _menhir_cell1_expr, 'r) _menhir_cell1_GT, 'r) _menhir_cell1_expr, 'r) _menhir_state
    (** State 045.
        Stack shape : expr GT expr.
        Start symbol: <undetermined>. *)

  | MenhirState046 : ((('s, 'r) _menhir_cell1_expr, 'r) _menhir_cell1_GE, 'r) _menhir_state
    (** State 046.
        Stack shape : expr GE.
        Start symbol: <undetermined>. *)

  | MenhirState047 : (((('s, 'r) _menhir_cell1_expr, 'r) _menhir_cell1_GE, 'r) _menhir_cell1_expr, 'r) _menhir_state
    (** State 047.
        Stack shape : expr GE expr.
        Start symbol: <undetermined>. *)

  | MenhirState048 : ((('s, 'r) _menhir_cell1_expr, 'r) _menhir_cell1_EQUAL, 'r) _menhir_state
    (** State 048.
        Stack shape : expr EQUAL.
        Start symbol: <undetermined>. *)

  | MenhirState049 : (((('s, 'r) _menhir_cell1_expr, 'r) _menhir_cell1_EQUAL, 'r) _menhir_cell1_expr, 'r) _menhir_state
    (** State 049.
        Stack shape : expr EQUAL expr.
        Start symbol: <undetermined>. *)

  | MenhirState050 : ((('s, 'r) _menhir_cell1_expr, 'r) _menhir_cell1_AND, 'r) _menhir_state
    (** State 050.
        Stack shape : expr AND.
        Start symbol: <undetermined>. *)

  | MenhirState051 : (((('s, 'r) _menhir_cell1_expr, 'r) _menhir_cell1_AND, 'r) _menhir_cell1_expr, 'r) _menhir_state
    (** State 051.
        Stack shape : expr AND expr.
        Start symbol: <undetermined>. *)

  | MenhirState052 : ((('s, 'r) _menhir_cell1_expr, 'r) _menhir_cell1_ASSIGN, 'r) _menhir_state
    (** State 052.
        Stack shape : expr ASSIGN.
        Start symbol: <undetermined>. *)

  | MenhirState053 : (((('s, 'r) _menhir_cell1_expr, 'r) _menhir_cell1_ASSIGN, 'r) _menhir_cell1_expr, 'r) _menhir_state
    (** State 053.
        Stack shape : expr ASSIGN expr.
        Start symbol: <undetermined>. *)

  | MenhirState056 : ((('s, 'r) _menhir_cell1_WHILE, 'r) _menhir_cell1_expr, 'r) _menhir_state
    (** State 056.
        Stack shape : WHILE expr.
        Start symbol: <undetermined>. *)

  | MenhirState058 : (('s, 'r) _menhir_cell1_IF, 'r) _menhir_state
    (** State 058.
        Stack shape : IF.
        Start symbol: <undetermined>. *)

  | MenhirState059 : ((('s, 'r) _menhir_cell1_IF, 'r) _menhir_cell1_expr, 'r) _menhir_state
    (** State 059.
        Stack shape : IF expr.
        Start symbol: <undetermined>. *)

  | MenhirState060 : (('s, 'r) _menhir_cell1_DO, 'r) _menhir_state
    (** State 060.
        Stack shape : DO.
        Start symbol: <undetermined>. *)

  | MenhirState064 : (('s, 'r) _menhir_cell1_stmt, 'r) _menhir_state
    (** State 064.
        Stack shape : stmt.
        Start symbol: <undetermined>. *)

  | MenhirState068 : (('s, 'r) _menhir_cell1_expr, 'r) _menhir_state
    (** State 068.
        Stack shape : expr.
        Start symbol: <undetermined>. *)

  | MenhirState074 : (((('s, 'r) _menhir_cell1_IF, 'r) _menhir_cell1_expr, 'r) _menhir_cell1_cstmt, 'r) _menhir_state
    (** State 074.
        Stack shape : IF expr cstmt.
        Start symbol: <undetermined>. *)

  | MenhirState084 : (('s, _menhir_box_prog) _menhir_cell1_decl, _menhir_box_prog) _menhir_state
    (** State 084.
        Stack shape : decl.
        Start symbol: prog. *)

  | MenhirState086 : ('s, _menhir_box_stmt) _menhir_state
    (** State 086.
        Stack shape : .
        Start symbol: stmt. *)

  | MenhirState087 : (('s, _menhir_box_stmt) _menhir_cell1_WHILE, _menhir_box_stmt) _menhir_state
    (** State 087.
        Stack shape : WHILE.
        Start symbol: stmt. *)

  | MenhirState088 : ((('s, _menhir_box_stmt) _menhir_cell1_WHILE, _menhir_box_stmt) _menhir_cell1_expr, _menhir_box_stmt) _menhir_state
    (** State 088.
        Stack shape : WHILE expr.
        Start symbol: stmt. *)

  | MenhirState089 : (('s, _menhir_box_stmt) _menhir_cell1_IF, _menhir_box_stmt) _menhir_state
    (** State 089.
        Stack shape : IF.
        Start symbol: stmt. *)

  | MenhirState090 : ((('s, _menhir_box_stmt) _menhir_cell1_IF, _menhir_box_stmt) _menhir_cell1_expr, _menhir_box_stmt) _menhir_state
    (** State 090.
        Stack shape : IF expr.
        Start symbol: stmt. *)

  | MenhirState097 : (((('s, _menhir_box_stmt) _menhir_cell1_IF, _menhir_box_stmt) _menhir_cell1_expr, _menhir_box_stmt) _menhir_cell1_cstmt, _menhir_box_stmt) _menhir_state
    (** State 097.
        Stack shape : IF expr cstmt.
        Start symbol: stmt. *)


and ('s, 'r) _menhir_cell1_cstmt = 
  | MenhirCell1_cstmt of 's * ('s, 'r) _menhir_state * (unit Iit_ast.stmt) * Lexing.position

and ('s, 'r) _menhir_cell1_decl = 
  | MenhirCell1_decl of 's * ('s, 'r) _menhir_state * (Iit_ast.p_stmt)

and ('s, 'r) _menhir_cell1_decllist = 
  | MenhirCell1_decllist of 's * ('s, 'r) _menhir_state * (Iit_ast.p_stmt list)

and ('s, 'r) _menhir_cell1_expr = 
  | MenhirCell1_expr of 's * ('s, 'r) _menhir_state * (unit Iit_ast.exp) * Lexing.position * Lexing.position

and ('s, 'r) _menhir_cell1_stmt = 
  | MenhirCell1_stmt of 's * ('s, 'r) _menhir_state * (Iit_ast.p_stmt) * Lexing.position

and ('s, 'r) _menhir_cell1_typ = 
  | MenhirCell1_typ of 's * ('s, 'r) _menhir_state * (Iit_ast.typ) * Lexing.position

and ('s, 'r) _menhir_cell1_AND = 
  | MenhirCell1_AND of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_ASSIGN = 
  | MenhirCell1_ASSIGN of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_CCHAR = 
  | MenhirCell1_CCHAR of 's * ('s, 'r) _menhir_state * Lexing.position

and ('s, 'r) _menhir_cell1_CINT = 
  | MenhirCell1_CINT of 's * ('s, 'r) _menhir_state * Lexing.position

and ('s, 'r) _menhir_cell1_CLG = 
  | MenhirCell1_CLG of 's * ('s, 'r) _menhir_state * Lexing.position

and ('s, 'r) _menhir_cell1_DIV = 
  | MenhirCell1_DIV of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_DO = 
  | MenhirCell1_DO of 's * ('s, 'r) _menhir_state * Lexing.position

and ('s, 'r) _menhir_cell1_EQUAL = 
  | MenhirCell1_EQUAL of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_GE = 
  | MenhirCell1_GE of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_GT = 
  | MenhirCell1_GT of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_IDENT = 
  | MenhirCell1_IDENT of 's * ('s, 'r) _menhir_state * (
# 3 "src/iitran/iit_tokens.mly"
       (string)
# 331 "src/iitran/iit_parser.ml"
) * Lexing.position * Lexing.position

and ('s, 'r) _menhir_cell1_IF = 
  | MenhirCell1_IF of 's * ('s, 'r) _menhir_state * Lexing.position

and ('s, 'r) _menhir_cell1_LE = 
  | MenhirCell1_LE of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_LPAREN = 
  | MenhirCell1_LPAREN of 's * ('s, 'r) _menhir_state * Lexing.position

and ('s, 'r) _menhir_cell1_LT = 
  | MenhirCell1_LT of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_MINUS = 
  | MenhirCell1_MINUS of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_NE = 
  | MenhirCell1_NE of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_NEG = 
  | MenhirCell1_NEG of 's * ('s, 'r) _menhir_state * Lexing.position

and ('s, 'r) _menhir_cell1_NOT = 
  | MenhirCell1_NOT of 's * ('s, 'r) _menhir_state * Lexing.position

and ('s, 'r) _menhir_cell1_OR = 
  | MenhirCell1_OR of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_PLUS = 
  | MenhirCell1_PLUS of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_TIMES = 
  | MenhirCell1_TIMES of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_WHILE = 
  | MenhirCell1_WHILE of 's * ('s, 'r) _menhir_state * Lexing.position

and _menhir_box_stmt = 
  | MenhirBox_stmt of (Iit_ast.p_stmt) [@@unboxed]

and _menhir_box_prog = 
  | MenhirBox_prog of (Iit_ast.p_stmt list) [@@unboxed]

let _menhir_action_02 =
  fun _1 ->
    (
# 26 "src/iitran/iit_parser.mly"
                                           ( CChar _1 )
# 381 "src/iitran/iit_parser.ml"
     : (Iit_ast.const))

let _menhir_action_03 =
  fun _1 ->
    (
# 27 "src/iitran/iit_parser.mly"
                                           ( CInt _1 )
# 389 "src/iitran/iit_parser.ml"
     : (Iit_ast.const))

let _menhir_action_04 =
  fun _1 ->
    (
# 104 "src/iitran/iit_parser.mly"
                  ( _1 )
# 397 "src/iitran/iit_parser.ml"
     : (unit Iit_ast.stmt))

let _menhir_action_05 =
  fun _1 ->
    (
# 80 "src/iitran/iit_parser.mly"
                                           ( SExp _1 )
# 405 "src/iitran/iit_parser.ml"
     : (unit Iit_ast.stmt_))

let _menhir_action_06 =
  fun () ->
    (
# 81 "src/iitran/iit_parser.mly"
                                           ( SStop )
# 413 "src/iitran/iit_parser.ml"
     : (unit Iit_ast.stmt_))

let _menhir_action_07 =
  fun _2 ->
    (
# 82 "src/iitran/iit_parser.mly"
                                           ( SDo _2 )
# 421 "src/iitran/iit_parser.ml"
     : (unit Iit_ast.stmt_))

let _menhir_action_08 =
  fun _2 _3 _5 ->
    (
# 83 "src/iitran/iit_parser.mly"
                                           ( SIf (_2, _3, Some _5) )
# 429 "src/iitran/iit_parser.ml"
     : (unit Iit_ast.stmt_))

let _menhir_action_09 =
  fun _2 _3 ->
    (
# 84 "src/iitran/iit_parser.mly"
                                           ( SWhile (_2, _3) )
# 437 "src/iitran/iit_parser.ml"
     : (unit Iit_ast.stmt_))

let _menhir_action_10 =
  fun _1 _2 _endpos__2_ _startpos__1_ ->
    let _endpos = _endpos__2_ in
    let _startpos = _startpos__1_ in
    let _loc = (_startpos, _endpos) in
    (
# 74 "src/iitran/iit_parser.mly"
                                              ( { sdesc = SDecl (_1, _2);
                                               sloc = _loc; }
					   )
# 450 "src/iitran/iit_parser.ml"
     : (Iit_ast.p_stmt))

let _menhir_action_11 =
  fun () ->
    (
# 121 "src/iitran/iit_parser.mly"
                                           ( [] )
# 458 "src/iitran/iit_parser.ml"
     : (Iit_ast.p_stmt list))

let _menhir_action_12 =
  fun _1 _2 ->
    (
# 122 "src/iitran/iit_parser.mly"
                                           ( _1::_2 )
# 466 "src/iitran/iit_parser.ml"
     : (Iit_ast.p_stmt list))

let _menhir_action_13 =
  fun _1 _endpos__1_ _startpos__1_ ->
    let _endpos = _endpos__1_ in
    let _startpos = _startpos__1_ in
    let _loc = (_startpos, _endpos) in
    (
# 55 "src/iitran/iit_parser.mly"
                                       ( { edesc = _1;
                                           eloc = _loc;
					   einfo = ();
					 }
                                       )
# 481 "src/iitran/iit_parser.ml"
     : (unit Iit_ast.exp))

let _menhir_action_14 =
  fun _1 ->
    (
# 31 "src/iitran/iit_parser.mly"
                                           ( EConst _1 )
# 489 "src/iitran/iit_parser.ml"
     : (unit Iit_ast.exp_))

let _menhir_action_15 =
  fun _1 ->
    (
# 32 "src/iitran/iit_parser.mly"
                                           ( EVar (_1) )
# 497 "src/iitran/iit_parser.ml"
     : (unit Iit_ast.exp_))

let _menhir_action_16 =
  fun _1 _3 ->
    (
# 33 "src/iitran/iit_parser.mly"
                                           ( EAssign (_1, _3) )
# 505 "src/iitran/iit_parser.ml"
     : (unit Iit_ast.exp_))

let _menhir_action_17 =
  fun _1 _3 ->
    (
# 34 "src/iitran/iit_parser.mly"
                                           ( EBinop (BAdd, _1, _3) )
# 513 "src/iitran/iit_parser.ml"
     : (unit Iit_ast.exp_))

let _menhir_action_18 =
  fun _1 _3 ->
    (
# 35 "src/iitran/iit_parser.mly"
                                           ( EBinop (BSub, _1, _3) )
# 521 "src/iitran/iit_parser.ml"
     : (unit Iit_ast.exp_))

let _menhir_action_19 =
  fun _1 _3 ->
    (
# 36 "src/iitran/iit_parser.mly"
                                           ( EBinop (BMul, _1, _3) )
# 529 "src/iitran/iit_parser.ml"
     : (unit Iit_ast.exp_))

let _menhir_action_20 =
  fun _1 _3 ->
    (
# 37 "src/iitran/iit_parser.mly"
                                           ( EBinop (BDiv, _1, _3) )
# 537 "src/iitran/iit_parser.ml"
     : (unit Iit_ast.exp_))

let _menhir_action_21 =
  fun _1 _3 ->
    (
# 38 "src/iitran/iit_parser.mly"
                                           ( EBinop (BAnd, _1, _3) )
# 545 "src/iitran/iit_parser.ml"
     : (unit Iit_ast.exp_))

let _menhir_action_22 =
  fun _1 _3 ->
    (
# 39 "src/iitran/iit_parser.mly"
                                           ( EBinop (BOr, _1, _3) )
# 553 "src/iitran/iit_parser.ml"
     : (unit Iit_ast.exp_))

let _menhir_action_23 =
  fun _1 _3 ->
    (
# 40 "src/iitran/iit_parser.mly"
                                           ( EBinop (BLt, _1, _3) )
# 561 "src/iitran/iit_parser.ml"
     : (unit Iit_ast.exp_))

let _menhir_action_24 =
  fun _1 _3 ->
    (
# 41 "src/iitran/iit_parser.mly"
                                           ( EBinop (BLe, _1, _3) )
# 569 "src/iitran/iit_parser.ml"
     : (unit Iit_ast.exp_))

let _menhir_action_25 =
  fun _1 _3 ->
    (
# 42 "src/iitran/iit_parser.mly"
                                           ( EBinop (BGt, _1, _3) )
# 577 "src/iitran/iit_parser.ml"
     : (unit Iit_ast.exp_))

let _menhir_action_26 =
  fun _1 _3 ->
    (
# 43 "src/iitran/iit_parser.mly"
                                           ( EBinop (BGe, _1, _3) )
# 585 "src/iitran/iit_parser.ml"
     : (unit Iit_ast.exp_))

let _menhir_action_27 =
  fun _1 _3 ->
    (
# 44 "src/iitran/iit_parser.mly"
                                           ( EBinop (BNe, _1, _3) )
# 593 "src/iitran/iit_parser.ml"
     : (unit Iit_ast.exp_))

let _menhir_action_28 =
  fun _1 _3 ->
    (
# 45 "src/iitran/iit_parser.mly"
                                           ( EBinop (BEq, _1, _3) )
# 601 "src/iitran/iit_parser.ml"
     : (unit Iit_ast.exp_))

let _menhir_action_29 =
  fun _2 ->
    (
# 46 "src/iitran/iit_parser.mly"
                                           ( EUnop (UNeg, _2) )
# 609 "src/iitran/iit_parser.ml"
     : (unit Iit_ast.exp_))

let _menhir_action_30 =
  fun _2 ->
    (
# 47 "src/iitran/iit_parser.mly"
                                           ( EUnop (UNot, _2) )
# 617 "src/iitran/iit_parser.ml"
     : (unit Iit_ast.exp_))

let _menhir_action_31 =
  fun _2 ->
    (
# 48 "src/iitran/iit_parser.mly"
                                           ( EUnop (UChar, _2) )
# 625 "src/iitran/iit_parser.ml"
     : (unit Iit_ast.exp_))

let _menhir_action_32 =
  fun _2 ->
    (
# 49 "src/iitran/iit_parser.mly"
                                           ( EUnop (ULog, _2) )
# 633 "src/iitran/iit_parser.ml"
     : (unit Iit_ast.exp_))

let _menhir_action_33 =
  fun _2 ->
    (
# 50 "src/iitran/iit_parser.mly"
                                           ( EUnop (UInt, _2) )
# 641 "src/iitran/iit_parser.ml"
     : (unit Iit_ast.exp_))

let _menhir_action_34 =
  fun _2 ->
    (
# 51 "src/iitran/iit_parser.mly"
                                           ( _2.edesc )
# 649 "src/iitran/iit_parser.ml"
     : (unit Iit_ast.exp_))

let _menhir_action_35 =
  fun _1 _endpos__1_ _startpos__1_ ->
    let _endpos = _endpos__1_ in
    let _startpos = _startpos__1_ in
    let _loc = (_startpos, _endpos) in
    (
# 98 "src/iitran/iit_parser.mly"
                                            ( { sdesc = _1;
                                               sloc = _loc; }
                                            )
# 662 "src/iitran/iit_parser.ml"
     : (unit Iit_ast.stmt))

let _menhir_action_36 =
  fun _1 _endpos__1_ _startpos__1_ ->
    let _endpos = _endpos__1_ in
    let _startpos = _startpos__1_ in
    let _loc = (_startpos, _endpos) in
    (
# 98 "src/iitran/iit_parser.mly"
                                            ( { sdesc = _1;
                                               sloc = _loc; }
                                            )
# 675 "src/iitran/iit_parser.ml"
     : (unit Iit_ast.stmt))

let _menhir_action_37 =
  fun _1 _endpos__1_ _startpos__1_ ->
    let _endpos = _endpos__1_ in
    let _startpos = _startpos__1_ in
    let _loc = (_startpos, _endpos) in
    (
# 98 "src/iitran/iit_parser.mly"
                                            ( { sdesc = _1;
                                               sloc = _loc; }
                                            )
# 688 "src/iitran/iit_parser.ml"
     : (Iit_ast.p_stmt))

let _menhir_action_38 =
  fun _1 ->
    (
# 108 "src/iitran/iit_parser.mly"
                  ( _1 )
# 696 "src/iitran/iit_parser.ml"
     : (unit Iit_ast.stmt))

let _menhir_action_39 =
  fun _2 _3 _5 ->
    (
# 88 "src/iitran/iit_parser.mly"
                                           ( SIf (_2, _3, Some _5) )
# 704 "src/iitran/iit_parser.ml"
     : (unit Iit_ast.stmt_))

let _menhir_action_40 =
  fun _2 _3 ->
    (
# 89 "src/iitran/iit_parser.mly"
                                           ( SIf (_2, _3, None) )
# 712 "src/iitran/iit_parser.ml"
     : (unit Iit_ast.stmt_))

let _menhir_action_41 =
  fun _2 _3 ->
    (
# 90 "src/iitran/iit_parser.mly"
                                           ( SWhile (_2, _3) )
# 720 "src/iitran/iit_parser.ml"
     : (unit Iit_ast.stmt_))

let _menhir_action_42 =
  fun _1 _2 ->
    (
# 126 "src/iitran/iit_parser.mly"
                        ( _1 @ _2 )
# 728 "src/iitran/iit_parser.ml"
     : (Iit_ast.p_stmt list))

let _menhir_action_43 =
  fun _1 ->
    (
# 69 "src/iitran/iit_parser.mly"
                                           ( [_1] )
# 736 "src/iitran/iit_parser.ml"
     : (string list))

let _menhir_action_44 =
  fun _1 _3 ->
    (
# 70 "src/iitran/iit_parser.mly"
                                            ( _1::_3 )
# 744 "src/iitran/iit_parser.ml"
     : (string list))

let _menhir_action_45 =
  fun _1 ->
    (
# 112 "src/iitran/iit_parser.mly"
                 ( _1 )
# 752 "src/iitran/iit_parser.ml"
     : (Iit_ast.p_stmt))

let _menhir_action_46 =
  fun _1 ->
    (
# 94 "src/iitran/iit_parser.mly"
                                           ( _1 )
# 760 "src/iitran/iit_parser.ml"
     : (unit Iit_ast.stmt_))

let _menhir_action_47 =
  fun _1 ->
    (
# 95 "src/iitran/iit_parser.mly"
                                           ( _1 )
# 768 "src/iitran/iit_parser.ml"
     : (unit Iit_ast.stmt_))

let _menhir_action_48 =
  fun () ->
    (
# 116 "src/iitran/iit_parser.mly"
                                           ( [] )
# 776 "src/iitran/iit_parser.ml"
     : (unit Iit_ast.stmt list))

let _menhir_action_49 =
  fun _1 _2 ->
    (
# 117 "src/iitran/iit_parser.mly"
                                           ( _1::_2 )
# 784 "src/iitran/iit_parser.ml"
     : (unit Iit_ast.stmt list))

let _menhir_action_50 =
  fun () ->
    (
# 63 "src/iitran/iit_parser.mly"
                                           ( TInteger )
# 792 "src/iitran/iit_parser.ml"
     : (Iit_ast.typ))

let _menhir_action_51 =
  fun () ->
    (
# 64 "src/iitran/iit_parser.mly"
                                           ( TCharacter )
# 800 "src/iitran/iit_parser.ml"
     : (Iit_ast.typ))

let _menhir_action_52 =
  fun () ->
    (
# 65 "src/iitran/iit_parser.mly"
                                           ( TLogical )
# 808 "src/iitran/iit_parser.ml"
     : (Iit_ast.typ))

let _menhir_print_token : token -> string =
  fun _tok ->
    match _tok with
    | Iit_tokens.AND ->
        "AND"
    | Iit_tokens.ASSIGN ->
        "ASSIGN"
    | Iit_tokens.CCHAR ->
        "CCHAR"
    | Iit_tokens.CHAR _ ->
        "CHAR"
    | Iit_tokens.CINT ->
        "CINT"
    | Iit_tokens.CLG ->
        "CLG"
    | Iit_tokens.COMMA ->
        "COMMA"
    | Iit_tokens.DIV ->
        "DIV"
    | Iit_tokens.DO ->
        "DO"
    | Iit_tokens.ELSE ->
        "ELSE"
    | Iit_tokens.END ->
        "END"
    | Iit_tokens.EOF ->
        "EOF"
    | Iit_tokens.EQUAL ->
        "EQUAL"
    | Iit_tokens.GE ->
        "GE"
    | Iit_tokens.GT ->
        "GT"
    | Iit_tokens.IDENT _ ->
        "IDENT"
    | Iit_tokens.IF ->
        "IF"
    | Iit_tokens.INT _ ->
        "INT"
    | Iit_tokens.LE ->
        "LE"
    | Iit_tokens.LPAREN ->
        "LPAREN"
    | Iit_tokens.LT ->
        "LT"
    | Iit_tokens.MINUS ->
        "MINUS"
    | Iit_tokens.NE ->
        "NE"
    | Iit_tokens.NEG ->
        "NEG"
    | Iit_tokens.NOT ->
        "NOT"
    | Iit_tokens.OR ->
        "OR"
    | Iit_tokens.PLUS ->
        "PLUS"
    | Iit_tokens.RPAREN ->
        "RPAREN"
    | Iit_tokens.STOP ->
        "STOP"
    | Iit_tokens.TCHARACTER ->
        "TCHARACTER"
    | Iit_tokens.TIMES ->
        "TIMES"
    | Iit_tokens.TINT ->
        "TINT"
    | Iit_tokens.TLOGICAL ->
        "TLOGICAL"
    | Iit_tokens.WHILE ->
        "WHILE"

let _menhir_fail : unit -> 'a =
  fun () ->
    Printf.eprintf "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

include struct
  
  [@@@ocaml.warning "-4-37"]
  
  let _menhir_run_102 : type  ttv_stack. ttv_stack -> _ -> _menhir_box_stmt =
    fun _menhir_stack _v ->
      MenhirBox_stmt _v
  
  let _menhir_run_082 : type  ttv_stack. (ttv_stack, _menhir_box_prog) _menhir_cell1_decllist -> _ -> _ -> _menhir_box_prog =
    fun _menhir_stack _v _tok ->
      match (_tok : MenhirBasics.token) with
      | Iit_tokens.EOF ->
          let MenhirCell1_decllist (_menhir_stack, _, _1) = _menhir_stack in
          let _2 = _v in
          let _v = _menhir_action_42 _1 _2 in
          MenhirBox_prog _v
      | _ ->
          _eRR ()
  
  let rec _menhir_run_011 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _menhir_stack = MenhirCell1_WHILE (_menhir_stack, _menhir_s, _startpos) in
      let _menhir_s = MenhirState011 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | Iit_tokens.NOT ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Iit_tokens.NEG ->
          _menhir_run_013 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Iit_tokens.LPAREN ->
          _menhir_run_014 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Iit_tokens.INT _v ->
          _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | Iit_tokens.IDENT _v ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | Iit_tokens.CLG ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Iit_tokens.CINT ->
          _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Iit_tokens.CHAR _v ->
          _menhir_run_019 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | Iit_tokens.CCHAR ->
          _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_012 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _menhir_stack = MenhirCell1_NOT (_menhir_stack, _menhir_s, _startpos) in
      let _menhir_s = MenhirState012 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | Iit_tokens.NOT ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Iit_tokens.NEG ->
          _menhir_run_013 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Iit_tokens.LPAREN ->
          _menhir_run_014 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Iit_tokens.INT _v ->
          _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | Iit_tokens.IDENT _v ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | Iit_tokens.CLG ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Iit_tokens.CINT ->
          _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Iit_tokens.CHAR _v ->
          _menhir_run_019 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | Iit_tokens.CCHAR ->
          _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_013 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _menhir_stack = MenhirCell1_NEG (_menhir_stack, _menhir_s, _startpos) in
      let _menhir_s = MenhirState013 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | Iit_tokens.NOT ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Iit_tokens.NEG ->
          _menhir_run_013 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Iit_tokens.LPAREN ->
          _menhir_run_014 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Iit_tokens.INT _v ->
          _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | Iit_tokens.IDENT _v ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | Iit_tokens.CLG ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Iit_tokens.CINT ->
          _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Iit_tokens.CHAR _v ->
          _menhir_run_019 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | Iit_tokens.CCHAR ->
          _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_014 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _menhir_stack = MenhirCell1_LPAREN (_menhir_stack, _menhir_s, _startpos) in
      let _menhir_s = MenhirState014 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | Iit_tokens.NOT ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Iit_tokens.NEG ->
          _menhir_run_013 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Iit_tokens.LPAREN ->
          _menhir_run_014 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Iit_tokens.INT _v ->
          _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | Iit_tokens.IDENT _v ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | Iit_tokens.CLG ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Iit_tokens.CINT ->
          _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Iit_tokens.CHAR _v ->
          _menhir_run_019 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | Iit_tokens.CCHAR ->
          _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_015 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
      let _tok = _menhir_lexer _menhir_lexbuf in
      let (_endpos__1_, _startpos__1_, _1) = (_endpos, _startpos, _v) in
      let _v = _menhir_action_03 _1 in
      _menhir_goto_const _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__1_ _startpos__1_ _v _menhir_s _tok
  
  and _menhir_goto_const : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      let (_endpos__1_, _startpos__1_, _1) = (_endpos, _startpos, _v) in
      let _v = _menhir_action_14 _1 in
      _menhir_goto_expr_ _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__1_ _startpos__1_ _v _menhir_s _tok
  
  and _menhir_goto_expr_ : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      let (_endpos__1_, _startpos__1_, _1) = (_endpos, _startpos, _v) in
      let _v = _menhir_action_13 _1 _endpos__1_ _startpos__1_ in
      _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__1_ _startpos__1_ _v _menhir_s _tok
  
  and _menhir_goto_expr : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState089 ->
          _menhir_run_090 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState087 ->
          _menhir_run_088 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState086 ->
          _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState088 ->
          _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState097 ->
          _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState090 ->
          _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState010 ->
          _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState056 ->
          _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState074 ->
          _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState059 ->
          _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState060 ->
          _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState064 ->
          _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState058 ->
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState011 ->
          _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState012 ->
          _menhir_run_055 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | MenhirState013 ->
          _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | MenhirState052 ->
          _menhir_run_053 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState050 ->
          _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState048 ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState046 ->
          _menhir_run_047 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState044 ->
          _menhir_run_045 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState042 ->
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState040 ->
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState038 ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState036 ->
          _menhir_run_037 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState034 ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState032 ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | MenhirState030 ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState027 ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | MenhirState014 ->
          _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState017 ->
          _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | MenhirState018 ->
          _menhir_run_024 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | MenhirState020 ->
          _menhir_run_022 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_090 : type  ttv_stack. ((ttv_stack, _menhir_box_stmt) _menhir_cell1_IF as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, _menhir_box_stmt) _menhir_state -> _ -> _menhir_box_stmt =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
      match (_tok : MenhirBasics.token) with
      | Iit_tokens.WHILE ->
          _menhir_run_087 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState090
      | Iit_tokens.TIMES ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState090
      | Iit_tokens.STOP ->
          _menhir_run_057 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState090
      | Iit_tokens.PLUS ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState090
      | Iit_tokens.OR ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState090
      | Iit_tokens.NOT ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState090
      | Iit_tokens.NEG ->
          _menhir_run_013 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState090
      | Iit_tokens.NE ->
          _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState090
      | Iit_tokens.MINUS ->
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState090
      | Iit_tokens.LT ->
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState090
      | Iit_tokens.LPAREN ->
          _menhir_run_014 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState090
      | Iit_tokens.LE ->
          _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState090
      | Iit_tokens.INT _v_0 ->
          _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState090
      | Iit_tokens.IF ->
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState090
      | Iit_tokens.IDENT _v_1 ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState090
      | Iit_tokens.GT ->
          _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState090
      | Iit_tokens.GE ->
          _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState090
      | Iit_tokens.EQUAL ->
          _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState090
      | Iit_tokens.DO ->
          _menhir_run_060 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState090
      | Iit_tokens.DIV ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState090
      | Iit_tokens.CLG ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState090
      | Iit_tokens.CINT ->
          _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState090
      | Iit_tokens.CHAR _v_2 ->
          _menhir_run_019 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2 MenhirState090
      | Iit_tokens.CCHAR ->
          _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState090
      | Iit_tokens.ASSIGN ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState090
      | Iit_tokens.AND ->
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState090
      | _ ->
          _eRR ()
  
  and _menhir_run_087 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_stmt) _menhir_state -> _menhir_box_stmt =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _menhir_stack = MenhirCell1_WHILE (_menhir_stack, _menhir_s, _startpos) in
      let _menhir_s = MenhirState087 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | Iit_tokens.NOT ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Iit_tokens.NEG ->
          _menhir_run_013 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Iit_tokens.LPAREN ->
          _menhir_run_014 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Iit_tokens.INT _v ->
          _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | Iit_tokens.IDENT _v ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | Iit_tokens.CLG ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Iit_tokens.CINT ->
          _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Iit_tokens.CHAR _v ->
          _menhir_run_019 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | Iit_tokens.CCHAR ->
          _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_016 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
      let _tok = _menhir_lexer _menhir_lexbuf in
      let (_endpos__1_, _startpos__1_, _1) = (_endpos, _startpos, _v) in
      let _v = _menhir_action_15 _1 in
      _menhir_goto_expr_ _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__1_ _startpos__1_ _v _menhir_s _tok
  
  and _menhir_run_017 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _menhir_stack = MenhirCell1_CLG (_menhir_stack, _menhir_s, _startpos) in
      let _menhir_s = MenhirState017 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | Iit_tokens.NOT ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Iit_tokens.NEG ->
          _menhir_run_013 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Iit_tokens.LPAREN ->
          _menhir_run_014 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Iit_tokens.INT _v ->
          _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | Iit_tokens.IDENT _v ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | Iit_tokens.CLG ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Iit_tokens.CINT ->
          _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Iit_tokens.CHAR _v ->
          _menhir_run_019 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | Iit_tokens.CCHAR ->
          _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_018 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _menhir_stack = MenhirCell1_CINT (_menhir_stack, _menhir_s, _startpos) in
      let _menhir_s = MenhirState018 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | Iit_tokens.NOT ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Iit_tokens.NEG ->
          _menhir_run_013 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Iit_tokens.LPAREN ->
          _menhir_run_014 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Iit_tokens.INT _v ->
          _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | Iit_tokens.IDENT _v ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | Iit_tokens.CLG ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Iit_tokens.CINT ->
          _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Iit_tokens.CHAR _v ->
          _menhir_run_019 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | Iit_tokens.CCHAR ->
          _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_019 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
      let _tok = _menhir_lexer _menhir_lexbuf in
      let (_endpos__1_, _startpos__1_, _1) = (_endpos, _startpos, _v) in
      let _v = _menhir_action_02 _1 in
      _menhir_goto_const _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__1_ _startpos__1_ _v _menhir_s _tok
  
  and _menhir_run_020 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _menhir_stack = MenhirCell1_CCHAR (_menhir_stack, _menhir_s, _startpos) in
      let _menhir_s = MenhirState020 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | Iit_tokens.NOT ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Iit_tokens.NEG ->
          _menhir_run_013 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Iit_tokens.LPAREN ->
          _menhir_run_014 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Iit_tokens.INT _v ->
          _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | Iit_tokens.IDENT _v ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | Iit_tokens.CLG ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Iit_tokens.CINT ->
          _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Iit_tokens.CHAR _v ->
          _menhir_run_019 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | Iit_tokens.CCHAR ->
          _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_027 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_expr as 'stack) -> _ -> _ -> ('stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_TIMES (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState027 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | Iit_tokens.NOT ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Iit_tokens.NEG ->
          _menhir_run_013 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Iit_tokens.LPAREN ->
          _menhir_run_014 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Iit_tokens.INT _v ->
          _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | Iit_tokens.IDENT _v ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | Iit_tokens.CLG ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Iit_tokens.CINT ->
          _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Iit_tokens.CHAR _v ->
          _menhir_run_019 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | Iit_tokens.CCHAR ->
          _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_057 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
      let _tok = _menhir_lexer _menhir_lexbuf in
      let (_endpos__1_, _startpos__1_) = (_endpos, _startpos) in
      let _v = _menhir_action_06 () in
      _menhir_goto_cstmt_ _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__1_ _startpos__1_ _v _menhir_s _tok
  
  and _menhir_goto_cstmt_ : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState086 ->
          _menhir_run_103 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState090 ->
          _menhir_run_095 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState088 ->
          _menhir_run_078 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState097 ->
          _menhir_run_078 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState056 ->
          _menhir_run_078 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState074 ->
          _menhir_run_078 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState059 ->
          _menhir_run_072 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState010 ->
          _menhir_run_069 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState060 ->
          _menhir_run_069 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState064 ->
          _menhir_run_069 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_103 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> _ -> _ -> (ttv_stack, _menhir_box_stmt) _menhir_state -> _ -> _menhir_box_stmt =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      let (_endpos__1_, _startpos__1_, _1) = (_endpos, _startpos, _v) in
      let _v = _menhir_action_46 _1 in
      _menhir_goto_stmt_ _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__1_ _startpos__1_ _v _menhir_s _tok
  
  and _menhir_goto_stmt_ : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState086 ->
          _menhir_run_091 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState090 ->
          _menhir_run_091 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState010 ->
          _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState059 ->
          _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState064 ->
          _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState060 ->
          _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_091 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> _ -> _ -> (ttv_stack, _menhir_box_stmt) _menhir_state -> _ -> _menhir_box_stmt =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      let (_endpos__1_, _startpos__1_, _1) = (_endpos, _startpos, _v) in
      let _v = _menhir_action_37 _1 _endpos__1_ _startpos__1_ in
      _menhir_goto_mk_stmt_stmt__ _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__1_ _v _menhir_s _tok
  
  and _menhir_goto_mk_stmt_stmt__ : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState086 ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok
      | MenhirState090 ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok
      | MenhirState010 ->
          _menhir_run_067 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok
      | MenhirState059 ->
          _menhir_run_067 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok
      | MenhirState060 ->
          _menhir_run_067 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok
      | MenhirState064 ->
          _menhir_run_067 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_094 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> _ -> (ttv_stack, _menhir_box_stmt) _menhir_state -> _ -> _menhir_box_stmt =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok ->
      let (_endpos__1_, _1) = (_endpos, _v) in
      let _v = _menhir_action_45 _1 in
      _menhir_goto_stmt _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__1_ _v _menhir_s _tok
  
  and _menhir_goto_stmt : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState086 ->
          _menhir_run_102 _menhir_stack _v
      | MenhirState090 ->
          _menhir_run_092 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | MenhirState059 ->
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | MenhirState010 ->
          _menhir_run_064 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok
      | MenhirState064 ->
          _menhir_run_064 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok
      | MenhirState060 ->
          _menhir_run_064 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_092 : type  ttv_stack. ((ttv_stack, _menhir_box_stmt) _menhir_cell1_IF, _menhir_box_stmt) _menhir_cell1_expr -> _ -> _ -> _ -> _ -> _ -> _menhir_box_stmt =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell1_expr (_menhir_stack, _, _2, _, _) = _menhir_stack in
      let MenhirCell1_IF (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
      let (_3, _endpos__3_) = (_v, _endpos) in
      let _v = _menhir_action_40 _2 _3 in
      _menhir_goto_ostmt_ _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__3_ _startpos__1_ _v _menhir_s _tok
  
  and _menhir_goto_ostmt_ : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState088 ->
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState097 ->
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState086 ->
          _menhir_run_093 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState090 ->
          _menhir_run_093 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState056 ->
          _menhir_run_075 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState074 ->
          _menhir_run_075 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState010 ->
          _menhir_run_066 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState059 ->
          _menhir_run_066 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState060 ->
          _menhir_run_066 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState064 ->
          _menhir_run_066 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_098 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> _ -> _ -> (ttv_stack, _menhir_box_stmt) _menhir_state -> _ -> _menhir_box_stmt =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      let (_endpos__1_, _startpos__1_, _1) = (_endpos, _startpos, _v) in
      let _v = _menhir_action_36 _1 _endpos__1_ _startpos__1_ in
      _menhir_goto_mk_stmt_ostmt__ _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__1_ _v _menhir_s _tok
  
  and _menhir_goto_mk_stmt_ostmt__ : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState088 ->
          _menhir_run_100 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok
      | MenhirState097 ->
          _menhir_run_100 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok
      | MenhirState056 ->
          _menhir_run_077 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok
      | MenhirState074 ->
          _menhir_run_077 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_100 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> _ -> (ttv_stack, _menhir_box_stmt) _menhir_state -> _ -> _menhir_box_stmt =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok ->
      let (_endpos__1_, _1) = (_endpos, _v) in
      let _v = _menhir_action_38 _1 in
      _menhir_goto_ostmt _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__1_ _v _menhir_s _tok
  
  and _menhir_goto_ostmt : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState088 ->
          _menhir_run_101 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | MenhirState097 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | MenhirState056 ->
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | MenhirState074 ->
          _menhir_run_076 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_101 : type  ttv_stack. ((ttv_stack, _menhir_box_stmt) _menhir_cell1_WHILE, _menhir_box_stmt) _menhir_cell1_expr -> _ -> _ -> _ -> _ -> _ -> _menhir_box_stmt =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell1_expr (_menhir_stack, _, _2, _, _) = _menhir_stack in
      let MenhirCell1_WHILE (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
      let (_3, _endpos__3_) = (_v, _endpos) in
      let _v = _menhir_action_41 _2 _3 in
      _menhir_goto_ostmt_ _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__3_ _startpos__1_ _v _menhir_s _tok
  
  and _menhir_run_099 : type  ttv_stack. (((ttv_stack, _menhir_box_stmt) _menhir_cell1_IF, _menhir_box_stmt) _menhir_cell1_expr, _menhir_box_stmt) _menhir_cell1_cstmt -> _ -> _ -> _ -> _ -> _ -> _menhir_box_stmt =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell1_cstmt (_menhir_stack, _, _3, _) = _menhir_stack in
      let MenhirCell1_expr (_menhir_stack, _, _2, _, _) = _menhir_stack in
      let MenhirCell1_IF (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
      let (_endpos__5_, _5) = (_endpos, _v) in
      let _v = _menhir_action_39 _2 _3 _5 in
      _menhir_goto_ostmt_ _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__5_ _startpos__1_ _v _menhir_s _tok
  
  and _menhir_run_080 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_WHILE, ttv_result) _menhir_cell1_expr -> _ -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell1_expr (_menhir_stack, _, _2, _, _) = _menhir_stack in
      let MenhirCell1_WHILE (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
      let (_3, _endpos__3_) = (_v, _endpos) in
      let _v = _menhir_action_41 _2 _3 in
      _menhir_goto_ostmt_ _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__3_ _startpos__1_ _v _menhir_s _tok
  
  and _menhir_run_076 : type  ttv_stack ttv_result. (((ttv_stack, ttv_result) _menhir_cell1_IF, ttv_result) _menhir_cell1_expr, ttv_result) _menhir_cell1_cstmt -> _ -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell1_cstmt (_menhir_stack, _, _3, _) = _menhir_stack in
      let MenhirCell1_expr (_menhir_stack, _, _2, _, _) = _menhir_stack in
      let MenhirCell1_IF (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
      let (_endpos__5_, _5) = (_endpos, _v) in
      let _v = _menhir_action_39 _2 _3 _5 in
      _menhir_goto_ostmt_ _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__5_ _startpos__1_ _v _menhir_s _tok
  
  and _menhir_run_077 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok ->
      let (_endpos__1_, _1) = (_endpos, _v) in
      let _v = _menhir_action_38 _1 in
      _menhir_goto_ostmt _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__1_ _v _menhir_s _tok
  
  and _menhir_run_093 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> _ -> _ -> (ttv_stack, _menhir_box_stmt) _menhir_state -> _ -> _menhir_box_stmt =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      let (_endpos__1_, _startpos__1_, _1) = (_endpos, _startpos, _v) in
      let _v = _menhir_action_47 _1 in
      _menhir_goto_stmt_ _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__1_ _startpos__1_ _v _menhir_s _tok
  
  and _menhir_run_075 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      let (_endpos__1_, _startpos__1_, _1) = (_endpos, _startpos, _v) in
      let _v = _menhir_action_36 _1 _endpos__1_ _startpos__1_ in
      _menhir_goto_mk_stmt_ostmt__ _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__1_ _v _menhir_s _tok
  
  and _menhir_run_066 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      let (_endpos__1_, _startpos__1_, _1) = (_endpos, _startpos, _v) in
      let _v = _menhir_action_47 _1 in
      _menhir_goto_stmt_ _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__1_ _startpos__1_ _v _menhir_s _tok
  
  and _menhir_run_070 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_IF, ttv_result) _menhir_cell1_expr -> _ -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell1_expr (_menhir_stack, _, _2, _, _) = _menhir_stack in
      let MenhirCell1_IF (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
      let (_3, _endpos__3_) = (_v, _endpos) in
      let _v = _menhir_action_40 _2 _3 in
      _menhir_goto_ostmt_ _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__3_ _startpos__1_ _v _menhir_s _tok
  
  and _menhir_run_064 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_stmt (_menhir_stack, _menhir_s, _v, _endpos) in
      match (_tok : MenhirBasics.token) with
      | Iit_tokens.WHILE ->
          _menhir_run_011 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState064
      | Iit_tokens.STOP ->
          _menhir_run_057 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState064
      | Iit_tokens.NOT ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState064
      | Iit_tokens.NEG ->
          _menhir_run_013 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState064
      | Iit_tokens.LPAREN ->
          _menhir_run_014 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState064
      | Iit_tokens.INT _v_0 ->
          _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState064
      | Iit_tokens.IF ->
          _menhir_run_058 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState064
      | Iit_tokens.IDENT _v_1 ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState064
      | Iit_tokens.DO ->
          _menhir_run_060 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState064
      | Iit_tokens.CLG ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState064
      | Iit_tokens.CINT ->
          _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState064
      | Iit_tokens.CHAR _v_2 ->
          _menhir_run_019 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2 MenhirState064
      | Iit_tokens.CCHAR ->
          _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState064
      | Iit_tokens.END | Iit_tokens.EOF ->
          let _v_3 = _menhir_action_48 () in
          _menhir_run_065 _menhir_stack _menhir_lexbuf _menhir_lexer _v_3 _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_058 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _menhir_stack = MenhirCell1_IF (_menhir_stack, _menhir_s, _startpos) in
      let _menhir_s = MenhirState058 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | Iit_tokens.NOT ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Iit_tokens.NEG ->
          _menhir_run_013 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Iit_tokens.LPAREN ->
          _menhir_run_014 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Iit_tokens.INT _v ->
          _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | Iit_tokens.IDENT _v ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | Iit_tokens.CLG ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Iit_tokens.CINT ->
          _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Iit_tokens.CHAR _v ->
          _menhir_run_019 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | Iit_tokens.CCHAR ->
          _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_060 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _menhir_stack = MenhirCell1_DO (_menhir_stack, _menhir_s, _startpos) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | Iit_tokens.WHILE ->
          _menhir_run_011 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState060
      | Iit_tokens.STOP ->
          _menhir_run_057 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState060
      | Iit_tokens.NOT ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState060
      | Iit_tokens.NEG ->
          _menhir_run_013 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState060
      | Iit_tokens.LPAREN ->
          _menhir_run_014 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState060
      | Iit_tokens.INT _v ->
          _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState060
      | Iit_tokens.IF ->
          _menhir_run_058 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState060
      | Iit_tokens.IDENT _v ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState060
      | Iit_tokens.DO ->
          _menhir_run_060 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState060
      | Iit_tokens.CLG ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState060
      | Iit_tokens.CINT ->
          _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState060
      | Iit_tokens.CHAR _v ->
          _menhir_run_019 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState060
      | Iit_tokens.CCHAR ->
          _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState060
      | Iit_tokens.END ->
          let _v = _menhir_action_48 () in
          _menhir_run_061 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_061 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_DO -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      match (_tok : MenhirBasics.token) with
      | Iit_tokens.END ->
          let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_DO (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
          let (_2, _endpos__3_) = (_v, _endpos) in
          let _v = _menhir_action_07 _2 in
          _menhir_goto_cstmt_ _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__3_ _startpos__1_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_065 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_stmt -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_stmt (_menhir_stack, _menhir_s, _1, _) = _menhir_stack in
      let _2 = _v in
      let _v = _menhir_action_49 _1 _2 in
      _menhir_goto_stmtlist _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_stmtlist : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState010 ->
          _menhir_run_082 _menhir_stack _v _tok
      | MenhirState064 ->
          _menhir_run_065 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState060 ->
          _menhir_run_061 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_067 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok ->
      let (_endpos__1_, _1) = (_endpos, _v) in
      let _v = _menhir_action_45 _1 in
      _menhir_goto_stmt _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__1_ _v _menhir_s _tok
  
  and _menhir_run_063 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      let (_endpos__1_, _startpos__1_, _1) = (_endpos, _startpos, _v) in
      let _v = _menhir_action_37 _1 _endpos__1_ _startpos__1_ in
      _menhir_goto_mk_stmt_stmt__ _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__1_ _v _menhir_s _tok
  
  and _menhir_run_095 : type  ttv_stack. (((ttv_stack, _menhir_box_stmt) _menhir_cell1_IF, _menhir_box_stmt) _menhir_cell1_expr as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, _menhir_box_stmt) _menhir_state -> _ -> _menhir_box_stmt =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      let (_endpos__1_, _startpos__1_, _1) = (_endpos, _startpos, _v) in
      let _v = _menhir_action_35 _1 _endpos__1_ _startpos__1_ in
      _menhir_goto_mk_stmt_cstmt__ _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__1_ _v _menhir_s _tok
  
  and _menhir_goto_mk_stmt_cstmt__ : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok ->
      let (_endpos__1_, _1) = (_endpos, _v) in
      let _v = _menhir_action_04 _1 in
      _menhir_goto_cstmt _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__1_ _v _menhir_s _tok
  
  and _menhir_goto_cstmt : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState090 ->
          _menhir_run_096 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok
      | MenhirState088 ->
          _menhir_run_081 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | MenhirState056 ->
          _menhir_run_081 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | MenhirState097 ->
          _menhir_run_079 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | MenhirState074 ->
          _menhir_run_079 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | MenhirState059 ->
          _menhir_run_073 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_096 : type  ttv_stack. (((ttv_stack, _menhir_box_stmt) _menhir_cell1_IF, _menhir_box_stmt) _menhir_cell1_expr as 'stack) -> _ -> _ -> _ -> _ -> ('stack, _menhir_box_stmt) _menhir_state -> _ -> _menhir_box_stmt =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_cstmt (_menhir_stack, _menhir_s, _v, _endpos) in
      match (_tok : MenhirBasics.token) with
      | Iit_tokens.ELSE ->
          let _menhir_s = MenhirState097 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | Iit_tokens.WHILE ->
              _menhir_run_087 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | Iit_tokens.STOP ->
              _menhir_run_057 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | Iit_tokens.NOT ->
              _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | Iit_tokens.NEG ->
              _menhir_run_013 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | Iit_tokens.LPAREN ->
              _menhir_run_014 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | Iit_tokens.INT _v ->
              _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | Iit_tokens.IF ->
              _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | Iit_tokens.IDENT _v ->
              _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | Iit_tokens.DO ->
              _menhir_run_060 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | Iit_tokens.CLG ->
              _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | Iit_tokens.CINT ->
              _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | Iit_tokens.CHAR _v ->
              _menhir_run_019 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | Iit_tokens.CCHAR ->
              _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_089 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_stmt) _menhir_state -> _menhir_box_stmt =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _menhir_stack = MenhirCell1_IF (_menhir_stack, _menhir_s, _startpos) in
      let _menhir_s = MenhirState089 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | Iit_tokens.NOT ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Iit_tokens.NEG ->
          _menhir_run_013 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Iit_tokens.LPAREN ->
          _menhir_run_014 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Iit_tokens.INT _v ->
          _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | Iit_tokens.IDENT _v ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | Iit_tokens.CLG ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Iit_tokens.CINT ->
          _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Iit_tokens.CHAR _v ->
          _menhir_run_019 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | Iit_tokens.CCHAR ->
          _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_081 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_WHILE, ttv_result) _menhir_cell1_expr -> _ -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell1_expr (_menhir_stack, _, _2, _, _) = _menhir_stack in
      let MenhirCell1_WHILE (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
      let (_3, _endpos__3_) = (_v, _endpos) in
      let _v = _menhir_action_09 _2 _3 in
      _menhir_goto_cstmt_ _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__3_ _startpos__1_ _v _menhir_s _tok
  
  and _menhir_run_079 : type  ttv_stack ttv_result. (((ttv_stack, ttv_result) _menhir_cell1_IF, ttv_result) _menhir_cell1_expr, ttv_result) _menhir_cell1_cstmt -> _ -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell1_cstmt (_menhir_stack, _, _3, _) = _menhir_stack in
      let MenhirCell1_expr (_menhir_stack, _, _2, _, _) = _menhir_stack in
      let MenhirCell1_IF (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
      let (_endpos__5_, _5) = (_endpos, _v) in
      let _v = _menhir_action_08 _2 _3 _5 in
      _menhir_goto_cstmt_ _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__5_ _startpos__1_ _v _menhir_s _tok
  
  and _menhir_run_073 : type  ttv_stack ttv_result. (((ttv_stack, ttv_result) _menhir_cell1_IF, ttv_result) _menhir_cell1_expr as 'stack) -> _ -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_cstmt (_menhir_stack, _menhir_s, _v, _endpos) in
      match (_tok : MenhirBasics.token) with
      | Iit_tokens.ELSE ->
          let _menhir_s = MenhirState074 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | Iit_tokens.WHILE ->
              _menhir_run_011 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | Iit_tokens.STOP ->
              _menhir_run_057 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | Iit_tokens.NOT ->
              _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | Iit_tokens.NEG ->
              _menhir_run_013 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | Iit_tokens.LPAREN ->
              _menhir_run_014 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | Iit_tokens.INT _v ->
              _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | Iit_tokens.IF ->
              _menhir_run_058 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | Iit_tokens.IDENT _v ->
              _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | Iit_tokens.DO ->
              _menhir_run_060 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | Iit_tokens.CLG ->
              _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | Iit_tokens.CINT ->
              _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | Iit_tokens.CHAR _v ->
              _menhir_run_019 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | Iit_tokens.CCHAR ->
              _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_078 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      let (_endpos__1_, _startpos__1_, _1) = (_endpos, _startpos, _v) in
      let _v = _menhir_action_35 _1 _endpos__1_ _startpos__1_ in
      _menhir_goto_mk_stmt_cstmt__ _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__1_ _v _menhir_s _tok
  
  and _menhir_run_072 : type  ttv_stack ttv_result. (((ttv_stack, ttv_result) _menhir_cell1_IF, ttv_result) _menhir_cell1_expr as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | Iit_tokens.ELSE ->
          let (_endpos__1_, _startpos__1_, _1) = (_endpos, _startpos, _v) in
          let _v = _menhir_action_35 _1 _endpos__1_ _startpos__1_ in
          _menhir_goto_mk_stmt_cstmt__ _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__1_ _v _menhir_s _tok
      | Iit_tokens.CCHAR | Iit_tokens.CHAR _ | Iit_tokens.CINT | Iit_tokens.CLG | Iit_tokens.DO | Iit_tokens.END | Iit_tokens.EOF | Iit_tokens.IDENT _ | Iit_tokens.IF | Iit_tokens.INT _ | Iit_tokens.LPAREN | Iit_tokens.NEG | Iit_tokens.NOT | Iit_tokens.STOP | Iit_tokens.WHILE ->
          let (_endpos__1_, _startpos__1_, _1) = (_endpos, _startpos, _v) in
          let _v = _menhir_action_46 _1 in
          _menhir_goto_stmt_ _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__1_ _startpos__1_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_069 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      let (_endpos__1_, _startpos__1_, _1) = (_endpos, _startpos, _v) in
      let _v = _menhir_action_46 _1 in
      _menhir_goto_stmt_ _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__1_ _startpos__1_ _v _menhir_s _tok
  
  and _menhir_run_030 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_expr as 'stack) -> _ -> _ -> ('stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_PLUS (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState030 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | Iit_tokens.NOT ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Iit_tokens.NEG ->
          _menhir_run_013 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Iit_tokens.LPAREN ->
          _menhir_run_014 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Iit_tokens.INT _v ->
          _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | Iit_tokens.IDENT _v ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | Iit_tokens.CLG ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Iit_tokens.CINT ->
          _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Iit_tokens.CHAR _v ->
          _menhir_run_019 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | Iit_tokens.CCHAR ->
          _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_034 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_expr as 'stack) -> _ -> _ -> ('stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_OR (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState034 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | Iit_tokens.NOT ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Iit_tokens.NEG ->
          _menhir_run_013 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Iit_tokens.LPAREN ->
          _menhir_run_014 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Iit_tokens.INT _v ->
          _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | Iit_tokens.IDENT _v ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | Iit_tokens.CLG ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Iit_tokens.CINT ->
          _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Iit_tokens.CHAR _v ->
          _menhir_run_019 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | Iit_tokens.CCHAR ->
          _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_036 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_expr as 'stack) -> _ -> _ -> ('stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_NE (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState036 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | Iit_tokens.NOT ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Iit_tokens.NEG ->
          _menhir_run_013 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Iit_tokens.LPAREN ->
          _menhir_run_014 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Iit_tokens.INT _v ->
          _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | Iit_tokens.IDENT _v ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | Iit_tokens.CLG ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Iit_tokens.CINT ->
          _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Iit_tokens.CHAR _v ->
          _menhir_run_019 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | Iit_tokens.CCHAR ->
          _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_038 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_expr as 'stack) -> _ -> _ -> ('stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_MINUS (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState038 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | Iit_tokens.NOT ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Iit_tokens.NEG ->
          _menhir_run_013 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Iit_tokens.LPAREN ->
          _menhir_run_014 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Iit_tokens.INT _v ->
          _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | Iit_tokens.IDENT _v ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | Iit_tokens.CLG ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Iit_tokens.CINT ->
          _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Iit_tokens.CHAR _v ->
          _menhir_run_019 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | Iit_tokens.CCHAR ->
          _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_040 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_expr as 'stack) -> _ -> _ -> ('stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_LT (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState040 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | Iit_tokens.NOT ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Iit_tokens.NEG ->
          _menhir_run_013 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Iit_tokens.LPAREN ->
          _menhir_run_014 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Iit_tokens.INT _v ->
          _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | Iit_tokens.IDENT _v ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | Iit_tokens.CLG ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Iit_tokens.CINT ->
          _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Iit_tokens.CHAR _v ->
          _menhir_run_019 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | Iit_tokens.CCHAR ->
          _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_042 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_expr as 'stack) -> _ -> _ -> ('stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_LE (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState042 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | Iit_tokens.NOT ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Iit_tokens.NEG ->
          _menhir_run_013 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Iit_tokens.LPAREN ->
          _menhir_run_014 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Iit_tokens.INT _v ->
          _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | Iit_tokens.IDENT _v ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | Iit_tokens.CLG ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Iit_tokens.CINT ->
          _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Iit_tokens.CHAR _v ->
          _menhir_run_019 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | Iit_tokens.CCHAR ->
          _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_044 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_expr as 'stack) -> _ -> _ -> ('stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_GT (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState044 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | Iit_tokens.NOT ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Iit_tokens.NEG ->
          _menhir_run_013 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Iit_tokens.LPAREN ->
          _menhir_run_014 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Iit_tokens.INT _v ->
          _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | Iit_tokens.IDENT _v ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | Iit_tokens.CLG ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Iit_tokens.CINT ->
          _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Iit_tokens.CHAR _v ->
          _menhir_run_019 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | Iit_tokens.CCHAR ->
          _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_046 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_expr as 'stack) -> _ -> _ -> ('stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_GE (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState046 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | Iit_tokens.NOT ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Iit_tokens.NEG ->
          _menhir_run_013 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Iit_tokens.LPAREN ->
          _menhir_run_014 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Iit_tokens.INT _v ->
          _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | Iit_tokens.IDENT _v ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | Iit_tokens.CLG ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Iit_tokens.CINT ->
          _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Iit_tokens.CHAR _v ->
          _menhir_run_019 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | Iit_tokens.CCHAR ->
          _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_048 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_expr as 'stack) -> _ -> _ -> ('stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_EQUAL (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState048 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | Iit_tokens.NOT ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Iit_tokens.NEG ->
          _menhir_run_013 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Iit_tokens.LPAREN ->
          _menhir_run_014 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Iit_tokens.INT _v ->
          _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | Iit_tokens.IDENT _v ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | Iit_tokens.CLG ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Iit_tokens.CINT ->
          _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Iit_tokens.CHAR _v ->
          _menhir_run_019 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | Iit_tokens.CCHAR ->
          _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_032 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_expr as 'stack) -> _ -> _ -> ('stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_DIV (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState032 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | Iit_tokens.NOT ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Iit_tokens.NEG ->
          _menhir_run_013 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Iit_tokens.LPAREN ->
          _menhir_run_014 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Iit_tokens.INT _v ->
          _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | Iit_tokens.IDENT _v ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | Iit_tokens.CLG ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Iit_tokens.CINT ->
          _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Iit_tokens.CHAR _v ->
          _menhir_run_019 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | Iit_tokens.CCHAR ->
          _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_052 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_expr as 'stack) -> _ -> _ -> ('stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_ASSIGN (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState052 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | Iit_tokens.NOT ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Iit_tokens.NEG ->
          _menhir_run_013 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Iit_tokens.LPAREN ->
          _menhir_run_014 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Iit_tokens.INT _v ->
          _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | Iit_tokens.IDENT _v ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | Iit_tokens.CLG ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Iit_tokens.CINT ->
          _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Iit_tokens.CHAR _v ->
          _menhir_run_019 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | Iit_tokens.CCHAR ->
          _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_050 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_expr as 'stack) -> _ -> _ -> ('stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_AND (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState050 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | Iit_tokens.NOT ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Iit_tokens.NEG ->
          _menhir_run_013 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Iit_tokens.LPAREN ->
          _menhir_run_014 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Iit_tokens.INT _v ->
          _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | Iit_tokens.IDENT _v ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | Iit_tokens.CLG ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Iit_tokens.CINT ->
          _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Iit_tokens.CHAR _v ->
          _menhir_run_019 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | Iit_tokens.CCHAR ->
          _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_088 : type  ttv_stack. ((ttv_stack, _menhir_box_stmt) _menhir_cell1_WHILE as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, _menhir_box_stmt) _menhir_state -> _ -> _menhir_box_stmt =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
      match (_tok : MenhirBasics.token) with
      | Iit_tokens.WHILE ->
          _menhir_run_087 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState088
      | Iit_tokens.TIMES ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState088
      | Iit_tokens.STOP ->
          _menhir_run_057 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState088
      | Iit_tokens.PLUS ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState088
      | Iit_tokens.OR ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState088
      | Iit_tokens.NOT ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState088
      | Iit_tokens.NEG ->
          _menhir_run_013 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState088
      | Iit_tokens.NE ->
          _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState088
      | Iit_tokens.MINUS ->
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState088
      | Iit_tokens.LT ->
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState088
      | Iit_tokens.LPAREN ->
          _menhir_run_014 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState088
      | Iit_tokens.LE ->
          _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState088
      | Iit_tokens.INT _v_0 ->
          _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState088
      | Iit_tokens.IF ->
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState088
      | Iit_tokens.IDENT _v_1 ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState088
      | Iit_tokens.GT ->
          _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState088
      | Iit_tokens.GE ->
          _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState088
      | Iit_tokens.EQUAL ->
          _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState088
      | Iit_tokens.DO ->
          _menhir_run_060 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState088
      | Iit_tokens.DIV ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState088
      | Iit_tokens.CLG ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState088
      | Iit_tokens.CINT ->
          _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState088
      | Iit_tokens.CHAR _v_2 ->
          _menhir_run_019 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2 MenhirState088
      | Iit_tokens.CCHAR ->
          _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState088
      | Iit_tokens.ASSIGN ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState088
      | Iit_tokens.AND ->
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState088
      | _ ->
          _eRR ()
  
  and _menhir_run_068 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | Iit_tokens.TIMES ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState068
      | Iit_tokens.PLUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState068
      | Iit_tokens.OR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState068
      | Iit_tokens.NE ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState068
      | Iit_tokens.MINUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState068
      | Iit_tokens.LT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState068
      | Iit_tokens.LE ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState068
      | Iit_tokens.GT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState068
      | Iit_tokens.GE ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState068
      | Iit_tokens.EQUAL ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState068
      | Iit_tokens.DIV ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState068
      | Iit_tokens.ASSIGN ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState068
      | Iit_tokens.AND ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState068
      | Iit_tokens.CCHAR | Iit_tokens.CHAR _ | Iit_tokens.CINT | Iit_tokens.CLG | Iit_tokens.DO | Iit_tokens.ELSE | Iit_tokens.END | Iit_tokens.EOF | Iit_tokens.IDENT _ | Iit_tokens.IF | Iit_tokens.INT _ | Iit_tokens.LPAREN | Iit_tokens.NEG | Iit_tokens.NOT | Iit_tokens.STOP | Iit_tokens.WHILE ->
          let (_endpos__1_, _startpos__1_, _1) = (_endpos, _startpos, _v) in
          let _v = _menhir_action_05 _1 in
          _menhir_goto_cstmt_ _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__1_ _startpos__1_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_059 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_IF as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
      match (_tok : MenhirBasics.token) with
      | Iit_tokens.WHILE ->
          _menhir_run_011 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState059
      | Iit_tokens.TIMES ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState059
      | Iit_tokens.STOP ->
          _menhir_run_057 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState059
      | Iit_tokens.PLUS ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState059
      | Iit_tokens.OR ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState059
      | Iit_tokens.NOT ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState059
      | Iit_tokens.NEG ->
          _menhir_run_013 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState059
      | Iit_tokens.NE ->
          _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState059
      | Iit_tokens.MINUS ->
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState059
      | Iit_tokens.LT ->
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState059
      | Iit_tokens.LPAREN ->
          _menhir_run_014 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState059
      | Iit_tokens.LE ->
          _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState059
      | Iit_tokens.INT _v_0 ->
          _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState059
      | Iit_tokens.IF ->
          _menhir_run_058 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState059
      | Iit_tokens.IDENT _v_1 ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState059
      | Iit_tokens.GT ->
          _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState059
      | Iit_tokens.GE ->
          _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState059
      | Iit_tokens.EQUAL ->
          _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState059
      | Iit_tokens.DO ->
          _menhir_run_060 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState059
      | Iit_tokens.DIV ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState059
      | Iit_tokens.CLG ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState059
      | Iit_tokens.CINT ->
          _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState059
      | Iit_tokens.CHAR _v_2 ->
          _menhir_run_019 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2 MenhirState059
      | Iit_tokens.CCHAR ->
          _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState059
      | Iit_tokens.ASSIGN ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState059
      | Iit_tokens.AND ->
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState059
      | _ ->
          _eRR ()
  
  and _menhir_run_056 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_WHILE as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
      match (_tok : MenhirBasics.token) with
      | Iit_tokens.WHILE ->
          _menhir_run_011 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState056
      | Iit_tokens.TIMES ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState056
      | Iit_tokens.STOP ->
          _menhir_run_057 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState056
      | Iit_tokens.PLUS ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState056
      | Iit_tokens.OR ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState056
      | Iit_tokens.NOT ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState056
      | Iit_tokens.NEG ->
          _menhir_run_013 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState056
      | Iit_tokens.NE ->
          _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState056
      | Iit_tokens.MINUS ->
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState056
      | Iit_tokens.LT ->
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState056
      | Iit_tokens.LPAREN ->
          _menhir_run_014 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState056
      | Iit_tokens.LE ->
          _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState056
      | Iit_tokens.INT _v_0 ->
          _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState056
      | Iit_tokens.IF ->
          _menhir_run_058 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState056
      | Iit_tokens.IDENT _v_1 ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState056
      | Iit_tokens.GT ->
          _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState056
      | Iit_tokens.GE ->
          _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState056
      | Iit_tokens.EQUAL ->
          _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState056
      | Iit_tokens.DO ->
          _menhir_run_060 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState056
      | Iit_tokens.DIV ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState056
      | Iit_tokens.CLG ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState056
      | Iit_tokens.CINT ->
          _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState056
      | Iit_tokens.CHAR _v_2 ->
          _menhir_run_019 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2 MenhirState056
      | Iit_tokens.CCHAR ->
          _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState056
      | Iit_tokens.ASSIGN ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState056
      | Iit_tokens.AND ->
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState056
      | _ ->
          _eRR ()
  
  and _menhir_run_055 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_NOT -> _ -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell1_NOT (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
      let (_endpos__2_, _2) = (_endpos, _v) in
      let _v = _menhir_action_30 _2 in
      _menhir_goto_expr_ _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__2_ _startpos__1_ _v _menhir_s _tok
  
  and _menhir_run_054 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_NEG -> _ -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell1_NEG (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
      let (_endpos__2_, _2) = (_endpos, _v) in
      let _v = _menhir_action_29 _2 in
      _menhir_goto_expr_ _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__2_ _startpos__1_ _v _menhir_s _tok
  
  and _menhir_run_053 : type  ttv_stack ttv_result. (((ttv_stack, ttv_result) _menhir_cell1_expr, ttv_result) _menhir_cell1_ASSIGN as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | Iit_tokens.TIMES ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState053
      | Iit_tokens.PLUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState053
      | Iit_tokens.OR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState053
      | Iit_tokens.NE ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState053
      | Iit_tokens.MINUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState053
      | Iit_tokens.LT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState053
      | Iit_tokens.LE ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState053
      | Iit_tokens.GT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState053
      | Iit_tokens.GE ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState053
      | Iit_tokens.EQUAL ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState053
      | Iit_tokens.DIV ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState053
      | Iit_tokens.ASSIGN ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState053
      | Iit_tokens.AND ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState053
      | Iit_tokens.CCHAR | Iit_tokens.CHAR _ | Iit_tokens.CINT | Iit_tokens.CLG | Iit_tokens.DO | Iit_tokens.ELSE | Iit_tokens.END | Iit_tokens.EOF | Iit_tokens.IDENT _ | Iit_tokens.IF | Iit_tokens.INT _ | Iit_tokens.LPAREN | Iit_tokens.NEG | Iit_tokens.NOT | Iit_tokens.RPAREN | Iit_tokens.STOP | Iit_tokens.WHILE ->
          let MenhirCell1_ASSIGN (_menhir_stack, _) = _menhir_stack in
          let MenhirCell1_expr (_menhir_stack, _menhir_s, _1, _startpos__1_, _) = _menhir_stack in
          let (_3, _endpos__3_) = (_v, _endpos) in
          let _v = _menhir_action_16 _1 _3 in
          _menhir_goto_expr_ _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__3_ _startpos__1_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_051 : type  ttv_stack ttv_result. (((ttv_stack, ttv_result) _menhir_cell1_expr, ttv_result) _menhir_cell1_AND as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | Iit_tokens.TIMES ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState051
      | Iit_tokens.PLUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState051
      | Iit_tokens.NE ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState051
      | Iit_tokens.MINUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState051
      | Iit_tokens.LT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState051
      | Iit_tokens.LE ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState051
      | Iit_tokens.GT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState051
      | Iit_tokens.GE ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState051
      | Iit_tokens.EQUAL ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState051
      | Iit_tokens.DIV ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState051
      | Iit_tokens.AND | Iit_tokens.ASSIGN | Iit_tokens.CCHAR | Iit_tokens.CHAR _ | Iit_tokens.CINT | Iit_tokens.CLG | Iit_tokens.DO | Iit_tokens.ELSE | Iit_tokens.END | Iit_tokens.EOF | Iit_tokens.IDENT _ | Iit_tokens.IF | Iit_tokens.INT _ | Iit_tokens.LPAREN | Iit_tokens.NEG | Iit_tokens.NOT | Iit_tokens.OR | Iit_tokens.RPAREN | Iit_tokens.STOP | Iit_tokens.WHILE ->
          let MenhirCell1_AND (_menhir_stack, _) = _menhir_stack in
          let MenhirCell1_expr (_menhir_stack, _menhir_s, _1, _startpos__1_, _) = _menhir_stack in
          let (_3, _endpos__3_) = (_v, _endpos) in
          let _v = _menhir_action_21 _1 _3 in
          _menhir_goto_expr_ _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__3_ _startpos__1_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_049 : type  ttv_stack ttv_result. (((ttv_stack, ttv_result) _menhir_cell1_expr, ttv_result) _menhir_cell1_EQUAL as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | Iit_tokens.TIMES ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState049
      | Iit_tokens.PLUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState049
      | Iit_tokens.MINUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState049
      | Iit_tokens.DIV ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState049
      | Iit_tokens.AND | Iit_tokens.ASSIGN | Iit_tokens.CCHAR | Iit_tokens.CHAR _ | Iit_tokens.CINT | Iit_tokens.CLG | Iit_tokens.DO | Iit_tokens.ELSE | Iit_tokens.END | Iit_tokens.EOF | Iit_tokens.EQUAL | Iit_tokens.GE | Iit_tokens.GT | Iit_tokens.IDENT _ | Iit_tokens.IF | Iit_tokens.INT _ | Iit_tokens.LE | Iit_tokens.LPAREN | Iit_tokens.LT | Iit_tokens.NE | Iit_tokens.NEG | Iit_tokens.NOT | Iit_tokens.OR | Iit_tokens.RPAREN | Iit_tokens.STOP | Iit_tokens.WHILE ->
          let MenhirCell1_EQUAL (_menhir_stack, _) = _menhir_stack in
          let MenhirCell1_expr (_menhir_stack, _menhir_s, _1, _startpos__1_, _) = _menhir_stack in
          let (_3, _endpos__3_) = (_v, _endpos) in
          let _v = _menhir_action_28 _1 _3 in
          _menhir_goto_expr_ _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__3_ _startpos__1_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_047 : type  ttv_stack ttv_result. (((ttv_stack, ttv_result) _menhir_cell1_expr, ttv_result) _menhir_cell1_GE as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | Iit_tokens.TIMES ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState047
      | Iit_tokens.PLUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState047
      | Iit_tokens.MINUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState047
      | Iit_tokens.DIV ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState047
      | Iit_tokens.AND | Iit_tokens.ASSIGN | Iit_tokens.CCHAR | Iit_tokens.CHAR _ | Iit_tokens.CINT | Iit_tokens.CLG | Iit_tokens.DO | Iit_tokens.ELSE | Iit_tokens.END | Iit_tokens.EOF | Iit_tokens.EQUAL | Iit_tokens.GE | Iit_tokens.GT | Iit_tokens.IDENT _ | Iit_tokens.IF | Iit_tokens.INT _ | Iit_tokens.LE | Iit_tokens.LPAREN | Iit_tokens.LT | Iit_tokens.NE | Iit_tokens.NEG | Iit_tokens.NOT | Iit_tokens.OR | Iit_tokens.RPAREN | Iit_tokens.STOP | Iit_tokens.WHILE ->
          let MenhirCell1_GE (_menhir_stack, _) = _menhir_stack in
          let MenhirCell1_expr (_menhir_stack, _menhir_s, _1, _startpos__1_, _) = _menhir_stack in
          let (_3, _endpos__3_) = (_v, _endpos) in
          let _v = _menhir_action_26 _1 _3 in
          _menhir_goto_expr_ _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__3_ _startpos__1_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_045 : type  ttv_stack ttv_result. (((ttv_stack, ttv_result) _menhir_cell1_expr, ttv_result) _menhir_cell1_GT as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | Iit_tokens.TIMES ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState045
      | Iit_tokens.PLUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState045
      | Iit_tokens.MINUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState045
      | Iit_tokens.DIV ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState045
      | Iit_tokens.AND | Iit_tokens.ASSIGN | Iit_tokens.CCHAR | Iit_tokens.CHAR _ | Iit_tokens.CINT | Iit_tokens.CLG | Iit_tokens.DO | Iit_tokens.ELSE | Iit_tokens.END | Iit_tokens.EOF | Iit_tokens.EQUAL | Iit_tokens.GE | Iit_tokens.GT | Iit_tokens.IDENT _ | Iit_tokens.IF | Iit_tokens.INT _ | Iit_tokens.LE | Iit_tokens.LPAREN | Iit_tokens.LT | Iit_tokens.NE | Iit_tokens.NEG | Iit_tokens.NOT | Iit_tokens.OR | Iit_tokens.RPAREN | Iit_tokens.STOP | Iit_tokens.WHILE ->
          let MenhirCell1_GT (_menhir_stack, _) = _menhir_stack in
          let MenhirCell1_expr (_menhir_stack, _menhir_s, _1, _startpos__1_, _) = _menhir_stack in
          let (_3, _endpos__3_) = (_v, _endpos) in
          let _v = _menhir_action_25 _1 _3 in
          _menhir_goto_expr_ _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__3_ _startpos__1_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_043 : type  ttv_stack ttv_result. (((ttv_stack, ttv_result) _menhir_cell1_expr, ttv_result) _menhir_cell1_LE as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | Iit_tokens.TIMES ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState043
      | Iit_tokens.PLUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState043
      | Iit_tokens.MINUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState043
      | Iit_tokens.DIV ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState043
      | Iit_tokens.AND | Iit_tokens.ASSIGN | Iit_tokens.CCHAR | Iit_tokens.CHAR _ | Iit_tokens.CINT | Iit_tokens.CLG | Iit_tokens.DO | Iit_tokens.ELSE | Iit_tokens.END | Iit_tokens.EOF | Iit_tokens.EQUAL | Iit_tokens.GE | Iit_tokens.GT | Iit_tokens.IDENT _ | Iit_tokens.IF | Iit_tokens.INT _ | Iit_tokens.LE | Iit_tokens.LPAREN | Iit_tokens.LT | Iit_tokens.NE | Iit_tokens.NEG | Iit_tokens.NOT | Iit_tokens.OR | Iit_tokens.RPAREN | Iit_tokens.STOP | Iit_tokens.WHILE ->
          let MenhirCell1_LE (_menhir_stack, _) = _menhir_stack in
          let MenhirCell1_expr (_menhir_stack, _menhir_s, _1, _startpos__1_, _) = _menhir_stack in
          let (_3, _endpos__3_) = (_v, _endpos) in
          let _v = _menhir_action_24 _1 _3 in
          _menhir_goto_expr_ _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__3_ _startpos__1_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_041 : type  ttv_stack ttv_result. (((ttv_stack, ttv_result) _menhir_cell1_expr, ttv_result) _menhir_cell1_LT as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | Iit_tokens.TIMES ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState041
      | Iit_tokens.PLUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState041
      | Iit_tokens.MINUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState041
      | Iit_tokens.DIV ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState041
      | Iit_tokens.AND | Iit_tokens.ASSIGN | Iit_tokens.CCHAR | Iit_tokens.CHAR _ | Iit_tokens.CINT | Iit_tokens.CLG | Iit_tokens.DO | Iit_tokens.ELSE | Iit_tokens.END | Iit_tokens.EOF | Iit_tokens.EQUAL | Iit_tokens.GE | Iit_tokens.GT | Iit_tokens.IDENT _ | Iit_tokens.IF | Iit_tokens.INT _ | Iit_tokens.LE | Iit_tokens.LPAREN | Iit_tokens.LT | Iit_tokens.NE | Iit_tokens.NEG | Iit_tokens.NOT | Iit_tokens.OR | Iit_tokens.RPAREN | Iit_tokens.STOP | Iit_tokens.WHILE ->
          let MenhirCell1_LT (_menhir_stack, _) = _menhir_stack in
          let MenhirCell1_expr (_menhir_stack, _menhir_s, _1, _startpos__1_, _) = _menhir_stack in
          let (_3, _endpos__3_) = (_v, _endpos) in
          let _v = _menhir_action_23 _1 _3 in
          _menhir_goto_expr_ _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__3_ _startpos__1_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_039 : type  ttv_stack ttv_result. (((ttv_stack, ttv_result) _menhir_cell1_expr, ttv_result) _menhir_cell1_MINUS as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | Iit_tokens.TIMES ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState039
      | Iit_tokens.DIV ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState039
      | Iit_tokens.AND | Iit_tokens.ASSIGN | Iit_tokens.CCHAR | Iit_tokens.CHAR _ | Iit_tokens.CINT | Iit_tokens.CLG | Iit_tokens.DO | Iit_tokens.ELSE | Iit_tokens.END | Iit_tokens.EOF | Iit_tokens.EQUAL | Iit_tokens.GE | Iit_tokens.GT | Iit_tokens.IDENT _ | Iit_tokens.IF | Iit_tokens.INT _ | Iit_tokens.LE | Iit_tokens.LPAREN | Iit_tokens.LT | Iit_tokens.MINUS | Iit_tokens.NE | Iit_tokens.NEG | Iit_tokens.NOT | Iit_tokens.OR | Iit_tokens.PLUS | Iit_tokens.RPAREN | Iit_tokens.STOP | Iit_tokens.WHILE ->
          let MenhirCell1_MINUS (_menhir_stack, _) = _menhir_stack in
          let MenhirCell1_expr (_menhir_stack, _menhir_s, _1, _startpos__1_, _) = _menhir_stack in
          let (_3, _endpos__3_) = (_v, _endpos) in
          let _v = _menhir_action_18 _1 _3 in
          _menhir_goto_expr_ _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__3_ _startpos__1_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_037 : type  ttv_stack ttv_result. (((ttv_stack, ttv_result) _menhir_cell1_expr, ttv_result) _menhir_cell1_NE as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | Iit_tokens.TIMES ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState037
      | Iit_tokens.PLUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState037
      | Iit_tokens.MINUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState037
      | Iit_tokens.DIV ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState037
      | Iit_tokens.AND | Iit_tokens.ASSIGN | Iit_tokens.CCHAR | Iit_tokens.CHAR _ | Iit_tokens.CINT | Iit_tokens.CLG | Iit_tokens.DO | Iit_tokens.ELSE | Iit_tokens.END | Iit_tokens.EOF | Iit_tokens.EQUAL | Iit_tokens.GE | Iit_tokens.GT | Iit_tokens.IDENT _ | Iit_tokens.IF | Iit_tokens.INT _ | Iit_tokens.LE | Iit_tokens.LPAREN | Iit_tokens.LT | Iit_tokens.NE | Iit_tokens.NEG | Iit_tokens.NOT | Iit_tokens.OR | Iit_tokens.RPAREN | Iit_tokens.STOP | Iit_tokens.WHILE ->
          let MenhirCell1_NE (_menhir_stack, _) = _menhir_stack in
          let MenhirCell1_expr (_menhir_stack, _menhir_s, _1, _startpos__1_, _) = _menhir_stack in
          let (_3, _endpos__3_) = (_v, _endpos) in
          let _v = _menhir_action_27 _1 _3 in
          _menhir_goto_expr_ _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__3_ _startpos__1_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_035 : type  ttv_stack ttv_result. (((ttv_stack, ttv_result) _menhir_cell1_expr, ttv_result) _menhir_cell1_OR as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | Iit_tokens.TIMES ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState035
      | Iit_tokens.PLUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState035
      | Iit_tokens.NE ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState035
      | Iit_tokens.MINUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState035
      | Iit_tokens.LT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState035
      | Iit_tokens.LE ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState035
      | Iit_tokens.GT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState035
      | Iit_tokens.GE ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState035
      | Iit_tokens.EQUAL ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState035
      | Iit_tokens.DIV ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState035
      | Iit_tokens.AND ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState035
      | Iit_tokens.ASSIGN | Iit_tokens.CCHAR | Iit_tokens.CHAR _ | Iit_tokens.CINT | Iit_tokens.CLG | Iit_tokens.DO | Iit_tokens.ELSE | Iit_tokens.END | Iit_tokens.EOF | Iit_tokens.IDENT _ | Iit_tokens.IF | Iit_tokens.INT _ | Iit_tokens.LPAREN | Iit_tokens.NEG | Iit_tokens.NOT | Iit_tokens.OR | Iit_tokens.RPAREN | Iit_tokens.STOP | Iit_tokens.WHILE ->
          let MenhirCell1_OR (_menhir_stack, _) = _menhir_stack in
          let MenhirCell1_expr (_menhir_stack, _menhir_s, _1, _startpos__1_, _) = _menhir_stack in
          let (_3, _endpos__3_) = (_v, _endpos) in
          let _v = _menhir_action_22 _1 _3 in
          _menhir_goto_expr_ _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__3_ _startpos__1_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_033 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_expr, ttv_result) _menhir_cell1_DIV -> _ -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell1_DIV (_menhir_stack, _) = _menhir_stack in
      let MenhirCell1_expr (_menhir_stack, _menhir_s, _1, _startpos__1_, _) = _menhir_stack in
      let (_3, _endpos__3_) = (_v, _endpos) in
      let _v = _menhir_action_20 _1 _3 in
      _menhir_goto_expr_ _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__3_ _startpos__1_ _v _menhir_s _tok
  
  and _menhir_run_031 : type  ttv_stack ttv_result. (((ttv_stack, ttv_result) _menhir_cell1_expr, ttv_result) _menhir_cell1_PLUS as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | Iit_tokens.TIMES ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState031
      | Iit_tokens.DIV ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState031
      | Iit_tokens.AND | Iit_tokens.ASSIGN | Iit_tokens.CCHAR | Iit_tokens.CHAR _ | Iit_tokens.CINT | Iit_tokens.CLG | Iit_tokens.DO | Iit_tokens.ELSE | Iit_tokens.END | Iit_tokens.EOF | Iit_tokens.EQUAL | Iit_tokens.GE | Iit_tokens.GT | Iit_tokens.IDENT _ | Iit_tokens.IF | Iit_tokens.INT _ | Iit_tokens.LE | Iit_tokens.LPAREN | Iit_tokens.LT | Iit_tokens.MINUS | Iit_tokens.NE | Iit_tokens.NEG | Iit_tokens.NOT | Iit_tokens.OR | Iit_tokens.PLUS | Iit_tokens.RPAREN | Iit_tokens.STOP | Iit_tokens.WHILE ->
          let MenhirCell1_PLUS (_menhir_stack, _) = _menhir_stack in
          let MenhirCell1_expr (_menhir_stack, _menhir_s, _1, _startpos__1_, _) = _menhir_stack in
          let (_3, _endpos__3_) = (_v, _endpos) in
          let _v = _menhir_action_17 _1 _3 in
          _menhir_goto_expr_ _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__3_ _startpos__1_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_028 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_expr, ttv_result) _menhir_cell1_TIMES -> _ -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell1_TIMES (_menhir_stack, _) = _menhir_stack in
      let MenhirCell1_expr (_menhir_stack, _menhir_s, _1, _startpos__1_, _) = _menhir_stack in
      let (_3, _endpos__3_) = (_v, _endpos) in
      let _v = _menhir_action_19 _1 _3 in
      _menhir_goto_expr_ _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__3_ _startpos__1_ _v _menhir_s _tok
  
  and _menhir_run_026 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_LPAREN as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | Iit_tokens.TIMES ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState026
      | Iit_tokens.RPAREN ->
          let _endpos_0 = _menhir_lexbuf.Lexing.lex_curr_p in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_LPAREN (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
          let (_2, _endpos__3_) = (_v, _endpos_0) in
          let _v = _menhir_action_34 _2 in
          _menhir_goto_expr_ _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__3_ _startpos__1_ _v _menhir_s _tok
      | Iit_tokens.PLUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState026
      | Iit_tokens.OR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState026
      | Iit_tokens.NE ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState026
      | Iit_tokens.MINUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState026
      | Iit_tokens.LT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState026
      | Iit_tokens.LE ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState026
      | Iit_tokens.GT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState026
      | Iit_tokens.GE ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState026
      | Iit_tokens.EQUAL ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState026
      | Iit_tokens.DIV ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState026
      | Iit_tokens.ASSIGN ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState026
      | Iit_tokens.AND ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState026
      | _ ->
          _eRR ()
  
  and _menhir_run_025 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_CLG -> _ -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell1_CLG (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
      let (_endpos__2_, _2) = (_endpos, _v) in
      let _v = _menhir_action_32 _2 in
      _menhir_goto_expr_ _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__2_ _startpos__1_ _v _menhir_s _tok
  
  and _menhir_run_024 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_CINT -> _ -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell1_CINT (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
      let (_endpos__2_, _2) = (_endpos, _v) in
      let _v = _menhir_action_33 _2 in
      _menhir_goto_expr_ _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__2_ _startpos__1_ _v _menhir_s _tok
  
  and _menhir_run_022 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_CCHAR -> _ -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell1_CCHAR (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
      let (_endpos__2_, _2) = (_endpos, _v) in
      let _v = _menhir_action_31 _2 in
      _menhir_goto_expr_ _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__2_ _startpos__1_ _v _menhir_s _tok
  
  let _menhir_run_010 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_decllist (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | Iit_tokens.WHILE ->
          _menhir_run_011 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState010
      | Iit_tokens.STOP ->
          _menhir_run_057 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState010
      | Iit_tokens.NOT ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState010
      | Iit_tokens.NEG ->
          _menhir_run_013 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState010
      | Iit_tokens.LPAREN ->
          _menhir_run_014 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState010
      | Iit_tokens.INT _v_0 ->
          _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState010
      | Iit_tokens.IF ->
          _menhir_run_058 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState010
      | Iit_tokens.IDENT _v_1 ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState010
      | Iit_tokens.DO ->
          _menhir_run_060 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState010
      | Iit_tokens.CLG ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState010
      | Iit_tokens.CINT ->
          _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState010
      | Iit_tokens.CHAR _v_2 ->
          _menhir_run_019 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2 MenhirState010
      | Iit_tokens.CCHAR ->
          _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState010
      | Iit_tokens.EOF ->
          let _v_3 = _menhir_action_48 () in
          _menhir_run_082 _menhir_stack _v_3 _tok
      | _ ->
          _menhir_fail ()
  
  let rec _menhir_run_085 : type  ttv_stack. (ttv_stack, _menhir_box_prog) _menhir_cell1_decl -> _ -> _ -> _ -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_decl (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _2 = _v in
      let _v = _menhir_action_12 _1 _2 in
      _menhir_goto_decllist _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_decllist : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState084 ->
          _menhir_run_085 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState000 ->
          _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  let rec _menhir_run_001 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _startpos__1_ = _startpos in
      let _v = _menhir_action_52 () in
      _menhir_goto_typ _menhir_stack _menhir_lexbuf _menhir_lexer _startpos__1_ _v _menhir_s _tok
  
  and _menhir_goto_typ : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _startpos _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_typ (_menhir_stack, _menhir_s, _v, _startpos) in
      match (_tok : MenhirBasics.token) with
      | Iit_tokens.IDENT _v_0 ->
          _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState004
      | _ ->
          _eRR ()
  
  and _menhir_run_005 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | Iit_tokens.COMMA ->
          let _menhir_stack = MenhirCell1_IDENT (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          let _menhir_s = MenhirState006 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | Iit_tokens.IDENT _v ->
              _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | Iit_tokens.CCHAR | Iit_tokens.CHAR _ | Iit_tokens.CINT | Iit_tokens.CLG | Iit_tokens.DO | Iit_tokens.EOF | Iit_tokens.IDENT _ | Iit_tokens.IF | Iit_tokens.INT _ | Iit_tokens.LPAREN | Iit_tokens.NEG | Iit_tokens.NOT | Iit_tokens.STOP | Iit_tokens.TCHARACTER | Iit_tokens.TINT | Iit_tokens.TLOGICAL | Iit_tokens.WHILE ->
          let (_endpos__1_, _1) = (_endpos, _v) in
          let _v = _menhir_action_43 _1 in
          _menhir_goto_src_iitran_iit_parser_list_IDENT_ _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__1_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_goto_src_iitran_iit_parser_list_IDENT_ : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState004 ->
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | MenhirState006 ->
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_008 : type  ttv_stack. (ttv_stack, _menhir_box_prog) _menhir_cell1_typ -> _ -> _ -> _ -> _ -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell1_typ (_menhir_stack, _menhir_s, _1, _startpos__1_) = _menhir_stack in
      let (_endpos__2_, _2) = (_endpos, _v) in
      let _v = _menhir_action_10 _1 _2 _endpos__2_ _startpos__1_ in
      let _menhir_stack = MenhirCell1_decl (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | Iit_tokens.TLOGICAL ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState084
      | Iit_tokens.TINT ->
          _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState084
      | Iit_tokens.TCHARACTER ->
          _menhir_run_003 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState084
      | Iit_tokens.CCHAR | Iit_tokens.CHAR _ | Iit_tokens.CINT | Iit_tokens.CLG | Iit_tokens.DO | Iit_tokens.EOF | Iit_tokens.IDENT _ | Iit_tokens.IF | Iit_tokens.INT _ | Iit_tokens.LPAREN | Iit_tokens.NEG | Iit_tokens.NOT | Iit_tokens.STOP | Iit_tokens.WHILE ->
          let _v_0 = _menhir_action_11 () in
          _menhir_run_085 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_002 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _startpos__1_ = _startpos in
      let _v = _menhir_action_50 () in
      _menhir_goto_typ _menhir_stack _menhir_lexbuf _menhir_lexer _startpos__1_ _v _menhir_s _tok
  
  and _menhir_run_003 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _startpos__1_ = _startpos in
      let _v = _menhir_action_51 () in
      _menhir_goto_typ _menhir_stack _menhir_lexbuf _menhir_lexer _startpos__1_ _v _menhir_s _tok
  
  and _menhir_run_007 : type  ttv_stack. (ttv_stack, _menhir_box_prog) _menhir_cell1_IDENT -> _ -> _ -> _ -> _ -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell1_IDENT (_menhir_stack, _menhir_s, _1, _, _) = _menhir_stack in
      let (_3, _endpos__3_) = (_v, _endpos) in
      let _v = _menhir_action_44 _1 _3 in
      _menhir_goto_src_iitran_iit_parser_list_IDENT_ _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__3_ _v _menhir_s _tok
  
  let _menhir_run_000 : type  ttv_stack. ttv_stack -> _ -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | Iit_tokens.TLOGICAL ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState000
      | Iit_tokens.TINT ->
          _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState000
      | Iit_tokens.TCHARACTER ->
          _menhir_run_003 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState000
      | Iit_tokens.CCHAR | Iit_tokens.CHAR _ | Iit_tokens.CINT | Iit_tokens.CLG | Iit_tokens.DO | Iit_tokens.EOF | Iit_tokens.IDENT _ | Iit_tokens.IF | Iit_tokens.INT _ | Iit_tokens.LPAREN | Iit_tokens.NEG | Iit_tokens.NOT | Iit_tokens.STOP | Iit_tokens.WHILE ->
          let _v = _menhir_action_11 () in
          _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState000 _tok
      | _ ->
          _eRR ()
  
  let _menhir_run_086 : type  ttv_stack. ttv_stack -> _ -> _ -> _menhir_box_stmt =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState086 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | Iit_tokens.WHILE ->
          _menhir_run_087 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Iit_tokens.STOP ->
          _menhir_run_057 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Iit_tokens.NOT ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Iit_tokens.NEG ->
          _menhir_run_013 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Iit_tokens.LPAREN ->
          _menhir_run_014 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Iit_tokens.INT _v ->
          _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | Iit_tokens.IF ->
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Iit_tokens.IDENT _v ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | Iit_tokens.DO ->
          _menhir_run_060 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Iit_tokens.CLG ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Iit_tokens.CINT ->
          _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Iit_tokens.CHAR _v ->
          _menhir_run_019 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | Iit_tokens.CCHAR ->
          _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
end

let stmt =
  fun _menhir_lexer _menhir_lexbuf ->
    let _menhir_stack = () in
    let MenhirBox_stmt v = _menhir_run_086 _menhir_stack _menhir_lexbuf _menhir_lexer in
    v

let prog =
  fun _menhir_lexer _menhir_lexbuf ->
    let _menhir_stack = () in
    let MenhirBox_prog v = _menhir_run_000 _menhir_stack _menhir_lexbuf _menhir_lexer in
    v

# 20 "src/iitran/iit_tokens.mly"
  

# 3082 "src/iitran/iit_parser.ml"
