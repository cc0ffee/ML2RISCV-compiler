%{
    open Ml_ast

    exception SyntaxError
    let syn_err = syn_err SyntaxError
       
%}


%right ARROW CONS
%left LT LE GT GE NE EQUAL
%left PLUS MINUS
%left TIMES DIV
%left AND OR
%left NEG NOT


%start prog

%type <Ml_ast.typ> atyp
%type <Ml_ast.typ> typ
%type <Ml_ast.typ> ttyp
%type <Ml_ast.const> const
%type <unit Ml_ast.exp_> expr_
%type <unit Ml_ast.exp_> simple_expr_
%type <unit Ml_ast.exp_> tuple_
%type <unit Ml_ast.exp_> app_expr_
%type <unit Ml_ast.exp> expr
%type <unit Ml_ast.exp> simple_expr
%type <string * Ml_ast.typ> ppat
%type <Ml_ast.typ option> optannot

%type <Ml_ast.p_prog> prog

%%
expr_:
| app_expr_                                { $1 }
| expr PLUS expr                       { EBinop (BAdd, $1, $3) }
| expr MINUS expr                      { EBinop (BSub, $1, $3) }
| expr TIMES expr                      { EBinop (BMul, $1, $3) }
| expr DIV expr                        { EBinop (BDiv, $1, $3) }
| expr AND expr                        { EBinop (BAnd, $1, $3) }
| expr OR expr                         { EBinop (BOr, $1, $3) }
| expr EQUAL expr                      { EBinop (BEq, $1, $3) }
| expr LT expr                         { EBinop (BLt, $1, $3) }
| expr LE expr                         { EBinop (BLe, $1, $3) }
| expr GT expr                         { EBinop (BGt, $1, $3) }
| expr GE expr                         { EBinop (BGe, $1, $3) }
| expr NE expr                         { EBinop (BNe, $1, $3) }
| expr PLUS error { syn_err "Expected expression" $loc }
| expr MINUS error { syn_err "Expected expression" $loc }
| expr TIMES error { syn_err "Expected expression" $loc }
| expr DIV error { syn_err "Expected expression" $loc }
| expr AND error { syn_err "Expected expression" $loc }
| expr OR error { syn_err "Expected expression" $loc }
| expr LT error { syn_err "Expected expression" $loc }
| expr LE error { syn_err "Expected expression" $loc }
| expr GT error { syn_err "Expected expression" $loc }
| expr GE error { syn_err "Expected expression" $loc }
| expr NE error { syn_err "Expected expression" $loc }
| MATCH expr WITH NIL ARROW expr PIPE IDENT CONS IDENT ARROW expr
                                           { EMatchList ($2, $6, $8, $10, $12) }
| MATCH expr WITH PIPE NIL ARROW expr PIPE IDENT CONS IDENT ARROW expr
                                           { EMatchList ($2, $7, $9, $11, $13) }
| IF expr THEN expr ELSE expr              { EIf ($2, $4, $6) }
| LET IDENT optannot EQUAL expr IN expr    { ELet ($2, $3, $5, $7) }
| LET REC IDENT ppat optannot EQUAL expr IN expr
                           { ELetFun (true, $3, fst $4, snd $4, $5, $7, $9) }
| LET IDENT ppat optannot EQUAL expr IN expr
                           { ELetFun (false, $2, fst $3, snd $3, $4, $6, $8) }
| LET LPAREN IDENT COMMA IDENT RPAREN EQUAL expr IN expr
    { ELetPair ($3, $5, $8, $10) }
| FUN ppat ARROW expr                      { EFun (fst $2, snd $2, $4) }
| simple_expr COLON typ                           { EAnnot ($1, $3) }
| simple_expr CONS error { syn_err "Expected expression" $loc }
| MATCH expr WITH NIL ARROW expr PIPE IDENT CONS IDENT ARROW error
    { syn_err "Invalid pattern match:\n  Only pattern matches of the form | [] -> e | h::t -> e are supported" $loc}
| MATCH expr WITH NIL ARROW expr PIPE IDENT CONS IDENT error
    { syn_err "Invalid pattern match:\n  Only pattern matches of the form | [] -> e | h::t -> e are supported" $loc}
| MATCH expr WITH NIL ARROW expr PIPE IDENT CONS error
    { syn_err "Invalid pattern match:\n  Only pattern matches of the form | [] -> e | h::t -> e are supported" $loc}
| MATCH expr WITH NIL ARROW expr PIPE IDENT error
    { syn_err "Invalid pattern match:\n  Only pattern matches of the form | [] -> e | h::t -> e are supported" $loc}
| MATCH expr WITH NIL ARROW expr PIPE error
    { syn_err "Invalid pattern match:\n  Only pattern matches of the form | [] -> e | h::t -> e are supported" $loc}
| MATCH expr WITH NIL ARROW expr error
    { syn_err "Invalid pattern match:\n  Only pattern matches of the form | [] -> e | h::t -> e are supported" $loc}
| MATCH expr WITH NIL ARROW error
    { syn_err "Invalid pattern match:\n  Only pattern matches of the form | [] -> e | h::t -> e are supported" $loc}
| MATCH expr WITH NIL error
    { syn_err "Invalid pattern match:\n  Only pattern matches of the form | [] -> e | h::t -> e are supported" $loc}
| MATCH expr WITH error { syn_err "Expected case after WITH" $loc }
| MATCH expr error { syn_err "Expected WITH" $loc }
| MATCH error { syn_err "Expected expression" $loc }
| IF expr THEN expr ELSE error { syn_err "Expected expression" $loc }
| IF expr THEN expr error { syn_err "Invalid token" $loc }
| IF expr THEN error { syn_err "Expected expression (if without else not supported)" $loc }
| IF expr error { syn_err "Expected THEN" $loc }
| IF error { syn_err "Expected expression" $loc }
| LET IDENT optannot EQUAL expr IN error
    { syn_err "Expected expression" $loc }
| LET IDENT optannot EQUAL expr error
    { syn_err "Expected IN" $loc }
| LET IDENT optannot EQUAL error
    { syn_err "Expected expression" $loc }
| LET IDENT error
    { syn_err "Expected = or pattern (Maybe you forgot a type annotation on a function?)" $loc }
| LET error
    { syn_err "Expected pattern or REC" $loc }
| LET REC IDENT ppat optannot EQUAL expr IN error
    { syn_err "Expected expression" $loc }
| LET REC IDENT ppat optannot EQUAL expr error
    { syn_err "Expected IN" $loc }
| LET REC IDENT ppat optannot EQUAL error
    { syn_err "Expected expression" $loc }
| LET REC IDENT ppat error
    { syn_err "Expected =" $loc }
| LET REC IDENT error
    { syn_err "Expected pattern (x : t) -- note type annotation is required" $loc }
| LET REC error
    { syn_err "Expected identifier" $loc }
| LET IDENT ppat optannot EQUAL expr IN error
    { syn_err "Expected expression" $loc }
| LET IDENT ppat optannot EQUAL expr error
    { syn_err "Expected IN" $loc }
| LET IDENT ppat optannot EQUAL error
    { syn_err "Expected expression" $loc }
| LET IDENT ppat error
    { syn_err "Expected =" $loc }
| LET LPAREN IDENT COMMA IDENT RPAREN EQUAL expr IN error
    { syn_err "Expected expression" $loc }
| LET LPAREN IDENT COMMA IDENT RPAREN EQUAL expr error
    { syn_err "Expected IN" $loc }
| LET LPAREN IDENT COMMA IDENT RPAREN EQUAL error
    { syn_err "Expected expression" $loc }
| LET LPAREN IDENT COMMA IDENT RPAREN error
    { syn_err "Expected =" $loc }
| LET LPAREN IDENT COMMA IDENT error
    { syn_err "Expected )" $loc }
| LET LPAREN IDENT COMMA error
    { syn_err "Expected identifier" $loc }
| LET LPAREN IDENT error
    { syn_err "Expected COMMA identifier RPAREN" $loc }
| FUN ppat ARROW error {syn_err "Expected expression" $loc}
| FUN ppat error { syn_err "Expected ->" $loc }
| FUN error { syn_err "Expected pattern" $loc }
| simple_expr COLON error { syn_err "Expected type" $loc }

app_expr_:
| simple_expr_                             { $1 }
| app_expr_ simple_expr                    { EApp (mk_exp $1 $loc, $2)}
;

tuple_:
| expr COMMA expr                                 { EPair ($1, $3) }
| expr COMMA tuple_                               { EPair ($1,
						           mk_exp $3 $loc) }
| expr COMMA error { syn_err "Expected expression" $loc }
;

simple_expr_:
  NOT simple_expr                                    { EUnop (UNot, $2) }
| NEG simple_expr                                    { EUnop (UNeg, $2) }
| simple_expr CONS simple_expr                       { ECons ($1, $3) }
| const                                    { EConst $1 }
| IDENT                                    { EVar $1 }
| LPAREN expr RPAREN                       { $2.edesc }
| NOT error { syn_err "Expected expression" $loc }
| NEG error { syn_err "Expected expression" $loc }
| LPAREN expr error { syn_err "Expected )" $loc }
| LPAREN tuple_ RPAREN                     { $2 }
| LPAREN tuple_ error {syn_err "Expected )" $loc }
| LPAREN error { syn_err "Expected )" $loc }
;

expr:
  expr_                                    { mk_exp $1 $loc }
;

simple_expr:
  simple_expr_                             { mk_exp $1 $loc }
;


ppat:
  LPAREN IDENT COLON typ RPAREN            { ($2, $4) }
;


optannot:
  COLON typ                                { Some $2 }
|                                          { None }

const:
  NUM                                      { CNum $1 }
| TRUE                                     { CBool true }
| FALSE                                    { CBool false }
| UNIT                                     { CTriv }
| NIL                                      { CNil }
;

atyp:
  TINT                                     { TInt }
| TBOOL                                    { TBool }
| TUNIT                                    { TUnit }
| atyp TLIST                               { TList $1 }
| LPAREN typ RPAREN                        { $2 }
| LPAREN typ error { syn_err "Expected )" $loc }
;

ttyp:
  atyp                                     { $1 }
| ttyp TIMES ttyp                           { TProd ($1, $3) }
| ttyp TIMES error { syn_err "Expected type" $loc }
;

typ:
  ttyp                                     { $1 }
| typ ARROW typ                            { TArrow ($1, $3) }
| typ ARROW error { syn_err "Expected type" $loc }
;

prog:
| LET IDENT optannot EQUAL expr DOUBLESEMI prog
    { Ml_ast.mk_exp (ELet ($2, $3, $5, $7)) $loc }
| LET REC IDENT ppat optannot EQUAL expr DOUBLESEMI prog
    { Ml_ast.mk_exp (ELetFun (true, $3, fst $4, snd $4, $5, $7, $9)) $loc }
| LET IDENT ppat optannot EQUAL expr DOUBLESEMI prog
    { Ml_ast.mk_exp (ELetFun (false, $2, fst $3, snd $3, $4, $6, $8)) $loc }
| expr DOUBLESEMI prog
    { Ml_ast.mk_exp (ELet ("_", None, $1, $3)) $loc }
| expr DOUBLESEMI EOF
    { $1 }
;
