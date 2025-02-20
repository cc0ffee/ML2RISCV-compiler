{
  open Llvm_tokens

(* Taken from https://v2.ocaml.org/releases/4.14/htmlman/lexyacc.html#s%3Alexyacc-common-errors
and https://stackoverflow.com/questions/35068495/ocamllex-case-insenstitive*)
let keywords = Hashtbl.create 10
let _ = List.iter (fun (kwd, tok) -> Hashtbl.add keywords kwd tok)
    [
    "add", ADD;
    "sub", SUB;
    "mul", MUL;
    "sdiv", DIV;
    "and", AND;
    "or", OR;
    "xor", XOR;
    "bitcast", BITCAST;
    "zext", ZEXT;
    "ptrtoint", PTRTOINT;
    "inttoptr", INTTOPTR;
    "trunc", TRUNC;
    "icmp", ICMP;
    "br", BR;
    "ret", RET;
    "call", CALL;
    "getelementptr", GETELEMENTPTR;
    "alloca", ALLOCA;
    "load", LOAD;
    "store", STORE;
    "phi", PHI;
    "define", DEFINE;
    "declare", DECLARE;
    "to", TO;
    "label", LABEL;
    "null", NULL;
    "type", TYP;
    "void", TVOID;
      ]

let structs : Llvm_ast.typ list Varmap.t ref = ref Varmap.empty
let should_be_ssa = ref false
}

let digit = ['0'-'9']
let identchar = ['a'-'z' 'A'-'Z' '_' '0'-'9' '$']
let ident = ['a'-'z' 'A'-'Z' '_'] identchar*
let ws = [' ' '\t']

rule token = parse
     | ws { token lexbuf }
     | '}' { RBRACE }
     | "\n" { Lexing.new_line lexbuf; token lexbuf }
     | "\r\n" { Lexing.new_line lexbuf; token lexbuf }
     | '-'? digit+ as n { CONST (int_of_string n) }
     | '%' (ident as v) {
       try let _ = Varmap.find v (!structs) in TSTRUCT v
       with Not_found -> LVAR (String.map (fun c -> if c = '$' then '_' else c) v) }
     | '@' (ident as v) { GVAR v }
     | '(' { LPAREN }
     | ')' { RPAREN }
     | '[' { LSQ }
     | ']' { RSQ }
     | '{' { LBRACE }
     | '*' { STAR }
     | ',' { COMMA }
     | ':' { COLON }
     | '=' { EQUAL }
     | eof { EOF }
     | 'i' (digit+ as n) { TINT (int_of_string n) }
     | ident as k
       { try Hashtbl.find keywords (String.lowercase_ascii k)
     	     with Not_found -> KEYWORD (String.lowercase_ascii k) }
