{

  open Ml_tokens
  exception Quit

}

let digit = ['0'-'9']
let identchar = ['a'-'z' 'A'-'Z' '\'' '_' '0'-'9']
let ident = ['a'-'z'] identchar*
let opencomment = "(*"
let closecomment = "*)"
let ws = [' ' '\t']

rule comment = parse
       | closecomment { token lexbuf }
       | '\n' { Lexing.new_line lexbuf; comment lexbuf }
       | "\r\n" { Lexing.new_line lexbuf; comment lexbuf }
       | _ { comment lexbuf}
and token = parse
       | ws { token lexbuf }
       | '\n' { Lexing.new_line lexbuf; token lexbuf }
       | "\r\n" { Lexing.new_line lexbuf; token lexbuf }
       | opencomment { comment lexbuf }
       | digit+ as n { NUM (int_of_string n) }
       | "true" { TRUE }
       | "false" { FALSE }
       | "()" { UNIT }
       | "[]" { NIL }

       | "int" { TINT }
       | "bool" { TBOOL }
       | "unit" { TUNIT }
       | "list" { TLIST }

       | "," { COMMA }
       | "+" { PLUS }
       | "-" { MINUS }
       | "*" { TIMES }
       | "/" { DIV }
       | "::" { CONS }
       | ":" { COLON }
       | "&&" { AND }
       | "||" { OR }
       | "<=" { LE }
       | "<" { LT }
       | ">=" { GE }
       | ">" { GT }
       | "<>" { NE }
             
       | "=" { EQUAL }

       | "fun" { FUN }
       | "->" { ARROW }

       | "if" { IF }
       | "then" { THEN }
       | "else" { ELSE }

       | "match" { MATCH }
       | "with" { WITH }
       | "|" { PIPE }

       | "let" { LET }
       | "rec" { REC }
       | "in" { IN }

       | "not" { NOT }
       | "~-" { NEG }

       | "(" { LPAREN }
       | ")" { RPAREN }
               
       | ident as s { IDENT s }

       | eof { EOF }
       | ";;" { DOUBLESEMI }
