(* cprint -- pretty printer of C program from abstract syntax *)

open C_ast
let version = "Cprint 4.0 Hugues Cassé et al., modified 2022 by Stefan Muller"

(*
** FrontC Pretty printer
** Modified to print desugared C ASTs for IIT CS 443
*)
let out = ref stdout
let width = ref 80
let tab = ref 2
let max_indent = ref 60

let line = ref ""
let line_len = ref 0
let current = ref ""
let current_len = ref 0
let spaces = ref 0
let follow = ref 0
let roll = ref 0

let print_tab size =
  (* output_string !out (String.make (size / 8) '\t'); *)
  output_string !out (String.make size ' ')

let flush _ =
  if !line <> "" then begin
    print_tab (!spaces + !follow);
    output_string !out !line;
    line := "";
    line_len := 0
  end

let commit _ =
  if !current <> "" then begin
    if !line = "" then begin
      line := !current;
      line_len := !current_len
    end else begin
      line := (!line ^ " " ^ !current);
      line_len := !line_len + 1 + !current_len
    end;
    current := "";
    current_len := 0
  end

let new_line _ =
  commit ();
  if !line <> "" then begin
    flush ();
    output_char !out '\n'
  end;
  follow := 0

let force_new_line _ =
  commit ();
  flush ();
  output_char !out '\n';
  follow := 0

let indent _ =
  new_line ();
  spaces := !spaces + !tab;
  if !spaces >= !max_indent then begin
    spaces := !tab;
    roll := !roll + 1
  end

let unindent _ =
  new_line ();
  spaces := !spaces - !tab;
  if (!spaces <= 0) && (!roll > 0) then begin
    spaces := ((!max_indent - 1) / !tab) * !tab;
    roll := !roll - 1
  end

let space _ = commit ()

let print str =
  current := !current ^ str;
  current_len := !current_len + (String.length str);
  if (!spaces + !follow + !line_len + 1 + !current_len) > !width
  then begin
    if !line_len = 0 then commit ();
    flush ();
    output_char !out '\n';
    if !follow = 0 then follow := !tab
  end


(*
** Useful primitives
*)
let print_commas nl fct lst =
  let _ = List.fold_left
      (fun com elt ->
	 if com then begin
	   print ",";
	   if nl then new_line () else space ()
	 end else ();
	 fct elt;
	 true)
      false
      lst in
  ()


let escape_string str =
  let lng = String.length str in
  let conv value = String.make 1 (Char.chr (value +
			                    (if value < 10 then (Char.code '0') else (Char.code 'a' - 10)))) in
  let rec build idx =
    if idx >= lng then ""
    else
      let sub = String.sub str idx 1 in
      let res = match sub with
	  "\n" -> "\\n"
	| "\"" -> "\\\""
	| "'" -> "\\'"
	| "\r" -> "\\r"
	| "\t" -> "\\t"
	| "\b" -> "\\b"
	| "\000" -> "\\0"
	| _ -> if sub = (Char.escaped (String.get sub 0))
	  then sub
	  else let code = Char.code (String.get sub 0) in
	    "\\"
	    ^ (conv (code / 64))
	    ^ (conv ((code mod 64) / 8))
	    ^ (conv (code mod 8)) in
      res ^ (build (idx + 1)) in
  build 0

(*
** Base Type Printing
*)

let rec string_of_typ typ =
  match typ with
  | TVoid -> "void"
  | TBool -> "_Bool"
  | TChar -> "char"
  | TInt -> "int"
  | TArray t -> (string_of_typ t) ^ "[]"
  | TStruct id -> id
  (* print_fields ("struct " ^ id) flds *)
  (*| TPtr t -> print_base_type t *)
  | TFunction (t, args) ->
     (string_of_typ t) ^ "(*)" ^ "("
     ^ String.concat ", " (List.map (fun (t, s) -> string_of_typ t) args) ^ ")"
(* | TNamedType s -> print s *)
    
let rec print_base_type typ =
  match typ with
  | TVoid -> print "void"
  | TBool -> print "_Bool"
  | TChar -> print "char"
  | TInt -> print "int"
  | TArray t -> print_base_type t
  | TStruct id -> print id
  (* print_fields ("struct " ^ id) flds *)
  (*| TPtr t -> print_base_type t *)
  | TFunction (t, _) -> print_base_type t
(* | TNamedType s -> print s *)

and print_fields id (flds : (string * typ) list) =
  print id;
  if flds = []
  then ()
  else begin
    print " {";
    indent ();
    List.iter
      (fun (s, t) -> print_base_type t; print " ";
                     print_type (fun _ -> print s) t;
                     print ";"; new_line ())
      flds;
    unindent ();
    print "}"
  end

and print_enum id items =
  print ("enum " ^ id);
  if items = []
  then ()
  else begin
    print " {";
    indent ();
    print_commas
      true
      (fun (id, exp) -> print id;
	begin
	  space ();
	  print "= ";
	  print_expression exp 1
	end)
      items;
    unindent ();
    print "}";
  end


(*
** Declaration Printing
*)
and get_base_type typ =
  match typ with
  (* TPtr typ -> get_base_type typ *)
  | TArray typ -> get_base_type typ
  | _ -> typ

and print_pointer typ =
  match typ with
  (* TPtr typ -> print_pointer typ; print "*" *)
  (* | TFunction _ -> print "*" *)
  | TArray typ -> print_pointer typ
  | _ -> (*print_base_type typ*) ()

and print_array typ =
  match typ with
    TArray (typ) ->
    print_array typ;
    print "[]"
  | _ -> ()

(**	Print a type.
   @param fct	Function called to display the name of the.
   @param typ	Type to display.
*)
and print_type (fct : unit -> unit) (typ : typ ) =
  let base = get_base_type typ in
  match base with
  | TFunction (typ', pars) ->
    print_type
      (fun _ ->
	 if base <> typ then print "(";
	 print_pointer typ;
	 fct ();
	 print_array typ;
	 if base <> typ then print ")";
	 print "(";
	 print_params pars;
	 print ")")
      typ'
  | _ -> print_pointer typ; fct (); print_array typ

and print_onlytype typ =
  print_base_type typ;
  print_type (fun _ -> ()) typ

and print_single_name (typ, name) =
  print_base_type typ;
  space ();
  print_type (fun _ -> print name) typ

and print_params (pars : (typ * string) list)  =
  print_commas false print_single_name pars;

(*
** Expression printing
**		Priorities
**		16	variables
**		15	. -> [] call()
**		14  ++, -- (post)
**		13	++ -- (pre) ~ ! - + & *(cast)
**		12	* / %
**		11	+ -
**		10	<< >>
**		9	< <= > >=
**		8	== !=
**		7	&
**		6	^
**		5	|
**		4	&&
**		3	||
**		2	? :
**		1	= ?=
**		0	,
*)
and get_operator exp =
  match exp.edesc with
  | EUnop (op, _) ->
    (match op with
       UNeg -> ("-", 13)
     | UNot -> ("!", 13)
    (*  | UMemOf -> ("*", 13)
     | UAddrOf -> ("&", 13)
     | UPreIncr -> ("++", 13)
     | UPreDecr -> ("--", 13)
     | UPostIncr -> ("++", 14)
     | UPostDecr -> ("--", 14)
      *)
)
  | EBinop (op, _, _) ->
    (match op with
       BMul -> ("*", 12)
     | BDiv -> ("/", 12)
     | BAdd -> ("+", 11)
     | BSub -> ("-", 11)
     | BLt -> ("<", 9)
     | BLe -> ("<=", 9)
     | BGt -> (">", 9)
     | BGe -> (">=", 9)
     | BEq -> ("==", 8)
     | BNe -> ("!=", 8)
     | BAnd -> ("&&", 4)
     | BOr -> ("||", 3)
    )
  | ECall _ -> ("", 15)
  | EArrIndex _ -> ("", 15)
  | EField _ -> ("", 15)
  | EAssign _ -> ("", 1)
  | ECast _ -> ("", 0)
  | _ -> ("", 16)

and print_comma_exps exps =
  print_commas false (fun exp -> print_expression exp 1) exps

and print_expression (exp : 'a exp) (lvl : int) =
  let (txt, lvl') = get_operator exp in
  let _ = if lvl > lvl' then print "(" else () in
  let _ = match exp.edesc with
    | EUnop (op, exp') ->
      (* (match op with
	 UPostIncr | UPostDecr ->
	 print_expression exp' lvl';
	 print txt
       | _ -> *)
	 print txt;
	 print_expression exp' lvl'
    | EBinop (_, exp1, exp2) ->
      (*if (op = SUB) && (lvl <= lvl') then print "(";*)
      print_expression exp1 lvl';
      space ();
      print txt;
      space ();
      (*print_expression exp2 (if op = SUB then (lvl' + 1) else lvl');*)
      print_expression exp2 (lvl' + 1)      
    (*if (op = SUB) && (lvl <= lvl') then print ")"*)
    | EAssign (l, exp2) ->
      (*if (op = SUB) && (lvl <= lvl') then print "(";*)
      print_expression (lhs_to_exp exp2.eloc exp2.einfo l) lvl';
      space ();
      print "=";
      space ();
      (*print_expression exp2 (if op = SUB then (lvl' + 1) else lvl');*)
      print_expression exp2 (lvl' + 1)
    | ENewArray (t, n) ->
       print "new(";
       print_type (fun _ -> ()) t;
       print "[";
       print (string_of_int n);
       print "]";
       print ")"
    | ENewStruct s ->
       print "new(";
       print s;
       print ")"
    | EConst c -> print_constant c
    | ECall (exp, args) ->
      print_expression exp 16;
      print "(";
      print_comma_exps args;
      print ")"
    | EVar (name, _) ->
      print name
    | EArrIndex (exp, idx) ->
      print_expression exp 16;
      print "[";
      print_expression idx 0;
      print "]"
    | EField (exp, fld) ->
      print_expression exp 16;
      print ("." ^ fld)
    | ECast (exp, t) ->
       print "(";
       print (string_of_typ t);
       print ")";
       print_expression exp 0
  in
  if lvl > lvl' then print ")" else ()

and print_constant cst =
  match cst with
    CInt i ->
    print (string_of_int i)
  | CChar c ->
     print ("'" ^ (escape_string (String.make 1 c)) ^ "'")

(*
** Statement printing
*)
and print_statement stat =
  match stat.sdesc with
  | SDecl (s, t, e) ->
     print_base_type t;
     space ();
     print s;
     (match e with
      | Some e ->
         space ();
         print "=";
         space ();
         print_expression e 1
      | None -> ());
     print ";";
     new_line ();
  | SExp exp ->
    print_expression exp 0;
    print ";";
    new_line ()
  | SBlock (stats) ->
    new_line ();
    print "{";
    indent ();
    List.iter print_statement stats;
    unindent ();
    print "}";
    new_line ();
  | SIf (exp, s1, s2) ->
    print "if(";
    print_expression exp 0;
    print ")";
    print_substatement s1;
    if s2.sdesc = SBlock []
    then ()
    else begin
      print "else";
      print_substatement s2;
      end
  | SFor (e1, e2, e3, s) ->
     print "for(";
     print_expression e1 0;
     print "; ";
     print_expression e2 0;
     print "; ";
     print_expression e3 0;
     print ")";
     print_substatement s
  | SBreak ->
    print "break;"; new_line ()
  | SContinue ->
     print "continue;"; new_line ()
  | SReturn None -> print "return;"; new_line ()
  | SReturn (Some exp) ->
    print "return";
    print " ";
    print_expression exp 0;
    print ";";
    new_line ()

and print_gnu_asm_arg (id, desc, exp) =
  if id <> "" then print ("[" ^ id ^ "]");
  print ("\"" ^ (escape_string desc) ^ "\"(");
  print_expression exp 0;
  print ("\"")

and print_substatement stat =
  match stat.sdesc with
    SIf _ ->
    new_line ();
    print "{";
    indent ();
    print_statement stat;
    unindent ();
    print "}";
    new_line ();
  | SBlock _ ->
    print_statement stat
  | _ ->
    indent ();
    print_statement stat;
    unindent ()

(*
** Declaration printing
*)
and print_defs defs =
  let prev = ref false in
  List.iter
    (fun def ->
       (match def.ddesc with
	  DName _ -> prev := false
	| _ ->
	  if not !prev then force_new_line ();
	  prev := true);
       print_def def)
    defs

and print_def def =
  match def.ddesc with

    DFun (s, t, body) ->
     print_single_name (t, s);
     print_statement body;
     force_new_line ();

  | DName names ->
     List.iter
       (fun (s, t, e) ->
         print_base_type t;
         print " ";
         print s;
         (match e with
          | Some e -> print " = "; print_expression e 1
          | None -> ()
         );
         print ";";
         new_line ()
       )
       names

  | DTypeDef names ->
     print "typedef ";
     List.iter
       (fun (s, t) ->
         print_single_name (t, s);
         print ";";
         new_line ()
       )
       names;
     force_new_line ()

  | DStructDef (id, fields) ->
     print "struct ";
     print_fields (id) fields;
     print ";";
     force_new_line ()

(*  print abstrac_syntax -> ()
 **		Pretty printing the given abstract syntax program.
*)
let print (result : out_channel) (defs : 'a file) =
  out := result;
  print_defs defs

let set_tab t = tab := t
let set_width w = width := w
