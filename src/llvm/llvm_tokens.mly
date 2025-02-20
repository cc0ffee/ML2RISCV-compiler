%token <string> KEYWORD
%token <string> LVAR
%token <string> GVAR
%token <string> TSTRUCT
%token <int> CONST
%token STAR
%token NULL
%token LPAREN RPAREN
%token DEFINE LSQ RSQ
%token DECLARE
%token LBRACE RBRACE
%token ADD SUB MUL DIV AND OR XOR
%token BITCAST ZEXT PTRTOINT INTTOPTR TRUNC
%token ICMP BR RET CALL GETELEMENTPTR ALLOCA LOAD STORE PHI
%token TO
%token <int> TINT
%token TVOID LABEL
%token COMMA
%token COLON
%token EQUAL
%token TYP
%token EOF

%%

%%
