%{
  [@@@coverage exclude_file]
  open Syntax.Surface
%}

%token <string> IDENT
(* keywords *)
%token DATA
       LET
       REC
       FUN
%token LPAREN RPAREN
       SEMICOLON
       COLON
       ASSIGN
       ARROW
%token EOF

%start <t> main
%%

let loc(p) ==
  | x = p; { Asai.Range.locate_lex $loc x }

let parens(p) == delimited(LPAREN, p, RPAREN)

let typ :=
  | name=IDENT; { Const { name } }
  | t1=typ; ARROW; t2=typ; <Arrow>

let app :=
  | ts=list(tm); { build_tm ts }

let tm :=
  | FUN; name=IDENT; ARROW; tm=tm; { Lambda {
      param_name = name; body = tm
    } }
  | name=IDENT; { Var { name } }
  | parens(app)

let top_level :=
  | DATA; name=IDENT;
    { Data {
        name
    } }
  | LET; name=IDENT; COLON; ty=typ;
    ASSIGN; ts=list(tm);
    { Let { name; recursive = false; ty; body = build_tm ts } }
  | LET; REC; name=IDENT; COLON; ty=typ;
    ASSIGN; ts=list(tm);
    { Let { name; recursive = false; ty; body = build_tm ts } }

let main :=
| ~ = list(loc(top_level)); EOF; <>
