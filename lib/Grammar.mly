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
       MATCH
       IMPORT OPEN
%token LPAREN RPAREN
       COLON
       VERT
       ASSIGN
       ARROW
       DOT
%token EOF

%start <t> main
%start <term> repl_term
%%

let loc(p) ==
  | x = p; { Asai.Range.locate_lex $loc x }

let parens(p) == delimited(LPAREN, p, RPAREN)

let typ :=
  | name=separated_nonempty_list(DOT, IDENT); { Const { name } }
  | t1=typ; ARROW; t2=typ; <Arrow>

let pat :=
  | ~=IDENT; ~=nonempty_list(pat); <Spine>
  | ~=IDENT; <PVar>
let case :=
  | VERT; ~=pat; ARROW; ~=tm; <Case>

let multi_tm :=
  | ts=nonempty_list(tm); { build_tm ts }
let tm :=
  | FUN; name=IDENT; ARROW; tm=tm; { Lambda { param_name = name; body = tm } }
  | MATCH; target=tm; cases=list(case); { Match { target; cases } }
  | name=separated_nonempty_list(DOT, IDENT); { Var { name } }
  | parens(multi_tm)

let ctor :=
  | VERT; name=IDENT; params=list(typ); { Ctor { name; params } }

let top_level :=
  | OPEN; ~=separated_nonempty_list(DOT, IDENT); <Open>
  | DATA; name=IDENT; ctors=list(ctor); { Data { name; ctors } }
  | LET; name=IDENT; COLON; ty=typ;
    ASSIGN; body=multi_tm;
    { Let { name; recursive = false; ty; body } }
  | LET; REC; name=IDENT; COLON; ty=typ;
    ASSIGN; body=multi_tm;
    { Let { name; recursive = true; ty; body } }

let repl_term :=
  | ts=list(tm); EOF; { build_tm ts }

let import :=
  | IMPORT; ~=separated_nonempty_list(DOT, IDENT); <>
let main :=
  | import_list=list(import); top_list=list(loc(top_level)); EOF;
    { { import_list; top_list } }
