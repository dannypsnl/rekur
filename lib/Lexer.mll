{
  exception SyntaxError of string
  let ident str = Grammar.IDENT str
  let illegal str = raise @@ SyntaxError str

  let dbg str = Format.printf "%s\n" str; flush stdout

  let return _lexbuf tok = tok
}

let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let int = '-'? digit+
let ident = (alpha) (alpha|digit|'_'|'-')*
let whitespace = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"

rule token =
  parse
  | "#" { comment lexbuf }
  | "data" { return lexbuf @@ Grammar.DATA }
  | "let" { return lexbuf @@ Grammar.LET }
  | "rec" { return lexbuf @@ Grammar.LET }
  | "\\" { return lexbuf @@ Grammar.FUN }
  | ident { return lexbuf @@ ident (Lexing.lexeme lexbuf) }
  | "->" { return lexbuf @@ Grammar.ARROW }
  | ':' { return lexbuf @@ Grammar.COLON }
  | ":=" { return lexbuf @@ Grammar.ASSIGN }
  | '(' { return lexbuf @@ Grammar.LPAREN }
  | ')' { return lexbuf @@ Grammar.RPAREN }
  | whitespace { token lexbuf }
  | newline { Lexing.new_line lexbuf; token lexbuf }
  | eof { Grammar.EOF }
  | _ { illegal @@ Lexing.lexeme lexbuf }

and comment =
  parse
  | newline { Lexing.new_line lexbuf; token lexbuf }
  | eof { Grammar.EOF }
  | _ { comment lexbuf }
