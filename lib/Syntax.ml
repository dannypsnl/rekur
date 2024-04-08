open Yuujinchou

module Surface = struct
  exception BuildTermFromNothing

  open Range

  type typ =
    | Const of { name : Trie.path }
        [@printer fun fmt path -> fprintf fmt "%s" (String.concat "." path)]
    | Arrow of typ * typ

  and term =
    | Lambda of { param_name : string; body : term }
    | Var of { name : Trie.path }
        [@printer fun fmt path -> fprintf fmt "%s" (String.concat "." path)]
    | App of term * term
    | Match of { target : term; cases : case list }
  [@@deriving show]

  and case = Case of pat * term [@@deriving show]
  and pat = PVar of string | PApp of string * pat list [@@deriving show]

  type top =
    | Open of Trie.path
    | Let of { name : string; recursive : bool; ty : typ; body : term }
    | Data of { name : string; ctors : ctor list }

  and ctor = Ctor of { name : string; params : typ list }

  type t = { import_list : Trie.path list; top_list : top located list }

  let rec build_tm (ts : term list) : term =
    match ts with
    | [] -> raise BuildTermFromNothing
    | [ x ] -> x
    | [ x; y ] -> App (x, y)
    | x :: y :: xs -> App (App (x, y), build_tm xs)
end

module Core = struct
  type typ =
    | Type [@printer fun fmt _ -> fprintf fmt "Type"]
    | Const of Trie.path
        [@printer fun fmt path -> fprintf fmt "%s" (String.concat "." path)]
    | Arrow of typ * typ
        [@printer
          fun fmt (a, b) -> fprintf fmt "%s -> %s" (show_typ a) (show_typ b)]
    | Meta
  [@@deriving show]

  let rec build_app (tys : typ list) (result : typ) : typ =
    match tys with [] -> result | t :: ts -> Arrow (t, build_app ts result)

  type term =
    | Var of Trie.path
        [@printer fun fmt path -> fprintf fmt "%s" (String.concat "." path)]
    | Lambda of Trie.path * term
        [@printer
          fun fmt (path, tm) ->
            fprintf fmt "\\%s -> %s" (String.concat "." path) (show_term tm)]
    | App of term * term
    | Match of term * case list
  [@@deriving show]

  and case = Case of pat * term

  and pat =
    (* this is wildcard pattern *)
    | PVar of string
    (* this is constructor pattern *)
    | Spine of string * pat list
  [@@deriving show]

  type value =
    (* spine is a value for normal form, e.g. `suc zero` is `Span suc [Span zero []]` *)
    | Spine of string * value list
        [@printer
          fun fmt (head, tail) ->
            let tail = List.map show_value tail in
            if List.is_empty tail then fprintf fmt "%s" head
            else fprintf fmt "%s %s" head (String.concat " " tail)]
    | Closure of (value -> value)
        [@printer fun fmt _ -> fprintf fmt "<closure>"]
  [@@deriving show]

  let rec result_ty (ty : typ) : typ =
    match ty with Arrow (_, p) -> result_ty p | t -> t
end
