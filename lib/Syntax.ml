open Yuujinchou

module Surface = struct
  exception BuildTermFromNothing

  open Range

  type typ = Const of { name : string } | Arrow of typ * typ

  and term =
    | Lambda of { param_name : string; body : term }
    | Var of { name : string }
    | App of term * term

  type top =
    | Let of { name : string; recursive : bool; ty : typ; body : term }
    | Data of { name : string; cases : case list }

  and case = Case of { name : string; params : typ list }

  type t = top located list

  let rec build_tm : term list -> term =
   fun ts ->
    match ts with
    | [] -> raise BuildTermFromNothing
    | [ x ] -> x
    | x :: xs -> App (x, build_tm xs)
end

module Core = struct
  type typ =
    | Const of string [@printer fun fmt -> fprintf fmt "%s"]
    | Arrow of typ * typ
        [@printer
          fun fmt (a, b) -> fprintf fmt "%s -> %s" (show_typ a) (show_typ b)]
    | Meta
  [@@deriving show]

  let rec build_app (tys : typ list) (result : typ) : typ =
    match tys with [] -> result | t :: ts -> Arrow (t, build_app ts result)

  type term =
    | Var of Trie.path
    | Lambda of Trie.path * term
    | App of term * term

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
end
