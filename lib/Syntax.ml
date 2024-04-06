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
    | Data of { name : string }

  type t = top located list

  let rec build_tm : term list -> term =
   fun ts ->
    match ts with
    | [] -> raise BuildTermFromNothing
    | [ x ] -> x
    | x :: xs -> App (x, build_tm xs)
end
