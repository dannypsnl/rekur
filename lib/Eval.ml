open Syntax

let rec eval (tm : Core.term) : Core.value =
  match tm with
  | Var x -> (
      match Environment.S.resolve x with
      | Some (v, _) -> v
      | None ->
          Reporter.fatalf NoVar_error "failed to find `%s` in context"
            (String.concat " " x))
  | Lambda (x, body) ->
      Closure
        (fun v ->
          Environment.S.section [] @@ fun _ ->
          Environment.S.include_singleton (x, (v, `Local));
          eval body)
  | App (a, b) -> apply (eval a) (eval b)

and apply (v : Core.value) (arg : Core.value) : Core.value =
  match v with
  | Spine (h, tail) -> Spine (h, List.append tail [ arg ])
  | Closure f -> f arg
