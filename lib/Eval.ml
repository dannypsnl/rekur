open Syntax

let rec eval (tm : Core.term) : Core.value =
  match tm with
  | Var x -> (
      match Environment.S.resolve x with
      | Some (v, _) -> v
      | None ->
          Reporter.fatalf NoVar_error "cannot find `%s` in environment"
            (String.concat " " x))
  | Lambda (x, body) ->
      Closure
        (fun v ->
          Environment.S.section [] @@ fun _ ->
          Environment.S.include_singleton (x, (v, `Local));
          eval body)
  | App (a, b) -> apply (eval a) (eval b)
  | Match (target, cases) ->
      let target = eval target in
      let v =
        List.fold_left
          (fun acc (Core.Case (pat, branch)) ->
            match acc with
            | None ->
                if eval_match target pat then Option.some @@ eval branch
                else Option.none
            | Some v -> Some v)
          None cases
      in
      Option.get v

and eval_match (target : Core.value) (pat : Core.pat) : bool =
  match pat with
  | PVar x ->
      Environment.S.section [] @@ fun _ ->
      Environment.S.include_singleton ([ x ], (target, `Local));
      true
  | Spine (head, ps) -> (
      match target with
      | Spine (h, tail) ->
          if h == head then
            List.fold_left2
              (fun acc p tail -> acc && eval_match tail p)
              true ps tail
          else false
      | Closure _ ->
          Reporter.fatal Eval_error "cannot pattern matching on closure")

and apply (v : Core.value) (arg : Core.value) : Core.value =
  match v with
  | Spine (h, tail) -> Spine (h, List.append tail [ arg ])
  | Closure f -> f arg
