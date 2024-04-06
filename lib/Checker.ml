open Syntax

let rec lift (from : Surface.typ) : Core.typ =
  match from with
  | Const { name = x } -> Const x
  | Arrow (a, b) -> Arrow (lift a, lift b)

let rec unify ~(expected : Core.typ) (actual : Core.typ) : unit =
  match (expected, actual) with
  | Const a, Const b ->
      if String.equal a b then () else report_mismatch expected actual
  | Arrow (a1, a2), Arrow (b1, b2) ->
      unify ~expected:a1 b1;
      unify ~expected:a2 b2
  | _, _ -> report_mismatch expected actual

and report_mismatch a b =
  Reporter.fatalf Type_error "type mismatched, expected: `%s`, got: `%s`"
    (Core.show_typ a) (Core.show_typ b)

let rec infer (tm : Surface.term) : Core.typ * Core.term =
  match tm with
  | Lambda _ -> Reporter.fatalf Type_error "cannot infer type of a lambda"
  | Var { name } -> (
      match Context.S.resolve [ name ] with
      | Some (ty, _) -> (ty, Var [ name ])
      | None ->
          Reporter.fatalf NoVar_error "failed to find `%s` in context" name)
  | App (a, b) -> (
      match infer a with
      | Arrow (t1, t2), a ->
          let tb, b = infer b in
          unify ~expected:t1 tb;
          (t2, App (a, b))
      | t, _ ->
          Reporter.fatalf Type_error "`%s` is not appliable type"
            (Core.show_typ t))

and check (tm : Surface.term) (ty : Core.typ) : Core.term =
  match (tm, ty) with
  | Lambda { param_name; body }, Arrow (pty, ty) ->
      Context.S.try_with ~shadow:Context.S.Silence.shadow @@ fun () ->
      Context.S.include_singleton ([ param_name ], (pty, `Local));
      let ty', body = infer body in
      unify ~expected:ty ty';
      Core.Lambda ([ param_name ], body)
  | tm, ty ->
      let ty', tm = infer tm in
      unify ~expected:ty ty';
      tm

let insert_constructor (data_type : Core.typ) (c : Surface.case) : unit =
  match c with
  | Case { name; params = tys } ->
      let tys = List.map lift tys in
      let ty = Core.build_app tys data_type in
      Eio.traceln "%s : %s" name (Core.show_typ ty);
      Context.S.include_singleton ~context_visible:`Visible
        ~context_export:`Export
        ([ name ], (ty, `Local));
      Environment.S.include_singleton ~context_visible:`Visible
        ~context_export:`Export
        ([ name ], (Spine (name, []), `Local))

let check_top (top : Surface.top) : unit =
  match top with
  | Data { name; cases } ->
      let data_type : Core.typ = Const name in
      List.iter (insert_constructor data_type) cases
  | Let { name; recursive; ty; body } ->
      if recursive then (* TODO: skip recursive part for now *)
        ()
      else
        let ty = lift ty in
        let body = check body ty in
        Context.S.include_singleton ~context_visible:`Visible
          ~context_export:`Export
          ([ name ], (ty, `Local));
        Environment.S.include_singleton ~context_visible:`Visible
          ~context_export:`Export
          ([ name ], (Eval.eval body, `Local));
        Eio.traceln "let %s = ... checked" name

let rec check_tree ~env : Surface.t -> unit =
 fun tops ->
  match tops with
  | [] -> ()
  | { loc; value = top } :: tops ->
      Reporter.merge_loc loc @@ fun () ->
      check_top top;
      check_tree ~env tops
