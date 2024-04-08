open Syntax

let rec lift (from : Surface.typ) : Core.typ =
  match from with
  | Const { name = x } -> (
      match Context.S.resolve x with
      | Some _ -> Const x
      | None ->
          Reporter.fatalf Type_error "no type named: `%s`" (String.concat "." x)
      )
  | Arrow (a, b) -> Arrow (lift a, lift b)

let path_equal a b =
  List.fold_left2 (fun acc x y -> acc && String.equal x y) true a b

let rec unify ~(expected : Core.typ) (actual : Core.typ) : unit =
  match (expected, actual) with
  | Const a, Const b ->
      if path_equal a b then () else report_mismatch expected actual
  | Arrow (a1, a2), Arrow (b1, b2) ->
      unify ~expected:a1 b1;
      unify ~expected:a2 b2
  | _, _ -> report_mismatch expected actual

and report_mismatch a b =
  Reporter.fatalf Type_error "type mismatched, expected: `%s`, got: `%s`"
    (Core.show_typ a) (Core.show_typ b)

let rec infer (tm : Surface.term) : Core.typ * Core.term =
  match tm with
  | Lambda { param_name; _ } ->
      Reporter.fatalf Type_error
        "cannot infer type of lambda term: `\\%s -> ...`" param_name
  | Var { name } -> (
      match Context.S.resolve name with
      | Some (ty, _) -> (ty, Var name)
      | None ->
          Reporter.fatalf NoVar_error "failed to find `%s` in context"
            (String.concat "." name))
  | App (a, b) -> (
      match infer a with
      | Arrow (t1, t2), a ->
          let tb, b = infer b in
          unify ~expected:t1 tb;
          (t2, App (a, b))
      | t, a ->
          Reporter.fatalf Type_error "`%s : %s` is not appliable"
            (Core.show_term a) (Core.show_typ t))
  | Match { target; cases } ->
      let target_ty, target = infer target in
      let tys : Core.typ list ref = ref [] in
      let cases =
        List.map
          (fun (Surface.Case (pat, branch)) ->
            let b_ty, branch = infer branch in
            tys := b_ty :: !tys;
            let pat = check_pattern target_ty pat in
            Core.Case (pat, branch))
          cases
      in
      let tys = !tys in
      let ty =
        List.fold_right
          (fun ty acc ->
            unify ~expected:acc ty;
            acc)
          (List.tl tys) (List.hd tys)
      in
      (ty, Match (target, cases))

and check_pattern (target_ty : Core.typ) (pat : Surface.pat) : Core.pat =
  match pat with
  | PVar x -> (
      match Context.S.resolve [ x ] with
      | Some (ty, `Constructor) ->
          unify ~expected:target_ty ty;
          Spine (x, [])
      | _ ->
          Context.S.include_singleton ([ x ], (target_ty, `Local));
          PVar x)
  | PApp (h, ps) -> (
      match Context.S.resolve [ h ] with
      | Some (ty, `Constructor) ->
          unify ~expected:target_ty (Core.result_ty ty);
          Spine (h, List.map (check_pattern target_ty) ps)
      | _ ->
          Reporter.fatalf Type_error "`%s` is not a constructor of `%s`" h
            (Core.show_typ target_ty))

and check (tm : Surface.term) (ty : Core.typ) : Core.term =
  match (tm, ty) with
  | Lambda { param_name; body }, Arrow (pty, ty) ->
      Context.S.try_with ~shadow:Context.S.Silence.shadow @@ fun () ->
      Context.S.include_singleton ([ param_name ], (pty, `Local));
      let body = check body ty in
      Core.Lambda ([ param_name ], body)
  | tm, Arrow _ ->
      Reporter.fatalf Type_error
        "cannot have non-lambda term `%s` under arrow type"
        (Surface.show_term tm)
  | tm, ty ->
      let ty', tm = infer tm in
      unify ~expected:ty ty';
      tm

let insert_constructor (data_type : Core.typ)
    (Ctor { name; params = tys } : Surface.ctor) : unit =
  let tys = List.map lift tys in
  let ty = Core.build_app tys data_type in
  Context.S.include_singleton ~context_visible:`Visible ~context_export:`Export
    ([ name ], (ty, `Constructor));
  Environment.S.include_singleton ~context_visible:`Visible
    ~context_export:`Export
    ([ name ], (Spine (name, []), `Local))

let rec process_file ~env ~working_dir source_path : Yuujinchou.Trie.path list =
  if Filename.extension source_path = ".kr" then (
    let { import_list; top_list } : Surface.t = Parser.parse_file source_path in
    (* 1. go to process all dependencies *)
    List.iter
      (fun path ->
        let _ =
          process_file ~env ~working_dir
            (Filename.concat working_dir (String.concat "/" path ^ ".kr"))
        in
        ())
      import_list;
    let module_name =
      Filename.chop_extension @@ Filename.basename source_path
    in
    Context.S.section [ module_name ] @@ fun () ->
    Environment.S.section [ module_name ] @@ fun () ->
    (* 2. apply import statements *)
    List.iter
      (fun p ->
        (Context.S.modify_visible
        @@ Yuujinchou.Language.(union [ all; renaming p [] ]));
        Environment.S.modify_visible
        @@ Yuujinchou.Language.(union [ all; renaming p [] ]))
      import_list;
    (* 3. start checking top-level definitions *)
    List.iter check_top top_list;
    import_list)
  else
    Reporter.fatalf IO_error
      "`%s` is not proper, a proper source file should be `*.kr`" source_path

and check_top ({ loc; value = top } : Surface.top Range.located) : unit =
  Reporter.merge_loc loc @@ fun () ->
  match top with
  | Open path ->
      (Context.S.modify_visible
      @@ Yuujinchou.Language.(union [ all; renaming path [] ]));
      Environment.S.modify_visible
      @@ Yuujinchou.Language.(union [ all; renaming path [] ])
  | Data { name; ctors } ->
      Context.S.include_singleton ~context_visible:`Visible
        ~context_export:`Export
        ([ name ], (Type, `Local));
      let data_type : Core.typ = Const [ name ] in
      List.iter (insert_constructor data_type) ctors
  | Let { name; recursive; ty; body } ->
      if recursive then (
        let ty = lift ty in
        Context.S.include_singleton ~context_visible:`Visible
          ~context_export:`Export
          ([ name ], (ty, `Local));
        let body = check body ty in
        Environment.S.include_singleton ~context_visible:`Visible
          ~context_export:`Export
          ([ name ], (Eval.eval body, `Local)))
      else
        let ty = lift ty in
        let body = check body ty in
        Context.S.include_singleton ~context_visible:`Visible
          ~context_export:`Export
          ([ name ], (ty, `Local));
        Environment.S.include_singleton ~context_visible:`Visible
          ~context_export:`Export
          ([ name ], (Eval.eval body, `Local))
