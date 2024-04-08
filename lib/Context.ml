open Yuujinchou
open Bwd

type modifier_cmd = Trace

module TypeContext = struct
  type data = Syntax.Core.typ
  type tag = [ `Imported | `Local | `Constructor ]
  type hook = modifier_cmd
  type context = [ `Visible | `Export ]
end

module S = Scope.Make (TypeContext)

(* Handle scoping effects *)
module Handler = struct
  open Syntax.Core

  let pp_path fmt = function
    | Emp -> Format.pp_print_string fmt "(root)"
    | path -> Format.pp_print_string fmt @@ String.concat "." (Bwd.to_list path)

  let pp_context fmt = function
    | Some `Visible -> Format.pp_print_string fmt " in the visible namespace"
    | Some `Export -> Format.pp_print_string fmt " in the export namespace"
    | None -> ()

  let pp_item fmt = function
    | x, `Imported -> Format.fprintf fmt "%s (imported)" (show_typ x)
    | x, `Local -> Format.fprintf fmt "%s (local)" (show_typ x)
    | x, `Constructor -> Format.fprintf fmt "%s (constructor)" (show_typ x)

  let shadow context path x y =
    Eio.traceln "shadowing, Γ ⊢ %a : %a -> %a%a.@." pp_path path pp_item x
      pp_item y pp_context context;
    y

  let not_found context prefix =
    Eio.traceln
      "[Warning] Could not find any data within the subtree at %a%a.@." pp_path
      prefix pp_context context

  let hook context prefix hook input =
    match hook with
    | Trace ->
        Eio.traceln "@[<v 2>[Info] Got the following bindings at %a%a:@;"
          pp_path prefix pp_context context;
        Trie.iter
          (fun path x -> Eio.traceln "%a => %a@;" pp_path path pp_item x)
          input;
        Eio.traceln "@]@.";
        input
end
