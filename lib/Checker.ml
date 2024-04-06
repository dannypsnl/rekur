open Syntax

let check_top (top : Surface.top) : unit =
  match top with Data _ -> () | Let _ -> ()

let rec check_tree ~env : Surface.t -> unit =
 fun tops ->
  match tops with
  | [] -> ()
  | { loc; value = top } :: tops ->
      Reporter.merge_loc loc @@ fun () ->
      check_top top;
      check_tree ~env tops
