open Cmdliner
module Tty = Asai.Tty.Make (Rekur.Reporter.Message)

let version =
  Format.asprintf "%s"
  @@
  match Build_info.V1.version () with
  | None -> "n/a"
  | Some v -> Build_info.V1.Version.to_string v

let process_file ~env source_path =
  if Filename.extension source_path = ".kr" then
    let tree = Rekur.Parser.parse_file source_path in
    Rekur.Checker.check_tree ~env tree
  else
    Rekur.Reporter.fatalf IO_error
      "`%s` is not proper, a proper source file should be `*.kr`" source_path

let run ~env filename = process_file ~env filename

let run_cmd ~env =
  let arg_file =
    let doc = "The program file to run." in
    Arg.required
    @@ Arg.pos 0 (Arg.some Arg.file) None
    @@ Arg.info [] ~docv:"PROG" ~doc
  in
  let doc = "Run input program file" in
  let man = [ `S Manpage.s_description; `P "" ] in
  let info = Cmd.info "run" ~version ~doc ~man in
  Cmd.v info Term.(const (run ~env) $ arg_file)

let cmd ~env =
  let doc = "a simple programming language" in
  let man = [ `S Manpage.s_bugs; `S Manpage.s_authors; `P "Lîm Tsú-thuàn" ] in

  let info = Cmd.info "rekur" ~version ~doc ~man in
  Cmd.group info [ run_cmd ~env ]

let () =
  let fatal diagnostics =
    Tty.display diagnostics;
    exit 1
  in
  Printexc.record_backtrace true;
  Eio_main.run @@ fun env ->
  Rekur.Reporter.run ~emit:Tty.display ~fatal @@ fun () ->
  let open Rekur.Context.Handler in
  Rekur.Context.S.run ~shadow ~not_found ~hook @@ fun () ->
  exit @@ Cmd.eval ~catch:false @@ cmd ~env
