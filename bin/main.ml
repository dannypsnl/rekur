open Eio
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
  Cmd.v info
    Term.(const (fun filename -> process_file ~env filename) $ arg_file)

let rec repl ~stdin ~stdout : unit =
  let read_buf = Buf_read.of_flow stdin ~initial_size:100 ~max_size:1_000_000 in
  Buf_write.with_flow stdout @@ fun w ->
  let open Rekur in
  (* print a prompt and requires a term *)
  Buf_write.printf w "> ";
  let input : string = Buf_read.line read_buf in
  (* parse the input term *)
  let term = Parser.catcher Grammar.repl_term (Lexing.from_string input) in
  (* infer term *)
  let ty, term = Checker.infer term in
  (* run to get value *)
  let v = Eval.eval term in
  Buf_write.printf w "%s : %s\n"
    ([%show: Syntax.Core.value] v)
    ([%show: Syntax.Core.typ] ty);
  repl ~stdin ~stdout

let load_cmd ~env =
  let arg_file =
    let doc = "The program file to load." in
    Arg.required
    @@ Arg.pos 0 (Arg.some Arg.file) None
    @@ Arg.info [] ~docv:"PROG" ~doc
  in
  let doc = "Load input program file into REPL" in
  let man = [ `S Manpage.s_description; `P "" ] in
  let info = Cmd.info "load" ~version ~doc ~man in
  Cmd.v info
    Term.(
      const (fun filename ->
          process_file ~env filename;
          let stdin = Stdenv.stdin env in
          let stdout = Stdenv.stdout env in
          repl ~stdin ~stdout)
      $ arg_file)

let cmd ~env =
  let doc = "a simple programming language" in
  let man = [ `S Manpage.s_bugs; `S Manpage.s_authors; `P "Lîm Tsú-thuàn" ] in

  let info = Cmd.info "rekur" ~version ~doc ~man in
  Cmd.group info [ run_cmd ~env; load_cmd ~env ]

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
  let open Rekur.Environment.Handler in
  Rekur.Environment.S.run ~shadow ~not_found ~hook @@ fun () ->
  exit @@ Cmd.eval ~catch:false @@ cmd ~env
