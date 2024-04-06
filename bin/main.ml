open Eio

let () =
  Eio_main.run @@ fun env ->
  let stdout = Stdenv.stdout env in
  Buf_write.with_flow stdout @@ fun w -> Buf_write.string w "hello"
