module Srt = Srt_stubs.Def(Srt_generated_stubs)

exception Error of Srt.errno*string

let check_err ret =
  match Srt.getlasterror (Ctypes.from_voidp Ctypes.int Ctypes.null) with
    | `Success -> ret
    | errno -> raise (Error (errno, Srt.getlasterror_str()))

include Srt

let socket _of _type _protocol =
  check_err (socket _of _type _protocol)

let create_socket () =
  check_err (create_socket ())
