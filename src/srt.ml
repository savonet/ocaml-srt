open Ctypes
open Unix_sys_socket

module Srt = Srt_stubs.Def(Srt_generated_stubs)

exception Error of Srt.errno*string

let check_err ret =
  if ret < 0 then
    match Srt.getlasterror (from_voidp Ctypes.int Ctypes.null) with
      | `Success -> assert false
      | errno -> 
           Srt.clearlasterror ();
           raise (Error (errno, Srt.getlasterror_str()))

include Srt

let socket _of _type _protocol =
  let socket = socket _of _type _protocol in
  check_err socket;
  socket

let create_socket () =
  let socket = create_socket () in
  check_err socket;
  socket

let bind socket sockaddr =
  let sockaddr =
    from_unix_sockaddr sockaddr
  in
  let len =
    match !@ (sockaddr |-> Sockaddr.sa_family) with
      | x when x = af_unix ->
        sizeof SockaddrUnix.t
      | x when x = af_inet ->
        sizeof SockaddrInet.t 
      | x when x = af_inet6 ->
        sizeof SockaddrInet6.t
      | _ -> assert false
  in
  check_err (bind socket sockaddr len)
