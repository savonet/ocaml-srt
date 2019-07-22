open Ctypes
open Unix_sys_socket

module Srt = Srt_stubs.Def(Srt_generated_stubs)

exception Invalid_argument of string
exception Error of Srt.errno*string

let check_err ret =
  if ret = -1 then
   begin
    match Srt.getlasterror (from_voidp Ctypes.int Ctypes.null) with
      | `Success -> assert false
      | errno -> 
           Srt.clearlasterror ();
           raise (Error (errno, Srt.getlasterror_str()))
   end;
  ret

include Srt

type 'a socket_opt = [
  | `Messageapi
  | `Transtype
]

let messageapi = `Messageapi
let transtype = `Transtype

let srtt_live = Int64.to_int srtt_live
let srtt_file = Int64.to_int srtt_file
let srtt_invalid = Int64.to_int srtt_invalid

let int_of_transtype = function
  | `Live    -> srtt_live
  | `File    -> srtt_file
  | `Invalid -> srtt_invalid

let transtype_of_int x = function
  | x when x = srtt_live -> `Live
  | x when x = srtt_file -> `File
  | x when x = srtt_invalid -> `Invalid
  | _ -> raise (Invalid_argument ("Invalid transtype value: " ^ (string_of_int x)))

let socket _of _type _protocol =
  let _of = match _of with
    | Unix.PF_UNIX ->
        raise (Invalid_argument "PF_UNIX is not supported")
    | Unix.PF_INET ->
        af_inet
    | Unix.PF_INET6 ->
        af_inet6
  in
  let _type = match _type with
    | Unix.SOCK_DGRAM ->
        sock_dgram
    | Unix.SOCK_STREAM ->
        sock_stream
    | Unix.SOCK_RAW ->
        raise (Invalid_argument "SOCK_RAW is not supported")
    | Unix.SOCK_SEQPACKET ->
        sock_seqpacket
  in
  check_err (socket _of _type _protocol)

let apply_sockaddr fn sockaddr =
  let sockaddr =
    from_unix_sockaddr sockaddr
  in
  let len =
    sizeof sockaddr_storage_t
  in
  fn sockaddr len

let bind socket socketaddr =
  ignore(check_err(apply_sockaddr (bind socket) socketaddr))

let connect socket socketaddr =
  ignore(check_err(apply_sockaddr (connect socket) socketaddr))

let accept socket =
  let sockaddr =
    allocate_n sockaddr_storage_t ~count:(sizeof sockaddr_storage_t)
  in
  let socklen =
    allocate int (sizeof sockaddr_storage_t)
  in
  let socket =
    check_err(accept socket sockaddr socklen);
  in
  socket, to_unix_sockaddr sockaddr

let rendez_vous socket sockaddr1 sockaddr2 =
  ignore(check_err(
    apply_sockaddr
      (apply_sockaddr (rendez_vous socket) sockaddr1)
      sockaddr2))

let listen sock backlog =
  ignore(check_err (listen sock backlog))

let send sock msg =
  check_err(send sock msg (String.length msg))

let sendmsg sock msg b v =
  check_err(sendmsg sock msg (String.length msg) b v)

let recv sock buf len =
  if Bytes.length buf < len then
    raise (Invalid_argument "buffer too short!");
  check_err(recv sock (ocaml_bytes_start buf) len)

let recvmsg = recv

let getsockflag sock opt =
  let arg = allocate int 0 in
  let arglen = allocate int (sizeof int) in
  ignore(check_err(getsockflag sock opt (to_voidp arg) arglen));
  let arg = !@ arg in
  match opt with
    | `Messageapi ->
          Obj.magic (arg <> 0)
    | `Transtype ->
          Obj.magic (transtype_of_int arg)        

let setsockflag sock opt v =
  let f t v =
    to_voidp (allocate t v)
  in 
  let arg, arglen =
    match opt with
      | `Messageapi ->
          let v = if (Obj.magic v) then 1 else 0 in
          f int v, sizeof int
      | `Transtype ->
          let transtype = int_of_transtype (Obj.magic v) in
          f int transtype, sizeof int
  in
  ignore(check_err(setsockflag sock opt arg arglen))

let close s =
  ignore(check_err(close s))
