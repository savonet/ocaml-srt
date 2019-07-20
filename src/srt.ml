open Ctypes
open Unix_sys_socket

module Srt = Srt_stubs.Def(Srt_generated_stubs)

exception Invalid_argument of string
exception Error of Srt.errno*string

let check_err ret =
  if ret < 0 then
   begin
    match Srt.getlasterror (from_voidp Ctypes.int Ctypes.null) with
      | `Success -> assert false
      | errno -> 
           Srt.clearlasterror ();
           raise (Error (errno, Srt.getlasterror_str()))
   end;
  ret

include Srt

let get_constant name =
  Int64.to_int
    (Srt_top_level_generated_types.constant name int64_t)

type 'a socket_opt = [
  | `Messageapi
  | `Transtype
]

let stro_messageapi =
  get_constant "SRTO_MESSAGEAPI"
let srto_transtype =
  get_constant "SRTO_TRANSTYPE"

let int_of_socket_opt = function
  | `Messageapi -> stro_messageapi
  | `Transtype -> srto_transtype

let messageapi = `Messageapi
let transtype = `Transtype

let srtt_live =
  get_constant "SRTT_LIVE"
let srtt_file =
  get_constant "SRTT_FILE"
let srtt_invalid =
  get_constant "SRTT_INVALID"

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
    match !@ (sockaddr |-> Sockaddr.sa_family) with
      | x when x = af_unix ->
        sizeof SockaddrUnix.t
      | x when x = af_inet ->
        sizeof SockaddrInet.t 
      | x when x = af_inet6 ->
        sizeof SockaddrInet6.t
      | _ -> assert false
  in
  fn sockaddr len

let bind socket socketaddr =
  ignore(check_err(apply_sockaddr (bind socket) socketaddr))

let connect socket socketaddr =
  ignore(check_err(apply_sockaddr (connect socket) socketaddr))

let accept socket sockaddr =
  let accept socket sockaddr socklen =
    accept socket sockaddr (allocate int socklen)
  in
  check_err(apply_sockaddr (accept socket) sockaddr)

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
  let _opt = int_of_socket_opt opt in
  let arg = allocate int 0 in
  let arglen = allocate int (sizeof int) in
  ignore(check_err(getsockflag sock _opt (to_voidp arg) arglen));
  let arg = !@ arg in
  match opt with
    | `Messageapi ->
          Obj.magic (arg <> 0)
    | `Transtype ->
          Obj.magic (transtype_of_int arg)        

let setsockflag sock opt v =
  let _opt = int_of_socket_opt opt in
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
  ignore(check_err(setsockflag sock _opt arg arglen))
