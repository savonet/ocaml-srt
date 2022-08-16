open Ctypes
open Posix_socket
module Srt = Srt_stubs.Def (Srt_generated_stubs)
open Srt
module Srt_locked = Srt_stubs_locked.Def (Srt_generated_stubs_locked)
open Srt_locked
open Unsigned

exception Invalid_argument of string
exception Error of errno * string

type errno = Srt.errno
type transtype = Srt.transtype
type socket_status = Srt.socket_status
type socket = Srt.socket

let resources = Hashtbl.create 0

let string_of_errno = function
  | `Easyncfail -> "Easyncfail"
  | `Easyncrcv -> "Easyncrcv"
  | `Easyncsnd -> "Easyncsnd"
  | `Eboundsock -> "Eboundsock"
  | `Econgest -> "Econgest"
  | `Econnfail -> "Econnfail"
  | `Econnlost -> "Econnlost"
  | `Econnrej -> "Econnrej"
  | `Econnsetup -> "Econnsetup"
  | `Econnsock -> "Econnsock"
  | `Eduplisten -> "Eduplisten"
  | `Efile -> "Efile"
  | `Einvalbufferapi -> "Einvalbufferapi"
  | `Einvalmsgapi -> "Einvalmsgapi"
  | `Einvop -> "Einvop"
  | `Einvparam -> "Einvparam"
  | `Einvpollid -> "Einvpollid"
  | `Einvrdoff -> "Einvrdoff"
  | `Einvsock -> "Einvsock"
  | `Einvwroff -> "Einvwroff"
  | `Elargemsg -> "Elargemsg"
  | `Enobuf -> "Enobuf"
  | `Enoconn -> "Enoconn"
  | `Enolisten -> "Enolisten"
  | `Enoserver -> "Enoserver"
  | `Epeererr -> "Epeererr"
  | `Epollempty -> "Epollempty"
  | `Erdperm -> "Erdperm"
  | `Erdvnoserv -> "Erdvnoserv"
  | `Erdvunbound -> "Erdvunbound"
  | `Eresource -> "Eresource"
  | `Esclosed -> "Esclosed"
  | `Esecfail -> "Esecfail"
  | `Esockfail -> "Esockfail"
  | `Esysobj -> "Esysobj"
  | `Ethread -> "Ethread"
  | `Etimeout -> "Etimeout"
  | `Eunboundsock -> "Eunboundsock"
  | `Eunknown -> "Eunknown"
  | `Ewrperm -> "Ewrperm"
  | `Success -> "Success"

let () =
  Printexc.register_printer (function
    | Error (errno, msg) ->
        Some (Printf.sprintf "Error(%s,%s)" (string_of_errno errno) msg)
    | _ -> None)

let check_err ret =
  if ret = -1 then begin
    match getlasterror (from_voidp Ctypes.int Ctypes.null) with
      | `Success -> assert false
      | errno ->
          let msg = getlasterror_str () in
          clearlasterror ();
          raise (Error (errno, msg))
  end;
  ret

type _ socket_opt =
  | Messageapi : bool socket_opt
  | Payloadsize : int socket_opt
  | Transtype : transtype socket_opt
  | Rcvsyn : bool socket_opt
  | Sndsyn : bool socket_opt
  | Conntimeo : int socket_opt
  | Rcvtimeo : int socket_opt
  | Sndtimeo : int socket_opt
  | Reuseaddr : bool socket_opt
  | Rcvbuf : int socket_opt
  | Sndbuf : int socket_opt
  | Udp_rcvbuf : int socket_opt
  | Udp_sndbuf : int socket_opt
  | Enforced_encryption : bool socket_opt
  | Passphrase : string socket_opt
  | Pbkeylen : int socket_opt
  | Streamid : string socket_opt

let messageapi = Messageapi
let payloadsize = Payloadsize
let transtype = Transtype
let rcvsyn = Rcvsyn
let sndsyn = Sndsyn
let conntimeo = Conntimeo
let rcvtimeo = Rcvtimeo
let sndtimeo = Sndtimeo
let reuseaddr = Reuseaddr
let rcvbuf = Rcvbuf
let sndbuf = Sndbuf
let udp_rcvbuf = Udp_rcvbuf
let udp_sndbuf = Udp_sndbuf
let enforced_encryption = Enforced_encryption
let passphrase = Passphrase
let pbkeylen = Pbkeylen
let streamid = Streamid

let srt_socket_opt_of_socket_opt (type a) : a socket_opt -> Srt.socket_opt =
  function
  | Messageapi -> `Messageapi
  | Payloadsize -> `Payloadsize
  | Transtype -> `Transtype
  | Rcvsyn -> `Rcvsyn
  | Sndsyn -> `Sndsyn
  | Conntimeo -> `Conntimeo
  | Rcvtimeo -> `Rcvtimeo
  | Sndtimeo -> `Sndtimeo
  | Reuseaddr -> `Reuseaddr
  | Rcvbuf -> `Rcvbuf
  | Sndbuf -> `Sndbuf
  | Udp_rcvbuf -> `Udp_rcvbuf
  | Udp_sndbuf -> `Udp_sndbuf
  | Enforced_encryption -> `Enforced_encryption
  | Pbkeylen -> `Pbkeylen
  | Passphrase -> `Passphrase
  | Streamid -> `Streamid

let srtt_live = Int64.to_int srtt_live
let srtt_file = Int64.to_int srtt_file
let srtt_invalid = Int64.to_int srtt_invalid

let int_of_transtype = function
  | `Live -> srtt_live
  | `File -> srtt_file
  | `Invalid -> srtt_invalid

let transtype_of_int = function
  | x when x = srtt_live -> `Live
  | x when x = srtt_file -> `File
  | x when x = srtt_invalid -> `Invalid
  | x ->
      raise (Invalid_argument ("Invalid transtype value: " ^ string_of_int x))

open Ctypes

let apply_sockaddr fn sockaddr =
  let sockaddr = from_unix_sockaddr sockaddr in
  let len = sizeof sockaddr_in_t in
  fn sockaddr len

let bind socket socketaddr =
  ignore (check_err (apply_sockaddr (bind socket) socketaddr))

let connect socket socketaddr =
  ignore (check_err (apply_sockaddr (connect socket) socketaddr))

let accept socket =
  let sockaddr = allocate_n sockaddr_t ~count:(sizeof sockaddr_storage_t) in
  let socklen = allocate int (sizeof sockaddr_t) in
  let socket = check_err (accept socket sockaddr socklen) in
  (socket, to_unix_sockaddr sockaddr)

let rendez_vous socket sockaddr1 sockaddr2 =
  ignore
    (check_err
       (apply_sockaddr
          (apply_sockaddr (rendez_vous socket) sockaddr1)
          sockaddr2))

type listen_callback = socket -> int -> Unix.sockaddr -> string option -> bool

let listen_callback sock fn =
  let fn _ s hs_version peeraddr streamid =
    let streamid =
      if is_null streamid then None
      else Some (string_from_ptr streamid ~length:(strlen streamid))
    in
    match fn s hs_version (to_unix_sockaddr peeraddr) streamid with
      | true -> 0
      | false | (exception _) -> -1
  in
  let ptr = Srt_stubs.ListenCallback.of_fun fn in
  let cleanup () = Srt_stubs.ListenCallback.free ptr in
  (try ignore (check_err (listen_callback sock ptr null))
   with exn ->
     let bt = Printexc.get_raw_backtrace () in
     cleanup ();
     Printexc.raise_with_backtrace exn bt);
  Hashtbl.add resources sock cleanup

let listen sock backlog = ignore (check_err (listen sock backlog))

let send sock msg =
  check_err (send sock (Bytes.unsafe_to_string msg) (Bytes.length msg))

let sendmsg sock msg b v =
  check_err (sendmsg sock (Bytes.unsafe_to_string msg) (Bytes.length msg) b v)

let mk_recv fn sock buf len =
  if Bytes.length buf < len then raise (Invalid_argument "buffer too short!");
  let ptr = allocate_n char ~count:len in
  let length = check_err (fn sock ptr len) in
  memcpy (ocaml_bytes_start buf) ptr length;
  length

let recv = mk_recv recv
let recvmsg = mk_recv recvmsg

let getsockflag : type a. socket -> a socket_opt -> a =
 fun sock opt ->
  let arg = allocate int 0 in
  let arglen = allocate int (sizeof int) in
  ignore
    (check_err
       (getsockflag sock
          (srt_socket_opt_of_socket_opt opt)
          (to_voidp arg) arglen));
  let to_bool () = !@arg <> 0 in
  let to_int () = !@arg in
  let to_string () =
    let arg = coerce (ptr int) (ptr char) arg in
    string_from_ptr arg ~length:!@arglen
  in
  match opt with
    | Enforced_encryption -> to_bool ()
    | Rcvsyn -> to_bool ()
    | Sndsyn -> to_bool ()
    | Reuseaddr -> to_bool ()
    | Messageapi -> to_bool ()
    | Conntimeo -> to_int ()
    | Rcvtimeo -> to_int ()
    | Sndtimeo -> to_int ()
    | Rcvbuf -> to_int ()
    | Sndbuf -> to_int ()
    | Udp_rcvbuf -> to_int ()
    | Pbkeylen -> to_int ()
    | Udp_sndbuf -> to_int ()
    | Payloadsize -> to_int ()
    | Transtype -> transtype_of_int !@arg
    | Passphrase -> to_string ()
    | Streamid -> to_string ()

let setsockflag : type a. socket -> a socket_opt -> a -> unit =
 fun sock opt v ->
  let f t v = to_voidp (allocate t v) in
  let of_bool v =
    let v = if v then 1 else 0 in
    (f int v, sizeof int)
  in
  let of_int v = (f int v, sizeof int) in
  let of_string v =
    let len = String.length v in
    let ptr = allocate_n char ~count:len in
    memcpy_str ptr (ocaml_string_start v) len;
    (to_voidp ptr, len)
  in
  let arg, arglen =
    match opt with
      | Enforced_encryption -> of_bool v
      | Rcvsyn -> of_bool v
      | Sndsyn -> of_bool v
      | Reuseaddr -> of_bool v
      | Messageapi -> of_bool v
      | Conntimeo -> of_int v
      | Rcvtimeo -> of_int v
      | Sndtimeo -> of_int v
      | Rcvbuf -> of_int v
      | Sndbuf -> of_int v
      | Udp_rcvbuf -> of_int v
      | Pbkeylen -> of_int v
      | Udp_sndbuf -> of_int v
      | Payloadsize -> of_int v
      | Transtype ->
          let transtype = int_of_transtype v in
          (f int transtype, sizeof int)
      | Passphrase -> of_string v
      | Streamid -> of_string v
  in
  ignore
    (check_err (setsockflag sock (srt_socket_opt_of_socket_opt opt) arg arglen))

let close s =
  ignore (check_err (close s));
  List.iter (fun fn -> fn ()) (Hashtbl.find_all resources s);
  Hashtbl.remove resources s

let getsockstate = getsockstate
let create_socket = create_socket
let cleanup = cleanup
let startup = startup

module Poll = struct
  type t = int
  type flag = Srt.poll_flag
  type event = { fd : socket; events : flag list }

  let create = epoll_create

  let add_usock ?flags eid s =
    let flags =
      match flags with
        | None -> Ctypes.(from_voidp int null)
        | Some flags ->
            let flags =
              List.fold_left
                (fun cur flag -> cur lor Int64.to_int (poll_flag_of_flag flag))
                0 flags
            in
            allocate Ctypes.int flags
    in
    ignore (check_err (epoll_add_usock eid s flags))

  let remove_usock eid s = ignore (check_err (epoll_remove_usock eid s))

  let update_usock ?flags eid s =
    let flags =
      match flags with
        | None -> Ctypes.(from_voidp int null)
        | Some flags ->
            let flags =
              List.fold_left
                (fun cur flag -> cur lor Int64.to_int (poll_flag_of_flag flag))
                0 flags
            in
            allocate Ctypes.int flags
    in
    ignore (check_err (epoll_update_usock eid s flags))

  let release eid = ignore (check_err (epoll_release eid))

  let uwait eid ~max_fds ~timeout =
    let timeout = Int64.of_int timeout in
    let events = CArray.make PollEvent.t max_fds in
    let nb_events =
      check_err (epoll_uwait eid (CArray.start events) max_fds timeout)
    in
    let events = CArray.to_list (CArray.sub events ~pos:0 ~length:nb_events) in
    List.map
      (fun event ->
        let fd = getf event PollEvent.fd in
        let events = getf event PollEvent.events in
        let events =
          List.fold_left
            (fun cur flag ->
              let poll_flag = Int64.to_int (poll_flag_of_flag flag) in
              if poll_flag land events <> 0 then flag :: cur else cur)
            [] [`Read; `Write; `Error]
        in
        { fd; events })
      events

  let wait eid ~max_read ~max_write ~timeout =
    let timeout = Int64.of_int timeout in
    let read_len = allocate int max_read in
    let read = CArray.make int max_read in
    let write_len = allocate int max_write in
    let write = CArray.make int max_write in
    ignore
      (check_err
         (epoll_wait eid (CArray.start read) read_len (CArray.start write)
            write_len timeout null null null null));
    let read = CArray.to_list (CArray.sub read ~pos:0 ~length:!@read_len) in
    let write = CArray.to_list (CArray.sub write ~pos:0 ~length:!@write_len) in
    (read, write)
end

module Log = struct
  type msg = {
    level : int;
    file : string;
    line : int;
    area : string;
    message : string;
  }

  type level = [ `Critical | `Error | `Warning | `Notice | `Debug ]

  let int_of_level = function
    | `Critical -> log_crit
    | `Error -> log_err
    | `Warning -> log_warning
    | `Notice -> log_notice
    | `Debug -> log_debug

  let setloglevel lvl = setloglevel (int_of_level lvl)

  external setup_log_callback : unit -> unit = "ocaml_srt_setup_log_callback"
    [@@noalloc]

  external process_log : (msg -> unit) -> unit = "ocaml_srt_process_log"

  let log_fn = ref (fun _ -> ())
  let log_fn_m = Mutex.create ()

  let mutexify fn x =
    Mutex.lock log_fn_m;
    try
      fn x;
      Mutex.unlock log_fn_m
    with exn ->
      let bt = Printexc.get_raw_backtrace () in
      Mutex.unlock log_fn_m;
      Printexc.raise_with_backtrace exn bt

  let set_handler fn =
    setup_log_callback ();
    mutexify (fun () -> log_fn := fn) ()

  external clear_callback : unit -> unit = "ocaml_srt_clear_log_callback"
    [@@noalloc]

  let clear_handler () =
    clear_callback ();
    mutexify (fun () -> log_fn := fun _ -> ()) ()

  let () =
    ignore (Thread.create (fun () -> process_log (fun msg -> !log_fn msg)) ())
end

module Stats = struct
  type t = {
    msTimeStamp : int64;
    pktSentTotal : int64;
    pktRecvTotal : int64;
    pktSndLossTotal : int;
    pktRcvLossTotal : int;
    pktRetransTotal : int;
    pktSentACKTotal : int;
    pktRecvACKTotal : int;
    pktSentNAKTotal : int;
    pktRecvNAKTotal : int;
    usSndDurationTotal : int64;
    pktSndDropTotal : int;
    pktRcvDropTotal : int;
    pktRcvUndecryptTotal : int;
    byteSentTotal : UInt64.t;
    byteRecvTotal : UInt64.t;
    byteRetransTotal : UInt64.t;
    byteSndDropTotal : UInt64.t;
    byteRcvDropTotal : UInt64.t;
    byteRcvUndecryptTotal : UInt64.t;
    pktSent : int64;
    pktRecv : int64;
    pktSndLoss : int;
    pktRcvLoss : int;
    pktRetrans : int;
    pktRcvRetrans : int;
    pktSentACK : int;
    pktRecvACK : int;
    pktSentNAK : int;
    pktRecvNAK : int;
    mbpsSendRate : float;
    mbpsRecvRate : float;
    usSndDuration : int64;
    pktReorderDistance : int;
    pktRcvAvgBelatedTime : float;
    pktRcvBelated : int64;
    pktSndDrop : int;
    pktRcvDrop : int;
    pktRcvUndecrypt : int;
    byteSent : UInt64.t;
    byteRecv : UInt64.t;
    byteRetrans : UInt64.t;
    byteSndDrop : UInt64.t;
    byteRcvDrop : UInt64.t;
    byteRcvUndecrypt : UInt64.t;
    usPktSndPeriod : float;
    pktFlowWindow : int;
    pktCongestionWindow : int;
    pktFlightSize : int;
    msRTT : float;
    mbpsBandwidth : float;
    byteAvailSndBuf : int;
    byteAvailRcvBuf : int;
    mbpsMaxBW : float;
    byteMSS : int;
    pktSndBuf : int;
    byteSndBuf : int;
    msSndBuf : int;
    msSndTsbPdDelay : int;
    pktRcvBuf : int;
    byteRcvBuf : int;
    msRcvBuf : int;
    msRcvTsbPdDelay : int;
    pktSndFilterExtraTotal : int;
    pktRcvFilterExtraTotal : int;
    pktRcvFilterSupplyTotal : int;
    pktRcvFilterLossTotal : int;
    pktSndFilterExtra : int;
    pktRcvFilterExtra : int;
    pktRcvFilterSupply : int;
    pktRcvFilterLoss : int;
  }

  let from_struct stats =
    {
      msTimeStamp = !@(stats |-> CBytePerfMon.msTimeStamp);
      pktSentTotal = !@(stats |-> CBytePerfMon.pktSentTotal);
      pktRecvTotal = !@(stats |-> CBytePerfMon.pktRecvTotal);
      pktSndLossTotal = !@(stats |-> CBytePerfMon.pktSndLossTotal);
      pktRcvLossTotal = !@(stats |-> CBytePerfMon.pktRcvLossTotal);
      pktRetransTotal = !@(stats |-> CBytePerfMon.pktRetransTotal);
      pktSentACKTotal = !@(stats |-> CBytePerfMon.pktSentACKTotal);
      pktRecvACKTotal = !@(stats |-> CBytePerfMon.pktRecvACKTotal);
      pktSentNAKTotal = !@(stats |-> CBytePerfMon.pktSentNAKTotal);
      pktRecvNAKTotal = !@(stats |-> CBytePerfMon.pktRecvNAKTotal);
      usSndDurationTotal = !@(stats |-> CBytePerfMon.usSndDurationTotal);
      pktSndDropTotal = !@(stats |-> CBytePerfMon.pktSndDropTotal);
      pktRcvDropTotal = !@(stats |-> CBytePerfMon.pktRcvDropTotal);
      pktRcvUndecryptTotal = !@(stats |-> CBytePerfMon.pktRcvUndecryptTotal);
      byteSentTotal = !@(stats |-> CBytePerfMon.byteSentTotal);
      byteRecvTotal = !@(stats |-> CBytePerfMon.byteRecvTotal);
      byteRetransTotal = !@(stats |-> CBytePerfMon.byteRetransTotal);
      byteSndDropTotal = !@(stats |-> CBytePerfMon.byteSndDropTotal);
      byteRcvDropTotal = !@(stats |-> CBytePerfMon.byteRcvDropTotal);
      byteRcvUndecryptTotal = !@(stats |-> CBytePerfMon.byteRcvUndecryptTotal);
      pktSent = !@(stats |-> CBytePerfMon.pktSent);
      pktRecv = !@(stats |-> CBytePerfMon.pktRecv);
      pktSndLoss = !@(stats |-> CBytePerfMon.pktSndLoss);
      pktRcvLoss = !@(stats |-> CBytePerfMon.pktRcvLoss);
      pktRetrans = !@(stats |-> CBytePerfMon.pktRetrans);
      pktRcvRetrans = !@(stats |-> CBytePerfMon.pktRcvRetrans);
      pktSentACK = !@(stats |-> CBytePerfMon.pktSentACK);
      pktRecvACK = !@(stats |-> CBytePerfMon.pktRecvACK);
      pktSentNAK = !@(stats |-> CBytePerfMon.pktSentNAK);
      pktRecvNAK = !@(stats |-> CBytePerfMon.pktRecvNAK);
      mbpsSendRate = !@(stats |-> CBytePerfMon.mbpsSendRate);
      mbpsRecvRate = !@(stats |-> CBytePerfMon.mbpsRecvRate);
      usSndDuration = !@(stats |-> CBytePerfMon.usSndDuration);
      pktReorderDistance = !@(stats |-> CBytePerfMon.pktReorderDistance);
      pktRcvAvgBelatedTime = !@(stats |-> CBytePerfMon.pktRcvAvgBelatedTime);
      pktRcvBelated = !@(stats |-> CBytePerfMon.pktRcvBelated);
      pktSndDrop = !@(stats |-> CBytePerfMon.pktSndDrop);
      pktRcvDrop = !@(stats |-> CBytePerfMon.pktRcvDrop);
      pktRcvUndecrypt = !@(stats |-> CBytePerfMon.pktRcvUndecrypt);
      byteSent = !@(stats |-> CBytePerfMon.byteSent);
      byteRecv = !@(stats |-> CBytePerfMon.byteRecv);
      byteRetrans = !@(stats |-> CBytePerfMon.byteRetrans);
      byteSndDrop = !@(stats |-> CBytePerfMon.byteSndDrop);
      byteRcvDrop = !@(stats |-> CBytePerfMon.byteRcvDrop);
      byteRcvUndecrypt = !@(stats |-> CBytePerfMon.byteRcvUndecrypt);
      usPktSndPeriod = !@(stats |-> CBytePerfMon.usPktSndPeriod);
      pktFlowWindow = !@(stats |-> CBytePerfMon.pktFlowWindow);
      pktCongestionWindow = !@(stats |-> CBytePerfMon.pktCongestionWindow);
      pktFlightSize = !@(stats |-> CBytePerfMon.pktFlightSize);
      msRTT = !@(stats |-> CBytePerfMon.msRTT);
      mbpsBandwidth = !@(stats |-> CBytePerfMon.mbpsBandwidth);
      byteAvailSndBuf = !@(stats |-> CBytePerfMon.byteAvailSndBuf);
      byteAvailRcvBuf = !@(stats |-> CBytePerfMon.byteAvailRcvBuf);
      mbpsMaxBW = !@(stats |-> CBytePerfMon.mbpsMaxBW);
      byteMSS = !@(stats |-> CBytePerfMon.byteMSS);
      pktSndBuf = !@(stats |-> CBytePerfMon.pktSndBuf);
      byteSndBuf = !@(stats |-> CBytePerfMon.byteSndBuf);
      msSndBuf = !@(stats |-> CBytePerfMon.msSndBuf);
      msSndTsbPdDelay = !@(stats |-> CBytePerfMon.msSndTsbPdDelay);
      pktRcvBuf = !@(stats |-> CBytePerfMon.pktRcvBuf);
      byteRcvBuf = !@(stats |-> CBytePerfMon.byteRcvBuf);
      msRcvBuf = !@(stats |-> CBytePerfMon.msRcvBuf);
      msRcvTsbPdDelay = !@(stats |-> CBytePerfMon.msRcvTsbPdDelay);
      pktSndFilterExtraTotal = !@(stats |-> CBytePerfMon.pktSndFilterExtraTotal);
      pktRcvFilterExtraTotal = !@(stats |-> CBytePerfMon.pktRcvFilterExtraTotal);
      pktRcvFilterSupplyTotal =
        !@(stats |-> CBytePerfMon.pktRcvFilterSupplyTotal);
      pktRcvFilterLossTotal = !@(stats |-> CBytePerfMon.pktRcvFilterLossTotal);
      pktSndFilterExtra = !@(stats |-> CBytePerfMon.pktSndFilterExtra);
      pktRcvFilterExtra = !@(stats |-> CBytePerfMon.pktRcvFilterExtra);
      pktRcvFilterSupply = !@(stats |-> CBytePerfMon.pktRcvFilterSupply);
      pktRcvFilterLoss = !@(stats |-> CBytePerfMon.pktRcvFilterLoss);
    }

  let bstats ?(clear = false) socket =
    let clear = if clear then 0 else 1 in
    let stats = allocate_n CBytePerfMon.t ~count:1 in
    ignore (check_err (bstats socket stats clear));
    from_struct stats

  let bistats ?(clear = false) ?(instantaneous = false) socket =
    let clear = if clear then 0 else 1 in
    let instantaneous = if instantaneous then 0 else 1 in
    let stats = allocate_n CBytePerfMon.t ~count:1 in
    ignore (check_err (bistats socket stats clear instantaneous));
    from_struct stats
end
