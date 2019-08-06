type socket

type socket_status = [
  | `Init
  | `Opened
  | `Listening
  | `Connecting
  | `Connected
  | `Broken
  | `Closing
  | `Closed
  | `Nonexist
]

type transtype = [
  | `Live
  | `File
  | `Invalid
]

type 'a socket_opt

val messageapi : bool socket_opt
val payloadsize : int socket_opt
val transtype : transtype socket_opt
val rcvsyn : bool socket_opt
val sndsyn : bool socket_opt
val reuseaddr : bool socket_opt
val rcvbuf : int socket_opt
val sndbuf : int socket_opt
val udp_rcvbuf : int socket_opt
val udp_sndbuf : int socket_opt

type errno = [
  | `Easyncfail
  | `Easyncrcv
  | `Easyncsnd
  | `Eboundsock
  | `Econgest
  | `Econnfail
  | `Econnlost
  | `Econnrej
  | `Econnsetup
  | `Econnsock
  | `Eduplisten
  | `Efile
  | `Einvalbufferapi
  | `Einvalmsgapi
  | `Einvop
  | `Einvparam
  | `Einvpollid
  | `Einvrdoff
  | `Einvsock
  | `Einvwroff
  | `Elargemsg
  | `Enobuf
  | `Enoconn
  | `Enolisten
  | `Enoserver
  | `Epeererr
  | `Erdperm
  | `Erdvnoserv
  | `Erdvunbound
  | `Eresource
  | `Esecfail
  | `Esockfail
  | `Ethread
  | `Etimeout
  | `Eunboundsock
  | `Eunknown
  | `Ewrperm
  | `Success
]

exception Error of errno*string

val startup : unit -> unit

val cleanup : unit -> unit

val create_socket : unit -> socket

val socket : Unix.socket_domain -> Unix.socket_type -> int -> socket

val getsockstate : socket -> socket_status

val bind : socket -> Unix.sockaddr -> unit

val listen : socket -> int -> unit

val accept : socket -> socket*Unix.sockaddr

val connect : socket -> Unix.sockaddr -> unit

val rendez_vous : socket -> Unix.sockaddr -> Unix.sockaddr -> unit

val send : socket -> bytes -> int

val recv : socket -> bytes -> int -> int

val sendmsg : socket -> bytes -> int -> bool -> int

val recvmsg : socket -> bytes -> int -> int

val getsockflag : socket -> 'a socket_opt -> 'a

val setsockflag : socket -> 'a socket_opt -> 'a -> unit

val close : socket -> unit

module Log : sig
  type msg = {
    level:   int;
    file:    string;
    line:    int;
    area:    string;
    message: string
  }

  type level = [
    | `Critical
    | `Error
    | `Warning
    | `Notice
    | `Debug
  ]

  val setloglevel : level -> unit

  val set_handler : (msg -> unit) -> unit

  val clear_handler : unit -> unit
end

module Poll : sig
  type t

  type flag = [
    | `Read
    | `Write
    | `Error
  ]

  val create : unit -> t

  val add_usock : t -> socket -> flag -> unit

  val remove_usock : t -> socket -> unit

  val update_usock : t -> socket -> flag -> unit

  val wait : t -> max_read:int -> max_write:int -> timeout:int -> (socket list * socket list)

  val release : t -> unit  
end
