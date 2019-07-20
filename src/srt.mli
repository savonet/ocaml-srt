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
val transtype : transtype socket_opt

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

val socket : Unix.socket_domain -> Unix.socket_type -> int -> socket

val getsockstate : socket -> socket_status

val bind : socket -> Unix.sockaddr -> unit

val listen : socket -> int -> unit

val accept : socket -> Unix.sockaddr -> socket

val connect : socket -> Unix.sockaddr -> unit

val rendez_vous : socket -> Unix.sockaddr -> Unix.sockaddr -> unit

val setloglevel : int -> unit

val send : socket -> string -> int

val recv : socket -> bytes -> int -> int

val sendmsg : socket -> string -> bool -> Unsigned.UInt64.t -> int

val recvmsg : socket -> bytes -> int -> int

val getsockflag : socket -> 'a socket_opt -> 'a

val setsockflag : socket -> 'a socket_opt -> 'a -> unit