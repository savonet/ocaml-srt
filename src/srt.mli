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

type socket_opt = [
  | `Conntimeo
  | `Event
  | `Fc
  | `Inputbw
  | `Iptos
  | `Ipttl
  | `Isn
  | `Kmpreannounce
  | `Kmrefreshrate
  | `Kmstate
  | `Latency
  | `Linger
  | `Lossmaxttl
  | `Maxbw
  | `Messageapi
  | `Minversion
  | `Mss
  | `Nakreport
  | `Oheadbw
  | `Passphrase
  | `Payloadsize
  | `Pbkeylen
  | `Peerlatency
  | `Peerversion
  | `Rcvbuf
  | `Rcvdata
  | `Rcvkmstate
  | `Rcvlatency
  | `Rcvsyn
  | `Rcvtimeo
  | `Rendezvous
  | `Reuseaddr
  | `Sender
  | `Smoother
  | `Sndbuf
  | `Snddata
  | `Snddropdelay
  | `Sndkmstate
  | `Sndsyn
  | `Sndtimeo
  | `State
  | `Streamid
  | `Strictenc
  | `Tlpktdrop
  | `Transtype
  | `Tsbpddelay
  | `Tsbpdmode
  | `Udp
  | `Udp
  | `Version
]

type transtype = [
  | `Live
  | `File
  | `Invalid
]

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

val socket : int -> int -> int -> socket

val create_socket : unit -> socket

val getsockstate : socket -> socket_status

val bind : socket -> Unix.sockaddr -> unit

val setloglevel : int -> unit
