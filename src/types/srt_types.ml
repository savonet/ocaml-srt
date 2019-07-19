module Def (S : Cstubs.Types.TYPE) = struct
  open S

  type socket = int

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

  let socket_status : socket_status typ = 
    enum "SRT_SOCKSTATUS" [
      `Init, constant "SRTS_INIT" int64_t;
      `Opened, constant "SRTS_OPENED" int64_t;
      `Listening, constant "SRTS_LISTENING" int64_t;
      `Connecting, constant "SRTS_CONNECTING" int64_t;
      `Connected, constant "SRTS_CONNECTED" int64_t;
      `Broken, constant "SRTS_BROKEN" int64_t;
      `Closing, constant "SRTS_CLOSING" int64_t;
      `Closed, constant "SRTS_CLOSED" int64_t;
      `Nonexist, constant "SRTS_NONEXIST" int64_t
    ]

  let socket_opt : socket_opt typ =
    enum "SRT_SOCKOPT" [
     `Mss, constant "SRTO_MSS" int64_t;
     `Sndsyn, constant "SRTO_SNDSYN" int64_t;
     `Rcvsyn, constant "SRTO_RCVSYN" int64_t;
     `Isn, constant "SRTO_ISN" int64_t;
     `Fc, constant "SRTO_FC" int64_t;
     `Sndbuf, constant "SRTO_SNDBUF" int64_t;
     `Rcvbuf, constant "SRTO_RCVBUF" int64_t;
     `Linger, constant "SRTO_LINGER" int64_t;
     `Udp, constant "SRTO_UDP_SNDBUF" int64_t;
     `Udp, constant "SRTO_UDP_RCVBUF" int64_t;
     `Rendezvous, constant "SRTO_RENDEZVOUS" int64_t;
     `Sndtimeo, constant "SRTO_SNDTIMEO" int64_t;
     `Rcvtimeo, constant "SRTO_RCVTIMEO" int64_t;
     `Reuseaddr, constant "SRTO_REUSEADDR" int64_t;
     `Maxbw, constant "SRTO_MAXBW" int64_t;
     `State, constant "SRTO_STATE" int64_t;
     `Event, constant "SRTO_EVENT" int64_t;
     `Snddata, constant "SRTO_SNDDATA" int64_t;
     `Rcvdata, constant "SRTO_RCVDATA" int64_t;
     `Sender, constant "SRTO_SENDER" int64_t;
     `Tsbpdmode, constant "SRTO_TSBPDMODE" int64_t;
     `Latency, constant "SRTO_LATENCY" int64_t;
     `Tsbpddelay, constant "SRTO_TSBPDDELAY" int64_t;
     `Inputbw, constant "SRTO_INPUTBW" int64_t;
     `Oheadbw, constant "SRTO_OHEADBW" int64_t;
     `Passphrase, constant "SRTO_PASSPHRASE" int64_t;
     `Pbkeylen, constant "SRTO_PBKEYLEN" int64_t;
     `Kmstate, constant "SRTO_KMSTATE" int64_t;
     `Ipttl, constant "SRTO_IPTTL" int64_t;
     `Iptos, constant "SRTO_IPTOS" int64_t;
     `Tlpktdrop, constant "SRTO_TLPKTDROP" int64_t;
     `Snddropdelay, constant "SRTO_SNDDROPDELAY" int64_t;
     `Nakreport, constant "SRTO_NAKREPORT" int64_t;
     `Version, constant "SRTO_VERSION" int64_t;
     `Peerversion, constant "SRTO_PEERVERSION" int64_t;
     `Conntimeo, constant "SRTO_CONNTIMEO" int64_t;
     `Sndkmstate, constant "SRTO_SNDKMSTATE" int64_t;
     `Rcvkmstate, constant "SRTO_RCVKMSTATE" int64_t;
     `Lossmaxttl, constant "SRTO_LOSSMAXTTL" int64_t;
     `Rcvlatency, constant "SRTO_RCVLATENCY" int64_t;
     `Peerlatency, constant "SRTO_PEERLATENCY" int64_t;
     `Minversion, constant "SRTO_MINVERSION" int64_t;
     `Streamid, constant "SRTO_STREAMID" int64_t;
     `Smoother, constant "SRTO_SMOOTHER" int64_t;
     `Messageapi, constant "SRTO_MESSAGEAPI" int64_t;
     `Payloadsize, constant "SRTO_PAYLOADSIZE" int64_t;
     `Transtype, constant "SRTO_TRANSTYPE" int64_t;
     `Kmrefreshrate, constant "SRTO_KMREFRESHRATE" int64_t;
     `Kmpreannounce, constant "SRTO_KMPREANNOUNCE" int64_t;
     `Strictenc, constant "SRTO_STRICTENC" int64_t
  ]

  let transtype : transtype typ =
    enum "SRT_TRANSTYPE" [
      `Live, constant "SRTT_LIVE" int64_t;
      `File, constant "SRTT_FILE" int64_t;
      `Invalid, constant "SRTT_INVALID" int64_t
  ]

  let live_def_plsize = constant "SRT_LIVE_DEF_PLSIZE" int
  let live_max_plzise = constant "SRT_LIVE_MAX_PLSIZE" int
  let live_def_latency_ms = constant "SRT_LIVE_DEF_LATENCY_MS" int

  let errno : errno typ =
    enum "SRT_ERRNO" [
     `Eunknown, constant "SRT_EUNKNOWN" int64_t;
     `Success, constant "SRT_SUCCESS" int64_t;
     `Econnsetup, constant "SRT_ECONNSETUP" int64_t;
     `Enoserver, constant "SRT_ENOSERVER" int64_t;
     `Econnrej, constant "SRT_ECONNREJ" int64_t;
     `Esockfail, constant "SRT_ESOCKFAIL" int64_t;
     `Esecfail, constant "SRT_ESECFAIL" int64_t;
     `Econnfail, constant "SRT_ECONNFAIL" int64_t;
     `Econnlost, constant "SRT_ECONNLOST" int64_t;
     `Enoconn, constant "SRT_ENOCONN" int64_t;
     `Eresource, constant "SRT_ERESOURCE" int64_t;
     `Ethread, constant "SRT_ETHREAD" int64_t;
     `Enobuf, constant "SRT_ENOBUF" int64_t;
     `Efile, constant "SRT_EFILE" int64_t;
     `Einvrdoff, constant "SRT_EINVRDOFF" int64_t;
     `Erdperm, constant "SRT_ERDPERM" int64_t;
     `Einvwroff, constant "SRT_EINVWROFF" int64_t;
     `Ewrperm, constant "SRT_EWRPERM" int64_t;
     `Einvop, constant "SRT_EINVOP" int64_t;
     `Eboundsock, constant "SRT_EBOUNDSOCK" int64_t;
     `Econnsock, constant "SRT_ECONNSOCK" int64_t;
     `Einvparam, constant "SRT_EINVPARAM" int64_t;
     `Einvsock, constant "SRT_EINVSOCK" int64_t;
     `Eunboundsock, constant "SRT_EUNBOUNDSOCK" int64_t;
     `Enolisten, constant "SRT_ENOLISTEN" int64_t;
     `Erdvnoserv, constant "SRT_ERDVNOSERV" int64_t;
     `Erdvunbound, constant "SRT_ERDVUNBOUND" int64_t;
     `Einvalmsgapi, constant "SRT_EINVALMSGAPI" int64_t;
     `Einvalbufferapi, constant "SRT_EINVALBUFFERAPI" int64_t;
     `Eduplisten, constant "SRT_EDUPLISTEN" int64_t;
     `Elargemsg, constant "SRT_ELARGEMSG" int64_t;
     `Einvpollid, constant "SRT_EINVPOLLID" int64_t;
     `Easyncfail, constant "SRT_EASYNCFAIL" int64_t;
     `Easyncsnd, constant "SRT_EASYNCSND" int64_t;
     `Easyncrcv, constant "SRT_EASYNCRCV" int64_t;
     `Etimeout, constant "SRT_ETIMEOUT" int64_t;
     `Econgest, constant "SRT_ECONGEST" int64_t;
     `Epeererr, constant "SRT_EPEERERR" int64_t
  ]
end
