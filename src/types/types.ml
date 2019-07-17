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

module Def (S : Cstubs.Types.TYPE) = struct
  open S

  let socket_status : socket_status typ = 
    enum "SRT_SOCKSTATUS" [
      `Init,       constant "SRTS_INIT"       int64_t;
      `Opened,     constant "SRTS_OPENED"     int64_t;
      `Listening,  constant "SRTS_LISTENING"  int64_t;
      `Connecting, constant "SRTS_CONNECTING" int64_t;
      `Connected,  constant "SRTS_CONNECTED"  int64_t;
      `Broken,     constant "SRTS_BROKEN"     int64_t;
      `Closing,    constant "SRTS_CLOSING"    int64_t;
      `Closed,     constant "SRTS_CLOSED"     int64_t;
      `Nonexist,   constant "SRTS_NONEXIST"   int64_t
    ]
end
