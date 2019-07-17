type socket = Stubs.socket

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

module Srt = Stubs.Def(Srt_stubs)
include Srt
