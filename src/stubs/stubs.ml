open Ctypes

type socket = int

let socket : socket typ = int

module T = Types.Def(Srt_types)

module Def (F : Cstubs.FOREIGN) = struct
  open F

  let getsockstate = foreign "srt_getsockstate" (socket @-> (returning T.socket_status))

  let setloglevel = foreign "srt_setloglevel" (int @-> (returning void))
end
