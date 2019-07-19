open Ctypes

type socket = int

module Def (F : Cstubs.FOREIGN) = struct
  include Srt_types.Def(Srt_generated_types)

  open F

  let startup = foreign "srt_startup" (void @-> (returning void))

  let cleanup = foreign "srt_cleanup" (void @-> (returning void))

  let getlasterror = foreign "srt_getlasterror" (ptr int @-> (returning errno))

  let getlasterror_str = foreign "srt_getlasterror_str" (void @-> (returning string))

  let clearlasterror = foreign "srt_clearlasterror" (void @-> (returning void))

  let socket = foreign "srt_socket" (int @-> int @-> int @-> (returning int))

  let create_socket = foreign "srt_create_socket" (void @-> (returning int))

  let bind = foreign "srt_bind" (int @-> ptr Unix_sys_socket.Sockaddr.t @-> int @-> (returning int)) 

  let getsockstate = foreign "srt_getsockstate" (int @-> (returning socket_status))

  let setloglevel = foreign "srt_setloglevel" (int @-> (returning void))
end
