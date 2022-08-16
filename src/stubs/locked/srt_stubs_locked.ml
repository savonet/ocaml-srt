open Ctypes

module Def (F : Cstubs.FOREIGN) = struct
  open F

  let memcpy =
    foreign "memcpy" (ocaml_bytes @-> ptr char @-> int @-> returning void)

  let memcpy_str =
    foreign "memcpy" (ocaml_string @-> ptr char @-> int @-> returning void)
end
