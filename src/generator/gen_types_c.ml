let c_headers =
  {|
#include <srt/srt.h>

#if (SRT_VERSION_MAJOR <= 1) && (SRT_VERSION_MINOR <= 4) && (SRT_VERSION_PATCH <= 1)
#define SRT_EPOLL_EVENT_STR SRT_EPOLL_EVENT_
#endif
|}

let () =
  let fname = Sys.argv.(1) in
  let oc = open_out_bin fname in
  let format = Format.formatter_of_out_channel oc in
  Format.fprintf format "%s@\n" c_headers;
  Cstubs.Types.write_c format (module Srt_types.Def);
  Format.pp_print_flush format ();
  close_out oc
