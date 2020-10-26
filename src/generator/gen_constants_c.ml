let c_headers =
  {|
#include <srt/srt.h>

#ifndef SRT_ENABLE_LOSTBYTESCOUNT
#define SRT_ENABLE_LOSTBYTESCOUNT 0
#endif
|}

let () =
  let fname = Sys.argv.(1) in
  let oc = open_out_bin fname in
  let format = Format.formatter_of_out_channel oc in
  Format.fprintf format "%s@\n" c_headers;
  Cstubs.Types.write_c format (module Srt_constants.Def);
  Format.pp_print_flush format ();
  close_out oc
