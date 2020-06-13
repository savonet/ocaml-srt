let c_headers = "#include <srt/srt.h>"

let () =
  let fname = Sys.argv.(1) in
  let oc = open_out_bin fname in
  let format = Format.formatter_of_out_channel oc in
  Format.fprintf format "%s@\n" c_headers;
  Cstubs.Types.write_c format (module Srt_types.Def);
  Format.pp_print_flush format ();
  close_out oc
