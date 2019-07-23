open Srt

let () =
  startup ();
  setloglevel 1;
  let s = socket Unix.PF_INET Unix.SOCK_DGRAM 0 in 
  Printf.printf "Messageapi: %b\n%!" (getsockflag s messageapi);
  Printf.printf "Setting transtype to file..\n%!";
  setsockflag s transtype `File;
  Printf.printf "Messageapi: %b\n%!" (getsockflag s messageapi);
  setsockflag s rcvsyn true;
  Printf.printf "Rcvsyn: %b\n%!" (getsockflag s rcvsyn);
  setsockflag s rcvsyn false;
  Printf.printf "Rcvsyn: %b\n%!" (getsockflag s rcvsyn);
  close s;
  cleanup()
