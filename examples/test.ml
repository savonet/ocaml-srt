open Srt

let () =
  startup ();
  Log.set_handler (fun { Log.message; _ } -> print_endline message);
  Log.setloglevel `Warning;
  let s = create_socket () in
  Printf.printf "Setting transtype to file..\n%!";
  setsockflag s transtype `File;
  setsockflag s rcvsyn true;
  Printf.printf "Rcvsyn: %b\n%!" (getsockflag s rcvsyn);
  setsockflag s rcvsyn false;
  Printf.printf "Rcvsyn: %b\n%!" (getsockflag s rcvsyn);
  setsockflag s payloadsize 1234;
  close s;
  cleanup ()
