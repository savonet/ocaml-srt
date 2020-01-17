(lang dune 2.0)
(version 0.1.2)
(name srt)
(source (github savonet/ocaml-srt))
(license MIT)
(authors "Romain Beauxis")
(maintainers "romain.beauxis@gmail.com")

(generate_opam_files true)

(package
 (name srt)
 (synopsis "Binding for the Secure, Reliable, Transport protocol library")
 (description "
Secure Reliable Transport (SRT) is an open source transport technology
that optimizes streaming performance across unpredictable networks, such
as the Internet.
This package provides OCaml bindings to the C implementation library.
")
 (depends
  (dune (> 2.0))
  ctypes
  posix-socket)
)
