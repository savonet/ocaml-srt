#!/bin/sh -e

SYSTEM=$1
shift

ML=$1
shift

OUTPUT=$1
shift

TOOLCHAIN=""

if test "${SYSTEM}" = "mingw" -o "${SYSTEM}" = "mingw64"; then
  TOOLCHAIN="-toolchain windows"
fi

ocamlfind ${TOOLCHAIN} ocamlopt \
  -linkpkg \
  -thread \
  -package ctypes.stubs \
  -package ctypes.foreign \
  -package posix-socket \
  -I ../stubs \
  -I ../stubs/.srt_stubs.objs/native/ \
  -I ../stubs/.srt_stubs.objs/byte/ \
  $@ ${ML} -o ${OUTPUT}
