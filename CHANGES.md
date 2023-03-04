0.3.0 (2022-03-04)
=====
* Added `rcvdata` and `rcvlatency`
* Added read/write constraints to socket options.
* Remove `max_*` arguments in polling functions.

0.2.2 (2022-09-21)
=====
* Reimplement log handler to be non-blocking and
  outside of the OCaml heap.
* Added `listen_callback` API
* Added `pbkeylen`, `passphrase` and `streamid` socket options.

0.2.1 (2022-01-02)
=====
* Added support for conn/rcn/sndtimeo.

0.2.0 (2021-06-18)
=====
* Added support for uwait polling.
* Added support for stats.
* Remove deprecated `srt_socket` 

0.1.1 (2020-06-13)
=====
* Switch to `posix-socket`
* Added support for `SRTO_ENFORCEDENCRYPTION`

0.1.0 (2019-09-17)
=====
* Initial release
