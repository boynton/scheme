LeeScheme

A little scheme compiler/runtime I wrote for fun a long time ago, and periodically run to see if it still works.

The default Makefile is for Mac OS X (tested on snow leopard). Some other historical makefiles are in a subdirectory.

The "contrib/bench.scm" file is interesting, as it shows some historical timings over a fairly long span of time (a Mac plus ran this 1000x slower than my iMac i7).

Sorry, no docs, this was just a personal plaything. It does support hygienic macros, full continuations, and most of R4RS (but not bignums).

