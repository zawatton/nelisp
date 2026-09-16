/* nl-ffi-loader-fixture-init.c -- an ordinary `cc -shared -fPIC' build
   (no -nostdlib) links crtbeginS.o/crtendS.o, which populate
   DT_INIT/DT_INIT_ARRAY/DT_FINI/DT_FINI_ARRAY even though this file
   defines no constructor of its own.  Used only to prove
   `nl-ffi-loader-open' refuses an object with initializers
   (`nl-ffi-loader-unsupported' reason `:has-initializers') -- the
   function itself is never called. */

int nl_ffi_loader_fixture_init_probe(int x) { return x + 1; }
