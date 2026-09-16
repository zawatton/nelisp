/* nl-ffi-loader-fixture-needs-dep.c -- linked WITH libm (no -nostdlib),
   so the resulting object carries a real DT_NEEDED for libm.so.6.  Used
   only to prove `nl-ffi-loader-open' refuses an object with a dependency
   (`nl-ffi-loader-unsupported' reason `:needs-dependency') -- the
   function itself is never called. */

extern double sqrt(double x);

double nl_ffi_loader_fixture_needs_dep(double x) { return sqrt(x); }
