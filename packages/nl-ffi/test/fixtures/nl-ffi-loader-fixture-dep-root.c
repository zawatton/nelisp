/* nl-ffi-loader-fixture-dep-root.c -- depends on (real DT_NEEDED, via
   -soname/-rpath, not a literal path) BOTH nl-ffi-loader-fixture-dep-
   leaf.so and nl-ffi-loader-fixture-dep-leaf2.so, in that link order
   (see the Makefile's ffi-loader target).  nl_ffi_loader_fixture_dep_
   leaf_value stays UNDEFINED in this object itself -- ordinary dynamic
   linking never bakes in which .so answers it at static-link time, so
   resolving it is entirely this loader's job at open time.  Calling
   nl_ffi_loader_fixture_dep_root_call and getting back x+1000+1 (not
   x+2000+1) proves both that a real dependency was resolved and called,
   and that the leaf linked FIRST is the one this loader's search order
   picks. */

extern int nl_ffi_loader_fixture_dep_leaf_value(int x);

int nl_ffi_loader_fixture_dep_root_call(int x) {
  return nl_ffi_loader_fixture_dep_leaf_value(x) + 1;
}
