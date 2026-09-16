/* nl-ffi-loader-fixture-dep-leaf2.c -- a second self-contained leaf
   library exporting the SAME symbol name as nl-ffi-loader-fixture-dep-
   leaf.c, with a different, distinguishable return value.  Exists only
   so nl-ffi-loader-fixture-dep-root.c can be linked against BOTH (in a
   fixed order) and the smoke test can prove which one this loader's
   documented cross-object search order actually picks. */

int nl_ffi_loader_fixture_dep_leaf_value(int x) { return x + 2000; }
