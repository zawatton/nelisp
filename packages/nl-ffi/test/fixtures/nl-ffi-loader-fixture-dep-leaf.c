/* nl-ffi-loader-fixture-dep-leaf.c -- a self-contained (-nostdlib) leaf
   library with no DT_NEEDED of its own, used as increment 2's real,
   loadable DT_NEEDED dependency.  Built with an explicit -soname so the
   referencing fixture's own DT_NEEDED records this exact bare name (not
   a path), exercising real search-path resolution rather than a literal
   path.  See nl-ffi-loader-fixture-dep-leaf2.c (same exported name,
   different return value) and nl-ffi-loader-fixture-dep-root.c (the
   dependent, linked against BOTH, in this order) for the search-order
   case this pair exists to prove. */

int nl_ffi_loader_fixture_dep_leaf_value(int x) { return x + 1000; }
