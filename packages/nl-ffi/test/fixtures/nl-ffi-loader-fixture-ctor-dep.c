/* nl-ffi-loader-fixture-ctor-dep.c -- a self-contained (-nostdlib)
   dependency with its own DT_INIT_ARRAY constructor, used together with
   nl-ffi-loader-fixture-ctor-root.c (which depends on it) to prove two
   things at once: an initializer's side effect is observable from Lisp
   after nl-ffi-loader-open returns, and a dependency's initializer runs
   before its dependent's.  The exported global starts at 0; this
   constructor sets it to 1 (and only 1, so re-running it -- it never
   is, but the guard documents the intent -- would not hide a real
   ordering bug). */

int nl_ffi_loader_fixture_ctor_order = 0;

__attribute__((constructor)) static void nl_ffi_loader_fixture_ctor_dep_init(void) {
  if (nl_ffi_loader_fixture_ctor_order == 0)
    nl_ffi_loader_fixture_ctor_order = 1;
}
