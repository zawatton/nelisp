/* nl-ffi-loader-fixture-ctor-root.c -- depends (real DT_NEEDED, via
   -soname/-rpath) on nl-ffi-loader-fixture-ctor-dep.so.  This
   constructor reads the dependency's exported global THROUGH a real
   relocation (it is UNDEFINED in this object) and records what it saw:
   2 when the dependency's own constructor had already run (the correct,
   required order), 99 when it had not (order 0) or something else is
   wrong.  The smoke test reads nl_ffi_loader_fixture_ctor_order back
   after nl-ffi-loader-open returns and requires exactly 2. */

extern int nl_ffi_loader_fixture_ctor_order;

__attribute__((constructor)) static void nl_ffi_loader_fixture_ctor_root_init(void) {
  nl_ffi_loader_fixture_ctor_order =
    (nl_ffi_loader_fixture_ctor_order == 1) ? 2 : 99;
}
