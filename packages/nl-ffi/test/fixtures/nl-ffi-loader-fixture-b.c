/* nl-ffi-loader-fixture-b.c -- compiled WITH -fno-plt (see the Makefile's
   `ffi-loader' target): this call to `nl_ffi_loader_fixture_triple'
   (defined in nl-ffi-loader-fixture-a.c, compiled WITHOUT -fno-plt)
   goes through a bare GOT load instead of a PLT stub, so the object
   needs an R_X86_64_GLOB_DAT relocation for it (`.rela.dyn') rather
   than the R_X86_64_JUMP_SLOT `nl_ffi_loader_fixture_double' gets in
   the other file. */

extern int nl_ffi_loader_fixture_triple(int x);

int nl_ffi_loader_fixture_call_triple(int x) {
  return nl_ffi_loader_fixture_triple(x) + 2;
}
