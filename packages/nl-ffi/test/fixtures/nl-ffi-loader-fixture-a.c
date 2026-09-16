/* nl-ffi-loader-fixture-a.c -- see nl-ffi-loader-fixture-b.c and the
   Makefile's `ffi-loader' target for the whole picture.

   Compiled WITHOUT -fno-plt: `nl_ffi_loader_fixture_call_double''s call
   to the exported `nl_ffi_loader_fixture_double' goes through an
   ordinary lazy PLT stub, so the object needs an R_X86_64_JUMP_SLOT
   relocation for it (`.rela.plt').  `nl_ffi_loader_fixture_local_ptr''s
   initializer takes the address of a `static' (internal-linkage, never
   in `.dynsym') function, which the linker can resolve to a fixed
   base+offset at link time with no symbol lookup at all -- an
   R_X86_64_RELATIVE relocation (`.rela.dyn').  `nl_ffi_loader_fixture_
   triple' is defined here but only ever CALLED from
   nl-ffi-loader-fixture-b.c (compiled with -fno-plt there), so its own
   relocation is an R_X86_64_GLOB_DAT, not a JUMP_SLOT -- see that file. */

int nl_ffi_loader_fixture_double(int x) { return x * 2; }

int nl_ffi_loader_fixture_call_double(int x) {
  return nl_ffi_loader_fixture_double(x) + 1;
}

int nl_ffi_loader_fixture_triple(int x) { return x * 3; }

static int nl_ffi_loader_fixture_local(int x) { return x + 3; }

int (*nl_ffi_loader_fixture_local_ptr)(int) = nl_ffi_loader_fixture_local;
