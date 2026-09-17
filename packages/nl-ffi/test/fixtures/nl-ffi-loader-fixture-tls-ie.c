/* nl-ffi-loader-fixture-tls-ie.c -- FFI step 3 increment 3 (TLS).  A
   `__thread' variable compiled with `-ftls-model=initial-exec' (see the
   Makefile's `ffi-loader' target), so it lowers to a single
   `R_X86_64_TPOFF64' relocation against a GOT-style slot rather than the
   General-Dynamic `__tls_get_addr' call sequence GCC's DEFAULT TLS model
   emits for ordinary `-fPIC -shared' code without this flag (confirmed
   with `readelf -r'/`-x' and `objdump -d' before writing this file -- see
   the report and nl-ffi-loader.el's Commentary, "TLS") -- the one TLS
   relocation type the loader implements. */

static __thread int nl_ffi_loader_fixture_tls_ie_var = 7;

int nl_ffi_loader_fixture_tls_ie_get(void) {
  return nl_ffi_loader_fixture_tls_ie_var;
}

void nl_ffi_loader_fixture_tls_ie_set(int v) {
  nl_ffi_loader_fixture_tls_ie_var = v;
}
