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

int nl_ffi_loader_fixture_tls_ie_exported_anchor = 17;
static int nl_ffi_loader_fixture_tls_ie_local_anchor = 29;

__thread int *nl_ffi_loader_fixture_tls_ie_exported_pointer =
    &nl_ffi_loader_fixture_tls_ie_exported_anchor;
static __thread int *nl_ffi_loader_fixture_tls_ie_local_pointer =
    &nl_ffi_loader_fixture_tls_ie_local_anchor;

int nl_ffi_loader_fixture_tls_ie_pointer_mask = 0;

int nl_ffi_loader_fixture_tls_ie_exported_pointer_matches(void) {
  return nl_ffi_loader_fixture_tls_ie_exported_pointer ==
         &nl_ffi_loader_fixture_tls_ie_exported_anchor;
}

int nl_ffi_loader_fixture_tls_ie_local_pointer_matches(void) {
  return nl_ffi_loader_fixture_tls_ie_local_pointer ==
         &nl_ffi_loader_fixture_tls_ie_local_anchor;
}

__attribute__((constructor)) static void nl_ffi_loader_fixture_tls_ie_check_pointers(void) {
  if (nl_ffi_loader_fixture_tls_ie_exported_pointer ==
      &nl_ffi_loader_fixture_tls_ie_exported_anchor)
    nl_ffi_loader_fixture_tls_ie_pointer_mask |= 1;
  if (nl_ffi_loader_fixture_tls_ie_local_pointer ==
      &nl_ffi_loader_fixture_tls_ie_local_anchor)
    nl_ffi_loader_fixture_tls_ie_pointer_mask |= 2;
}

/* Check the bounded x86-64 TCB directly, without dereferencing any pointer
   obtained from its fields.  The caller first proves the loader-owned mapping
   covers the complete positive range used below. */
int nl_ffi_loader_fixture_tls_ie_check_tcb(unsigned long long expected_tp) {
  unsigned long long tcb;
  unsigned long long self;
  unsigned char guard;
  __asm__ volatile("movq %%fs:0, %0" : "=r"(tcb));
  __asm__ volatile("movq %%fs:16, %0" : "=r"(self));
  __asm__ volatile("movb %%fs:40, %0" : "=q"(guard));
  return tcb == expected_tp && self == expected_tp && guard == 0 ? 42 : 0;
}
