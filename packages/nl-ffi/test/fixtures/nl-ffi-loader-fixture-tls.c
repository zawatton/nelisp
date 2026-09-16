/* nl-ffi-loader-fixture-tls.c -- a `__thread' variable forces a real
   PT_TLS program header.  Used only to prove `nl-ffi-loader-open'
   refuses an object that needs a TLS block
   (`nl-ffi-loader-unsupported' reason `:tls-segment') -- the function
   itself is never called. */

static __thread int nl_ffi_loader_fixture_tls_var = 7;

int nl_ffi_loader_fixture_tls_get(void) { return nl_ffi_loader_fixture_tls_var; }
