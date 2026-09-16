/* nl-ffi-loader-fixture-ifunc.c -- a self-contained (-nostdlib) IFUNC
   (R_X86_64_IRELATIVE) fixture.  The resolver runs real CPUID and picks
   the SSE2 implementation -- always the outcome on any x86-64 host, but
   only known at resolve (load) time, exactly the pattern a real glibc
   ifunc resolver uses for choosing between optimized implementations.
   nl_ffi_loader_fixture_call_ifunc is an ORDINARY function that calls
   the ifunc'd one through a normal relocated call site (the realistic
   case: an ffi:defun-generated call, or any other intra-object call,
   reaches an ifunc'd function this same way).  A loader that mishandled
   IRELATIVE by treating the addend as a plain target address (skipping
   the resolver call entirely) would end up calling THIS resolver as if
   it were the two-argument int function, and its bogus (huge pointer-
   valued) return would fail the smoke test's arithmetic check
   immediately -- see nl-ffi-loader-standalone-smoke.el. */

static int nl_ffi_loader_fixture_ifunc_generic(int x) { return x + 1; }
static int nl_ffi_loader_fixture_ifunc_sse2(int x) { return x + 2; }

static void *nl_ffi_loader_fixture_ifunc_resolver(void) {
  unsigned int eax, ebx, ecx, edx;
  __asm__("cpuid"
          : "=a"(eax), "=b"(ebx), "=c"(ecx), "=d"(edx)
          : "a"(1)
          :);
  /* CPUID.01H:EDX.SSE2[bit 26] -- architecturally always set on x86-64. */
  if (edx & (1u << 26))
    return (void *)nl_ffi_loader_fixture_ifunc_sse2;
  return (void *)nl_ffi_loader_fixture_ifunc_generic;
}

/* Hidden visibility is required to get a real R_X86_64_IRELATIVE
   relocation here: a DEFAULT-visibility (exported, preemptible) ifunc
   in a -shared/-fPIC build instead compiles to an ordinary
   R_X86_64_JUMP_SLOT relocation whose referenced .dynsym entry merely
   has st_info type STT_GNU_IFUNC -- a related but DIFFERENT mechanism
   (confirmed empirically; both were compiled and compared with
   readelf -r before choosing this fixture's shape) that this
   increment's scope (R_X86_64_IRELATIVE specifically, per the design
   brief) does not cover; see nl-ffi-loader.el's Commentary. */
__attribute__((visibility("hidden")))
int nl_ffi_loader_fixture_ifunc_call(int x)
  __attribute__((ifunc("nl_ffi_loader_fixture_ifunc_resolver")));

int nl_ffi_loader_fixture_call_ifunc(int x) {
  return nl_ffi_loader_fixture_ifunc_call(x) + 10;
}
