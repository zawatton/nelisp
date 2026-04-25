/* nelisp-runtime-module.c — Phase 7.5.4 Emacs module wrapper for
 * libnelisp_runtime.{so,dylib}.
 *
 * Doc 32 v2 §7 (Phase 7.5.4 / T33).  The Phase 7.5.1 MVP shipped a
 * subprocess bridge (`nelisp-runtime exec-bytes <file>` ≈ 1ms / call,
 * see `nelisp-cc-runtime--exec-real`).  This module is the in-process
 * variant: it dlopens libnelisp_runtime.so, looks up the
 * `nelisp_syscall_*` symbols + an internally-emitted `exec_bytes`
 * helper, and exposes them as Emacs Lisp functions through the
 * standard `emacs-module.h` API.  Round-trip is dominated by
 * make_function / funcall plumbing — typically <10 µs / call,
 * i.e. ~100x faster than the subprocess hop, which is what brings
 * the Doc 32 v2 §7 bench gate (≥ 100 tool calls/sec) into reach
 * without reserving any per-call subprocess budget.
 *
 * Build:
 *
 *   1. cargo build --release            (produces libnelisp_runtime.so)
 *   2. cc -shared -fPIC -Wall -Wextra \
 *        -I/usr/include \
 *        -o target/release/nelisp-runtime-module.so \
 *        nelisp-runtime/c-bindings/nelisp-runtime-module.c
 *
 * The module deliberately does NOT link libnelisp_runtime.so at
 * compile time — instead it uses dlopen(3) at module init so the same
 * .so binary works whether the cdylib lives next to it (Cargo
 * default) or on a system-wide LD_LIBRARY_PATH location.  This also
 * sidesteps the soname / install-name churn that would otherwise be
 * required when ANVIL ships a relocatable tarball.
 *
 * Loaded via:
 *
 *   (module-load "/abs/path/to/nelisp-runtime-module.so")
 *
 * Exposed Emacs Lisp functions (top-level after module-load):
 *
 *   (nelisp-runtime-module-load-cdylib PATH)
 *       → t on success, signals on failure.  Tells the C wrapper
 *         where libnelisp_runtime.so lives so dlopen can find it
 *         even when LD_LIBRARY_PATH does not include the worktree.
 *         Emacs-side `setenv' updates `process-environment' only,
 *         not libc's env, so we cannot rely on the env-var hook
 *         alone — the explicit Lisp call is the contract.
 *         Idempotent: subsequent calls are no-ops once the cdylib
 *         is loaded.
 *
 *   (nelisp-runtime-module-syscall-smoke)
 *       → integer (= 0 on success, matches the CLI exit code)
 *
 *   (nelisp-runtime-module-exec-bytes BYTES)
 *       → integer (= the i64 return value of the bytes called as
 *                    `extern "C" fn() -> i64`)
 *       BYTES is a unibyte string of the machine code stream.  The
 *       module mmaps a JIT page, copies BYTES, transitions to RX,
 *       calls the entry point, and unmaps.  Errors signal Emacs
 *       (non_local_exit_signal) so callers see Lisp-style errors
 *       rather than zero-as-sentinel ambiguity.
 *
 *   (nelisp-runtime-module-version)
 *       → string  ("nelisp-runtime-module phase-7.5.4")
 *
 * The module is GPL-3.0-or-later, matching the parent project.  */

#include <emacs-module.h>

#include <dlfcn.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <stddef.h>

/* Required by Emacs to confirm the module is GPL-compatible.  */
int plugin_is_GPL_compatible;

/* ----------------------------------------------------------------
 * dlopen handle for libnelisp_runtime.so + cached function pointers.
 * ----------------------------------------------------------------*/

static void *runtime_handle = NULL;

/* Function pointer types matching the cdylib symbols (see
 * nelisp-runtime/src/syscall/mod.rs).  */
typedef ptrdiff_t (*write_fn_t) (int, const unsigned char *, size_t);
typedef int       (*munmap_fn_t) (unsigned char *, size_t);
typedef unsigned char *(*mmap_fn_t) (unsigned char *, size_t, int, int, int, long);
typedef unsigned char *(*mmap_jit_fn_t) (unsigned char *, size_t, int, int, int, long);
typedef int       (*mprotect_fn_t) (unsigned char *, size_t, int);
typedef int       (*clear_icache_fn_t) (unsigned char *, unsigned char *);
typedef int       (*jit_wp_fn_t) (int);

/* Mirror the NELISP_PROT_* / NELISP_MAP_* constants from the
 * Rust side, so we don't have to dlsym() them at runtime — they are
 * `pub const`-style values, not symbols.  Keep these in sync with
 * nelisp-runtime/src/syscall/mod.rs.  */
#define NELISP_PROT_READ   0x1
#define NELISP_PROT_WRITE  0x2
#define NELISP_PROT_EXEC   0x4
#define NELISP_MAP_PRIVATE   0x02
#define NELISP_MAP_ANONYMOUS 0x20
#define NELISP_MAP_JIT       0x800

static write_fn_t        sym_write        = NULL;
static munmap_fn_t       sym_munmap       = NULL;
static mmap_fn_t         sym_mmap         = NULL;
static mmap_jit_fn_t     sym_mmap_jit     = NULL;
static mprotect_fn_t     sym_mprotect     = NULL;
static clear_icache_fn_t sym_clear_icache = NULL;
static jit_wp_fn_t       sym_jit_wp       = NULL;

/* ----------------------------------------------------------------
 * Helpers.
 * ----------------------------------------------------------------*/

static void
signal_error (emacs_env *env, const char *msg)
{
  emacs_value sym  = env->intern (env, "error");
  emacs_value data = env->make_string (env, msg, (ptrdiff_t) strlen (msg));
  emacs_value list_args[1] = { data };
  emacs_value list_fn      = env->intern (env, "list");
  emacs_value list_data    = env->funcall (env, list_fn, 1, list_args);
  env->non_local_exit_signal (env, sym, list_data);
}

static int
load_runtime_symbols (const char *override_path, char *errbuf, size_t errlen)
{
  if (runtime_handle != NULL)
    return 0;  /* Already loaded — idempotent.  */

  /* Search order:
   *   1. NELISP_RUNTIME_SO env var (explicit override).
   *   2. The override_path argument (passed by the auto-detect helper
   *      in nelisp-cc-runtime.el, which knows the worktree layout).
   *   3. Bare "libnelisp_runtime.so" — works when the .so lives on
   *      LD_LIBRARY_PATH or has been symlinked into a system path.
   */
  const char *env_so = getenv ("NELISP_RUNTIME_SO");
  const char *candidates[3] = { env_so, override_path, "libnelisp_runtime.so" };
  for (int i = 0; i < 3; i++)
    {
      if (candidates[i] == NULL || candidates[i][0] == '\0')
        continue;
      runtime_handle = dlopen (candidates[i], RTLD_NOW | RTLD_LOCAL);
      if (runtime_handle != NULL)
        break;
    }
  if (runtime_handle == NULL)
    {
      const char *err = dlerror ();
      snprintf (errbuf, errlen,
                "nelisp-runtime-module: dlopen failed: %s",
                err ? err : "(no diagnostic)");
      return -1;
    }

  /* Resolve the syscall surface we actually use.  Missing symbols are
   * a hard error — the cdylib must be from a compatible source tree.
   * Spelled out individually so the dlsym name is a string literal,
   * not a macro-stripped substring (cleaner under -Wpedantic and
   * easier to grep when a symbol rename happens upstream).  */
  sym_write = (write_fn_t) dlsym (runtime_handle, "nelisp_syscall_write");
  if (sym_write == NULL)
    {
      snprintf (errbuf, errlen,
                "nelisp-runtime-module: dlsym(nelisp_syscall_write) failed");
      return -1;
    }
  sym_munmap = (munmap_fn_t) dlsym (runtime_handle, "nelisp_syscall_munmap");
  if (sym_munmap == NULL)
    {
      snprintf (errbuf, errlen,
                "nelisp-runtime-module: dlsym(nelisp_syscall_munmap) failed");
      return -1;
    }
  sym_mmap = (mmap_fn_t) dlsym (runtime_handle, "nelisp_syscall_mmap");
  if (sym_mmap == NULL)
    {
      snprintf (errbuf, errlen,
                "nelisp-runtime-module: dlsym(nelisp_syscall_mmap) failed");
      return -1;
    }
  sym_mmap_jit = (mmap_jit_fn_t) dlsym (runtime_handle,
                                         "nelisp_syscall_mmap_jit");
  if (sym_mmap_jit == NULL)
    {
      snprintf (errbuf, errlen,
                "nelisp-runtime-module: dlsym(nelisp_syscall_mmap_jit) failed");
      return -1;
    }
  sym_mprotect = (mprotect_fn_t) dlsym (runtime_handle,
                                         "nelisp_syscall_mprotect");
  if (sym_mprotect == NULL)
    {
      snprintf (errbuf, errlen,
                "nelisp-runtime-module: dlsym(nelisp_syscall_mprotect) failed");
      return -1;
    }
  sym_clear_icache = (clear_icache_fn_t) dlsym (runtime_handle,
                                                 "nelisp_syscall_clear_icache");
  if (sym_clear_icache == NULL)
    {
      snprintf (errbuf, errlen,
                "nelisp-runtime-module: dlsym(nelisp_syscall_clear_icache) failed");
      return -1;
    }
  sym_jit_wp = (jit_wp_fn_t) dlsym (runtime_handle,
                                     "nelisp_syscall_jit_write_protect");
  if (sym_jit_wp == NULL)
    {
      snprintf (errbuf, errlen,
                "nelisp-runtime-module: dlsym(nelisp_syscall_jit_write_protect) failed");
      return -1;
    }

  return 0;
}

/* Round N up to the next multiple of `page_size`.  */
static size_t
page_align (size_t n, size_t page_size)
{
  return ((n + page_size - 1) / page_size) * page_size;
}

/* ----------------------------------------------------------------
 * Lisp-callable: (nelisp-runtime-module-load-cdylib PATH)
 *
 * Load `libnelisp_runtime.so` from PATH (a Lisp string).  Idempotent:
 * once the handle is open, subsequent calls return t without
 * re-loading.  Required because Emacs `setenv` only updates the Lisp
 * `process-environment` variable, not libc's environment, so the
 * `NELISP_RUNTIME_SO` env-var hook is unreliable — the Lisp side
 * passes the path explicitly via this function.
 * ----------------------------------------------------------------*/

static emacs_value
Fnelisp_runtime_module_load_cdylib (emacs_env *env, ptrdiff_t nargs,
                                     emacs_value *args, void *data)
{
  (void) nargs; (void) data;

  if (runtime_handle != NULL)
    return env->intern (env, "t");

  /* Extract PATH (Lisp string) into a heap buffer.  */
  ptrdiff_t plen = 0;
  env->copy_string_contents (env, args[0], NULL, &plen);
  if (plen <= 1)
    {
      signal_error (env, "nelisp-runtime-module-load-cdylib: empty PATH");
      return env->intern (env, "nil");
    }
  char *path = malloc ((size_t) plen);
  if (path == NULL)
    {
      signal_error (env, "nelisp-runtime-module-load-cdylib: OOM");
      return env->intern (env, "nil");
    }
  if (!env->copy_string_contents (env, args[0], path, &plen))
    {
      free (path);
      signal_error (env, "nelisp-runtime-module-load-cdylib: copy failed");
      return env->intern (env, "nil");
    }

  char errbuf[512] = { 0 };
  int rc = load_runtime_symbols (path, errbuf, sizeof errbuf);
  free (path);
  if (rc != 0)
    {
      signal_error (env, errbuf);
      return env->intern (env, "nil");
    }
  return env->intern (env, "t");
}

/* ----------------------------------------------------------------
 * Lisp-callable: (nelisp-runtime-module-version)
 * ----------------------------------------------------------------*/

static emacs_value
Fnelisp_runtime_module_version (emacs_env *env, ptrdiff_t nargs,
                                 emacs_value *args, void *data)
{
  (void) nargs; (void) args; (void) data;
  const char *v = "nelisp-runtime-module phase-7.5.4";
  return env->make_string (env, v, (ptrdiff_t) strlen (v));
}

/* ----------------------------------------------------------------
 * Lisp-callable: (nelisp-runtime-module-syscall-smoke)
 *
 * Mirrors the CLI's `--syscall-smoke` exit code semantics: returns 0
 * when every probed FFI symbol behaves correctly, non-zero on first
 * failure.  Designed so existing T13 ERT can switch over by replacing
 * the call-process hop with a funcall.
 * ----------------------------------------------------------------*/

static emacs_value
Fnelisp_runtime_module_syscall_smoke (emacs_env *env, ptrdiff_t nargs,
                                       emacs_value *args, void *data)
{
  (void) nargs; (void) args; (void) data;

  char errbuf[256] = { 0 };
  if (load_runtime_symbols (NULL, errbuf, sizeof errbuf) != 0)
    {
      signal_error (env, errbuf);
      return env->make_integer (env, -1);
    }

  /* 1. write probe.  */
  const char *msg = "nelisp-runtime-module syscall smoke OK\n";
  ptrdiff_t n = sym_write (1, (const unsigned char *) msg, strlen (msg));
  if ((size_t) n != strlen (msg))
    return env->make_integer (env, 1);

  /* 2. mmap a 4 KiB anon page, touch, munmap.  */
  unsigned char *p = sym_mmap (NULL, 4096,
                                NELISP_PROT_READ | NELISP_PROT_WRITE,
                                NELISP_MAP_PRIVATE | NELISP_MAP_ANONYMOUS,
                                -1, 0);
  if (p == NULL || (intptr_t) p == -1)
    return env->make_integer (env, 3);
  *p = 0x42;
  if (sym_munmap (p, 4096) != 0)
    return env->make_integer (env, 3);

  return env->make_integer (env, 0);
}

/* ----------------------------------------------------------------
 * Lisp-callable: (nelisp-runtime-module-exec-bytes BYTES)
 *
 * BYTES must be a *unibyte string* (Emacs `string-bytes` == length).
 * The module mmaps a JIT page, copies BYTES into it, transitions the
 * page to RX (Linux mprotect; macOS pthread_jit_write_protect_np via
 * the Rust shim), invalidates the I-cache, calls the bytes as
 * `extern "C" fn() -> i64`, prints + returns the i64, and munmaps.
 *
 * On any failure the function signals an Emacs error rather than
 * returning a sentinel; this keeps the contract symmetric with the
 * subprocess path's (:error EXIT STDOUT) tuple.
 * ----------------------------------------------------------------*/

static emacs_value
Fnelisp_runtime_module_exec_bytes (emacs_env *env, ptrdiff_t nargs,
                                    emacs_value *args, void *data)
{
  (void) nargs; (void) data;

  char errbuf[256] = { 0 };
  if (load_runtime_symbols (NULL, errbuf, sizeof errbuf) != 0)
    {
      signal_error (env, errbuf);
      return env->make_integer (env, -1);
    }

  /* 1. Extract the unibyte string into a heap buffer.  */
  ptrdiff_t len = 0;
  bool ok = env->copy_string_contents (env, args[0], NULL, &len);
  if (!ok)
    {
      /* The caller may have passed a multibyte string; we ask for the
       * raw byte length first, then copy.  copy_string_contents sets
       * `len` to the required buffer size on the first call.  */
    }
  if (len <= 1)  /* len includes trailing NUL; empty bytes = signal.  */
    {
      signal_error (env, "nelisp-runtime-module-exec-bytes: empty BYTES");
      return env->make_integer (env, -1);
    }
  char *buf = malloc ((size_t) len);
  if (buf == NULL)
    {
      signal_error (env, "nelisp-runtime-module-exec-bytes: OOM");
      return env->make_integer (env, -1);
    }
  if (!env->copy_string_contents (env, args[0], buf, &len))
    {
      free (buf);
      signal_error (env, "nelisp-runtime-module-exec-bytes: copy failed");
      return env->make_integer (env, -1);
    }
  /* The trailing NUL is appended by copy_string_contents — do not
   * include it in the executable extent.  */
  size_t code_len = (size_t) len - 1;
  if (code_len == 0)
    {
      free (buf);
      signal_error (env, "nelisp-runtime-module-exec-bytes: zero-length code");
      return env->make_integer (env, -1);
    }

  /* 2. Page-align upward.  */
  size_t page_size  = 4096;
  size_t mapped_len = page_align (code_len, page_size);

  /* 3. Mmap a JIT-capable page.  */
  unsigned char *page =
    sym_mmap_jit (NULL, mapped_len,
                  NELISP_PROT_READ | NELISP_PROT_WRITE,
                  NELISP_MAP_PRIVATE | NELISP_MAP_ANONYMOUS | NELISP_MAP_JIT,
                  -1, 0);
  if (page == NULL || (intptr_t) page == -1)
    {
      free (buf);
      signal_error (env, "nelisp-runtime-module-exec-bytes: mmap_jit failed");
      return env->make_integer (env, -1);
    }

  /* 4. macOS Apple Silicon: opt the current thread into write mode.
   *    No-op on Linux / x86_64.  */
  sym_jit_wp (0);

  /* 5. Copy bytes into the page.  */
  memcpy (page, buf, code_len);
  free (buf);

  /* 6. macOS: lock back to exec-only.  Linux: flip RW → RX.  */
  sym_jit_wp (1);
  int rc = sym_mprotect (page, mapped_len,
                         NELISP_PROT_READ | NELISP_PROT_EXEC);
  if (rc != 0)
    {
      sym_munmap (page, mapped_len);
      signal_error (env, "nelisp-runtime-module-exec-bytes: mprotect(RX) failed");
      return env->make_integer (env, -1);
    }

  /* 7. arm64 demands an explicit I-cache flush; x86_64 is no-op.  */
  sym_clear_icache (page, page + code_len);

  /* 8. Hand the page off to the CPU under the System V AMD64 /
   *    AAPCS64 `extern "C" fn() -> i64` ABI.  No arguments.  */
  long (*entry) (void) = (long (*) (void)) (void *) page;
  long result = entry ();

  /* 9. Cleanup.  */
  sym_munmap (page, mapped_len);

  return env->make_integer (env, (intmax_t) result);
}

/* ----------------------------------------------------------------
 * Module init.
 * ----------------------------------------------------------------*/

/* Helper: bind `name` -> `func` at top level.  Same pattern as the
 * canonical Emacs module sample.  */
static void
bind_function (emacs_env *env, const char *name, emacs_value func)
{
  emacs_value Qfset    = env->intern (env, "fset");
  emacs_value Qsym     = env->intern (env, name);
  emacs_value args[2]  = { Qsym, func };
  env->funcall (env, Qfset, 2, args);
}

static void
provide_feature (emacs_env *env, const char *feature)
{
  emacs_value Qfeat    = env->intern (env, feature);
  emacs_value Qprovide = env->intern (env, "provide");
  emacs_value args[1]  = { Qfeat };
  env->funcall (env, Qprovide, 1, args);
}

int
emacs_module_init (struct emacs_runtime *runtime)
{
  /* ABI version check.  Emacs 25+ ship this header; refuse to load
   * on older runtimes (the handshake is the documented compat
   * gate, see emacs-module.h §"emacs_runtime").  */
  if ((size_t) runtime->size < sizeof (struct emacs_runtime))
    return 1;

  emacs_env *env = runtime->get_environment (runtime);
  if ((size_t) env->size < sizeof (struct emacs_env_25))
    return 2;

  /* (nelisp-runtime-module-syscall-smoke) — 0 args.  */
  bind_function (env, "nelisp-runtime-module-syscall-smoke",
                 env->make_function (env, 0, 0,
                                     Fnelisp_runtime_module_syscall_smoke,
                                     "Run libnelisp_runtime FFI smoke probes.\n"
                                     "Returns 0 on success, non-zero on first failure.",
                                     NULL));

  /* (nelisp-runtime-module-exec-bytes BYTES) — 1 arg.  */
  bind_function (env, "nelisp-runtime-module-exec-bytes",
                 env->make_function (env, 1, 1,
                                     Fnelisp_runtime_module_exec_bytes,
                                     "Execute BYTES (unibyte string) as machine code.\n"
                                     "Returns the i64 return value from the entry point.",
                                     NULL));

  /* (nelisp-runtime-module-load-cdylib PATH) — 1 arg.  */
  bind_function (env, "nelisp-runtime-module-load-cdylib",
                 env->make_function (env, 1, 1,
                                     Fnelisp_runtime_module_load_cdylib,
                                     "dlopen libnelisp_runtime.so at the given PATH.\n"
                                     "Idempotent: subsequent calls return t.",
                                     NULL));

  /* (nelisp-runtime-module-version) — 0 args.  */
  bind_function (env, "nelisp-runtime-module-version",
                 env->make_function (env, 0, 0,
                                     Fnelisp_runtime_module_version,
                                     "Return the nelisp-runtime-module version string.",
                                     NULL));

  provide_feature (env, "nelisp-runtime-module");
  return 0;
}
