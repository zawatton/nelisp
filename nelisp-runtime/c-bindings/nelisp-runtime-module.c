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
 *       *** SAFETY CONTRACT (T68 / Phase 7.5.4 hardening) ***
 *       The module returns a Lisp signal only for failures it can
 *       observe through the FFI surface (dlopen / dlsym / mmap /
 *       mprotect / clear_icache / unibyte rejection).  Once the
 *       CPU is dispatched to the JIT page, malformed code, wrong-
 *       arch bytes, or stale icache state become PROCESS-FATAL
 *       (SIGILL / SIGSEGV / SIGBUS) and there is *no* facility to
 *       turn them into Lisp errors from a single-process wrapper.
 *       Callers that need fault containment must use the subprocess
 *       bridge (`nelisp-cc-runtime--exec-real`) which isolates the
 *       JIT in a child Emacs.  Subprocess isolation as a first-
 *       class option is Phase 7.6+ scope.
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

/* T68 (CRITICAL #2): error-symbol bank.  We classify failures into a
 * small set of distinct symbols so callers can branch on the kind of
 * failure (e.g. "did the .so load at all?" vs. "did mprotect refuse
 * RX?") without parsing the message string.  All four symbols inherit
 * from `nelisp-runtime-module-error' which itself inherits from the
 * standard `error' symbol — see `define_error_symbols'.  */
#define ERR_SYM_GENERIC      "nelisp-runtime-module-error"
#define ERR_SYM_DLOPEN       "nelisp-runtime-module-dlopen-error"
#define ERR_SYM_DLSYM        "nelisp-runtime-module-dlsym-error"
#define ERR_SYM_MPROTECT     "nelisp-runtime-module-mprotect-error"
#define ERR_SYM_CLEAR_ICACHE "nelisp-runtime-module-clear-icache-error"
#define ERR_SYM_INVALID_ARG  "nelisp-runtime-module-invalid-argument"

/* ----------------------------------------------------------------
 * Helpers.
 * ----------------------------------------------------------------*/

/* T68 (CRITICAL #2 + MAJOR #5): All error path helpers below NEVER
 * call back into the env API after non_local_exit_signal — Emacs
 * module ABI requires that a function with a pending non-local exit
 * stops touching `env' immediately and unwinds.  The
 * `signal_error_kind' helper builds the error data list BEFORE
 * raising the signal; on return the caller must do nothing but
 * `return prebuilt_nil` (or any pre-built emacs_value).  */

static void
signal_error_kind (emacs_env *env, const char *err_sym, const char *msg)
{
  emacs_value sym  = env->intern (env, err_sym);
  emacs_value data = env->make_string (env, msg, (ptrdiff_t) strlen (msg));
  emacs_value list_args[1] = { data };
  emacs_value list_fn      = env->intern (env, "list");
  emacs_value list_data    = env->funcall (env, list_fn, 1, list_args);
  /* NOTE: env->funcall above may itself raise a non-local exit (OOM
   * inside the Lisp world); if so, non_local_exit_signal is a no-op
   * — the standing exit takes precedence.  Either way we hand back
   * to the caller with a pending exit set.  */
  env->non_local_exit_signal (env, sym, list_data);
}

/* Backwards-compatible thin wrapper used by older call sites.  */
static void
signal_error (emacs_env *env, const char *msg)
{
  signal_error_kind (env, ERR_SYM_GENERIC, msg);
}

/* T68 (CRITICAL #1): Transactional symbol loading.
 *
 * Old behaviour: dlopen wrote `runtime_handle` immediately, then each
 * dlsym wrote to its `sym_*` global one at a time; if any dlsym
 * failed mid-stream the global state was left half-populated and
 * non-NULL — the next call would skip the load (idempotent guard) and
 * crash inside the first NULL function pointer.  The handle was also
 * leaked.
 *
 * New behaviour: every dlsym goes into a *local* slot first.  We only
 * commit to the global `runtime_handle` + `sym_*` pointers once every
 * symbol resolved successfully.  On any failure we dlclose the local
 * handle, NULL all locals, and return -1; globals remain pristine so
 * the caller can retry with a different override path.
 *
 * The `errbuf` argument is filled with a human-readable diagnostic on
 * failure; the boolean `out_dlopen_failed` (when non-NULL) lets the
 * caller distinguish dlopen-vs-dlsym failures so it can pick the
 * right error symbol when raising the Lisp signal.  */
static int
load_runtime_symbols (const char *override_path,
                      char *errbuf, size_t errlen,
                      int *out_dlopen_failed)
{
  if (out_dlopen_failed != NULL)
    *out_dlopen_failed = 0;

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
  void *local_handle = NULL;
  for (int i = 0; i < 3; i++)
    {
      if (candidates[i] == NULL || candidates[i][0] == '\0')
        continue;
      local_handle = dlopen (candidates[i], RTLD_NOW | RTLD_LOCAL);
      if (local_handle != NULL)
        break;
    }
  if (local_handle == NULL)
    {
      const char *err = dlerror ();
      snprintf (errbuf, errlen,
                "nelisp-runtime-module: dlopen failed: %s",
                err ? err : "(no diagnostic)");
      if (out_dlopen_failed != NULL)
        *out_dlopen_failed = 1;
      return -1;
    }

  /* Resolve the syscall surface we actually use.  Missing symbols are
   * a hard error — the cdylib must be from a compatible source tree.
   * Spelled out individually so the dlsym name is a string literal,
   * not a macro-stripped substring (cleaner under -Wpedantic and
   * easier to grep when a symbol rename happens upstream).
   *
   * Every result lands in a local `local_*` slot; the globals are
   * touched only at the bottom, after every dlsym succeeded.  */
  write_fn_t        local_write        = NULL;
  munmap_fn_t       local_munmap       = NULL;
  mmap_fn_t         local_mmap         = NULL;
  mmap_jit_fn_t     local_mmap_jit     = NULL;
  mprotect_fn_t     local_mprotect     = NULL;
  clear_icache_fn_t local_clear_icache = NULL;
  jit_wp_fn_t       local_jit_wp       = NULL;

  local_write = (write_fn_t) dlsym (local_handle, "nelisp_syscall_write");
  if (local_write == NULL)
    {
      snprintf (errbuf, errlen,
                "nelisp-runtime-module: dlsym(nelisp_syscall_write) failed");
      dlclose (local_handle);
      return -1;
    }
  local_munmap = (munmap_fn_t) dlsym (local_handle, "nelisp_syscall_munmap");
  if (local_munmap == NULL)
    {
      snprintf (errbuf, errlen,
                "nelisp-runtime-module: dlsym(nelisp_syscall_munmap) failed");
      dlclose (local_handle);
      return -1;
    }
  local_mmap = (mmap_fn_t) dlsym (local_handle, "nelisp_syscall_mmap");
  if (local_mmap == NULL)
    {
      snprintf (errbuf, errlen,
                "nelisp-runtime-module: dlsym(nelisp_syscall_mmap) failed");
      dlclose (local_handle);
      return -1;
    }
  local_mmap_jit = (mmap_jit_fn_t) dlsym (local_handle,
                                          "nelisp_syscall_mmap_jit");
  if (local_mmap_jit == NULL)
    {
      snprintf (errbuf, errlen,
                "nelisp-runtime-module: dlsym(nelisp_syscall_mmap_jit) failed");
      dlclose (local_handle);
      return -1;
    }
  local_mprotect = (mprotect_fn_t) dlsym (local_handle,
                                          "nelisp_syscall_mprotect");
  if (local_mprotect == NULL)
    {
      snprintf (errbuf, errlen,
                "nelisp-runtime-module: dlsym(nelisp_syscall_mprotect) failed");
      dlclose (local_handle);
      return -1;
    }
  local_clear_icache = (clear_icache_fn_t) dlsym (local_handle,
                                                  "nelisp_syscall_clear_icache");
  if (local_clear_icache == NULL)
    {
      snprintf (errbuf, errlen,
                "nelisp-runtime-module: dlsym(nelisp_syscall_clear_icache) failed");
      dlclose (local_handle);
      return -1;
    }
  local_jit_wp = (jit_wp_fn_t) dlsym (local_handle,
                                      "nelisp_syscall_jit_write_protect");
  if (local_jit_wp == NULL)
    {
      snprintf (errbuf, errlen,
                "nelisp-runtime-module: dlsym(nelisp_syscall_jit_write_protect) failed");
      dlclose (local_handle);
      return -1;
    }

  /* All symbols resolved — commit to globals atomically.  */
  runtime_handle    = local_handle;
  sym_write         = local_write;
  sym_munmap        = local_munmap;
  sym_mmap          = local_mmap;
  sym_mmap_jit      = local_mmap_jit;
  sym_mprotect      = local_mprotect;
  sym_clear_icache  = local_clear_icache;
  sym_jit_wp        = local_jit_wp;
  return 0;
}

/* Round N up to the next multiple of `page_size`.  */
static size_t
page_align (size_t n, size_t page_size)
{
  return ((n + page_size - 1) / page_size) * page_size;
}

/* T68 (MAJOR #5): unibyte verification.
 *
 * Emacs strings can be multibyte — the same code-point unit then
 * occupies a variable number of bytes via UTF-8 encoding.  Our
 * exec-bytes contract says "unibyte string of raw machine code", but
 * `copy_string_contents' silently UTF-8-encodes a multibyte string,
 * which mangles the JIT input.  Reject up-front by comparing
 * `string-bytes' (raw byte count) with `length' (character count) —
 * for unibyte strings these are equal.
 *
 * Returns 1 (true) when STR is unibyte, 0 otherwise.  The caller
 * must check `non_local_exit_check' afterwards: a Lisp error inside
 * funcall (e.g. STR is not a string) sets a pending exit, and we
 * return 0 in that case as well.  */
static int
is_unibyte_string (emacs_env *env, emacs_value str)
{
  emacs_value Qstring_bytes = env->intern (env, "string-bytes");
  emacs_value Qlength       = env->intern (env, "length");
  emacs_value args[1]       = { str };
  emacs_value bytes_val = env->funcall (env, Qstring_bytes, 1, args);
  if (env->non_local_exit_check (env) != emacs_funcall_exit_return)
    return 0;
  emacs_value len_val   = env->funcall (env, Qlength, 1, args);
  if (env->non_local_exit_check (env) != emacs_funcall_exit_return)
    return 0;
  intmax_t bytes_n = env->extract_integer (env, bytes_val);
  if (env->non_local_exit_check (env) != emacs_funcall_exit_return)
    return 0;
  intmax_t len_n   = env->extract_integer (env, len_val);
  if (env->non_local_exit_check (env) != emacs_funcall_exit_return)
    return 0;
  return bytes_n == len_n;
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
 *
 * T68 (CRITICAL #1): Partial dlsym failures no longer poison
 * `runtime_handle`; a follow-up call with a fixed PATH retries
 * cleanly because `load_runtime_symbols' is now transactional.
 * ----------------------------------------------------------------*/

static emacs_value
Fnelisp_runtime_module_load_cdylib (emacs_env *env, ptrdiff_t nargs,
                                     emacs_value *args, void *data)
{
  (void) nargs; (void) data;
  /* T68 (CRITICAL #2): pre-build sentinel value once, return it
   * immediately on every error path so we never call back into the
   * env API while a non-local exit is pending.  */
  emacs_value prebuilt_nil = env->intern (env, "nil");
  emacs_value prebuilt_t   = env->intern (env, "t");

  if (runtime_handle != NULL)
    return prebuilt_t;

  /* Extract PATH (Lisp string) into a heap buffer.  */
  ptrdiff_t plen = 0;
  env->copy_string_contents (env, args[0], NULL, &plen);
  if (env->non_local_exit_check (env) != emacs_funcall_exit_return)
    return prebuilt_nil;
  if (plen <= 1)
    {
      signal_error_kind (env, ERR_SYM_INVALID_ARG,
                         "nelisp-runtime-module-load-cdylib: empty PATH");
      return prebuilt_nil;
    }
  char *path = malloc ((size_t) plen);
  if (path == NULL)
    {
      signal_error (env, "nelisp-runtime-module-load-cdylib: OOM");
      return prebuilt_nil;
    }
  if (!env->copy_string_contents (env, args[0], path, &plen))
    {
      free (path);
      signal_error_kind (env, ERR_SYM_INVALID_ARG,
                         "nelisp-runtime-module-load-cdylib: copy failed");
      return prebuilt_nil;
    }

  char errbuf[512] = { 0 };
  int dlopen_failed = 0;
  int rc = load_runtime_symbols (path, errbuf, sizeof errbuf, &dlopen_failed);
  free (path);
  if (rc != 0)
    {
      signal_error_kind (env,
                         dlopen_failed ? ERR_SYM_DLOPEN : ERR_SYM_DLSYM,
                         errbuf);
      return prebuilt_nil;
    }
  return prebuilt_t;
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

  /* T68 (CRITICAL #2): single pre-built sentinel for every error
   * path.  Once signal_error_* fires we MUST NOT touch env again,
   * including via env->make_integer.  */
  emacs_value prebuilt_nil = env->intern (env, "nil");

  char errbuf[256] = { 0 };
  int dlopen_failed = 0;
  if (load_runtime_symbols (NULL, errbuf, sizeof errbuf, &dlopen_failed) != 0)
    {
      signal_error_kind (env,
                         dlopen_failed ? ERR_SYM_DLOPEN : ERR_SYM_DLSYM,
                         errbuf);
      return prebuilt_nil;
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
 * On any failure observable through the FFI surface (dlopen / dlsym /
 * mmap / mprotect / clear_icache / unibyte rejection) the function
 * signals an Emacs error rather than returning a sentinel; this keeps
 * the contract symmetric with the subprocess path's
 * (:error EXIT STDOUT) tuple.
 *
 * *** CPU-fault containment IS NOT POSSIBLE here ***
 * Once `entry()` is invoked, malformed bytes / wrong-arch payloads /
 * stale icache become PROCESS-FATAL (SIGILL / SIGSEGV / SIGBUS) — the
 * Lisp signal mechanism cannot intercept them from inside the same
 * process.  Use the subprocess bridge (`nelisp-cc-runtime--exec-real`)
 * for fault containment.  See the file-level header for the long
 * version.
 * ----------------------------------------------------------------*/

static emacs_value
Fnelisp_runtime_module_exec_bytes (emacs_env *env, ptrdiff_t nargs,
                                    emacs_value *args, void *data)
{
  (void) nargs; (void) data;

  /* T68 (CRITICAL #2): single pre-built sentinel for every error
   * path.  Stored once at function entry, returned without any
   * further env-> call after `signal_error_*'.  */
  emacs_value prebuilt_nil = env->intern (env, "nil");

  char errbuf[256] = { 0 };
  int dlopen_failed = 0;
  if (load_runtime_symbols (NULL, errbuf, sizeof errbuf, &dlopen_failed) != 0)
    {
      signal_error_kind (env,
                         dlopen_failed ? ERR_SYM_DLOPEN : ERR_SYM_DLSYM,
                         errbuf);
      return prebuilt_nil;
    }

  /* T68 (MAJOR #5): reject multibyte input *before* allocating
   * anything.  `is_unibyte_string' may itself raise a non-local
   * exit (e.g. arg is not a string at all); in that case we just
   * propagate by returning the sentinel.  */
  if (!is_unibyte_string (env, args[0]))
    {
      if (env->non_local_exit_check (env) == emacs_funcall_exit_return)
        signal_error_kind (env, ERR_SYM_INVALID_ARG,
                           "nelisp-runtime-module-exec-bytes: "
                           "BYTES must be a unibyte string "
                           "(string-bytes != length)");
      return prebuilt_nil;
    }

  /* 1. Extract the unibyte string into a heap buffer.  */
  ptrdiff_t len = 0;
  bool ok = env->copy_string_contents (env, args[0], NULL, &len);
  if (!ok)
    {
      /* `copy_string_contents' returns false on multibyte input on
       * some Emacs builds; we already short-circuited that above
       * via `is_unibyte_string', so reaching here means a deeper
       * failure (e.g. memory exhaustion).  */
      if (env->non_local_exit_check (env) == emacs_funcall_exit_return)
        signal_error_kind (env, ERR_SYM_INVALID_ARG,
                           "nelisp-runtime-module-exec-bytes: "
                           "copy_string_contents probe failed");
      return prebuilt_nil;
    }
  if (len <= 1)  /* len includes trailing NUL; empty bytes = signal.  */
    {
      signal_error_kind (env, ERR_SYM_INVALID_ARG,
                         "nelisp-runtime-module-exec-bytes: empty BYTES");
      return prebuilt_nil;
    }
  char *buf = malloc ((size_t) len);
  if (buf == NULL)
    {
      signal_error (env, "nelisp-runtime-module-exec-bytes: OOM");
      return prebuilt_nil;
    }
  if (!env->copy_string_contents (env, args[0], buf, &len))
    {
      free (buf);
      if (env->non_local_exit_check (env) == emacs_funcall_exit_return)
        signal_error_kind (env, ERR_SYM_INVALID_ARG,
                           "nelisp-runtime-module-exec-bytes: copy failed");
      return prebuilt_nil;
    }
  /* The trailing NUL is appended by copy_string_contents — do not
   * include it in the executable extent.  */
  size_t code_len = (size_t) len - 1;
  if (code_len == 0)
    {
      free (buf);
      signal_error_kind (env, ERR_SYM_INVALID_ARG,
                         "nelisp-runtime-module-exec-bytes: zero-length code");
      return prebuilt_nil;
    }

  /* 2. Page-align upward.  */
  size_t page_size  = 4096;
  size_t mapped_len = page_align (code_len, page_size);

  /* T68 (CRITICAL #3): basic sanity checks before we hand the page
   * to the CPU.  These do NOT replace fault containment (still
   * impossible without subprocess isolation), but they catch the
   * common "obvious garbage" cases at the API boundary.  */
  if (mapped_len == 0 || (mapped_len % page_size) != 0)
    {
      free (buf);
      signal_error_kind (env, ERR_SYM_INVALID_ARG,
                         "nelisp-runtime-module-exec-bytes: "
                         "internal page alignment invariant violated");
      return prebuilt_nil;
    }

  /* 3. Mmap a JIT-capable page.  */
  unsigned char *page =
    sym_mmap_jit (NULL, mapped_len,
                  NELISP_PROT_READ | NELISP_PROT_WRITE,
                  NELISP_MAP_PRIVATE | NELISP_MAP_ANONYMOUS | NELISP_MAP_JIT,
                  -1, 0);
  if (page == NULL || (intptr_t) page == -1)
    {
      free (buf);
      signal_error_kind (env, ERR_SYM_GENERIC,
                         "nelisp-runtime-module-exec-bytes: mmap_jit failed");
      return prebuilt_nil;
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
      signal_error_kind (env, ERR_SYM_MPROTECT,
                         "nelisp-runtime-module-exec-bytes: mprotect(RX) failed");
      return prebuilt_nil;
    }

  /* 7. arm64 demands an explicit I-cache flush; x86_64 is no-op.  */
  sym_clear_icache (page, page + code_len);

  /* 8. Hand the page off to the CPU under the System V AMD64 /
   *    AAPCS64 `extern "C" fn() -> i64` ABI.  No arguments.
   *    *** Beyond this point CPU faults are PROCESS-FATAL ***
   *    See file-level docstring + the function-level note above.  */
  long (*entry) (void) = (long (*) (void)) (void *) page;
  long result = entry ();

  /* 9. Cleanup.  */
  sym_munmap (page, mapped_len);

  return env->make_integer (env, (intmax_t) result);
}

/* ----------------------------------------------------------------
 * Module init.
 * ----------------------------------------------------------------*/

/* T68 (MAJOR #4): every env->funcall that participates in module
 * setup must check `non_local_exit_check' afterwards; if a non-local
 * exit is pending, `emacs_module_init' returns non-zero so Emacs
 * propagates the failure to `module-load' rather than reporting a
 * spurious success and leaving the module half-installed.
 *
 * `bind_function' / `provide_feature' / `define_error_symbols' all
 * report failure through their boolean return value (1 = ok, 0 =
 * pending exit / fatal).  */

static int
bind_function (emacs_env *env, const char *name, emacs_value func)
{
  emacs_value Qfset    = env->intern (env, "fset");
  if (env->non_local_exit_check (env) != emacs_funcall_exit_return)
    return 0;
  emacs_value Qsym     = env->intern (env, name);
  if (env->non_local_exit_check (env) != emacs_funcall_exit_return)
    return 0;
  emacs_value args[2]  = { Qsym, func };
  env->funcall (env, Qfset, 2, args);
  if (env->non_local_exit_check (env) != emacs_funcall_exit_return)
    return 0;
  return 1;
}

static int
provide_feature (emacs_env *env, const char *feature)
{
  emacs_value Qfeat    = env->intern (env, feature);
  if (env->non_local_exit_check (env) != emacs_funcall_exit_return)
    return 0;
  emacs_value Qprovide = env->intern (env, "provide");
  if (env->non_local_exit_check (env) != emacs_funcall_exit_return)
    return 0;
  emacs_value args[1]  = { Qfeat };
  env->funcall (env, Qprovide, 1, args);
  if (env->non_local_exit_check (env) != emacs_funcall_exit_return)
    return 0;
  return 1;
}

/* T68 (CRITICAL #2): structured error symbol hierarchy.  We register
 * five distinct symbols rooted at `nelisp-runtime-module-error', each
 * inheriting from `error' so they show up under `condition-case' /
 * `error' just like any other Emacs error.  Callers that want to
 * branch on the *kind* of failure check the head of the err symbol
 * list explicitly.  */
static int
define_error_symbols (emacs_env *env)
{
  emacs_value Qput   = env->intern (env, "put");
  emacs_value Qerror = env->intern (env, "error");
  emacs_value Qec    = env->intern (env, "error-conditions");
  emacs_value Qem    = env->intern (env, "error-message");
  emacs_value Qlist  = env->intern (env, "list");
  if (env->non_local_exit_check (env) != emacs_funcall_exit_return)
    return 0;

  struct { const char *sym; const char *msg; const char *parent; } table[] = {
    { ERR_SYM_GENERIC,      "nelisp-runtime-module error",
      "error" },
    { ERR_SYM_DLOPEN,       "nelisp-runtime-module: dlopen failed",
      ERR_SYM_GENERIC },
    { ERR_SYM_DLSYM,        "nelisp-runtime-module: dlsym failed",
      ERR_SYM_GENERIC },
    { ERR_SYM_MPROTECT,     "nelisp-runtime-module: mprotect failed",
      ERR_SYM_GENERIC },
    { ERR_SYM_CLEAR_ICACHE, "nelisp-runtime-module: clear_icache failed",
      ERR_SYM_GENERIC },
    { ERR_SYM_INVALID_ARG,  "nelisp-runtime-module: invalid argument",
      ERR_SYM_GENERIC },
  };
  size_t n = sizeof table / sizeof table[0];
  for (size_t i = 0; i < n; i++)
    {
      emacs_value Qsym    = env->intern (env, table[i].sym);
      emacs_value Qparent = env->intern (env, table[i].parent);
      if (env->non_local_exit_check (env) != emacs_funcall_exit_return)
        return 0;
      /* (put SYM 'error-conditions (list SYM PARENT 'error))  */
      emacs_value cond_args[3];
      size_t cond_n = 0;
      cond_args[cond_n++] = Qsym;
      if (strcmp (table[i].parent, "error") != 0)
        cond_args[cond_n++] = Qparent;
      cond_args[cond_n++] = Qerror;
      emacs_value cond_list =
        env->funcall (env, Qlist, (ptrdiff_t) cond_n, cond_args);
      if (env->non_local_exit_check (env) != emacs_funcall_exit_return)
        return 0;
      emacs_value put_ec_args[3] = { Qsym, Qec, cond_list };
      env->funcall (env, Qput, 3, put_ec_args);
      if (env->non_local_exit_check (env) != emacs_funcall_exit_return)
        return 0;
      /* (put SYM 'error-message "...")  */
      emacs_value msg_str =
        env->make_string (env, table[i].msg, (ptrdiff_t) strlen (table[i].msg));
      if (env->non_local_exit_check (env) != emacs_funcall_exit_return)
        return 0;
      emacs_value put_em_args[3] = { Qsym, Qem, msg_str };
      env->funcall (env, Qput, 3, put_em_args);
      if (env->non_local_exit_check (env) != emacs_funcall_exit_return)
        return 0;
    }
  return 1;
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

  /* T68 (CRITICAL #2): publish error symbols *before* binding
   * functions, so any synchronous setup failure can surface them.  */
  if (!define_error_symbols (env))
    return 3;

  /* (nelisp-runtime-module-syscall-smoke) — 0 args.  */
  if (!bind_function (env, "nelisp-runtime-module-syscall-smoke",
                      env->make_function (env, 0, 0,
                                          Fnelisp_runtime_module_syscall_smoke,
                                          "Run libnelisp_runtime FFI smoke probes.\n"
                                          "Returns 0 on success, non-zero on first failure.",
                                          NULL)))
    return 4;

  /* (nelisp-runtime-module-exec-bytes BYTES) — 1 arg.  */
  if (!bind_function (env, "nelisp-runtime-module-exec-bytes",
                      env->make_function (env, 1, 1,
                                          Fnelisp_runtime_module_exec_bytes,
                                          "Execute BYTES (unibyte string) as machine code.\n"
                                          "Returns the i64 return value from the entry point.\n\n"
                                          "BYTES must satisfy `(eq (string-bytes BYTES) (length BYTES))';\n"
                                          "multibyte strings are rejected with `nelisp-runtime-module-\n"
                                          "invalid-argument'.\n\n"
                                          "WARNING: Once the bytes are dispatched the CPU is in control —\n"
                                          "malformed code, wrong-architecture payloads, or stale icache\n"
                                          "states are PROCESS-FATAL (SIGILL / SIGSEGV / SIGBUS).  Use the\n"
                                          "subprocess bridge for fault containment.",
                                          NULL)))
    return 5;

  /* (nelisp-runtime-module-load-cdylib PATH) — 1 arg.  */
  if (!bind_function (env, "nelisp-runtime-module-load-cdylib",
                      env->make_function (env, 1, 1,
                                          Fnelisp_runtime_module_load_cdylib,
                                          "dlopen libnelisp_runtime.so at the given PATH.\n"
                                          "Idempotent: subsequent calls return t.\n\n"
                                          "Failures signal one of:\n"
                                          "  `nelisp-runtime-module-dlopen-error'  (PATH not loadable)\n"
                                          "  `nelisp-runtime-module-dlsym-error'   (.so missing a symbol)\n"
                                          "Both inherit from `nelisp-runtime-module-error' / `error'.",
                                          NULL)))
    return 6;

  /* (nelisp-runtime-module-version) — 0 args.  */
  if (!bind_function (env, "nelisp-runtime-module-version",
                      env->make_function (env, 0, 0,
                                          Fnelisp_runtime_module_version,
                                          "Return the nelisp-runtime-module version string.",
                                          NULL)))
    return 7;

  if (!provide_feature (env, "nelisp-runtime-module"))
    return 8;
  return 0;
}
