/* nelisp-sqlite-module.c — T77 (Wave 1 agent C) Emacs module wrapper
 * for the SQLite FFI surface in `nelisp-runtime/src/sqlite.rs`.
 *
 * T54 standalone gap analysis: anvil-memory / -state / -defs /
 * -org-index reach for Emacs 30 builtin `sqlite-execute' /
 * `sqlite-select' / `sqlite-open' (~110 call sites / 7 modules).  This
 * module bridges those calls to the Rust runtime so NeLisp standalone
 * can run anvil unchanged.
 *
 * Build:
 *
 *   1. cargo build --release           (libnelisp_runtime.so)
 *   2. cc -shared -fPIC -Wall -Wextra -I<emacs-module.h> \
 *        -o target/release/nelisp-sqlite-module.so \
 *        nelisp-runtime/c-bindings/nelisp-sqlite-module.c -ldl
 *
 * Same dlopen-at-init pattern as `nelisp-runtime-module.c'; the cdylib
 * is loaded explicitly via `nelisp-sqlite-module-load-cdylib' so the
 * module works regardless of LD_LIBRARY_PATH (Emacs `setenv' updates
 * Lisp `process-environment' but not libc's environ).
 *
 * Loaded via:
 *
 *   (module-load "/abs/path/to/nelisp-sqlite-module.so")
 *
 * Exposed Emacs Lisp functions (top-level after module-load):
 *
 *   (nelisp-sqlite-module-load-cdylib PATH)
 *       → t on success.  Idempotent.
 *
 *   (nelisp-sqlite-module-open PATH)
 *       → integer handle (> 0) on success, signals on failure.
 *
 *   (nelisp-sqlite-module-close HANDLE)
 *       → t on success, signals on bad handle.
 *
 *   (nelisp-sqlite-module-execute HANDLE SQL ARGS-JSON)
 *       → integer rows-affected.  ARGS-JSON is a string (JSON array)
 *         or nil (no parameters).  Signals on SQL error.
 *
 *   (nelisp-sqlite-module-query HANDLE SQL ARGS-JSON)
 *       → string (JSON array of arrays).  ARGS-JSON same as -execute.
 *         Signals on SQL error.  Caller side decodes via nelisp-json.
 *
 *   (nelisp-sqlite-module-alive-p HANDLE)
 *       → t / nil.
 *
 *   (nelisp-sqlite-module-version)
 *       → string ("nelisp-sqlite-module phase-7.5.4")
 *
 * GPL-3.0-or-later, matching the parent project.  */

#include <emacs-module.h>

#include <dlfcn.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <stddef.h>

int plugin_is_GPL_compatible;

/* ----------------------------------------------------------------
 * dlopen handle for libnelisp_runtime.so + cached function pointers.
 * ----------------------------------------------------------------*/

static void *runtime_handle = NULL;

typedef int64_t (*sqlite_open_fn_t)    (const char *path);
typedef int64_t (*sqlite_close_fn_t)   (int64_t handle);
typedef int64_t (*sqlite_execute_fn_t) (int64_t handle,
                                         const char *sql,
                                         const char *args_json);
typedef int64_t (*sqlite_query_fn_t)   (int64_t handle,
                                         const char *sql,
                                         const char *args_json,
                                         unsigned char *out_buf,
                                         size_t out_buf_len);
typedef int64_t (*sqlite_alive_fn_t)   (int64_t handle);

static sqlite_open_fn_t    sym_open    = NULL;
static sqlite_close_fn_t   sym_close   = NULL;
static sqlite_execute_fn_t sym_execute = NULL;
static sqlite_query_fn_t   sym_query   = NULL;
static sqlite_alive_fn_t   sym_alive   = NULL;

/* Mirror SqliteFfiError discriminants from sqlite.rs.  Keep in sync.  */
#define NL_RC_GENERIC      (-1)
#define NL_RC_BAD_PATH     (-2)
#define NL_RC_BAD_HANDLE   (-3)
#define NL_RC_BAD_SQL      (-4)
#define NL_RC_BAD_ARGS     (-5)
#define NL_RC_BAD_OUT_BUF  (-6)
#define NL_RC_SQLITE_ERROR (-7)
#define NL_RC_NEED_MORE    (-10000)

/* ----------------------------------------------------------------
 * Helpers.
 * ----------------------------------------------------------------*/

static void
signal_error (emacs_env *env, const char *msg)
{
  emacs_value sym       = env->intern (env, "error");
  emacs_value data      = env->make_string (env, msg, (ptrdiff_t) strlen (msg));
  emacs_value list_args[1] = { data };
  emacs_value list_fn   = env->intern (env, "list");
  emacs_value list_data = env->funcall (env, list_fn, 1, list_args);
  env->non_local_exit_signal (env, sym, list_data);
}

static int
load_runtime_symbols (const char *override_path, char *errbuf, size_t errlen)
{
  if (runtime_handle != NULL)
    return 0;
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
                "nelisp-sqlite-module: dlopen failed: %s",
                err ? err : "(no diagnostic)");
      return -1;
    }

#define DLSYM(NAME, TYPE, VAR)                                          \
  do {                                                                  \
    VAR = (TYPE) dlsym (runtime_handle, NAME);                          \
    if (VAR == NULL)                                                    \
      {                                                                 \
        snprintf (errbuf, errlen,                                       \
                  "nelisp-sqlite-module: dlsym(%s) failed", NAME);      \
        return -1;                                                      \
      }                                                                 \
  } while (0)

  DLSYM ("nl_sqlite_open",    sqlite_open_fn_t,    sym_open);
  DLSYM ("nl_sqlite_close",   sqlite_close_fn_t,   sym_close);
  DLSYM ("nl_sqlite_execute", sqlite_execute_fn_t, sym_execute);
  DLSYM ("nl_sqlite_query",   sqlite_query_fn_t,   sym_query);
  DLSYM ("nl_sqlite_alive",   sqlite_alive_fn_t,   sym_alive);

#undef DLSYM
  return 0;
}

/* Extract a Lisp string into a freshly malloc'd C string (NUL-terminated).
 * Returns NULL and signals an error on OOM / copy failure.  Empty Lisp
 * strings yield a "" buffer (not NULL) so callers can distinguish empty
 * from absent (nil → NULL handled at the call site).  */
static char *
lisp_to_cstr (emacs_env *env, emacs_value v)
{
  ptrdiff_t plen = 0;
  if (!env->copy_string_contents (env, v, NULL, &plen))
    return NULL;
  if (plen <= 0)
    plen = 1;
  char *buf = malloc ((size_t) plen);
  if (buf == NULL)
    return NULL;
  if (!env->copy_string_contents (env, v, buf, &plen))
    {
      free (buf);
      return NULL;
    }
  return buf;
}

/* Map a negative i64 return code to a human-readable error string.
 * NEED_MORE is special-cased by the query function and never reaches
 * here.  */
static const char *
errcode_to_msg (int64_t rc)
{
  switch (rc)
    {
    case NL_RC_GENERIC:      return "nelisp-sqlite: generic error";
    case NL_RC_BAD_PATH:     return "nelisp-sqlite: bad path argument";
    case NL_RC_BAD_HANDLE:   return "nelisp-sqlite: bad / freed handle";
    case NL_RC_BAD_SQL:      return "nelisp-sqlite: bad SQL argument";
    case NL_RC_BAD_ARGS:     return "nelisp-sqlite: bad args JSON";
    case NL_RC_BAD_OUT_BUF:  return "nelisp-sqlite: bad output buffer";
    case NL_RC_SQLITE_ERROR: return "nelisp-sqlite: SQLite error (see stderr)";
    default:                 return "nelisp-sqlite: unknown error";
    }
}

/* ----------------------------------------------------------------
 * Lisp-callable: (nelisp-sqlite-module-load-cdylib PATH)
 * ----------------------------------------------------------------*/

static emacs_value
Fnl_sqlite_load_cdylib (emacs_env *env, ptrdiff_t nargs,
                        emacs_value *args, void *data)
{
  (void) nargs; (void) data;
  if (runtime_handle != NULL)
    return env->intern (env, "t");
  char *path = lisp_to_cstr (env, args[0]);
  if (path == NULL)
    {
      signal_error (env, "nelisp-sqlite-module-load-cdylib: bad PATH arg");
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
 * Lisp-callable: (nelisp-sqlite-module-version)
 * ----------------------------------------------------------------*/

static emacs_value
Fnl_sqlite_version (emacs_env *env, ptrdiff_t nargs,
                    emacs_value *args, void *data)
{
  (void) nargs; (void) args; (void) data;
  const char *v = "nelisp-sqlite-module phase-7.5.4";
  return env->make_string (env, v, (ptrdiff_t) strlen (v));
}

/* ----------------------------------------------------------------
 * Lisp-callable: (nelisp-sqlite-module-open PATH)
 * ----------------------------------------------------------------*/

static emacs_value
Fnl_sqlite_open (emacs_env *env, ptrdiff_t nargs,
                 emacs_value *args, void *data)
{
  (void) nargs; (void) data;
  char errbuf[256] = { 0 };
  if (load_runtime_symbols (NULL, errbuf, sizeof errbuf) != 0)
    {
      signal_error (env, errbuf);
      return env->make_integer (env, -1);
    }
  char *path = lisp_to_cstr (env, args[0]);
  if (path == NULL)
    {
      signal_error (env, "nelisp-sqlite-module-open: bad PATH arg");
      return env->make_integer (env, -1);
    }
  int64_t handle = sym_open (path);
  free (path);
  if (handle <= 0)
    {
      signal_error (env, errcode_to_msg (handle));
      return env->make_integer (env, -1);
    }
  return env->make_integer (env, (intmax_t) handle);
}

/* ----------------------------------------------------------------
 * Lisp-callable: (nelisp-sqlite-module-close HANDLE)
 * ----------------------------------------------------------------*/

static emacs_value
Fnl_sqlite_close (emacs_env *env, ptrdiff_t nargs,
                  emacs_value *args, void *data)
{
  (void) nargs; (void) data;
  char errbuf[256] = { 0 };
  if (load_runtime_symbols (NULL, errbuf, sizeof errbuf) != 0)
    {
      signal_error (env, errbuf);
      return env->intern (env, "nil");
    }
  intmax_t handle = env->extract_integer (env, args[0]);
  int64_t rc = sym_close ((int64_t) handle);
  if (rc != 0)
    {
      signal_error (env, errcode_to_msg (rc));
      return env->intern (env, "nil");
    }
  return env->intern (env, "t");
}

/* ----------------------------------------------------------------
 * Lisp-callable: (nelisp-sqlite-module-alive-p HANDLE)
 * ----------------------------------------------------------------*/

static emacs_value
Fnl_sqlite_alive_p (emacs_env *env, ptrdiff_t nargs,
                    emacs_value *args, void *data)
{
  (void) nargs; (void) data;
  char errbuf[256] = { 0 };
  if (load_runtime_symbols (NULL, errbuf, sizeof errbuf) != 0)
    {
      signal_error (env, errbuf);
      return env->intern (env, "nil");
    }
  intmax_t handle = env->extract_integer (env, args[0]);
  int64_t rc = sym_alive ((int64_t) handle);
  return rc == 1 ? env->intern (env, "t") : env->intern (env, "nil");
}

/* Decode a Lisp string-or-nil ARGS-JSON into a malloc'd C string ("" /
 * NULL distinction).  Returns NULL on nil; "" on empty string; new buf
 * on non-empty.  Caller frees.  */
static char *
args_json_to_cstr (emacs_env *env, emacs_value v)
{
  emacs_value Qnil = env->intern (env, "nil");
  if (env->eq (env, v, Qnil))
    return NULL;
  return lisp_to_cstr (env, v);
}

/* ----------------------------------------------------------------
 * Lisp-callable: (nelisp-sqlite-module-execute HANDLE SQL ARGS-JSON)
 * ----------------------------------------------------------------*/

static emacs_value
Fnl_sqlite_execute (emacs_env *env, ptrdiff_t nargs,
                    emacs_value *args, void *data)
{
  (void) nargs; (void) data;
  char errbuf[256] = { 0 };
  if (load_runtime_symbols (NULL, errbuf, sizeof errbuf) != 0)
    {
      signal_error (env, errbuf);
      return env->make_integer (env, -1);
    }
  intmax_t handle = env->extract_integer (env, args[0]);
  char *sql       = lisp_to_cstr (env, args[1]);
  if (sql == NULL)
    {
      signal_error (env, "nelisp-sqlite-module-execute: bad SQL arg");
      return env->make_integer (env, -1);
    }
  char *aj = args_json_to_cstr (env, args[2]);
  int64_t rc = sym_execute ((int64_t) handle, sql, aj);
  free (sql);
  free (aj);
  if (rc < 0)
    {
      signal_error (env, errcode_to_msg (rc));
      return env->make_integer (env, -1);
    }
  return env->make_integer (env, (intmax_t) rc);
}

/* ----------------------------------------------------------------
 * Lisp-callable: (nelisp-sqlite-module-query HANDLE SQL ARGS-JSON)
 *
 * Returns a Lisp unibyte string holding the JSON array-of-arrays
 * payload.  We pre-probe the required size with a NULL/0 buffer call,
 * malloc once, fill, and free.  Pre-probing is cheap (the SQL still
 * runs but the row scan is a single pass), and avoids a retry loop.
 * ----------------------------------------------------------------*/

static emacs_value
Fnl_sqlite_query (emacs_env *env, ptrdiff_t nargs,
                  emacs_value *args, void *data)
{
  (void) nargs; (void) data;
  char errbuf[256] = { 0 };
  if (load_runtime_symbols (NULL, errbuf, sizeof errbuf) != 0)
    {
      signal_error (env, errbuf);
      return env->intern (env, "nil");
    }
  intmax_t handle = env->extract_integer (env, args[0]);
  char *sql       = lisp_to_cstr (env, args[1]);
  if (sql == NULL)
    {
      signal_error (env, "nelisp-sqlite-module-query: bad SQL arg");
      return env->intern (env, "nil");
    }
  char *aj = args_json_to_cstr (env, args[2]);

  /* Probe.  */
  int64_t probe = sym_query ((int64_t) handle, sql, aj, NULL, 0);
  if (probe >= 0)
    {
      /* Empty result fit in zero bytes; payload is "[]".  */
      free (sql);
      free (aj);
      const char *empty = "[]";
      return env->make_string (env, empty, 2);
    }
  if (probe > NL_RC_NEED_MORE)
    {
      /* Real error code, not buffer-too-small.  */
      free (sql);
      free (aj);
      signal_error (env, errcode_to_msg (probe));
      return env->intern (env, "nil");
    }
  size_t needed = (size_t) (NL_RC_NEED_MORE - probe);
  unsigned char *buf = malloc (needed + 1);
  if (buf == NULL)
    {
      free (sql);
      free (aj);
      signal_error (env, "nelisp-sqlite-module-query: OOM");
      return env->intern (env, "nil");
    }
  int64_t n = sym_query ((int64_t) handle, sql, aj, buf, needed);
  free (sql);
  free (aj);
  if (n < 0)
    {
      free (buf);
      signal_error (env, errcode_to_msg (n));
      return env->intern (env, "nil");
    }
  buf[n] = '\0';
  emacs_value s = env->make_string (env, (const char *) buf, (ptrdiff_t) n);
  free (buf);
  return s;
}

/* ----------------------------------------------------------------
 * Module init.
 * ----------------------------------------------------------------*/

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
  if ((size_t) runtime->size < sizeof (struct emacs_runtime))
    return 1;
  emacs_env *env = runtime->get_environment (runtime);
  if ((size_t) env->size < sizeof (struct emacs_env_25))
    return 2;

  bind_function (env, "nelisp-sqlite-module-load-cdylib",
                 env->make_function (env, 1, 1, Fnl_sqlite_load_cdylib,
                                     "dlopen libnelisp_runtime.so at PATH.\n"
                                     "Idempotent: subsequent calls return t.",
                                     NULL));
  bind_function (env, "nelisp-sqlite-module-version",
                 env->make_function (env, 0, 0, Fnl_sqlite_version,
                                     "Return the nelisp-sqlite-module version string.",
                                     NULL));
  bind_function (env, "nelisp-sqlite-module-open",
                 env->make_function (env, 1, 1, Fnl_sqlite_open,
                                     "Open a SQLite connection at PATH; return integer handle.",
                                     NULL));
  bind_function (env, "nelisp-sqlite-module-close",
                 env->make_function (env, 1, 1, Fnl_sqlite_close,
                                     "Close the connection identified by HANDLE.",
                                     NULL));
  bind_function (env, "nelisp-sqlite-module-execute",
                 env->make_function (env, 3, 3, Fnl_sqlite_execute,
                                     "Execute SQL against HANDLE; return rows-affected.\n"
                                     "ARGS-JSON is a JSON array string, or nil.",
                                     NULL));
  bind_function (env, "nelisp-sqlite-module-query",
                 env->make_function (env, 3, 3, Fnl_sqlite_query,
                                     "Query SQL against HANDLE; return JSON array-of-arrays.\n"
                                     "ARGS-JSON is a JSON array string, or nil.",
                                     NULL));
  bind_function (env, "nelisp-sqlite-module-alive-p",
                 env->make_function (env, 1, 1, Fnl_sqlite_alive_p,
                                     "Return t if HANDLE refers to a live connection.",
                                     NULL));

  provide_feature (env, "nelisp-sqlite-module");
  return 0;
}
