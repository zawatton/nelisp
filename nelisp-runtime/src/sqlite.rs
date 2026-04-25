//! T77 (Wave 1 agent C) — SQLite FFI binding for NeLisp.
//!
//! T54 standalone gap analysis revealed that `anvil-memory`,
//! `anvil-state`, `anvil-defs`, and `anvil-org-index` all reach for
//! the host Emacs 30 builtin `sqlite-execute` / `sqlite-select` /
//! `sqlite-open` API directly (~110 call sites across 7 modules).
//! Without a NeLisp-side equivalent the architecture α delegate chain
//! cannot run on a host whose Emacs lacks the SQLite primitives, and
//! Stage D standalone (no-Emacs) is permanently blocked.
//!
//! This module ships the substrate: rusqlite v0.31 with the `bundled`
//! feature (libsqlite3-sys static link, +~1.5 MiB binary) plus a
//! 5-symbol C ABI surface that the `nelisp-sqlite.el` wrapper resolves
//! via dlsym at module init.  The Lisp side then re-exports the
//! Emacs 30-compatible signatures (`sqlite-open` / `sqlite-close` /
//! `sqlite-execute` / `sqlite-select` / `sqlitep`) so anvil's existing
//! call sites work unchanged when bound to NeLisp.
//!
//! ## ABI contract
//!
//! All extern "C" entry points use a tiny string-in / string-out
//! protocol:
//!
//!   * Connection identity is an `i64` handle issued by `nl_sqlite_open`;
//!     `0` is reserved as "no connection" / sentinel for errors.
//!   * Negative return values from `nl_sqlite_*` are NeLisp error codes
//!     (see [`SqliteFfiError`]), positive (or zero) values are the
//!     successful payload.
//!   * SQL parameters are encoded as a JSON array (e.g. `[1, "alpha"]`).
//!     `nelisp-json` handles encode on the Lisp side.
//!   * Query results are encoded as a JSON array of arrays
//!     (`[[id, name], ...]`) and copied into a caller-provided buffer.
//!     The function returns the number of bytes written, or
//!     `RC_NEED_MORE - required` when the buffer is too small (caller
//!     re-allocates and retries — Lisp side handles automatically).
//!
//! ## Concurrency
//!
//! Connections live behind a global `Mutex<HashMap<i64, Connection>>`,
//! which means cross-thread sharing is fine but contention is
//! pessimistic.  This matches the host Emacs 30 builtin's effective
//! single-threaded semantics; anvil currently has zero call sites
//! that exercise concurrent connection access (verified 2026-04-25),
//! so this is not on the critical path.  Phase 7.5+ may revisit if
//! pool benchmarks demand it.

use once_cell::sync::Lazy;
use rusqlite::{params_from_iter, types::ValueRef, Connection, ToSql};
use std::collections::HashMap;
use std::ffi::CStr;
use std::os::raw::c_char;
use std::sync::atomic::{AtomicI64, Ordering};
use std::sync::Mutex;

// ---------------------------------------------------------------------------
// Return-code conventions.  Negative = error (see SqliteFfiError); 0+ = data
// or row-count.
// ---------------------------------------------------------------------------

/// Opaque handle ID for a SQLite connection.  `0` is a sentinel for
/// "no connection" — `nl_sqlite_open` never issues 0.
pub type SqliteHandle = i64;

/// Errors surfaced through the FFI ABI.  Encoded as negative `i64`.
#[repr(i64)]
#[derive(Debug, Clone, Copy)]
pub enum SqliteFfiError {
    /// Generic "something went wrong" — see stderr for diagnostic.
    Generic = -1,
    /// `path` argument was NULL or contained invalid UTF-8.
    BadPath = -2,
    /// Caller passed an unknown / freed connection handle.
    BadHandle = -3,
    /// `sql` argument was NULL or contained invalid UTF-8.
    BadSql = -4,
    /// `args_json` argument was NULL or invalid JSON.
    BadArgs = -5,
    /// `out_buf` was NULL.
    BadOutBuf = -6,
    /// rusqlite returned an error during execute/query.
    SqliteError = -7,
    /// The output buffer was too small.  The function returns
    /// `RC_NEED_MORE - required_bytes` (so the caller can resize and
    /// retry without parsing a separate error code).
    NeedMore = -10000,
}

impl SqliteFfiError {
    fn code(self) -> i64 {
        self as i64
    }
}

// ---------------------------------------------------------------------------
// Global connection registry.  Wrapped in a Mutex; rusqlite::Connection is
// `Send` but not `Sync`.
// ---------------------------------------------------------------------------

static CONNS: Lazy<Mutex<HashMap<SqliteHandle, Connection>>> =
    Lazy::new(|| Mutex::new(HashMap::new()));
static NEXT_HANDLE: AtomicI64 = AtomicI64::new(1);

fn issue_handle() -> SqliteHandle {
    NEXT_HANDLE.fetch_add(1, Ordering::SeqCst)
}

// ---------------------------------------------------------------------------
// Internal helpers (string conversion, JSON arg decoding, value -> JSON).
// ---------------------------------------------------------------------------

unsafe fn cstr_to_str<'a>(p: *const c_char) -> Option<&'a str> {
    if p.is_null() {
        return None;
    }
    CStr::from_ptr(p).to_str().ok()
}

fn json_value_to_sql(v: &serde_json::Value) -> Box<dyn ToSql> {
    use serde_json::Value;
    match v {
        Value::Null => Box::new(rusqlite::types::Null),
        Value::Bool(b) => Box::new(*b as i64),
        Value::Number(n) => {
            if let Some(i) = n.as_i64() {
                Box::new(i)
            } else if let Some(f) = n.as_f64() {
                Box::new(f)
            } else {
                Box::new(n.to_string())
            }
        }
        Value::String(s) => Box::new(s.clone()),
        // JSON arrays / objects are flattened to their string form so the
        // caller can still round-trip via JSON columns.  This matches
        // Emacs 30 builtin behavior, which treats vector / list args
        // verbatim.
        other => Box::new(other.to_string()),
    }
}

fn value_ref_to_json(vr: ValueRef<'_>) -> serde_json::Value {
    use rusqlite::types::ValueRef as VR;
    match vr {
        VR::Null => serde_json::Value::Null,
        VR::Integer(i) => serde_json::Value::Number(i.into()),
        VR::Real(f) => serde_json::Number::from_f64(f)
            .map(serde_json::Value::Number)
            .unwrap_or(serde_json::Value::Null),
        VR::Text(t) => serde_json::Value::String(
            std::str::from_utf8(t).unwrap_or("").to_string(),
        ),
        // Blobs are surfaced as base64 strings so JSON stays valid.
        // anvil-XXX call sites do not currently use BLOB columns, so a
        // simple lossy hex encoding is acceptable for the substrate
        // round-trip.  Phase 7.5+ may revisit if a binary-blob consumer
        // appears.
        VR::Blob(b) => {
            let mut s = String::with_capacity(b.len() * 2);
            for byte in b {
                s.push_str(&format!("{:02x}", byte));
            }
            serde_json::Value::String(s)
        }
    }
}

/// Decode the `args_json` argument into a Vec of boxed `ToSql`.  Empty
/// pointer / empty string / "null" / "[]" all map to "no parameters".
unsafe fn decode_args(
    args_json: *const c_char,
) -> Result<Vec<Box<dyn ToSql>>, SqliteFfiError> {
    if args_json.is_null() {
        return Ok(Vec::new());
    }
    let s = match cstr_to_str(args_json) {
        Some(s) => s,
        None => return Err(SqliteFfiError::BadArgs),
    };
    let trimmed = s.trim();
    if trimmed.is_empty() || trimmed == "null" || trimmed == "[]" {
        return Ok(Vec::new());
    }
    let v: serde_json::Value =
        serde_json::from_str(trimmed).map_err(|_| SqliteFfiError::BadArgs)?;
    let arr = match v {
        serde_json::Value::Array(a) => a,
        // Single scalar -> one-element vec.  Convenience for callers
        // that pass `42` instead of `[42]`.
        other => vec![other],
    };
    Ok(arr.iter().map(json_value_to_sql).collect())
}

/// Write `payload` into `out_buf` if it fits; otherwise return
/// `NeedMore - required`.  Returns bytes written (≥ 0) on success.
unsafe fn write_buf(
    out_buf: *mut u8,
    out_buf_len: usize,
    payload: &str,
) -> i64 {
    let bytes = payload.as_bytes();
    if out_buf.is_null() {
        return SqliteFfiError::BadOutBuf.code();
    }
    if bytes.len() > out_buf_len {
        // Negative encoding: NEED_MORE - required.  Caller decodes by
        // taking abs().  Cap at i64::MIN/2 to avoid overflow in pathological
        // sizes (>= 2^62 bytes would have bigger problems).
        let needed = bytes.len() as i64;
        return SqliteFfiError::NeedMore.code().saturating_sub(needed);
    }
    std::ptr::copy_nonoverlapping(bytes.as_ptr(), out_buf, bytes.len());
    bytes.len() as i64
}

// ---------------------------------------------------------------------------
// Public C ABI surface — 5 symbols + 1 buffer-size probe.
// ---------------------------------------------------------------------------

/// Open a connection to `path` (UTF-8 C string).  Pass `:memory:` for
/// an in-memory database.  Returns a positive handle on success, or a
/// negative `SqliteFfiError` code on failure.
#[no_mangle]
pub unsafe extern "C" fn nl_sqlite_open(path: *const c_char) -> SqliteHandle {
    let path_str = match cstr_to_str(path) {
        Some(s) if !s.is_empty() => s,
        _ => return SqliteFfiError::BadPath.code(),
    };
    let conn = match Connection::open(path_str) {
        Ok(c) => c,
        Err(e) => {
            eprintln!("nl_sqlite_open({}): {}", path_str, e);
            return SqliteFfiError::SqliteError.code();
        }
    };
    let handle = issue_handle();
    let mut map = match CONNS.lock() {
        Ok(m) => m,
        Err(_) => return SqliteFfiError::Generic.code(),
    };
    map.insert(handle, conn);
    handle
}

/// Close the connection identified by `handle`.  Returns 0 on success,
/// negative `SqliteFfiError` on failure (e.g., unknown handle).
/// Idempotent: closing an unknown handle is `BadHandle`.
#[no_mangle]
pub unsafe extern "C" fn nl_sqlite_close(handle: SqliteHandle) -> i64 {
    let mut map = match CONNS.lock() {
        Ok(m) => m,
        Err(_) => return SqliteFfiError::Generic.code(),
    };
    match map.remove(&handle) {
        Some(_) => 0,
        None => SqliteFfiError::BadHandle.code(),
    }
}

/// Execute a non-query SQL statement (DDL or DML) with optional
/// JSON-encoded parameter array.  Returns rows affected on success,
/// negative `SqliteFfiError` code on failure.
///
/// Semantically matches Emacs 30 `sqlite-execute`.
#[no_mangle]
pub unsafe extern "C" fn nl_sqlite_execute(
    handle: SqliteHandle,
    sql: *const c_char,
    args_json: *const c_char,
) -> i64 {
    let sql_str = match cstr_to_str(sql) {
        Some(s) if !s.is_empty() => s,
        _ => return SqliteFfiError::BadSql.code(),
    };
    let args = match decode_args(args_json) {
        Ok(a) => a,
        Err(e) => return e.code(),
    };
    let map = match CONNS.lock() {
        Ok(m) => m,
        Err(_) => return SqliteFfiError::Generic.code(),
    };
    let conn = match map.get(&handle) {
        Some(c) => c,
        None => return SqliteFfiError::BadHandle.code(),
    };
    let arg_refs: Vec<&dyn ToSql> = args.iter().map(|b| b.as_ref()).collect();
    // Use prepare + raw_execute path so PRAGMA-style statements that
    // return rows (e.g. `PRAGMA busy_timeout = 5000') do not trip
    // rusqlite's `Connection::execute' "Execute returned results"
    // guard.  Anvil's call sites mix DDL / DML / PRAGMA / SELECT under
    // a single `sqlite-execute' shape, so we mirror the host Emacs 30
    // builtin which is also tolerant of row-returning statements via
    // execute.
    let mut stmt = match conn.prepare(sql_str) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("nl_sqlite_execute prepare: {}", e);
            return SqliteFfiError::SqliteError.code();
        }
    };
    let mut rows = match stmt.query(params_from_iter(arg_refs.iter())) {
        Ok(r) => r,
        Err(e) => {
            eprintln!("nl_sqlite_execute query: {}", e);
            return SqliteFfiError::SqliteError.code();
        }
    };
    // Drain the (possibly empty) result set so the statement runs to
    // completion before we read `changes()'.
    loop {
        match rows.next() {
            Ok(Some(_)) => continue,
            Ok(None) => break,
            Err(e) => {
                eprintln!("nl_sqlite_execute step: {}", e);
                return SqliteFfiError::SqliteError.code();
            }
        }
    }
    drop(rows);
    drop(stmt);
    conn.changes() as i64
}

/// Run a SELECT-style query, serialize the resulting rows as a JSON
/// array of arrays into `out_buf`, and return bytes written.
///
/// Buffer-size protocol: if `out_buf_len` is too small, returns
/// `NeedMore - required_bytes` (always strictly less than
/// `NeedMore = -10000`).  Caller resizes and retries.  Pass
/// `out_buf = NULL && out_buf_len = 0` to *probe* the required size
/// without writing anything (the probe is performed unconditionally
/// when the supplied buffer is too small).
///
/// Semantically matches Emacs 30 `sqlite-select` (returning row vectors
/// after JSON decode on the Lisp side).
#[no_mangle]
pub unsafe extern "C" fn nl_sqlite_query(
    handle: SqliteHandle,
    sql: *const c_char,
    args_json: *const c_char,
    out_buf: *mut u8,
    out_buf_len: usize,
) -> i64 {
    let sql_str = match cstr_to_str(sql) {
        Some(s) if !s.is_empty() => s,
        _ => return SqliteFfiError::BadSql.code(),
    };
    let args = match decode_args(args_json) {
        Ok(a) => a,
        Err(e) => return e.code(),
    };
    let map = match CONNS.lock() {
        Ok(m) => m,
        Err(_) => return SqliteFfiError::Generic.code(),
    };
    let conn = match map.get(&handle) {
        Some(c) => c,
        None => return SqliteFfiError::BadHandle.code(),
    };
    let mut stmt = match conn.prepare(sql_str) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("nl_sqlite_query prepare: {}", e);
            return SqliteFfiError::SqliteError.code();
        }
    };
    let column_count = stmt.column_count();
    let arg_refs: Vec<&dyn ToSql> = args.iter().map(|b| b.as_ref()).collect();

    let mut rows = match stmt.query(params_from_iter(arg_refs.iter())) {
        Ok(r) => r,
        Err(e) => {
            eprintln!("nl_sqlite_query exec: {}", e);
            return SqliteFfiError::SqliteError.code();
        }
    };
    let mut out_rows: Vec<serde_json::Value> = Vec::new();
    loop {
        match rows.next() {
            Ok(Some(row)) => {
                let mut cells: Vec<serde_json::Value> = Vec::with_capacity(column_count);
                for i in 0..column_count {
                    match row.get_ref(i) {
                        Ok(vr) => cells.push(value_ref_to_json(vr)),
                        Err(e) => {
                            eprintln!("nl_sqlite_query get_ref({}): {}", i, e);
                            return SqliteFfiError::SqliteError.code();
                        }
                    }
                }
                out_rows.push(serde_json::Value::Array(cells));
            }
            Ok(None) => break,
            Err(e) => {
                eprintln!("nl_sqlite_query row: {}", e);
                return SqliteFfiError::SqliteError.code();
            }
        }
    }
    let payload = match serde_json::to_string(&serde_json::Value::Array(out_rows)) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("nl_sqlite_query serialize: {}", e);
            return SqliteFfiError::Generic.code();
        }
    };
    // Probe mode: caller passed a NULL buffer purely to learn the size.
    if out_buf.is_null() && out_buf_len == 0 {
        let needed = payload.as_bytes().len() as i64;
        return SqliteFfiError::NeedMore.code().saturating_sub(needed);
    }
    write_buf(out_buf, out_buf_len, &payload)
}

/// Probe whether `handle` refers to a live connection.  Returns 1 if
/// live, 0 if not.  Used by `nelisp-sqlitep' to keep the predicate
/// honest without re-implementing the registry on the Lisp side.
#[no_mangle]
pub unsafe extern "C" fn nl_sqlite_alive(handle: SqliteHandle) -> i64 {
    if handle <= 0 {
        return 0;
    }
    let map = match CONNS.lock() {
        Ok(m) => m,
        Err(_) => return 0,
    };
    if map.contains_key(&handle) {
        1
    } else {
        0
    }
}

// ---------------------------------------------------------------------------
// Tests (cargo test --release runs these against the same registry as the
// FFI consumer).
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use std::ffi::CString;

    fn cs(s: &str) -> CString {
        CString::new(s).unwrap()
    }

    #[test]
    fn open_close_roundtrip() {
        let path = cs(":memory:");
        unsafe {
            let h = nl_sqlite_open(path.as_ptr());
            assert!(h > 0, "expected positive handle, got {}", h);
            assert_eq!(nl_sqlite_alive(h), 1);
            assert_eq!(nl_sqlite_close(h), 0);
            assert_eq!(nl_sqlite_alive(h), 0);
            // Double-close = BadHandle.
            assert_eq!(nl_sqlite_close(h), SqliteFfiError::BadHandle.code());
        }
    }

    #[test]
    fn create_insert_select() {
        let path = cs(":memory:");
        let create = cs("CREATE TABLE t (id INTEGER, name TEXT)");
        let insert = cs("INSERT INTO t VALUES (?, ?)");
        let select = cs("SELECT id, name FROM t ORDER BY id");
        let null = cs("null");
        let args1 = cs("[1, \"alpha\"]");
        let args2 = cs("[2, \"beta\"]");

        unsafe {
            let h = nl_sqlite_open(path.as_ptr());
            assert!(h > 0);
            assert_eq!(
                nl_sqlite_execute(h, create.as_ptr(), null.as_ptr()),
                0
            );
            assert_eq!(
                nl_sqlite_execute(h, insert.as_ptr(), args1.as_ptr()),
                1
            );
            assert_eq!(
                nl_sqlite_execute(h, insert.as_ptr(), args2.as_ptr()),
                1
            );

            // Probe required size.
            let probe = nl_sqlite_query(
                h,
                select.as_ptr(),
                null.as_ptr(),
                std::ptr::null_mut(),
                0,
            );
            let need_more = SqliteFfiError::NeedMore.code();
            assert!(probe < need_more, "probe got {}", probe);
            let needed = (need_more - probe) as usize;
            assert!(needed > 0);

            let mut buf = vec![0u8; needed];
            let n = nl_sqlite_query(
                h,
                select.as_ptr(),
                null.as_ptr(),
                buf.as_mut_ptr(),
                buf.len(),
            );
            assert_eq!(n, needed as i64);
            let payload = std::str::from_utf8(&buf[..n as usize]).unwrap();
            assert_eq!(payload, "[[1,\"alpha\"],[2,\"beta\"]]");

            assert_eq!(nl_sqlite_close(h), 0);
        }
    }

    #[test]
    fn need_more_then_resize() {
        let path = cs(":memory:");
        let create = cs("CREATE TABLE t (n INTEGER)");
        let insert = cs("INSERT INTO t VALUES (?)");
        let select = cs("SELECT n FROM t");
        let null = cs("null");

        unsafe {
            let h = nl_sqlite_open(path.as_ptr());
            assert_eq!(nl_sqlite_execute(h, create.as_ptr(), null.as_ptr()), 0);
            for i in 0..50 {
                let arg = cs(&format!("[{}]", i));
                assert_eq!(
                    nl_sqlite_execute(h, insert.as_ptr(), arg.as_ptr()),
                    1
                );
            }
            let mut tiny = vec![0u8; 4];
            let r = nl_sqlite_query(
                h,
                select.as_ptr(),
                null.as_ptr(),
                tiny.as_mut_ptr(),
                tiny.len(),
            );
            let need_more = SqliteFfiError::NeedMore.code();
            assert!(r < need_more, "r = {}", r);
            let needed = (need_more - r) as usize;
            let mut big = vec![0u8; needed];
            let n = nl_sqlite_query(
                h,
                select.as_ptr(),
                null.as_ptr(),
                big.as_mut_ptr(),
                big.len(),
            );
            assert_eq!(n, needed as i64);
            assert_eq!(nl_sqlite_close(h), 0);
        }
    }

    #[test]
    fn bad_handle_errors() {
        unsafe {
            let sql = cs("SELECT 1");
            let null = cs("null");
            assert_eq!(
                nl_sqlite_execute(99999, sql.as_ptr(), null.as_ptr()),
                SqliteFfiError::BadHandle.code()
            );
            let mut buf = vec![0u8; 64];
            assert_eq!(
                nl_sqlite_query(
                    99999,
                    sql.as_ptr(),
                    null.as_ptr(),
                    buf.as_mut_ptr(),
                    buf.len()
                ),
                SqliteFfiError::BadHandle.code()
            );
            assert_eq!(
                nl_sqlite_close(99999),
                SqliteFfiError::BadHandle.code()
            );
        }
    }

    #[test]
    fn cjk_utf8_roundtrip() {
        let path = cs(":memory:");
        let create = cs("CREATE TABLE t (s TEXT)");
        let insert = cs("INSERT INTO t VALUES (?)");
        let select = cs("SELECT s FROM t");
        let null = cs("null");
        let args = cs("[\"日本語テスト🎌\"]");

        unsafe {
            let h = nl_sqlite_open(path.as_ptr());
            assert_eq!(nl_sqlite_execute(h, create.as_ptr(), null.as_ptr()), 0);
            assert_eq!(nl_sqlite_execute(h, insert.as_ptr(), args.as_ptr()), 1);
            let probe = nl_sqlite_query(
                h,
                select.as_ptr(),
                null.as_ptr(),
                std::ptr::null_mut(),
                0,
            );
            let need = (SqliteFfiError::NeedMore.code() - probe) as usize;
            let mut buf = vec![0u8; need];
            let n = nl_sqlite_query(
                h,
                select.as_ptr(),
                null.as_ptr(),
                buf.as_mut_ptr(),
                buf.len(),
            );
            assert_eq!(n, need as i64);
            let payload = std::str::from_utf8(&buf[..n as usize]).unwrap();
            assert!(payload.contains("日本語テスト🎌"), "got {}", payload);
            assert_eq!(nl_sqlite_close(h), 0);
        }
    }
}
