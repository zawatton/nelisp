//! Time-family `extern "C"' trampolines for `nl-current-unix-time' and
//! `nl-format-unix-time'.
//!
//! ABI shapes:
//!   `nl_jit_current_unix_time' : `extern "C" fn(i64, i64) -> i64'
//!     (0-arg primitive bridged via `nl-jit-call-i64-i64'; padding args
//!     ignored).
//!   `nl_jit_format_unix_time'  : `extern "C" fn(*const Sexp,
//!     *const Sexp, *mut Sexp) -> i64' (2-arg Sexp via `nl-jit-call-out-2').

use crate::eval::sexp::Sexp;

const TRAMPOLINE_OK: i64 = 0;
const TRAMPOLINE_ERR: i64 = 1;

/// `(nl-current-unix-time)' — seconds since the Unix epoch as i64.
/// Padding args ignored (bridged via `nl-jit-call-i64-i64').
#[no_mangle]
pub extern "C" fn nl_jit_current_unix_time(_a: i64, _b: i64) -> i64 {
    std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .map(|d| d.as_secs() as i64)
        .unwrap_or(0)
}

/// `(nl-format-unix-time FORMAT EPOCH-INT)' — strftime-style format of
/// a UTC epoch.  FORMAT must be `Sexp::Str' / `Sexp::MutStr'; EPOCH
/// must be `Sexp::Int' or `Sexp::Float' (truncated).  Writes a fresh
/// `Sexp::Str' into OUT.  Returns TRAMPOLINE_ERR on type mismatch or
/// invalid epoch (re-signalled as `wrong-type-argument' by the elisp wrapper).
#[no_mangle]
pub extern "C" fn nl_jit_format_unix_time(
    fmt_arg: *const Sexp,
    epoch_arg: *const Sexp,
    out: *mut Sexp,
) -> i64 {
    let fmt_ref = unsafe { &*fmt_arg };
    let epoch_ref = unsafe { &*epoch_arg };
    let fmt_str = match fmt_ref {
        Sexp::Str(s) => s.clone(),
        Sexp::MutStr(rc) => rc.value.clone(),
        _ => return TRAMPOLINE_ERR,
    };
    let epoch = match epoch_ref {
        Sexp::Int(i) => *i,
        Sexp::Float(f) => *f as i64,
        _ => return TRAMPOLINE_ERR,
    };
    use chrono::{TimeZone, Utc};
    let dt = match Utc.timestamp_opt(epoch, 0).single() {
        Some(d) => d,
        None => return TRAMPOLINE_ERR,
    };
    unsafe {
        *out = Sexp::Str(dt.format(&fmt_str).to_string());
    }
    TRAMPOLINE_OK
}
