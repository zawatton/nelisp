#![cfg(all(target_os = "linux", target_arch = "x86_64"))]

//! Doc 117 §117.B (cont) — probe test for the
//! `bi_write_stdout_bytes' elisp swap.
//!
//! Exercises the Phase 47 `.o' compiled from
//! `lisp/nelisp-cc-bi-write-stdout-bytes.el' through the safe wrapper
//! `nelisp_build_tool::elisp_cc_spike::bi_write_stdout_bytes'.  The
//! handler is a single 3-arg `extern-call' to libc `write(1, ptr, len)';
//! the assertion target is the `write(2)' i64 return:
//!   * non-empty `Sexp::Str' → bytes-written count (= `String::len`).
//!   * empty `Sexp::Str'     → 0.
//!   * `Sexp::Symbol'        → same as Str (= shared inline String header).
//!
//! Each test redirects fd 1 to `/dev/null' for the duration of the
//! call so the probe doesn't pollute cargo's test output stream; the
//! return value is what we assert against (not the actual bytes
//! landing anywhere).  `--test-threads=1' in the standing
//! integration-tests cargo invocation prevents the redirect from
//! racing other tests that print.

use nelisp_build_tool::eval::sexp::Sexp;
use std::os::unix::io::RawFd;

/// Redirect fd 1 to `/dev/null' for the duration of `f', then restore
/// the original fd via `dup2'.  Returns the value `f' produced.
fn with_stdout_to_devnull<F, R>(f: F) -> R
where
    F: FnOnce() -> R,
{
    // SAFETY: this entire function performs unsafe libc calls.  The
    // dup/dup2/close trio is the standard "save + replace + restore"
    // pattern for fd 1; the test harness runs with --test-threads=1
    // so there is no concurrent observer of fd 1 to race against.
    unsafe {
        // Open `/dev/null' write-only.
        let devnull: RawFd = libc::open(
            b"/dev/null\0".as_ptr() as *const libc::c_char,
            libc::O_WRONLY,
        );
        assert!(devnull >= 0, "libc::open(/dev/null) failed");
        // Save the current fd 1.
        let saved: RawFd = libc::dup(1);
        assert!(saved >= 0, "libc::dup(1) failed");
        // Replace fd 1 with /dev/null.
        let dup_rc = libc::dup2(devnull, 1);
        assert!(dup_rc >= 0, "libc::dup2(devnull, 1) failed");
        let _ = libc::close(devnull);

        let r = f();

        // Restore.
        let dup_rc = libc::dup2(saved, 1);
        assert!(dup_rc >= 0, "libc::dup2(saved, 1) failed");
        let _ = libc::close(saved);
        r
    }
}

fn run_write_stdout(input: Sexp) -> i64 {
    with_stdout_to_devnull(|| {
        unsafe {
            nelisp_build_tool::elisp_cc_spike::bi_write_stdout_bytes(
                &input as *const Sexp,
            )
        }
    })
}

#[test]
fn bi_write_stdout_bytes_empty_str_is_zero() {
    // libc `write(1, ptr, 0)' = no-op, returns 0.
    assert_eq!(run_write_stdout(Sexp::Str(String::new())), 0);
}

#[test]
fn bi_write_stdout_bytes_ascii_word_matches_byte_len() {
    let s = "hello";
    let rc = run_write_stdout(Sexp::Str(s.into()));
    // `write' may short-write but for `/dev/null' with N <= PIPE_BUF
    // (= 4096 on Linux) the kernel always accepts the full payload.
    assert_eq!(
        rc,
        s.len() as i64,
        "write(1, \"hello\", 5) must return 5 (full payload accepted)",
    );
}

#[test]
fn bi_write_stdout_bytes_multibyte_utf8_writes_bytes_not_chars() {
    // Japanese "藤澤" is 6 UTF-8 bytes (2 codepoints × 3 bytes each) —
    // the elisp body uses `str-len' (= byte count) for the write
    // length, NOT a char count.
    let s = "藤澤";
    assert_eq!(s.chars().count(), 2);
    assert_eq!(s.len(), 6);
    let rc = run_write_stdout(Sexp::Str(s.into()));
    assert_eq!(rc, s.len() as i64);
}

#[test]
fn bi_write_stdout_bytes_symbol_variant_writes_bytes() {
    // Symbols share the same inline `String` header layout as
    // `Sexp::Str' — `nl_str_bytes_ptr' (§122.H) handles both variants.
    let s = "symbol-name";
    let rc = run_write_stdout(Sexp::Symbol(s.into()));
    assert_eq!(rc, s.len() as i64);
}
