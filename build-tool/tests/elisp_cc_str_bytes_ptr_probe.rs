#![cfg(all(target_os = "linux", target_arch = "x86_64"))]

//! Doc 122 §122.H — probe for the new `str-bytes-ptr' Phase 47 grammar op
//! (= Rust `nl_str_bytes_ptr' extern) and the first I/O syscall sweep
//! consumer `nelisp_bi_write_stderr_line' (Doc 117 §117.B).
//!
//! The `nl_str_bytes_ptr' extern is the *outward* counterpart of §122.A's
//! `sexp-write-str' allocator: given a `Sexp::Str' / `Sexp::Symbol' /
//! `Sexp::MutStr', return the raw `*const u8' data pointer of the inner
//! byte buffer.  Pair with `str-len' (= byte count) to reach exactly
//! `len' UTF-8 bytes at `[ptr, ptr + len)'.
//!
//! Coverage:
//!
//!   1. `empty_str' / `empty_symbol' / `empty_mut_str' — the
//!      `String::as_ptr()' contract permits a `dangling-but-aligned'
//!      pointer for the empty case; we don't dereference but assert
//!      non-null is *not* required (= can be anything).
//!   2. `ascii_str_round_trip' — read the bytes back from the elisp-
//!      returned pointer using `std::slice::from_raw_parts(ptr, len)'
//!      and check byte-for-byte against the original input.  Exercises
//!      the `Sexp::Str' arm + the full lib.rs round-trip.
//!   3. `utf8_str_round_trip' — same shape with a multi-byte UTF-8
//!      payload (Japanese 藤澤 = 6 bytes / 2 chars).  Proves the data
//!      pointer reaches the raw UTF-8 byte stream, not a pre-decoded
//!      `&str' view.
//!   4. `symbol_round_trip' — `Sexp::Symbol(String)' shares the inline
//!      `String' header with `Sexp::Str' but has a different tag byte;
//!      the §122.H Rust extern's match arm covers both.
//!   5. `mut_str_round_trip' — `Sexp::MutStr(NlStrRef)' lives one
//!      indirection away from the bytes (= the §122.H Rust extern's
//!      reason for existing).  Proves the dispatch handles the
//!      heap-resident `NlStr.value: String' the same way.
//!   6. `non_string_returns_null' — passing `Sexp::Int(42)' returns
//!      `null' (= the catch-all match arm).

use nelisp_build_tool::eval::sexp::Sexp;

// `Sexp' is `#[repr(C, u8)]' (= see `eval/sexp_abi_assert.rs') so passing
// it across an `extern "C"' boundary by raw pointer is sound — same
// rationale as the `elisp_cc_spike` extern block in `build-tool/src/lib.rs'.
// Suppress the `improper_ctypes' lint that fires because `Sexp` embeds
// `String' (which is not `#[repr(C)]') in some variants.
#[allow(improper_ctypes)]
extern "C" {
    fn nl_str_bytes_ptr(sexp_ptr: *const Sexp) -> *const u8;
}

fn bytes_ptr(input: &Sexp) -> *const u8 {
    unsafe { nl_str_bytes_ptr(input as *const Sexp) }
}

#[test]
fn nl_str_bytes_ptr_ascii_str_round_trip() {
    let s = "hello";
    let sexp = Sexp::Str(s.to_string());
    let p = bytes_ptr(&sexp);
    assert!(!p.is_null(), "non-empty str must yield non-null bytes ptr");
    let got = unsafe { std::slice::from_raw_parts(p, s.len()) };
    assert_eq!(got, s.as_bytes(), "ascii bytes must round-trip exactly");
}

#[test]
fn nl_str_bytes_ptr_utf8_str_round_trip() {
    // Japanese 藤澤 — 2 codepoints × 3 UTF-8 bytes = 6 bytes total.
    let s = "藤澤";
    assert_eq!(s.len(), 6);
    let sexp = Sexp::Str(s.to_string());
    let p = bytes_ptr(&sexp);
    assert!(!p.is_null());
    let got = unsafe { std::slice::from_raw_parts(p, s.len()) };
    assert_eq!(
        got,
        s.as_bytes(),
        "utf-8 bytes must round-trip raw (not decoded)"
    );
}

#[test]
fn nl_str_bytes_ptr_empty_str_safe() {
    // `String::as_ptr()' on empty may return a dangling-but-aligned
    // pointer.  We don't dereference — just assert the call returns
    // without crashing and the len-0 pairing is benign.
    let sexp = Sexp::Str(String::new());
    let _p = bytes_ptr(&sexp);
    // No assertion on null-ness — the contract permits either; the
    // important invariant is that pairing with `str-len' = 0 means
    // no bytes are read.
}

#[test]
fn nl_str_bytes_ptr_symbol_round_trip() {
    // `Sexp::Symbol' shares the inline `String' header layout with
    // `Sexp::Str' — the §122.H Rust extern's match arm treats them
    // identically.
    let name = "my-symbol-name";
    let sexp = Sexp::Symbol(name.to_string());
    let p = bytes_ptr(&sexp);
    assert!(!p.is_null());
    let got = unsafe { std::slice::from_raw_parts(p, name.len()) };
    assert_eq!(got, name.as_bytes());
}

#[test]
fn nl_str_bytes_ptr_mut_str_round_trip() {
    // `Sexp::MutStr(NlStrRef)' lives one heap indirection away from
    // the bytes.  The §122.H Rust extern's MutStr arm dispatches
    // through `NlStrRef::deref' to reach the inner `NlStr.value: String'.
    let body = "mutable buffer payload";
    let sexp = Sexp::mut_str(body);
    let p = bytes_ptr(&sexp);
    assert!(!p.is_null());
    let got = unsafe { std::slice::from_raw_parts(p, body.len()) };
    assert_eq!(got, body.as_bytes());
}

#[test]
fn nl_str_bytes_ptr_non_string_returns_null() {
    // Non-string variants return `null' (= catch-all match arm in
    // `nl_str_bytes_ptr').  Callers pair with `str-len' which would
    // also read garbage for non-strings, so the elisp body must
    // tag-check upstream — this test pins the documented contract.
    let sexp = Sexp::Int(42);
    let p = bytes_ptr(&sexp);
    assert!(p.is_null(), "non-string variant must yield null bytes ptr");
}

#[test]
fn elisp_cc_bi_write_stderr_line_round_trip() {
    // Doc 117 §117.B — the first I/O syscall sweep consumer of
    // §122.H.  The elisp body issues `write(2, bytes, len)' against
    // fd 2 (stderr).  We exercise the safe wrapper directly with a
    // short payload + assert the libc `write' return value matches
    // the byte length.  Note: this also writes the payload to the
    // test process's stderr (= visible if `--nocapture' is passed,
    // muted otherwise by cargo's default test harness).  No newline
    // — the trailing `\n' is the Rust shim's responsibility.
    let payload = "doc-122-H probe stderr line\n";
    let sexp = Sexp::Str(payload.to_string());
    let rc =
        unsafe { nelisp_build_tool::elisp_cc_spike::bi_write_stderr_line(&sexp as *const Sexp) };
    // libc `write(2, fd=stderr, ...)' returns the byte count on
    // success; -1 only on EBADF / EIO / EPIPE etc.  Inside cargo test
    // the test harness keeps fd 2 open so success is the only path.
    assert_eq!(
        rc,
        payload.len() as i64,
        "write(2) must return payload byte count on success"
    );
}

#[test]
fn elisp_cc_bi_write_stderr_line_empty_str() {
    // Empty payload → `write(2, ptr, 0)' is a documented no-op that
    // returns 0.  Tests the `str-len = 0' edge of the elisp body.
    let sexp = Sexp::Str(String::new());
    let rc =
        unsafe { nelisp_build_tool::elisp_cc_spike::bi_write_stderr_line(&sexp as *const Sexp) };
    assert_eq!(rc, 0, "empty-payload write must return 0 bytes");
}

#[test]
fn elisp_cc_bi_write_stderr_line_utf8_payload() {
    // Multi-byte UTF-8 payload — proves the elisp body's
    // `str-bytes-ptr' op reaches the raw byte stream and `write(2)'
    // emits the full byte count, not the char count.
    let payload = "藤澤 §122.H";
    let sexp = Sexp::Str(payload.to_string());
    let rc =
        unsafe { nelisp_build_tool::elisp_cc_spike::bi_write_stderr_line(&sexp as *const Sexp) };
    assert_eq!(
        rc,
        payload.len() as i64,
        "utf-8 write(2) must return byte count, not char count"
    );
}
