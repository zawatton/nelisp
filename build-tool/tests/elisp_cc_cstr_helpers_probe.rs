#![cfg(all(target_os = "linux", target_arch = "x86_64"))]

use nelisp_build_tool::eval::sexp::Sexp;

fn build_cstr(s: &str) -> (*mut u8, i64) {
    let sexp = Sexp::Str(s.into());
    let buf = unsafe { nelisp_build_tool::elisp_cc_spike::cstr_from_sexp(&sexp as *const Sexp) };
    assert!(
        !buf.is_null(),
        "cstr_from_sexp({:?}) must succeed on a healthy host",
        s
    );
    (buf, (s.len() as i64) + 1)
}

unsafe fn buf_to_vec(buf: *mut u8, n: usize) -> Vec<u8> {
    let mut out = Vec::with_capacity(n + 1);
    for i in 0..=n {
        out.push(unsafe { *buf.add(i) });
    }
    out
}

// ---- Case 1: empty input ----

#[test]
fn cstr_empty_input_yields_single_nul_byte() {
    let s = "";
    let (buf, size) = build_cstr(s);

    // size == 1 (= 0 payload bytes + 1 NUL).
    assert_eq!(size, 1);

    // The single byte at offset 0 must be the NUL terminator.
    let bytes = unsafe { buf_to_vec(buf, 0) };
    assert_eq!(bytes, vec![0u8]);

    let rc = unsafe { nelisp_build_tool::elisp_cc_spike::cstr_drop(buf, size) };
    assert_eq!(rc, 1, "cstr_drop must return rax = 1 sentinel");
}

// ---- Case 2: single ASCII byte ----

#[test]
fn cstr_single_ascii_byte_then_nul() {
    let s = "x";
    let (buf, size) = build_cstr(s);

    assert_eq!(size, 2);

    let bytes = unsafe { buf_to_vec(buf, 1) };
    assert_eq!(bytes, vec![b'x', 0u8]);

    let rc = unsafe { nelisp_build_tool::elisp_cc_spike::cstr_drop(buf, size) };
    assert_eq!(rc, 1);
}

// ---- Case 3: real path string ----

#[test]
fn cstr_real_path_bytes_match_input_plus_nul() {
    let s = "/etc/hostname";
    let (buf, size) = build_cstr(s);

    assert_eq!(size, (s.len() as i64) + 1);

    let bytes = unsafe { buf_to_vec(buf, s.len()) };
    let mut expected: Vec<u8> = s.bytes().collect();
    expected.push(0);
    assert_eq!(
        bytes, expected,
        "cstr_from_sexp must copy payload bytes verbatim + append NUL"
    );

    let rc = unsafe { nelisp_build_tool::elisp_cc_spike::cstr_drop(buf, size) };
    assert_eq!(rc, 1);
}

// ---- Case 4: libc open(2) round-trip ----

#[test]
fn cstr_buffer_works_with_libc_open() {
    // `/etc/hostname' exists on every linux host and is world-readable
    // without privileges.  If the helper produced a malformed buffer
    // (= missing NUL terminator, bad alignment, scribble somewhere)
    // libc would either return -1 with EINVAL/ENOENT or segfault when
    // walking past the end looking for the NUL.
    let path = "/etc/hostname";
    let (buf, size) = build_cstr(path);

    extern "C" {
        fn open(pathname: *const u8, flags: i32) -> i32;
        fn close(fd: i32) -> i32;
    }
    const O_RDONLY: i32 = 0;

    let fd = unsafe { open(buf as *const u8, O_RDONLY) };
    assert!(
        fd >= 0,
        "open(\"/etc/hostname\", O_RDONLY) returned fd={} — buffer \
         from cstr_from_sexp is not acceptable to libc",
        fd
    );

    let close_rc = unsafe { close(fd) };
    assert_eq!(close_rc, 0, "close(fd) must succeed");

    let rc = unsafe { nelisp_build_tool::elisp_cc_spike::cstr_drop(buf, size) };
    assert_eq!(rc, 1);
}
