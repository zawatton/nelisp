#![cfg(all(target_os = "linux", target_arch = "x86_64"))]

use std::os::unix::io::RawFd;

fn with_stdin_from_bytes<F, R>(data: &[u8], f: F) -> R
where
    F: FnOnce() -> R,
{
    // SAFETY: see the `bi_write_stdout_bytes' probe for the same
    // save / replace / restore pattern.
    unsafe {
        let mut fds: [libc::c_int; 2] = [0; 2];
        let pipe_rc = libc::pipe(fds.as_mut_ptr());
        assert_eq!(pipe_rc, 0, "libc::pipe failed");
        let rd: RawFd = fds[0];
        let wr: RawFd = fds[1];

        // Pre-feed the pipe.  Writes <= PIPE_BUF are atomic on Linux.
        if !data.is_empty() {
            let n = libc::write(wr, data.as_ptr() as *const libc::c_void, data.len());
            assert_eq!(
                n as usize,
                data.len(),
                "pipe pre-feed write did not transfer the full payload",
            );
        }
        // Closing the write end signals EOF after the buffer drains —
        // also useful so the read side terminates cleanly even when
        // the caller's `limit' > `data.len()`.
        let _ = libc::close(wr);

        // Save fd 0, replace with the read end.
        let saved: RawFd = libc::dup(0);
        assert!(saved >= 0, "libc::dup(0) failed");
        let dup_rc = libc::dup2(rd, 0);
        assert!(dup_rc >= 0, "libc::dup2(rd, 0) failed");
        let _ = libc::close(rd);

        let r = f();

        // Restore.
        let dup_rc = libc::dup2(saved, 0);
        assert!(dup_rc >= 0, "libc::dup2(saved, 0) failed");
        let _ = libc::close(saved);
        r
    }
}

fn run_read_stdin_bytes(limit: i64, fed: &[u8]) -> (i64, Vec<u8>) {
    let mut buf = vec![0u8; limit as usize];
    let rc = with_stdin_from_bytes(fed, || unsafe {
        nelisp_build_tool::elisp_cc_spike::bi_read_stdin_bytes(buf.as_mut_ptr(), limit)
    });
    (rc, buf)
}

#[test]
fn bi_read_stdin_bytes_eof_when_pipe_is_empty() {
    // Empty pipe + immediate close = EOF on first read.  libc `read'
    // returns 0; the Rust shim maps that to `Sexp::Nil', but the
    // elisp body itself just propagates the 0 through rax.
    let (rc, _buf) = run_read_stdin_bytes(64, b"");
    assert_eq!(rc, 0, "read on empty+closed pipe must return 0 (EOF)");
}

#[test]
fn bi_read_stdin_bytes_short_payload_returns_byte_count() {
    let fed = b"hello";
    let (rc, buf) = run_read_stdin_bytes(64, fed);
    assert_eq!(
        rc,
        fed.len() as i64,
        "read on `hello'-fed pipe must return 5 (the full payload)",
    );
    // The first `rc' bytes of the destination must match the input.
    assert_eq!(&buf[..rc as usize], fed);
}

#[test]
fn bi_read_stdin_bytes_exact_limit_payload_fills_buffer() {
    let fed = b"abcdefghij"; // 10 bytes.
    let (rc, buf) = run_read_stdin_bytes(10, fed);
    assert_eq!(rc, fed.len() as i64);
    assert_eq!(&buf[..], fed);
}

#[test]
fn bi_read_stdin_bytes_oversize_payload_caps_at_limit() {
    // Feed 20 bytes, ask for 8 — `read' must cap at 8 (= the buffer
    // size we passed as `limit').  The remaining 12 bytes are left in
    // the pipe; for this probe we don't drain them (the pipe is
    // restored to its prior state on function exit).
    let fed = b"abcdefghijklmnopqrst"; // 20 bytes.
    let (rc, buf) = run_read_stdin_bytes(8, fed);
    assert_eq!(rc, 8, "read with limit=8 on 20-byte payload must return 8");
    assert_eq!(&buf[..], &fed[..8]);
}
