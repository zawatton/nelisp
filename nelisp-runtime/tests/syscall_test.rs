//! Phase 7.0 cargo tests.
//!
//! Each of the 10 syscall thin wrappers gets at least one test; mmap
//! / munmap / mprotect share a single end-to-end test because they
//! must compose to be useful at all.  The tests intentionally avoid
//! `unsafe` helper layers — every call goes through the same C ABI
//! `nelisp_syscall_*` symbol the cdylib exports, so a regression in
//! linkage or signature will show up here before it reaches the ERT
//! smoke layer.

use std::ffi::CString;
use std::os::unix::ffi::OsStrExt;

use nelisp_runtime::*;

#[test]
fn write_returns_full_length_on_stderr() {
    let msg = b"phase70-cargo-test write smoke\n";
    unsafe {
        let n = nelisp_syscall_write(2, msg.as_ptr(), msg.len());
        assert_eq!(n, msg.len() as isize, "write returned {n}");
    }
}

#[test]
fn write_then_read_via_pipe_roundtrip() {
    let mut fds = [0i32; 2];
    let pipe_rc = unsafe { libc::pipe(fds.as_mut_ptr()) };
    assert_eq!(pipe_rc, 0, "pipe(2) failed");

    let payload = b"roundtrip-7.0";
    unsafe {
        let w = nelisp_syscall_write(fds[1], payload.as_ptr(), payload.len());
        assert_eq!(w, payload.len() as isize);
        // Close write side so read returns EOF after payload bytes.
        assert_eq!(nelisp_syscall_close(fds[1]), 0);

        let mut buf = vec![0u8; payload.len()];
        let r = nelisp_syscall_read(fds[0], buf.as_mut_ptr(), buf.len());
        assert_eq!(r, payload.len() as isize);
        assert_eq!(&buf[..], &payload[..]);
        assert_eq!(nelisp_syscall_close(fds[0]), 0);
    }
}

#[test]
fn open_close_existing_file() {
    // /dev/null is the closest thing we have to a portable
    // always-readable zero-byte file under unit-test conditions.
    let path = CString::new("/dev/null").unwrap();
    unsafe {
        let fd = nelisp_syscall_open(path.as_ptr(), NELISP_O_RDONLY, 0);
        assert!(fd >= 0, "open(/dev/null) returned {fd}");
        assert_eq!(nelisp_syscall_close(fd), 0);
    }
}

#[test]
fn close_invalid_fd_returns_negative() {
    unsafe {
        let r = nelisp_syscall_close(-1);
        assert!(r != 0, "close(-1) unexpectedly succeeded");
    }
}

#[test]
fn mmap_anon_private_4k_then_munmap() {
    unsafe {
        let prot = NELISP_PROT_READ | NELISP_PROT_WRITE;
        let flags = NELISP_MAP_PRIVATE | NELISP_MAP_ANONYMOUS;
        let p = nelisp_syscall_mmap(std::ptr::null_mut(), 4096, prot, flags, -1, 0);
        assert!(!p.is_null());
        assert!(p as isize != -1, "mmap returned MAP_FAILED");
        // Touch the page to prove RW worked.
        *p = 0xAB;
        assert_eq!(*p, 0xAB);
        assert_eq!(nelisp_syscall_munmap(p, 4096), 0);
    }
}

#[test]
fn mprotect_makes_anon_page_read_only() {
    unsafe {
        let prot_rw = NELISP_PROT_READ | NELISP_PROT_WRITE;
        let flags = NELISP_MAP_PRIVATE | NELISP_MAP_ANONYMOUS;
        let p = nelisp_syscall_mmap(std::ptr::null_mut(), 4096, prot_rw, flags, -1, 0);
        assert!(p as isize != -1);
        *p = 1;
        let rc = nelisp_syscall_mprotect(p, 4096, NELISP_PROT_READ);
        assert_eq!(rc, 0, "mprotect returned {rc}");
        // Reading must still work after RO transition.
        assert_eq!(*p, 1);
        // Restore RW so munmap doesn't depend on the protection state.
        let _ = nelisp_syscall_mprotect(p, 4096, prot_rw);
        assert_eq!(nelisp_syscall_munmap(p, 4096), 0);
    }
}

#[test]
fn getenv_path_is_non_null() {
    let key = CString::new("PATH").unwrap();
    unsafe {
        let v = nelisp_syscall_getenv(key.as_ptr());
        assert!(!v.is_null(), "PATH env was unexpectedly NULL");
    }
}

#[test]
fn setenv_then_getenv_roundtrip() {
    let key = CString::new("NELISP_PHASE70_TEST_VAR").unwrap();
    let val = CString::new("phase-7.0-syscall-stub-ok").unwrap();
    unsafe {
        let rc = nelisp_syscall_setenv(key.as_ptr(), val.as_ptr(), 1);
        assert_eq!(rc, 0, "setenv returned {rc}");
        let p = nelisp_syscall_getenv(key.as_ptr());
        assert!(!p.is_null());
        let got = std::ffi::CStr::from_ptr(p).to_bytes();
        assert_eq!(got, val.to_bytes());
    }
}

#[test]
fn stat_dev_null_is_char_special_size_zero() {
    let path = CString::new("/dev/null").unwrap();
    let mut sb = NelispStat::default();
    unsafe {
        let r = nelisp_syscall_stat(path.as_ptr(), &mut sb);
        assert_eq!(r, 0, "stat(/dev/null) returned {r}");
    }
    assert_eq!(sb.st_size, 0);
    // S_IFCHR mask: 0o020000.  Phase 7.0 only checks the type bit so
    // the test stays portable across Linux / macOS where the rest of
    // the mode bits drift.
    let s_ifmt: u32 = 0o170000;
    let s_ifchr: u32 = 0o020000;
    assert_eq!(sb.st_mode & s_ifmt, s_ifchr);
}

#[test]
fn fstat_via_dev_null_fd() {
    let path = CString::new("/dev/null").unwrap();
    let mut sb = NelispStat::default();
    unsafe {
        let fd = nelisp_syscall_open(path.as_ptr(), NELISP_O_RDONLY, 0);
        assert!(fd >= 0);
        let r = nelisp_syscall_fstat(fd, &mut sb);
        assert_eq!(r, 0);
        assert_eq!(nelisp_syscall_close(fd), 0);
    }
    assert_eq!(sb.st_size, 0);
}

#[test]
fn nelisp_stat_struct_layout_is_64_bytes() {
    // Phase 7.5 FFI consumers will reject a layout drift here, so we
    // pin the size & alignment explicitly even though Phase 7.0
    // doesn't dlsym the struct yet.  64 bytes = 9 fields w/ one 4-byte
    // padding hole between `st_mode` (u32) and `st_nlink` (u64).
    use std::mem;
    assert_eq!(mem::size_of::<NelispStat>(), 64);
    assert_eq!(mem::align_of::<NelispStat>(), 8);
}

#[test]
fn syscall_constants_match_libc() {
    // Sanity check: the NELISP_* re-exports are not stale.
    assert_eq!(NELISP_PROT_READ, libc::PROT_READ);
    assert_eq!(NELISP_PROT_WRITE, libc::PROT_WRITE);
    assert_eq!(NELISP_PROT_EXEC, libc::PROT_EXEC);
    assert_eq!(NELISP_MAP_PRIVATE, libc::MAP_PRIVATE);
    assert_eq!(NELISP_MAP_ANONYMOUS, libc::MAP_ANON);
    assert_eq!(NELISP_O_RDONLY, libc::O_RDONLY);
    assert_eq!(NELISP_O_RDWR, libc::O_RDWR);
}

#[test]
fn syscall_error_last_os_error_returns_some_value() {
    // We can't depend on a specific errno (the previous test may have
    // succeeded), but `last_os_error()` must never panic and must
    // return a non-negative integer.
    let e = SyscallError::last_os_error();
    assert!(e.raw() >= 0);
}

// `exit` is exercised by the binary smoke, not here — calling it from
// a unit test would terminate the cargo-test runner.  Instead we verify
// the symbol exists and has the expected signature shape.
#[test]
fn exit_symbol_exists() {
    let f: unsafe extern "C" fn(libc::c_int) -> ! = nelisp_syscall_exit;
    let _ = f as *const ();
}

#[test]
fn ffi_path_resolves_for_current_exe() {
    // Phase 7.0 ERT smoke layer relies on the binary; mirror the same
    // current_exe round trip here so a regression in OS path handling
    // shows up under `cargo test` instead of waiting for ERT.
    let exe = std::env::current_exe().expect("current_exe");
    let path = CString::new(exe.as_os_str().as_bytes()).unwrap();
    let mut sb = NelispStat::default();
    unsafe {
        let r = nelisp_syscall_stat(path.as_ptr(), &mut sb);
        assert_eq!(r, 0);
    }
    assert!(sb.st_size > 0);
}
