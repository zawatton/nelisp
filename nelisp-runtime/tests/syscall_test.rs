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

// ---------------------------------------------------------------------------
// MAP_JIT update (Doc 28 v2 §6.9) — three primitives feed the Phase
// 7.1.3 arm64 backend.  Linux x86_64 (the host CI target) exercises
// only the no-op paths; macOS arm64 hosts will additionally exercise
// the live `pthread_jit_write_protect_np` + `__clear_cache` paths via
// `make test-runtime` once available.
// ---------------------------------------------------------------------------

#[test]
fn mmap_jit_anonymous_4k_then_munmap() {
    unsafe {
        // On Linux NELISP_MAP_JIT == 0 so the OR is a no-op; on macOS
        // arm64 it sets the JIT flag.  Either way the page must
        // allocate and round-trip cleanly.
        let prot = NELISP_PROT_READ | NELISP_PROT_WRITE;
        let flags = NELISP_MAP_PRIVATE | NELISP_MAP_ANONYMOUS | NELISP_MAP_JIT;
        let p = nelisp_syscall_mmap_jit(std::ptr::null_mut(), 4096, prot, flags, -1, 0);
        assert!(!p.is_null());
        assert!(p as isize != -1, "mmap_jit returned MAP_FAILED");
        // Touch one byte to prove RW worked even with the JIT flag
        // set (on hosts where MAP_JIT is 0 this is just a normal page;
        // on Apple Silicon the kernel still honours initial RW prot).
        *p = 0xCD;
        assert_eq!(*p, 0xCD);
        assert_eq!(nelisp_syscall_munmap(p, 4096), 0);
    }
}

#[test]
fn clear_icache_zero_range_returns_zero() {
    // Calling `__clear_cache(p, p)` is documented as a no-op; we use
    // it here to prove the symbol resolves and the return-zero contract
    // holds across cfg gates (x86_64 stub / aarch64 real path).
    unsafe {
        let r = nelisp_syscall_clear_icache(std::ptr::null_mut(), std::ptr::null_mut());
        assert_eq!(r, 0);
    }
}

#[test]
fn jit_write_protect_toggle_no_panic() {
    // Linux: pure no-op returning 0.  macOS arm64: real
    // `pthread_jit_write_protect_np` call.  In neither case do we
    // require a non-zero return — the underlying API has no error
    // channel — but the call must not panic, deadlock, or signal.
    unsafe {
        let r1 = nelisp_syscall_jit_write_protect(0);
        let r2 = nelisp_syscall_jit_write_protect(1);
        // Stub path returns 0; macOS path also returns 0 (we always
        // ignore the void underlying return).  A future host with a
        // real error channel may relax this assertion.
        assert_eq!(r1, 0);
        assert_eq!(r2, 0);
    }
}

#[test]
fn map_jit_constant_matches_per_os_contract() {
    // Linux / Windows: MAP_JIT must degrade to 0 so callers can OR it
    // unconditionally without flipping unrelated bits.  macOS: 0x800.
    if cfg!(target_os = "macos") {
        assert_eq!(NELISP_MAP_JIT, 0x800);
    } else {
        assert_eq!(NELISP_MAP_JIT, 0);
    }
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
