//! Syscall trampolines, dlsym-exported via `-rdynamic'.
//!
//! - `nl_jit_syscall_call(nr, a0..a5) -> i64' — non-variadic wrapper
//!   around `libc::syscall' with errno normalization (negative on
//!   error = `-errno', else raw return).
//! - `nl_jit_syscall_supported_p() -> i64' — cfg-driven 1/0.
//!
//! Bridge name map (`bridge.rs::unified_fn_ptr'):
//!   nelisp_jit_syscall              -> nl_jit_syscall_call
//!   nelisp_jit_syscall_supported_p  -> nl_jit_syscall_supported_p

/// Linux build returns 1, others return 0.
#[cfg(target_os = "linux")]
const SUPPORTED_CONST: i64 = 1;
#[cfg(not(target_os = "linux"))]
const SUPPORTED_CONST: i64 = 0;

/// `nl_jit_syscall_call(nr, a0..a5) -> i64' — non-variadic wrapper for
/// `libc::syscall' that dlsym-resolved nelisp-cc code calls through a
/// direct CALL fixup.  Errno is normalized in-wrapper so the call site
/// returns one i64 (negative on error = `-errno', else the raw return
/// value).
///
/// SAFETY: caller must pass a valid syscall NR + appropriately-typed
/// integer args per the Linux syscall ABI.  `(i64 × 7) -> i64' shape.
#[cfg(target_os = "linux")]
#[no_mangle]
pub unsafe extern "C" fn nl_jit_syscall_call(
    nr: i64,
    a0: i64,
    a1: i64,
    a2: i64,
    a3: i64,
    a4: i64,
    a5: i64,
) -> i64 {
    let r = libc::syscall(nr, a0, a1, a2, a3, a4, a5);
    if r == -1 {
        -(*libc::__errno_location() as i64)
    } else {
        r as i64
    }
}

/// Non-Linux: ENOSYS so the dlsym entry surface is identical across
/// platforms even though the OS surface is currently Linux-only.
#[cfg(not(target_os = "linux"))]
#[no_mangle]
pub unsafe extern "C" fn nl_jit_syscall_call(
    _nr: i64,
    _a0: i64,
    _a1: i64,
    _a2: i64,
    _a3: i64,
    _a4: i64,
    _a5: i64,
) -> i64 {
    -38 /* ENOSYS */
}

/// `nl_jit_syscall_supported_p() -> i64' — 0-arg constant predicate.
/// Returns 1 on Linux, 0 otherwise.
#[no_mangle]
pub unsafe extern "C" fn nl_jit_syscall_supported_p() -> i64 {
    SUPPORTED_CONST
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn jit_syscall_supported_p_returns_const() {
        // On Linux must be 1; on other hosts 0.
        assert_eq!(unsafe { nl_jit_syscall_supported_p() }, SUPPORTED_CONST);
    }

    #[cfg(target_os = "linux")]
    #[test]
    fn jit_syscall_getpid_matches_libc() {
        let nr = libc::SYS_getpid as i64;
        let r = unsafe { nl_jit_syscall_call(nr, 0, 0, 0, 0, 0, 0) };
        let direct = unsafe { libc::getpid() } as i64;
        assert_eq!(r, direct);
        assert!(r > 0);
    }

    #[cfg(target_os = "linux")]
    #[test]
    fn jit_syscall_invalid_fd_returns_neg_ebadf() {
        // SYS_read with fd=999 → -EBADF.
        let nr = libc::SYS_read as i64;
        let mut buf = [0u8; 8];
        let r = unsafe {
            nl_jit_syscall_call(
                nr,
                999,
                buf.as_mut_ptr() as i64,
                buf.len() as i64,
                0,
                0,
                0,
            )
        };
        assert!(r < 0, "expected errno, got {}", r);
        assert_eq!(r, -(libc::EBADF as i64));
    }
}
