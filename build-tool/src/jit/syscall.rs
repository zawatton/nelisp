//! Syscall trampolines exported for `bridge.rs`.

#[cfg(target_os = "linux")]
const SUPPORTED_CONST: i64 = 1;
#[cfg(not(target_os = "linux"))]
const SUPPORTED_CONST: i64 = 0;

/// `libc::syscall` with `-errno` normalization.
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

/// Non-Linux keeps the same symbol surface and returns `-ENOSYS`.
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

#[no_mangle]
pub unsafe extern "C" fn nl_jit_syscall_supported_p() -> i64 {
    SUPPORTED_CONST
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn jit_syscall_supported_p_returns_const() {
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
