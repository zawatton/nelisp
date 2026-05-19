//! Syscall trampolines exported for `bridge.rs`.  Linux x86_64 only
//! (= `lib.rs::compile_error!' enforces).

/// `libc::syscall` with `-errno` normalization.
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

#[no_mangle]
pub unsafe extern "C" fn nl_jit_syscall_supported_p() -> i64 { 1 }

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn jit_syscall_supported_p_returns_const() {
        assert_eq!(unsafe { nl_jit_syscall_supported_p() }, 1);
    }

    #[test]
    fn jit_syscall_getpid_matches_libc() {
        let nr = libc::SYS_getpid as i64;
        let r = unsafe { nl_jit_syscall_call(nr, 0, 0, 0, 0, 0, 0) };
        let direct = unsafe { libc::getpid() } as i64;
        assert_eq!(r, direct);
        assert!(r > 0);
    }

    #[test]
    fn jit_syscall_invalid_fd_returns_neg_ebadf() {
        let nr = libc::SYS_read as i64;
        let mut buf = [0u8; 8];
        let r = unsafe {
            nl_jit_syscall_call(nr, 999, buf.as_mut_ptr() as i64, buf.len() as i64, 0, 0, 0)
        };
        assert!(r < 0, "expected errno, got {}", r);
        assert_eq!(r, -(libc::EBADF as i64));
    }
}
