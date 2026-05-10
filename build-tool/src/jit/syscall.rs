//! Phase 7.1.6.e (Doc 28 §3.6.e) — syscall trampolines, dlsym-exported.
//!
//! Pre-7.1.6.e this module hosted a Cranelift IR builder pair that
//! emitted (a) a 2-instruction `iconst + return' for the
//! `nelisp--syscall-supported-p' constant predicate (= 1 on Linux,
//! 0 elsewhere) and (b) a single-`call' trampoline forwarding
//! `(nr, a0..a5)' to the host `libc::syscall' through an `Linkage::
//! Import' helper symbol `nl_jit_syscall_call'.  The Cranelift IR
//! was wrapped by the `JitSyscall' fn-ptr struct brought up at first-
//! access by the unified JITModule — same architecture as the cons /
//! access / arith / predicate clusters deleted in 7.1.6.a.2 / .b /
//! .c.arith / .d.
//!
//! Doc 81 Stage 81.4 + Phase 7.1.6.a.1 dlsym precursor (`6666e61')
//! shipped the elisp-side replacement infrastructure.  Per Doc 28
//! §3.6.e, the syscall cluster mirrors the predicate pattern: there
//! was no separate `lowered_syscall' Rust trampoline body — the
//! Cranelift IR was the implementation for the `supported_p'
//! constant arm, while the helper `nl_jit_syscall_call' already
//! covered the actual libc hop.  This sub-stage collapses the
//! `supported_p' Cranelift IR into a single `#[no_mangle]' Rust
//! trampoline body and re-exports the existing `nl_jit_syscall_call'
//! helper as `#[no_mangle]' so the dlsym bridge can resolve both.
//!
//! Phase 7.1.6.e (this commit) deletes:
//!
//!   - `JitSyscall' / `SyscallIds' fn-ptr structs.
//!   - `declare_const_i64' / `declare_syscall_trampoline' Cranelift
//!     IR builders.
//!   - `register_symbols' / `declare_funcs' / `collect_funcs' wiring
//!     (= `unified_jit()' has now no remaining sub-modules and is
//!     itself deleted; see `jit::mod').
//!
//! What stays (= the surface this module owns post-7.1.6.e):
//!
//!   - `nl_jit_syscall_call(nr, a0..a5) -> i64' — the existing
//!     non-variadic wrapper around `libc::syscall' with errno
//!     normalization.  Now exposed as `#[no_mangle] pub unsafe extern
//!     "C"' so the dlsym bridge resolves it directly.  This is what
//!     the elisp `nelisp--syscall' wrapper actually calls — the
//!     pre-7.1.6.e `nelisp_jit_syscall' Cranelift trampoline was a
//!     same-shape pass-through to this helper, so dropping the
//!     Cranelift wrapper page removes one indirection without
//!     changing semantics.
//!   - `nl_jit_syscall_supported_p() -> i64' — a 1-line `cfg(target_os
//!     = "linux")' constant fn returning 1 / 0.  Mirrors the deleted
//!     2-instruction Cranelift IR.
//!
//! Bridge name mapping (in `bridge.rs::unified_fn_ptr'):
//!
//!   nelisp_jit_syscall              -> nl_jit_syscall_call
//!   nelisp_jit_syscall_supported_p  -> nl_jit_syscall_supported_p
//!
//! 25 specialized primitives (= `sendmsg' / `recvmsg' / `pollfd' /
//! `signalfd' / `sockaddr_un' / etc. buffer handling) are unchanged
//! — they live in `eval::builtins' as Rust dispatcher entries (out
//! of scope per Doc 62 §2.2.1; Doc 80 Stage 5.8 elisp-ifies these in
//! parallel).
//!
//! The `-rdynamic' link flag in `.cargo/config.toml' (= already added
//! by Phase 7.1.6.a.2; syscall trampolines just inherit) pushes the
//! `#[no_mangle]' symbols into the binary's dynamic symbol table so
//! `dlsym(RTLD_DEFAULT, ...)' can locate them at runtime.

/// Linux build returns 1, others return 0.  Mirrors the deleted
/// 2-instruction Cranelift IR (`iconst + return') — the Rust optimizer
/// folds this to the same single-load-+-return at any release profile.
#[cfg(target_os = "linux")]
const SUPPORTED_CONST: i64 = 1;
#[cfg(not(target_os = "linux"))]
const SUPPORTED_CONST: i64 = 0;

/// `nl_jit_syscall_call(nr, a0..a5) -> i64' — non-variadic wrapper for
/// `libc::syscall' that JIT (or dlsym-resolved nelisp-cc) code can
/// call through Cranelift's `Linkage::Import' mechanism / direct
/// dlsym fixup.  Errno is normalized in-wrapper so the JIT entry
/// returns one i64 (negative on error = `-errno', else the raw
/// return value).
///
/// Phase 7.1.6.e dlsym-exported.
///
/// SAFETY: caller must pass a valid syscall NR + appropriately-typed
/// integer args per the Linux syscall ABI.  Same contract as the
/// deleted Cranelift IR (= identical `(i64 × 7) -> i64' shape).
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

/// Non-Linux build: ENOSYS-equivalent so the dlsym entry surface
/// remains identical across platforms even though the OS surface is
/// currently Linux-only (Doc 62 §3 5.6 = future Path B).
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
/// Returns 1 on Linux, 0 otherwise.  Mirrors the deleted Cranelift IR
/// `iconst + return' fragment 1-to-1.
///
/// Phase 7.1.6.e dlsym-exported.
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
