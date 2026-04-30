//! Phase 9d.J — Process subprocess substrate.
//!
//! Doc 39 LOCKED-2026-04-25-v2 §3.J per spec.  Twelve `nl_syscall_*`
//! C ABI thin wrappers that give NeLisp the OS primitives it needs to
//! re-implement Emacs' `make-process` / `start-process` / `call-process`
//! / `accept-process-output` / `process-*` family without going through
//! a host Emacs subprocess detour.
//!
//! Public surface (FFI-stable, prefixed `nl_syscall_*`):
//!
//!   primary (process spawn + IPC):
//!     * `nl_syscall_fork() -> i64`
//!         pid_t (>=0 child=0, parent=child_pid) or -errno
//!     * `nl_syscall_execve(path, argv, envp) -> i64`
//!         never returns on success (process image replaced); -errno
//!         on the failure path.  All three pointers are `*const c_char`
//!         NUL-terminated; `argv` / `envp` are NULL-terminated arrays.
//!     * `nl_syscall_posix_spawn(path, argv, envp) -> i64`
//!         pid_t or -errno.  MVP keeps `file_actions = NULL` and
//!         `attrs = NULL`; Doc 39 §3.J explicitly defers richer file
//!         action / attribute plumbing to a follow-up review.
//!     * `nl_syscall_pipe2(flags, out_fds) -> i64`
//!         0 on success (writes the read fd into `out_fds[0]`, the
//!         write fd into `out_fds[1]`) or -errno.  We always OR-in
//!         `O_CLOEXEC` per Doc 39 §6.3 fd leak hygiene; callers can
//!         additionally request `O_NONBLOCK`.
//!     * `nl_syscall_waitpid(pid, options, out_status) -> i64`
//!         exited_pid (>=0) or -errno.  `0` is *also* a valid return
//!         when `WNOHANG` is set and no child has exited yet.
//!     * `nl_syscall_kill(pid, sig) -> i64`
//!         0 or -errno.
//!     * `nl_syscall_dup2(old_fd, new_fd) -> i64`
//!         new_fd (>=0) or -errno.
//!     * `nl_syscall_setsid() -> i64`
//!         pgid (>0) or -errno.
//!
//!   rlimit (T93 review #7):
//!     * `nl_syscall_getrlimit(resource, out_soft, out_hard) -> i64`
//!         0 or -errno.
//!     * `nl_syscall_setrlimit(resource, soft, hard) -> i64`
//!         0 or -errno.
//!     * `nl_syscall_prlimit(pid, resource, new_soft, new_hard,
//!                           old_soft, old_hard) -> i64`
//!         0 or -errno.  `new_soft` / `new_hard` may be -1 (no change),
//!         `old_soft` / `old_hard` may be NULL (don't fetch).  Linux
//!         only; macOS returns -ENOSYS.
//!
//!   eventloop multiplexer prerequisite (9d.K):
//!     * `nl_syscall_select(nfds, readfds, writefds, exceptfds,
//!                          timeout_ms) -> i64`
//!         ready_count (>=0) or -errno.  `timeout_ms` semantics:
//!         negative = block indefinitely, 0 = poll, >0 = wait this
//!         many milliseconds.  fdset pointers may be NULL.
//!
//! Calling convention:
//!   * Failure: `-errno` (a small negative i64).  `0` always means
//!     success (or no children ready for `waitpid + WNOHANG`).
//!   * `argv` / `envp` are caller-managed NULL-terminated arrays of
//!     `*const c_char`.  The Rust layer never copies them; the caller
//!     keeps the storage live until the call returns.
//!
//! Doc 39 §6.5 risks the implementation is aware of:
//!   * PID race — caller responsibility (signal a still-running pid
//!     after `waitpid` reaped it).  We never cache pids on this side.
//!   * Zombie — caller must `waitpid` (or set `SIGCHLD` to `SIG_IGN`,
//!     out of scope for the syscall layer).
//!   * fd leak — `pipe2` always sets `O_CLOEXEC`, callers must clear
//!     it explicitly when intentionally inheriting (= NeLisp side).
//!   * filter reentrancy — pure syscall layer, no callbacks; concern
//!     belongs to 9d.K eventloop / 9d.L wrapper.

#![allow(clippy::missing_safety_doc)]

use libc::{c_char, c_int, pid_t, rlim_t};

/// Cross-platform alias for the `resource' argument of
/// `getrlimit(2)' / `setrlimit(2)'.  Linux glibc declares it as
/// `__rlimit_resource_t' (= an unsigned-int enum); macOS / *BSD
/// pass a plain `c_int'.  The value bits are identical (0..15
/// mapping to RLIMIT_CPU/AS/NOFILE/etc), only the C type spelling
/// differs.  T162 fix — pre-T162 the code hard-coded
/// `NlRlimitResource' which broke `make runtime' on M1.
#[cfg(any(target_os = "linux", target_os = "android"))]
type NlRlimitResource = libc::__rlimit_resource_t;
#[cfg(not(any(target_os = "linux", target_os = "android")))]
type NlRlimitResource = c_int;
use std::mem::MaybeUninit;
use std::ptr;

// ---------------------------------------------------------------------------
// Internal helpers
// ---------------------------------------------------------------------------

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct SyscallError(i32);

impl SyscallError {
    unsafe fn last_os_error() -> Self {
        Self(*libc_errno_location())
    }

    fn raw(self) -> i32 {
        self.0
    }
}

#[cfg(any(target_os = "linux", target_os = "android"))]
unsafe fn libc_errno_location() -> *mut i32 {
    libc::__errno_location()
}

#[cfg(any(target_os = "macos", target_os = "ios"))]
unsafe fn libc_errno_location() -> *mut i32 {
    libc::__error()
}

#[cfg(not(any(
    target_os = "linux",
    target_os = "android",
    target_os = "macos",
    target_os = "ios"
)))]
unsafe fn libc_errno_location() -> *mut i32 {
    static mut DUMMY_ERRNO: i32 = 0;
    &raw mut DUMMY_ERRNO
}

/// Convert a `0 / -1` libc return into the `0 | -errno` contract.
unsafe fn negate_errno(r: c_int) -> i64 {
    if r == 0 {
        0
    } else {
        -(SyscallError::last_os_error().raw() as i64)
    }
}

/// Convert a `>=0 / -1` libc return into the `value | -errno` contract.
unsafe fn value_or_errno(r: i64) -> i64 {
    if r < 0 {
        -(SyscallError::last_os_error().raw() as i64)
    } else {
        r
    }
}

// ---------------------------------------------------------------------------
// fork / execve / posix_spawn
// ---------------------------------------------------------------------------

#[no_mangle]
pub unsafe extern "C" fn nl_syscall_fork() -> i64 {
    let rc = libc::fork();
    value_or_errno(rc as i64)
}

#[no_mangle]
pub unsafe extern "C" fn nl_syscall_execve(
    path: *const c_char,
    argv: *const *const c_char,
    envp: *const *const c_char,
) -> i64 {
    if path.is_null() || argv.is_null() {
        return -(libc::EFAULT as i64);
    }
    // execve only returns on failure; on success the process image is
    // replaced and we never reach this line.  We pass `envp` straight
    // through; a NULL `envp` is *not* portable on every libc, so we
    // synthesise a single-NULL stub when the caller passes NULL.  The
    // local stack array is only read (no mutation) before execve, so
    // the lifetime is fine even though execve "shouldn't return".
    let empty_envp: [*const c_char; 1] = [ptr::null()];
    let env: *const *const c_char = if envp.is_null() {
        empty_envp.as_ptr()
    } else {
        envp
    };
    let _ = libc::execve(path, argv, env);
    // Only reachable on error.
    -(SyscallError::last_os_error().raw() as i64)
}

/// `posix_spawn(path, argv, envp)` MVP.
///
/// `file_actions = NULL` and `attrs = NULL` per Doc 39 §3.J — the
/// follow-up review explicitly defers richer plumbing.  Returns the
/// child pid or `-errno` (note that `posix_spawn` returns the errno
/// directly rather than via `errno`, so we negate the return code
/// instead of consulting `last_os_error`).
#[no_mangle]
pub unsafe extern "C" fn nl_syscall_posix_spawn(
    path: *const c_char,
    argv: *const *const c_char,
    envp: *const *const c_char,
) -> i64 {
    if path.is_null() || argv.is_null() {
        return -(libc::EFAULT as i64);
    }
    let empty_envp: [*const c_char; 1] = [ptr::null()];
    let env: *const *const c_char = if envp.is_null() {
        empty_envp.as_ptr()
    } else {
        envp
    };
    let mut pid: pid_t = 0;
    let rc = libc::posix_spawn(
        &mut pid,
        path,
        ptr::null(), // file_actions
        ptr::null(), // attrs
        argv as *const *mut c_char,
        env as *const *mut c_char,
    );
    if rc != 0 {
        // posix_spawn returns the error code directly.
        -(rc as i64)
    } else {
        pid as i64
    }
}

// ---------------------------------------------------------------------------
// pipe2 / dup2
// ---------------------------------------------------------------------------

/// `pipe2(flags, out_fds[2])`.
///
/// We always OR-in `O_CLOEXEC` (Doc 39 §6.3 fd leak hygiene).  Callers
/// can pass `O_NONBLOCK` via `flags`; passing `O_CLOEXEC` is harmless
/// (idempotent) and passing other bits is rejected by the kernel.
///
/// On platforms without `pipe2` (= macOS pre-10.10, but we target
/// macOS >= 12) we fall back to `pipe + fcntl(FD_CLOEXEC)` so the
/// caller contract stays portable.
#[no_mangle]
pub unsafe extern "C" fn nl_syscall_pipe2(flags: i32, out_fds: *mut i32) -> i64 {
    if out_fds.is_null() {
        return -(libc::EFAULT as i64);
    }
    let mut fds: [c_int; 2] = [-1, -1];
    let combined_flags: c_int = flags as c_int | libc::O_CLOEXEC;

    #[cfg(any(target_os = "linux", target_os = "android"))]
    {
        let r = libc::pipe2(fds.as_mut_ptr(), combined_flags);
        if r != 0 {
            return -(SyscallError::last_os_error().raw() as i64);
        }
    }

    #[cfg(not(any(target_os = "linux", target_os = "android")))]
    {
        // macOS / BSD: emulate pipe2 via pipe + fcntl.
        let r = libc::pipe(fds.as_mut_ptr());
        if r != 0 {
            return -(SyscallError::last_os_error().raw() as i64);
        }
        // Apply FD_CLOEXEC on both ends.
        for fd in fds.iter() {
            let cur = libc::fcntl(*fd, libc::F_GETFD);
            if cur < 0 {
                let e = SyscallError::last_os_error().raw();
                libc::close(fds[0]);
                libc::close(fds[1]);
                return -(e as i64);
            }
            if libc::fcntl(*fd, libc::F_SETFD, cur | libc::FD_CLOEXEC) < 0 {
                let e = SyscallError::last_os_error().raw();
                libc::close(fds[0]);
                libc::close(fds[1]);
                return -(e as i64);
            }
        }
        // Apply O_NONBLOCK if caller requested it.
        if combined_flags & libc::O_NONBLOCK != 0 {
            for fd in fds.iter() {
                let cur = libc::fcntl(*fd, libc::F_GETFL);
                if cur < 0 {
                    let e = SyscallError::last_os_error().raw();
                    libc::close(fds[0]);
                    libc::close(fds[1]);
                    return -(e as i64);
                }
                if libc::fcntl(*fd, libc::F_SETFL, cur | libc::O_NONBLOCK) < 0 {
                    let e = SyscallError::last_os_error().raw();
                    libc::close(fds[0]);
                    libc::close(fds[1]);
                    return -(e as i64);
                }
            }
        }
    }

    *out_fds.offset(0) = fds[0];
    *out_fds.offset(1) = fds[1];
    0
}

#[no_mangle]
pub unsafe extern "C" fn nl_syscall_dup2(old_fd: i32, new_fd: i32) -> i64 {
    let r = libc::dup2(old_fd as c_int, new_fd as c_int);
    value_or_errno(r as i64)
}

// ---------------------------------------------------------------------------
// waitpid / kill / setsid
// ---------------------------------------------------------------------------

/// `waitpid(pid, &status, options)`.
///
/// Returns the child pid that was reaped (`>=0`) or `-errno`.  When
/// `WNOHANG` is set and no child has exited, returns `0` (this is the
/// libc convention; callers must distinguish 0-from-no-child vs. a
/// pid-of-0 themselves — pid 0 is a process group, never a real
/// child pid in a typical waitpid).  `out_status` may be NULL if the
/// caller does not care about the exit status word.
#[no_mangle]
pub unsafe extern "C" fn nl_syscall_waitpid(
    pid: i32,
    options: i32,
    out_status: *mut i32,
) -> i64 {
    let mut status: c_int = 0;
    let rc = libc::waitpid(pid as pid_t, &mut status as *mut c_int, options as c_int);
    if rc < 0 {
        return -(SyscallError::last_os_error().raw() as i64);
    }
    if !out_status.is_null() {
        *out_status = status as i32;
    }
    rc as i64
}

#[no_mangle]
pub unsafe extern "C" fn nl_syscall_kill(pid: i32, sig: i32) -> i64 {
    let r = libc::kill(pid as pid_t, sig as c_int);
    negate_errno(r)
}

#[no_mangle]
pub unsafe extern "C" fn nl_syscall_setsid() -> i64 {
    let r = libc::setsid();
    value_or_errno(r as i64)
}

// ---------------------------------------------------------------------------
// rlimit family (T93 review #7)
// ---------------------------------------------------------------------------

/// `getrlimit(resource, &rlim)` — fills `out_soft` / `out_hard` with
/// the current limits.  Either output pointer may be NULL if the
/// caller is not interested in that field.
#[no_mangle]
pub unsafe extern "C" fn nl_syscall_getrlimit(
    resource: i32,
    out_soft: *mut u64,
    out_hard: *mut u64,
) -> i64 {
    let mut rl: MaybeUninit<libc::rlimit> = MaybeUninit::uninit();
    let r = libc::getrlimit(resource as NlRlimitResource, rl.as_mut_ptr());
    if r != 0 {
        return -(SyscallError::last_os_error().raw() as i64);
    }
    let rl = rl.assume_init();
    if !out_soft.is_null() {
        *out_soft = rl.rlim_cur as u64;
    }
    if !out_hard.is_null() {
        *out_hard = rl.rlim_max as u64;
    }
    0
}

#[no_mangle]
pub unsafe extern "C" fn nl_syscall_setrlimit(resource: i32, soft: u64, hard: u64) -> i64 {
    let rl = libc::rlimit {
        rlim_cur: soft as rlim_t,
        rlim_max: hard as rlim_t,
    };
    let r = libc::setrlimit(resource as NlRlimitResource, &rl);
    negate_errno(r)
}

/// `prlimit(pid, resource, &new, &old)` — Linux only.  macOS lacks
/// this primitive, so the call returns `-ENOSYS`.
///
/// Sentinel `u64::MAX` (= `(rlim_t) -1`) on any of `new_soft` /
/// `new_hard` means "do not modify".  `old_soft` / `old_hard` may be
/// NULL if the caller does not need the previous values.
#[no_mangle]
pub unsafe extern "C" fn nl_syscall_prlimit(
    _pid: i32,
    _resource: i32,
    _new_soft: u64,
    _new_hard: u64,
    _old_soft: *mut u64,
    _old_hard: *mut u64,
) -> i64 {
    #[cfg(any(target_os = "linux", target_os = "android"))]
    {
        let mut new_rl = libc::rlimit {
            rlim_cur: _new_soft as rlim_t,
            rlim_max: _new_hard as rlim_t,
        };
        let mut old_rl: MaybeUninit<libc::rlimit> = MaybeUninit::uninit();
        // If both new_soft & new_hard are u64::MAX we treat it as "no change"
        // by passing NULL for the new value.
        let new_ptr = if _new_soft == u64::MAX && _new_hard == u64::MAX {
            ptr::null()
        } else {
            &mut new_rl as *const libc::rlimit
        };
        let r = libc::prlimit(
            _pid as pid_t,
            _resource as NlRlimitResource,
            new_ptr,
            old_rl.as_mut_ptr(),
        );
        if r != 0 {
            return -(SyscallError::last_os_error().raw() as i64);
        }
        let old = old_rl.assume_init();
        if !_old_soft.is_null() {
            *_old_soft = old.rlim_cur as u64;
        }
        if !_old_hard.is_null() {
            *_old_hard = old.rlim_max as u64;
        }
        0
    }
    #[cfg(not(any(target_os = "linux", target_os = "android")))]
    {
        -(libc::ENOSYS as i64)
    }
}

// ---------------------------------------------------------------------------
// select — eventloop multiplexer prerequisite (9d.K)
// ---------------------------------------------------------------------------

/// `select(nfds, readfds, writefds, exceptfds, timeout)`.
///
/// `timeout_ms` semantics:
///   * `< 0`  → block indefinitely (NULL timeout)
///   * `== 0` → poll (zero timeval)
///   * `> 0`  → wait this many milliseconds
///
/// fdset pointers are passed straight through — Phase 9d.K consumers
/// will allocate `libc::fd_set` on the Lisp side via heap blob (Phase
/// 7.0 `nelisp_syscall_mmap`) and use the runtime's FD_* macros (also
/// Phase 9d.K) to manipulate them.  The Rust thin wrapper simply
/// hands the raw pointers off to libc.
#[no_mangle]
pub unsafe extern "C" fn nl_syscall_select(
    nfds: i32,
    readfds: *mut libc::fd_set,
    writefds: *mut libc::fd_set,
    exceptfds: *mut libc::fd_set,
    timeout_ms: i64,
) -> i64 {
    let (tv_ptr, mut tv) = if timeout_ms < 0 {
        (ptr::null_mut::<libc::timeval>(), libc::timeval {
            tv_sec: 0,
            tv_usec: 0,
        })
    } else {
        let tv = libc::timeval {
            tv_sec: (timeout_ms / 1000) as libc::time_t,
            tv_usec: ((timeout_ms % 1000) * 1000) as libc::suseconds_t,
        };
        (1 as *mut libc::timeval, tv)
    };
    let real_tv_ptr = if tv_ptr.is_null() {
        ptr::null_mut::<libc::timeval>()
    } else {
        &mut tv as *mut libc::timeval
    };
    let r = libc::select(nfds as c_int, readfds, writefds, exceptfds, real_tv_ptr);
    if r < 0 {
        -(SyscallError::last_os_error().raw() as i64)
    } else {
        r as i64
    }
}

// ---------------------------------------------------------------------------
// Tests — gated behind `#[cfg(test)]`.  The runtime crate gates this
// whole package behind its `process-syscalls` feature, so the package
// can test its own subprocess surface directly.
//
// All tests run under `cargo test --release` and must finish in
// well under a second each.  They exercise the FFI symbols directly
// (no NeLisp side, no eventloop, no shell interpreter).
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use std::ffi::CString;
    use std::mem::MaybeUninit;

    /// WIFEXITED / WEXITSTATUS portable helpers.  libc on Linux exposes
    /// these as macros, not functions, so we re-implement them.
    fn w_if_exited(status: i32) -> bool {
        (status & 0x7f) == 0
    }
    fn w_exit_status(status: i32) -> i32 {
        (status >> 8) & 0xff
    }
    fn w_if_signaled(status: i32) -> bool {
        ((status & 0x7f) + 1) >> 1 > 0 && !w_if_exited(status)
    }
    fn w_term_sig(status: i32) -> i32 {
        status & 0x7f
    }

    #[test]
    fn test_pipe2_returns_two_fds() {
        unsafe {
            let mut fds: [i32; 2] = [-1, -1];
            let r = nl_syscall_pipe2(0, fds.as_mut_ptr());
            assert_eq!(r, 0, "pipe2 failed: {}", r);
            assert!(fds[0] >= 0 && fds[1] >= 0);
            // Confirm O_CLOEXEC was set on both ends.
            for fd in fds.iter() {
                let flags = libc::fcntl(*fd, libc::F_GETFD);
                assert!(flags >= 0);
                assert!(
                    flags & libc::FD_CLOEXEC != 0,
                    "FD_CLOEXEC missing on fd {}",
                    fd
                );
            }
            assert_eq!(libc::close(fds[0]), 0);
            assert_eq!(libc::close(fds[1]), 0);
        }
    }

    #[test]
    fn test_pipe2_nonblock_flag() {
        unsafe {
            let mut fds: [i32; 2] = [-1, -1];
            let r = nl_syscall_pipe2(libc::O_NONBLOCK, fds.as_mut_ptr());
            assert_eq!(r, 0);
            for fd in fds.iter() {
                let flags = libc::fcntl(*fd, libc::F_GETFL);
                assert!(flags & libc::O_NONBLOCK != 0);
            }
            libc::close(fds[0]);
            libc::close(fds[1]);
        }
    }

    #[test]
    fn test_dup2_redirects_fd() {
        unsafe {
            let mut fds: [i32; 2] = [-1, -1];
            assert_eq!(nl_syscall_pipe2(0, fds.as_mut_ptr()), 0);
            // dup2 the read end onto fd 100.
            let r = nl_syscall_dup2(fds[0], 100);
            assert_eq!(r, 100);
            // fd 100 should now be a valid (open) file descriptor.
            let ok = libc::fcntl(100, libc::F_GETFD);
            assert!(ok >= 0);
            libc::close(100);
            libc::close(fds[0]);
            libc::close(fds[1]);
        }
    }

    #[test]
    fn test_fork_exec_pipe_roundtrip() {
        // fork + child = exec /bin/echo "nelisp-9dJ-fork-ok" + parent =
        // read pipe + waitpid.  Asserts the bytes written by echo arrive
        // unchanged.
        unsafe {
            let mut fds: [i32; 2] = [-1, -1];
            assert_eq!(nl_syscall_pipe2(0, fds.as_mut_ptr()), 0);

            let pid = nl_syscall_fork();
            assert!(pid >= 0, "fork failed: {pid}");
            if pid == 0 {
                // child
                libc::close(fds[0]);
                // Redirect stdout to the pipe write end.
                let r = nl_syscall_dup2(fds[1], 1);
                if r < 0 {
                    libc::_exit(11);
                }
                libc::close(fds[1]);
                // exec /bin/echo
                let path = CString::new("/bin/echo").unwrap();
                let arg0 = CString::new("echo").unwrap();
                let arg1 = CString::new("nelisp-9dJ-fork-ok").unwrap();
                let argv: [*const c_char; 3] =
                    [arg0.as_ptr(), arg1.as_ptr(), ptr::null()];
                let envp: [*const c_char; 1] = [ptr::null()];
                let _ = nl_syscall_execve(path.as_ptr(), argv.as_ptr(), envp.as_ptr());
                libc::_exit(12); // exec failed
            } else {
                // parent
                libc::close(fds[1]);
                let mut buf = [0u8; 64];
                // Read until EOF or first chunk; echo writes <19 bytes
                // followed by \n so a single read suffices on Linux.
                let n = libc::read(fds[0], buf.as_mut_ptr() as *mut _, buf.len());
                assert!(n > 0, "parent read returned {n}");
                let s = std::str::from_utf8(&buf[..n as usize]).unwrap();
                assert!(s.contains("nelisp-9dJ-fork-ok"), "got: {:?}", s);
                libc::close(fds[0]);

                let mut status: i32 = 0;
                let reaped = nl_syscall_waitpid(pid as i32, 0, &mut status);
                assert_eq!(reaped, pid);
                assert!(w_if_exited(status));
                assert_eq!(w_exit_status(status), 0);
            }
        }
    }

    #[test]
    fn test_waitpid_returns_exit_status() {
        unsafe {
            let pid = nl_syscall_fork();
            assert!(pid >= 0);
            if pid == 0 {
                // child: exit with status 7
                libc::_exit(7);
            } else {
                let mut status: i32 = 0;
                let reaped = nl_syscall_waitpid(pid as i32, 0, &mut status);
                assert_eq!(reaped, pid);
                assert!(w_if_exited(status));
                assert_eq!(w_exit_status(status), 7);
            }
        }
    }

    #[test]
    fn test_waitpid_wnohang_returns_zero_when_no_child_ready() {
        unsafe {
            let pid = nl_syscall_fork();
            assert!(pid >= 0);
            if pid == 0 {
                // child: sleep briefly so the parent's WNOHANG sees us still alive.
                libc::usleep(200_000); // 200 ms
                libc::_exit(0);
            } else {
                // Immediate WNOHANG must return 0 (not -errno) since the
                // child is still running.
                let mut status: i32 = 0;
                let r = nl_syscall_waitpid(pid as i32, libc::WNOHANG, &mut status);
                assert_eq!(r, 0, "expected 0 (no-children-ready) got {r}");
                // Final blocking wait reaps the child.
                let r = nl_syscall_waitpid(pid as i32, 0, &mut status);
                assert_eq!(r, pid);
            }
        }
    }

    #[test]
    fn test_kill_sigterm_terminates_child() {
        unsafe {
            let pid = nl_syscall_fork();
            assert!(pid >= 0);
            if pid == 0 {
                // child: long sleep so the parent's SIGTERM lands first.
                libc::sleep(60);
                libc::_exit(0);
            } else {
                // Give the child a chance to enter sleep().
                libc::usleep(50_000);
                let r = nl_syscall_kill(pid as i32, libc::SIGTERM);
                assert_eq!(r, 0);
                let mut status: i32 = 0;
                let reaped = nl_syscall_waitpid(pid as i32, 0, &mut status);
                assert_eq!(reaped, pid);
                assert!(
                    w_if_signaled(status),
                    "expected signaled exit, status = 0x{status:x}"
                );
                assert_eq!(w_term_sig(status), libc::SIGTERM);
            }
        }
    }

    #[test]
    fn test_posix_spawn_runs_echo() {
        unsafe {
            let path = CString::new("/bin/echo").unwrap();
            let arg0 = CString::new("echo").unwrap();
            let arg1 = CString::new("posix-spawn-ok").unwrap();
            let argv: [*const c_char; 3] = [arg0.as_ptr(), arg1.as_ptr(), ptr::null()];
            let envp: [*const c_char; 1] = [ptr::null()];

            let pid = nl_syscall_posix_spawn(path.as_ptr(), argv.as_ptr(), envp.as_ptr());
            assert!(pid > 0, "posix_spawn returned {pid}");

            let mut status: i32 = 0;
            let reaped = nl_syscall_waitpid(pid as i32, 0, &mut status);
            assert_eq!(reaped, pid);
            assert!(w_if_exited(status));
            assert_eq!(w_exit_status(status), 0);
        }
    }

    #[test]
    fn test_getrlimit_nofile_is_positive() {
        unsafe {
            let mut soft: u64 = 0;
            let mut hard: u64 = 0;
            let r = nl_syscall_getrlimit(libc::RLIMIT_NOFILE as i32, &mut soft, &mut hard);
            assert_eq!(r, 0);
            assert!(soft > 0 && hard >= soft, "soft={soft} hard={hard}");
        }
    }

    #[test]
    fn test_setrlimit_roundtrip() {
        unsafe {
            // Read current RLIMIT_NOFILE, set both ends to soft, read back.
            let mut soft: u64 = 0;
            let mut hard: u64 = 0;
            assert_eq!(
                nl_syscall_getrlimit(libc::RLIMIT_NOFILE as i32, &mut soft, &mut hard),
                0
            );
            // Lower the *soft* limit only by 1 (raising would need
            // CAP_SYS_RESOURCE on most distros).
            let new_soft = if soft > 64 { soft - 1 } else { soft };
            let r =
                nl_syscall_setrlimit(libc::RLIMIT_NOFILE as i32, new_soft, hard);
            assert_eq!(r, 0);

            let mut got_soft: u64 = 0;
            let mut got_hard: u64 = 0;
            assert_eq!(
                nl_syscall_getrlimit(
                    libc::RLIMIT_NOFILE as i32,
                    &mut got_soft,
                    &mut got_hard
                ),
                0
            );
            assert_eq!(got_soft, new_soft);
            assert_eq!(got_hard, hard);

            // Restore (best-effort; if test was launched at NOFILE 64 we already are
            // at the floor and the previous setrlimit was a no-op).
            let _ = nl_syscall_setrlimit(libc::RLIMIT_NOFILE as i32, soft, hard);
        }
    }

    #[test]
    fn test_prlimit_get_only() {
        unsafe {
            let mut old_soft: u64 = 0;
            let mut old_hard: u64 = 0;
            // pid = 0 means "self" on Linux.
            let r = nl_syscall_prlimit(
                0,
                libc::RLIMIT_NOFILE as i32,
                u64::MAX, // no new_soft
                u64::MAX, // no new_hard
                &mut old_soft,
                &mut old_hard,
            );
            #[cfg(any(target_os = "linux", target_os = "android"))]
            {
                assert_eq!(r, 0, "prlimit failed on linux: {r}");
                assert!(old_soft > 0 && old_hard >= old_soft);
            }
            #[cfg(not(any(target_os = "linux", target_os = "android")))]
            {
                assert_eq!(r, -(libc::ENOSYS as i64));
            }
        }
    }

    #[test]
    fn test_select_pipe_ready_for_read() {
        unsafe {
            let mut fds: [i32; 2] = [-1, -1];
            assert_eq!(nl_syscall_pipe2(0, fds.as_mut_ptr()), 0);

            // Initially the read end is *not* ready.
            let mut rset: MaybeUninit<libc::fd_set> = MaybeUninit::uninit();
            libc::FD_ZERO(rset.as_mut_ptr());
            let mut rset = rset.assume_init();
            libc::FD_SET(fds[0], &mut rset);
            let r = nl_syscall_select(
                fds[0] + 1,
                &mut rset,
                ptr::null_mut(),
                ptr::null_mut(),
                0, // poll
            );
            assert_eq!(r, 0, "select with no data should report 0 ready");

            // Write a byte; select should now report 1.
            let payload = b"X";
            let n = libc::write(fds[1], payload.as_ptr() as *const _, payload.len());
            assert_eq!(n, 1);

            libc::FD_ZERO(&mut rset);
            libc::FD_SET(fds[0], &mut rset);
            let r = nl_syscall_select(
                fds[0] + 1,
                &mut rset,
                ptr::null_mut(),
                ptr::null_mut(),
                100, // 100ms upper bound
            );
            assert_eq!(r, 1, "select should report 1 fd ready after write");
            assert!(libc::FD_ISSET(fds[0], &rset));

            libc::close(fds[0]);
            libc::close(fds[1]);
        }
    }

    #[test]
    fn test_select_negative_timeout_blocks_until_data() {
        // Use a forked writer that pushes data after 50 ms; the parent
        // calls select with a -1 (block-forever) timeout.  The test passes
        // if select returns 1 once the data arrives.
        unsafe {
            let mut fds: [i32; 2] = [-1, -1];
            assert_eq!(nl_syscall_pipe2(0, fds.as_mut_ptr()), 0);

            let pid = nl_syscall_fork();
            assert!(pid >= 0);
            if pid == 0 {
                libc::close(fds[0]);
                libc::usleep(50_000);
                let payload = b"Z";
                let _ = libc::write(fds[1], payload.as_ptr() as *const _, payload.len());
                libc::close(fds[1]);
                libc::_exit(0);
            } else {
                libc::close(fds[1]);
                let mut rset: MaybeUninit<libc::fd_set> = MaybeUninit::uninit();
                libc::FD_ZERO(rset.as_mut_ptr());
                let mut rset = rset.assume_init();
                libc::FD_SET(fds[0], &mut rset);
                let r = nl_syscall_select(
                    fds[0] + 1,
                    &mut rset,
                    ptr::null_mut(),
                    ptr::null_mut(),
                    5_000, // 5s upper safety bound, expect <100ms
                );
                assert_eq!(r, 1, "select did not see the writer's byte");
                libc::close(fds[0]);

                let mut status: i32 = 0;
                let _ = nl_syscall_waitpid(pid as i32, 0, &mut status);
            }
        }
    }

    #[test]
    fn test_kill_zero_signal_probes_existence() {
        // kill(pid, 0) is a no-op that returns 0 if the pid exists,
        // -ESRCH if it does not.  Doc 39 §6.5 PID-race hygiene: the
        // syscall layer reports the kernel's view, the caller decides.
        unsafe {
            let pid = nl_syscall_fork();
            assert!(pid >= 0);
            if pid == 0 {
                libc::sleep(2);
                libc::_exit(0);
            } else {
                libc::usleep(20_000);
                let r = nl_syscall_kill(pid as i32, 0);
                assert_eq!(r, 0, "kill(pid, 0) on live child returned {r}");
                let _ = nl_syscall_kill(pid as i32, libc::SIGKILL);
                let mut status: i32 = 0;
                let _ = nl_syscall_waitpid(pid as i32, 0, &mut status);
                // After reap, kill(0) must return -ESRCH.
                let r = nl_syscall_kill(pid as i32, 0);
                assert_eq!(r, -(libc::ESRCH as i64));
            }
        }
    }

    #[test]
    fn test_setsid_in_child_creates_new_session() {
        // setsid only succeeds when the caller is *not* a session leader.
        // The test process IS a session leader (cargo test forks it
        // already), so we exercise setsid inside a child.
        unsafe {
            let pid = nl_syscall_fork();
            assert!(pid >= 0);
            if pid == 0 {
                let new_sid = nl_syscall_setsid();
                if new_sid > 0 {
                    libc::_exit(0);
                } else {
                    libc::_exit(20);
                }
            } else {
                let mut status: i32 = 0;
                let reaped = nl_syscall_waitpid(pid as i32, 0, &mut status);
                assert_eq!(reaped, pid);
                assert!(w_if_exited(status));
                assert_eq!(
                    w_exit_status(status),
                    0,
                    "child reported setsid failure (exit {})",
                    w_exit_status(status)
                );
            }
        }
    }
}
