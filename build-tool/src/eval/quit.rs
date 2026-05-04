//! Doc 51 Track M (2026-05-04) — process-global quit-flag plumbing.
//!
//! Mirrors GNU Emacs's `Vquit_flag`: any code path that wants to
//! interrupt the evaluator (= C-g from the keyboard, SIGINT to a
//! batch process, an external trigger from a sibling thread, …)
//! flips the flag with [`set_quit_flag`].  The evaluator polls the
//! flag at every call to [`super::eval`] and converts a pending
//! flag into [`EvalError::Quit`], which then unwinds through
//! `condition-case`'s `quit` clause / `unwind-protect` / etc.
//!
//! The flag is `AtomicBool` because the canonical path is "set from
//! a signal handler".  `AtomicBool::store(true, …)` is async-signal-
//! safe on every platform Rust supports, so a SIGINT handler can
//! drop the flag without violating POSIX.

use std::sync::atomic::{AtomicBool, Ordering};

static QUIT_FLAG: AtomicBool = AtomicBool::new(false);

/// Set the global quit flag.  Async-signal-safe: a real `SIGINT`
/// handler may call this directly.  Idempotent — repeated calls
/// without an intervening clear simply re-mark the flag.
pub fn set_quit_flag() {
    QUIT_FLAG.store(true, Ordering::SeqCst);
}

/// Clear the global quit flag.  Used by [`take_quit_flag`] and by
/// `condition-case`'s `quit` clause once a quit has been caught.
pub fn clear_quit_flag() {
    QUIT_FLAG.store(false, Ordering::SeqCst);
}

/// Read the flag without changing it (= `quit-flag-pending-p`).
pub fn is_quit_pending() -> bool {
    QUIT_FLAG.load(Ordering::SeqCst)
}

/// Atomically take the flag.  Returns `true` if it was set, and
/// resets it to `false` in the same step.  This is the primitive
/// the evaluator uses at every poll point.
pub fn take_quit_flag() -> bool {
    QUIT_FLAG.swap(false, Ordering::SeqCst)
}

/// Install a SIGINT handler that flips [`set_quit_flag`] instead of
/// the system default (= terminate).  The handler does NOT re-raise:
/// the canonical Emacs flow is "Ctrl+C = `quit`, eval loop unwinds,
/// command loop continues".  Idempotent; a Once gate makes repeated
/// calls a no-op.  Unix-only — on non-Unix this is a no-op that
/// returns success so callers don't need to cfg-gate.
#[cfg(unix)]
pub fn install_sigint_handler() {
    use std::sync::Once;
    static ONCE: Once = Once::new();
    ONCE.call_once(|| {
        extern "C" fn handler(_signum: libc::c_int) {
            // `AtomicBool::store` is async-signal-safe on every
            // platform Rust supports.  Setting the flag is the
            // entirety of the handler's job — the next eval-time
            // poll converts it into `EvalError::Quit`.
            QUIT_FLAG.store(true, Ordering::SeqCst);
        }
        unsafe {
            let mut sa: libc::sigaction = std::mem::zeroed();
            sa.sa_sigaction = handler as *const () as usize;
            libc::sigemptyset(&mut sa.sa_mask);
            // SA_RESTART so an in-flight `read`/`poll` on stdin
            // restarts after the handler runs (= the next eval
            // boundary picks up the flag instead of seeing EINTR).
            sa.sa_flags = libc::SA_RESTART;
            libc::sigaction(libc::SIGINT, &sa, std::ptr::null_mut());
        }
        SIGINT_INSTALLED.store(true, Ordering::SeqCst);
    });
}

#[cfg(not(unix))]
pub fn install_sigint_handler() {
    // No-op: Windows uses SetConsoleCtrlHandler which we have not
    // wired yet (Doc 51 follow-up).
}

/// Has [`install_sigint_handler`] run yet?  Test/diagnostic helper.
#[cfg(unix)]
pub fn sigint_handler_installed_p() -> bool {
    SIGINT_INSTALLED.load(Ordering::SeqCst)
}

#[cfg(not(unix))]
pub fn sigint_handler_installed_p() -> bool {
    false
}

#[cfg(unix)]
static SIGINT_INSTALLED: AtomicBool = AtomicBool::new(false);
