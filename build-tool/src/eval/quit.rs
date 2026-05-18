//! Process-global quit-flag plumbing.  Mirrors GNU Emacs's
//! `Vquit_flag`: code that wants to interrupt the evaluator (C-g,
//! SIGINT, …) flips the flag with [`set_quit_flag`]; the evaluator
//! polls at every [`super::eval`] entry and converts a pending flag
//! into [`EvalError::Quit`].  Storage is `AtomicI64` (0 = clear, 1 =
//! pending) so the elisp helper can reach the same memory through
//! the `atomic-fetch-add` / `atomic-compare-exchange` /
//! `ptr-read-u64` grammar ops.  `AtomicI64::store` is async-signal-
//! safe, so a real `SIGINT` handler can drop the flag from the
//! signal context.

use std::sync::atomic::{AtomicBool, AtomicI64, Ordering};

/// Process-global quit flag.  Mutators (set/clear/pending-p) live in
/// elisp Phase 47 (`nelisp-cc-bi-quit-flag.el', atomic-compare-exchange
/// + ptr-read-u64).  Rust retains the static storage, the pointer
/// getter the elisp side dereferences, the eval-poll consumer, and
/// the SIGINT handler that flips the slot from signal context.
static QUIT_FLAG: AtomicI64 = AtomicI64::new(0);

#[no_mangle]
pub extern "C" fn nl_quit_flag_ptr() -> *mut i64 {
    QUIT_FLAG.as_ptr()
}

/// Atomically take the flag.  Returns `true` if it was set, and
/// resets it to `false` in the same step.  This is the primitive
/// the evaluator uses at every poll point.
pub fn take_quit_flag() -> bool {
    QUIT_FLAG.swap(0, Ordering::SeqCst) != 0
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
            // `AtomicI64::store' is async-signal-safe.  Setting the
            // flag is the entirety of the handler's job — the next
            // eval-time poll converts it into `EvalError::Quit`.
            QUIT_FLAG.store(1, Ordering::SeqCst);
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
    // No-op: Windows uses SetConsoleCtrlHandler — not yet wired.
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
