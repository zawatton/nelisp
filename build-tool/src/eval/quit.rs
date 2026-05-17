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
//! Storage is `AtomicI64` (= 8-byte aligned u64 cell, value 0 = clear,
//! value 1 = pending) so the Doc 117 §117.B elisp-cc swap can reach
//! the same memory through the §122.E `(atomic-fetch-add PTR DELTA)`
//! / `(atomic-compare-exchange PTR EXP NEW)` / `(ptr-read-u64 PTR 0)`
//! grammar ops (= each one lowers to a SeqCst x86_64 load/CAS/fetch-
//! add inline).  `AtomicI64::store(0/1, SeqCst)` remains async-signal-
//! safe on every platform Rust supports (= the same guarantee
//! `AtomicBool::store' carried), so a real `SIGINT` handler can still
//! drop the flag from the signal context.

use std::sync::atomic::{AtomicBool, AtomicI64, Ordering};

/// Process-global quit flag — 0 means clear, 1 means pending.  Stored
/// as `AtomicI64' (vs the previous `AtomicBool') so the Doc 117 §117.B
/// elisp helper in `lisp/nelisp-cc-bi-quit-flag.el` can read / mutate
/// the same memory through the §122.E atomic / raw-mem grammar ops
/// (= those operate on `*mut i64' for atomicity guarantees).  All Rust
/// mutators (= `set_quit_flag' / `clear_quit_flag') normalize to the
/// canonical 0/1 values; the elisp side does the same via CAS.
static QUIT_FLAG: AtomicI64 = AtomicI64::new(0);

/// Raw pointer to the static `QUIT_FLAG' storage, intended for the
/// Phase 47 elisp body in `lisp/nelisp-cc-bi-quit-flag.el' (Doc 117
/// §117.B).  The elisp body calls this via `extern-call' to obtain
/// the slot address, then operates on it via §122.E `atomic-
/// compare-exchange' (= set / clear) / `ptr-read-u64' (= pending-p).
///
/// SAFETY: the returned pointer aliases a `'static` `AtomicI64`.
/// Reads via `ptr-read-u64` are racy with respect to concurrent
/// writers in the strict C++ memory-model sense, but x86_64 aligned
/// 64-bit loads are atomic at the hardware level (= the `MOV' is
/// indivisible), so the value observed is always one of the prior
/// `store' results — never a torn read.  Writers go through the
/// `atomic-compare-exchange' op which lowers to a `LOCK CMPXCHG'.
#[no_mangle]
pub extern "C" fn nl_quit_flag_ptr() -> *mut i64 {
    QUIT_FLAG.as_ptr()
}

/// Set the global quit flag.  Async-signal-safe: a real `SIGINT`
/// handler may call this directly.  Idempotent — repeated calls
/// without an intervening clear simply re-mark the flag.
pub fn set_quit_flag() {
    QUIT_FLAG.store(1, Ordering::SeqCst);
}

/// Clear the global quit flag.  Used by [`take_quit_flag`] and by
/// `condition-case`'s `quit` clause once a quit has been caught.
pub fn clear_quit_flag() {
    QUIT_FLAG.store(0, Ordering::SeqCst);
}

/// Read the flag without changing it (= `quit-flag-pending-p`).
pub fn is_quit_pending() -> bool {
    QUIT_FLAG.load(Ordering::SeqCst) != 0
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
            // `AtomicI64::store' is async-signal-safe on every
            // platform Rust supports (= same guarantee the previous
            // `AtomicBool::store' carried).  Setting the flag is the
            // entirety of the handler's job — the next eval-time
            // poll converts it into `EvalError::Quit`.
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
