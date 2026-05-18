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

use std::sync::atomic::{AtomicI64, Ordering};

/// Process-global quit flag — 0 = clear, 1 = pending.  Stored as
/// `AtomicI64' so the elisp helper can mutate the same memory
/// through atomic / raw-mem grammar ops (= those operate on `*mut
/// i64').  Rust mutators normalize to 0/1; elisp side does the same
/// via CAS.
static QUIT_FLAG: AtomicI64 = AtomicI64::new(0);

/// Raw pointer to the static `QUIT_FLAG' storage, for the elisp body
/// that reads / mutates the slot via `extern-call' + atomic ops.
///
/// SAFETY: returns a pointer aliasing a `'static` `AtomicI64`.
/// x86_64 aligned 64-bit loads are atomic at the hardware level so
/// `ptr-read-u64` is never torn; writers go through CAS (LOCK CMPXCHG).
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

/// Async-signal-safe SIGINT handler — hoisted to module scope (per
/// Doc 117 §117.D.gaps.4) so the elisp dispatcher in
/// `lisp/nelisp-cc-bi-install-sigint-handler.el' can take its address
/// via [`nl_sigint_handler_addr`] and stash it into the `sa_sigaction'
/// slot of a §122.J `struct-make' sigaction buffer.
///
/// Signal handlers themselves can never be elisp (= elisp emit needs
/// the runtime allocator, which is async-signal-unsafe); only the
/// sigaction-struct construction + libc dispatch moves to elisp.
///
/// `AtomicI64::store' is async-signal-safe.  Setting the flag is the
/// entirety of the handler's job — the next eval-time poll converts
/// it into `EvalError::Quit`.
#[cfg(unix)]
extern "C" fn nl_sigint_handler_impl(_signum: libc::c_int) {
    QUIT_FLAG.store(1, Ordering::SeqCst);
}

/// Returns the address of [`nl_sigint_handler_impl`] cast to `i64' so
/// the elisp body can write it into the `sa_sigaction' slot via
/// §122.J `struct-field-set' / `ptr-write-u64'.
///
/// SAFETY: the returned address aliases a `'static` `extern "C" fn'
/// in the crate's `.text' segment — never freed, always callable.
#[cfg(unix)]
#[no_mangle]
pub extern "C" fn nl_sigint_handler_addr() -> i64 {
    nl_sigint_handler_impl as *const () as i64
}

/// Process-global SIGINT-installed flag — 0 = not installed, 1 =
/// installed.  Stored as `AtomicI64' (same layout as `QUIT_FLAG') so
/// the elisp installer can mark it via `ptr-write-u64' and the
/// diagnostic accessor can read it via `ptr-read-u64'.
#[cfg(unix)]
static SIGINT_INSTALLED: AtomicI64 = AtomicI64::new(0);

/// Surface the `SIGINT_INSTALLED' static's address to the elisp
/// installer (= so it can flip 0 -> 1 once `sigaction(2)' returns).
///
/// SAFETY: aliases a `'static` `AtomicI64' — same contract as
/// [`nl_quit_flag_ptr`].
#[cfg(unix)]
#[no_mangle]
pub extern "C" fn nl_sigint_installed_ptr() -> *mut i64 {
    SIGINT_INSTALLED.as_ptr()
}

/// Install a SIGINT handler that flips [`set_quit_flag`] instead of
/// the system default (= terminate).  The handler does NOT re-raise:
/// the canonical Emacs flow is "Ctrl+C = `quit`, eval loop unwinds,
/// command loop continues".  Idempotent; the body short-circuits when
/// `SIGINT_INSTALLED' is already 1.  Unix-only — on non-Unix this is
/// a no-op that returns success so callers don't need to cfg-gate.
///
/// Doc 117 §117.D.gaps.4: the sigaction-struct construction + libc
/// `sigaction(2)' dispatch now lives in the Phase 47 elisp object
/// compiled from `lisp/nelisp-cc-bi-install-sigint-handler.el'.  The
/// Rust signal-handler fn pointer ([`nl_sigint_handler_impl`]) stays
/// here — async-signal-safety forbids elisp inside the handler body.
#[cfg(unix)]
pub fn install_sigint_handler() {
    if SIGINT_INSTALLED.load(Ordering::SeqCst) != 0 {
        return;
    }
    // SAFETY: the elisp object takes no Rust pointers — it builds the
    // sigaction buffer itself via §122.J `struct-make' + populates the
    // handler-fn slot via the `nl_sigint_handler_addr' extern.  The
    // returned i64 is the libc `sigaction(2)' rc which we discard
    // (matching the pre-swap body which also ignored the rc).
    let _rc = unsafe { crate::elisp_cc_spike::bi_install_sigint_handler() };
}

#[cfg(not(unix))]
pub fn install_sigint_handler() {
    // No-op: Windows uses SetConsoleCtrlHandler — not yet wired.
}

/// Has [`install_sigint_handler`] run yet?  Test/diagnostic helper.
#[cfg(unix)]
pub fn sigint_handler_installed_p() -> bool {
    SIGINT_INSTALLED.load(Ordering::SeqCst) != 0
}

#[cfg(not(unix))]
pub fn sigint_handler_installed_p() -> bool {
    false
}
