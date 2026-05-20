//! Process-global quit-flag plumbing (mirrors GNU Emacs `Vquit_flag`).
//! Storage is `AtomicI64` (0=clear, 1=pending); elisp mutators live in
//! `nelisp-cc-bi-quit-flag.el` (atomic ops).  `AtomicI64::store` is
//! async-signal-safe so a real SIGINT handler can write from signal context.

use std::sync::atomic::{AtomicBool, AtomicI64, Ordering};

/// Process-global quit flag (0=clear, 1=pending).
static QUIT_FLAG: AtomicI64 = AtomicI64::new(0);

#[no_mangle]
pub extern "C" fn nl_quit_flag_ptr() -> *mut i64 {
    QUIT_FLAG.as_ptr()
}

/// Atomically take-and-clear the flag. Returns `true` if it was set.
pub fn take_quit_flag() -> bool {
    QUIT_FLAG.swap(0, Ordering::SeqCst) != 0
}

/// Install a SIGINT handler that sets the quit flag (idempotent via Once).
/// Unix-only; non-Unix is a no-op so callers need no cfg-gate.
#[cfg(unix)]
pub fn install_sigint_handler() {
    use std::sync::Once;
    static ONCE: Once = Once::new();
    ONCE.call_once(|| {
        extern "C" fn handler(_signum: libc::c_int) {
            QUIT_FLAG.store(1, Ordering::SeqCst);
        }
        unsafe {
            let mut sa: libc::sigaction = std::mem::zeroed();
            sa.sa_sigaction = handler as *const () as usize;
            libc::sigemptyset(&mut sa.sa_mask);
            sa.sa_flags = libc::SA_RESTART; // restart interrupted read/poll
            libc::sigaction(libc::SIGINT, &sa, std::ptr::null_mut());
        }
        SIGINT_INSTALLED.store(true, Ordering::SeqCst);
    });
}

#[cfg(not(unix))]
pub fn install_sigint_handler() {} // no-op: Windows not yet wired

/// Returns `true` if [`install_sigint_handler`] has run. Test helper.
#[cfg(unix)]
pub fn sigint_handler_installed_p() -> bool { SIGINT_INSTALLED.load(Ordering::SeqCst) }
#[cfg(not(unix))]
pub fn sigint_handler_installed_p() -> bool { false }

#[cfg(unix)]
static SIGINT_INSTALLED: AtomicBool = AtomicBool::new(false);
