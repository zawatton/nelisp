//! Doc 47 Stage 4c — async-signal-safe crash handler skeleton.
//!
//! The seed installs handlers for synchronous fatal signals before it
//! jumps into image-provided machine code.  If that code faults, the
//! handler writes one fixed diagnostic to stderr and terminates via
//! `_exit(130)` so the process does not die with an opaque default
//! crash action.

use std::io;
use std::path::PathBuf;

use libc::{self, c_int};

use super::error::ImageError;

pub const NL_IMAGE_FAULT_EXIT_CODE: i32 = 130;

const SIGNAL_HANDLER_PATH: &str = "<signal-handlers>";
const MSG_PREFIX: &[u8] = b"nelisp-runtime: caught signal ";
const MSG_SUFFIX: &[u8] = b", aborting (exit 130)\n";

fn signal_install_error(op: &'static str) -> ImageError {
    let source = io::Error::last_os_error();
    ImageError::Io {
        path: PathBuf::from(SIGNAL_HANDLER_PATH),
        source: io::Error::new(source.kind(), format!("{op}: {source}")),
    }
}

extern "C" fn nl_image_signal_handler(sig: c_int) {
    let mut msg = [0u8; 64];
    let mut len = 0usize;

    msg[..MSG_PREFIX.len()].copy_from_slice(MSG_PREFIX);
    len += MSG_PREFIX.len();

    let mut digits = [0u8; 12];
    let mut digit_len = 0usize;
    let mut n = sig;
    if n == 0 {
        digits[0] = b'0';
        digit_len = 1;
    } else {
        if n < 0 {
            msg[len] = b'-';
            len += 1;
            n = -n;
        }
        while n > 0 {
            digits[digit_len] = b'0' + (n % 10) as u8;
            digit_len += 1;
            n /= 10;
        }
        digits[..digit_len].reverse();
    }

    msg[len..len + digit_len].copy_from_slice(&digits[..digit_len]);
    len += digit_len;
    msg[len..len + MSG_SUFFIX.len()].copy_from_slice(MSG_SUFFIX);
    len += MSG_SUFFIX.len();

    unsafe {
        let _ = libc::write(libc::STDERR_FILENO, msg.as_ptr().cast(), len);
        libc::_exit(NL_IMAGE_FAULT_EXIT_CODE);
    }
}

/// Install SIGSEGV / SIGBUS / SIGFPE handlers for the current
/// process.  Idempotent: later calls overwrite the same handler with
/// itself.
///
/// SAFETY: mutates process-global signal dispositions.
pub unsafe fn install_signal_handlers() -> Result<(), ImageError> {
    let mut sa: libc::sigaction = std::mem::zeroed();
    sa.sa_sigaction = nl_image_signal_handler as *const () as usize;
    sa.sa_flags = 0;

    if libc::sigemptyset(&mut sa.sa_mask) != 0 {
        return Err(signal_install_error("sigemptyset failed"));
    }

    for sig in [libc::SIGSEGV, libc::SIGBUS, libc::SIGFPE] {
        if libc::sigaction(sig, &sa, std::ptr::null_mut()) != 0 {
            return Err(signal_install_error("sigaction failed"));
        }
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::{install_signal_handlers, NL_IMAGE_FAULT_EXIT_CODE};

    #[test]
    fn fault_exit_code_is_130() {
        assert_eq!(NL_IMAGE_FAULT_EXIT_CODE, 130);
    }

    #[test]
    fn install_signal_handlers_succeeds() {
        let result = unsafe { install_signal_handlers() };
        assert!(result.is_ok(), "expected Ok(()), got {result:?}");
    }

    #[test]
    fn install_signal_handlers_is_idempotent() {
        let first = unsafe { install_signal_handlers() };
        let second = unsafe { install_signal_handlers() };
        assert!(first.is_ok(), "expected first call Ok(()), got {first:?}");
        assert!(second.is_ok(), "expected second call Ok(()), got {second:?}");
    }
}
