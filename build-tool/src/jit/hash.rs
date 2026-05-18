//! `nl-secure-hash' trampoline.  Shape: `extern "C" fn(*const Sexp,
//! *const Sexp, *mut Sexp) -> i64' (2-arg Sexp via `nl-jit-call-out-2');
//! writes lowercase hex digest as a fresh `Sexp::Str' into the out-slot.
//! Algorithms: sha1 / sha224 / sha256 / sha384 / sha512 / md5.

use crate::eval::sexp::Sexp;

const TRAMPOLINE_OK: i64 = 0;
const TRAMPOLINE_ERR: i64 = 1;

fn hex_lower(bytes: &[u8]) -> String {
    let mut out = String::with_capacity(bytes.len() * 2);
    for b in bytes {
        out.push_str(&format!("{:02x}", b));
    }
    out
}

fn algo_name(v: &Sexp) -> Option<&str> {
    match v {
        Sexp::Symbol(s) | Sexp::Str(s) => Some(s.as_str()),
        Sexp::MutStr(rc) => Some(rc.value.as_str()),
        _ => None,
    }
}

fn text_bytes(v: &Sexp) -> Option<Vec<u8>> {
    match v {
        Sexp::Str(s) => Some(s.as_bytes().to_vec()),
        Sexp::MutStr(rc) => Some(rc.value.as_bytes().to_vec()),
        Sexp::Symbol(s) => Some(s.as_bytes().to_vec()),
        _ => None,
    }
}

/// `(nl-secure-hash ALGO STRING)' — returns TRAMPOLINE_ERR for
/// unsupported ALGO or non-string STRING; the elisp wrapper re-signals
/// as `wrong-type-argument'.
#[no_mangle]
pub extern "C" fn nl_jit_secure_hash(
    algo_arg: *const Sexp,
    str_arg: *const Sexp,
    out: *mut Sexp,
) -> i64 {
    let algo_ref = unsafe { &*algo_arg };
    let str_ref = unsafe { &*str_arg };
    let algo = match algo_name(algo_ref) {
        Some(a) => a.to_string(),
        None => return TRAMPOLINE_ERR,
    };
    let bytes = match text_bytes(str_ref) {
        Some(b) => b,
        None => return TRAMPOLINE_ERR,
    };
    let hex = match algo.as_str() {
        "sha1" => {
            use sha1::{Digest, Sha1};
            let mut h = Sha1::new();
            h.update(&bytes);
            hex_lower(&h.finalize())
        }
        "sha256" => {
            use sha2::{Digest, Sha256};
            let mut h = Sha256::new();
            h.update(&bytes);
            hex_lower(&h.finalize())
        }
        "sha224" => {
            use sha2::{Digest, Sha224};
            let mut h = Sha224::new();
            h.update(&bytes);
            hex_lower(&h.finalize())
        }
        "sha384" => {
            use sha2::{Digest, Sha384};
            let mut h = Sha384::new();
            h.update(&bytes);
            hex_lower(&h.finalize())
        }
        "sha512" => {
            use sha2::{Digest, Sha512};
            let mut h = Sha512::new();
            h.update(&bytes);
            hex_lower(&h.finalize())
        }
        "md5" => {
            let digest = md5::compute(&bytes);
            hex_lower(&digest.0)
        }
        _ => return TRAMPOLINE_ERR,
    };
    unsafe {
        *out = Sexp::Str(hex);
    }
    TRAMPOLINE_OK
}
