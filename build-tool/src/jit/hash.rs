//! `nl-secure-hash` non-SHA1 fallback (sha224/256/384/512/md5).
//! SHA1 is handled by Phase 47 elisp object `nl_jit_secure_hash.o'.

use crate::eval::sexp::Sexp;

use super::{TRAMPOLINE_ERR, TRAMPOLINE_OK};

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

#[no_mangle]
pub extern "C" fn nl_jit_secure_hash_non_sha1(
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
    macro_rules! sha2_hex {
        ($ty:ty) => {{
            use sha2::Digest;
            let mut h = <$ty>::new();
            h.update(&bytes);
            hex_lower(&h.finalize())
        }};
    }
    let hex = match algo.as_str() {
        "sha256" => sha2_hex!(sha2::Sha256),
        "sha224" => sha2_hex!(sha2::Sha224),
        "sha384" => sha2_hex!(sha2::Sha384),
        "sha512" => sha2_hex!(sha2::Sha512),
        "md5" => hex_lower(&md5::compute(&bytes).0),
        _ => return TRAMPOLINE_ERR,
    };
    unsafe {
        *out = Sexp::Str(hex);
    }
    TRAMPOLINE_OK
}
