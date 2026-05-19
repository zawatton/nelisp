//! `string-match-p` trampoline with a few literal fast paths.

use crate::eval::sexp::Sexp;

use super::{read_sexp_str, TRAMPOLINE_ERR, TRAMPOLINE_OK};

fn all_digits(s: &str) -> bool {
    !s.is_empty() && s.chars().all(|c| c.is_ascii_digit())
}

fn match_inner(pat: &str, text: &str) -> bool {
    match pat {
        "\\`-?[0-9]+\\(\\.[0-9]+\\)?\\'" => {
            let s = text.strip_prefix('-').unwrap_or(text);
            let mut parts = s.split('.');
            let first = parts.next().unwrap_or("");
            let second = parts.next();
            parts.next().is_none() && all_digits(first) && second.map_or(true, all_digits)
        }
        "\\`{.*}\\'" => text.starts_with('{') && text.ends_with('}'),
        "\\`[0-9]+\\.[0-9]+\\.[0-9]+\\.[0-9]+\\'" => {
            let parts: Vec<&str> = text.split('.').collect();
            parts.len() == 4 && parts.iter().all(|p| all_digits(p))
        }
        "^[[:space:]]*$" | "\\`[[:space:]]*\\'" => text.chars().all(|c| c.is_whitespace()),
        "^[\u{00A0}]*$" => text.chars().all(|c| c == '\u{00A0}'),
        "[\n\r]" => text.contains('\n') || text.contains('\r'),
        _ => {
            let anchored_start = pat.starts_with("\\`") || pat.starts_with('^');
            let anchored_end = pat.ends_with("\\'") || pat.ends_with('$');
            let literal = pat
                .replace("\\`", "").replace("\\'", "")
                .replace('^', "").replace('$', "")
                .replace("\\.", ".").replace("\\\\", "\\");
            match (anchored_start, anchored_end) {
                (true, true) => text == literal,
                (true, false) => text.starts_with(&literal),
                (false, true) => text.ends_with(&literal),
                (false, false) => text.contains(&literal),
            }
        }
    }
}

/// Returns `TRAMPOLINE_ERR` only for non-string inputs.
#[no_mangle]
pub extern "C" fn nl_jit_string_match_p(
    pat_arg: *const Sexp,
    text_arg: *const Sexp,
    out: *mut Sexp,
) -> i64 {
    let pat = match read_sexp_str(unsafe { &*pat_arg }) {
        Some(p) => p,
        None => return TRAMPOLINE_ERR,
    };
    let text = match read_sexp_str(unsafe { &*text_arg }) {
        Some(t) => t,
        None => return TRAMPOLINE_ERR,
    };
    unsafe { *out = if match_inner(&pat, &text) { Sexp::T } else { Sexp::Nil }; }
    TRAMPOLINE_OK
}
