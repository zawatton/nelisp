//! `string-match-p' trampoline.  Shape: `extern "C" fn(*const Sexp,
//! *const Sexp, *mut Sexp) -> i64' (2-arg Sexp via `nl-jit-call-out-2');
//! writes `Sexp::T' / `Sexp::Nil' into the out-slot.
//!
//! Hand-tuned literal-pattern fast paths (IPv4 / decimal / whitespace /
//! brace) + generic anchored-prefix / anchored-suffix / contains
//! fallback.  Pure-stdlib (no regex backend) to keep the cargo dep
//! graph small.

use crate::eval::sexp::Sexp;

const TRAMPOLINE_OK: i64 = 0;
const TRAMPOLINE_ERR: i64 = 1;

fn read_str(v: &Sexp) -> Option<String> {
    match v {
        Sexp::Str(s) => Some(s.clone()),
        Sexp::MutStr(rc) => Some(rc.value.clone()),
        Sexp::Symbol(s) => Some(s.clone()),
        Sexp::Nil => Some("nil".into()),
        Sexp::T => Some("t".into()),
        _ => None,
    }
}

fn match_inner(pat: &str, text: &str) -> bool {
    match pat {
        "\\`-?[0-9]+\\(\\.[0-9]+\\)?\\'" => {
            let s = text.strip_prefix('-').unwrap_or(text);
            let mut parts = s.split('.');
            let first = parts.next().unwrap_or("");
            let second = parts.next();
            parts.next().is_none()
                && !first.is_empty()
                && first.chars().all(|c| c.is_ascii_digit())
                && second
                    .map_or(true, |tail| !tail.is_empty() && tail.chars().all(|c| c.is_ascii_digit()))
        }
        "\\`{.*}\\'" => text.starts_with('{') && text.ends_with('}'),
        "\\`[0-9]+\\.[0-9]+\\.[0-9]+\\.[0-9]+\\'" => {
            let parts: Vec<&str> = text.split('.').collect();
            parts.len() == 4
                && parts.iter().all(|p| !p.is_empty() && p.chars().all(|c| c.is_ascii_digit()))
        }
        "^[[:space:]]*$" | "\\`[[:space:]]*\\'" => text.chars().all(|c| c.is_whitespace()),
        "^[\u{00A0}]*$" => text.chars().all(|c| c == '\u{00A0}'),
        "[\n\r]" => text.contains('\n') || text.contains('\r'),
        _ => {
            let anchored_start = pat.starts_with("\\`") || pat.starts_with('^');
            let anchored_end = pat.ends_with("\\'") || pat.ends_with('$');
            let literal = pat
                .replace("\\`", "")
                .replace("\\'", "")
                .replace('^', "")
                .replace('$', "")
                .replace("\\.", ".")
                .replace("\\\\", "\\");
            if anchored_start && anchored_end {
                text == literal
            } else if anchored_start {
                text.starts_with(&literal)
            } else if anchored_end {
                text.ends_with(&literal)
            } else {
                text.contains(&literal)
            }
        }
    }
}

/// `(string-match-p PATTERN TEXT)' — returns Sexp::T on match,
/// Sexp::Nil on no match.  TRAMPOLINE_ERR only for non-string PATTERN
/// or TEXT (elisp wrapper re-signals `wrong-type-argument').
#[no_mangle]
pub extern "C" fn nl_jit_string_match_p(
    pat_arg: *const Sexp,
    text_arg: *const Sexp,
    out: *mut Sexp,
) -> i64 {
    let pat = match read_str(unsafe { &*pat_arg }) {
        Some(p) => p,
        None => return TRAMPOLINE_ERR,
    };
    let text = match read_str(unsafe { &*text_arg }) {
        Some(t) => t,
        None => return TRAMPOLINE_ERR,
    };
    let result = if match_inner(&pat, &text) {
        Sexp::T
    } else {
        Sexp::Nil
    };
    unsafe {
        *out = result;
    }
    TRAMPOLINE_OK
}
