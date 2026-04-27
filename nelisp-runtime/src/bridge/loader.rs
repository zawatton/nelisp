use std::fs;
use std::path::Path;

use crate::eval::{self, Env, Sexp};
use crate::reader::{self, lexer, parser};

use super::BridgeError;

const CANONICAL_FILES: &[&str] = &[
    "nelisp-read.el",
    "nelisp-eval.el",
    "nelisp-macro.el",
    "nelisp-load.el",
    "nelisp.el",
];

pub fn canonical_files() -> &'static [&'static str] {
    CANONICAL_FILES
}

struct TrackedForm {
    form: Sexp,
    line: u32,
}

pub fn bootstrap_files(env: &mut Env, src_dir: &Path) -> Result<(usize, usize), BridgeError> {
    let mut files_loaded = 0usize;
    let mut forms_evaluated = 0usize;

    for rel in CANONICAL_FILES {
        let path = src_dir.join(rel);
        let count = load_file(env, &path)?;
        files_loaded += 1;
        forms_evaluated += count;
    }

    Ok((files_loaded, forms_evaluated))
}

fn load_file(env: &mut Env, path: &Path) -> Result<usize, BridgeError> {
    let source = fs::read_to_string(path)
        .map_err(|err| BridgeError::ReadError(err.to_string(), path.to_path_buf()))?;
    let normalized = normalize_bootstrap_source(&source);
    let forms = parse_tracked_forms(&normalized, path)?;
    let mut count = 0usize;

    for tracked in forms {
        count += 1;
        if handle_bridge_form(env, &tracked.form)? {
            continue;
        }
        eval::eval(&tracked.form, env).map_err(|err| {
            BridgeError::EvalError(
                format!(
                    "line {} in {}: {}",
                    tracked.line,
                    form_context(&tracked.form),
                    err
                ),
                path.to_path_buf(),
            )
        })?;
    }

    Ok(count)
}

fn normalize_bootstrap_source(source: &str) -> String {
    let bytes = source.as_bytes();
    let mut out = String::with_capacity(source.len());
    let mut i = 0usize;
    let mut in_string = false;
    let mut in_comment = false;

    while i < bytes.len() {
        let b = bytes[i];
        if in_comment {
            out.push(b as char);
            if b == b'\n' {
                in_comment = false;
            }
            i += 1;
            continue;
        }
        if in_string {
            out.push(b as char);
            if b == b'\\' && i + 1 < bytes.len() {
                i += 1;
                out.push(bytes[i] as char);
            } else if b == b'"' {
                in_string = false;
            }
            i += 1;
            continue;
        }

        match b {
            b';' => {
                in_comment = true;
                out.push(';');
                i += 1;
            }
            b'"' => {
                in_string = true;
                out.push('"');
                i += 1;
            }
            b'#' if i + 2 < bytes.len() && bytes[i + 1] == b'\'' => {
                if let Some((token, consumed)) = read_symbol_token(&bytes[(i + 2)..]) {
                    out.push_str("(function ");
                    out.push_str(&token);
                    out.push(')');
                    i += 2 + consumed;
                } else {
                    out.push('#');
                    i += 1;
                }
            }
            b'?' => {
                if let Some((value, consumed)) = read_char_literal(&bytes[i..]) {
                    out.push_str(&value.to_string());
                    i += consumed;
                } else {
                    out.push('?');
                    i += 1;
                }
            }
            _ => {
                out.push(b as char);
                i += 1;
            }
        }
    }

    out
}

fn read_symbol_token(bytes: &[u8]) -> Option<(String, usize)> {
    let mut end = 0usize;
    while end < bytes.len() {
        let b = bytes[end];
        if matches!(b, b'(' | b')' | b'[' | b']' | b'"' | b'\'' | b';')
            || b.is_ascii_whitespace()
        {
            break;
        }
        end += 1;
    }
    if end == 0 {
        None
    } else {
        Some((String::from_utf8_lossy(&bytes[..end]).into_owned(), end))
    }
}

fn read_char_literal(bytes: &[u8]) -> Option<(i64, usize)> {
    if bytes.len() < 2 || bytes[0] != b'?' {
        return None;
    }
    if bytes[1] == b'\\' {
        let escaped = *bytes.get(2)?;
        let value = match escaped {
            b'n' => b'\n',
            b't' => b'\t',
            b'r' => b'\r',
            b'f' => 0x0c,
            b'a' => 0x07,
            b'b' => 0x08,
            b'e' => 27,
            b'0' => 0,
            b's' | b' ' => b' ',
            other => other,
        };
        Some((value as i64, 3))
    } else {
        Some((bytes[1] as i64, 2))
    }
}

fn parse_tracked_forms(source: &str, path: &Path) -> Result<Vec<TrackedForm>, BridgeError> {
    let _ = reader::read_all(source)
        .map_err(|err| BridgeError::ReadError(err.to_string(), path.to_path_buf()))?;
    let tokens = lexer::tokenize(source)
        .map_err(|err| BridgeError::ReadError(err.to_string(), path.to_path_buf()))?;
    let mut forms = Vec::new();
    let mut pos = 0usize;

    while pos < tokens.len() {
        let line = tokens[pos].pos.line;
        let (form, consumed) = parser::parse_one(&tokens[pos..])
            .map_err(|err| BridgeError::ReadError(err.to_string(), path.to_path_buf()))?;
        forms.push(TrackedForm { form, line });
        pos += consumed;
    }

    Ok(forms)
}

fn handle_bridge_form(env: &mut Env, form: &Sexp) -> Result<bool, BridgeError> {
    let Some((head, args)) = list_head(form) else {
        return Ok(false);
    };

    match head {
        "require" | "provide" | "declare-function" | "defgroup" | "defface" => Ok(true),
        "define-error" => Ok(true),
        "nelisp--install-core-macros" => Ok(true),
        "defvar" => {
            handle_defvar_like(env, &args, false);
            Ok(true)
        }
        "defconst" => {
            handle_defvar_like(env, &args, true);
            Ok(true)
        }
        "defcustom" => {
            handle_defvar_like(env, &args, false);
            Ok(true)
        }
        _ => Ok(false),
    }
}

fn handle_defvar_like(env: &mut Env, args: &[Sexp], is_constant: bool) {
    if let Some(Sexp::Symbol(name)) = args.first() {
        let value = args
            .get(1)
            .map(bridge_bootstrap_value)
            .unwrap_or(Sexp::Nil);
        env.defvar(name, value, is_constant);
    }
}

fn bridge_bootstrap_value(form: &Sexp) -> Sexp {
    let Some((head, args)) = list_head(form) else {
        return form.clone();
    };

    match head {
        "make-hash-table" => {
            if let Some(label) = args.first() {
                Sexp::Symbol(format!("__bridge_hash_{}", sanitize_label(label)))
            } else {
                Sexp::Symbol("__bridge_hash_table".into())
            }
        }
        "make-symbol" => match args.first() {
            Some(Sexp::Str(name)) => Sexp::Symbol(name.clone()),
            Some(other) => Sexp::Symbol(format!("__bridge_symbol_{}", sanitize_label(other))),
            None => Sexp::Symbol("__bridge_symbol".into()),
        },
        _ => form.clone(),
    }
}

fn sanitize_label(sexp: &Sexp) -> String {
    match sexp {
        Sexp::Symbol(s) | Sexp::Str(s) => s
            .chars()
            .map(|ch| {
                if ch.is_ascii_alphanumeric() || ch == '-' || ch == '_' {
                    ch
                } else {
                    '_'
                }
            })
            .collect(),
        _ => "value".into(),
    }
}

fn list_head(form: &Sexp) -> Option<(&str, Vec<Sexp>)> {
    match form {
        Sexp::Cons(head, tail) => match head.as_ref() {
            Sexp::Symbol(name) => Some((name.as_str(), list_to_vec(tail).ok()?)),
            _ => None,
        },
        _ => None,
    }
}

fn list_to_vec(list: &Sexp) -> Result<Vec<Sexp>, ()> {
    let mut out = Vec::new();
    let mut cur = list;
    loop {
        match cur {
            Sexp::Nil => return Ok(out),
            Sexp::Cons(car, cdr) => {
                out.push((**car).clone());
                cur = cdr;
            }
            _ => return Err(()),
        }
    }
}

fn form_context(form: &Sexp) -> String {
    match form {
        Sexp::Cons(head, _) => match head.as_ref() {
            Sexp::Symbol(name) => format!("top-level `{}`", name),
            _ => format!("form `{}`", reader::fmt_sexp(form)),
        },
        _ => format!("form `{}`", reader::fmt_sexp(form)),
    }
}
