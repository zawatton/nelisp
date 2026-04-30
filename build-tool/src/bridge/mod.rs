//! Phase 8.0.3 — NeLisp self-host bridge (Doc 44 §3.4 LOCKED).
//!
//! The bridge bootstraps a dependency-ordered subset of the NeLisp
//! source tree through the shipped Rust reader + evaluator, then
//! exposes a takeover API that routes future evaluations through the
//! installed `nelisp-eval-form` closure when available.

mod loader;
mod takeover;

use std::fmt;
use std::path::{Path, PathBuf};
use std::time::{Duration, Instant};

use crate::eval::{Env, Sexp};

pub use loader::canonical_files;
pub use takeover::takeover_to_nelisp_eval;

/// Summary of a completed bootstrap pass.
#[derive(Debug, Clone)]
pub struct BootstrapReport {
    pub files_loaded: usize,
    pub forms_evaluated: usize,
    pub elapsed: Duration,
}

/// Bridge-layer failures surfaced during bootstrap or takeover.
#[derive(Debug, Clone)]
pub enum BridgeError {
    ReadError(String, PathBuf),
    EvalError(String, PathBuf),
    SrcDirNotFound(PathBuf),
    TakeoverFailed(String),
}

impl fmt::Display for BridgeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BridgeError::ReadError(msg, path) => {
                write!(f, "bridge read error in {}: {}", path.display(), msg)
            }
            BridgeError::EvalError(msg, path) => {
                write!(f, "bridge eval error in {}: {}", path.display(), msg)
            }
            BridgeError::SrcDirNotFound(path) => {
                write!(f, "NeLisp source directory not found: {}", path.display())
            }
            BridgeError::TakeoverFailed(msg) => write!(f, "bridge takeover failed: {}", msg),
        }
    }
}

impl std::error::Error for BridgeError {}

/// Bootstrap the NeLisp self-host into `env` from `src_dir`.
///
/// The loader consumes the canonical Phase 8.0.3 file list in
/// dependency order, parses each file with `reader::read_all`'s
/// underlying tokenizer/parser, and evaluates each top-level form
/// sequentially in the shared `env`.
pub fn bootstrap_self_host(
    env: &mut Env,
    src_dir: &Path,
) -> Result<BootstrapReport, BridgeError> {
    if !src_dir.is_dir() {
        return Err(BridgeError::SrcDirNotFound(src_dir.to_path_buf()));
    }

    let started = Instant::now();
    let (files_loaded, forms_evaluated) = loader::bootstrap_files(env, src_dir)?;
    takeover::takeover_to_nelisp_eval(env)?;

    Ok(BootstrapReport {
        files_loaded,
        forms_evaluated,
        elapsed: started.elapsed(),
    })
}

/// Evaluate `form` through the post-bootstrap self-host dispatcher.
///
/// The bridge first attempts the installed `nelisp-eval-form`
/// closure.  If the cold-start bootstrap only installed a partial
/// self-host surface, it falls back to the shipped Rust evaluator so
/// callers still get a total evaluation API during bootstrap bring-up.
pub fn eval_via_self_host(form: &Sexp, env: &mut Env) -> Result<Sexp, BridgeError> {
    takeover::eval_via_self_host(form, env)
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;
    use std::time::{SystemTime, UNIX_EPOCH};

    fn unique_temp_dir(label: &str) -> PathBuf {
        let nanos = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_nanos();
        let dir = std::env::temp_dir().join(format!(
            "nelisp-bridge-{}-{}-{}",
            label,
            std::process::id(),
            nanos
        ));
        fs::create_dir_all(&dir).unwrap();
        dir
    }

    fn write_file(dir: &Path, name: &str, body: &str) {
        fs::write(dir.join(name), body).unwrap();
    }

    fn write_minimal_tree(dir: &Path, eval_form_body: &str) {
        write_file(
            dir,
            "nelisp-read.el",
            "(defun nelisp-read-all (_s) nil)\n(provide 'nelisp-read)\n",
        );
        write_file(
            dir,
            "nelisp-eval.el",
            &format!(
                "(defun nelisp-eval-form (form env) {})\n(provide 'nelisp-eval)\n",
                eval_form_body
            ),
        );
        write_file(dir, "nelisp-macro.el", "(provide 'nelisp-macro)\n");
        write_file(dir, "nelisp-load.el", "(provide 'nelisp-load)\n");
        write_file(dir, "nelisp.el", "(provide 'nelisp)\n");
    }

    fn resolve_real_src_dir() -> Option<PathBuf> {
        if let Some(path) = std::env::var_os("NELISP_SRC_DIR") {
            let path = PathBuf::from(path);
            if path.is_dir() {
                return Some(path);
            }
        }
        if let Some(home) = std::env::var_os("ANVIL_HOME") {
            let path = PathBuf::from(home).join("src");
            if path.is_dir() {
                return Some(path);
            }
        }
        None
    }

    #[test]
    fn bootstrap_with_synthetic_minimal_src() {
        let dir = unique_temp_dir("minimal");
        write_minimal_tree(&dir, "(+ (eval form) 1)");

        let mut env = Env::new_global();
        let report = bootstrap_self_host(&mut env, &dir).unwrap();
        assert_eq!(report.files_loaded, canonical_files().len());
        assert_eq!(report.forms_evaluated, 7);

        let got = eval_via_self_host(&crate::reader::read_str("(+ 1 2 3)").unwrap(), &mut env)
            .unwrap();
        assert_eq!(got, Sexp::Int(7));

        let _ = fs::remove_dir_all(dir);
    }

    #[test]
    fn bootstrap_missing_src_dir_errors() {
        let missing = PathBuf::from("/tmp/definitely-missing-nelisp-bridge-src");
        let mut env = Env::new_global();
        match bootstrap_self_host(&mut env, &missing) {
            Err(BridgeError::SrcDirNotFound(path)) => assert_eq!(path, missing),
            other => panic!("expected missing-dir error, got {:?}", other),
        }
    }

    #[test]
    fn bootstrap_eval_failure_keeps_file_context() {
        let dir = unique_temp_dir("eval-fail");
        write_file(dir.as_path(), "nelisp-read.el", "(provide 'nelisp-read)\n");
        write_file(dir.as_path(), "nelisp-eval.el", "(missing-bootstrap-fn 1)\n");
        write_file(dir.as_path(), "nelisp-macro.el", "(provide 'nelisp-macro)\n");
        write_file(dir.as_path(), "nelisp-load.el", "(provide 'nelisp-load)\n");
        write_file(dir.as_path(), "nelisp.el", "(provide 'nelisp)\n");

        let mut env = Env::new_global();
        match bootstrap_self_host(&mut env, &dir) {
            Err(BridgeError::EvalError(msg, path)) => {
                assert!(msg.contains("line 1"));
                assert!(msg.contains("missing-bootstrap-fn"));
                assert!(path.ends_with("nelisp-eval.el"));
            }
            other => panic!("expected eval failure, got {:?}", other),
        }

        let _ = fs::remove_dir_all(dir);
    }

    #[test]
    fn real_src_smoke_skips_without_env() {
        let Some(src_dir) = resolve_real_src_dir() else {
            return;
        };

        let mut env = Env::new_global();
        let report = bootstrap_self_host(&mut env, &src_dir).unwrap();
        assert_eq!(report.files_loaded, canonical_files().len());
        assert!(report.forms_evaluated > 0);

        let plus = crate::reader::read_str("(+ 1 2 3)").unwrap();
        assert_eq!(eval_via_self_host(&plus, &mut env).unwrap(), Sexp::Int(6));

        let square = crate::reader::read_str("(funcall (lambda (n) (* n n)) 7)").unwrap();
        assert_eq!(eval_via_self_host(&square, &mut env).unwrap(), Sexp::Int(49));
    }
}
