//! Phase 7 Stage 7.7.a (Doc 72) — STDLIB elisp → AOT image baker.
//!
//! Reads each `lisp/*.el` listed in `STDLIB_FILES` (= 1:1 mirror of
//! `Env::new_global`'s STDLIB_SOURCES) and emits a sibling
//! `lisp/*.image` file produced by `image::compile_elisp_to_image`.
//!
//! Why a dedicated binary: Stage 7.7.b will swap `Env::new_global`'s
//! `reader::read_all(include_str!(...))` to `image::decode_image
//! (include_bytes!(...))`, which removes the Rust reader from the
//! production startup path.  The `.image` files must therefore exist
//! at compile time of the main `nelisp` binary, so the bake step runs
//! ahead of `cargo build` (= `make bake-images`, see Makefile).
//!
//! The baker itself uses the Rust reader internally
//! (`compile_elisp_to_image` → `reader::read_all`).  That is fine:
//! Stage 7.7.c gates the Rust reader behind `cfg(test, feature =
//! "rust-reader")`, and the baker compiles with the feature on (= the
//! baker is dev-tooling, not production runtime).
//!
//! CLI:
//!
//!     nelisp-baker [--lisp-dir DIR] [--check]
//!
//! `--lisp-dir` defaults to the workspace's `lisp/` directory
//! (resolved via `CARGO_MANIFEST_DIR/../lisp`).  `--check` runs the
//! bake but compares against on-disk bytes instead of overwriting; non-
//! zero exit means a `.el` was edited without rebaking.

use nelisp_build_tool::image;
use std::env;
use std::fs;
use std::path::PathBuf;
use std::process::ExitCode;

/// 1:1 mirror of `Env::new_global`'s STDLIB_SOURCES list.  Keep in
/// sync — Stage 7.7.b's `STDLIB_IMAGES` will iterate the same names.
const STDLIB_FILES: &[&str] = &[
    "nelisp-stdlib-eval-special.el",
    "nelisp-stdlib.el",
    "nelisp-stdlib-list.el",
    "nelisp-stdlib-hof.el",
    "nelisp-stdlib-search.el",
    "nelisp-stdlib-plist-str.el",
    "nelisp-stdlib-misc.el",
    "nelisp-stdlib-os.el",
    "nelisp-pcase.el",
    "nelisp-cl-macros.el",
    "nelisp-stdlib-hash.el",
    "nelisp-stdlib-equal.el",
    "nelisp-stdlib-prn.el",
    "nelisp-stdlib-reader.el",
    "nelisp-stdlib-eval-core.el",
];

fn main() -> ExitCode {
    let mut args = env::args().skip(1);
    let mut lisp_dir: Option<PathBuf> = None;
    let mut check_only = false;
    while let Some(arg) = args.next() {
        match arg.as_str() {
            "--lisp-dir" => match args.next() {
                Some(v) => lisp_dir = Some(PathBuf::from(v)),
                None => {
                    eprintln!("nelisp-baker: --lisp-dir requires an argument");
                    return ExitCode::from(2);
                }
            },
            "--check" => check_only = true,
            "--help" | "-h" => {
                println!("usage: nelisp-baker [--lisp-dir DIR] [--check]");
                return ExitCode::SUCCESS;
            }
            other => {
                eprintln!("nelisp-baker: unknown argument {}", other);
                return ExitCode::from(2);
            }
        }
    }
    let lisp_dir = match lisp_dir {
        Some(p) => p,
        None => default_lisp_dir(),
    };
    if !lisp_dir.is_dir() {
        eprintln!(
            "nelisp-baker: lisp dir does not exist: {}",
            lisp_dir.display()
        );
        return ExitCode::from(1);
    }

    let mut mismatches = Vec::new();
    for name in STDLIB_FILES {
        let src_path = lisp_dir.join(name);
        let img_path = lisp_dir.join(format!("{}.image", name));
        let source = match fs::read_to_string(&src_path) {
            Ok(s) => s,
            Err(e) => {
                eprintln!("nelisp-baker: cannot read {}: {}", src_path.display(), e);
                return ExitCode::from(1);
            }
        };
        let bytes = match image::compile_elisp_to_image(&source) {
            Ok(b) => b,
            Err(e) => {
                eprintln!("nelisp-baker: bake {} failed: {}", name, e);
                return ExitCode::from(7);
            }
        };
        if check_only {
            let on_disk = fs::read(&img_path).unwrap_or_default();
            if on_disk != bytes {
                mismatches.push(name.to_string());
            }
        } else if let Err(e) = fs::write(&img_path, &bytes) {
            eprintln!("nelisp-baker: cannot write {}: {}", img_path.display(), e);
            return ExitCode::from(1);
        } else {
            println!("baked {} ({} bytes)", img_path.display(), bytes.len());
        }
    }

    if check_only && !mismatches.is_empty() {
        eprintln!("nelisp-baker: stale images detected:");
        for n in &mismatches {
            eprintln!("  {}.image", n);
        }
        eprintln!("run `make bake-images` to refresh.");
        return ExitCode::from(3);
    }
    ExitCode::SUCCESS
}

fn default_lisp_dir() -> PathBuf {
    // CARGO_MANIFEST_DIR is build-tool/, sibling lisp/ is one up.
    let manifest = env!("CARGO_MANIFEST_DIR");
    PathBuf::from(manifest).join("..").join("lisp")
}
