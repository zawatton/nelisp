//! NeLisp build-time tool — Doc 47 §3.1 phase 6 carve-out skeleton.
//!
//! This crate exists to host code that runs at *build time* against
//! `.el` source to mint a `nelisp.image` v1 file, but does not need
//! to ship in the runtime binary that consumes the image.  Per Doc
//! 47 §1.1 the long-term split is:
//!
//! ```text
//! nelisp-runtime  = seed loader + image boot + syscall thin-wrappers
//!                   (target ≤ 4,000 LOC, image-only)
//! nelisp-build-tool = reader + minimal evaluator + dumper
//!                     (Doc 44 minimal interpreter lives here)
//! ```
//!
//! Stage 5a (this file): empty stub so the workspace builds.  Stages
//! 5b / 5c will progressively move `bin/nelisp.rs`, `eval/`,
//! `reader/`, and `bridge/` across from `nelisp-runtime` and re-wire
//! the `anvil-runtime` consumer to depend on this crate instead.
