//! NeLisp build-time tool — Doc 47 §3.1 phase 6 carve-out.
//!
//! Hosts the Doc 44 minimal interpreter (reader + evaluator + Elisp
//! self-host bridge) so the `nelisp-runtime` ship crate stays
//! image-only.  Per Doc 47 §1.1 the long-term split is:
//!
//! ```text
//! nelisp-runtime    = seed loader + image boot + syscall thin-wrappers
//!                     (target ≤ 4,000 LOC, image-only)
//! nelisp-build-tool = reader + minimal evaluator + dumper
//!                     (Doc 44 minimal interpreter lives here)
//! ```
//!
//! Stage 5a created the workspace + empty crate.  Stage 5b moved
//! `bin/nelisp.rs` here.  Stage 5c (this commit) `git mv`-ed
//! `eval/`, `reader/`, `bridge/` across from `nelisp-runtime/src/`
//! and re-wired the `anvil-runtime` consumer to depend on this
//! crate instead.  Future Stage 6+ will add the dumper that bakes a
//! heap evaluated by these modules into a `nelisp.image` v1 file.

pub mod bridge;
pub mod eval;
pub mod image;
// Phase 5 Stage 5.0 / Doc 77b Stage b.4 — Cranelift JIT.  Lowered
// primitives flow through elisp wrappers in
// `lisp/nelisp-jit-strategy.el' that call JIT entries via the
// `nl-jit-call-*' bridge primitives; eval-loop dispatches builtins
// directly to `eval::builtins::dispatch' (no `lower_entries' hook).
//
// Phase 7.1.7.a (2026-05-10): narrowed to `pub(crate)' — the only
// crate-external surface ever needed was the `bi_*' re-exports for
// `eval::builtins::dispatch' which are siblings inside the crate.
// Keeping this `pub(crate)' lets `UnifiedJit' field types stay
// `pub(super)' without `private_interfaces' warnings.
pub(crate) mod jit;
// Reader feature gate (Doc 73 §2.4 / Doc 98 §98.3).  All production
// `reader::read_*' callsites are gone: boot reads pre-baked NELIMG v3
// frozen-heap images via `image::decode_v3_into', and `eval::eval_str'
// / `eval_str_all' route through the elisp reader.  The Rust reader
// survives only inside `image-baker' feature builds (= `nelisp-baker'
// dev tool) where `image::iterative_bake_one' parses each stdlib
// source on the way to its v3 image, plus the reader's own ERTs.
#[cfg(any(test, feature = "image-baker"))]
pub mod reader;

// Doc 99 §99.B spike — C-callable function compiled from elisp by the
// Phase 47 chain.  `build.rs' runs `scripts/compile-elisp-objects.el'
// to produce `target/<...>/elisp-objects/nelisp_spike_noop.o' and
// links it into the crate via `cargo:rustc-link-lib=static=...'.
// This module gives the symbol a Rust home so cargo doesn't dead-code-
// eliminate it from the final binary and `cargo test' can probe the
// round-trip end-to-end.  Linux x86_64 only — other targets skip the
// build step entirely (see `build.rs::link_elisp_cc_spike').
#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
pub mod elisp_cc_spike {
    extern "C" {
        fn nelisp_spike_noop() -> i64;
    }

    /// Doc 99 §99.B probe — call the elisp-compiled function and return
    /// its result.  Used by `tests/elisp_cc_spike_probe.rs' to prove the
    /// build chain (elisp source → Phase 47 compile → ET_REL .o → ar
    /// static archive → cargo link) terminates in a callable symbol.
    pub fn probe() -> i64 {
        unsafe { nelisp_spike_noop() }
    }
}
