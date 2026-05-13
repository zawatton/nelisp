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
    use crate::eval::sexp::Sexp;

    // Sexp is `#[repr(C, u8)]` (see `eval/sexp.rs:57' + the assertions
    // in `eval/sexp_abi_assert.rs') so passing it across an extern "C"
    // boundary by raw pointer is sound — the elisp `.o' only touches
    // bytes at the offsets `nelisp-sexp--offset-*' name (= 0 for tag,
    // 8 for the i64 payload of Sexp::Int).  Rust's `improper_ctypes'
    // lint is conservative because Sexp's variants embed a `String'
    // (which is not `#[repr(C)]'), so the lint trips even though we
    // never pass a Sexp by value.
    #[allow(improper_ctypes)]
    extern "C" {
        fn nelisp_spike_noop() -> i64;
        // Doc 99 §99.C — recursive i64 factorial from
        // `lisp/nelisp-cc-fact-i64.el'.  N must satisfy 0 ≤ N ≤ 20
        // (= the fixnum-safe range for i64); the elisp body itself
        // doesn't range-check, so callers in safe Rust must clamp.
        fn nelisp_fact_i64(n: i64) -> i64;
        // Doc 100 §100.C — `(truncate INT)' Int arm.  Reads the i64
        // payload at `*arg0' (must be `Sexp::Int' — caller's
        // precondition, not checked here) and writes a fresh
        // `Sexp::Int' with the same payload into `*result_slot'.
        // Returns `result_slot' for caller ergonomics.  Defined in
        // `lisp/nelisp-cc-truncate-int.el'.
        fn nelisp_truncate_int(arg0: *const Sexp, result_slot: *mut Sexp) -> *mut Sexp;
        // Doc 100 §100.D Stage 1 — 12 `nl_jit_arith_*' trampoline
        // swaps.  Defined in `lisp/nelisp-cc-jit-arith.el', wired to
        // `unified_fn_ptr' in `jit/bridge.rs::arith_link'.  These
        // declarations also pin the symbols into the test binary's
        // link line (= the rlib's `bridge::arith_link' extern block
        // alone gets DCE'd by `--gc-sections' before integration
        // tests can call them).
        pub fn nelisp_jit_add2(a: i64, b: i64) -> i64;
        pub fn nelisp_jit_sub2(a: i64, b: i64) -> i64;
        pub fn nelisp_jit_mul2(a: i64, b: i64) -> i64;
        pub fn nelisp_jit_eq2(a: i64, b: i64) -> i64;
        pub fn nelisp_jit_lt2(a: i64, b: i64) -> i64;
        pub fn nelisp_jit_gt2(a: i64, b: i64) -> i64;
        pub fn nelisp_jit_le2(a: i64, b: i64) -> i64;
        pub fn nelisp_jit_ge2(a: i64, b: i64) -> i64;
        pub fn nelisp_jit_logior2(a: i64, b: i64) -> i64;
        pub fn nelisp_jit_logand2(a: i64, b: i64) -> i64;
        pub fn nelisp_jit_logxor2(a: i64, b: i64) -> i64;
        pub fn nelisp_jit_ash(n: i64, c: i64) -> i64;
        // Doc 110 §110.E.2.a — `jit/float.rs' 4 arithmetic trampoline
        // swaps (add / sub / mul / div).  Defined in
        // `lisp/nelisp-cc-jit-float.el'; wired to `unified_fn_ptr' via
        // the `float_link' module in `jit/bridge.rs'.  These decls
        // also pin the symbols into the integration test binary's
        // link line (= the rlib's `float_link' extern block alone
        // gets DCE'd by `--gc-sections' before tests can call them).
        pub fn nl_jit_float_add(a: f64, b: f64) -> f64;
        pub fn nl_jit_float_sub(a: f64, b: f64) -> f64;
        pub fn nl_jit_float_mul(a: f64, b: f64) -> f64;
        pub fn nl_jit_float_div(a: f64, b: f64) -> f64;
        // Doc 110 §110.C.2.a — 4 ordered comparison trampoline swaps.
        // i64 return matches the `extern "C" fn(f64, f64) -> i64'
        // float.rs cmp shape.  NaN semantics: 0 (= matches Rust).
        pub fn nl_jit_float_lt(a: f64, b: f64) -> i64;
        pub fn nl_jit_float_gt(a: f64, b: f64) -> i64;
        pub fn nl_jit_float_le(a: f64, b: f64) -> i64;
        pub fn nl_jit_float_ge(a: f64, b: f64) -> i64;
    }

    /// Doc 99 §99.B probe — call the elisp-compiled function and return
    /// its result.  Used by `tests/elisp_cc_spike_probe.rs' to prove the
    /// build chain (elisp source → Phase 47 compile → ET_REL .o → ar
    /// static archive → cargo link) terminates in a callable symbol.
    pub fn probe() -> i64 {
        unsafe { nelisp_spike_noop() }
    }

    /// Doc 99 §99.C — i64 factorial implemented in elisp.  Wraps the
    /// `nelisp_fact_i64' extern.  The caller is responsible for the
    /// 0..=20 range invariant; the Rust shim in `eval::builtins'
    /// enforces it before calling.
    pub fn fact_i64(n: i64) -> i64 {
        unsafe { nelisp_fact_i64(n) }
    }

    /// Doc 100 §100.C — `(truncate INT)' Int arm, elisp-compiled.
    ///
    /// `arg0' must point at a valid `Sexp::Int' value; `result_slot'
    /// must point at a 32-byte writable Sexp slot.  The elisp body
    /// reads the i64 payload of `*arg0' and writes `Sexp::Int(same)'
    /// into `*result_slot'.  No allocation, no Rust helpers — every
    /// memory access is a single `disp8' load / store emitted by
    /// Phase 47's `sexp-int-unwrap' / `sexp-int-make' grammar forms.
    ///
    /// # Safety
    ///
    /// - `arg0' must be a non-null pointer to an initialized `Sexp::Int'.
    /// - `result_slot' must be a non-null, properly aligned 32-byte
    ///   writable region (= `&mut MaybeUninit<Sexp>' or
    ///   `&mut Sexp::Nil' both work).  The elisp body writes bytes
    ///   `[0, 1)` and `[8, 16)` only; the remaining bytes are left
    ///   unmodified, so callers that read them must initialize first.
    pub unsafe fn truncate_int(arg0: *const Sexp, result_slot: *mut Sexp) -> *mut Sexp {
        nelisp_truncate_int(arg0, result_slot)
    }

    /// Doc 100 §100.D Stage 1 probes — thin safe wrappers around the
    /// 12 elisp-compiled jit/arith trampolines.  Used by
    /// `tests/elisp_cc_jit_arith_probe.rs' to assert every member of
    /// the `.o' archive resolves and computes the expected i64 result.
    /// `unified_fn_ptr` in `jit/bridge.rs` resolves the same symbols
    /// at runtime via the `arith_link' submodule, but those
    /// references alone get DCE'd in test-bin builds; surfacing the
    /// externs through `pub fn' here pins the link.
    pub fn jit_add2(a: i64, b: i64) -> i64 { unsafe { nelisp_jit_add2(a, b) } }
    pub fn jit_sub2(a: i64, b: i64) -> i64 { unsafe { nelisp_jit_sub2(a, b) } }
    pub fn jit_mul2(a: i64, b: i64) -> i64 { unsafe { nelisp_jit_mul2(a, b) } }
    pub fn jit_eq2(a: i64, b: i64) -> i64 { unsafe { nelisp_jit_eq2(a, b) } }
    pub fn jit_lt2(a: i64, b: i64) -> i64 { unsafe { nelisp_jit_lt2(a, b) } }
    pub fn jit_gt2(a: i64, b: i64) -> i64 { unsafe { nelisp_jit_gt2(a, b) } }
    pub fn jit_le2(a: i64, b: i64) -> i64 { unsafe { nelisp_jit_le2(a, b) } }
    pub fn jit_ge2(a: i64, b: i64) -> i64 { unsafe { nelisp_jit_ge2(a, b) } }
    pub fn jit_logior2(a: i64, b: i64) -> i64 { unsafe { nelisp_jit_logior2(a, b) } }
    pub fn jit_logand2(a: i64, b: i64) -> i64 { unsafe { nelisp_jit_logand2(a, b) } }
    pub fn jit_logxor2(a: i64, b: i64) -> i64 { unsafe { nelisp_jit_logxor2(a, b) } }
    pub fn jit_ash(n: i64, c: i64) -> i64 { unsafe { nelisp_jit_ash(n, c) } }

    /// Doc 110 §110.E.2.a probes — thin safe wrappers around the 4
    /// elisp-compiled `jit/float.rs' replacements (add / sub / mul /
    /// div).  Used by `tests/elisp_cc_jit_float_probe.rs' to assert
    /// every member of the `.o' archive resolves and computes the
    /// expected f64 result.  The comparison trampolines (eq_eps /
    /// lt / gt / le / ge) keep their Rust impl until §110.E.2.b.
    pub fn jit_float_add(a: f64, b: f64) -> f64 { unsafe { nl_jit_float_add(a, b) } }
    pub fn jit_float_sub(a: f64, b: f64) -> f64 { unsafe { nl_jit_float_sub(a, b) } }
    pub fn jit_float_mul(a: f64, b: f64) -> f64 { unsafe { nl_jit_float_mul(a, b) } }
    pub fn jit_float_div(a: f64, b: f64) -> f64 { unsafe { nl_jit_float_div(a, b) } }

    /// Doc 110 §110.C.2.a probes — thin safe wrappers around the 4
    /// elisp-compiled ordered comparison trampolines (lt / gt / le
    /// / ge).  Result is 0 or 1 as i64.  NaN inputs return 0 to
    /// match Rust's `<' / `>' / `<=' / `>=' semantics.  Used by
    /// `tests/elisp_cc_jit_float_probe.rs' to assert every member
    /// resolves and produces the expected boolean.
    pub fn jit_float_lt(a: f64, b: f64) -> i64 { unsafe { nl_jit_float_lt(a, b) } }
    pub fn jit_float_gt(a: f64, b: f64) -> i64 { unsafe { nl_jit_float_gt(a, b) } }
    pub fn jit_float_le(a: f64, b: f64) -> i64 { unsafe { nl_jit_float_le(a, b) } }
    pub fn jit_float_ge(a: f64, b: f64) -> i64 { unsafe { nl_jit_float_ge(a, b) } }
}
