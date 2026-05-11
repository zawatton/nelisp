//! Doc 99 §99.C probe — recursive i64 factorial implemented in elisp,
//! validated end-to-end against a Rust reference implementation.
//!
//! This is the smallest "real swap" pattern: the algorithmic body
//! lives only in `lisp/nelisp-cc-fact-i64.el', compiled to a `.o' by
//! the Phase 47 chain and linked into the binary via §99.B's wiring.
//! The Rust `bi_nl_fact_i64' dispatch arm is a Sexp-unwrap / range-
//! check / Sexp-wrap shim — no Rust copy of the computation exists in
//! production.
//!
//! The cross-impl check below has a *test-only* Rust reference
//! `rust_fact_i64_reference' so we can prove the elisp result is
//! correct (= matches the canonical factorial sequence).  This
//! reference is not used in production code paths.

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
fn rust_fact_i64_reference(n: i64) -> i64 {
    if n <= 1 {
        1
    } else {
        n * rust_fact_i64_reference(n - 1)
    }
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn nelisp_fact_i64_matches_rust_reference() {
    for n in 0..=20i64 {
        let elisp = nelisp_build_tool::elisp_cc_spike::fact_i64(n);
        let rust = rust_fact_i64_reference(n);
        assert_eq!(
            elisp, rust,
            "Doc 99 §99.C: fact_i64({}) elisp = {} but rust = {}",
            n, elisp, rust
        );
    }
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn nelisp_fact_i64_known_values() {
    // Spot-check a handful of canonical factorial values so a future
    // regression (e.g. ABI change, label-handling drift) shows up as
    // a specific-value mismatch instead of just an `assert_eq' line.
    let cases: &[(i64, i64)] = &[
        (0, 1),
        (1, 1),
        (5, 120),
        (10, 3_628_800),
        (12, 479_001_600),
        (20, 2_432_902_008_176_640_000),
    ];
    for &(n, expected) in cases {
        let got = nelisp_build_tool::elisp_cc_spike::fact_i64(n);
        assert_eq!(got, expected, "fact_i64({}) returned {}", n, got);
    }
}

#[cfg(not(all(target_os = "linux", target_arch = "x86_64")))]
#[test]
fn nelisp_fact_i64_skipped_on_non_linux_x86_64() {
    eprintln!("Doc 99 §99.C probe skipped: only x86_64-linux supported in v1");
}
