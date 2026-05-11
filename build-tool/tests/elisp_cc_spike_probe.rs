//! Doc 99 §99.B spike probe — proves the elisp → ET_REL .o → static
//! archive → cargo link chain terminates in a working C-callable symbol.
//!
//! When this test passes, every step of the §99.B pipeline has worked:
//!
//!   1. `scripts/compile-elisp-objects.el' parsed
//!      `lisp/nelisp-cc-spike-noop.el' and emitted a valid ET_REL
//!      `.o' via `nelisp-phase47-compile-to-object'.
//!   2. `build.rs' wrapped that `.o' into `libnelisp_elisp_spike.a'
//!      via `ar rcs' and told cargo to link the archive.
//!   3. The linker resolved the `extern "C" fn nelisp_spike_noop' in
//!      `elisp_cc_spike::probe' against the archive's STT_FUNC entry.
//!   4. The function body — emitted purely by elisp + Phase 47 +
//!      §99.A ET_REL writer — returned the integer literal `42'
//!      through the SysV AMD64 ABI return register.

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn nelisp_spike_noop_returns_42() {
    let got = nelisp_build_tool::elisp_cc_spike::probe();
    assert_eq!(got, 42, "Doc 99 §99.B spike round-trip mismatch");
}

#[cfg(not(all(target_os = "linux", target_arch = "x86_64")))]
#[test]
fn nelisp_spike_noop_skipped_on_non_linux_x86_64() {
    // §99.B spike is x86_64-Linux only; arm64 is a Stage 99.A
    // follow-up.  Keep one test compiled per target so `cargo test'
    // reports a deterministic count.
    eprintln!("Doc 99 §99.B probe skipped: only x86_64-linux supported in v1");
}
