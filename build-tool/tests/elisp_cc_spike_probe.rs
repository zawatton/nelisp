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
