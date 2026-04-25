//! Phase 7.1 完遂 gate 3-axis bench — Rust-side scaffold.
//!
//! Doc 28 v2 §5.1 LOCKED-2026-04-25-v2.  The *primary* 3-axis bench
//! lives on the Lisp side (`bench/nelisp-cc-bench-actual.el` driven by
//! `make bench-actual`) because the gate compares NeLisp native code
//! exec time vs the Emacs bytecode VM, both of which are Lisp-runtime
//! quantities.  This Rust harness is a *secondary* baseline: it
//! measures the in-process FFI bridge's pure overhead — the cost of
//! mmap'ing a JIT page, copying machine code, transitioning to RX,
//! calling the bytes as `extern "C" fn() -> i64`, and unmapping.
//!
//! Why this matters for §5.2:
//!
//! The Lisp-side bench's `:native-elapsed` includes both the FFI
//! dispatch overhead AND the actual generated-code execution.  When
//! Phase 7.5 callee resolution lands, the dispatch overhead becomes
//! the dominant fixed cost on small benches like fact-iter (~500 µs
//! target, ~10 µs FFI overhead = 2% drift).  This Rust harness
//! quantifies that overhead independently so the §5.2 verification
//! report can subtract it (or document why it doesn't).
//!
//! Usage:
//!
//!   cd nelisp-runtime
//!   cargo bench --bench nelisp_cc_bench
//!
//! N=10 runs of each tight-loop probe; the median is what we report.
//!
//! Note: criterion is intentionally *not* in the dependency tree
//! (Doc 27 §3 7.0 keeps `nelisp-runtime/Cargo.toml` to libc only).
//! This file uses `std::time::Instant` for a zero-dep harness — the
//! reported numbers are mean-based, which is sufficient for the
//! "FFI overhead is small relative to bench targets" sanity check.

#![cfg(not(target_os = "windows"))] // mmap is POSIX

use std::time::Instant;

/// Number of iterations per probe.  10 is the Doc 28 v2 §5.2
/// reference-host pinning.
const ITERATIONS: usize = 10;

/// `xor rax, rax; ret` — smallest portable Linux-x86_64 payload for
/// the FFI bridge round-trip overhead probe.
const PROBE_BYTES: [u8; 4] = [0x48, 0x31, 0xC0, 0xC3];

/// `mov eax, 42; ret` — proves the rax round-trip carries a literal.
const PROBE_BYTES_42: [u8; 6] = [0xB8, 0x2A, 0x00, 0x00, 0x00, 0xC3];

/// Drive PROBE_BYTES through mmap_jit + mprotect + call + munmap once
/// and return the elapsed time in nanoseconds.
fn measure_one_round_trip(bytes: &[u8]) -> u128 {
    let start = Instant::now();
    unsafe {
        let len = bytes.len();
        let mapped_len = page_align_up(len, 4096);
        let prot_rw = nelisp_runtime::NELISP_PROT_READ | nelisp_runtime::NELISP_PROT_WRITE;
        let flags = nelisp_runtime::NELISP_MAP_PRIVATE
            | nelisp_runtime::NELISP_MAP_ANONYMOUS
            | nelisp_runtime::NELISP_MAP_JIT;
        let page = nelisp_runtime::nelisp_syscall_mmap_jit(
            std::ptr::null_mut(),
            mapped_len,
            prot_rw,
            flags,
            -1,
            0,
        );
        if page.is_null() || page as isize == -1 {
            // mmap failed — return an obvious sentinel.  The bench
            // surface is informational; we don't panic on a CI host
            // without overcommit budget.
            return u128::MAX;
        }
        nelisp_runtime::nelisp_syscall_jit_write_protect(0);
        std::ptr::copy_nonoverlapping(bytes.as_ptr(), page, len);
        nelisp_runtime::nelisp_syscall_jit_write_protect(1);
        let rc = nelisp_runtime::nelisp_syscall_mprotect(
            page,
            mapped_len,
            nelisp_runtime::NELISP_PROT_READ | nelisp_runtime::NELISP_PROT_EXEC,
        );
        if rc != 0 {
            nelisp_runtime::nelisp_syscall_munmap(page, mapped_len);
            return u128::MAX;
        }
        nelisp_runtime::nelisp_syscall_clear_icache(page, page.add(len));
        let entry: extern "C" fn() -> i64 = std::mem::transmute(page);
        let _result = entry();
        nelisp_runtime::nelisp_syscall_munmap(page, mapped_len);
    }
    start.elapsed().as_nanos()
}

fn page_align_up(n: usize, page: usize) -> usize {
    ((n + page - 1) / page) * page
}

/// Drive `f` ITERATIONS times and return (mean_ns, min_ns, max_ns).
fn timing_summary<F: Fn() -> u128>(f: F) -> (f64, u128, u128) {
    let mut samples: Vec<u128> = Vec::with_capacity(ITERATIONS);
    // Warm: run twice and discard so any first-call mmap setup or
    // page-cache miss does not skew the mean.
    f();
    f();
    for _ in 0..ITERATIONS {
        samples.push(f());
    }
    let total: u128 = samples.iter().sum();
    let mean = total as f64 / ITERATIONS as f64;
    let min = *samples.iter().min().unwrap_or(&0);
    let max = *samples.iter().max().unwrap_or(&0);
    (mean, min, max)
}

fn ffi_overhead_xor_rax() {
    let (mean_ns, min_ns, max_ns) = timing_summary(|| measure_one_round_trip(&PROBE_BYTES));
    println!(
        "ffi_overhead_xor_rax (N={}): mean={:.1}ns min={}ns max={}ns ({:.2}µs/call)",
        ITERATIONS,
        mean_ns,
        min_ns,
        max_ns,
        mean_ns / 1_000.0
    );
}

fn ffi_overhead_mov_eax_42() {
    let (mean_ns, min_ns, max_ns) = timing_summary(|| measure_one_round_trip(&PROBE_BYTES_42));
    println!(
        "ffi_overhead_mov_eax_42 (N={}): mean={:.1}ns min={}ns max={}ns ({:.2}µs/call)",
        ITERATIONS,
        mean_ns,
        min_ns,
        max_ns,
        mean_ns / 1_000.0
    );
}

fn main() {
    println!("Phase 7.1 FFI bridge overhead bench (Doc 28 v2 §5.1)");
    println!("Reference host: pinned (=current dev machine)");
    println!();
    ffi_overhead_xor_rax();
    ffi_overhead_mov_eax_42();
    println!();
    println!("Note: the *primary* 3-axis §5.2 gate (fib(30) 30x / fact-iter");
    println!("20x / alloc-heavy 5x) lives on the Lisp side — `make bench-actual'");
    println!("is the gate runner.  This binary only quantifies the FFI dispatch");
    println!("overhead so the §5.2 report can subtract it.");
}
