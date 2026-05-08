//! Phase 5 Doc 62 §4.2 — JIT lowering bench harness.
//!
//! Validates the dispatcher-bypass speedup hypothesis from Doc 62 §1.2:
//! by replacing `dispatch(name, ...)' (= name match-arm) with a direct
//! lowered-function call from the eval loop's `try_lower' hook, hot
//! primitives should run noticeably faster.  Doc 62 §4.2 sets the bar
//! at 5x〜20x — this module measures the actual ratios.
//!
//! Run with:
//! ```sh
//! cargo test --release -p nelisp-build-tool --lib \
//!   -- --ignored --nocapture jit::bench
//! ```
//!
//! Each bench primitive runs N iterations through the JIT-lowered
//! function and the dispatcher fallback, then reports the wallclock
//! ratio.  All benches are `#[ignore]'d so the regular `cargo test'
//! gate does not trigger them — bench timing is noisy and not worth
//! the per-PR cost.
//!
//! Methodology notes:
//! - We measure the inner loop only (= bypass eval overhead so the
//!   delta isolates the JIT lookup vs dispatcher arm).  Both paths
//!   end up in the same `bi_*' helpers for non-inlined primitives,
//!   so the delta = `try_lower' HashMap-get-and-call vs `dispatch'
//!   match-arm-and-call.
//! - For `length' / `car' / `cdr' / `eq' there are inline fast paths
//!   that skip the helper entirely on the Nil / tag-byte cases — the
//!   bench reports both (= "hot" for inline-friendly inputs, "cold"
//!   for inputs that fall through to the helper).
//! - Warm-up iterations prime the JIT module compile cache (= the
//!   first call triggers `OnceLock' init) and CPU branch predictor.

use std::time::{Duration, Instant};

use crate::eval::env::Env;
use crate::eval::sexp::Sexp;

use super::lower_entries;

/// Iteration counts per Doc 62 §4.2.  `add2' is cheap so 1M; the
/// others 100k.
const ITERS_ADD2: u64 = 1_000_000;
const ITERS_SYSCALL: u64 = 100_000;
const ITERS_CONS_CYCLE: u64 = 100_000;
const ITERS_INLINE_TAG: u64 = 1_000_000;
const WARMUP: u64 = 10_000;

fn bench_jit(name: &str, iters: u64, args: &[Sexp], env: &mut Env) -> Duration {
    let entry = *lower_entries()
        .get(name)
        .unwrap_or_else(|| panic!("`{}' not in JIT registry", name));
    for _ in 0..WARMUP {
        let _ = entry(args, env);
    }
    let start = Instant::now();
    for _ in 0..iters {
        let _ = entry(args, env);
    }
    start.elapsed()
}

fn bench_dispatch(name: &str, iters: u64, args: &[Sexp], env: &mut Env) -> Duration {
    for _ in 0..WARMUP {
        let _ = crate::eval::builtins::dispatch(name, args, env);
    }
    let start = Instant::now();
    for _ in 0..iters {
        let _ = crate::eval::builtins::dispatch(name, args, env);
    }
    start.elapsed()
}

/// Doc 77 Stage 1.D (2026-05-09): probe `dispatch' once before
/// running the bench loop.  When the primitive is **JIT-only** (=
/// its `bi_*' arm was deleted in Doc 62 Stage C), `dispatch' returns
/// `Err(UnboundFunction)' and the bench's "dispatch ns/op" number
/// would just measure name-lookup miss in the dispatcher's `match'
/// — not the primitive's true dispatch cost.  In that case we
/// return `None' so the caller reports "dispatch = N/A" instead of
/// emitting a misleading JIT-vs-miss ratio (or panicking when the
/// caller is `.unwrap()'-happy, as `bench_car_cdr_cons_cycle' was
/// pre-Doc-77).
fn bench_dispatch_or_none(
    name: &str,
    iters: u64,
    args: &[Sexp],
    env: &mut Env,
) -> Option<Duration> {
    if crate::eval::builtins::dispatch(name, args, env).is_err() {
        return None;
    }
    Some(bench_dispatch(name, iters, args, env))
}

fn report(label: &str, iters: u64, jit: Duration, disp: Duration) -> f64 {
    let speedup = disp.as_secs_f64() / jit.as_secs_f64();
    let jit_per = jit.as_nanos() as f64 / iters as f64;
    let disp_per = disp.as_nanos() as f64 / iters as f64;
    println!(
        "[bench] {:<28} {:>10} iters | jit {:>9.2}ns/op | dispatch {:>9.2}ns/op | speedup {:.2}x",
        label, iters, jit_per, disp_per, speedup
    );
    speedup
}

/// Variant of `report' for JIT-only primitives where dispatch
/// comparison is meaningless.  Prints just the JIT timing.
fn report_jit_only(label: &str, iters: u64, jit: Duration) {
    let jit_per = jit.as_nanos() as f64 / iters as f64;
    println!(
        "[bench] {:<28} {:>10} iters | jit {:>9.2}ns/op | dispatch       N/A    (JIT-only primitive)",
        label, iters, jit_per
    );
}

/// Helper combining `report' + `report_jit_only': when `dispatch_opt'
/// is `Some(d)' emit the side-by-side line and assert the speedup
/// floor; otherwise emit the JIT-only line and skip the assertion
/// (= dispatch is uncomparable for JIT-only primitives).
fn report_or_jit_only(
    label: &str,
    iters: u64,
    jit: Duration,
    dispatch_opt: Option<Duration>,
) -> Option<f64> {
    match dispatch_opt {
        Some(d) => Some(report(label, iters, jit, d)),
        None => {
            report_jit_only(label, iters, jit);
            None
        }
    }
}

#[test]
#[ignore]
fn bench_add2() {
    // `nelisp--add2' is JIT-only (= `bi_add2' was deleted in Doc 62
    // Stage C-Phase1).  Pre-Doc-77 this still printed a "dispatch"
    // ns/op number, but it was measuring `dispatch' name-lookup miss
    // — not a real comparison.  Use `_or_none' to suppress the
    // misleading number.
    let mut env = Env::new_global();
    let args = vec![Sexp::Int(7), Sexp::Int(8)];
    let jit = bench_jit("nelisp--add2", ITERS_ADD2, &args, &mut env);
    let disp = bench_dispatch_or_none("nelisp--add2", ITERS_ADD2, &args, &mut env);
    if let Some(s) = report_or_jit_only("add2 (Int+Int)", ITERS_ADD2, jit, disp) {
        assert!(
            s >= 0.9,
            "JIT path slower than dispatch (= {:.2}x) — likely a regression",
            s
        );
    }
}

#[test]
#[ignore]
fn bench_syscall_getpid() {
    // `nelisp--syscall' has a `bi_syscall' fallback in `dispatch'
    // (= it is one of the two `ENV_TOGGLEABLE_ENTRIES' in Doc 77
    // Stage 1.C), so the JIT-vs-dispatch comparison is meaningful.
    let mut env = Env::new_global();
    // SYS_getpid = 39 on Linux x86_64.  No args after the syscall nr.
    let args = vec![Sexp::Int(39)];
    let jit = bench_jit("nelisp--syscall", ITERS_SYSCALL, &args, &mut env);
    let disp = bench_dispatch("nelisp--syscall", ITERS_SYSCALL, &args, &mut env);
    let speedup = report("syscall(getpid)", ITERS_SYSCALL, jit, disp);
    // Doc 77 §0 baseline = 0.92-0.97x for `syscall(getpid)' since the
    // libc syscall (~130ns) dwarfs the JIT trampoline boundary cost
    // (~3-5ns), so JIT is ~always slightly behind dispatch.  The
    // assert is a *regression* gate, not a perf goal — relaxed to
    // 0.80x to absorb run-to-run jitter on ITERS=100k while still
    // catching catastrophic slowdowns from a future change.
    assert!(speedup >= 0.80, "JIT slower than dispatch — regression");
}

#[test]
#[ignore]
fn bench_car_cdr_cons_cycle() {
    // Build a 100k-deep cons chain via `cons', then walk it via
    // `car' / `cdr'.  Reuses one Env so the JIT module is hot.
    let mut env = Env::new_global();

    // `cons' bench: build short bounded chains repeatedly.  Each
    // outer iter resets `acc' to Nil and runs an inner 64-deep build
    // — keeps the cons graph shallow so the recursive `Drop' on
    // `Sexp::Cons(Rc<...>, Rc<RefCell<Sexp>>)' does not blow the
    // stack at iteration boundaries.  Total cons calls = ITERS * 64.
    const CHAIN_DEPTH: u64 = 64;
    let cons_iters = ITERS_CONS_CYCLE * CHAIN_DEPTH;
    // Doc 77 Stage 1.D (2026-05-09): `cons' is JIT-only, so the
    // dispatch path can never accumulate `acc' (= dispatch returns
    // Err which used to crash with `.unwrap()').  Skip the dispatch
    // bench entirely and report jit-only timing.
    let cons_jit = {
        let entry = *lower_entries().get("cons").expect("cons lowered");
        for _ in 0..WARMUP {
            let _ = entry(&[Sexp::Int(1), Sexp::Nil], &mut env);
        }
        let start = Instant::now();
        for _ in 0..ITERS_CONS_CYCLE {
            let mut acc = Sexp::Nil;
            for _ in 0..CHAIN_DEPTH {
                acc = entry(&[Sexp::Int(1), acc], &mut env).unwrap();
            }
            // acc dropped here, depth = CHAIN_DEPTH (= safe).
        }
        start.elapsed()
    };
    report_jit_only("cons (build 64-deep chain)", cons_iters, cons_jit);

    // Build a fixed-length chain to bench car/cdr against.
    let mut chain = Sexp::Nil;
    for i in 0..1_000 {
        chain = Sexp::cons(Sexp::Int(i), chain);
    }

    // car bench (= always hits Cons fast path).  car is JIT-only so
    // dispatch returns Err — `_or_none' suppresses the misleading
    // dispatch number that pre-Doc-77 was just measuring lookup miss.
    let car_args = vec![chain.clone()];
    let car_jit = bench_jit("car", ITERS_CONS_CYCLE, &car_args, &mut env);
    let car_disp = bench_dispatch_or_none("car", ITERS_CONS_CYCLE, &car_args, &mut env);
    report_or_jit_only("car (Cons head)", ITERS_CONS_CYCLE, car_jit, car_disp);

    // cdr bench (= JIT-only, same as car).
    let cdr_args = vec![chain.clone()];
    let cdr_jit = bench_jit("cdr", ITERS_CONS_CYCLE, &cdr_args, &mut env);
    let cdr_disp = bench_dispatch_or_none("cdr", ITERS_CONS_CYCLE, &cdr_args, &mut env);
    report_or_jit_only("cdr (Cons tail)", ITERS_CONS_CYCLE, cdr_jit, cdr_disp);
}

#[test]
#[ignore]
fn bench_inline_tag_paths() {
    // `(car nil)' / `(cdr nil)' / `(length nil)' / `(eq nil nil)' all
    // hit the inline fast path that skips the helper entirely.  This
    // is the case where Stage 5.5/5.4/5.3 inline emit should give the
    // largest speedup.
    let mut env = Env::new_global();

    // Doc 77 Stage 1.D: car / cdr / length / eq are all JIT-only,
    // so dispatch returns Err and the comparison number is bogus.
    // Use `_or_none' to print "dispatch = N/A" cleanly.
    let nil = vec![Sexp::Nil];
    let car_nil_jit = bench_jit("car", ITERS_INLINE_TAG, &nil, &mut env);
    let car_nil_disp = bench_dispatch_or_none("car", ITERS_INLINE_TAG, &nil, &mut env);
    report_or_jit_only("car nil (inline)", ITERS_INLINE_TAG, car_nil_jit, car_nil_disp);

    let cdr_nil_jit = bench_jit("cdr", ITERS_INLINE_TAG, &nil, &mut env);
    let cdr_nil_disp = bench_dispatch_or_none("cdr", ITERS_INLINE_TAG, &nil, &mut env);
    report_or_jit_only("cdr nil (inline)", ITERS_INLINE_TAG, cdr_nil_jit, cdr_nil_disp);

    let length_nil_jit = bench_jit("length", ITERS_INLINE_TAG, &nil, &mut env);
    let length_nil_disp = bench_dispatch_or_none("length", ITERS_INLINE_TAG, &nil, &mut env);
    report_or_jit_only(
        "length nil (inline)",
        ITERS_INLINE_TAG,
        length_nil_jit,
        length_nil_disp,
    );

    let eq_int_args = vec![Sexp::Int(42), Sexp::Int(42)];
    let eq_jit = bench_jit("eq", ITERS_INLINE_TAG, &eq_int_args, &mut env);
    let eq_disp = bench_dispatch_or_none("eq", ITERS_INLINE_TAG, &eq_int_args, &mut env);
    report_or_jit_only("eq Int+Int (inline)", ITERS_INLINE_TAG, eq_jit, eq_disp);
}
