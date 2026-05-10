//! Phase 7.1.6.c (Doc 28 §3.6.c) — arith trampolines, dlsym-exported.
//!
//! Pre-7.1.6.c this module hosted Cranelift IR builders that emitted
//! the 12 arith / cmp / bitwise primitives directly as Cranelift
//! instructions (= `iadd' / `isub' / `imul' / `icmp' / `bor' / `band'
//! / `bxor' / `ishl' / `sshr') wrapped by a `JitArith' fn-ptr struct
//! brought up at first-access by the unified JITModule.  Unlike the
//! cons (7.1.6.a.2) / access (7.1.6.b) clusters, arith had *no* Rust
//! trampoline body — the Cranelift IR was the implementation.
//!
//! Doc 81 Stage 81.4 + Phase 7.1.6.a.1 dlsym precursor (`6666e61')
//! shipped the elisp-side replacement infrastructure.  Per Doc 28
//! §3.6.c.2, the 12 arith primitives go through a *different* path
//! than the trampoline-based cons/access primitives: the nelisp-cc
//! backend (= `nelisp-cc-x86_64.el' / `-arm64.el') emits the host
//! machine-code `iadd' / `isub' / etc. directly via existing
//! `ssa-iadd' / `ssa-isub' / ... opcodes — no `:ssa-call-primitive'
//! trampoline shape needed because the operations map 1-to-1 to host
//! ISA instructions.
//!
//! Phase 7.1.6.c (this commit) deletes:
//!
//!   - `JitArith' / `ArithIds' fn-ptr structs.
//!   - `declare_binop' / `declare_ash' Cranelift IR builders.
//!   - `register_symbols' / `declare_funcs' / `collect_funcs' wiring
//!     (= `unified_jit()' no longer constructs an arith cluster JIT
//!     wrapper page).
//!
//! What stays (= the surface this module still owns post-7.1.6.c):
//!
//!   - 12 plain Rust trampolines `nl_jit_arith_*' that mirror the
//!     deleted Cranelift IR semantics 1-to-1 (= `wrapping_add' for
//!     `iadd', `wrapping_sub' for `isub', etc.).  Each is `#[no_mangle]
//!     pub unsafe extern "C"' so the dlsym bridge can resolve them at
//!     runtime.  Bodies match the Cranelift IR's `wrapping' contract
//!     (= overflow wraps both ways) and the `ash' bounds-check
//!     contract (= caller bounds-checks count ∈ [-62, +62] before
//!     invoking; out-of-range is UB at the dlsym call site, same as
//!     the Cranelift IR pre-7.1.6.c).
//!
//! `nelisp-jit-substrate.el' / `nelisp-jit-strategy.el' still call
//! `(nl-jit-call-i64-i64 "nelisp_jit_*" …)' which goes through
//! `bridge::unified_fn_ptr'.  Post-7.1.6.c those names resolve directly
//! to the `nl_jit_arith_*' trampolines — no Cranelift wrapper in
//! between (= one fewer indirection, same as cons / access takeover).
//! Compiled hot paths (= nelisp-cc output) skip the bridge entirely
//! and emit the host arithmetic instruction inline.
//!
//! The `-rdynamic' link flag in `.cargo/config.toml' (= already added
//! by Phase 7.1.6.a.2; arith trampolines just inherit) pushes the 12
//! `#[no_mangle]' symbols into the binary's dynamic symbol table so
//! `dlsym(RTLD_DEFAULT, ...)' can locate them at runtime.

// ---- 3 wrapping arithmetic ops --------------------------------------

/// `(- ADD2 A B) -> A + B' with wrapping i64 overflow semantics.
/// Phase 7.1.6.c dlsym-exported.
#[no_mangle]
pub unsafe extern "C" fn nl_jit_arith_add2(a: i64, b: i64) -> i64 {
    a.wrapping_add(b)
}

/// `(- SUB2 A B) -> A - B' with wrapping i64 overflow semantics.
/// Phase 7.1.6.c dlsym-exported.
#[no_mangle]
pub unsafe extern "C" fn nl_jit_arith_sub2(a: i64, b: i64) -> i64 {
    a.wrapping_sub(b)
}

/// `(- MUL2 A B) -> A * B' with wrapping i64 overflow semantics.
/// Phase 7.1.6.c dlsym-exported.
#[no_mangle]
pub unsafe extern "C" fn nl_jit_arith_mul2(a: i64, b: i64) -> i64 {
    a.wrapping_mul(b)
}

// ---- 5 signed integer comparisons → 0/1 i64 -------------------------

/// `(- NUM-EQ2 A B) -> 1 if A == B else 0' (signed i64 compare).
/// Phase 7.1.6.c dlsym-exported.
#[no_mangle]
pub unsafe extern "C" fn nl_jit_arith_eq2(a: i64, b: i64) -> i64 {
    (a == b) as i64
}

/// `(- NUM-LT2 A B) -> 1 if A < B else 0' (signed i64 compare).
/// Phase 7.1.6.c dlsym-exported.
#[no_mangle]
pub unsafe extern "C" fn nl_jit_arith_lt2(a: i64, b: i64) -> i64 {
    (a < b) as i64
}

/// `(- NUM-GT2 A B) -> 1 if A > B else 0' (signed i64 compare).
/// Phase 7.1.6.c dlsym-exported.
#[no_mangle]
pub unsafe extern "C" fn nl_jit_arith_gt2(a: i64, b: i64) -> i64 {
    (a > b) as i64
}

/// `(- NUM-LE2 A B) -> 1 if A <= B else 0' (signed i64 compare).
/// Phase 7.1.6.c dlsym-exported.
#[no_mangle]
pub unsafe extern "C" fn nl_jit_arith_le2(a: i64, b: i64) -> i64 {
    (a <= b) as i64
}

/// `(- NUM-GE2 A B) -> 1 if A >= B else 0' (signed i64 compare).
/// Phase 7.1.6.c dlsym-exported.
#[no_mangle]
pub unsafe extern "C" fn nl_jit_arith_ge2(a: i64, b: i64) -> i64 {
    (a >= b) as i64
}

// ---- 3 bitwise ops --------------------------------------------------

/// `(- LOGIOR2 A B) -> A | B'.  Phase 7.1.6.c dlsym-exported.
#[no_mangle]
pub unsafe extern "C" fn nl_jit_arith_logior2(a: i64, b: i64) -> i64 {
    a | b
}

/// `(- LOGAND2 A B) -> A & B'.  Phase 7.1.6.c dlsym-exported.
#[no_mangle]
pub unsafe extern "C" fn nl_jit_arith_logand2(a: i64, b: i64) -> i64 {
    a & b
}

/// `(- LOGXOR2 A B) -> A ^ B'.  Phase 7.1.6.c dlsym-exported.
#[no_mangle]
pub unsafe extern "C" fn nl_jit_arith_logxor2(a: i64, b: i64) -> i64 {
    a ^ b
}

// ---- ash --------------------------------------------------------------

/// `(ash N COUNT) -> N << COUNT' (positive count) or `N >> -COUNT'
/// (negative count, sign-extending).  Caller is responsible for
/// bounds-checking COUNT ∈ [-62, +62]; out-of-range counts produce
/// undefined behaviour (same contract as the deleted Cranelift IR
/// pre-7.1.6.c).  Phase 7.1.6.c dlsym-exported.
#[no_mangle]
pub unsafe extern "C" fn nl_jit_arith_ash(n: i64, count: i64) -> i64 {
    if count < 0 {
        // sshr by abs(count): sign-extend the upper bits.
        n >> (-count)
    } else {
        // ishl by count: shift in zeros from the right.
        n << count
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn jit_add2_compiles_and_runs() {
        assert_eq!(unsafe { nl_jit_arith_add2(1, 2) }, 3);
        assert_eq!(unsafe { nl_jit_arith_add2(0, 0) }, 0);
        assert_eq!(unsafe { nl_jit_arith_add2(-7, 10) }, 3);
        // Wrapping: i64::MAX + 1 = i64::MIN.
        assert_eq!(unsafe { nl_jit_arith_add2(i64::MAX, 1) }, i64::MIN);
    }

    #[test]
    fn jit_sub_mul() {
        assert_eq!(unsafe { nl_jit_arith_sub2(10, 3) }, 7);
        assert_eq!(unsafe { nl_jit_arith_sub2(0, 1) }, -1);
        assert_eq!(unsafe { nl_jit_arith_mul2(6, 7) }, 42);
        assert_eq!(unsafe { nl_jit_arith_mul2(-3, 4) }, -12);
    }

    #[test]
    fn jit_cmp_signed() {
        assert_eq!(unsafe { nl_jit_arith_eq2(5, 5) }, 1);
        assert_eq!(unsafe { nl_jit_arith_eq2(5, 4) }, 0);
        assert_eq!(unsafe { nl_jit_arith_lt2(3, 4) }, 1);
        assert_eq!(unsafe { nl_jit_arith_lt2(4, 3) }, 0);
        assert_eq!(unsafe { nl_jit_arith_lt2(-1, 1) }, 1);
        assert_eq!(unsafe { nl_jit_arith_gt2(4, 3) }, 1);
        assert_eq!(unsafe { nl_jit_arith_le2(3, 3) }, 1);
        assert_eq!(unsafe { nl_jit_arith_le2(4, 3) }, 0);
        assert_eq!(unsafe { nl_jit_arith_ge2(3, 3) }, 1);
        assert_eq!(unsafe { nl_jit_arith_ge2(2, 3) }, 0);
    }

    #[test]
    fn jit_bitwise() {
        assert_eq!(unsafe { nl_jit_arith_logior2(0b1100, 0b0011) }, 0b1111);
        assert_eq!(unsafe { nl_jit_arith_logand2(0b1110, 0b0111) }, 0b0110);
        assert_eq!(unsafe { nl_jit_arith_logxor2(0b1100, 0b1010) }, 0b0110);
    }

    #[test]
    fn jit_ash_left_shift() {
        // count > 0 → ishl
        assert_eq!(unsafe { nl_jit_arith_ash(1, 3) }, 8);
        assert_eq!(unsafe { nl_jit_arith_ash(0xFF, 4) }, 0xFF0);
        // count = 0 → identity (ishl by 0)
        assert_eq!(unsafe { nl_jit_arith_ash(42, 0) }, 42);
        assert_eq!(unsafe { nl_jit_arith_ash(-42, 0) }, -42);
        // negatives shift left preserves sign-extension at top bits
        assert_eq!(unsafe { nl_jit_arith_ash(-1, 1) }, -2);
    }

    #[test]
    fn jit_ash_right_shift_signed() {
        // count < 0 → sshr by abs(count); sign bit is replicated.
        assert_eq!(unsafe { nl_jit_arith_ash(8, -3) }, 1);
        assert_eq!(unsafe { nl_jit_arith_ash(0xFF0, -4) }, 0xFF);
        // -8 >> 3 = -1 (all sign bits shifted in)
        assert_eq!(unsafe { nl_jit_arith_ash(-8, -3) }, -1);
        assert_eq!(unsafe { nl_jit_arith_ash(-100, -1) }, -50);
        // -1 >> 1 = -1 (every bit set, sign-extends to all-ones)
        assert_eq!(unsafe { nl_jit_arith_ash(-1, -1) }, -1);
    }
}
