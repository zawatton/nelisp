//! Phase 7.1.6 cluster takeover (Doc 28 §3.6 COMPLETE) — arith trampolines,
//! dlsym-exported.
//!
//! The 12 plain Rust trampolines `nl_jit_arith_*' mirror the
//! pre-takeover Cranelift IR semantics 1-to-1 (= `wrapping_add' for
//! `iadd', `wrapping_sub' for `isub', etc.).  Each is `#[no_mangle] pub
//! unsafe extern "C"' so the dlsym bridge resolves them at runtime.
//! Bodies match the IR's `wrapping' overflow contract and the `ash'
//! bounds-check contract (= caller bounds-checks count ∈ [-62, +62];
//! out-of-range is UB at the call site).
//!
//! Two callers reach the trampolines: (1) `nelisp-jit-substrate.el' /
//! `-strategy.el' via `bridge::unified_fn_ptr's name → fn-ptr table
//! (`(nl-jit-call-i64-i64 "nelisp_jit_*" …)'), and (2) compiled hot
//! paths from nelisp-cc that emit the host arithmetic instruction
//! inline via `ssa-iadd' / `ssa-isub' / etc., bypassing the bridge.

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
