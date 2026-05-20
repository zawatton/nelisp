#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
use nelisp_build_tool::elisp_cc_spike::{
    jit_add2, jit_ash, jit_eq2, jit_ge2, jit_gt2, jit_le2, jit_logand2, jit_logior2, jit_logxor2,
    jit_lt2, jit_mul2, jit_sub2,
};

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn arith_add_sub_mul() {
    assert_eq!(jit_add2(2, 3), 5);
    assert_eq!(jit_add2(-7, 10), 3);
    // Wrapping: x86_64 ADD already wraps 2's-complement so this
    // matches the Rust trampoline's `wrapping_add' contract.
    assert_eq!(jit_add2(i64::MAX, 1), i64::MIN);

    assert_eq!(jit_sub2(10, 3), 7);
    assert_eq!(jit_sub2(0, 1), -1);

    assert_eq!(jit_mul2(6, 7), 42);
    assert_eq!(jit_mul2(-3, 4), -12);
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn arith_cmp_signed_returns_0_or_1() {
    assert_eq!(jit_eq2(5, 5), 1);
    assert_eq!(jit_eq2(5, 4), 0);

    assert_eq!(jit_lt2(3, 4), 1);
    assert_eq!(jit_lt2(4, 3), 0);
    assert_eq!(jit_lt2(-1, 1), 1);

    assert_eq!(jit_gt2(4, 3), 1);
    assert_eq!(jit_gt2(3, 4), 0);

    assert_eq!(jit_le2(3, 3), 1);
    assert_eq!(jit_le2(4, 3), 0);

    assert_eq!(jit_ge2(3, 3), 1);
    assert_eq!(jit_ge2(2, 3), 0);
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn arith_bitwise() {
    assert_eq!(jit_logior2(0b1100, 0b0011), 0b1111);
    assert_eq!(jit_logand2(0b1110, 0b0111), 0b0110);
    assert_eq!(jit_logxor2(0b1100, 0b1010), 0b0110);
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn arith_ash_left_shift() {
    // count > 0 -> ishl (`shl rax, cl' after rcx <- count)
    assert_eq!(jit_ash(1, 3), 8);
    assert_eq!(jit_ash(0xFF, 4), 0xFF0);
    // count = 0 -> identity (shl by 0)
    assert_eq!(jit_ash(42, 0), 42);
    assert_eq!(jit_ash(-42, 0), -42);
    // negatives shift left preserves sign-extension at top bits
    assert_eq!(jit_ash(-1, 1), -2);
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn arith_ash_right_shift_signed() {
    // count < 0 -> sshr by abs(count); sign bit replicates
    assert_eq!(jit_ash(8, -3), 1);
    assert_eq!(jit_ash(0xFF0, -4), 0xFF);
    // -8 >> 3 = -1 (all sign bits shifted in)
    assert_eq!(jit_ash(-8, -3), -1);
    assert_eq!(jit_ash(-100, -1), -50);
    // -1 >> 1 = -1 (every bit set, sign-extends to all-ones)
    assert_eq!(jit_ash(-1, -1), -1);
}

#[cfg(not(all(target_os = "linux", target_arch = "x86_64")))]
#[test]
fn elisp_cc_jit_arith_skipped_on_non_linux_x86_64() {
    eprintln!(
        "Doc 100 §100.D Stage 1 probe skipped: only x86_64-linux \
         emits the elisp arith .o (Stage 2 will lift the restriction)"
    );
}
