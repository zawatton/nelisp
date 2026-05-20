#![cfg(all(target_os = "linux", target_arch = "x86_64"))]

use nelisp_build_tool::eval::sexp::{Sexp, SEXP_TAG_INT};

#[test]
fn truncate_int_signed_extremes_round_trip() {
    // i64::MIN itself overflows on 2's-complement negation in some
    // intermediate paths, so use `i64::MIN + 1' as the lower bound
    // boundary case — that's already a stricter check than v0 needs.
    let cases: &[i64] = &[
        i64::MIN + 1,
        -1_000_000_000_000,
        -42,
        -1,
        0,
        1,
        42,
        1_000_000_000_000,
        i64::MAX - 1,
        i64::MAX,
    ];
    for &n in cases {
        let input = Sexp::Int(n);
        let mut slot: Sexp = Sexp::Nil;
        let returned = unsafe {
            nelisp_build_tool::elisp_cc_spike::truncate_int(
                &input as *const Sexp,
                &mut slot as *mut Sexp,
            )
        };
        assert_eq!(
            returned, &mut slot as *mut Sexp,
            "truncate_int({n}): expected returned slot pointer to equal caller-provided slot",
        );
        // Pattern-match Sexp::Int from the slot.  Each value must be
        // the identity (= same payload).
        match &slot {
            Sexp::Int(got) => assert_eq!(*got, n, "truncate_int({n}) round-trip"),
            other => panic!("truncate_int({n}) wrote a non-Int Sexp: {other:?}"),
        }
    }
}

#[test]
fn truncate_int_writes_correct_tag_byte() {
    // Inspect the raw byte at offset 0 of the slot to confirm the elisp
    // body wrote `SEXP_TAG_INT' (= 2), not some other discriminant.
    // Reading via `*const u8' is sound for `#[repr(C, u8)]` enums.
    let input = Sexp::Int(123);
    let mut slot: Sexp = Sexp::Nil;
    unsafe {
        nelisp_build_tool::elisp_cc_spike::truncate_int(
            &input as *const Sexp,
            &mut slot as *mut Sexp,
        );
    }
    let tag = unsafe { *(&slot as *const Sexp as *const u8) };
    assert_eq!(
        tag, SEXP_TAG_INT,
        "elisp .o wrote tag byte {tag} but expected SEXP_TAG_INT = {SEXP_TAG_INT}",
    );
}

#[test]
fn truncate_int_writes_correct_payload_bytes() {
    // Inspect the 8 bytes at offset 8 (= `nelisp-sexp--offset-payload')
    // as a little-endian i64 to confirm the elisp body wrote the same
    // payload it read from the input.
    let input = Sexp::Int(0x0123_4567_89AB_CDEFi64);
    let mut slot: Sexp = Sexp::Nil;
    unsafe {
        nelisp_build_tool::elisp_cc_spike::truncate_int(
            &input as *const Sexp,
            &mut slot as *mut Sexp,
        );
    }
    let payload_ptr = unsafe { (&slot as *const Sexp as *const u8).add(8) as *const i64 };
    let got = unsafe { *payload_ptr };
    assert_eq!(
        got, 0x0123_4567_89AB_CDEFi64,
        "elisp .o wrote payload bytes 0x{got:016X}",
    );
}

#[test]
fn truncate_int_eight_value_spread_matches_input_clone() {
    // Doc 99 §99.C used 0..=20 to cover its full domain.  For Int
    // identity the magnitude space is huge, so cover a representative
    // exponential spread plus the small-number band heavily used by
    // production callers.
    let cases: &[i64] = &[
        0,
        1,
        -1,
        7,
        -7,
        100,
        1_000,
        1_000_000_000,
        -2_147_483_648,
        9_223_372_036_854_775_807,
    ];
    for &n in cases {
        let input = Sexp::Int(n);
        let mut slot: Sexp = Sexp::Nil;
        unsafe {
            nelisp_build_tool::elisp_cc_spike::truncate_int(
                &input as *const Sexp,
                &mut slot as *mut Sexp,
            );
        }
        // Compare against a fresh `Sexp::Int(n)' (= what the swapped-
        // out Rust line `Ok(Sexp::Int(*n))' used to produce).
        assert_eq!(slot, Sexp::Int(n), "elisp .o output mismatch for {n}");
    }
}
