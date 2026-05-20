#![cfg(all(target_os = "linux", target_arch = "x86_64"))]

use nelisp_build_tool::eval::sexp::Sexp;

// ---- helpers ----

fn make_empty_mut_str(slot: &mut Sexp, cap: i64) {
    let slot_ptr = slot as *mut Sexp;
    let returned = unsafe { nelisp_build_tool::elisp_cc_spike::mut_str_make_empty(slot_ptr, cap) };
    assert_eq!(
        returned, slot_ptr,
        "mut-str-make-empty must return the caller-provided slot pointer"
    );
    // Sanity check on the tag — the slot should now hold a
    // `Sexp::MutStr(_)` regardless of the cap value.
    assert!(
        matches!(slot, Sexp::MutStr(_)),
        "mut-str-make-empty must populate slot with Sexp::MutStr, got {:?}",
        slot
    );
}

fn push_byte(ptr: &mut Sexp, byte: i64) {
    let p = ptr as *mut Sexp;
    let rc = unsafe { nelisp_build_tool::elisp_cc_spike::mut_str_push_byte(p, byte) };
    assert_eq!(rc, 1, "mut-str-push-byte must return rax = 1 sentinel");
}

fn push_codepoint(ptr: &mut Sexp, cp: i64) {
    let p = ptr as *mut Sexp;
    let rc = unsafe { nelisp_build_tool::elisp_cc_spike::mut_str_push_codepoint(p, cp) };
    assert_eq!(rc, 1, "mut-str-push-codepoint must return rax = 1 sentinel");
}

fn mut_str_byte_len(ptr: &Sexp) -> i64 {
    let p = ptr as *const Sexp;
    unsafe { nelisp_build_tool::elisp_cc_spike::mut_str_len(p) }
}

fn finalize_into(src: &Sexp, dst: &mut Sexp) {
    let src_p = src as *const Sexp;
    let dst_p = dst as *mut Sexp;
    let returned = unsafe { nelisp_build_tool::elisp_cc_spike::mut_str_finalize(src_p, dst_p) };
    assert_eq!(
        returned, dst_p,
        "mut-str-finalize must return the caller-provided slot pointer"
    );
}

// ---- Case 1: empty mut-str + finalize → empty Str ----

#[test]
fn mut_str_empty_then_finalize_yields_empty_str() {
    let mut mut_slot = Sexp::Nil;
    let mut out = Sexp::Nil;
    make_empty_mut_str(&mut mut_slot, 0);
    assert_eq!(
        mut_str_byte_len(&mut_slot),
        0,
        "fresh empty mut-str must have len 0"
    );
    finalize_into(&mut_slot, &mut out);
    match out {
        Sexp::Str(ref text) => {
            assert_eq!(
                text.len(),
                0,
                "finalize of empty mut-str must yield empty Str"
            );
            assert_eq!(text, "");
        }
        other => panic!("expected Sexp::Str, got {:?}", other),
    }
}

// ---- Case 2: single-byte push + finalize ----

#[test]
fn mut_str_single_byte_push_finalize_yields_one_byte_str() {
    let mut mut_slot = Sexp::Nil;
    let mut out = Sexp::Nil;
    make_empty_mut_str(&mut mut_slot, 4);
    push_byte(&mut mut_slot, b'A' as i64);
    assert_eq!(
        mut_str_byte_len(&mut_slot),
        1,
        "after 1 push byte-len must be 1"
    );
    finalize_into(&mut_slot, &mut out);
    match out {
        Sexp::Str(ref text) => {
            assert_eq!(text.len(), 1);
            assert_eq!(text.as_bytes(), b"A");
        }
        other => panic!("expected Sexp::Str, got {:?}", other),
    }
}

// ---- Case 3: multi-byte ASCII via push-byte ----

#[test]
fn mut_str_multi_byte_ascii_push_round_trips() {
    // Build "hello" one byte at a time, then finalize.  This mirrors
    // the Reader lexer's symbol-name accumulation pattern.
    let mut mut_slot = Sexp::Nil;
    let mut out = Sexp::Nil;
    make_empty_mut_str(&mut mut_slot, 8);
    for &b in b"hello".iter() {
        push_byte(&mut mut_slot, b as i64);
    }
    assert_eq!(mut_str_byte_len(&mut_slot), 5);
    finalize_into(&mut_slot, &mut out);
    match out {
        Sexp::Str(ref text) => {
            assert_eq!(text, "hello");
            assert_eq!(text.as_bytes(), b"hello");
        }
        other => panic!("expected Sexp::Str, got {:?}", other),
    }
}

// ---- Case 4: UTF-8 push-codepoint round-trip ----

#[test]
fn mut_str_push_codepoint_utf8_round_trip() {
    // Encode "A日🦀" via push-codepoint:
    //   'A'    = 0x41           (1 byte)
    //   '日'   = U+65E5         (3 bytes: E6 97 A5)
    //   '🦀'  = U+1F980        (4 bytes: F0 9F A6 80)
    // Total: 1 + 3 + 4 = 8 bytes.
    let mut mut_slot = Sexp::Nil;
    let mut out = Sexp::Nil;
    make_empty_mut_str(&mut mut_slot, 16);
    push_codepoint(&mut mut_slot, 0x41);
    assert_eq!(mut_str_byte_len(&mut_slot), 1);
    push_codepoint(&mut mut_slot, 0x65E5);
    assert_eq!(mut_str_byte_len(&mut_slot), 1 + 3);
    push_codepoint(&mut mut_slot, 0x1F980);
    assert_eq!(mut_str_byte_len(&mut_slot), 1 + 3 + 4);
    finalize_into(&mut_slot, &mut out);
    match out {
        Sexp::Str(ref text) => {
            let expected = "A日🦀";
            assert_eq!(text, expected, "UTF-8 push round-trip must match");
            assert_eq!(text.as_bytes().len(), 8);
        }
        other => panic!("expected Sexp::Str, got {:?}", other),
    }
}

// ---- Case 5: mut-str-len reports running byte count ----

#[test]
fn mut_str_len_progresses_as_ops_accumulate() {
    // Verify that `mut-str-len` reflects every push immediately and
    // that the final length agrees with the finalize output length.
    let mut mut_slot = Sexp::Nil;
    let mut out = Sexp::Nil;
    make_empty_mut_str(&mut mut_slot, 16);
    assert_eq!(mut_str_byte_len(&mut_slot), 0);
    push_byte(&mut mut_slot, b'x' as i64);
    assert_eq!(mut_str_byte_len(&mut_slot), 1);
    push_byte(&mut mut_slot, b'y' as i64);
    assert_eq!(mut_str_byte_len(&mut_slot), 2);
    push_codepoint(&mut mut_slot, 0x4E2D); // U+4E2D '中' (3 bytes)
    assert_eq!(mut_str_byte_len(&mut_slot), 2 + 3);
    push_codepoint(&mut mut_slot, 0x10000); // 4-byte UTF-8
    assert_eq!(mut_str_byte_len(&mut_slot), 2 + 3 + 4);
    finalize_into(&mut_slot, &mut out);
    match out {
        Sexp::Str(ref text) => {
            assert_eq!(text.as_bytes().len(), 9);
            // Reconstruct expected: "xy" + U+4E2D + U+10000.
            let mut expected = String::new();
            expected.push('x');
            expected.push('y');
            expected.push(char::from_u32(0x4E2D).unwrap());
            expected.push(char::from_u32(0x10000).unwrap());
            assert_eq!(text, &expected);
        }
        other => panic!("expected Sexp::Str, got {:?}", other),
    }
}

// ---- Case 6: clone-not-move semantics (finalize keeps source live) ----

#[test]
fn mut_str_finalize_leaves_source_pushable() {
    // Doc 122 §122.B contract: `nl_mut_str_finalize` clones the
    // inner String, so the source MutStr remains live and push-able
    // after a finalize.  The Reader lexer relies on this for sub-
    // token snapshots (= emit a `Sexp::Str` snapshot mid-token, then
    // continue accumulating).
    let mut mut_slot = Sexp::Nil;
    let mut snap1 = Sexp::Nil;
    let mut snap2 = Sexp::Nil;
    make_empty_mut_str(&mut mut_slot, 8);
    for &b in b"ab".iter() {
        push_byte(&mut mut_slot, b as i64);
    }
    finalize_into(&mut_slot, &mut snap1);
    // Source still usable — push more bytes.
    for &b in b"cd".iter() {
        push_byte(&mut mut_slot, b as i64);
    }
    assert_eq!(mut_str_byte_len(&mut_slot), 4);
    finalize_into(&mut_slot, &mut snap2);
    match snap1 {
        Sexp::Str(ref text) => assert_eq!(text, "ab"),
        other => panic!("expected Sexp::Str for snap1, got {:?}", other),
    }
    match snap2 {
        Sexp::Str(ref text) => assert_eq!(text, "abcd"),
        other => panic!("expected Sexp::Str for snap2, got {:?}", other),
    }
}
