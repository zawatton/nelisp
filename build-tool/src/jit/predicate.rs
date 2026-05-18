//! Predicate trampolines (`nl_jit_predicate_eq' / `_ref_eq' / `_sxhash'
//! / `_type_of'), dlsym-exported.  `eq' / `ref_eq' run in elisp on
//! linux-x86_64 (slow path via `nl_sexp_eq' extern); `sxhash' / `type_of'
//! stay Rust (grammar gaps — hash composition + sexp-write-symbol).
//! Other targets fall through to the Rust trampolines via `bridge::predicate_link'.

use crate::eval::sexp::{Sexp, SEXP_TAG_INT};

/// `(eq A B) -> 1 if equal, 0 otherwise' trampoline.  4-stage:
/// same-ref / tag-eq / Int payload fast path / `sexp_eq' slow path.
///
/// # Safety
/// Caller must pass valid `*const Sexp' pointers.
#[no_mangle]
pub unsafe extern "C" fn nl_jit_predicate_eq(
    a: *const Sexp,
    b: *const Sexp,
) -> i64 {
    if a == b { return 1; }
    let a_tag = *(a as *const u8);
    let b_tag = *(b as *const u8);
    if a_tag != b_tag { return 0; }
    if a_tag == SEXP_TAG_INT {
        let a_int = *((a as *const u8).add(8) as *const i64);
        let b_int = *((b as *const u8).add(8) as *const i64);
        return (a_int == b_int) as i64;
    }
    if crate::eval::special_forms::sexp_eq(&*a, &*b) { 1 } else { 0 }
}

/// `(nelisp--ref-eq A B)' — writes `Sexp::T' / `Sexp::Nil' into `out'.
/// Body delegates to `sexp_eq' which uses `Rc::ptr_eq' for shared-heap
/// variants.  Always succeeds (= returns 0).
#[no_mangle]
pub unsafe extern "C" fn nl_jit_ref_eq(
    a: *const Sexp,
    b: *const Sexp,
    out: *mut Sexp,
) -> i64 {
    *out = if crate::eval::special_forms::sexp_eq(&*a, &*b) {
        Sexp::T
    } else {
        Sexp::Nil
    };
    0
}

// `(sxhash X)' — kept in Rust for `DefaultHasher' bit-exactness.
#[no_mangle]
pub unsafe extern "C" fn nl_jit_sxhash(arg: *const Sexp, out: *mut Sexp) -> i64 {
    use std::collections::hash_map::DefaultHasher;
    use std::hash::{Hash, Hasher};
    fn fold<H: Hasher>(v: &Sexp, h: &mut H) {
        match v {
            Sexp::Nil => 0u8.hash(h),
            Sexp::T => 1u8.hash(h),
            Sexp::Int(n) => { 2u8.hash(h); n.hash(h); }
            Sexp::Float(x) => { 3u8.hash(h); x.to_bits().hash(h); }
            Sexp::Symbol(s) => { 4u8.hash(h); s.hash(h); }
            Sexp::Str(s) => { 5u8.hash(h); s.hash(h); }
            Sexp::MutStr(rc) => { 5u8.hash(h); rc.value.hash(h); }
            Sexp::Cons(b) => { 6u8.hash(h); fold(&b.car, h); fold(&b.cdr, h); }
            Sexp::Vector(rc) => {
                7u8.hash(h);
                for it in rc.value.iter() { fold(it, h); }
            }
            Sexp::CharTable(_) => 9u8.hash(h),
            Sexp::BoolVector(rc) => {
                10u8.hash(h);
                for &b in rc.value.iter() { (b as u8).hash(h); }
            }
            Sexp::Cell(c) => fold(&c.value, h),
            Sexp::Record(rec) => {
                11u8.hash(h);
                fold(&rec.type_tag, h);
                for s in rec.slots.iter() { fold(s, h); }
            }
        }
    }
    let mut h = DefaultHasher::new();
    fold(&*arg, &mut h);
    *out = Sexp::Int((h.finish() & 0x3FFF_FFFF_FFFF_FFFFu64) as i64);
    0
}

/// `(type-of OBJECT)' — returns type-name symbol.  Records emit their
/// `type_tag' verbatim (cl-defstruct first-class); `Cell's unwrap.
#[no_mangle]
pub unsafe extern "C" fn nl_jit_type_of(arg: *const Sexp, out: *mut Sexp) -> i64 {
    let mut v: Sexp = (*arg).clone();
    while let Sexp::Cell(c) = v {
        v = c.value.clone();
    }
    if let Sexp::Record(rec) = &v {
        if let Sexp::Symbol(_) = &rec.type_tag {
            *out = rec.type_tag.clone();
            return 0;
        }
        *out = Sexp::Symbol("record".into());
        return 0;
    }
    let tag = match v {
        Sexp::Cons(_) => "cons",
        Sexp::Nil | Sexp::T | Sexp::Symbol(_) => "symbol",
        Sexp::Int(_) => "integer",
        Sexp::Float(_) => "float",
        Sexp::Str(_) | Sexp::MutStr(_) => "string",
        Sexp::Vector(_) => "vector",
        Sexp::CharTable(_) => "char-table",
        Sexp::BoolVector(_) => "bool-vector",
        Sexp::Cell(_) | Sexp::Record(_) => unreachable!(),
    };
    *out = Sexp::Symbol(tag.into());
    0
}

#[cfg(test)]
mod tests {
    use super::*;

    // --- Inline fast paths ---

    #[test]
    fn jit_eq_int_equal_inline() {
        // Same Int → trampoline int fast path returns 1.
        let a = Sexp::Int(7);
        let b = Sexp::Int(7);
        assert_eq!(unsafe { nl_jit_predicate_eq(&a, &b) }, 1);
    }

    #[test]
    fn jit_eq_int_unequal_inline() {
        // Different Int → trampoline int fast path returns 0.
        let a = Sexp::Int(7);
        let b = Sexp::Int(8);
        assert_eq!(unsafe { nl_jit_predicate_eq(&a, &b) }, 0);
    }

    #[test]
    fn jit_eq_tag_mismatch_short_circuits() {
        // Different tags → trampoline tag-byte arm returns 0 without
        // entering the slow path.  Verifies the Sexp::Int vs Sexp::Float
        // pairing (= same payload size, different tag) routes to the
        // diff arm, not the int_eq fast path.
        let a = Sexp::Int(0);
        let b = Sexp::Float(0.0);
        assert_eq!(unsafe { nl_jit_predicate_eq(&a, &b) }, 0);
    }

    #[test]
    fn jit_eq_nil_t_via_helper() {
        // Nil/T have matching tags but no payload — the slow path
        // handles this through `sexp_eq''s `(Nil, Nil) | (T, T) => true'
        // arm.
        let nil = Sexp::Nil;
        let t = Sexp::T;
        assert_eq!(unsafe { nl_jit_predicate_eq(&nil, &nil) }, 1);
        assert_eq!(unsafe { nl_jit_predicate_eq(&t, &t) }, 1);
        // Mismatched tags → diff arm inline.
        assert_eq!(unsafe { nl_jit_predicate_eq(&nil, &t) }, 0);
    }

    // --- Slow paths via sexp_eq ---

    #[test]
    fn jit_eq_symbol_by_name_via_helper() {
        // Symbol matches via `sexp_eq''s name-equality arm.
        let a = Sexp::Symbol("foo".into());
        let b = Sexp::Symbol("foo".into());
        assert_eq!(unsafe { nl_jit_predicate_eq(&a, &b) }, 1);
    }

    #[test]
    fn jit_eq_cons_identity_via_helper() {
        // Two separately-constructed cons cells with same value are
        // NOT eq (= identity check via Rc::ptr_eq inside `sexp_eq',
        // reached after both same-ref check and tag-equal branches).
        let a = Sexp::cons(Sexp::Int(1), Sexp::Int(2));
        let b = Sexp::cons(Sexp::Int(1), Sexp::Int(2));
        assert_eq!(unsafe { nl_jit_predicate_eq(&a, &b) }, 0);
        // The same cell IS eq with itself — same-ref short-circuit
        // returns 1 from the first arm before any helper call.
        assert_eq!(unsafe { nl_jit_predicate_eq(&a, &a) }, 1);
    }

    // --- Same-ref short-circuit ---

    #[test]
    fn jit_eq_same_ref_short_circuit() {
        // For every variant, comparing a Sexp ref to itself must
        // return 1 without entering the slow path.  Pre-7.1.6.d this
        // worked for Int via the `block_int_eq' Cranelift IR arm and
        // for non-Int variants via `block_same' (= the inline same-ref
        // check); now both are unified in the trampoline's first arm.
        let int = Sexp::Int(42);
        let flt = Sexp::Float(3.14);
        let nil = Sexp::Nil;
        let t = Sexp::T;
        let sym = Sexp::Symbol("x".into());
        let s = Sexp::Str("hello".into());
        let cons = Sexp::cons(Sexp::Int(1), Sexp::Int(2));
        for r in [&int, &flt, &nil, &t, &sym, &s, &cons] {
            let p = r as *const Sexp;
            assert_eq!(unsafe { nl_jit_predicate_eq(p, p) }, 1);
        }
    }
}
