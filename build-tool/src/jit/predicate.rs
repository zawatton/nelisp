//! Predicate trampolines.  `eq' / `ref_eq' bodies live in
//! `lisp/nelisp-cc-jit-predicate-eq.el' + `nelisp-cc-jit-ref-eq.el'
//! (Phase 47-compiled `.o' in `libnelisp_elisp_spike.a'); `bridge::
//! predicate_link' resolves the externs.  `sxhash' / `type_of' stay
//! Rust — grammar gaps (hash composition + sexp-write-symbol).

use crate::eval::sexp::Sexp;

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
