//! Doc 111 §111.E #6 probe — direct calls into the Phase 47-compiled
//! `mirror_is_constant' helper.  Verifies the elisp body's
//! composition of #1 (`mirror_lookup_entry') + §111.B
//! `record-slot-ref-ptr' (slot 3) + §100 `sexp-tag' equality against
//! `SEXP_TAG_T' (= 1).

#![cfg(all(target_os = "linux", target_arch = "x86_64"))]

use nelisp_build_tool::eval::sexp::Sexp;

fn fnv1a(s: &str) -> u32 {
    let mut h: u32 = 0x811C9DC5;
    for c in s.chars() {
        h ^= c as u32;
        h = h.wrapping_mul(0x01000193);
    }
    h
}

fn build_empty_mirror(bucket_count: usize) -> Sexp {
    assert!(
        bucket_count.is_power_of_two(),
        "bucket count must be a power of 2"
    );
    let buckets = Sexp::vector(vec![Sexp::Nil; bucket_count]);
    let ht = Sexp::record(
        Sexp::Symbol("fast-hash-table".into()),
        vec![Sexp::Int(bucket_count as i64), buckets, Sexp::Int(0)],
    );
    Sexp::record(
        Sexp::Symbol("nelisp-env".into()),
        vec![ht, Sexp::Nil, Sexp::Nil],
    )
}

/// Install a 4-slot symbol-entry with caller-controlled slot 3
/// (= constant flag) so the probe can build both `Sexp::T' (= const)
/// and `Sexp::Nil' (= non-const) cases.
fn install_entry_with_constant(
    mirror: &Sexp,
    name: &str,
    value: Sexp,
    function: Sexp,
    constant: Sexp,
) {
    let env_rec = match mirror {
        Sexp::Record(r) => r,
        _ => panic!("mirror must be Sexp::Record"),
    };
    let ht_rec = match env_rec.slots.get(0) {
        Some(Sexp::Record(r)) => r,
        _ => panic!("mirror.slots[0] must be Sexp::Record"),
    };
    let bucket_count = match ht_rec.slots.get(0) {
        Some(Sexp::Int(n)) => *n as u32,
        _ => panic!("ht.slots[0] must be Sexp::Int"),
    };
    let buckets = match ht_rec.slots.get(1) {
        Some(Sexp::Vector(v)) => v,
        _ => panic!("ht.slots[1] must be Sexp::Vector"),
    };
    let idx = (fnv1a(name) & (bucket_count - 1)) as usize;
    let entry = Sexp::record(
        Sexp::Symbol("symbol-entry".into()),
        vec![value, function, Sexp::Nil, constant],
    );
    let pair = Sexp::cons(Sexp::Str(name.to_string()), entry);
    unsafe {
        buckets.with_value_mut(|v| {
            let old = v[idx].clone();
            v[idx] = Sexp::cons(pair, old);
        });
    }
}

fn run_is_constant(mirror: &Sexp, sym: &Sexp) -> i64 {
    unsafe {
        nelisp_build_tool::elisp_cc_spike::mirror_is_constant(
            mirror as *const Sexp,
            sym as *const Sexp,
        )
    }
}

// ---- 3-case verification gate ----

#[test]
fn mirror_is_constant_positive_t_slot_returns_1() {
    let mirror = build_empty_mirror(1024);
    install_entry_with_constant(&mirror, "pi", Sexp::Int(3), Sexp::Nil, Sexp::T);

    let sym = Sexp::Symbol("pi".into());
    assert_eq!(run_is_constant(&mirror, &sym), 1);
}

#[test]
fn mirror_is_constant_negative_nil_slot_returns_0() {
    let mirror = build_empty_mirror(1024);
    install_entry_with_constant(&mirror, "x", Sexp::Int(42), Sexp::Nil, Sexp::Nil);

    let sym = Sexp::Symbol("x".into());
    assert_eq!(run_is_constant(&mirror, &sym), 0);
}

#[test]
fn mirror_is_constant_edge_absent_entry_returns_0() {
    let mirror = build_empty_mirror(1024);
    let sym = Sexp::Symbol("ghost".into());
    assert_eq!(run_is_constant(&mirror, &sym), 0);
}
