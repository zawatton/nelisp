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

fn install_entry(mirror: &Sexp, name: &str, value: Sexp, function: Sexp) {
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
        vec![value, function, Sexp::Nil, Sexp::Nil],
    );
    let pair = Sexp::cons(Sexp::Str(name.to_string()), entry);
    unsafe {
        buckets.with_value_mut(|v| {
            let old = v[idx].clone();
            v[idx] = Sexp::cons(pair, old);
        });
    }
}

fn run_is_fbound(mirror: &Sexp, sym: &Sexp, unbound: &Sexp) -> i64 {
    unsafe {
        nelisp_build_tool::elisp_cc_spike::mirror_is_fbound(
            mirror as *const Sexp,
            sym as *const Sexp,
            unbound as *const Sexp,
        )
    }
}

// ---- 3-case verification gate ----

#[test]
fn mirror_is_fbound_positive_function_returns_1() {
    let mirror = build_empty_mirror(1024);
    let unbound = Sexp::Symbol("nelisp--unbound-marker".into());
    install_entry(
        &mirror,
        "car",
        unbound.clone(),
        Sexp::Symbol("builtin-car".into()),
    );

    let sym = Sexp::Symbol("car".into());
    assert_eq!(run_is_fbound(&mirror, &sym, &unbound), 1);
}

#[test]
fn mirror_is_fbound_negative_unbound_returns_0() {
    let mirror = build_empty_mirror(1024);
    let unbound = Sexp::Symbol("nelisp--unbound-marker".into());
    // Entry exists but function slot = unbound-marker.
    install_entry(&mirror, "datum", Sexp::Int(7), unbound.clone());

    let sym = Sexp::Symbol("datum".into());
    assert_eq!(run_is_fbound(&mirror, &sym, &unbound), 0);
}

#[test]
fn mirror_is_fbound_edge_absent_entry_returns_0() {
    let mirror = build_empty_mirror(1024);
    let unbound = Sexp::Symbol("nelisp--unbound-marker".into());

    let sym = Sexp::Symbol("ghost".into());
    assert_eq!(run_is_fbound(&mirror, &sym, &unbound), 0);
}
