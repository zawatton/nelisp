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

fn run_lookup_function(mirror: &Sexp, sym: &Sexp) -> Sexp {
    let mut slot = Sexp::Nil;
    let slot_ptr = &mut slot as *mut Sexp;
    let returned = unsafe {
        nelisp_build_tool::elisp_cc_spike::mirror_lookup_function(
            mirror as *const Sexp,
            sym as *const Sexp,
            slot_ptr,
        )
    };
    assert_eq!(
        returned, slot_ptr,
        "extern must return the caller-provided slot pointer"
    );
    slot
}

// ---- 3-case verification gate ----

#[test]
fn mirror_lookup_function_positive_hit_returns_function_clone() {
    let mirror = build_empty_mirror(1024);
    install_entry(
        &mirror,
        "car",
        Sexp::Symbol("nelisp--unbound-marker".into()),
        Sexp::Symbol("builtin-car".into()),
    );
    let sym = Sexp::Symbol("car".into());
    assert_eq!(
        run_lookup_function(&mirror, &sym),
        Sexp::Symbol("builtin-car".into()),
    );
}

#[test]
fn mirror_lookup_function_negative_miss_returns_nil() {
    let mirror = build_empty_mirror(1024);
    install_entry(
        &mirror,
        "car",
        Sexp::Nil,
        Sexp::Symbol("builtin-car".into()),
    );
    let sym = Sexp::Symbol("cdr".into());
    assert_eq!(run_lookup_function(&mirror, &sym), Sexp::Nil);
}

#[test]
fn mirror_lookup_function_edge_empty_mirror_returns_nil() {
    let mirror = build_empty_mirror(1024);
    let sym = Sexp::Symbol("anything".into());
    assert_eq!(run_lookup_function(&mirror, &sym), Sexp::Nil);
}
