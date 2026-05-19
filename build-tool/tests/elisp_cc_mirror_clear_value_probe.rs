//! Doc 111 §111.E #9 probe — `mirror_clear_value' (= slot 0 cleared
//! to the unbound-marker sentinel passed in by caller, mirrors
//! `makunbound').

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
    assert!(bucket_count.is_power_of_two());
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
        _ => panic!(),
    };
    let ht_rec = match env_rec.slots.get(0) {
        Some(Sexp::Record(r)) => r,
        _ => panic!(),
    };
    let bucket_count = match ht_rec.slots.get(0) {
        Some(Sexp::Int(n)) => *n as u32,
        _ => panic!(),
    };
    let buckets = match ht_rec.slots.get(1) {
        Some(Sexp::Vector(v)) => v,
        _ => panic!(),
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

fn read_slot(mirror: &Sexp, name: &str, n: usize) -> Option<Sexp> {
    let env_rec = match mirror {
        Sexp::Record(r) => r,
        _ => return None,
    };
    let ht_rec = match env_rec.slots.get(0)? {
        Sexp::Record(r) => r,
        _ => return None,
    };
    let bucket_count = match ht_rec.slots.get(0)? {
        Sexp::Int(n) => *n as u32,
        _ => return None,
    };
    let buckets = match ht_rec.slots.get(1)? {
        Sexp::Vector(v) => v,
        _ => return None,
    };
    let idx = (fnv1a(name) & (bucket_count - 1)) as usize;
    let mut cur = buckets.value.get(idx)?;
    while let Sexp::Cons(c) = cur {
        if let Sexp::Cons(pair) = &c.car {
            if let Sexp::Str(k) = &pair.car {
                if k == name {
                    if let Sexp::Record(r) = &pair.cdr {
                        return r.slots.get(n).cloned();
                    }
                }
            }
        }
        cur = &c.cdr;
    }
    None
}

fn run_clear_value(mirror: &Sexp, sym: &Sexp, unbound: &Sexp) -> i64 {
    unsafe {
        nelisp_build_tool::elisp_cc_spike::mirror_clear_value(
            mirror as *const Sexp,
            sym as *const Sexp,
            unbound as *const Sexp,
        )
    }
}

#[test]
fn mirror_clear_value_positive_hit_sets_slot_0_to_unbound() {
    let mirror = build_empty_mirror(1024);
    install_entry(&mirror, "v", Sexp::Int(42), Sexp::Nil);
    let sym = Sexp::Symbol("v".into());
    let unbound = Sexp::Symbol("nelisp--unbound-marker".into());
    assert_eq!(run_clear_value(&mirror, &sym, &unbound), 1);
    assert_eq!(
        read_slot(&mirror, "v", 0).unwrap(),
        Sexp::Symbol("nelisp--unbound-marker".into()),
    );
}

#[test]
fn mirror_clear_value_negative_miss_returns_zero() {
    let mirror = build_empty_mirror(1024);
    install_entry(&mirror, "x", Sexp::Int(1), Sexp::Nil);
    let sym = Sexp::Symbol("not-in-mirror".into());
    let unbound = Sexp::Symbol("nelisp--unbound-marker".into());
    assert_eq!(run_clear_value(&mirror, &sym, &unbound), 0);
    // Existing entry unchanged.
    assert_eq!(read_slot(&mirror, "x", 0).unwrap(), Sexp::Int(1));
}

#[test]
fn mirror_clear_value_edge_empty_mirror_returns_zero() {
    let mirror = build_empty_mirror(1024);
    let sym = Sexp::Symbol("any".into());
    let unbound = Sexp::Symbol("nelisp--unbound-marker".into());
    assert_eq!(run_clear_value(&mirror, &sym, &unbound), 0);
}

#[test]
fn mirror_clear_value_does_not_touch_function_slot() {
    let mirror = build_empty_mirror(1024);
    install_entry(&mirror, "k", Sexp::Int(10), Sexp::Symbol("fn".into()));
    let sym = Sexp::Symbol("k".into());
    let unbound = Sexp::Symbol("nelisp--unbound-marker".into());
    assert_eq!(run_clear_value(&mirror, &sym, &unbound), 1);
    assert_eq!(read_slot(&mirror, "k", 0).unwrap(), unbound);
    assert_eq!(
        read_slot(&mirror, "k", 1).unwrap(),
        Sexp::Symbol("fn".into())
    );
}
