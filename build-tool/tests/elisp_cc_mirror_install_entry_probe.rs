//! Doc 111 §111.E #12 probe — `mirror_install_entry' (= all-4-slots
//! update on existing entry).  The auto-vivify branch (= prepend new
//! cons cell to bucket) stays in Rust under the dispatcher for now;
//! this probe exercises only the existing-entry update path which
//! returns 1 on hit.

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

fn install_entry_with_slots(
    mirror: &Sexp,
    name: &str,
    value: Sexp,
    function: Sexp,
    plist: Sexp,
    constant: Sexp,
) {
    let env_rec = match mirror { Sexp::Record(r) => r, _ => panic!() };
    let ht_rec = match env_rec.slots.get(0) { Some(Sexp::Record(r)) => r, _ => panic!() };
    let bucket_count = match ht_rec.slots.get(0) { Some(Sexp::Int(n)) => *n as u32, _ => panic!() };
    let buckets = match ht_rec.slots.get(1) { Some(Sexp::Vector(v)) => v, _ => panic!() };
    let idx = (fnv1a(name) & (bucket_count - 1)) as usize;
    let entry = Sexp::record(
        Sexp::Symbol("symbol-entry".into()),
        vec![value, function, plist, constant],
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
    let env_rec = match mirror { Sexp::Record(r) => r, _ => return None };
    let ht_rec = match env_rec.slots.get(0)? { Sexp::Record(r) => r, _ => return None };
    let bucket_count = match ht_rec.slots.get(0)? { Sexp::Int(n) => *n as u32, _ => return None };
    let buckets = match ht_rec.slots.get(1)? { Sexp::Vector(v) => v, _ => return None };
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

fn run_install_entry(
    mirror: &Sexp,
    sym: &Sexp,
    value: &Sexp,
    function: &Sexp,
    plist: &Sexp,
    constant: &Sexp,
) -> i64 {
    unsafe {
        nelisp_build_tool::elisp_cc_spike::mirror_install_entry(
            mirror as *const Sexp,
            sym as *const Sexp,
            value as *const Sexp,
            function as *const Sexp,
            plist as *const Sexp,
            constant as *const Sexp,
        )
    }
}

#[test]
fn mirror_install_entry_positive_hit_overwrites_all_4_slots() {
    let mirror = build_empty_mirror(1024);
    install_entry_with_slots(
        &mirror,
        "k",
        Sexp::Int(0),
        Sexp::Nil,
        Sexp::Nil,
        Sexp::Nil,
    );

    let sym = Sexp::Symbol("k".into());
    let v = Sexp::Int(42);
    let f = Sexp::Symbol("car".into());
    let p = Sexp::Str("plist-data".into());
    let c = Sexp::T;
    assert_eq!(run_install_entry(&mirror, &sym, &v, &f, &p, &c), 1);

    assert_eq!(read_slot(&mirror, "k", 0).unwrap(), v);
    assert_eq!(read_slot(&mirror, "k", 1).unwrap(), f);
    assert_eq!(read_slot(&mirror, "k", 2).unwrap(), p);
    assert_eq!(read_slot(&mirror, "k", 3).unwrap(), c);
}

#[test]
fn mirror_install_entry_negative_miss_returns_zero() {
    let mirror = build_empty_mirror(1024);
    install_entry_with_slots(
        &mirror,
        "exists",
        Sexp::Nil,
        Sexp::Nil,
        Sexp::Nil,
        Sexp::Nil,
    );
    let sym = Sexp::Symbol("absent".into());
    let nil = Sexp::Nil;
    assert_eq!(run_install_entry(&mirror, &sym, &nil, &nil, &nil, &nil), 0);
}

#[test]
fn mirror_install_entry_edge_empty_mirror_returns_zero() {
    let mirror = build_empty_mirror(1024);
    let sym = Sexp::Symbol("any".into());
    let nil = Sexp::Nil;
    assert_eq!(run_install_entry(&mirror, &sym, &nil, &nil, &nil, &nil), 0);
}

#[test]
fn mirror_install_entry_idempotent_double_install() {
    let mirror = build_empty_mirror(1024);
    install_entry_with_slots(
        &mirror,
        "k",
        Sexp::Int(0),
        Sexp::Nil,
        Sexp::Nil,
        Sexp::Nil,
    );

    let sym = Sexp::Symbol("k".into());
    let v1 = Sexp::Int(1);
    let f1 = Sexp::Symbol("f1".into());
    let p1 = Sexp::Nil;
    let c1 = Sexp::Nil;
    assert_eq!(run_install_entry(&mirror, &sym, &v1, &f1, &p1, &c1), 1);

    let v2 = Sexp::Int(2);
    let f2 = Sexp::Symbol("f2".into());
    let p2 = Sexp::Str("p2".into());
    let c2 = Sexp::T;
    assert_eq!(run_install_entry(&mirror, &sym, &v2, &f2, &p2, &c2), 1);

    assert_eq!(read_slot(&mirror, "k", 0).unwrap(), v2);
    assert_eq!(read_slot(&mirror, "k", 1).unwrap(), f2);
    assert_eq!(read_slot(&mirror, "k", 2).unwrap(), p2);
    assert_eq!(read_slot(&mirror, "k", 3).unwrap(), c2);
}
