//! Doc 111 §111.E #8 probe — `mirror_set_function' (= slot 1 sibling
//! of `mirror_set_value').  Verifies the function-cell write path.

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

/// Read slot N of the symbol-entry record installed under `name'.
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

fn run_set_function(mirror: &Sexp, sym: &Sexp, val: &Sexp) -> i64 {
    unsafe {
        nelisp_build_tool::elisp_cc_spike::mirror_set_function(
            mirror as *const Sexp,
            sym as *const Sexp,
            val as *const Sexp,
        )
    }
}

#[test]
fn mirror_set_function_positive_hit_overwrites_slot_1() {
    let mirror = build_empty_mirror(1024);
    install_entry(&mirror, "alpha", Sexp::Nil, Sexp::Int(0));

    let sym = Sexp::Symbol("alpha".into());
    let new_func = Sexp::Symbol("car".into());
    let rc = run_set_function(&mirror, &sym, &new_func);
    assert_eq!(rc, 1);
    assert_eq!(
        read_slot(&mirror, "alpha", 1).unwrap(),
        Sexp::Symbol("car".into()),
    );
    // Slot 0 must be untouched.
    assert_eq!(read_slot(&mirror, "alpha", 0).unwrap(), Sexp::Nil);
}

#[test]
fn mirror_set_function_negative_miss_returns_zero() {
    let mirror = build_empty_mirror(1024);
    install_entry(&mirror, "alpha", Sexp::Nil, Sexp::Int(0));
    let sym = Sexp::Symbol("beta".into());
    let v = Sexp::Symbol("car".into());
    assert_eq!(run_set_function(&mirror, &sym, &v), 0);
}

#[test]
fn mirror_set_function_edge_empty_mirror_returns_zero() {
    let mirror = build_empty_mirror(1024);
    let sym = Sexp::Symbol("any".into());
    let v = Sexp::Nil;
    assert_eq!(run_set_function(&mirror, &sym, &v), 0);
}

#[test]
fn mirror_set_function_does_not_touch_other_slots() {
    let mirror = build_empty_mirror(1024);
    install_entry(&mirror, "k", Sexp::Int(7), Sexp::Symbol("old".into()));
    let sym = Sexp::Symbol("k".into());
    let v = Sexp::Symbol("new".into());
    assert_eq!(run_set_function(&mirror, &sym, &v), 1);
    assert_eq!(read_slot(&mirror, "k", 0).unwrap(), Sexp::Int(7));
    assert_eq!(
        read_slot(&mirror, "k", 1).unwrap(),
        Sexp::Symbol("new".into())
    );
}
