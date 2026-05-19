//! Doc 111 §111.E #7 probe — direct calls into the Phase 47-compiled
//! `mirror_set_value' helper.  Verifies the elisp body's composition
//! of `extern-call' into `nelisp_mirror_lookup_entry' (= §111.E #1) +
//! `record-slot-set' (§111.B) end-to-end.
//!
//! The mirror layout is constructed by hand (= same shape as
//! `Env::install_empty_mirror_rust_direct' + `mirror_prepend_to_bucket')
//! so the probe runs without any crate-private API surface; it asserts
//! on the post-call `slots[0]' value of the matched symbol-entry
//! record.

#![cfg(all(target_os = "linux", target_arch = "x86_64"))]

use nelisp_build_tool::eval::sexp::Sexp;

/// FNV-1a 32-bit hash — same loop as `env_helpers::mirror_fnv1a'.
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

/// Read `slots[0]' of the symbol-entry record installed under `name'.
/// Walks the mirror by hand (= test-side mirror_lookup_entry that
/// returns the entry record by clone, not a raw pointer) so the
/// post-write assertion runs against a known-good observation path.
fn read_value_slot(mirror: &Sexp, name: &str) -> Option<Sexp> {
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
                        return r.slots.get(0).cloned();
                    }
                }
            }
        }
        cur = &c.cdr;
    }
    None
}

fn run_set_value(mirror: &Sexp, sym: &Sexp, val: &Sexp) -> i64 {
    unsafe {
        nelisp_build_tool::elisp_cc_spike::mirror_set_value(
            mirror as *const Sexp,
            sym as *const Sexp,
            val as *const Sexp,
        )
    }
}

// ---- 3-case verification gate ----

#[test]
fn mirror_set_value_positive_hit_overwrites_slot_0() {
    // Install entry with initial value 42, then call mirror_set_value
    // to overwrite slot 0 with Int(99).
    let mirror = build_empty_mirror(1024);
    install_entry(&mirror, "alpha", Sexp::Int(42), Sexp::Nil);

    let sym = Sexp::Symbol("alpha".into());
    let new_val = Sexp::Int(99);
    let rc = run_set_value(&mirror, &sym, &new_val);
    assert_eq!(rc, 1, "set_value of existing key must return 1");

    let observed = read_value_slot(&mirror, "alpha").expect("entry must exist post-set");
    assert_eq!(
        observed,
        Sexp::Int(99),
        "slot[0] must reflect the new value",
    );
}

#[test]
fn mirror_set_value_negative_miss_returns_zero_no_change() {
    // Lookup miss: helper returns 0 and the mirror is unchanged.
    let mirror = build_empty_mirror(1024);
    install_entry(&mirror, "alpha", Sexp::Int(42), Sexp::Nil);

    let sym = Sexp::Symbol("gamma".into());
    let new_val = Sexp::Int(99);
    let rc = run_set_value(&mirror, &sym, &new_val);
    assert_eq!(rc, 0, "set_value of absent key must return 0");

    // alpha's slot 0 must be untouched.
    let observed = read_value_slot(&mirror, "alpha").expect("alpha must still exist");
    assert_eq!(
        observed,
        Sexp::Int(42),
        "miss must not perturb existing entries",
    );
}

#[test]
fn mirror_set_value_edge_empty_mirror_returns_zero() {
    // Empty mirror: every lookup misses, so set_value returns 0.
    let mirror = build_empty_mirror(1024);
    let sym = Sexp::Symbol("anything".into());
    let new_val = Sexp::Int(7);
    let rc = run_set_value(&mirror, &sym, &new_val);
    assert_eq!(rc, 0, "set_value against empty mirror must return 0");
}

// ---- Extra robustness cases ----

#[test]
fn mirror_set_value_overwrites_boxed_value_refcount_safely() {
    // Install entry whose slot 0 is a boxed Str; overwrite with
    // another boxed Str.  The §111.B refcount-aware drop must run
    // (= no leak, no double-free).  We verify by reading the new
    // value back; a refcount bug would surface as a crash or wrong
    // observation.
    let mirror = build_empty_mirror(1024);
    install_entry(&mirror, "key", Sexp::Str("initial".into()), Sexp::Nil);

    let sym = Sexp::Symbol("key".into());
    let new_val = Sexp::Str("overwritten".into());
    let rc = run_set_value(&mirror, &sym, &new_val);
    assert_eq!(rc, 1);

    let observed = read_value_slot(&mirror, "key").unwrap();
    assert_eq!(observed, Sexp::Str("overwritten".into()));
}

#[test]
fn mirror_set_value_idempotent_double_write() {
    // Two consecutive set_value calls on the same key: both must hit,
    // and the final slot value is the second write.
    let mirror = build_empty_mirror(1024);
    install_entry(&mirror, "key", Sexp::Int(0), Sexp::Nil);

    let sym = Sexp::Symbol("key".into());

    let v1 = Sexp::Int(10);
    assert_eq!(run_set_value(&mirror, &sym, &v1), 1);
    assert_eq!(read_value_slot(&mirror, "key"), Some(Sexp::Int(10)));

    let v2 = Sexp::Int(20);
    assert_eq!(run_set_value(&mirror, &sym, &v2), 1);
    assert_eq!(read_value_slot(&mirror, "key"), Some(Sexp::Int(20)));
}
