//! Doc 111 §111.E #1 probe — direct calls into the Phase 47-compiled
//! `mirror_lookup_entry' helper.  Verifies the elisp body's
//! composition of `record-slot-ref-ptr' (§111.B) + `vector-ref-ptr'
//! (§111.C) + `sexp-payload-ptr' / `cons-cdr-raw-from-box' (§101.B)
//! + `str-eq' (§101.C) + `extern-call' into `nl_mirror_fnv1a_sexp'
//! (§100.A) end-to-end.
//!
//! The mirror layout is constructed by hand here (= the same shape
//! produced by `Env::install_empty_mirror_rust_direct' +
//! `mirror_prepend_to_bucket') so the probe runs without any
//! crate-private API surface; it asserts only on the `*const Sexp'
//! identity of the returned slot vs. the slot known to hold the
//! installed `symbol-entry' record.

#![cfg(all(target_os = "linux", target_arch = "x86_64"))]

use nelisp_build_tool::eval::sexp::Sexp;

/// FNV-1a 32-bit hash matching `nl_mirror_fnv1a_sexp' (= same loop as
/// `env_mirror::mirror_fnv1a').  Duplicated locally because the
/// upstream function is `pub(crate)' and integration tests live in a
/// separate compilation unit.  Used to pick the bucket index the
/// elisp walker will land on.
fn fnv1a(s: &str) -> u32 {
    let mut h: u32 = 0x811C9DC5;
    for c in s.chars() {
        h ^= c as u32;
        h = h.wrapping_mul(0x01000193);
    }
    h
}

/// Build an empty mirror: `Sexp::Record("nelisp-env", [ht, Nil, Nil])'
/// where `ht = Sexp::Record("fast-hash-table", [Int(count), Vector,
/// Int(0)])'.  `count' is required to be a power of 2 because the
/// elisp body uses the cheap `(h & (count - 1))' mask matching the
/// Rust fast path.
fn build_empty_mirror(bucket_count: usize) -> Sexp {
    assert!(
        bucket_count.is_power_of_two(),
        "bucket count must be a power of 2",
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

/// Install one `(KEY . ENTRY)' pair into the mirror, mirroring
/// `mirror_prepend_to_bucket'.  ENTRY is a fresh `symbol-entry' record
/// with the supplied value/function slots (plist/constant default to
/// `Nil').
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
    // SAFETY: the probe holds exclusive ownership of `mirror' (and
    // therefore the bucket vector) for the duration of the test;
    // no other borrows into `buckets.value' are live.
    unsafe {
        buckets.with_value_mut(|v| {
            let old = v[idx].clone();
            v[idx] = Sexp::cons(pair, old);
        });
    }
}

fn run_lookup(mirror: &Sexp, sym: &Sexp) -> *const Sexp {
    unsafe {
        nelisp_build_tool::elisp_cc_spike::mirror_lookup_entry(
            mirror as *const Sexp,
            sym as *const Sexp,
        )
    }
}

// ---- 3-case verification gate ----

#[test]
fn mirror_lookup_entry_positive_hit_returns_entry_record() {
    let mirror = build_empty_mirror(1024);
    install_entry(
        &mirror,
        "alpha",
        Sexp::Int(42),
        Sexp::Symbol("nelisp--unbound-marker".into()),
    );
    install_entry(&mirror, "beta", Sexp::Int(7), Sexp::Nil);

    let sym = Sexp::Symbol("alpha".into());
    let entry_ptr = run_lookup(&mirror, &sym);
    assert!(
        !entry_ptr.is_null(),
        "lookup of installed key `alpha' must not return null",
    );
    // The returned pointer addresses a Sexp slot inside the bucket's
    // `(KEY . ENTRY)` cons box; that slot holds `Sexp::Record(_)'.
    let entry = unsafe { &*entry_ptr };
    match entry {
        Sexp::Record(r) => {
            // Type tag = `symbol-entry', value slot = Int(42).
            assert!(
                matches!(&r.type_tag, Sexp::Symbol(s) if s == "symbol-entry"),
                "entry type-tag must be `symbol-entry', got {:?}",
                r.type_tag,
            );
            assert_eq!(
                r.slots.get(0),
                Some(&Sexp::Int(42)),
                "entry slot[0] (value cell) must match installed Int(42)",
            );
        }
        other => panic!("returned slot must be Sexp::Record, got {:?}", other),
    }
}

#[test]
fn mirror_lookup_entry_negative_miss_returns_null() {
    let mirror = build_empty_mirror(1024);
    install_entry(&mirror, "alpha", Sexp::Int(42), Sexp::Nil);
    install_entry(&mirror, "beta", Sexp::Int(7), Sexp::Nil);

    // `gamma' was never installed; the lookup must walk to end-of-
    // bucket (or hit an empty bucket) and return 0.
    let sym = Sexp::Symbol("gamma".into());
    let entry_ptr = run_lookup(&mirror, &sym);
    assert!(
        entry_ptr.is_null(),
        "lookup of absent key `gamma' must return null, got {:p}",
        entry_ptr,
    );
}

#[test]
fn mirror_lookup_entry_edge_empty_mirror_returns_null() {
    // Empty mirror = every bucket is `Sexp::Nil'.  The elisp body's
    // `sexp-payload-ptr' over the Nil slot returns 0, which feeds the
    // walker's `(if (= box-ptr 0) 0 ...)' termination guard.
    let mirror = build_empty_mirror(1024);
    let sym = Sexp::Symbol("anything".into());
    let entry_ptr = run_lookup(&mirror, &sym);
    assert!(
        entry_ptr.is_null(),
        "lookup against an empty mirror must return null, got {:p}",
        entry_ptr,
    );
}

// ---- Extra robustness cases ----
//
// These augment the §3.E #1 verification gate of "≥ 3/3" with two
// cases that exercise (a) multi-entry bucket collision walks (= the
// FNV-1a hash maps multiple installed keys into the same bucket; the
// elisp walker must skip the non-matching head cells) and (b) round-
// trip with a deliberately small bucket count to force collisions on
// commonly-tested names.

#[test]
fn mirror_lookup_entry_walks_past_collisions() {
    // 4-bucket mirror forces FNV1a hash collisions across short
    // inputs.  We install enough keys that at least one bucket has
    // multiple cells; the walker must traverse the cdr chain.
    let mirror = build_empty_mirror(4);
    let names = ["a", "b", "c", "d", "e", "f", "g", "h"];
    for n in &names {
        install_entry(
            &mirror,
            n,
            Sexp::Str(format!("v-{}", n)),
            Sexp::Nil,
        );
    }
    for n in &names {
        let sym = Sexp::Symbol((*n).to_string());
        let entry_ptr = run_lookup(&mirror, &sym);
        assert!(!entry_ptr.is_null(),
                "lookup of `{}' must succeed in 4-bucket layout", n);
        let entry = unsafe { &*entry_ptr };
        match entry {
            Sexp::Record(r) => {
                assert_eq!(
                    r.slots.get(0),
                    Some(&Sexp::Str(format!("v-{}", n))),
                    "entry slot[0] mismatch for `{}'",
                    n,
                );
            }
            other => panic!("entry not a Record: {:?}", other),
        }
    }
}

#[test]
fn mirror_lookup_entry_str_key_lookup_round_trip() {
    // The bucket KEY is stored as `Sexp::Str' but the lookup `sym'
    // is `Sexp::Symbol'; `str-eq' must succeed on payload equality
    // regardless of the wrapping tag (= matches the Rust impl's
    // `if let Sexp::Str(k) = &pair.car { if k == name { ... } }'
    // semantics).
    let mirror = build_empty_mirror(1024);
    install_entry(
        &mirror,
        "hello",
        Sexp::Int(123),
        Sexp::Nil,
    );

    // Lookup via Sexp::Str (= same tag as the stored KEY) — must hit.
    let str_sym = Sexp::Str("hello".into());
    assert!(!run_lookup(&mirror, &str_sym).is_null(),
            "Str-keyed lookup must hit");

    // Lookup via Sexp::Symbol (= the production caller's tag) — must
    // also hit, because `str-eq' compares only the inner `String'
    // payload.
    let sym_sym = Sexp::Symbol("hello".into());
    assert!(!run_lookup(&mirror, &sym_sym).is_null(),
            "Symbol-keyed lookup must hit (str-eq is tag-agnostic)");
}
