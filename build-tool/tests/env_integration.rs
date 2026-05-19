//! Doc 102 Phase 8 + Doc 104 Stage 3.b regression tests.
//!
//! Relocated from `src/eval/env.rs::tests' into `tests/' (Doc 131, 2026-05-18)
//! to keep `src/eval/env.rs' production-only — same carve-out pattern that
//! Doc 130 applied to the 3,057-LOC `src/eval/tests.rs'.

use nelisp_build_tool::eval as eval_mod;
use nelisp_build_tool::eval::sexp::Sexp;
use nelisp_build_tool::eval::Env;
use std::collections::HashMap;

#[test]
fn phase8_session1_globals_record_is_nelisp_env_after_bootstrap() {
    // Doc 102 Phase 8 Sprint Session 1 — verify `new_global' installs
    // a fresh `nelisp-env' record into `globals_record'.
    let env = Env::new_global();
    match &env.globals_record {
        Sexp::Record(r) => match &r.type_tag {
            Sexp::Symbol(s) => assert_eq!(s, "nelisp-env"),
            other => panic!("globals_record type tag is not a symbol: {:?}", other),
        },
        other => panic!("globals_record is not a Record: {:?}", other),
    }
}

#[test]
fn phase8_session2_globals_record_mirrors_rust_hashmap_sentinels() {
    // Doc 102 Phase 8 Sprint Session 2 — verify the populated elisp env
    // mirror contains the same key sentinels as the Rust HashMap.
    // Spot-checks `car' (= function cell, ~1000 stdlib fns), `t' (=
    // value + constant), and `make-hash-table' (= function cell, mid-
    // STDLIB load order) — together prove that builtins, intern-
    // constant, and STDLIB image decode all reached the mirror.
    let mut env = Env::new_global();
    let lookup_fn = env
        .lookup_function("nelisp-env-lookup-function")
        .expect("nelisp-env-lookup-function not loaded");
    let record = env.globals_record.clone();
    for name in &["car", "make-hash-table"] {
        let result = eval_mod::apply_function(
            &lookup_fn,
            &[record.clone(), Sexp::Str((*name).into())],
            &mut env,
        );
        assert!(
            result.is_ok(),
            "mirror missing function `{}': {:?}",
            name,
            result.err()
        );
    }
    // `t' is a value-cell entry (= intern_constant path).
    let lookup_value_fn = env
        .lookup_function("nelisp-env-lookup-value")
        .expect("nelisp-env-lookup-value not loaded");
    let t_result =
        eval_mod::apply_function(&lookup_value_fn, &[record, Sexp::Str("t".into())], &mut env);
    assert!(
        matches!(t_result, Ok(Sexp::T)),
        "mirror missing `t': {:?}",
        t_result
    );
}

#[test]
fn phase8_session3_post_bootstrap_set_value_propagates_to_mirror() {
    // Doc 102 Phase 8 Sprint Session 3 — after bootstrap, a Rust-side
    // `set_value' on a fresh global must surface in the elisp mirror.
    // Validates that `mirror_set_value' is wired correctly and that
    // the elisp env has not gone stale post-populate.
    let mut env = Env::new_global();
    env.set_value("doc-102-phase-8-session-3-probe", Sexp::Int(4242))
        .expect("set_value failed");
    let lookup_fn = env
        .lookup_function("nelisp-env-lookup-value")
        .expect("nelisp-env-lookup-value not loaded");
    let record = env.globals_record.clone();
    let result = eval_mod::apply_function(
        &lookup_fn,
        &[record, Sexp::Str("doc-102-phase-8-session-3-probe".into())],
        &mut env,
    );
    assert!(
        matches!(result, Ok(Sexp::Int(4242))),
        "mirror did not observe post-bootstrap set_value: {:?}",
        result
    );
}

#[test]
fn phase8_session3_post_bootstrap_set_function_propagates_to_mirror() {
    // Counterpart for the function cell — validate `set_function'
    // dual-writes through `mirror_set_function'.
    let mut env = Env::new_global();
    let sentinel = Sexp::list_from(&[Sexp::Symbol("builtin".into()), Sexp::Symbol("car".into())]);
    env.set_function("doc-102-phase-8-session-3-fn-probe", sentinel.clone());
    let lookup_fn = env
        .lookup_function("nelisp-env-lookup-function")
        .expect("nelisp-env-lookup-function not loaded");
    let record = env.globals_record.clone();
    let result = eval_mod::apply_function(
        &lookup_fn,
        &[
            record,
            Sexp::Str("doc-102-phase-8-session-3-fn-probe".into()),
        ],
        &mut env,
    );
    assert!(
        matches!(&result, Ok(v) if *v == sentinel),
        "mirror did not observe post-bootstrap set_function: {:?}",
        result
    );
}

#[test]
fn phase8_session4_rust_direct_lookup_function_matches_rust_hashmap() {
    // Doc 102 Phase 8 Sprint Session 4 — Rust-direct mirror_lookup_*
    // accessors return the same Sexp the Rust HashMap holds.  Spot-
    // checks `car' / `cdr' / `eq' / `make-hash-table' covering
    // builtins + STDLIB-loaded functions.
    let env = Env::new_global();
    for name in &["car", "cdr", "eq", "make-hash-table"] {
        let rust_side = env.lookup_function(name).expect(name);
        let mirror_side = env.mirror_lookup_function(name);
        assert!(
            mirror_side != env.unbound_marker,
            "mirror missing function `{}'",
            name,
        );
        assert_eq!(rust_side, mirror_side, "mismatch for `{}'", name);
    }
}

#[test]
fn phase8_session4_rust_direct_lookup_value_returns_t_for_constant_t() {
    // `t' is interned with value Sexp::T + constant flag.  Verify
    // the mirror returns Sexp::T (not the unbound-marker sentinel).
    let env = Env::new_global();
    let v = env.mirror_lookup_value("t");
    assert!(v != env.unbound_marker, "mirror missing value for `t'");
    assert!(matches!(v, Sexp::T));
}

#[test]
fn phase8_session4_rust_direct_is_fbound_matches_rust() {
    let env = Env::new_global();
    // Present in env: `car'.  Absent: invented name.
    assert!(env.mirror_is_fbound("car"));
    assert!(!env.mirror_is_fbound("doc-102-session-4-absent-fn"));
}

// `phase8_session4_fnv1a_matches_elisp_hash_loop' moved to
// `eval/env_helpers.rs::tests::mirror_fnv1a_matches_elisp_hash_loop'
// (Doc 102 Phase 8 → Doc 114 Step 5 consolidation); Doc 115 §115.7
// deleted the test alongside the deleted Rust hash impl.  Bit-
// equality coverage moves to the integration probe at
// `tests/elisp_cc_fnv1a_probe.rs'.

// ---- Doc 104 Stage 3.b regression tests ----

fn frames_record_depth(env: &Env) -> i64 {
    match &env.frames_record {
        Sexp::Record(r) => match r.slots.get(1) {
            Some(Sexp::Int(n)) => *n,
            other => panic!("frames_record slot 1 not Int: {:?}", other),
        },
        other => panic!("frames_record not Record: {:?}", other),
    }
}

#[test]
fn doc104_stage3b_install_stage0_yields_empty_lexframe_stack() {
    // Bootstrap should leave an empty `nelisp-lexframe-stack' record
    // (depth = 0, BACKING vec pre-allocated to the initial capacity).
    let env = Env::new_global_no_stdlib();
    match &env.frames_record {
        Sexp::Record(r) => match &r.type_tag {
            Sexp::Symbol(s) => assert_eq!(s, "nelisp-lexframe-stack"),
            other => panic!("frames_record type tag not a symbol: {:?}", other),
        },
        other => panic!("frames_record is not a Record: {:?}", other),
    }
    assert_eq!(frames_record_depth(&env), 0);
}

#[test]
fn doc104_stage3b_push_pop_dual_writes_keep_depths_aligned() {
    // Stage 3.b shipped this as a dual-write parity check; Stage
    // 3.d retired the Vec write so the assertion now tracks the
    // mirror depth only.  Walking past the initial capacity (= 8)
    // still exercises the capacity-doubling grow path.
    let mut env = Env::new_global_no_stdlib();
    for i in 0..20 {
        env.push_frame();
        assert_eq!(
            frames_record_depth(&env),
            (i + 1) as i64,
            "mirror depth wrong after push #{}",
            i
        );
    }
    for i in 0..20 {
        env.pop_frame();
        assert_eq!(
            frames_record_depth(&env),
            (19 - i) as i64,
            "mirror depth wrong after pop #{}",
            i
        );
    }
    assert_eq!(frames_record_depth(&env), 0);
}

#[test]
fn doc104_stage3b_bind_local_visible_via_mirror() {
    // After bind_local, NAME must resolve via both find_frame_cell
    // (which Stage 3.c flipped to mirror walks) and the direct
    // mirror helper.  Both paths now read the mirror; the test
    // remains green and acts as a regression gate for Stage 3.d.
    let mut env = Env::new_global_no_stdlib();
    env.push_frame();
    env.bind_local("doc104-stage3b-probe", Sexp::Int(7777));
    // Find via frame_stack_find_rust_direct (= the canonical post-3.c
    // mirror walk; find_frame_cell is private since Doc 131).
    let cell_via_find = env
        .frame_stack_find_rust_direct("doc104-stage3b-probe")
        .expect("frame_stack_find_rust_direct missing");
    match cell_via_find {
        Sexp::Cell(c) => assert_eq!(c.value.clone(), Sexp::Int(7777)),
        other => panic!("stack find returned non-Cell: {:?}", other),
    }
    // Direct mirror walk.
    let mirror_cell = env
        .frame_lookup_rust_direct("doc104-stage3b-probe")
        .expect("mirror direct missing");
    match mirror_cell {
        Sexp::Cell(c) => assert_eq!(c.value.clone(), Sexp::Int(7777)),
        other => panic!("mirror frame slot is not Sexp::Cell: {:?}", other),
    }
}

#[test]
fn doc104_stage3b_stack_find_walks_innermost_first() {
    // Outer frame binds X=1, inner shadows with X=2.  Mirror walk
    // must return the inner value.
    let mut env = Env::new_global_no_stdlib();
    env.push_frame();
    env.bind_local("doc104-stage3b-shadow", Sexp::Int(1));
    env.push_frame();
    env.bind_local("doc104-stage3b-shadow", Sexp::Int(2));
    let cell = env
        .frame_stack_find_rust_direct("doc104-stage3b-shadow")
        .expect("stack_find missing");
    match cell {
        Sexp::Cell(c) => assert_eq!(c.value.clone(), Sexp::Int(2)),
        other => panic!("stack_find returned non-Cell: {:?}", other),
    }
}

#[test]
fn doc104_stage3b_bind_local_preserves_cell_identity_across_stacks() {
    // Closure write-through invariant: the FrameCell observed via
    // frame_stack_find_rust_direct (= the canonical Stage 3.c mirror walk)
    // and the one obtained via frame_lookup_rust_direct must share the same
    // NlCellRef Rc — a write via one handle is visible through the
    // other.  Drives the Stage 3.d cutover safety case (= closure
    // setq still hits the binding's slot).
    let mut env = Env::new_global_no_stdlib();
    env.push_frame();
    env.bind_local("doc104-stage3b-write-through", Sexp::Int(10));
    let cell_via_find_sexp = env
        .frame_stack_find_rust_direct("doc104-stage3b-write-through")
        .expect("frame_stack_find_rust_direct missing");
    let cell_via_find = match cell_via_find_sexp {
        Sexp::Cell(c) => c,
        other => panic!("stack find returned non-Cell: {:?}", other),
    };
    let mirror_cell_sexp = env
        .frame_lookup_rust_direct("doc104-stage3b-write-through")
        .expect("mirror direct missing");
    let mirror_cell = match mirror_cell_sexp {
        Sexp::Cell(c) => c,
        other => panic!("mirror slot not Sexp::Cell: {:?}", other),
    };
    // Mutate via one handle; the other must observe.
    unsafe { mirror_cell.set_value(Sexp::Int(99)) };
    assert_eq!(
        cell_via_find.value.clone(),
        Sexp::Int(99),
        "write through mirror handle not visible via find handle"
    );
}

#[test]
fn doc104_stage3b_capture_mirror_matches_vec_capture() {
    // capture_lexical walks the elisp lexframe stack (via Phase
    // 4.b apply_function dispatch into
    // `nelisp-lexframe-stack-capture-to-depth') and returns an
    // alist of `(NAME . CELL)' with inner-shadows-outer dedup.
    // The captured-env alist key type is either Sexp::Str or
    // Sexp::Symbol depending on how the elisp helper preserves
    // the stored key (= the fast-hash bucket stores Sexp::Str).
    let mut env = Env::new_global();
    env.push_frame();
    env.bind_local("a", Sexp::Int(1));
    env.bind_local("b", Sexp::Int(2));
    env.push_frame();
    env.bind_local("a", Sexp::Int(3)); // shadows outer
    env.bind_local("c", Sexp::Int(4));

    let alist = env.capture_lexical();
    // Build a {name -> value} map from the alist.
    let mut seen: HashMap<String, Sexp> = HashMap::new();
    let mut cur = &alist;
    while let Sexp::Cons(c) = cur {
        if let Sexp::Cons(pair) = &c.car {
            let key = match &pair.car {
                Sexp::Str(k) | Sexp::Symbol(k) => Some(k.clone()),
                _ => None,
            };
            if let Some(k) = key {
                let v = match &pair.cdr {
                    Sexp::Cell(c) => c.value.clone(),
                    other => other.clone(),
                };
                seen.insert(k, v);
            }
        }
        cur = &c.cdr;
    }
    assert_eq!(seen.get("a"), Some(&Sexp::Int(3)), "inner shadow lost");
    assert_eq!(seen.get("b"), Some(&Sexp::Int(2)));
    assert_eq!(seen.get("c"), Some(&Sexp::Int(4)));
    assert_eq!(seen.len(), 3, "extra / missing names in capture");
}
