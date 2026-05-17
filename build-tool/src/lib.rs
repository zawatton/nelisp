//! NeLisp build-time tool — Doc 47 §3.1 phase 6 carve-out.
//!
//! Hosts the Doc 44 minimal interpreter (reader + evaluator + Elisp
//! self-host bridge) so the `nelisp-runtime` ship crate stays
//! image-only.  Per Doc 47 §1.1 the long-term split is:
//!
//! ```text
//! nelisp-runtime    = seed loader + image boot + syscall thin-wrappers
//!                     (target ≤ 4,000 LOC, image-only)
//! nelisp-build-tool = reader + minimal evaluator + dumper
//!                     (Doc 44 minimal interpreter lives here)
//! ```
//!
//! Stage 5a created the workspace + empty crate.  Stage 5b moved
//! `bin/nelisp.rs` here.  Stage 5c (this commit) `git mv`-ed
//! `eval/`, `reader/`, `bridge/` across from `nelisp-runtime/src/`
//! and re-wired the `anvil-runtime` consumer to depend on this
//! crate instead.  Future Stage 6+ will add the dumper that bakes a
//! heap evaluated by these modules into a `nelisp.image` v1 file.
//!
//! Doc 114: this crate is **x86_64-linux only**.  Phase 47 emit
//! (`scripts/compile-elisp-objects.el`) produces ELF64 `.o` files
//! linked directly into the crate; the dispatch sites in `eval/` and
//! `jit/` assume those symbols are present.  Cross-arch hosts must
//! build via Docker / Linux VM.  Multi-arch returns later as
//! additional Phase 47 emit branches (e.g.,
//! `lisp/nelisp-asm-aarch64.el`), never as a re-introduced Rust
//! fallback — see `docs/design/114-x86_64-linux-pivot.org`.

#[cfg(not(all(target_os = "linux", target_arch = "x86_64")))]
compile_error!(
    "Doc 114: nelisp-build-tool requires x86_64-linux \
     (Phase 47 emit is x86_64-linux only).  Build via Docker / Linux VM."
);

pub mod bridge;
pub mod eval;
pub mod image;
// Phase 5 Stage 5.0 / Doc 77b Stage b.4 — Cranelift JIT.  Lowered
// primitives flow through elisp wrappers in
// `lisp/nelisp-jit-strategy.el' that call JIT entries via the
// `nl-jit-call-*' bridge primitives; eval-loop dispatches builtins
// directly to `eval::builtins::dispatch' (no `lower_entries' hook).
//
// Phase 7.1.7.a (2026-05-10): narrowed to `pub(crate)' — the only
// crate-external surface ever needed was the `bi_*' re-exports for
// `eval::builtins::dispatch' which are siblings inside the crate.
// Keeping this `pub(crate)' lets `UnifiedJit' field types stay
// `pub(super)' without `private_interfaces' warnings.
pub(crate) mod jit;
// Reader feature gate (Doc 73 §2.4 / Doc 98 §98.3).  All production
// `reader::read_*' callsites are gone: boot reads pre-baked NELIMG v3
// frozen-heap images via `image::decode_v3_into', and `eval::eval_str'
// / `eval_str_all' route through the elisp reader.  The Rust reader
// survives only inside `image-baker' feature builds (= `nelisp-baker'
// dev tool) where `image::iterative_bake_one' parses each stdlib
// source on the way to its v3 image, plus the reader's own ERTs.
#[cfg(any(test, feature = "image-baker"))]
pub mod reader;

// Doc 99 §99.B spike — C-callable function compiled from elisp by the
// Phase 47 chain.  `build.rs' runs `scripts/compile-elisp-objects.el'
// to produce `target/<...>/elisp-objects/nelisp_spike_noop.o' and
// links it into the crate via `cargo:rustc-link-lib=static=...'.
// This module gives the symbol a Rust home so cargo doesn't dead-code-
// eliminate it from the final binary and `cargo test' can probe the
// round-trip end-to-end.  Doc 114: crate-level guard at top of lib.rs
// makes this module-level cfg redundant; non-x86_64-linux builds fail
// at the crate boundary with a clear compile_error!.
pub mod elisp_cc_spike {
    use crate::eval::sexp::Sexp;

    // Sexp is `#[repr(C, u8)]` (see `eval/sexp.rs:57' + the assertions
    // in `eval/sexp_abi_assert.rs') so passing it across an extern "C"
    // boundary by raw pointer is sound — the elisp `.o' only touches
    // bytes at the offsets `nelisp-sexp--offset-*' name (= 0 for tag,
    // 8 for the i64 payload of Sexp::Int).  Rust's `improper_ctypes'
    // lint is conservative because Sexp's variants embed a `String'
    // (which is not `#[repr(C)]'), so the lint trips even though we
    // never pass a Sexp by value.
    #[allow(improper_ctypes)]
    extern "C" {
        fn nelisp_spike_noop() -> i64;
        // Doc 99 §99.C — recursive i64 factorial from
        // `lisp/nelisp-cc-fact-i64.el'.  N must satisfy 0 ≤ N ≤ 20
        // (= the fixnum-safe range for i64); the elisp body itself
        // doesn't range-check, so callers in safe Rust must clamp.
        fn nelisp_fact_i64(n: i64) -> i64;
        // Doc 100 §100.C — `(truncate INT)' Int arm.  Reads the i64
        // payload at `*arg0' (must be `Sexp::Int' — caller's
        // precondition, not checked here) and writes a fresh
        // `Sexp::Int' with the same payload into `*result_slot'.
        // Returns `result_slot' for caller ergonomics.  Defined in
        // `lisp/nelisp-cc-truncate-int.el'.
        fn nelisp_truncate_int(arg0: *const Sexp, result_slot: *mut Sexp) -> *mut Sexp;
        // Doc 101 §101.B — `(length CONS)' proper-list walk compiled
        // from `lisp/nelisp-cc-length-cons.el'.  `arg0' must point at
        // `Sexp::Cons(_)` or `Sexp::Nil`; result is written into
        // `*result_slot` as `Sexp::Int(n)`.
        fn nelisp_length_cons(arg0: *const Sexp, result_slot: *mut Sexp) -> *mut Sexp;
        // Doc 101 §101.C — `(eq SYMBOL SYMBOL)' through
        // `lisp/nelisp-cc-eq-symbol.el'.  Returns `result_slot'
        // after writing the tag byte for `nil' or `t'.
        fn nelisp_eq_symbol(
            arg0: *const Sexp,
            arg1: *const Sexp,
            result_slot: *mut Sexp,
        ) -> *mut Sexp;
        // Doc 101 §101.D — `(cons A B)' constructor compiled from
        // `lisp/nelisp-cc-cons-construct.el'.  Writes `Sexp::Cons(_)'
        // into `*result_slot' and returns that same pointer.
        fn nelisp_cons_construct(
            arg0: *const Sexp,
            arg1: *const Sexp,
            result_slot: *mut Sexp,
        ) -> *mut Sexp;
        // Doc 117 §117.A.2 — `(string-bytes STR)' byte-length helper
        // compiled from `lisp/nelisp-cc-bi-string-bytes.el'.  `arg0'
        // must point at a `Sexp::Str' / `Sexp::Symbol' (= the Rust
        // shim in `eval/builtins.rs::bi_string_bytes' unwraps
        // `Sexp::MutStr' into the underlying `Sexp::Str' view before
        // calling).  Writes `Sexp::Int(byte_count)' into `*result_slot'
        // and returns the same pointer for caller ergonomics.
        fn nelisp_bi_string_bytes(arg0: *const Sexp, result_slot: *mut Sexp) -> *mut Sexp;
        // Doc 111 §111.B — `(recordp X)' predicate compiled from
        // `lisp/nelisp-cc-recordp.el'.  Writes Sexp::T / Sexp::Nil
        // into `*result_slot' and returns that same pointer.
        fn nelisp_recordp(arg0: *const Sexp, result_slot: *mut Sexp) -> *mut Sexp;
        // Doc 111 §111.C — `(aref VECTOR IDX)' Vector arm compiled
        // from `lisp/nelisp-cc-aref-vector.el'.  Rust pre-validates
        // that `arg0' is a Vector and `arg1' is an in-range Int.
        fn nelisp_aref_vector(
            arg0: *const Sexp,
            arg1: *const Sexp,
            result_slot: *mut Sexp,
        ) -> *mut Sexp;
        // Doc 117 §117.A.1 — `(make-vector N INIT)' allocate + fill
        // compiled from `lisp/nelisp-cc-bi-make-vector.el'.  Rust
        // pre-validates that `n_ptr' points at `Sexp::Int' with
        // N >= 0; the elisp body allocates a fresh `Sexp::Vector(N)'
        // via `vector-make' (§115.1) and fills each slot [0, N) with
        // a refcount-aware clone of `*init_ptr' via `vector-slot-set'
        // (§111.E).  Returns i64 = 1 on success (= `and' chain
        // terminator); the caller reads the fresh vector from
        // `*result_slot'.
        fn nelisp_bi_make_vector(
            n_ptr: *const Sexp,
            init_ptr: *const Sexp,
            result_slot: *mut Sexp,
        ) -> i64;
        // Doc 111 §111.D — Cell read+write ops compiled from
        // `lisp/nelisp-cc-cell-ops.el'.  Each op is a separate `.o' in
        // the static archive so the integration test in
        // `tests/phase47_cell.rs' can drive each one independently.
        //
        //   `nelisp_cell_value(arg0, slot)' — inline 32-byte copy of
        //     `NlCell.value' into `*slot'.  No refcount work (MVP,
        //     same contract as `cons-car' / `cons-cdr').
        //   `nelisp_cell_set_value(arg0, val_ptr)' — refcount-aware
        //     overwrite via `nl_cell_set_value' extern.
        //   `nelisp_cell_make(val_ptr, slot)' — allocate a fresh
        //     NlCell via `nl_alloc_cell' and write `Sexp::Cell(box)'
        //     into `*slot'.
        //   `nelisp_cell_null_p(arg0)' — i64 1 iff `NlCell.value's tag
        //     is `Sexp::Nil', else 0.
        fn nelisp_cell_value(arg0: *const Sexp, result_slot: *mut Sexp) -> *mut Sexp;
        fn nelisp_cell_set_value(arg0: *const Sexp, val_ptr: *const Sexp);
        fn nelisp_cell_make(val_ptr: *const Sexp, result_slot: *mut Sexp) -> *mut Sexp;
        fn nelisp_cell_null_p(arg0: *const Sexp) -> i64;
        // Doc 122 §122.A — `sexp-write-str' / `sexp-write-symbol' Phase
        // 47 grammar ops compiled from `lisp/nelisp-cc-sexp-write-str.el'.
        // Each op evaluates 3 args (slot, bytes_ptr, len), marshals them
        // to rdi/rsi/rdx, and calls the Rust `nl_alloc_str' /
        // `nl_alloc_symbol' extern (in `build-tool/src/eval/nlstr.rs')
        // which writes a fresh `Sexp::Str' / `Sexp::Symbol' into `*slot'
        // and returns the slot pointer.  Unlike the cell/vector/record
        // allocators these write the full 40-byte `Sexp' value inline
        // (= `Sexp::Str' / `Sexp::Symbol' carry their `String' header
        // inline at payload offset 8..32, not via an `*mut NlXXX'
        // pointer indirection — see comments in `eval/nlstr.rs').
        fn nelisp_sexp_write_str(
            slot: *mut Sexp,
            bytes_ptr: *const u8,
            len: i64,
        ) -> *mut Sexp;
        fn nelisp_sexp_write_symbol(
            slot: *mut Sexp,
            bytes_ptr: *const u8,
            len: i64,
        ) -> *mut Sexp;
        // Doc 122 §122.B — Mutable string builder Phase 47 grammar ops
        // compiled from `lisp/nelisp-cc-mut-str.el'.  Each op evaluates
        // its args, marshals them to rdi/rsi per SysV AMD64, and calls
        // the matching `nl_mut_str_*' / `nl_alloc_mut_str' Rust extern
        // (in `build-tool/src/eval/nlstr.rs').  The push/len/finalize
        // ops dereference the `Sexp::MutStr' payload pointer at offset
        // 8 to reach the inner `NlStr.value: String' (= one extra
        // indirection vs. §122.A's inline String layout, because
        // `Sexp::MutStr' wraps an `NlStrRef' / `NonNull<NlStr>').
        fn nelisp_mut_str_make_empty(slot: *mut Sexp, cap: i64) -> *mut Sexp;
        fn nelisp_mut_str_push_byte(ptr: *mut Sexp, byte: i64) -> i64;
        fn nelisp_mut_str_push_codepoint(ptr: *mut Sexp, cp: i64) -> i64;
        fn nelisp_mut_str_len(ptr: *const Sexp) -> i64;
        fn nelisp_mut_str_finalize(
            ptr: *const Sexp,
            slot: *mut Sexp,
        ) -> *mut Sexp;
        // Doc 111 §111.E #1 — `mirror_lookup_entry' Phase 47 helper
        // compiled from `lisp/nelisp-cc-mirror-lookup-entry.el'.
        // Returns the `*const Sexp' of the matching symbol-entry
        // Record (= the Sexp slot inside the bucket's (KEY . ENTRY)
        // pair NlConsBox), or 0 on miss / empty mirror.
        //
        // Pre-conditions (= caller / dispatcher responsibility,
        // mirror the Rust impl's early-`return None' arms):
        //   mirror_ptr.tag = Sexp::Record (= globals_record).
        //   mirror_ptr.slots[0].tag = Sexp::Record (= fast-hash-table).
        //   ht.slots[0] = Sexp::Int (= bucket count, power of 2).
        //   ht.slots[1] = Sexp::Vector (= buckets).
        fn nelisp_mirror_lookup_entry(
            mirror_ptr: *const Sexp,
            sym_ptr: *const Sexp,
        ) -> i64;
        // Doc 111 §111.E #2-6 — Group A compose-on-#1 helpers.  Each
        // is a thin Phase 47 object that calls
        // `nelisp_mirror_lookup_entry' via the `extern-call' grammar
        // form and adds a 1-2 op tail to read the requested
        // symbol-entry slot.  See `lisp/nelisp-cc-mirror-*.el' for
        // the per-helper source body.
        //
        //   `nelisp_mirror_lookup_value(M, S, SLOT)' — copy entry
        //      slot 0 (value cell) into SLOT via record-slot-ref, or
        //      write Sexp::Nil on miss.  Returns SLOT.
        //   `nelisp_mirror_lookup_function(M, S, SLOT)' — slot 1
        //      counterpart of value.
        //   `nelisp_mirror_is_bound(M, S, UNBOUND)' — i64 1 iff entry
        //      exists AND slot 0 != UNBOUND (= the caller-supplied
        //      `Sexp::Symbol("nelisp--unbound-marker")' sentinel).
        //   `nelisp_mirror_is_fbound(M, S, UNBOUND)' — slot 1
        //      counterpart of is_bound.
        //   `nelisp_mirror_is_constant(M, S)' — i64 1 iff entry
        //      exists AND slot 3 has tag `SEXP_TAG_T' (= 1).
        fn nelisp_mirror_lookup_value(
            mirror_ptr: *const Sexp,
            sym_ptr: *const Sexp,
            result_slot: *mut Sexp,
        ) -> *mut Sexp;
        fn nelisp_mirror_lookup_function(
            mirror_ptr: *const Sexp,
            sym_ptr: *const Sexp,
            result_slot: *mut Sexp,
        ) -> *mut Sexp;
        fn nelisp_mirror_is_bound(
            mirror_ptr: *const Sexp,
            sym_ptr: *const Sexp,
            unbound_ptr: *const Sexp,
        ) -> i64;
        fn nelisp_mirror_is_fbound(
            mirror_ptr: *const Sexp,
            sym_ptr: *const Sexp,
            unbound_ptr: *const Sexp,
        ) -> i64;
        fn nelisp_mirror_is_constant(
            mirror_ptr: *const Sexp,
            sym_ptr: *const Sexp,
        ) -> i64;
        // Doc 111 §111.E Group B (#7-#12) — env_mirror.rs write path
        // Phase 47 helpers.  Each one composes #1 `mirror_lookup_entry'
        // (via the `extern-call' grammar form) with a §111.B
        // `record-slot-set' on the matched entry's slot N.  Returns 1
        // on hit (entry slot was overwritten in place) or 0 on miss
        // (caller dispatches to the Rust auto-vivify path).
        fn nelisp_mirror_set_value(
            mirror_ptr: *const Sexp,
            sym_ptr: *const Sexp,
            val_ptr: *const Sexp,
        ) -> i64;
        fn nelisp_mirror_set_function(
            mirror_ptr: *const Sexp,
            sym_ptr: *const Sexp,
            val_ptr: *const Sexp,
        ) -> i64;
        fn nelisp_mirror_clear_value(
            mirror_ptr: *const Sexp,
            sym_ptr: *const Sexp,
            unbound_ptr: *const Sexp,
        ) -> i64;
        fn nelisp_mirror_clear_function(
            mirror_ptr: *const Sexp,
            sym_ptr: *const Sexp,
            unbound_ptr: *const Sexp,
        ) -> i64;
        fn nelisp_mirror_set_constant(
            mirror_ptr: *const Sexp,
            sym_ptr: *const Sexp,
            flag_ptr: *const Sexp,
        ) -> i64;
        fn nelisp_mirror_install_entry(
            mirror_ptr: *const Sexp,
            sym_ptr: *const Sexp,
            value_ptr: *const Sexp,
            function_ptr: *const Sexp,
            plist_ptr: *const Sexp,
            constant_ptr: *const Sexp,
        ) -> i64;
        // Doc 111 §111.E #19-#26 Group E — env_lexframe.rs Phase 47
        // rewrites.  Each `nelisp_frame_*' below is the Phase 47-
        // compiled pure-elisp implementation in
        // `lisp/nelisp-cc-frame-*.el' (Doc 115 §115.1-7).  The former
        // `nl_frame_*' Rust shims (whole Phase 47 shims module under
        // `eval/') were deleted in Doc 115 §115.8.
        fn nelisp_frame_stack_depth(frames_ptr: *const Sexp) -> i64;
        fn nelisp_frame_stack_ensure_capacity(
            frames_ptr: *const Sexp,
            needed: i64,
            scratch_slot: *mut Sexp,
        ) -> i64;
        fn nelisp_frame_push(
            frames_ptr: *const Sexp,
            scratch_vec_ptr: *const Sexp,
            _pad: i64,
        ) -> i64;
        fn nelisp_frame_pop(
            frames_ptr: *const Sexp,
            scratch_slot: *mut Sexp,
        ) -> i64;
        fn nelisp_frame_bind(
            frames_ptr: *const Sexp,
            name_ptr: *const Sexp,
            cell_ptr: *const Sexp,
            scratch_pair_slot: *mut Sexp,
            scratch_outer_slot: *mut Sexp,
            scratch_count_slot: *mut Sexp,
        ) -> i64;
        fn nelisp_frame_stack_find(
            frames_ptr: *const Sexp,
            name_ptr: *const Sexp,
        ) -> i64;
        fn nelisp_wrap_alist_cells(
            alist_ptr: *const Sexp,
            result_slot: *mut Sexp,
            work_slot: *mut Sexp,
            name_slot: *mut Sexp,
            cell_slot: *mut Sexp,
            inner_slot: *mut Sexp,
        ) -> i64;
        // Doc 115 §115.7 — pure-elisp 32-bit FNV-1a hash compiled
        // from `lisp/nelisp-cc-fnv1a.el'.  Replaces the deleted
        // Rust `mirror_fnv1a' free fn + `nl_mirror_fnv1a_sexp'
        // extern wrapper in `env_helpers.rs'.  `str_ptr' must
        // point at a `Sexp::Str(_)' or `Sexp::Symbol(_)'; the
        // helper reads bytes via `str-byte-at' (= matches the
        // 24-byte `String' header layout shared by both arms).
        // Returns: i64 — the 32-bit hash zero-extended into the
        // low 32 bits (= `(logand h #xFFFFFFFF)' after every
        // multiply guarantees the high 32 bits are 0).
        fn nelisp_fnv1a(str_ptr: *const Sexp) -> i64;
        // Doc 100 §100.D Stage 1 — 12 `nl_jit_arith_*' trampoline
        // swaps.  Defined in `lisp/nelisp-cc-jit-arith.el', wired to
        // `unified_fn_ptr' in `jit/bridge.rs::arith_link'.  These
        // declarations also pin the symbols into the test binary's
        // link line (= the rlib's `bridge::arith_link' extern block
        // alone gets DCE'd by `--gc-sections' before integration
        // tests can call them).
        pub fn nelisp_jit_add2(a: i64, b: i64) -> i64;
        pub fn nelisp_jit_sub2(a: i64, b: i64) -> i64;
        pub fn nelisp_jit_mul2(a: i64, b: i64) -> i64;
        pub fn nelisp_jit_eq2(a: i64, b: i64) -> i64;
        pub fn nelisp_jit_lt2(a: i64, b: i64) -> i64;
        pub fn nelisp_jit_gt2(a: i64, b: i64) -> i64;
        pub fn nelisp_jit_le2(a: i64, b: i64) -> i64;
        pub fn nelisp_jit_ge2(a: i64, b: i64) -> i64;
        pub fn nelisp_jit_logior2(a: i64, b: i64) -> i64;
        pub fn nelisp_jit_logand2(a: i64, b: i64) -> i64;
        pub fn nelisp_jit_logxor2(a: i64, b: i64) -> i64;
        pub fn nelisp_jit_ash(n: i64, c: i64) -> i64;
        // Doc 110 §110.E.2.a — `jit/float.rs' 4 arithmetic trampoline
        // swaps (add / sub / mul / div).  Defined in
        // `lisp/nelisp-cc-jit-float.el'; wired to `unified_fn_ptr' via
        // the `float_link' module in `jit/bridge.rs'.  These decls
        // also pin the symbols into the integration test binary's
        // link line (= the rlib's `float_link' extern block alone
        // gets DCE'd by `--gc-sections' before tests can call them).
        pub fn nl_jit_float_add(a: f64, b: f64) -> f64;
        pub fn nl_jit_float_sub(a: f64, b: f64) -> f64;
        pub fn nl_jit_float_mul(a: f64, b: f64) -> f64;
        pub fn nl_jit_float_div(a: f64, b: f64) -> f64;
        // Doc 110 §110.C.2.a — 4 ordered comparison trampoline swaps.
        // i64 return matches the `extern "C" fn(f64, f64) -> i64'
        // float.rs cmp shape.  NaN semantics: 0 (= matches Rust).
        pub fn nl_jit_float_lt(a: f64, b: f64) -> i64;
        pub fn nl_jit_float_gt(a: f64, b: f64) -> i64;
        pub fn nl_jit_float_le(a: f64, b: f64) -> i64;
        pub fn nl_jit_float_ge(a: f64, b: f64) -> i64;
        // Doc 110 §110.C.2.b — EQ-EPS trampoline.  Returns 1 iff
        // `(a - b).abs() < 1e-15' AND both inputs are ordered (=
        // not NaN), matching the Rust float.rs body bit-for-bit.
        pub fn nl_jit_float_eq_eps(a: f64, b: f64) -> i64;
        // Doc 110 §110.F — 3 `jit/math.rs' trampoline swaps.
        // `float' = identity, `exp' / `log' = libm extern call
        // through the new `(f64-call)' grammar form.  Shape is
        // `extern "C" fn(f64) -> f64' (= xmm0/d0 in, xmm0/d0 out).
        pub fn nl_jit_float_float(x: f64) -> f64;
        pub fn nl_jit_float_exp(x: f64) -> f64;
        pub fn nl_jit_float_log(x: f64) -> f64;
        // Doc 120 §120.A — `jit/predicate.rs' 2 of 4 trampoline swaps
        // (`predicate_eq' + `ref_eq'; `sxhash' + `type_of' stay Rust).
        // Defined in `lisp/nelisp-cc-jit-predicate-eq.el' +
        // `lisp/nelisp-cc-jit-ref-eq.el'; wired to `unified_fn_ptr' via
        // the `predicate_link' module in `jit/bridge.rs'.  These decls
        // also pin the symbols into the integration test binary's
        // link line (= the rlib's `predicate_link' extern block alone
        // gets DCE'd by `--gc-sections' before tests can call them).
        pub fn nelisp_jit_predicate_eq(a: *const Sexp, b: *const Sexp) -> i64;
        pub fn nelisp_jit_ref_eq(
            a: *const Sexp,
            b: *const Sexp,
            out: *mut Sexp,
        ) -> i64;
        // Doc 120 §120.B — `jit/box_accessor.rs' 4 of 11 record-family
        // trampoline swaps (`record_type' / `record_len' / `record_ref'
        // / `record_set'; `record_alloc' stays Rust + 6 non-record
        // entries SKIP per blocker notes in `jit/box_accessor.rs').
        // Defined in `lisp/nelisp-cc-jit-record.el'; wired to
        // `unified_fn_ptr' via the `box_accessor_link' module in
        // `jit/bridge.rs'.  Same dead-symbol-on-rlib pinning rationale
        // as the §120.A externs above.
        pub fn nelisp_jit_record_type(arg: *const Sexp, out: *mut Sexp) -> i64;
        pub fn nelisp_jit_record_len(arg: *const Sexp, out: *mut Sexp) -> i64;
        pub fn nelisp_jit_record_ref(
            arg: *const Sexp,
            idx: i64,
            out: *mut Sexp,
        ) -> i64;
        pub fn nelisp_jit_record_set(
            arg: *const Sexp,
            idx: i64,
            val: *const Sexp,
            out: *mut Sexp,
        ) -> i64;
        // Doc 120 §120.D — `jit/access.rs' 4 of 4 trampoline swaps
        // (`length' / `aref' / `aset' / `elt').  Defined in
        // `lisp/nelisp-cc-jit-{length,aref,aset,elt}.el'; wired to
        // `unified_fn_ptr' via the `access_link' module in
        // `jit/bridge.rs'.  Same dead-symbol-on-rlib pinning rationale
        // as the §120.A / §120.B externs above.  The Str / BoolVector
        // sub-arms reach narrow Rust externs in `jit/access.rs' via
        // `extern-call' from the elisp bodies (= same shape
        // `nl_sexp_eq' uses for the §120.A predicate-eq slow path).
        pub fn nelisp_jit_length(arg: *const Sexp, out: *mut Sexp) -> i64;
        pub fn nelisp_jit_aref(arg: *const Sexp, idx: i64, out: *mut Sexp) -> i64;
        pub fn nelisp_jit_aset(
            arg: *const Sexp,
            idx: i64,
            val: *const Sexp,
            out: *mut Sexp,
        ) -> i64;
        pub fn nelisp_jit_elt(arg: *const Sexp, idx: i64, out: *mut Sexp) -> i64;
    }

    /// Doc 99 §99.B probe — call the elisp-compiled function and return
    /// its result.  Used by `tests/elisp_cc_spike_probe.rs' to prove the
    /// build chain (elisp source → Phase 47 compile → ET_REL .o → ar
    /// static archive → cargo link) terminates in a callable symbol.
    pub fn probe() -> i64 {
        unsafe { nelisp_spike_noop() }
    }

    /// Doc 99 §99.C — i64 factorial implemented in elisp.  Wraps the
    /// `nelisp_fact_i64' extern.  The caller is responsible for the
    /// 0..=20 range invariant; the Rust shim in `eval::builtins'
    /// enforces it before calling.
    pub fn fact_i64(n: i64) -> i64 {
        unsafe { nelisp_fact_i64(n) }
    }

    /// Doc 100 §100.C — `(truncate INT)' Int arm, elisp-compiled.
    ///
    /// `arg0' must point at a valid `Sexp::Int' value; `result_slot'
    /// must point at a 32-byte writable Sexp slot.  The elisp body
    /// reads the i64 payload of `*arg0' and writes `Sexp::Int(same)'
    /// into `*result_slot'.  No allocation, no Rust helpers — every
    /// memory access is a single `disp8' load / store emitted by
    /// Phase 47's `sexp-int-unwrap' / `sexp-int-make' grammar forms.
    ///
    /// # Safety
    ///
    /// - `arg0' must be a non-null pointer to an initialized `Sexp::Int'.
    /// - `result_slot' must be a non-null, properly aligned 32-byte
    ///   writable region (= `&mut MaybeUninit<Sexp>' or
    ///   `&mut Sexp::Nil' both work).  The elisp body writes bytes
    ///   `[0, 1)` and `[8, 16)` only; the remaining bytes are left
    ///   unmodified, so callers that read them must initialize first.
    pub unsafe fn truncate_int(arg0: *const Sexp, result_slot: *mut Sexp) -> *mut Sexp {
        nelisp_truncate_int(arg0, result_slot)
    }

    /// Doc 101 §101.B — `(length CONS)' proper-list walk, elisp-compiled.
    ///
    /// `arg0' must point at `Sexp::Cons(_)` or `Sexp::Nil`;
    /// `result_slot' must point at a writable 32-byte Sexp slot.
    /// The elisp body walks the raw `NlConsBox*` chain and writes
    /// `Sexp::Int(list-length)` into `*result_slot`.
    pub unsafe fn length_cons(arg0: *const Sexp, result_slot: *mut Sexp) -> *mut Sexp {
        nelisp_length_cons(arg0, result_slot)
    }

    /// Doc 101 §101.C — `(eq SYMBOL SYMBOL)' via elisp-compiled
    /// Symbol/Str read ops.
    ///
    /// `arg0' and `arg1' must point at valid `Sexp` values.
    /// `slot' must point at a writable 32-byte Sexp slot.
    ///
    /// # Safety
    ///
    /// - `arg0' / `arg1' must be non-null pointers to initialized `Sexp`s.
    /// - `slot' must be non-null and writable for at least one Sexp slot.
    pub unsafe fn eq_symbol(arg0: *const Sexp, arg1: *const Sexp, slot: *mut Sexp) -> *mut Sexp {
        nelisp_eq_symbol(arg0, arg1, slot)
    }

    /// Doc 101 §101.D — `(cons A B)' via elisp-compiled Cons
    /// construction ops.
    ///
    /// `arg0` / `arg1` must point at initialized `Sexp` values and
    /// `slot` must point at a writable 32-byte Sexp slot.
    pub unsafe fn cons_construct(arg0: *const Sexp, arg1: *const Sexp, slot: *mut Sexp) -> *mut Sexp {
        nelisp_cons_construct(arg0, arg1, slot)
    }

    /// Doc 117 §117.A.2 — `(string-bytes STR)' byte-length via the
    /// elisp-compiled `str-len' + `sexp-int-make' pair.
    ///
    /// `arg0' must point at `Sexp::Str(_)' or `Sexp::Symbol(_)' — the
    /// Rust shim in `eval::builtins::bi_string_bytes' unwraps
    /// `Sexp::MutStr' into the underlying `Sexp::Str' view before
    /// calling, so this safe wrapper does not need a MutStr-aware path.
    /// `slot' must point at a writable 32-byte Sexp slot
    /// (`Sexp::Nil' default-init works because the elisp body
    /// overwrites bytes `[0, 1)` for the tag plus `[8, 16)` for the
    /// i64 payload).  Returns `slot' for caller ergonomics.
    ///
    /// # Safety
    ///
    /// - `arg0' must be non-null and point at an initialized
    ///   `Sexp::Str' / `Sexp::Symbol'.
    /// - `slot' must be non-null, properly aligned, and writable for
    ///   one Sexp slot.
    pub unsafe fn bi_string_bytes(arg0: *const Sexp, slot: *mut Sexp) -> *mut Sexp {
        nelisp_bi_string_bytes(arg0, slot)
    }

    /// Doc 111 §111.B — `(recordp X)' via elisp-compiled Record ops.
    pub unsafe fn recordp(arg0: *const Sexp, slot: *mut Sexp) -> *mut Sexp {
        nelisp_recordp(arg0, slot)
    }

    /// Doc 111 §111.C — `(aref VECTOR IDX)' via elisp-compiled
    /// Vector read ops.
    ///
    /// The compiled body emits a `vector-ref' op that copies the
    /// selected element into `slot` via the refcount-aware
    /// `nl_sexp_clone_into' helper (Doc 111 §111.C v3 fix).  The
    /// caller-provided `slot' must be initialized to `Sexp::Nil'
    /// (bit-shape Copy) so the helper's `ptr::write' does not Drop
    /// arbitrary bytes.
    pub unsafe fn aref_vector(
        arg0: *const Sexp,
        arg1: *const Sexp,
        slot: *mut Sexp,
    ) -> *mut Sexp {
        nelisp_aref_vector(arg0, arg1, slot)
    }

    /// Doc 117 §117.A.1 — `(make-vector N INIT)' via elisp-compiled
    /// `vector-make' (§115.1) + `vector-slot-set' (§111.E) fill loop.
    ///
    /// `n_ptr` must point at a `Sexp::Int` with payload N >= 0 (= the
    /// Rust dispatcher in `eval::builtins::bi_make_vector' enforces
    /// the non-negative invariant before calling).  `init_ptr` may
    /// point at any Sexp shape; `vector-slot-set' clones it once per
    /// slot via the refcount-aware `nl_vector_set_slot' helper, so
    /// the caller's `*init_ptr' refcount is preserved.  `slot' must
    /// point at a writable 32-byte Sexp slot pre-initialised to
    /// `Sexp::Nil'; on return it holds the fresh
    /// `Sexp::Vector(NlVectorRef)' with N copies of `*init_ptr'.
    /// Returns `i64' = 1 on success (= the `and' chain terminator
    /// inside the elisp body — same convention as `frame_push'
    /// (§115.3) / `frame_pop' (§115.2)).
    ///
    /// # Safety
    ///
    /// - `n_ptr` must be non-null and point at an initialised
    ///   `Sexp::Int(N)` with N >= 0.
    /// - `init_ptr` must be non-null and point at an initialised
    ///   `Sexp` value.
    /// - `slot` must be non-null, properly aligned, and writable for
    ///   one Sexp slot.  Initialise it to `Sexp::Nil` so the
    ///   `vector-make' op's tag + payload writes do not collide with
    ///   a live heap-tagged Sexp drop.
    pub unsafe fn bi_make_vector(
        n_ptr: *const Sexp,
        init_ptr: *const Sexp,
        slot: *mut Sexp,
    ) -> i64 {
        nelisp_bi_make_vector(n_ptr, init_ptr, slot)
    }

    /// Doc 111 §111.D — `(cell-value H SLOT)' via elisp-compiled Cell ops.
    ///
    /// `arg0` must point at `Sexp::Cell(_)`; `slot` must point at a
    /// writable 32-byte Sexp slot.  The elisp body copies the cell's
    /// inline `value` Sexp (= NlCell offset 0) into `*slot` via two
    /// 16-byte `movdqu' pairs.  No refcount maintenance — see the
    /// MVP note in `lisp/nelisp-cc-cell-ops.el'.
    ///
    /// # Safety
    /// - `arg0` must be non-null and point at `Sexp::Cell(NlCellRef)`.
    /// - `slot` must be non-null, properly aligned, and writable for
    ///   one Sexp slot (32 bytes).
    pub unsafe fn cell_value(arg0: *const Sexp, slot: *mut Sexp) -> *mut Sexp {
        nelisp_cell_value(arg0, slot)
    }

    /// Doc 111 §111.D — `(cell-set-value H VAL)' via elisp-compiled Cell ops.
    ///
    /// Delegates to the Rust extern `nl_cell_set_value' which calls
    /// `NlCellRef::set_value' (drop-then-write with refcount-aware
    /// semantics via `Sexp::Drop' + `Sexp::Clone').
    ///
    /// # Safety
    /// - `arg0` must be non-null and point at `Sexp::Cell(NlCellRef)`.
    /// - `val_ptr` must be non-null and point at an initialized `Sexp`.
    /// - No other `&Sexp` borrow into the cell's `value` may be live.
    pub unsafe fn cell_set_value(arg0: *const Sexp, val_ptr: *const Sexp) {
        nelisp_cell_set_value(arg0, val_ptr)
    }

    /// Doc 111 §111.D — `(cell-make VAL SLOT)' via elisp-compiled Cell ops.
    ///
    /// Allocates a fresh `NlCell` via the Rust extern `nl_alloc_cell'
    /// (refcount-aware clone of `*val_ptr` into the new cell) and
    /// writes `Sexp::Cell(box)` into `*slot`.
    ///
    /// # Safety
    /// - `val_ptr` must be non-null and point at an initialized `Sexp`.
    /// - `slot` must be non-null, properly aligned, and writable for
    ///   one Sexp slot (32 bytes).
    pub unsafe fn cell_make(val_ptr: *const Sexp, slot: *mut Sexp) -> *mut Sexp {
        nelisp_cell_make(val_ptr, slot)
    }

    /// Doc 111 §111.D — `(cell-null-p H)' via elisp-compiled Cell ops.
    ///
    /// Returns 1 iff the cell's `value` Sexp currently has tag
    /// `Sexp::Nil', else 0.  Inline tag check, no extern call.
    ///
    /// # Safety
    /// - `arg0` must be non-null and point at `Sexp::Cell(NlCellRef)`.
    pub unsafe fn cell_null_p(arg0: *const Sexp) -> i64 {
        nelisp_cell_null_p(arg0)
    }

    /// Doc 122 §122.A — `(sexp-write-str SLOT BYTES-PTR LEN)' via
    /// elisp-compiled Phase 47 grammar op.
    ///
    /// Calls the Rust `nl_alloc_str' extern which copies `len' bytes
    /// from `bytes_ptr' into a fresh `String', wraps as
    /// `Sexp::Str(_)', and writes the resulting 40-byte Sexp value
    /// into `*slot'.  Returns `slot' for caller ergonomics.
    ///
    /// # Safety
    /// - `slot' must be non-null, properly aligned, and writable for
    ///   one `Sexp' slot (40 bytes).  Pre-initialise to `Sexp::Nil`
    ///   so the inline `ptr::write' does not drop arbitrary bytes
    ///   (same convention as `cons_construct' / `cell_make').
    /// - `bytes_ptr' must be non-null when `len > 0' and point at
    ///   `len' initialized bytes of valid UTF-8.  `len == 0' permits
    ///   a dangling-but-aligned `bytes_ptr'.
    pub unsafe fn sexp_write_str(
        slot: *mut Sexp,
        bytes_ptr: *const u8,
        len: i64,
    ) -> *mut Sexp {
        nelisp_sexp_write_str(slot, bytes_ptr, len)
    }

    /// Doc 122 §122.A — `(sexp-write-symbol SLOT BYTES-PTR LEN)' via
    /// elisp-compiled Phase 47 grammar op.
    ///
    /// Same shape as [`sexp_write_str`] but produces `Sexp::Symbol(_)`
    /// instead of `Sexp::Str(_)'.  Does NOT consult any intern table —
    /// see Doc 122 §5 open question.
    ///
    /// # Safety
    /// Identical to [`sexp_write_str`].
    pub unsafe fn sexp_write_symbol(
        slot: *mut Sexp,
        bytes_ptr: *const u8,
        len: i64,
    ) -> *mut Sexp {
        nelisp_sexp_write_symbol(slot, bytes_ptr, len)
    }

    /// Doc 122 §122.B — `(mut-str-make-empty SLOT CAP)' via
    /// elisp-compiled Phase 47 grammar op.
    ///
    /// Calls `nl_alloc_mut_str(cap, slot)' which allocates a fresh
    /// `NlStrRef::new(String::with_capacity(cap))' and writes
    /// `Sexp::MutStr(rc)' into `*slot'.  Returns `slot'.
    ///
    /// # Safety
    /// - `slot' must be non-null, properly aligned, and writable for
    ///   one `Sexp' slot.  Pre-init to `Sexp::Nil'.
    /// - `cap' must be `>= 0' (negative values are clamped to 0).
    pub unsafe fn mut_str_make_empty(slot: *mut Sexp, cap: i64) -> *mut Sexp {
        nelisp_mut_str_make_empty(slot, cap)
    }

    /// Doc 122 §122.B — `(mut-str-push-byte PTR BYTE)' via Phase 47
    /// grammar op.  Wraps `nl_mut_str_push_byte'; returns the i64
    /// sentinel 1 left in rax by the emit code.
    ///
    /// # Safety
    /// `ptr' must be non-null and point at a live `Sexp::MutStr'.
    /// See `nl_mut_str_push_byte' for the full contract.
    pub unsafe fn mut_str_push_byte(ptr: *mut Sexp, byte: i64) -> i64 {
        nelisp_mut_str_push_byte(ptr, byte)
    }

    /// Doc 122 §122.B — `(mut-str-push-codepoint PTR CP)' via Phase
    /// 47 grammar op.  Wraps `nl_mut_str_push_codepoint'.
    ///
    /// # Safety
    /// Same as [`mut_str_push_byte`].
    pub unsafe fn mut_str_push_codepoint(ptr: *mut Sexp, cp: i64) -> i64 {
        nelisp_mut_str_push_codepoint(ptr, cp)
    }

    /// Doc 122 §122.B — `(mut-str-len PTR)' via Phase 47 grammar op.
    /// Wraps `nl_mut_str_len'; returns the current byte length.
    ///
    /// # Safety
    /// `ptr' must be non-null and point at a live `Sexp::MutStr'.
    pub unsafe fn mut_str_len(ptr: *const Sexp) -> i64 {
        nelisp_mut_str_len(ptr)
    }

    /// Doc 122 §122.B — `(mut-str-finalize PTR SLOT)' via Phase 47
    /// grammar op.  Wraps `nl_mut_str_finalize'; clones the inner
    /// `String' into a fresh `Sexp::Str' written into `*slot'.
    /// Source MutStr remains live + push-able.
    ///
    /// # Safety
    /// Both pointers must satisfy the contracts on their underlying
    /// externs.  See `nl_mut_str_finalize'.
    pub unsafe fn mut_str_finalize(
        ptr: *const Sexp,
        slot: *mut Sexp,
    ) -> *mut Sexp {
        nelisp_mut_str_finalize(ptr, slot)
    }

    /// Doc 111 §111.E #1 — Phase 47 `mirror_lookup_entry' probe wrapper.
    ///
    /// Walks the env-mirror fast-hash-table for `sym_ptr` (= a
    /// `Sexp::Symbol(_)' / `Sexp::Str(_)') and returns the raw
    /// `*const Sexp' of the matching symbol-entry Record, or 0 on
    /// miss / empty mirror.  The returned pointer is *not* refcount-
    /// bumped — it borrows the slot owned by the bucket's `(KEY .
    /// ENTRY)` cons pair, so callers must clone (`nl_sexp_clone_into`)
    /// before storing the result anywhere that outlives the mirror.
    ///
    /// Used by `tests/elisp_cc_mirror_lookup_entry_probe.rs' to drive
    /// the §111.E #1 verification gate.  Production callers (= the
    /// env_mirror.rs Rust impl + its 5 compose-on-it siblings) still
    /// route through `Env::mirror_lookup_entry' — the extern wrapper
    /// dispatch swap lands in a follow-up commit after all Group A/B
    /// helpers ship.
    ///
    /// # Safety
    /// - `mirror_ptr' must be non-null and point at a `Sexp::Record(_)'
    ///   built by `Env::install_empty_mirror_rust_direct' (or its
    ///   `mirror_install_entry' descendants).  The pre-conditions
    ///   listed on the extern decl must hold.
    /// - `sym_ptr' must be non-null and point at a `Sexp::Symbol(_)`
    ///   or `Sexp::Str(_)`.
    /// - The returned pointer is only valid while `*mirror_ptr` and
    ///   its bucket chain remain unchanged.
    pub unsafe fn mirror_lookup_entry(
        mirror_ptr: *const Sexp,
        sym_ptr: *const Sexp,
    ) -> *const Sexp {
        nelisp_mirror_lookup_entry(mirror_ptr, sym_ptr) as *const Sexp
    }

    /// Doc 111 §111.E #2 — Phase 47 `mirror_lookup_value' probe wrapper.
    pub unsafe fn mirror_lookup_value(
        mirror_ptr: *const Sexp,
        sym_ptr: *const Sexp,
        result_slot: *mut Sexp,
    ) -> *mut Sexp {
        nelisp_mirror_lookup_value(mirror_ptr, sym_ptr, result_slot)
    }

    /// Doc 111 §111.E #3 — Phase 47 `mirror_lookup_function' probe wrapper.
    pub unsafe fn mirror_lookup_function(
        mirror_ptr: *const Sexp,
        sym_ptr: *const Sexp,
        result_slot: *mut Sexp,
    ) -> *mut Sexp {
        nelisp_mirror_lookup_function(mirror_ptr, sym_ptr, result_slot)
    }

    /// Doc 111 §111.E #4 — Phase 47 `mirror_is_bound' probe wrapper.
    pub unsafe fn mirror_is_bound(
        mirror_ptr: *const Sexp,
        sym_ptr: *const Sexp,
        unbound_ptr: *const Sexp,
    ) -> i64 {
        nelisp_mirror_is_bound(mirror_ptr, sym_ptr, unbound_ptr)
    }

    /// Doc 111 §111.E #5 — Phase 47 `mirror_is_fbound' probe wrapper.
    pub unsafe fn mirror_is_fbound(
        mirror_ptr: *const Sexp,
        sym_ptr: *const Sexp,
        unbound_ptr: *const Sexp,
    ) -> i64 {
        nelisp_mirror_is_fbound(mirror_ptr, sym_ptr, unbound_ptr)
    }

    /// Doc 111 §111.E #6 — Phase 47 `mirror_is_constant' probe wrapper.
    pub unsafe fn mirror_is_constant(
        mirror_ptr: *const Sexp,
        sym_ptr: *const Sexp,
    ) -> i64 {
        nelisp_mirror_is_constant(mirror_ptr, sym_ptr)
    }

    /// Doc 111 §111.E #7 — Phase 47 `mirror_set_value' probe wrapper.
    pub unsafe fn mirror_set_value(
        mirror_ptr: *const Sexp,
        sym_ptr: *const Sexp,
        val_ptr: *const Sexp,
    ) -> i64 {
        nelisp_mirror_set_value(mirror_ptr, sym_ptr, val_ptr)
    }

    /// Doc 111 §111.E #8 — Phase 47 `mirror_set_function' probe wrapper.
    pub unsafe fn mirror_set_function(
        mirror_ptr: *const Sexp,
        sym_ptr: *const Sexp,
        val_ptr: *const Sexp,
    ) -> i64 {
        nelisp_mirror_set_function(mirror_ptr, sym_ptr, val_ptr)
    }

    /// Doc 111 §111.E #9 — Phase 47 `mirror_clear_value' probe wrapper.
    pub unsafe fn mirror_clear_value(
        mirror_ptr: *const Sexp,
        sym_ptr: *const Sexp,
        unbound_ptr: *const Sexp,
    ) -> i64 {
        nelisp_mirror_clear_value(mirror_ptr, sym_ptr, unbound_ptr)
    }

    /// Doc 111 §111.E #10 — Phase 47 `mirror_clear_function' probe wrapper.
    pub unsafe fn mirror_clear_function(
        mirror_ptr: *const Sexp,
        sym_ptr: *const Sexp,
        unbound_ptr: *const Sexp,
    ) -> i64 {
        nelisp_mirror_clear_function(mirror_ptr, sym_ptr, unbound_ptr)
    }

    /// Doc 111 §111.E #11 — Phase 47 `mirror_set_constant' probe wrapper.
    pub unsafe fn mirror_set_constant(
        mirror_ptr: *const Sexp,
        sym_ptr: *const Sexp,
        flag_ptr: *const Sexp,
    ) -> i64 {
        nelisp_mirror_set_constant(mirror_ptr, sym_ptr, flag_ptr)
    }

    /// Doc 111 §111.E #12 — Phase 47 `mirror_install_entry' probe wrapper.
    pub unsafe fn mirror_install_entry(
        mirror_ptr: *const Sexp,
        sym_ptr: *const Sexp,
        value_ptr: *const Sexp,
        function_ptr: *const Sexp,
        plist_ptr: *const Sexp,
        constant_ptr: *const Sexp,
    ) -> i64 {
        nelisp_mirror_install_entry(
            mirror_ptr, sym_ptr,
            value_ptr, function_ptr, plist_ptr, constant_ptr,
        )
    }

    // ---- Doc 111 §111.E Group E (env_lexframe.rs) probes -----------

    pub unsafe fn frame_stack_depth(frames_ptr: *const Sexp) -> i64 {
        nelisp_frame_stack_depth(frames_ptr)
    }

    pub unsafe fn frame_stack_ensure_capacity(
        frames_ptr: *const Sexp,
        needed: i64,
    ) -> i64 {
        // Doc 115 §115.1 — the pure-elisp implementation in
        // `lisp/nelisp-cc-frame-ensure-capacity.el' takes a 3rd
        // `scratch_slot' parameter (= caller-owned `*mut Sexp' to
        // hold the freshly-allocated `Sexp::Vector' before installing
        // it into the frames-record's slot 0).  We allocate a stack-
        // local `Sexp::Nil' here and pass its pointer so the public
        // 2-arg API is preserved for existing callers / probes.
        let mut scratch = Sexp::Nil;
        nelisp_frame_stack_ensure_capacity(
            frames_ptr,
            needed,
            &mut scratch as *mut Sexp,
        )
    }

    pub unsafe fn frame_push(frames_ptr: *const Sexp) -> i64 {
        // Doc 115 §115.3 — the pure-elisp implementation in
        // `lisp/nelisp-cc-frame-push.el' takes a 7-slot `Sexp::Vector'
        // scratch (= two type-tag symbols + five scratch slots) that
        // the safe wrapper allocates internally so the public 1-arg
        // API is preserved.  See the elisp commentary for the per-slot
        // layout and refcount discipline.  The third `_pad' i64 arg is
        // ignored by the helper but flips outer-defun arity to odd (=
        // 3), which matches the static rsp-alignment assumption baked
        // into `vector-make' / `record-make' / `vector-slot-set'.
        let scratch_vec = Sexp::vector(vec![
            Sexp::Symbol("nelisp-lexframe".into()),
            Sexp::Symbol("fast-hash-table".into()),
            Sexp::Nil, // ensure_capacity scratch
            Sexp::Nil, // bucket vector scratch
            Sexp::Nil, // fast-hash-table record scratch
            Sexp::Nil, // lexframe record scratch
            Sexp::Nil, // reusable Sexp::Int scratch
        ]);
        nelisp_frame_push(
            frames_ptr,
            &scratch_vec as *const Sexp,
            0,
        )
    }

    pub unsafe fn frame_pop(frames_ptr: *const Sexp) -> i64 {
        // Doc 115 §115.2 — the pure-elisp implementation in
        // `lisp/nelisp-cc-frame-pop.el' takes a 2nd `scratch_slot'
        // parameter (= caller-owned `*mut Sexp' initialised to
        // `Sexp::Nil'; used both as the Nil-source for `vector-slot-
        // set' on the backing element and then overwritten in place
        // with the new depth `Sexp::Int' before `record-slot-set' on
        // slot 1).  We allocate a stack-local `Sexp::Nil' here and
        // pass its pointer so the public 1-arg API is preserved for
        // existing callers / probes.
        let mut scratch = Sexp::Nil;
        nelisp_frame_pop(frames_ptr, &mut scratch as *mut Sexp)
    }

    pub unsafe fn frame_bind(
        frames_ptr: *const Sexp,
        name_ptr: *const Sexp,
        cell_ptr: *const Sexp,
    ) -> i64 {
        // Doc 115 §115.5 — the pure-elisp implementation in
        // `lisp/nelisp-cc-frame-bind.el' takes 3 caller-owned scratch
        // `*mut Sexp' slots (= the freshly-allocated inner pair, outer
        // bucket cell, and `Sexp::Int(new-count)' for the slot-2 bump).
        // We allocate stack-local `Sexp::Nil's here and pass their
        // pointers so the public 3-arg API is preserved for existing
        // callers / probes.
        let mut scratch_pair = Sexp::Nil;
        let mut scratch_outer = Sexp::Nil;
        let mut scratch_count = Sexp::Nil;
        nelisp_frame_bind(
            frames_ptr,
            name_ptr,
            cell_ptr,
            &mut scratch_pair as *mut Sexp,
            &mut scratch_outer as *mut Sexp,
            &mut scratch_count as *mut Sexp,
        )
    }

    pub unsafe fn frame_stack_find(
        frames_ptr: *const Sexp,
        name_ptr: *const Sexp,
    ) -> *const Sexp {
        // Doc 115 §115.6 — the Rust shim `nl_frame_stack_find' has
        // been replaced by the pure-elisp implementation in
        // `lisp/nelisp-cc-frame-stack-find.el'.  The elisp body
        // returns the cell-slot pointer encoded as i64 (or 0 on
        // miss); we widen back to `*const Sexp' for the public API.
        nelisp_frame_stack_find(frames_ptr, name_ptr) as *const Sexp
    }

    pub unsafe fn wrap_alist_cells(
        alist_ptr: *const Sexp,
        result_slot: *mut Sexp,
    ) -> i64 {
        // Doc 115 §115.4 — the pure-elisp implementation in
        // `lisp/nelisp-cc-wrap-alist-cells.el' takes 4 extra scratch-
        // slot pointers for the per-iteration cell-wrap +
        // name-clone-into + single-slot ping-pong outer-cons build.
        // Stack-allocate them here as `Sexp::Nil' so the public 2-arg
        // API is preserved for existing callers.
        let mut work_slot = Sexp::Nil;
        let mut name_slot = Sexp::Nil;
        let mut cell_slot = Sexp::Nil;
        let mut inner_slot = Sexp::Nil;
        let rc = nelisp_wrap_alist_cells(
            alist_ptr,
            result_slot,
            &mut work_slot as *mut Sexp,
            &mut name_slot as *mut Sexp,
            &mut cell_slot as *mut Sexp,
            &mut inner_slot as *mut Sexp,
        );
        // Mem-forget the work slots' contents to prevent the
        // wrapper-local Drops from decrementing refcounts on heap
        // nodes that the result chain already accounts for.  Phase
        // 47's `cons-make' / `cell-make' raw 32-byte copies do not
        // bump refcount on nested boxed payloads (= MVP ownership
        // constraint), so each result-chain reference is matched 1:1
        // with the original `+1' from `nl_alloc_consbox' /
        // `nl_alloc_cell' / `nl_sexp_clone_into'.  The work slots
        // hold "extra" handles whose refcount-claim was never
        // actually counted; Drop on those would underflow.
        core::ptr::write(&mut work_slot as *mut Sexp, Sexp::Nil);
        core::ptr::write(&mut name_slot as *mut Sexp, Sexp::Nil);
        core::ptr::write(&mut cell_slot as *mut Sexp, Sexp::Nil);
        core::ptr::write(&mut inner_slot as *mut Sexp, Sexp::Nil);
        rc
    }

    /// Doc 115 §115.7 — pure-elisp 32-bit FNV-1a hash, safe wrapper.
    ///
    /// `str_ptr' must be non-null and point at a `Sexp::Str(_)' or
    /// `Sexp::Symbol(_)' (= the two arms whose payload is a `String'
    /// with byte data at `str_ptr + 24..(+ 24 len)').  The body reads
    /// `len' bytes via `str-byte-at' and returns the 32-bit FNV-1a
    /// hash zero-extended to `i64'.  Empty input returns the FNV
    /// offset basis (= `0x811C9DC5' = 2166136261).
    ///
    /// For ASCII inputs (= every elisp identifier that lands in the
    /// env mirror) the result is bit-equal to the deleted Rust
    /// `mirror_fnv1a' free fn.  Non-ASCII inputs diverge (= the elisp
    /// path iterates bytes while the Rust path iterated Unicode
    /// codepoints) but the env mirror never receives non-ASCII keys
    /// so the divergence is documentation-only.
    ///
    /// # Safety
    /// - `str_ptr' must be non-null and point at an initialized
    ///   `Sexp::Str' or `Sexp::Symbol' value.  Any other tag is
    ///   undefined behaviour (= the helper reads `String::len' at
    ///   offset 24 and `String::ptr' at offset 16; both fields are
    ///   only meaningful for the Str / Symbol arms).
    pub unsafe fn fnv1a(str_ptr: *const Sexp) -> i64 {
        nelisp_fnv1a(str_ptr)
    }

    /// Doc 100 §100.D Stage 1 probes — thin safe wrappers around the
    /// 12 elisp-compiled jit/arith trampolines.  Used by
    /// `tests/elisp_cc_jit_arith_probe.rs' to assert every member of
    /// the `.o' archive resolves and computes the expected i64 result.
    /// `unified_fn_ptr` in `jit/bridge.rs` resolves the same symbols
    /// at runtime via the `arith_link' submodule, but those
    /// references alone get DCE'd in test-bin builds; surfacing the
    /// externs through `pub fn' here pins the link.
    pub fn jit_add2(a: i64, b: i64) -> i64 { unsafe { nelisp_jit_add2(a, b) } }
    pub fn jit_sub2(a: i64, b: i64) -> i64 { unsafe { nelisp_jit_sub2(a, b) } }
    pub fn jit_mul2(a: i64, b: i64) -> i64 { unsafe { nelisp_jit_mul2(a, b) } }
    pub fn jit_eq2(a: i64, b: i64) -> i64 { unsafe { nelisp_jit_eq2(a, b) } }
    pub fn jit_lt2(a: i64, b: i64) -> i64 { unsafe { nelisp_jit_lt2(a, b) } }
    pub fn jit_gt2(a: i64, b: i64) -> i64 { unsafe { nelisp_jit_gt2(a, b) } }
    pub fn jit_le2(a: i64, b: i64) -> i64 { unsafe { nelisp_jit_le2(a, b) } }
    pub fn jit_ge2(a: i64, b: i64) -> i64 { unsafe { nelisp_jit_ge2(a, b) } }
    pub fn jit_logior2(a: i64, b: i64) -> i64 { unsafe { nelisp_jit_logior2(a, b) } }
    pub fn jit_logand2(a: i64, b: i64) -> i64 { unsafe { nelisp_jit_logand2(a, b) } }
    pub fn jit_logxor2(a: i64, b: i64) -> i64 { unsafe { nelisp_jit_logxor2(a, b) } }
    pub fn jit_ash(n: i64, c: i64) -> i64 { unsafe { nelisp_jit_ash(n, c) } }

    /// Doc 110 §110.E.2.a probes — thin safe wrappers around the 4
    /// elisp-compiled `jit/float.rs' replacements (add / sub / mul /
    /// div).  Used by `tests/elisp_cc_jit_float_probe.rs' to assert
    /// every member of the `.o' archive resolves and computes the
    /// expected f64 result.  The comparison trampolines (eq_eps /
    /// lt / gt / le / ge) keep their Rust impl until §110.E.2.b.
    pub fn jit_float_add(a: f64, b: f64) -> f64 { unsafe { nl_jit_float_add(a, b) } }
    pub fn jit_float_sub(a: f64, b: f64) -> f64 { unsafe { nl_jit_float_sub(a, b) } }
    pub fn jit_float_mul(a: f64, b: f64) -> f64 { unsafe { nl_jit_float_mul(a, b) } }
    pub fn jit_float_div(a: f64, b: f64) -> f64 { unsafe { nl_jit_float_div(a, b) } }

    /// Doc 110 §110.C.2.a probes — thin safe wrappers around the 4
    /// elisp-compiled ordered comparison trampolines (lt / gt / le
    /// / ge).  Result is 0 or 1 as i64.  NaN inputs return 0 to
    /// match Rust's `<' / `>' / `<=' / `>=' semantics.  Used by
    /// `tests/elisp_cc_jit_float_probe.rs' to assert every member
    /// resolves and produces the expected boolean.
    pub fn jit_float_lt(a: f64, b: f64) -> i64 { unsafe { nl_jit_float_lt(a, b) } }
    pub fn jit_float_gt(a: f64, b: f64) -> i64 { unsafe { nl_jit_float_gt(a, b) } }
    pub fn jit_float_le(a: f64, b: f64) -> i64 { unsafe { nl_jit_float_le(a, b) } }
    pub fn jit_float_ge(a: f64, b: f64) -> i64 { unsafe { nl_jit_float_ge(a, b) } }

    /// Doc 110 §110.C.2.b probe — thin safe wrapper for EQ-EPS.
    /// Result: 1 iff `(a - b).abs() < 1e-15' AND ordered.
    /// NaN inputs return 0.
    pub fn jit_float_eq_eps(a: f64, b: f64) -> i64 { unsafe { nl_jit_float_eq_eps(a, b) } }

    /// Doc 110 §110.F probes — thin safe wrappers around the 3
    /// elisp-compiled `jit/math.rs' replacements (float identity,
    /// exp, log).  `float' is pure pass-through; `exp' / `log'
    /// delegate to libm via the §110.F `(f64-call)' grammar form.
    pub fn jit_float_float(x: f64) -> f64 { unsafe { nl_jit_float_float(x) } }
    pub fn jit_float_exp(x: f64) -> f64 { unsafe { nl_jit_float_exp(x) } }
    pub fn jit_float_log(x: f64) -> f64 { unsafe { nl_jit_float_log(x) } }

    /// Doc 120 §120.A probe — thin safe wrapper around the Phase 47-
    /// compiled `nelisp_jit_predicate_eq' trampoline.  ABI matches the
    /// deleted Rust `nl_jit_predicate_eq': `(*const Sexp, *const Sexp)
    /// -> i64' returning 1 iff `(eq a b)' else 0.
    ///
    /// # Safety
    /// - `a' / `b' must be non-null pointers to initialized `Sexp's.
    pub unsafe fn jit_predicate_eq(a: *const Sexp, b: *const Sexp) -> i64 {
        nelisp_jit_predicate_eq(a, b)
    }

    /// Doc 120 §120.A probe — thin safe wrapper around the Phase 47-
    /// compiled `nelisp_jit_ref_eq' trampoline.  ABI matches the
    /// deleted Rust `nl_jit_ref_eq': `(*const Sexp, *const Sexp, *mut
    /// Sexp) -> i64'; writes `Sexp::T' / `Sexp::Nil' into `*out' and
    /// returns 0 (= always succeeds).
    ///
    /// # Safety
    /// - `a' / `b' must be non-null pointers to initialized `Sexp's.
    /// - `out' must be non-null, properly aligned, and writable for
    ///   one 32-byte Sexp slot pre-initialised to `Sexp::Nil'.
    pub unsafe fn jit_ref_eq(a: *const Sexp, b: *const Sexp, out: *mut Sexp) -> i64 {
        nelisp_jit_ref_eq(a, b, out)
    }

    /// Doc 120 §120.B probe — thin safe wrapper around the Phase 47-
    /// compiled `nelisp_jit_record_type' trampoline.  ABI matches the
    /// pre-§120.B Rust impl in `jit/box_accessor.rs': `(*const Sexp,
    /// *mut Sexp) -> i64' returning 0 on OK (Record arg, `*out =
    /// arg.type_tag'), 1 on ERR (non-Record).
    ///
    /// # Safety
    /// - `arg' must be a non-null pointer to an initialized `Sexp'.
    /// - `out' must be non-null, properly aligned, and writable for
    ///   one 32-byte Sexp slot pre-initialised to `Sexp::Nil'.
    pub unsafe fn jit_record_type(arg: *const Sexp, out: *mut Sexp) -> i64 {
        nelisp_jit_record_type(arg, out)
    }

    /// Doc 120 §120.B probe — thin safe wrapper around the Phase 47-
    /// compiled `nelisp_jit_record_len' trampoline.  ABI matches the
    /// pre-§120.B Rust impl: `(*const Sexp, *mut Sexp) -> i64'
    /// returning 0 on OK (Record arg, `*out = Sexp::Int(slots.len)`),
    /// 1 on ERR (non-Record).
    ///
    /// # Safety
    /// - `arg' must be a non-null pointer to an initialized `Sexp'.
    /// - `out' must be non-null, properly aligned, and writable for
    ///   one 32-byte Sexp slot pre-initialised to `Sexp::Nil'.
    pub unsafe fn jit_record_len(arg: *const Sexp, out: *mut Sexp) -> i64 {
        nelisp_jit_record_len(arg, out)
    }

    /// Doc 120 §120.B probe — thin safe wrapper around the Phase 47-
    /// compiled `nelisp_jit_record_ref' trampoline.  ABI matches the
    /// pre-§120.B Rust impl: `(*const Sexp, i64, *mut Sexp) -> i64'
    /// returning 0 on OK (Record arg + idx in [0, slots.len), `*out =
    /// arg.slots[idx]'), 1 on ERR (non-Record OR OOR).
    ///
    /// # Safety
    /// - `arg' must be a non-null pointer to an initialized `Sexp'.
    /// - `out' must be non-null, properly aligned, and writable for
    ///   one 32-byte Sexp slot pre-initialised to `Sexp::Nil'.
    pub unsafe fn jit_record_ref(
        arg: *const Sexp,
        idx: i64,
        out: *mut Sexp,
    ) -> i64 {
        nelisp_jit_record_ref(arg, idx, out)
    }

    /// Doc 120 §120.B probe — thin safe wrapper around the Phase 47-
    /// compiled `nelisp_jit_record_set' trampoline.  ABI matches the
    /// pre-§120.B Rust impl: `(*const Sexp, i64, *const Sexp, *mut
    /// Sexp) -> i64' returning 0 on OK (Record + OOR-valid idx,
    /// `slots[idx] := clone(*val)` and `*out = clone(*val)`), 1 on ERR.
    ///
    /// # Safety
    /// - `arg' / `val' must be non-null pointers to initialized `Sexp's.
    /// - `out' must be non-null, properly aligned, and writable for
    ///   one 32-byte Sexp slot pre-initialised to `Sexp::Nil'.
    pub unsafe fn jit_record_set(
        arg: *const Sexp,
        idx: i64,
        val: *const Sexp,
        out: *mut Sexp,
    ) -> i64 {
        nelisp_jit_record_set(arg, idx, val, out)
    }

    /// Doc 120 §120.D probe — thin safe wrapper around the Phase 47-
    /// compiled `nelisp_jit_length' trampoline.  ABI matches the
    /// pre-§120.D Rust impl in `jit/access.rs': `(*const Sexp, *mut
    /// Sexp) -> i64' returning 0 on OK (Nil / Vector input, `*out =
    /// Sexp::Int(len)'), 1 on ERR (Str / other variant — strategy.el
    /// `condition-case' fallback handles `string' via
    /// `nelisp--mut-str-len').
    ///
    /// # Safety
    /// - `arg' must be a non-null pointer to an initialized `Sexp'.
    /// - `out' must be non-null, properly aligned, and writable for
    ///   one 32-byte Sexp slot pre-initialised to `Sexp::Nil'.
    pub unsafe fn jit_length(arg: *const Sexp, out: *mut Sexp) -> i64 {
        nelisp_jit_length(arg, out)
    }

    /// Doc 120 §120.D probe — thin safe wrapper around the Phase 47-
    /// compiled `nelisp_jit_aref' trampoline.  ABI: `(*const Sexp,
    /// i64, *mut Sexp) -> i64' returning 0 on OK (Vector / BoolVector
    /// in-range), 1 on ERR (negative idx / OOR / non-array tag).
    ///
    /// # Safety
    /// - `arg' must be a non-null pointer to an initialized `Sexp'.
    /// - `out' must be non-null, properly aligned, and writable for
    ///   one 32-byte Sexp slot pre-initialised to `Sexp::Nil'.
    pub unsafe fn jit_aref(arg: *const Sexp, idx: i64, out: *mut Sexp) -> i64 {
        nelisp_jit_aref(arg, idx, out)
    }

    /// Doc 120 §120.D probe — thin safe wrapper around the Phase 47-
    /// compiled `nelisp_jit_aset' trampoline.  ABI: `(*const Sexp,
    /// i64, *const Sexp, *mut Sexp) -> i64' returning 0 on OK
    /// (Vector / BoolVector in-range), 1 on ERR.
    ///
    /// # Safety
    /// - `arg' / `val' must be non-null pointers to initialized `Sexp's.
    /// - `out' must be non-null, properly aligned, and writable for
    ///   one 32-byte Sexp slot pre-initialised to `Sexp::Nil'.
    pub unsafe fn jit_aset(
        arg: *const Sexp,
        idx: i64,
        val: *const Sexp,
        out: *mut Sexp,
    ) -> i64 {
        nelisp_jit_aset(arg, idx, val, out)
    }

    /// Doc 120 §120.D probe — thin safe wrapper around the Phase 47-
    /// compiled `nelisp_jit_elt' trampoline.  ABI: `(*const Sexp,
    /// i64, *mut Sexp) -> i64' returning 0 on OK (Vector / Cons-list
    /// in-range), 1 on ERR.
    ///
    /// # Safety
    /// - `arg' must be a non-null pointer to an initialized `Sexp'.
    /// - `out' must be non-null, properly aligned, and writable for
    ///   one 32-byte Sexp slot pre-initialised to `Sexp::Nil'.
    pub unsafe fn jit_elt(arg: *const Sexp, idx: i64, out: *mut Sexp) -> i64 {
        nelisp_jit_elt(arg, idx, out)
    }
}
