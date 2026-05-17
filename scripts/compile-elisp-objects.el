;;; compile-elisp-objects.el --- Doc 99 §99.B build orchestrator  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 99 §99.B build-time orchestrator.  Invoked by
;; `build-tool/build.rs' as
;;
;;   emacs --batch -Q -L lisp -l scripts/compile-elisp-objects.el \
;;         -f compile-elisp-objects-emit-all
;;
;; The function walks a hard-coded manifest of (elisp-source-feature .
;; output-object) pairs, runs `nelisp-phase47-compile-to-object' on the
;; canonical source from each feature, and writes the resulting ET_REL
;; .o files under `target/elisp-objects/'.
;;
;; The manifest is intentionally a defconst (not TOML) — §99.B is a
;; one-entry spike and parsing TOML inside `emacs --batch' would just
;; add a build dep on `toml.el'.  When the manifest grows past ~5
;; entries we can migrate.

;;; Code:

(require 'cl-lib)

;; Hard-coded `lisp/' resolution: this script is run from the repo
;; root by `cargo build', so relative paths resolve from there.
(let* ((this (or load-file-name buffer-file-name))
       (script-dir (and this (file-name-directory this)))
       (repo-root (and script-dir
                       (file-name-as-directory
                        (expand-file-name ".." script-dir))))
       (lisp-dir (and repo-root (expand-file-name "lisp" repo-root))))
  (when (and lisp-dir (file-directory-p lisp-dir))
    (add-to-list 'load-path lisp-dir)))

(require 'nelisp-phase47-compiler)

(defconst compile-elisp-objects-manifest
  '((nelisp-cc-spike-noop
     :source-var nelisp-cc-spike-noop--source
     :output "nelisp_spike_noop.o")
    (nelisp-cc-fact-i64
     :source-var nelisp-cc-fact-i64--source
     :output "nelisp_fact_i64.o")
    ;; Doc 100 §100.C — first real bi_* swap: `(truncate INT)' Int arm.
    (nelisp-cc-truncate-int
     :source-var nelisp-cc-truncate-int--source
     :output "nelisp_truncate_int.o")
    ;; Doc 101 §101.B — `(length CONS)' proper-list walk.
    (nelisp-cc-length-cons
     :source-var nelisp-cc-length-cons--source
     :output "nelisp_length_cons.o")
    ;; Doc 101 §101.C — `(eq SYMBOL SYMBOL)' via Symbol/Str read ops.
    (nelisp-cc-eq-symbol
     :source-var nelisp-cc-eq-symbol--source
     :output "nelisp_eq_symbol.o")
    ;; Doc 101 §101.D — `(cons A B)' via Cons construction ops.
    (nelisp-cc-cons-construct
     :source-var nelisp-cc-cons-construct--source
     :output "nelisp_cons_construct.o")
    ;; Doc 117 §117.A.2 — `(string-bytes STR)' byte-length swap.  Rust
    ;; keeps arity + tag dispatch + WrongType error; elisp owns the
    ;; `str-len' + `sexp-int-make' pair.
    (nelisp-cc-bi-string-bytes
     :source-var nelisp-cc-bi-string-bytes--source
     :output "nelisp_bi_string_bytes.o")
    ;; Doc 111 §111.B — `(recordp X)' via direct Sexp tag test.
    (nelisp-cc-recordp
     :source-var nelisp-cc-recordp--source
     :output "nelisp_recordp.o"
     :requires-arch x86_64)
    ;; Doc 111 §111.C — `(aref VECTOR IDX)' Vector arm swap.
    (nelisp-cc-aref-vector
     :source-var nelisp-cc-aref-vector--source
     :output "nelisp_aref_vector.o"
     :requires-arch x86_64)
    ;; Doc 117 §117.A.1 — `(make-vector N INIT)' allocate + fill swap.
    (nelisp-cc-bi-make-vector
     :source-var nelisp-cc-bi-make-vector--source
     :output "nelisp_bi_make_vector.o"
     :requires-arch x86_64)
    ;; Doc 111 §111.D — Cell read+write op probes (= no user-visible
    ;; swap, used only by `tests/phase47_cell.rs').  Four entries, one
    ;; per grammar op (`cell-value' / `cell-set-value' / `cell-make' /
    ;; `cell-null-p').  Linux-x86_64 only — same gate `nelisp-cc-recordp'
    ;; uses; aarch64 emit ships in a follow-up.
    (nelisp-cc-cell-ops
     :source-var nelisp-cc-cell-ops--value-source
     :output "nelisp_cell_value.o"
     :requires-arch x86_64)
    (nelisp-cc-cell-ops
     :source-var nelisp-cc-cell-ops--set-value-source
     :output "nelisp_cell_set_value.o"
     :requires-arch x86_64)
    (nelisp-cc-cell-ops
     :source-var nelisp-cc-cell-ops--make-source
     :output "nelisp_cell_make.o"
     :requires-arch x86_64)
    (nelisp-cc-cell-ops
     :source-var nelisp-cc-cell-ops--null-p-source
     :output "nelisp_cell_null_p.o"
     :requires-arch x86_64)
    ;; Doc 111 §111.E #1 — `mirror_lookup_entry' Phase 47 rewrite
    ;; (= Group A foundation helper for env_mirror.rs).  Linux-x86_64
    ;; only for the same reason as `nelisp-cc-recordp' (= aarch64
    ;; record/vector-ref-ptr emit ships in a follow-up).
    (nelisp-cc-mirror-lookup-entry
     :source-var nelisp-cc-mirror-lookup-entry--source
     :output "nelisp_mirror_lookup_entry.o"
     :requires-arch x86_64)
    ;; Doc 111 §111.E #2-6 — Group A compose-on-#1 helpers.  Each
    ;; thin object reuses `nelisp_mirror_lookup_entry' via the
    ;; `extern-call' grammar form and adds a 1-2 op tail
    ;; (`record-slot-ref' / `symbol-eq' / `sexp-tag') to read the
    ;; requested slot.  Linux-x86_64 only (= shares the same arch
    ;; gate as #1).
    (nelisp-cc-mirror-lookup-value
     :source-var nelisp-cc-mirror-lookup-value--source
     :output "nelisp_mirror_lookup_value.o"
     :requires-arch x86_64)
    (nelisp-cc-mirror-lookup-function
     :source-var nelisp-cc-mirror-lookup-function--source
     :output "nelisp_mirror_lookup_function.o"
     :requires-arch x86_64)
    (nelisp-cc-mirror-is-bound
     :source-var nelisp-cc-mirror-is-bound--source
     :output "nelisp_mirror_is_bound.o"
     :requires-arch x86_64)
    (nelisp-cc-mirror-is-fbound
     :source-var nelisp-cc-mirror-is-fbound--source
     :output "nelisp_mirror_is_fbound.o"
     :requires-arch x86_64)
    (nelisp-cc-mirror-is-constant
     :source-var nelisp-cc-mirror-is-constant--source
     :output "nelisp_mirror_is_constant.o"
     :requires-arch x86_64)
    ;; Doc 111 §111.E #19-#26 Group E — env_lexframe.rs Phase 47
    ;; rewrites.  Each entry wraps a Rust shim that mirrors the
    ;; corresponding `Env::frame_*' method (= `nl_frame_*' externs in
    ;; `build-tool/src/eval/env_lexframe_phase47_shims.rs').  Linux-
    ;; x86_64 only for the same `extern-call' + record/vector ABI
    ;; reasons; aarch64 emit ships in a follow-up.
    (nelisp-cc-frame-stack-view
     :source-var nelisp-cc-frame-stack-view--source
     :output "nelisp_frame_stack_depth.o"
     :requires-arch x86_64)
    (nelisp-cc-frame-ensure-capacity
     :source-var nelisp-cc-frame-ensure-capacity--source
     :output "nelisp_frame_stack_ensure_capacity.o"
     :requires-arch x86_64)
    (nelisp-cc-frame-push
     :source-var nelisp-cc-frame-push--source
     :output "nelisp_frame_push.o"
     :requires-arch x86_64)
    (nelisp-cc-frame-pop
     :source-var nelisp-cc-frame-pop--source
     :output "nelisp_frame_pop.o"
     :requires-arch x86_64)
    (nelisp-cc-frame-bind
     :source-var nelisp-cc-frame-bind--source
     :output "nelisp_frame_bind.o"
     :requires-arch x86_64)
    (nelisp-cc-frame-stack-find
     :source-var nelisp-cc-frame-stack-find--source
     :output "nelisp_frame_stack_find.o"
     :requires-arch x86_64)
    (nelisp-cc-wrap-alist-cells
     :source-var nelisp-cc-wrap-alist-cells--source
     :output "nelisp_wrap_alist_cells.o"
     :requires-arch x86_64)
    ;; Doc 115 §115.7 — pure-elisp 32-bit FNV-1a hash.  Replaces the
    ;; Rust `mirror_fnv1a' free fn + `nl_mirror_fnv1a_sexp' extern
    ;; wrapper in `env_helpers.rs'.  Linux-x86_64 only for the same
    ;; reason as `nelisp-cc-mirror-lookup-entry' (= `str-byte-at' +
    ;; `str-len' ABI offsets are x86_64-encoded).
    (nelisp-cc-fnv1a
     :source-var nelisp-cc-fnv1a--source
     :output "nelisp_fnv1a.o"
     :requires-arch x86_64)
    ;; Doc 111 §111.E Group B (#7-#12) — env_mirror.rs write path
    ;; Phase 47 rewrites.  Each helper composes `mirror_lookup_entry'
    ;; (= #1) via `extern-call' + a §111.B `record-slot-set' write to
    ;; the matched entry's slot N.  Linux-x86_64 only for the same
    ;; reason as `nelisp-cc-mirror-lookup-entry'.
    (nelisp-cc-mirror-set-value
     :source-var nelisp-cc-mirror-set-value--source
     :output "nelisp_mirror_set_value.o"
     :requires-arch x86_64)
    (nelisp-cc-mirror-set-function
     :source-var nelisp-cc-mirror-set-function--source
     :output "nelisp_mirror_set_function.o"
     :requires-arch x86_64)
    (nelisp-cc-mirror-clear-value
     :source-var nelisp-cc-mirror-clear-value--source
     :output "nelisp_mirror_clear_value.o"
     :requires-arch x86_64)
    (nelisp-cc-mirror-clear-function
     :source-var nelisp-cc-mirror-clear-function--source
     :output "nelisp_mirror_clear_function.o"
     :requires-arch x86_64)
    (nelisp-cc-mirror-set-constant
     :source-var nelisp-cc-mirror-set-constant--source
     :output "nelisp_mirror_set_constant.o"
     :requires-arch x86_64)
    (nelisp-cc-mirror-install-entry
     :source-var nelisp-cc-mirror-install-entry--source
     :output "nelisp_mirror_install_entry.o"
     :requires-arch x86_64)
    ;; Doc 100 §100.D — `jit/arith.rs' 12-trampoline swap.  Each entry
    ;; emits one `.o' file exporting one `nelisp_jit_NAME' symbol that
    ;; the `unified_fn_ptr' table in `build-tool/src/jit/bridge.rs'
    ;; resolves at link time.  Linux-x86_64 only — non-linux / non-
    ;; x86_64 build paths keep the legacy Rust trampolines for now.
    (nelisp-cc-jit-arith
     :source-var nelisp-cc-jit-arith-add2--source
     :output "nelisp_jit_add2.o")
    (nelisp-cc-jit-arith
     :source-var nelisp-cc-jit-arith-sub2--source
     :output "nelisp_jit_sub2.o")
    (nelisp-cc-jit-arith
     :source-var nelisp-cc-jit-arith-mul2--source
     :output "nelisp_jit_mul2.o")
    (nelisp-cc-jit-arith
     :source-var nelisp-cc-jit-arith-eq2--source
     :output "nelisp_jit_eq2.o")
    (nelisp-cc-jit-arith
     :source-var nelisp-cc-jit-arith-lt2--source
     :output "nelisp_jit_lt2.o")
    (nelisp-cc-jit-arith
     :source-var nelisp-cc-jit-arith-gt2--source
     :output "nelisp_jit_gt2.o")
    (nelisp-cc-jit-arith
     :source-var nelisp-cc-jit-arith-le2--source
     :output "nelisp_jit_le2.o")
    (nelisp-cc-jit-arith
     :source-var nelisp-cc-jit-arith-ge2--source
     :output "nelisp_jit_ge2.o")
    (nelisp-cc-jit-arith
     :source-var nelisp-cc-jit-arith-logior2--source
     :output "nelisp_jit_logior2.o")
    (nelisp-cc-jit-arith
     :source-var nelisp-cc-jit-arith-logand2--source
     :output "nelisp_jit_logand2.o")
    (nelisp-cc-jit-arith
     :source-var nelisp-cc-jit-arith-logxor2--source
     :output "nelisp_jit_logxor2.o")
    (nelisp-cc-jit-arith
     :source-var nelisp-cc-jit-arith-ash--source
     :output "nelisp_jit_ash.o")
    ;; Doc 110 §110.E.2.a — `jit/float.rs' 4 arithmetic trampoline
    ;; swaps (add / sub / mul / div).  Each entry emits one `.o'
    ;; exporting one `nl_jit_float_*' symbol that the linker
    ;; resolves into the static archive against the Rust extern
    ;; in `build-tool/src/jit/bridge.rs::float_link'.  The 5
    ;; comparison trampolines (eq_eps / lt / gt / le / ge) need
    ;; §110.C compiler integration and ship in §110.E.2.b.
    ;;
    ;; `:requires-arch x86_64' — Stage 2.a only ships the x86_64
    ;; f64 emit path; aarch64 f64 emit ships in §110.D and removes
    ;; the gate.  Until then, building on aarch64 skips these
    ;; entries and the Rust trampolines stay live (gated by an
    ;; inverse `#[cfg]' in `build-tool/src/jit/float.rs').
    (nelisp-cc-jit-float
     :source-var nelisp-cc-jit-float-add--source
     :output "nl_jit_float_add.o"
     :requires-arch (x86_64 aarch64))
    (nelisp-cc-jit-float
     :source-var nelisp-cc-jit-float-sub--source
     :output "nl_jit_float_sub.o"
     :requires-arch (x86_64 aarch64))
    (nelisp-cc-jit-float
     :source-var nelisp-cc-jit-float-mul--source
     :output "nl_jit_float_mul.o"
     :requires-arch (x86_64 aarch64))
    (nelisp-cc-jit-float
     :source-var nelisp-cc-jit-float-div--source
     :output "nl_jit_float_div.o"
     :requires-arch (x86_64 aarch64))
    ;; Doc 110 §110.C.2.a — 4 ordered comparison trampolines.
    ;; eq_eps stays Rust until §110.C.2.b (= rodata + NaN mask).
    (nelisp-cc-jit-float
     :source-var nelisp-cc-jit-float-lt--source
     :output "nl_jit_float_lt.o"
     :requires-arch (x86_64 aarch64))
    (nelisp-cc-jit-float
     :source-var nelisp-cc-jit-float-gt--source
     :output "nl_jit_float_gt.o"
     :requires-arch (x86_64 aarch64))
    (nelisp-cc-jit-float
     :source-var nelisp-cc-jit-float-le--source
     :output "nl_jit_float_le.o"
     :requires-arch (x86_64 aarch64))
    (nelisp-cc-jit-float
     :source-var nelisp-cc-jit-float-ge--source
     :output "nl_jit_float_ge.o"
     :requires-arch (x86_64 aarch64))
    ;; Doc 110 §110.C.2.b — EQ-EPS (= `(a-b).abs() < 1e-15').
    ;; Uses the SUBSD + ANDPD + UCOMISD + SETB/SETNP/AND mask
    ;; sequence emitted by `--emit-f64-eq-eps'.
    (nelisp-cc-jit-float
     :source-var nelisp-cc-jit-float-eq-eps--source
     :output "nl_jit_float_eq_eps.o"
     :requires-arch (x86_64 aarch64))
    ;; Doc 110 §110.F — `jit/math.rs' 3 unary f64 trampoline swaps.
    ;; `float' = identity, `exp' / `log' = libm extern call via the
    ;; new `(f64-call SYM ARG)' grammar form introduced in §110.F.
    (nelisp-cc-jit-math
     :source-var nelisp-cc-jit-math-float--source
     :output "nl_jit_float_float.o"
     :requires-arch (x86_64 aarch64))
    (nelisp-cc-jit-math
     :source-var nelisp-cc-jit-math-exp--source
     :output "nl_jit_float_exp.o"
     :requires-arch (x86_64 aarch64))
    (nelisp-cc-jit-math
     :source-var nelisp-cc-jit-math-log--source
     :output "nl_jit_float_log.o"
     :requires-arch (x86_64 aarch64))
    ;; Doc 120 §120.A — `jit/predicate.rs' partial swap.
    ;; `nl_jit_predicate_eq' (`(eq A B)' trampoline) and
    ;; `nl_jit_ref_eq' (`(nelisp--ref-eq A B)' trampoline) move to
    ;; Phase 47 elisp.  Both compose the slow path through a fresh
    ;; `nl_sexp_eq' Rust extern in `eval/special_forms.rs' (= thin
    ;; `#[no_mangle]' wrapper around the existing `sexp_eq' free
    ;; fn).  `nl_jit_sxhash' + `nl_jit_type_of' stay Rust — see the
    ;; blocker note in `build-tool/src/jit/predicate.rs'.  Linux-
    ;; x86_64 only — `extern-call' ABI ships aarch64 in a follow-up.
    (nelisp-cc-jit-predicate-eq
     :source-var nelisp-cc-jit-predicate-eq--source
     :output "nelisp_jit_predicate_eq.o"
     :requires-arch x86_64)
    (nelisp-cc-jit-ref-eq
     :source-var nelisp-cc-jit-ref-eq--source
     :output "nelisp_jit_ref_eq.o"
     :requires-arch x86_64)
    ;; Doc 120 §120.B — `jit/box_accessor.rs' record family partial swap.
    ;; 4 of 11 trampolines (= `nl_jit_record_type', `nl_jit_record_len',
    ;; `nl_jit_record_ref', `nl_jit_record_set') move to Phase 47 elisp.
    ;; `nl_jit_record_alloc' stays Rust (list-walk-to-count requires a
    ;; grammar primitive not yet shipped); the 6 non-record trampolines
    ;; (mut-str / bool-vector / codepoint / char-table) all SKIP with
    ;; documented blockers in `build-tool/src/jit/box_accessor.rs'.
    ;; Linux-x86_64 only — `extern-call' ABI ships aarch64 in follow-up.
    (nelisp-cc-jit-record
     :source-var nelisp-cc-jit-record-type--source
     :output "nelisp_jit_record_type.o"
     :requires-arch x86_64)
    (nelisp-cc-jit-record
     :source-var nelisp-cc-jit-record-len--source
     :output "nelisp_jit_record_len.o"
     :requires-arch x86_64)
    (nelisp-cc-jit-record
     :source-var nelisp-cc-jit-record-ref--source
     :output "nelisp_jit_record_ref.o"
     :requires-arch x86_64)
    (nelisp-cc-jit-record
     :source-var nelisp-cc-jit-record-set--source
     :output "nelisp_jit_record_set.o"
     :requires-arch x86_64)
    ;; Doc 122 §122.A — `sexp-write-str' / `sexp-write-symbol' grammar
    ;; ops.  Two entries, one per op, packaged as standalone
    ;; Phase 47-compiled `defun's so `tests/elisp_cc_sexp_write_str_
    ;; probe.rs' can drive each round-trip independently.  Same
    ;; Linux-x86_64 gate as `nelisp-cc-cell-ops' (aarch64 emit lands in
    ;; the same future doc that covers §115.1+ Phase 47 emit ops).
    (nelisp-cc-sexp-write-str
     :source-var nelisp-cc-sexp-write-str--str-source
     :output "nelisp_sexp_write_str.o"
     :requires-arch x86_64)
    (nelisp-cc-sexp-write-str
     :source-var nelisp-cc-sexp-write-str--symbol-source
     :output "nelisp_sexp_write_symbol.o"
     :requires-arch x86_64)
    ;; Doc 122 §122.G — `sexp-write-float' grammar op (= Reader Float
    ;; unlock).  Single entry; same Linux-x86_64 gate as the §122.A
    ;; siblings.  Both params f64-class (MVP uniform-class defun
    ;; restriction); test harness bit-casts the slot pointer through
    ;; xmm0 before invocation.
    (nelisp-cc-sexp-write-float
     :source-var nelisp-cc-sexp-write-float--source
     :output "nelisp_sexp_write_float.o"
     :requires-arch x86_64)
    ;; Doc 120 §120.D — `jit/access.rs' all 4 trampoline swaps
    ;; (`nl_jit_access_length' / `_aref' / `_aset' / `_elt').  Vector
    ;; arms use the existing `vector-ref' / `vector-slot-set' /
    ;; `vector-len' grammar.  Cons-list `elt' walker reuses §101.B
    ;; `cons-cdr-raw-from-box' / `sexp-payload-ptr' shape.  Str arm
    ;; of `length' degrades to ERR fallthrough (= strategy.el's
    ;; `condition-case' dispatches to `nelisp--mut-str-len'); the
    ;; BoolVector arms of `aref' / `aset' delegate to narrow Rust
    ;; externs (`nl_jit_access_{aref,aset}_bool_vector_inner') via
    ;; `extern-call' until Phase 47 grows `bool-vector-*' grammar
    ;; primitives (= Doc 122 §122.B cluster).  Linux-x86_64 only —
    ;; `extern-call' ABI ships aarch64 in follow-up.
    (nelisp-cc-jit-length
     :source-var nelisp-cc-jit-length--source
     :output "nelisp_jit_length.o"
     :requires-arch x86_64)
    (nelisp-cc-jit-aref
     :source-var nelisp-cc-jit-aref--source
     :output "nelisp_jit_aref.o"
     :requires-arch x86_64)
    (nelisp-cc-jit-aset
     :source-var nelisp-cc-jit-aset--source
     :output "nelisp_jit_aset.o"
     :requires-arch x86_64)
    (nelisp-cc-jit-elt
     :source-var nelisp-cc-jit-elt--source
     :output "nelisp_jit_elt.o"
     :requires-arch x86_64)
    ;; Doc 122 §122.B — Mutable string builder grammar ops.  Five
    ;; entries, one per op, packaged as standalone Phase 47-compiled
    ;; `defun's so `tests/elisp_cc_mut_str_probe.rs' can drive each
    ;; round-trip independently.  Same Linux-x86_64 gate as
    ;; `nelisp-cc-sexp-write-str' (§122.A); aarch64 emit lands with
    ;; the rest of the Phase 47 aarch64 sweep.
    (nelisp-cc-mut-str
     :source-var nelisp-cc-mut-str--make-empty-source
     :output "nelisp_mut_str_make_empty.o"
     :requires-arch x86_64)
    (nelisp-cc-mut-str
     :source-var nelisp-cc-mut-str--push-byte-source
     :output "nelisp_mut_str_push_byte.o"
     :requires-arch x86_64)
    (nelisp-cc-mut-str
     :source-var nelisp-cc-mut-str--push-codepoint-source
     :output "nelisp_mut_str_push_codepoint.o"
     :requires-arch x86_64)
    (nelisp-cc-mut-str
     :source-var nelisp-cc-mut-str--len-source
     :output "nelisp_mut_str_len.o"
     :requires-arch x86_64)
    (nelisp-cc-mut-str
     :source-var nelisp-cc-mut-str--finalize-source
     :output "nelisp_mut_str_finalize.o"
     :requires-arch x86_64)
    ;; Doc 122 §122.D — UTF-8 helper grammar ops.  Three entries,
    ;; one per op, packaged as standalone Phase 47-compiled `defun's
    ;; so `tests/elisp_cc_utf8_probe.rs' can drive each round-trip
    ;; independently.  Same Linux-x86_64 gate as `nelisp-cc-mut-str'
    ;; (§122.B); aarch64 emit lands with the rest of the Phase 47
    ;; aarch64 sweep.
    (nelisp-cc-utf8
     :source-var nelisp-cc-utf8--char-count-source
     :output "nelisp_str_char_count.o"
     :requires-arch x86_64)
    (nelisp-cc-utf8
     :source-var nelisp-cc-utf8--codepoint-at-source
     :output "nelisp_str_codepoint_at.o"
     :requires-arch x86_64)
    (nelisp-cc-utf8
     :source-var nelisp-cc-utf8--is-alphanumeric-at-source
     :output "nelisp_str_is_alphanumeric_at.o"
     :requires-arch x86_64)
    ;; Doc 122 §122.E — Atomic + raw memory primitives.  Six entries,
    ;; one per op, packaged as standalone Phase 47-compiled `defun's
    ;; so `tests/elisp_cc_atomic_raw_mem_probe.rs' can drive each
    ;; round-trip independently.  Substrate gate for Doc 123-128
    ;; (= refcount elisp化, nl*.rs::Clone/Drop elisp化, alloc /
    ;; dealloc handlers).  Same Linux-x86_64 gate as the §122.A/B
    ;; siblings; aarch64 emit lands with the rest of the Phase 47
    ;; aarch64 sweep.
    (nelisp-cc-atomic-raw-mem
     :source-var nelisp-cc-atomic-raw-mem--fetch-add-source
     :output "nelisp_atomic_fetch_add.o"
     :requires-arch x86_64)
    (nelisp-cc-atomic-raw-mem
     :source-var nelisp-cc-atomic-raw-mem--compare-exchange-source
     :output "nelisp_atomic_compare_exchange.o"
     :requires-arch x86_64)
    (nelisp-cc-atomic-raw-mem
     :source-var nelisp-cc-atomic-raw-mem--read-u64-source
     :output "nelisp_ptr_read_u64.o"
     :requires-arch x86_64)
    (nelisp-cc-atomic-raw-mem
     :source-var nelisp-cc-atomic-raw-mem--write-u64-source
     :output "nelisp_ptr_write_u64.o"
     :requires-arch x86_64)
    (nelisp-cc-atomic-raw-mem
     :source-var nelisp-cc-atomic-raw-mem--read-u8-source
     :output "nelisp_ptr_read_u8.o"
     :requires-arch x86_64)
    (nelisp-cc-atomic-raw-mem
     :source-var nelisp-cc-atomic-raw-mem--write-u8-source
     :output "nelisp_ptr_write_u8.o"
     :requires-arch x86_64)
    ;; Doc 125 §125.A — alloc / dealloc primitive grammar ops.  Two
    ;; entries, one per op, packaged as standalone Phase 47-compiled
    ;; `defun's so `tests/elisp_cc_alloc_dealloc_probe.rs' can drive
    ;; each round-trip independently.  Substrate gate for Doc 124.G-K
    ;; (= NlBox Drop kernels' if-zero-refcount free branch) +
    ;; Doc 126-128 (= bridge GC arena allocator).  Same Linux-x86_64
    ;; gate as the §122.E siblings (= raw_mem.rs is the shared Rust
    ;; module); aarch64 emit lands with the rest of the Phase 47
    ;; aarch64 sweep.
    (nelisp-cc-alloc-dealloc
     :source-var nelisp-cc-alloc-dealloc--alloc-bytes-source
     :output "nelisp_alloc_bytes.o"
     :requires-arch x86_64)
    (nelisp-cc-alloc-dealloc
     :source-var nelisp-cc-alloc-dealloc--dealloc-bytes-source
     :output "nelisp_dealloc_bytes.o"
     :requires-arch x86_64)
    ;; Doc 123 §123.A — first substrate elisp化 stage.  Replaces the
    ;; simplest macro from `build-tool/src/eval/rc_primitives.rs' (=
    ;; the refcount-inc kernel) with a pure-elisp body that uses the
    ;; §122.E `atomic-fetch-add' op.  Substrate gate validation for
    ;; the Doc 123-128 chain — proves §122.E grammar is sufficient to
    ;; mutate the `NlConsBox' refcount slot from elisp without a Rust
    ;; hop.  Same Linux-x86_64 gate as the §122.E parent.
    (nelisp-cc-rc-inc
     :source-var nelisp-cc-rc-inc--source
     :output "nelisp_rc_inc.o"
     :requires-arch x86_64)
    ;; Doc 123 §123.B — second substrate elisp化 stage.
    (nelisp-cc-rc-dec
     :source-var nelisp-cc-rc-dec--source
     :output "nelisp_rc_dec.o"
     :requires-arch x86_64)
    ;; Doc 123 §123.C — refcount-reader twins.
    (nelisp-cc-rc-strong-count
     :source-var nelisp-cc-rc-strong-count--source
     :output "nelisp_rc_strong_count.o"
     :requires-arch x86_64)
    (nelisp-cc-rc-kind
     :source-var nelisp-cc-rc-kind--source
     :output "nelisp_rc_kind.o"
     :requires-arch x86_64)
    ;; Doc 124 §124.A — NlConsBox Clone elisp kernel.  First stage of
    ;; the `nl*.rs::Clone/Drop' substrate elisp化 chain (Doc 123 sibling,
    ;; expanded scope to `nl{consbox,vector,cell,record,str}.rs').  The
    ;; elisp body bumps the refcount via §122.E `atomic-fetch-add' (same
    ;; pattern as Doc 123 §123.A `nelisp_rc_inc') and returns the input
    ;; pointer unchanged (= the cloned-handle's pointer; `NlConsBoxRef'
    ;; is `#[repr(transparent)]' so the trait impl's `Self { ptr,
    ;; _marker }' construction is an ABI no-op).  §124.F sweep stage
    ;; will replace the inline Rust `impl Clone for NlConsBoxRef' body
    ;; in `nlconsbox.rs:343-355' once §124.B-E sibling PoCs land.
    (nelisp-cc-nlconsbox-clone
     :source-var nelisp-cc-nlconsbox-clone--source
     :output "nelisp_nlconsbox_clone.o"
     :requires-arch x86_64)
    ;; Doc 124 §124.G — NlConsBox Drop elisp kernel.
    (nelisp-cc-nlconsbox-drop
     :source-var nelisp-cc-nlconsbox-drop--source
     :output "nelisp_nlconsbox_drop.o"
     :requires-arch x86_64)
    ;; Doc 124 §124.H — NlVector Drop elisp kernel.  Identical shape
    ;; to §124.G modulo per-type layout: REFCOUNT_OFFSET = 24,
    ;; SIZE_OF_NLVECTOR = 32 (= 24-byte Vec<Sexp> header + 8-byte
    ;; AtomicUsize trailer), ALIGN = 8.
    (nelisp-cc-nlvector-drop
     :source-var nelisp-cc-nlvector-drop--source
     :output "nelisp_nlvector_drop.o"
     :requires-arch x86_64)
    ;; Doc 124 §124.I/J/K — sibling Drop kernels.  Same shape as §124.G/H
    ;; modulo per-type layout literals:
    ;;   §124.I NlCell:   REFCOUNT_OFFSET = 32, SIZE = 40, ALIGN = 8
    ;;   §124.J NlRecord: REFCOUNT_OFFSET = 56, SIZE = 64, ALIGN = 8
    ;;   §124.K NlStr:    REFCOUNT_OFFSET = 24, SIZE = 32, ALIGN = 8
    (nelisp-cc-nlcell-drop
     :source-var nelisp-cc-nlcell-drop--source
     :output "nelisp_nlcell_drop.o"
     :requires-arch x86_64)
    (nelisp-cc-nlrecord-drop
     :source-var nelisp-cc-nlrecord-drop--source
     :output "nelisp_nlrecord_drop.o"
     :requires-arch x86_64)
    (nelisp-cc-nlstr-drop
     :source-var nelisp-cc-nlstr-drop--source
     :output "nelisp_nlstr_drop.o"
     :requires-arch x86_64)
    ;; Doc 124 §124.B-E — mechanical sibling Clone kernels.  Identical
    ;; shape to §124.A modulo the per-type REFCOUNT_OFFSET literal:
    ;;   §124.B NlVector: 24, §124.C NlCell: 32,
    ;;   §124.D NlRecord: 56, §124.E NlStr: 24.
    (nelisp-cc-nlvector-clone
     :source-var nelisp-cc-nlvector-clone--source
     :output "nelisp_nlvector_clone.o"
     :requires-arch x86_64)
    (nelisp-cc-nlcell-clone
     :source-var nelisp-cc-nlcell-clone--source
     :output "nelisp_nlcell_clone.o"
     :requires-arch x86_64)
    (nelisp-cc-nlrecord-clone
     :source-var nelisp-cc-nlrecord-clone--source
     :output "nelisp_nlrecord_clone.o"
     :requires-arch x86_64)
    (nelisp-cc-nlstr-clone
     :source-var nelisp-cc-nlstr-clone--source
     :output "nelisp_nlstr_clone.o"
     :requires-arch x86_64)
    ;; Doc 116 §116.A — pure-elisp Reader lexer.  Single manifest
    ;; entry; the source defconst is a `(seq DEFUN ...)' of ~20
    ;; mutually-recursive tail-call helpers and one public entry
    ;; `nelisp_reader_lex_one'.  Linux-x86_64 only (= same gate
    ;; as `nelisp-cc-mut-str' / `nelisp-cc-utf8'; aarch64 emit
    ;; will land with the rest of the Phase 47 aarch64 sweep when
    ;; the `extern-call' ABI's aarch64 path is complete).
    (nelisp-cc-reader-lexer
     :source-var nelisp-cc-reader-lexer--source
     :output "nelisp_reader_lex_one.o"
     :requires-arch x86_64)
    ;; Doc 122 §122.C — Extended extern-call (f64 args + f64 return) probes.
    ;; Three entries wrapping libm `sqrt' / `sin' / `cos' to verify the
    ;; new type-annotated arg shape + `extern-call-f64' head op + per-
    ;; class register placement (= f64 → xmm0-7, i64 → rdi-r9).
    ;; Linux-x86_64 only — `extern-call' ABI ships aarch64 in follow-up.
    (nelisp-cc-extern-call-f64
     :source-var nelisp-cc-extern-call-f64--sqrt-source
     :output "nelisp_libm_sqrt.o"
     :requires-arch x86_64)
    (nelisp-cc-extern-call-f64
     :source-var nelisp-cc-extern-call-f64--sin-source
     :output "nelisp_libm_sin.o"
     :requires-arch x86_64)
    (nelisp-cc-extern-call-f64
     :source-var nelisp-cc-extern-call-f64--cos-source
     :output "nelisp_libm_cos.o"
     :requires-arch x86_64)
    ;; Doc 116 §116.B — pure-elisp Reader parser.  Single manifest
    ;; entry; the source defconst is a `(seq DEFUN ...)' of ~25
    ;; mutually-recursive helpers + one public entry
    ;; `nelisp_reader_parse_one'.  Consumes §116.A's token stream
    ;; via `extern-call nelisp_reader_lex_one'.  Linux-x86_64 only.
    (nelisp-cc-reader-parser
     :source-var nelisp-cc-reader-parser--source
     :output "nelisp_reader_parse_one.o"
     :requires-arch x86_64))
  "Build-time manifest of elisp features → ET_REL output files.
Each entry is `(FEATURE :source-var SYM :output BASENAME)' where
FEATURE is the feature to `require', SYM is the defconst holding
the canonical source form, and BASENAME is the .o filename written
to OUT-DIR.  When the spike grows past one entry, switch to a TOML
manifest under `build-tool/elisp-objects.toml'.")

(defun compile-elisp-objects--out-dir ()
  "Return the absolute path of `target/elisp-objects/' under the repo root.
Honors the `NELISP_ELISP_OBJECTS_DIR' environment variable when set
(= `build.rs' passes `$OUT_DIR' / elisp-objects so cargo doesn't see
stale artifacts across feature combinations)."
  (or (getenv "NELISP_ELISP_OBJECTS_DIR")
      (let* ((this (or load-file-name buffer-file-name))
             (script-dir (and this (file-name-directory this)))
             (repo-root (and script-dir
                             (file-name-as-directory
                             (expand-file-name ".." script-dir)))))
        (expand-file-name "target/elisp-objects" repo-root))))

(defun compile-elisp-objects--target-arch ()
  "Return the Phase 47 target arch symbol for this batch run.
Defaults to `x86_64' when `NELISP_PHASE47_TARGET_ARCH' is unset.
Signals a clear error on unsupported values."
  (pcase (or (getenv "NELISP_PHASE47_TARGET_ARCH") "x86_64")
    ("x86_64" 'x86_64)
    ("aarch64" 'aarch64)
    (other
     (error
      "compile-elisp-objects: unsupported NELISP_PHASE47_TARGET_ARCH %S (expected x86_64 or aarch64)"
      other))))

(defun compile-elisp-objects--target-format ()
  "Return the Phase 47 output format symbol for this batch run.
Looks at `NELISP_PHASE47_TARGET_OS' (forwarded by `build.rs')
and picks `'mach-o' for macOS, `'elf' otherwise (= linux + the
default).  Doc 100 §100.D Stage 3 added Mach-O support for
macos-aarch64; other OS / format combinations stay on ELF."
  (pcase (or (getenv "NELISP_PHASE47_TARGET_OS") "linux")
    ("macos" 'mach-o)
    (_ 'elf)))

(defun compile-elisp-objects-emit-all ()
  "Compile every manifest entry to its output `.o' file.
Returns the list of absolute paths written.  Used by
`build-tool/build.rs' as the `-f' callback (= no args, exits the
batch process via the outer `emacs --batch' driver).

Each manifest entry may specify a `:requires-arch SYM' keyword
(Doc 110 §110.E.2.a) — when present and the current target arch
does not match SYM, the entry is silently skipped.  This lets
Stage 2.a ship the x86_64 f64 swap entries while aarch64 builds
fall through to the Rust trampolines that remain in
`build-tool/src/jit/float.rs' under an inverse cfg gate."
  (let* ((out-dir (compile-elisp-objects--out-dir))
         (arch (compile-elisp-objects--target-arch))
         (format (compile-elisp-objects--target-format))
         (written nil))
    (unless (file-directory-p out-dir)
      (make-directory out-dir t))
    (dolist (entry compile-elisp-objects-manifest)
      (let* ((feature (car entry))
             (props   (cdr entry))
             (src-var (plist-get props :source-var))
             (output  (plist-get props :output))
             (requires-arch (plist-get props :requires-arch))
             (out-path (expand-file-name output out-dir)))
        (cond
         ((and requires-arch
               (cond ((symbolp requires-arch) (not (eq requires-arch arch)))
                     ((listp requires-arch) (not (memq arch requires-arch)))
                     (t t)))
          (message "[compile-elisp-objects] skipping %s -> %s (= requires %S, building %S)"
                   feature output requires-arch arch))
         (t
          (require feature)
          (unless (boundp src-var)
            (error "compile-elisp-objects: %S has no :source-var %S"
                   feature src-var))
          (let ((sexp (symbol-value src-var)))
            (message "[compile-elisp-objects] %s -> %s"
                     feature out-path)
            (nelisp-phase47-compile-to-object sexp out-path
                                              :arch arch :format format)
            (push out-path written))))))
    (nreverse written)))

(provide 'compile-elisp-objects)

;;; compile-elisp-objects.el ends here
