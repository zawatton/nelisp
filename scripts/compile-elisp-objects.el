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
    ;; Doc 117 §117.B — quit-flag atomic ops swap (3 entries, shared
    ;; source file).  Rust shim calls `nl_quit_flag_ptr' to obtain the
    ;; static slot's address, then dispatches to one of these kernels
    ;; via `extern "C"' to perform the §122.E `atomic-compare-exchange'
    ;; (= set / clear) or `ptr-read-u64' (= pending-p) op.  Linux-
    ;; x86_64 only — same gate the rest of the §122.E call sites use.
    (nelisp-cc-bi-quit-flag
     :source-var nelisp-cc-bi-quit-flag--set-source
     :output "nelisp_bi_set_quit_flag.o"
     :requires-arch x86_64)
    (nelisp-cc-bi-quit-flag
     :source-var nelisp-cc-bi-quit-flag--clear-source
     :output "nelisp_bi_clear_quit_flag.o"
     :requires-arch x86_64)
    (nelisp-cc-bi-quit-flag
     :source-var nelisp-cc-bi-quit-flag--pending-p-source
     :output "nelisp_bi_quit_flag_pending_p.o"
     :requires-arch x86_64)
    ;; Doc 117 §117.B / Doc 122 §122.H — first I/O syscall swap.
    ;; Algorithmic body of `(nelisp--write-stderr-line STR)' moves
    ;; into Phase 47 elisp via the new §122.H `str-bytes-ptr' grammar
    ;; op (= Rust `nl_str_bytes_ptr' extern that returns the data
    ;; pointer of any string-y Sexp variant safely).  The Rust shim
    ;; keeps arity + tag dispatch + trailing-newline + flush; the
    ;; elisp body is one `extern-call write 2 ptr len' syscall.
    (nelisp-cc-bi-write-stderr-line
     :source-var nelisp-cc-bi-write-stderr-line--source
     :output "nelisp_bi_write_stderr_line.o"
     :requires-arch x86_64)
    ;; Doc 117 §117.B (cont) — I/O syscall sweep batch.
    ;; `(nelisp--write-stdout-bytes STR)' — twin of write-stderr-line
    ;; modulo (fd=1, no trailing newline).  Same §122.H grammar.
    (nelisp-cc-bi-write-stdout-bytes
     :source-var nelisp-cc-bi-write-stdout-bytes--source
     :output "nelisp_bi_write_stdout_bytes.o"
     :requires-arch x86_64)
    ;; `(read-stdin-bytes LIMIT)' — the libc `read(0, buf, limit)'
    ;; syscall lifted into elisp.  Buffer alloc + UTF-8 lossy wrap
    ;; stay in Rust (= `from_utf8_lossy' has no Phase 47 grammar
    ;; equivalent; future §122.X `sexp-write-str-lossy' would let the
    ;; full body migrate).  Elisp body returns the i64 byte count.
    (nelisp-cc-bi-read-stdin-bytes
     :source-var nelisp-cc-bi-read-stdin-bytes--source
     :output "nelisp_bi_read_stdin_bytes.o"
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
    ;; Doc 119 §119.A — auto-vivify fold.  Two new building-block
    ;; helpers (`mirror_alloc_entry' + `mirror_bucket_prepend') plus
    ;; four `_or_insert' wrappers that absorb the miss-path of helpers
    ;; #7 / #8 / #11 / #12 into pure elisp (= drops `mirror_insert_new_entry'
    ;; + `mirror_prepend_to_bucket' from `env_helpers.rs').  Linux-
    ;; x86_64 only — same arch gate as the Group A/B mirror helpers.
    (nelisp-cc-mirror-alloc-entry
     :source-var nelisp-cc-mirror-alloc-entry--source
     :output "nelisp_mirror_alloc_entry.o"
     :requires-arch x86_64)
    (nelisp-cc-mirror-bucket-prepend
     :source-var nelisp-cc-mirror-bucket-prepend--source
     :output "nelisp_mirror_bucket_prepend.o"
     :requires-arch x86_64)
    (nelisp-cc-mirror-set-value-or-insert
     :source-var nelisp-cc-mirror-set-value-or-insert--source
     :output "nelisp_mirror_set_value_or_insert.o"
     :requires-arch x86_64)
    (nelisp-cc-mirror-set-function-or-insert
     :source-var nelisp-cc-mirror-set-function-or-insert--source
     :output "nelisp_mirror_set_function_or_insert.o"
     :requires-arch x86_64)
    (nelisp-cc-mirror-set-constant-or-insert
     :source-var nelisp-cc-mirror-set-constant-or-insert--source
     :output "nelisp_mirror_set_constant_or_insert.o"
     :requires-arch x86_64)
    (nelisp-cc-mirror-install-entry-or-insert
     :source-var nelisp-cc-mirror-install-entry-or-insert--source
     :output "nelisp_mirror_install_entry_or_insert.o"
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
    ;; 5 of 11 trampolines (= `nl_jit_record_type', `nl_jit_record_len',
    ;; `nl_jit_record_ref', `nl_jit_record_set', `nl_jit_record_alloc')
    ;; move to Phase 47 elisp; the 6 non-record trampolines
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
    (nelisp-cc-jit-record
     :source-var nelisp-cc-jit-record-alloc--source
     :output "nl_jit_record_alloc.o"
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
    (nelisp-cc-sexp-write-str
     :source-var nelisp-cc-jit-intern--source
     :output "nl_jit_intern.o"
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
    ;; `extern-call' — now provided by Phase-47 elisp objects below
    ;; (= §120.D BoolVector sub-arm swap, Rust bodies deleted).
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
    ;; Doc 120 §120.D sub-arm helpers — Phase-47 elisp replacements for
    ;; the two narrow Rust `#[no_mangle]' externs in `jit/access.rs'
    ;; that the BoolVector arms of `aref' / `aset' call via `extern-call'.
    ;; Deleted from Rust; provided by these two objects.
    (nelisp-cc-jit-access-aref-bool-vector-inner
     :source-var nelisp-cc-jit-access-aref-bool-vector-inner--source
     :output "nl_jit_access_aref_bool_vector_inner.o"
     :requires-arch x86_64)
    (nelisp-cc-jit-access-aset-bool-vector-inner
     :source-var nelisp-cc-jit-access-aset-bool-vector-inner--source
     :output "nl_jit_access_aset_bool_vector_inner.o"
     :requires-arch x86_64)
    ;; Doc 120 §120.C — `jit/cons.rs' 4 of 5 trampoline swaps
    ;; (`cons_car' / `_cdr' / `_setcar' / `_setcdr'; `_make' stays
    ;; Rust per blocker note in `jit/cons.rs').  Linux-x86_64 only —
    ;; `extern-call' ABI ships aarch64 in follow-up.
    (nelisp-cc-jit-cons
     :source-var nelisp-cc-jit-cons-car--source
     :output "nelisp_jit_cons_car.o"
     :requires-arch x86_64)
    (nelisp-cc-jit-cons
     :source-var nelisp-cc-jit-cons-cdr--source
     :output "nelisp_jit_cons_cdr.o"
     :requires-arch x86_64)
    (nelisp-cc-jit-cons
     :source-var nelisp-cc-jit-cons-setcar--source
     :output "nelisp_jit_cons_setcar.o"
     :requires-arch x86_64)
    (nelisp-cc-jit-cons
     :source-var nelisp-cc-jit-cons-setcdr--source
     :output "nelisp_jit_cons_setcdr.o"
     :requires-arch x86_64)
    ;; `nl_cons_car_ptr' / `nl_cons_cdr_ptr' — narrow slot-pointer
    ;; helpers used by `nelisp_jit_cons_car' / `nelisp_jit_cons_cdr'
    ;; via `extern-call'.  Replaced from Rust `jit/cons.rs'.
    ;; car_ptr = sexp-payload-ptr (NlConsBox* = &car @ offset 0).
    ;; cdr_ptr = sexp-payload-ptr + 32 (= &cdr = box + sizeof::<Sexp>()).
    ;; Linux-x86_64 only — same arch gate as the §120.C sibling trampolines.
    (nelisp-cc-jit-cons-car-ptr
     :source-var nelisp-cc-jit-cons-car-ptr--source
     :output "nelisp_nl_cons_car_ptr.o"
     :requires-arch x86_64)
    (nelisp-cc-jit-cons-cdr-ptr
     :source-var nelisp-cc-jit-cons-cdr-ptr--source
     :output "nelisp_nl_cons_cdr_ptr.o"
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
    (nelisp-cc-mut-str
     :source-var nelisp-cc-jit-make-mut-str--source
     :output "nl_jit_make_mut_str.o"
     :requires-arch x86_64)
    (nelisp-cc-mut-str
     :source-var nelisp-cc-jit-mut-str-len--source
     :output "nl_jit_mut_str_len.o"
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
    ;; Doc 122 §122.I — CString construction helper.  Single `(seq
    ;; DEFUN ...)' manifest exposing `nelisp_cstr_from_sexp(str-ptr)
    ;; -> *mut u8' + `nelisp_cstr_drop(buf-ptr, size) -> 1' + the
    ;; internal `_copy_loop' / `_inner' / `_prog2' helpers.  Composes
    ;; only existing Phase 47 grammar (§101.C str-len/byte-at, §122.E
    ;; ptr-write-u8, §125.A alloc-bytes/dealloc-bytes) — no new opcode.
    ;; Same Linux-x86_64 gate as the §125.A parent (= the underlying
    ;; alloc-bytes / ptr-write-u8 emit paths are x86_64-only today).
    ;; Unlocks Doc 117 §117.D.gaps.3 Tier C (= bi_open / bi_stat /
    ;; bi_mkdir / ~12 file-I/O handlers can materialise libc
    ;; `const char *path' arguments in pure Phase 47 elisp).
    (nelisp-cc-cstr-helpers
     :source-var nelisp-cc-cstr-helpers--source
     :output "nelisp_cstr_helpers.o"
     :requires-arch x86_64)
    ;; Doc 117 §117.D.gaps.3 file-I/O sweep (powered by the §122.I
    ;; CString helper above).  Three handlers consumed:
    ;;   - `bi_syscall_stat'         -> libc `stat(2)' kernel
    ;;   - `bi_syscall_canonicalize' -> libc `realpath(3)' kernel
    ;;   - `bi_nl_write_file'        -> libc `open(2)' + `write(2)' +
    ;;                                  `close(2)' chained kernel.
    ;; Each elisp body owns the path CString lifecycle (= alloc via
    ;; §122.I, free via §125.A) and the libc syscall edge.  The Rust
    ;; shim retains arg validation + result-buffer allocation + Sexp
    ;; wrap (= `Sexp::Symbol' tag for stat, `Sexp::Str' for
    ;; canonicalize, `Sexp::T' / Internal-err for write-file).
    ;; Linux-x86_64 only — same arch gate as the §122.I parent.
    (nelisp-cc-bi-syscall-stat
     :source-var nelisp-cc-bi-syscall-stat--source
     :output "nelisp_bi_syscall_stat.o"
     :requires-arch x86_64)
    (nelisp-cc-bi-syscall-canonicalize
     :source-var nelisp-cc-bi-syscall-canonicalize--source
     :output "nelisp_bi_syscall_canonicalize.o"
     :requires-arch x86_64)
    (nelisp-cc-bi-nl-write-file
     :source-var nelisp-cc-bi-nl-write-file--source
     :output "nelisp_bi_nl_write_file.o"
     :requires-arch x86_64)
    ;; Doc 117 §117.D.gaps.3 (cont.) — second file-I/O sweep batch.
    ;; Two additional handlers consumed (after the 3-handler initial
    ;; batch above):
    ;;   - `bi_nl_make_directory'   -> libc `mkdir(2)' kernel.
    ;;   - `bi_syscall_read_file'   -> libc `open(2)' + `read(2)' +
    ;;                                 `close(2)' chained kernel
    ;;                                 (mirror twin of `bi_nl_write_file').
    ;; Linux-x86_64 only — same arch gate as the §122.I parent.
    (nelisp-cc-bi-nl-make-directory
     :source-var nelisp-cc-bi-nl-make-directory--source
     :output "nelisp_bi_nl_make_directory.o"
     :requires-arch x86_64)
    (nelisp-cc-bi-syscall-read-file
     :source-var nelisp-cc-bi-syscall-read-file--source
     :output "nelisp_bi_syscall_read_file.o"
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
    ;; Doc 123 §123.D — walk-children + payload-ptr (the last MEDIUM
    ;; stage of Doc 123's substrate elisp化 chain).  Two source files:
    ;; `nelisp_rc_payload_ptr' (= `ptr-read-u64' at offset 8 of the
    ;; outer `Sexp' = `SEXP_PAYLOAD_OFFSET'; mirrors `bi_nl_rc_payload_ptr'
    ;; Cons arm) and `nelisp_gc_walk_children' (= two `cons-make' allocs
    ;; driven by `ptr-read-u64' for the box-ptr extraction; mirrors
    ;; `bi_nl_gc_walk_children' Cons arm via `Sexp::list_from(&[car, cdr])').
    ;; Cons-only support; Vector/Record/Cell child walks DEFERRED.
    (nelisp-cc-rc-payload-ptr
     :source-var nelisp-cc-rc-payload-ptr--source
     :output "nelisp_rc_payload_ptr.o"
     :requires-arch x86_64)
    (nelisp-cc-gc-walk-children
     :source-var nelisp-cc-gc-walk-children--source
     :output "nelisp_gc_walk_children.o"
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
    ;; Doc 124 §124.L+ — Drop kernels for the remaining 2 NlBox types
    ;; (NlBoolVector + NlCharTable) so the legacy `nlrc_drop_box!' macro
    ;; in `build-tool/src/eval/nlrc.rs' has zero callers and is deleted.
    ;; Same shape as §124.G-K modulo per-type layout literals:
    ;;   NlBoolVector: REFCOUNT_OFFSET = 24,  SIZE = 32,  ALIGN = 8
    ;;   NlCharTable:  REFCOUNT_OFFSET = 120, SIZE = 128, ALIGN = 8
    (nelisp-cc-nlboolvector-drop
     :source-var nelisp-cc-nlboolvector-drop--source
     :output "nelisp_nlboolvector_drop.o"
     :requires-arch x86_64)
    (nelisp-cc-nlchartable-drop
     :source-var nelisp-cc-nlchartable-drop--source
     :output "nelisp_nlchartable_drop.o"
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
    ;; Doc 127 — `(signal TAG DATA)' tag-dispatch swap.  Single manifest
    ;; entry; the body does a 3-way `symbol-eq' chain and returns an
    ;; i64 discriminant (0=quit / 1=arith-error / 2=wrong-type-argument /
    ;; 3=user-error).  The Rust shim keeps arity check, WrongType guard,
    ;; EvalError construction, and data-field extraction; only the symbol
    ;; name comparison moves into elisp (>= 20 Rust LOC saved).
    ;; Linux-x86_64 only -- `symbol-eq' uses the x86_64 string-eq-core.
    (nelisp-cc-bi-signal
     :source-var nelisp-cc-bi-signal--dispatch-source
     :output "nelisp_bi_signal_dispatch.o"
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
    ;; Doc 116 §116.B — pure-elisp Reader parser.  Single manifest
    ;; entry; the source defconst is a `(seq DEFUN ...)' of ~25
    ;; mutually-recursive helpers + one public entry
    ;; `nelisp_reader_parse_one'.  Consumes §116.A's token stream
    ;; via `extern-call nelisp_reader_lex_one'.  Linux-x86_64 only.
    (nelisp-cc-reader-parser
     :source-var nelisp-cc-reader-parser--source
     :output "nelisp_reader_parse_one.o"
     :requires-arch x86_64)
    ;; Doc 122 §122.B / Doc 120 §120.B — `jit/box_accessor.rs'
    ;; `nl_jit_bool_vector_len' trampoline swap.  Reads Vec<bool>.length
    ;; via two `ptr-read-u64' hops (Sexp payload → NlBoolVector* → offset
    ;; 16) and writes `Sexp::Int(len)' to *out.  Rust body deleted.
    ;; `bridge.rs::_ELISP_ARCHIVE_ANCHOR' anchors the symbol for dlsym.
    (nelisp-cc-jit-bool-vector-len
     :source-var nelisp-cc-jit-bool-vector-len--source
     :output "nl_jit_bool_vector_len.o"
     :requires-arch x86_64)
    ;; Doc 122 §122.D / Doc 120 §120.B — `jit/box_accessor.rs'
    ;; `nl_jit_str_codepoint_at' trampoline swap.  Char-indexed UTF-8
    ;; codepoint read via a tail-recursive `str-codepoint-at' byte
    ;; walker with scratch slots at out+8 / out+16.  Rust body deleted.
    ;; `bridge.rs::_ELISP_ARCHIVE_ANCHOR' anchors the public symbol.
    (nelisp-cc-jit-str-codepoint-at
     :source-var nelisp-cc-jit-str-codepoint-at--source
     :output "nl_jit_str_codepoint_at.o"
     :requires-arch x86_64)
    ;; Doc 122 §122.A + §122.E — `jit/strings.rs' `nl_jit_make_symbol'
    ;; trampoline swap.  Per-process counter (surfaced by the Rust
    ;; `nl_make_symbol_counter_ptr' getter) + name-copy loop + 20-byte
    ;; literal suffix + 16-nibble hex formatter, all in Phase 47 elisp.
    ;; Seven-entry `(seq DEFUN ...)' manifest: prog2 + copy + hex + suffix
    ;; + write + inner + public entry.  Rust body deleted;
    ;; `MAKE_SYMBOL_COUNTER: AtomicI64' static + getter remain in Rust.
    ;; `bridge.rs::_ELISP_ARCHIVE_ANCHOR' anchors `nl_jit_make_symbol'.
    (nelisp-cc-jit-make-symbol
     :source-var nelisp-cc-jit-make-symbol--source
     :output "nl_jit_make_symbol.o"
     :requires-arch x86_64)
    (nelisp-cc-jit-type-of
     :source-var nelisp-cc-jit-type-of--source
     :output "nl_jit_type_of.o"
     :requires-arch x86_64)
    (nelisp-cc-jit-sxhash
     :source-var nelisp-cc-jit-sxhash--source
     :output "nl_jit_sxhash.o"
     :requires-arch x86_64)
    ;; db33abdd (2026-05-19) — `jit/box_accessor.rs' `nl_record_type_tag_ptr'
    ;; Rust body deleted; Phase-47-compiled elisp `.o' replaces it.
    ;; NlRecord::type_tag is at offset 0, so sexp-payload-ptr gives *const Sexp
    ;; directly.  bridge.rs anchor re-added (was reverted at bf670ee4).
    (nelisp-cc-jit-record-type-tag-ptr
     :source-var nelisp-cc-jit-record-type-tag-ptr--source
     :output "nl_record_type_tag_ptr.o"
     :requires-arch x86_64)
    ;; Doc 86 §86.1.e.2 (2026-05-19) — `jit/strings.rs' `nl_jit_concat_ints'
    ;; Rust body deleted; Phase-47-compiled elisp `.o' replaces it.
    ;; Uses `mut-str-make-empty' / `mut-str-push-codepoint' / `mut-str-finalize'
    ;; (§122.B grammar ops) to build the result Sexp::Str incrementally.
    ;; `bridge.rs::_ELISP_ARCHIVE_ANCHOR' (count 49→51) anchors `nl_jit_concat_ints'.
    (nelisp-cc-jit-concat-ints
     :source-var nelisp-cc-jit-concat-ints--source
     :output "nl_jit_concat_ints.o"
     :requires-arch x86_64)
    ;; Doc 86 §86.2 (2026-05-19) — `sf_quote' Rust body deleted from
    ;; `build-tool/src/eval/special_forms.rs'; Phase-47-compiled elisp
    ;; `.o' replaces it.  Thin Rust shell in `sf_quote' calls
    ;; `crate::elisp_cc_spike::sf_quote_call(args, &mut out)'.
    (nelisp-cc-sf-quote
     :source-var nelisp-cc-sf-quote--source
     :output "nl_sf_quote.o"
     :requires-arch x86_64)
    ;; Phase 47 elisp migration — `nl_jit_downcase' trampoline swap.
    ;; ASCII A-Z fold (bytes 65-90 → +32); non-ASCII bytes pass
    ;; through unchanged.  Rust body deleted from `jit/strings.rs'.
    ;; `bridge.rs::_ELISP_ARCHIVE_ANCHOR' anchors `nl_jit_downcase'.
    (nelisp-cc-jit-downcase
     :source-var nelisp-cc-jit-downcase--source
     :output "nl_jit_downcase.o"
     :requires-arch x86_64)
    ;; Phase 47 elisp migration — `nl_jit_upcase' trampoline swap.
    ;; ASCII a-z fold (bytes 97-122 → -32); non-ASCII bytes pass
    ;; through unchanged.  Rust body deleted from `jit/strings.rs'.
    ;; `bridge.rs::_ELISP_ARCHIVE_ANCHOR' anchors `nl_jit_upcase'.
    (nelisp-cc-jit-upcase
     :source-var nelisp-cc-jit-upcase--source
     :output "nl_jit_upcase.o"
     :requires-arch x86_64)
    ;; Phase 47 elisp migration — `nl_jit_split_by_non_alnum' trampoline
    ;; swap.  Splits STR-ARG at non-alphanumeric bytes (ASCII [0-9A-Za-z]
    ;; + bytes>=128 as word-continuation); builds a reversed cons list of
    ;; Sexp::Str parts in *OUT via `sexp-write-str' + `cons-make' +
    ;; `ptr-write-u8' (RC-safe byte-copy pattern).  The Elisp wrapper
    ;; applies `nreverse' to restore forward order.  Rust body deleted
    ;; from `jit/strings.rs'.  `bridge.rs::_ELISP_ARCHIVE_ANCHOR'
    ;; (count 54→55) anchors `nl_jit_split_by_non_alnum'.
    (nelisp-cc-jit-split-by-non-alnum
     :source-var nelisp-cc-jit-split-by-non-alnum--source
     :output "nl_jit_split_by_non_alnum.o"
     :requires-arch x86_64)
    ;; Phase 47 Tier-1 special forms elisp化.
    ;; `nl_sf_progn' replaces the `sf_progn' Rust body in
    ;; `eval/special_forms.rs'.
    (nelisp-cc-sf-progn
     :source-var nelisp-cc-sf-progn--source
     :output "nl_sf_progn.o"
     :requires-arch x86_64)
    ;; `nl_sf_if' replaces the `sf_if' Rust body.
    (nelisp-cc-sf-if
     :source-var nelisp-cc-sf-if--source
     :output "nl_sf_if.o"
     :requires-arch x86_64)
    ;; `nl_sf_setq' replaces the `sf_setq' Rust body.
    (nelisp-cc-sf-setq
     :source-var nelisp-cc-sf-setq--source
     :output "nl_sf_setq.o"
     :requires-arch x86_64)
    ;; `nl_sf_while' replaces the `sf_while' Rust body.
    (nelisp-cc-sf-while
     :source-var nelisp-cc-sf-while--source
     :output "nl_sf_while.o"
     :requires-arch x86_64)
    ;; Phase 47 Tier-1 special forms elisp化 — let / let*.
    ;; `nl_sf_let' replaces the `sf_let' Rust body.  Uses the new
    ;; `nl_let_setup(bindings, env, sequential=0)' + `nl_env_pop_frame(env)'
    ;; externs to delegate frame-push + bind to Rust, keeping the
    ;; body-eval + frame-pop control flow in elisp.
    (nelisp-cc-sf-let
     :source-var nelisp-cc-sf-let--source
     :output "nl_sf_let.o"
     :requires-arch x86_64)
    ;; `nl_sf_let_star' replaces the `sf_let_star' Rust body.  Identical
    ;; structure to `nl_sf_let' but calls nl_let_setup with sequential=1.
    (nelisp-cc-sf-let-star
     :source-var nelisp-cc-sf-let-star--source
     :output "nl_sf_let_star.o"
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
