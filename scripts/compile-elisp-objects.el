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
    ;; Doc 119 §119.A — `(truncate FLOAT)' Float arm via G4+G5.
    ;; `sexp-float-unwrap' + `bits-to-f64' + `f64-to-i64-trunc' replaces
    ;; the Rust inline `Sexp::Float(x) => Ok(Sexp::Int(*x as i64))'.
    ;; Linux-x86_64 only (f64-to-i64-trunc / bits-to-f64 MVP scope).
    (nelisp-cc-truncate-float
     :source-var nelisp-cc-truncate-float--source
     :output "nelisp_truncate_float.o"
     :requires-arch x86_64)
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
    ;; `nl-fact-i64' Rust wrapper swap — range-check + extern-call fact_i64.
    (nelisp-cc-bi-nl-fact-i64
     :source-var nelisp-cc-bi-nl-fact-i64--source
     :output "nelisp_bi_nl_fact_i64.o"
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
    ;; Wave i — frame_stack_install_sexp body → Phase 47 .o.
    (nelisp-cc-frame-stack-install
     :source-var nelisp-cc-frame-stack-install--source
     :output "nelisp_frame_stack_install.o"
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
    ;; Doc 86 §86.4 — `nelisp--env-globals-op' read/clear/pred dispatch
    ;; (7 of 10 arms): get-value / get-function / is-bound / is-fbound /
    ;; is-constant / clear-value / clear-function.  Returns i64 result code;
    ;; set-* and capture-lexical handled by Rust.  Linux-x86_64 only —
    ;; same `extern-call' + symbol-name-eq ABI gate as the §111.E sibling
    ;; helpers.
    (nelisp-cc-env-shim-op
     :source-var nelisp-cc-env-shim-op--source
     :output "nelisp_env_shim_op.o"
     :requires-arch x86_64)
    ;; Wave c+ — `nelisp--env-globals-op' set-* dispatch (3 arms):
    ;; set-value / set-function / set-constant.  Replaces 3 Rust match
    ;; arms in `env_shim.rs::bi_globals_op'; Rust thin wrapper pre-builds
    ;; the scratch vector and this .o dispatches + clones result back.
    (nelisp-cc-env-shim-set-op
     :source-var nelisp-cc-env-shim-set-op--source
     :output "nelisp_env_shim_set_op.o"
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
    ;; Doc 120 §120.B — `jit/box_accessor.rs' record family swap + later full delete.
    ;; 5 record trampolines moved first; 3 remaining (mut-str-set-codepoint /
    ;; char-table-aref / char-table-aset) moved last + file deleted (-74 LOC).
    ;; See manifest tail for the 3 final entries.
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
    ;; Doc 120.E — fused `cons-make-with-clone' grammar replaces the
    ;; last `jit/cons.rs' trampoline (`nl_jit_cons_make').  Allocates
    ;; an `NlConsBox' + deep-clones car/cdr via `nl_sexp_clone_into' +
    ;; tags the out slot as `Sexp::Cons(box)' — all in elisp.
    (nelisp-cc-jit-cons-make
     :source-var nelisp-cc-jit-cons-make--source
     :output "nelisp_jit_cons_make.o"
     :requires-arch x86_64)
    ;; Doc 117.D — bi_syscall name→nr resolver Phase 47 swap.  25-entry
    ;; (symbol-name-eq) chain replaces the inline `match' arm in the
    ;; Linux x86_64 `bi_syscall' body, letting the Rust shim shrink to
    ;; arity-check + Int passthrough + extern dispatch.
    (nelisp-cc-bi-syscall-resolve-nr
     :source-var nelisp-cc-bi-syscall-resolve-nr--source
     :output "nelisp_bi_syscall_resolve_nr.o"
     :requires-arch x86_64)
    ;; Doc 118 — `nelisp--f64-trunc' mode-dispatch + div + truncate swap.
    ;; Rust shim keeps arity check + WrongType guard; computation moves
    ;; to Phase 47 elisp via `nl_bi_f64_trunc_div_bits' Rust helper.
    ;; Linux-x86_64 only (f64-to-i64-trunc / bits-to-f64 MVP scope).
    (nelisp-cc-bi-f64-trunc
     :source-var nelisp-cc-bi-f64-trunc--source
     :output "nl_bi_f64_trunc_impl.o"
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
    ;; Doc 125 §125.B — mmap-backed `nl_alloc_bytes' / `nl_dealloc_bytes'
    ;; implementations.  These replace the `std::alloc'-based Rust bodies
    ;; in `build-tool/src/eval/raw_mem.rs' with Phase 47-compiled elisp
    ;; that emits inline SYSCALL via the new `syscall-direct' grammar op.
    ;; Must precede the §125.A alloc-dealloc entries below so the PLT
    ;; calls from `nelisp_alloc_bytes.o' / `nelisp_dealloc_bytes.o'
    ;; resolve to these mmap implementations rather than the Rust externs.
    (nelisp-cc-alloc-mem
     :source-var nelisp-cc-alloc-mem--alloc-source
     :output "nl_alloc_bytes_mmap.o"
     :requires-arch x86_64)
    (nelisp-cc-alloc-mem
     :source-var nelisp-cc-alloc-mem--dealloc-source
     :output "nl_dealloc_bytes_mmap.o"
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
    ;; Wave A25.1-min — Phase 47 self-application foundation (getenv +
    ;; stat-mtime only).  `locate-file' deferred — its `_branch' helper
    ;; needs an arity-≤6 split to fit Phase 47 SysV AMD64's 6 GP regs.
    ;; Both shipped helpers compose only existing Phase 47 grammar; they
    ;; are consumed by the future A25.2 driver rewrite of
    ;; `compile-elisp-objects.el' as Phase 47 native main entry.
    ;; Linux-x86_64 only (same gate as parents).
    (nelisp-cc-bi-getenv
     :source-var nelisp-cc-bi-getenv--source
     :output "nelisp_bi_getenv.o"
     :requires-arch x86_64)
    (nelisp-cc-bi-syscall-stat-mtime
     :source-var nelisp-cc-bi-syscall-stat-mtime--source
     :output "nelisp_bi_syscall_stat_mtime.o"
     :requires-arch x86_64)
    ;; Wave A25.2 — Phase 47 self-application driver kernel (PoC).
    ;; `nelisp_meta_should_rebuild' composes two `nelisp_bi_syscall_stat_mtime'
    ;; calls + a pure-arithmetic decision (= src missing / out missing /
    ;; out_mtime < src_mtime).  Foundation for the future A25.3 standalone
    ;; bootstrap of the meta-driver iteration loop.  Composes only existing
    ;; Phase 47 grammar — no new opcode.  Linux-x86_64 only (= same gate as
    ;; the A25.1-min parents).
    (nelisp-cc-meta-driver
     :source-var nelisp-cc-meta-driver--source
     :output "nelisp_meta_should_rebuild.o"
     :requires-arch x86_64)
    ;; Wave A26 — Phase 47 manifest walker kernel.  `nelisp_meta_walk'
    ;; batch-evaluates the per-entry up-to-date check by chaining
    ;; `extern-call' calls into the A25.2 `nelisp_meta_should_rebuild'
    ;; kernel for every (src, out) pair in the input vector pair, then
    ;; packs the 0/1 decisions into a single i64 bitmask written to
    ;; the caller's result slot.  Collapses N elisp -> native bridge
    ;; calls into 1 bridge call; the elisp driver only walks the
    ;; returned bitmask to dispatch the (heavy) emit step.  Composes
    ;; only existing Phase 47 grammar — no new opcode.  Linux-x86_64
    ;; only (= same gate as the A25.2 parent).
    (nelisp-cc-bi-meta-walk
     :source-var nelisp-cc-bi-meta-walk--source
     :output "nelisp_meta_walk.o"
     :requires-arch x86_64)
    ;; Wave A30 — Phase 47 per-entry dispatch loop kernel.
    ;; `nelisp_meta_dispatch_loop' collapses the 212-iter elisp dispatch
    ;; loop in `compile-elisp-objects-meta--walk' (= the final cached-path
    ;; bottleneck after A26-A29 walker chain) into a single Phase 47
    ;; native call.  Per-chunk walker computes `(dirty AND NOT arch-skip)'
    ;; emit-needed bitmasks via `vector-slot-set' into a pre-allocated
    ;; result vector, and accumulates popcount of `(NOT arch-skip)' into
    ;; the caller-owned result slot.  Composes only existing Phase 47
    ;; grammar (= §111.C vector ops + §125.A alloc-bytes + §100.D logand/
    ;; logxor + §100 sexp-int) — no new opcode, no Rust extern added.
    ;; Linux-x86_64 only (= same gate as the A25.2 / A26 parents).
    (nelisp-cc-bi-meta-dispatch-loop
     :source-var nelisp-cc-bi-meta-dispatch-loop--source
     :output "nelisp_meta_dispatch_loop.o"
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
    ;; nl_consbox_set_car Phase 47 cutover spike — copies the 32-byte
    ;; Sexp at VAL into the car slot (offset 0) of the NlConsBox at BOX
    ;; via four ptr-write-u64 / ptr-read-u64 word-copy pairs.  Resolves
    ;; the PLT reloc emitted by the `cons-set-car' grammar op and the
    ;; 28-symbol undefined list introduced when commit fa8932eb deleted
    ;; the Rust `nl_consbox_set_car' body.  x86_64 only (= same gate as
    ;; other ptr-read/write-u64 consumers).
    (nelisp-cc-nlconsbox-set-car
     :source-var nelisp-cc-nlconsbox-set-car--source
     :output "nl_consbox_set_car.o"
     :requires-arch x86_64)
    ;; Doc 133 batch — nl_consbox_set_cdr: copies 32 bytes (4 × u64)
    ;; from val into box+32..+63 (= cdr slot, offset 32 per NlConsBox
    ;; layout).  Same raw-copy spike scope as nl_consbox_set_car.
    (nelisp-cc-nlconsbox-set-cdr
     :source-var nelisp-cc-nlconsbox-set-cdr--source
     :output "nl_consbox_set_cdr.o"
     :requires-arch x86_64)
    ;; Doc 133 batch — nl_cell_set_value: copies 32 bytes (4 × u64)
    ;; from val into cell+0..+31 (= NlCell.value, offset 0).
    (nelisp-cc-nlcell-set-value
     :source-var nelisp-cc-nlcell-set-value--source
     :output "nl_cell_set_value.o"
     :requires-arch x86_64)
    ;; Doc 133 batch — nl_cell_get_value: two-hop — reads NlCell* from
    ;; cell_ptr+8 (Sexp::Cell payload), copies NlCell.value (offset 0,
    ;; 32 bytes) into *out, returns 0.
    (nelisp-cc-nlcell-get-value
     :source-var nelisp-cc-nlcell-get-value--source
     :output "nl_cell_get_value.o"
     :requires-arch x86_64)
    ;; Doc 133 batch — nl_vector_set_slot: peeks Vec.data_ptr from
    ;; vec_ptr+8, writes 32 bytes to data_ptr + n*32 (= n << 5).
    (nelisp-cc-nlvector-set-slot
     :source-var nelisp-cc-nlvector-set-slot--source
     :output "nl_vector_set_slot.o"
     :requires-arch x86_64)
    ;; Doc 133 batch — nl_record_set_slot: peeks Vec.data_ptr from
    ;; record+40 (= NlRecord.slots.ptr), writes 32 bytes to
    ;; data_ptr + n*32 (= n << 5).
    (nelisp-cc-nlrecord-set-slot
     :source-var nelisp-cc-nlrecord-set-slot--source
     :output "nl_record_set_slot.o"
     :requires-arch x86_64)
    ;; nl_alloc_consbox elisp swap — allocates a fresh NlConsBox
    ;; (car=Nil, cdr=Nil, refcount=1) using alloc-bytes + sexp-write-nil
    ;; + ptr-write-u64.  Replaces the Rust `nl_alloc_consbox' body in
    ;; `build-tool/src/eval/nlconsbox.rs'.  Output symbol name matches
    ;; the PLT reloc target emitted by the Phase 47 cons-make /
    ;; cons-make-with-clone emitters.
    (nelisp-cc-nlconsbox-alloc
     :source-var nelisp-cc-nlconsbox-alloc--source
     :output "nl_alloc_consbox.o"
     :requires-arch x86_64)
    ;; nl_alloc_cell — Phase 47 migration of `nl_alloc_cell' from
    ;; `build-tool/src/eval/nlcell.rs'.  Allocates a 40-byte NlCell
    ;; (value = clone of *initial via nl_sexp_clone_into, refcount = 1).
    (nelisp-cc-nlcell-alloc
     :source-var nelisp-cc-nlcell-alloc--source
     :output "nl_alloc_cell.o"
     :requires-arch x86_64)
    ;; nl_alloc_vector — Phase 47 migration of `nl_alloc_vector' from
    ;; `build-tool/src/eval/nlvector.rs'.  Allocates a 32-byte NlVector
    ;; with a cap*32-byte Nil-filled element buffer (Vec.capacity, ptr,
    ;; len = cap; refcount = 1).
    (nelisp-cc-nlvector-alloc
     :source-var nelisp-cc-nlvector-alloc--source
     :output "nl_alloc_vector.o"
     :requires-arch x86_64)
    ;; nl_alloc_record — Phase 47 migration of `nl_alloc_record' from
    ;; `build-tool/src/eval/nlrecord.rs'.  Allocates a 64-byte NlRecord
    ;; with type_tag clone + n*32-byte Nil-filled slots buffer (refcount = 1).
    (nelisp-cc-nlrecord-alloc
     :source-var nelisp-cc-nlrecord-alloc--source
     :output "nl_alloc_record.o"
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
    (nelisp-cc-nlboolvector-clone
     :source-var nelisp-cc-nlboolvector-clone--source
     :output "nelisp_nlboolvector_clone.o"
     :requires-arch x86_64)
    (nelisp-cc-nlchartable-drop
     :source-var nelisp-cc-nlchartable-drop--source
     :output "nelisp_nlchartable_drop.o"
     :requires-arch x86_64)
    (nelisp-cc-nlchartable-clone
     :source-var nelisp-cc-nlchartable-clone--source
     :output "nelisp_nlchartable_clone.o"
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
    ;; Doc 128 §128.A — nlstr.rs direct-symbol Phase 47 migrations.
    ;; Each entry exports the original `nl_*' C-linkage symbol, replacing
    ;; the Rust `#[no_mangle]' body.  Grammar-op PLT stubs that called the
    ;; Rust externs now resolve to these Phase 47 implementations.
    ;;
    ;;   nl_mut_str_len    — ptr-read-u64 two-hop (sexp→NlStr*→len@16).
    ;;   nl_str_bytes_ptr  — tag-dispatch: MutStr→[NlStr*+8]; Str/Sym→[sexp+16].
    ;;   nl_alloc_str      — alloc-bytes + byte-copy + Sexp::Str header.
    ;;   nl_alloc_symbol   — same as nl_alloc_str with tag=4 (Symbol).
    ;;   nl_alloc_mut_str  — alloc-bytes NlStr box + char buf, write headers.
    ;;   nl_mut_str_finalize — clone MutStr String into fresh Sexp::Str.
    ;;
    ;; `nl_alloc_str' and `nl_alloc_symbol' share a `(seq DEFUN ...)' source
    ;; form compiled into a single `.o'; `nl_mut_str_finalize' similarly
    ;; packages its private copy-loop helper in a `(seq DEFUN ...)'.
    ;; Net Rust LOC deleted: ~58 lines (bodies + private helpers with no
    ;; remaining callers: build_string + mut_str_value).
    (nelisp-cc-nlstr-direct-ops
     :source-var nelisp-cc-nlstr-direct-ops--mut-str-len-source
     :output "nl_mut_str_len.o"
     :requires-arch x86_64)
    (nelisp-cc-nlstr-direct-ops
     :source-var nelisp-cc-nlstr-direct-ops--str-bytes-ptr-source
     :output "nl_str_bytes_ptr.o"
     :requires-arch x86_64)
    (nelisp-cc-nlstr-direct-ops
     :source-var nelisp-cc-nlstr-direct-ops--alloc-str-source
     :output "nl_alloc_str.o"
     :requires-arch x86_64)
    (nelisp-cc-nlstr-direct-ops
     :source-var nelisp-cc-nlstr-direct-ops--alloc-mut-str-source
     :output "nl_alloc_mut_str.o"
     :requires-arch x86_64)
    (nelisp-cc-nlstr-direct-ops
     :source-var nelisp-cc-nlstr-direct-ops--mut-str-finalize-source
     :output "nl_mut_str_finalize.o"
     :requires-arch x86_64)
    ;; Wave n2 §n2.A — UTF-8 direct-symbol migrations from nlstr.rs.
    ;; nl_str_char_count: count codepoints by counting non-continuation bytes.
    ;; nl_str_codepoint_at: decode one UTF-8 codepoint at byte_idx.
    ;; nl_str_is_alphanumeric_at: ASCII fast-path + Unicode delegate.
    (nelisp-cc-nlstr-utf8-direct
     :source-var nelisp-cc-nlstr-utf8-direct--char-count-source
     :output "nl_str_char_count.o"
     :requires-arch x86_64)
    (nelisp-cc-nlstr-utf8-direct
     :source-var nelisp-cc-nlstr-utf8-direct--codepoint-at-source
     :output "nl_str_codepoint_at.o"
     :requires-arch x86_64)
    (nelisp-cc-nlstr-utf8-direct
     :source-var nelisp-cc-nlstr-utf8-direct--is-alphanumeric-at-source
     :output "nl_str_is_alphanumeric_at.o"
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
    ;; Doc 122 §122.A — `jit/strings.rs' `nl_jit_symbol_name' Rust body
    ;; deleted; Phase-47-compiled elisp `.o' replaces it.  Symbol(4)/Str(5)
    ;; copy content via str-bytes-ptr + str-len; Nil(0) → "nil"; T(1) → "t".
    ;; bridge.rs anchor entry added; alias("nelisp_jit_symbol_name") unchanged.
    (nelisp-cc-jit-symbol-name
     :source-var nelisp-cc-jit-symbol-name--source
     :output "nl_jit_symbol_name.o"
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
     :requires-arch x86_64)
    ;; `nl_sf_lambda' + `nl_sf_function' replace lambda + function Rust bodies.
    (nelisp-cc-sf-lambda
     :source-var nelisp-cc-sf-lambda--source
     :output "nl_sf_lambda.o"
     :requires-arch x86_64)
    (nelisp-cc-sf-function
     :source-var nelisp-cc-sf-function--source
     :output "nl_sf_function.o"
     :requires-arch x86_64)
    ;; Phase 47 — `nl_sf_condition_case' replaces `sf_condition_case' +
    ;; `clause_matches' + `eval_handler' Rust bodies via the new
    ;; `nl_cc_match_and_bind' + existing `nelisp_eval_call_with_err' +
    ;; `nl_env_pop_frame' externs in `eval/special_forms.rs' / `eval/mod.rs'.
    (nelisp-cc-sf-condition-case
     :source-var nelisp-cc-sf-condition-case--source
     :output "nl_sf_condition_case.o"
     :requires-arch x86_64)
    ;; Phase 47 — `apply_lambda_inner' body deleted from `eval/mod.rs'.
    ;; `nl_apply_lambda_inner' orchestrates frame-push + formals-bind +
    ;; body-eval + frame-pop in elisp.  Actual formals-binding delegated
    ;; to `nl_push_and_bind' Rust extern in `eval/special_forms.rs'.
    (nelisp-cc-apply-lambda-inner
     :source-var nelisp-cc-apply-lambda-inner--source
     :output "nl_apply_lambda_inner.o"
     :requires-arch x86_64)
    ;; Phase 47 — `nl_bf_formal_tag' formal-parameter tag classifier.
    ;; Replaces the 9-LOC Rust extern in `special_forms.rs'.
    ;; Returns 1=&optional, 2=&rest, 0=other-symbol, -1=non-symbol.
    ;; Called from `nl_bind_formals_impl.o' via extern-call.
    (nelisp-cc-bf-formal-tag
     :source-var nelisp-cc-bf-formal-tag--source
     :output "nl_bf_formal_tag.o"
     :requires-arch x86_64)
    ;; Phase 47 — `nl_bf_args_nth_ptr' cons-list indexed fetch.
    ;; Replaces the 11-LOC Rust extern in `special_forms.rs'.
    ;; Returns *const Sexp of args[idx].car, or 0 when idx >= len.
    ;; `nl_bf_args_nth_step' is the private recursive walker helper.
    ;; Called from `nl_bind_formals_impl.o' via extern-call.
    (nelisp-cc-bf-args-nth-ptr
     :source-var nelisp-cc-bf-args-nth-ptr--source
     :output "nl_bf_args_nth_ptr.o"
     :requires-arch x86_64)
    ;; Wave j — `nl_bf_precompute' Rust body (19 LOC) → Phase 47 elisp.
    ;; CPS chain: counts required formals (stopping at &optional/&rest),
    ;; counts total args, packs as (req << 36) | (args_len << 20).
    ;; Uses nl_bf_formal_tag extern.  Called from nl_bind_formals_impl.o.
    (nelisp-cc-bf-precompute
     :source-var nelisp-cc-bf-precompute--source
     :output "nl_bf_precompute.o"
     :requires-arch x86_64)
    ;; Phase 47 Tier-C — `bind_formals_impl' Stage 1 parallel implementation.
    ;; `nl_bind_formals_impl' implements the full Required/Optional/Rest state
    ;; machine in elisp.  Stage 2 will rewire `nl_bind_formals' / `nl_push_and_bind'
    ;; to delegate to this elisp entry.  New externs: nl_bf_precompute,
    ;; nl_bf_bind_sym, nl_bf_bind_optional,
    ;; nl_bf_bind_rest, nl_bf_err_arity, nl_bf_err_type, nl_bf_err_dangling_rest.
    ;; (nl_bf_formal_tag, nl_bf_args_nth_ptr now resolved from their own .o)
    (nelisp-cc-bind-formals
     :source-var nelisp-cc-bind-formals--source
     :output "nl_bind_formals_impl.o"
     :requires-arch x86_64)
    ;; Doc 122 §122.J — `sexp.rs' formatter chain elisp化.  Replaces
    ;; `write_quoted_string' / `write_sexp' / `write_reader_macro' /
    ;; `list_tag_and_arg' / `write_list_body' (~155 LOC) with a single
    ;; Phase 47-compiled `.o' that calls two new Rust helpers
    ;; (`nl_i64_append_to_mut_str' / `nl_f64_bits_append_to_mut_str')
    ;; in `nlstr.rs' for int/float formatting.  The Rust `fmt_sexp'
    ;; entry point becomes a thin `cc_wrap' call to `nelisp_fmt_sexp'.
    ;; Linux-x86_64 only — same arch gate as the §122.A/B siblings.
    (nelisp-cc-sexp-fmt
     :source-var nelisp-cc-sexp-fmt--source
     :output "nelisp_fmt_sexp.o"
     :requires-arch x86_64)
    ;; Phase 47 — `nl_cons_prepend_clone' Rust body deleted from
    ;; `build-tool/src/eval/special_forms.rs'; Phase-47-compiled elisp
    ;; `.o' replaces it.  Single defun using the Doc 120.E fused
    ;; `cons-make-with-clone' grammar op (= nl_alloc_consbox + two
    ;; nl_sexp_clone_into calls + tag/payload write into *out).  Alias-safe
    ;; when out = one of the source pointers.  Linux-x86_64 only — same
    ;; arch gate as the other Phase 47 cons-family migrations.
    (nelisp-cc-cons-prepend-clone
     :source-var nelisp-cc-cons-prepend-clone--source
     :output "nl_cons_prepend_clone.o"
     :requires-arch x86_64)
    ;; Phase 47 — `sf_unwind_protect' Rust body deleted from
    ;; `build-tool/src/eval/special_forms.rs'; Phase-47-compiled elisp
    ;; `.o' replaces it.  8 defuns (seq form) walk `(BODYFORM CLEANUP...)'
    ;; args: eval body via `nelisp_eval_call', then eval each cleanup form
    ;; via `nl_eval_is_truthy' (errors silently discarded — does NOT touch
    ;; `nelisp--last-signal-data').  Final rc = body-rc; body error stash
    ;; survives all cleanup steps.  Linux-x86_64 only — same arch gate
    ;; as the other sf_* migrations above.
    (nelisp-cc-sf-unwind-protect
     :source-var nelisp-cc-sf-unwind-protect--source
     :output "nl_sf_unwind_protect.o"
     :requires-arch x86_64)
    ;; Phase 47 swap — `nl_sexp_eq': tag-dispatch equality test replacing
    ;; the `#[no_mangle] extern "C" fn nl_sexp_eq' in special_forms.rs.
    ;; Callers using `extern-call nl_sexp_eq' resolve to this .o unchanged.
    (nelisp-cc-sexp-eq
     :source-var nelisp-cc-sexp-eq--source
     :output "nl_sexp_eq.o")
    ;; Phase 47 swap — `nl_symbol_is_lambda': single `symbol-name-eq' op
    ;; replacing the `#[no_mangle] extern "C" fn nl_symbol_is_lambda' in
    ;; special_forms.rs.  Callers using `extern-call nl_symbol_is_lambda'
    ;; resolve to this .o unchanged.
    (nelisp-cc-symbol-is-lambda
     :source-var nelisp-cc-symbol-is-lambda--source
     :output "nl_symbol_is_lambda.o")
    ;; Phase 47 swap — `nl_jit_secure_hash' SHA1 arm migration from
    ;; `build-tool/src/jit/hash.rs'.  SHA1 is implemented in pure
    ;; Phase 47 bitwise arithmetic; sha224/256/384/512/md5 are now also
    ;; implemented in Phase 47 via nl_jit_secure_hash_non_sha1_ext.
    (nelisp-cc-jit-secure-hash
     :source-var nelisp-cc-jit-secure-hash--source
     :output "nl_jit_secure_hash.o")
    ;; Phase 47 swap — sha224/256/384/512/md5 ext arm.
    ;; Replaces `nl_jit_secure_hash_non_sha1' Rust function in
    ;; `build-tool/src/jit/hash.rs' (-69 LOC, sha2+md5 crates removed).
    (nelisp-cc-jit-secure-hash-ext
     :source-var nelisp-cc-jit-secure-hash-ext--source
     :output "nl_jit_secure_hash_ext.o")
    ;; Phase 47 swap — `nl_jit_string_match_p' literal/anchored fast-path
    ;; migration from `build-tool/src/jit/regex.rs'.  Implements the 7
    ;; hard-coded pattern branches and the fallback literal match in pure
    ;; Phase 47 byte-level ops.  Rust `regex.rs' body deleted (-71 LOC).
    (nelisp-cc-jit-regex
     :source-var nelisp-cc-jit-regex--source
     :output "nl_jit_string_match_p.o")
    ;; Doc 120 alias — `nl_jit_alias' Phase 47 swap: maps user-facing
    ;; JIT names (e.g. `nelisp_jit_car') to canonical dlsym names
    ;; (e.g. `nelisp_jit_cons_car'), replacing the 18-LOC Rust `alias'
    ;; fn in `build-tool/src/jit/bridge.rs'.  Uses the new `sexp-name-eq'
    ;; grammar op (accepts both Symbol tag=4 and Str tag=5).
    (nelisp-cc-jit-alias
     :source-var nelisp-cc-jit-alias--source
     :output "nl_jit_alias.o"
     :requires-arch x86_64)
    ;; Phase 47 — `eval_inner' body + `apply_combiner' cluster (~141 LOC)
    ;; deleted from `build-tool/src/eval/mod.rs'.
    ;; `nl_eval_inner' dispatches on sexp-tag: self-eval / symbol / cons / cell.
    ;; Symbol and Cons paths delegate to new Rust extern helpers
    ;; `nl_apply_symbol_fn' + `nl_eval_apply_cons_head' in `eval/special_forms.rs'
    ;; which implement the full apply_combiner logic (special forms, macros,
    ;; delegation, arg eval, apply_function).  The thin Rust `eval' function
    ;; calls `nl_eval_inner' via cc_wrap.
    (nelisp-cc-eval-inner
     :source-var nelisp-cc-eval-inner--source
     :output "nl_eval_inner.o"
     :requires-arch x86_64)
    ;; Phase 47 — `nl_jit_syscall_call' + `nl_jit_syscall_supported_p'
    ;; Rust bodies deleted from `build-tool/src/jit/syscall.rs' (full
    ;; file delete, -57 LOC).  Uses the new `syscall-direct' grammar op
    ;; that emits the Linux raw SYSCALL instruction directly.
    (nelisp-cc-jit-syscall-call
     :source-var nelisp-cc-jit-syscall-call--source
     :output "nl_jit_syscall_call.o"
     :requires-arch x86_64)
    ;; Doc 120.B residuals — `jit/box_accessor.rs' final 3 trampolines
    ;; (char-table-aref / char-table-aset / mut-str-set-codepoint).
    ;; box_accessor.rs deleted (-74 LOC); thin Rust helpers for the
    ;; opaque CharTable Vec iteration and MutStr set_value moved to
    ;; nlchartable.rs / nlstr.rs; elisp bodies export the ABI symbols.
    ;; Linux-x86_64 only — same extern-call gate as other box_accessor swaps.
    (nelisp-cc-jit-char-table-aref
     :source-var nelisp-cc-jit-char-table-aref--source
     :output "nl_jit_char_table_aref.o"
     :requires-arch x86_64)
    (nelisp-cc-jit-char-table-aset
     :source-var nelisp-cc-jit-char-table-aset--source
     :output "nl_jit_char_table_aset.o"
     :requires-arch x86_64)
    (nelisp-cc-jit-mut-str-set-codepoint
     :source-var nelisp-cc-jit-mut-str-set-codepoint--source
     :output "nl_jit_mut_str_set_codepoint.o"
     :requires-arch x86_64)
    ;; Wave a-2 — Env::{lookup_value,set_value,lookup_function} Phase 47 .o.
    ;; lookup_value: frame-first (nl_cell_get_value = refcount-safe clone)
    ;;   then mirror check.  Fixes Wave a double-free SIGABRT.
    ;; set_value: constant guard → frame write (cell-set-value) → mirror vivify.
    ;; lookup_function: mirror entry check → mirror_lookup_function fill.
    (nelisp-cc-env-lookup-value
     :source-var nelisp-cc-env-lookup-value--source
     :output "nelisp_env_lookup_value.o"
     :requires-arch x86_64)
    (nelisp-cc-env-set-value
     :source-var nelisp-cc-env-set-value--source
     :output "nelisp_env_set_value.o"
     :requires-arch x86_64)
    (nelisp-cc-env-lookup-function
     :source-var nelisp-cc-env-lookup-function--source
     :output "nelisp_env_lookup_function.o"
     :requires-arch x86_64)
    ;; Wave b — Env::bind_local Phase 47 .o.
    ;; depth check → frame path (cell-make + nelisp_frame_bind)
    ;;           or mirror path (nelisp_mirror_set_value_or_insert).
    (nelisp-cc-env-bind-local
     :source-var nelisp-cc-env-bind-local--source
     :output "nelisp_env_bind_local.o"
     :requires-arch x86_64)
    ;; Wave h — install empty globals mirror + frame stack Phase 47 .o.
    ;; Allocates nelisp-env record (fast-hash-table with 1024 buckets)
    ;; and nelisp-lexframe-stack record (8-element backing vector).
    (nelisp-cc-env-install-empty
     :source-var nelisp-cc-env-install-empty--source
     :output "nelisp_env_install_empty_globals_frames.o"
     :requires-arch x86_64)
    ;; Wave k — tty raw-mode / winsize / jobctrl Phase 47 .o.
    ;; Moves tcgetattr/cfmakeraw/tcsetattr/poll/read/ioctl syscall bodies from
    ;; build-tool/src/eval/builtins.rs tty_raw / tty_winsize / tty_jobctrl
    ;; inline modules (~160 LOC) into elisp .o.  Rust shim keeps signal
    ;; handlers, Once-gated installers, and static storage.
    (nelisp-cc-bi-tty-raw
     :source-var nelisp-cc-bi-tty-raw--source
     :output "nelisp_tty_raw.o"
     :requires-arch x86_64)
    ;; Wave A33.N — emit-value leaf-arm Phase 47 kernels (回避策 A).
    ;; `nelisp_emit_value_imm' + `nelisp_emit_value_ref_gp' Phase 47-
    ;; compile the two *leaf* hot arms of `--emit-value' (= the A33.3
    ;; integer-tag dispatch over A33.4 flat IR vectors) so the standalone
    ;; self-host emits the `imm' (= `mov rax, imm32') and GP-class `ref'
    ;; (= `mov rax, [rbp - 8*(slot+1)]') byte sequences natively.  Each
    ;; kernel reads the IR node's integer field at its A33.4 fixed offset
    ;; via `vector-ref-ptr' + `sexp-int-unwrap', writes the fixed-layout
    ;; opcode + imm/disp bytes into a caller-preallocated out-vec via
    ;; `vector-slot-set', and writes the byte count into the caller-owned
    ;; result slot.  Composes only existing Phase 47 grammar (= §111.C
    ;; vector ops + §100 sexp-int + §100.D logand/sar/arith + §125.A
    ;; alloc-bytes) — no new opcode, no Rust extern added.  Linux-x86_64
    ;; only (= same gate as the A26 / A30 parents).
    (nelisp-cc-bi-emit-value
     :source-var nelisp-cc-bi-emit-value--source
     :output "nelisp_emit_value.o"
     :requires-arch x86_64)
    ;; Doc 133 cutover — nl_tty_read_byte: re-provides the no-arg
    ;; C-ABI function deleted by fa8932eb.  Allocates a 1-byte heap
    ;; buffer (alloc-bytes 1 1), calls read(0, buf, 1) via extern-call,
    ;; reads the byte with ptr-read-u8, frees the buffer on both
    ;; success and error paths, returns byte (0..255) or -1.
    ;; Three-entry seq: nl_tty_rb_prog1 / nl_tty_rb_dispatch /
    ;; nl_tty_read_byte.  Linux-x86_64 only.
    (nelisp-cc-tty-read-byte
     :source-var nelisp-cc-tty-read-byte--source
     :output "nl_tty_read_byte.o"
     :requires-arch x86_64)
    ;; Doc 134 Stage 134.A — nl_eval_is_truthy: re-provides the
    ;; fa8932eb-deleted Rust body.  Evaluates FORM via
    ;; nelisp_eval_call (extern-call), checks the Sexp::Nil tag
    ;; (tag=0 → falsy), frees the 32-byte scratch slot on all paths.
    ;; Five-entry seq: nl_eit_prog1 / nl_eit_tag_check /
    ;; nl_eit_rc_check / nl_eit_with_scratch / nl_eval_is_truthy.
    ;; Linux-x86_64 only.
    (nelisp-cc-eval-is-truthy
     :source-var nelisp-cc-eval-is-truthy--source
     :output "nl_eval_is_truthy.o"
     :requires-arch x86_64)
    ;; Phase 47 swap — `nl_sexp_clone_into(src, dst)' re-provides the
    ;; deleted Rust `core::ptr::write(dst, (*src).clone())' from sexp.rs.
    ;; 3-way tag dispatch: inline atoms (tags 0..3) → 4×u64 bit-copy;
    ;; Str(5)/Symbol(4) → deep String copy via nl_alloc_str/symbol;
    ;; boxed (tags 6..12) → per-type nelisp_nl*_clone rc-bump + bit-copy.
    ;; Six-entry seq: nl_sci_prog2 / nl_sci_copy / nl_sci_bump /
    ;; nl_sci_rc / nl_sci_dispatch / nl_sexp_clone_into.
    ;; Linux-x86_64 only.
    (nelisp-cc-sexp-clone-into
     :source-var nelisp-cc-sexp-clone-into--source
     :output "nl_sexp_clone_into.o"
     :requires-arch x86_64)
    ;; Doc 135 Stage 135.C — eval-port env-leaf wiring (7 entries).
    ;; Lowered from packages/nelisp-sys/eval-port/*.nl via nelisp-sys-backend.
    ;; Resolves the env-leaf + non-env undefined link symbols introduced when
    ;; commit fa8932eb deleted the Rust bodies from build-tool/src/eval/mod.rs
    ;; and nlchartable.rs / nlstr.rs.
    ;;
    ;; env-leaves-simple: nl_env_lookup_val / nl_env_set_value(6-arg) /
    ;;   nl_env_pop_frame — 3 simple EvalCtx accessors.
    (nelisp-cc-evalport-env-leaves-simple
     :source-var nelisp-cc-evalport-env-leaves-simple--source
     :output "evalport-env-leaves-simple.o"
     :requires-arch x86_64)
    ;; env-leaves-bind: nl_env_set_value(3-arg FIXED) / nl_bf_bind_sym /
    ;;   nl_bf_bind_optional / nl_bf_bind_rest / nl_bf_err_arity /
    ;;   nl_bf_err_type / nl_bf_err_dangling_rest — bind+stash helpers.
    (nelisp-cc-evalport-env-leaves-bind
     :source-var nelisp-cc-evalport-env-leaves-bind--source
     :output "evalport-env-leaves-bind.o"
     :requires-arch x86_64)
    ;; env-leaves-frame: nl_push_and_bind / nl_env_push_captured
    ;;   — frame-compose helpers.
    (nelisp-cc-evalport-env-leaves-frame
     :source-var nelisp-cc-evalport-env-leaves-frame--source
     :output "evalport-env-leaves-frame.o"
     :requires-arch x86_64)
    ;; env-leaves-logic: nl_let_setup / nl_cc_match_and_bind
    ;;   — let + condition-case env helpers.
    (nelisp-cc-evalport-env-leaves-logic
     :source-var nelisp-cc-evalport-env-leaves-logic--source
     :output "evalport-env-leaves-logic.o"
     :requires-arch x86_64)
    ;; nonenv-char-table: nl_char_table_get_raw / nl_char_table_set_raw
    ;;   — CharTable Vec search + push helpers.
    (nelisp-cc-evalport-nonenv-char-table
     :source-var nelisp-cc-evalport-nonenv-char-table--source
     :output "evalport-nonenv-char-table.o"
     :requires-arch x86_64)
    ;; nonenv-mut-str-push: nl_mut_str_push_byte / nl_mut_str_push_codepoint
    ;;   — MutStr append + UTF-8 encode helpers.
    (nelisp-cc-evalport-nonenv-mut-str-push
     :source-var nelisp-cc-evalport-nonenv-mut-str-push--source
     :output "evalport-nonenv-mut-str-push.o"
     :requires-arch x86_64)
    ;; nonenv-mut-str-set-cp: nl_mut_str_set_codepoint_raw
    ;;   — MutStr in-place codepoint replacement.
    (nelisp-cc-evalport-nonenv-mut-str-set-cp
     :source-var nelisp-cc-evalport-nonenv-mut-str-set-cp--source
     :output "evalport-nonenv-mut-str-set-cp.o"
     :requires-arch x86_64)
    ;; Doc 136 — nl_str_to_float: decimal float string parser.
    ;;   Resolves nl_str_to_float undefined from fa8932eb deletion.
    (nelisp-cc-evalport-str-to-float
     :source-var nelisp-cc-evalport-str-to-float--source
     :output "evalport-str-to-float.o"
     :requires-arch x86_64)
    ;; Doc 135 Stage 135.E — nl_tty_memcpy_to_saved: termios save-buffer memcpy.
    ;;   Resolves nl_tty_memcpy_to_saved undefined from fa8932eb deletion.
    (nelisp-cc-evalport-tty-memcpy
     :source-var nelisp-cc-evalport-tty-memcpy--source
     :output "evalport-tty-memcpy.o"
     :requires-arch x86_64)
    ;; Doc 135 Stage 135.E — nl_env_capture_lexical: closure lexical-env snapshot.
    ;;   Resolves nl_env_capture_lexical undefined from fa8932eb deletion
    ;;   (delegates to nl_capture_descend_native, T in nelisp_frame_stack_find.o).
    (nelisp-cc-evalport-capture-lexical
     :source-var nelisp-cc-evalport-capture-lexical--source
     :output "evalport-capture-lexical.o"
     :requires-arch x86_64)
    ;; Doc 135 Stage 135.E (M2 swap) — the elisp COMBINER (re-wired aot-free).
    ;;   combiner-arglist: nl_eval_arg_list (+ walk).
    ;;   combiner-apply:   nl_apply_function + nl_apply_* helpers + special-form arms.
    ;;   combiner-entry:   nl_eval recursion-wrapper + nl_eval_ctx_make/in_ctx.
    ;;   bootstrap:        nl_install_builtins (60 builtins) + nl_install_one.
    ;; Externs nelisp_eval_call/nelisp_apply_function (Rust) KEPT; nl_eval_inner_cons
    ;; stays Rust (combiner-cons NOT wired here → no duplicate symbol).
    (nelisp-cc-evalport-combiner-arglist
     :source-var nelisp-cc-evalport-combiner-arglist--source
     :output "evalport-combiner-arglist.o"
     :requires-arch x86_64)
    (nelisp-cc-evalport-combiner-apply
     :source-var nelisp-cc-evalport-combiner-apply--source
     :output "evalport-combiner-apply.o"
     :requires-arch x86_64)
    (nelisp-cc-evalport-combiner-entry
     :source-var nelisp-cc-evalport-combiner-entry--source
     :output "evalport-combiner-entry.o"
     :requires-arch x86_64)
    (nelisp-cc-evalport-bootstrap
     :source-var nelisp-cc-evalport-bootstrap--source
     :output "evalport-bootstrap.o"
     :requires-arch x86_64)
    ;; Doc 135 Stage 135.E (M2) — native nelisp_aot_builtin_call1 provider.
    ;;   The combiner (nl_eval / nl_apply_do_fset) lowers a 1-arg env-independent
    ;;   builtin via Doc 129.6 AOT delegation; Doc 129 shipped only an in-Emacs
    ;;   bridge, never a native symbol.  This builds the (builtin NAME) sentinel
    ;;   + (ARG) list and forwards to the kept Rust nelisp_apply_function.
    (nelisp-cc-evalport-aot-builtin-call1
     :source-var nelisp-cc-evalport-aot-builtin-call1--source
     :output "evalport-aot-builtin-call1.o"
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
and picks `'mach-o' for macOS, `'coff' for Windows (= mingw / msvc
targets), `'elf' otherwise (= linux + the default).
  Doc 100 §100.D Stage 3 added Mach-O for macos-aarch64.
  Doc 101 §101.A adds COFF for windows-x86_64."
  (pcase (or (getenv "NELISP_PHASE47_TARGET_OS") "linux")
    ("macos"   'mach-o)
    ("windows" 'coff)
    (_         'elf)))

;; ---------------------------------------------------------------------------
;; Wave 9 incremental compile: skip entries whose .o is newer than its source.
;; ---------------------------------------------------------------------------

(defun compile-elisp-objects--source-file (feature)
  "Return absolute path to the .el source file backing FEATURE, or nil.
Uses `locate-file' on `load-path' (= the same lookup `require' performs)
to find `<feature>.el'.  Returns nil if FEATURE isn't a file-backed
feature (e.g. tangled from somewhere we can't trace)."
  (let* ((basename (concat (symbol-name feature) ".el"))
         (path (locate-file basename load-path)))
    (and path (expand-file-name path))))

(defvar compile-elisp-objects--compiler-deps nil
  "Cached mtime of compiler core files.  If any of these is newer than a
manifest entry's .o, the entry rebuilds even when its own .el is older.")

(defun compile-elisp-objects--compiler-mtime ()
  "Return the latest mtime among core compiler/layout files (memoized).
Returns nil if files can't be located.  Used as a coarse cache-invalidator:
when phase47-compiler.el / sexp-layout.el / asm-* change, EVERY .o needs
to be rebuilt because the emitted code may shift."
  (or compile-elisp-objects--compiler-deps
      (setq compile-elisp-objects--compiler-deps
            (let ((latest nil))
              (dolist (basename '("nelisp-phase47-compiler.el"
                                  "nelisp-sexp-layout.el"
                                  "nelisp-asm-x86_64.el"
                                  "nelisp-asm-arm64.el"
                                  "nelisp-elf-write.el"))
                (let ((path (locate-file basename load-path)))
                  (when path
                    (let ((mt (file-attribute-modification-time
                               (file-attributes path))))
                      (when (or (null latest) (time-less-p latest mt))
                        (setq latest mt))))))
              latest))))

(defun compile-elisp-objects--up-to-date-p (feature out-path)
  "Return non-nil if OUT-PATH exists and is newer than FEATURE source +
compiler core.  Returns nil → rebuild needed."
  (and (file-exists-p out-path)
       (let* ((out-mtime (file-attribute-modification-time
                          (file-attributes out-path)))
              (src-path (compile-elisp-objects--source-file feature))
              (src-mtime (and src-path
                              (file-attribute-modification-time
                               (file-attributes src-path))))
              (cc-mtime (compile-elisp-objects--compiler-mtime)))
         (and src-mtime
              (not (time-less-p out-mtime src-mtime))
              (or (null cc-mtime)
                  (not (time-less-p out-mtime cc-mtime)))))))

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
`build-tool/src/jit/float.rs' under an inverse cfg gate.

Wave 9 incremental: entries whose `.o' is newer than the source `.el'
AND the compiler core files are skipped (= up-to-date)."
  (let* ((out-dir (compile-elisp-objects--out-dir))
         (arch (compile-elisp-objects--target-arch))
         (format (compile-elisp-objects--target-format))
         (written nil)
         (skipped 0))
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
         ((compile-elisp-objects--up-to-date-p feature out-path)
          (setq skipped (1+ skipped))
          (push out-path written))
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
    (when (> skipped 0)
      (message "[compile-elisp-objects] incremental: %d up-to-date skipped" skipped))
    (nreverse written)))

(defun compile-elisp-objects-emit-range ()
  "Compile manifest entries in [NELISP_RANGE_START, NELISP_RANGE_END).
Reads bounds from env (0-based half-open).  Wave 9 R8 multi-process
parallelism: `build.rs' partitions the 208-entry manifest into N
chunks and spawns N nelisp processes, each calling this function
with its own START/END.  Same per-entry behaviour as
`compile-elisp-objects-emit-all' (= :requires-arch skip honored,
same out-dir, same :arch/:format)."
  (let* ((out-dir (compile-elisp-objects--out-dir))
         (arch (compile-elisp-objects--target-arch))
         (format (compile-elisp-objects--target-format))
         (total (length compile-elisp-objects-manifest))
         (start (string-to-number (or (getenv "NELISP_RANGE_START") "0")))
         (end (let ((e (getenv "NELISP_RANGE_END")))
                (if (and e (> (length e) 0)) (string-to-number e) total)))
         (written nil)
         (idx 0))
    (when (< start 0) (setq start 0))
    (when (> end total) (setq end total))
    (unless (file-directory-p out-dir)
      (make-directory out-dir t))
    (dolist (entry compile-elisp-objects-manifest)
      (when (and (>= idx start) (< idx end))
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
           ((compile-elisp-objects--up-to-date-p feature out-path)
            ;; Wave 9 incremental: .o newer than .el + compiler core → skip.
            (push out-path written))
           (t
            (require feature)
            (unless (boundp src-var)
              (error "compile-elisp-objects: %S has no :source-var %S"
                     feature src-var))
            (let ((sexp (symbol-value src-var)))
              (message "[compile-elisp-objects] [%d/%d) %s -> %s"
                       idx end feature out-path)
              (nelisp-phase47-compile-to-object sexp out-path
                                                :arch arch :format format)
              (push out-path written))))))
      (setq idx (1+ idx)))
    (nreverse written)))

(provide 'compile-elisp-objects)

;;; compile-elisp-objects.el ends here
