;;; nelisp-cc-env-lookup-function.el --- Wave a-2: Env::lookup_function AOT .o  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Wave a-2 — `Env::lookup_function' body migrated to AOT elisp .o.
;; Replaces the 7-LOC Rust body in
;; `build-tool/src/eval/env_helpers.rs::Env::lookup_function'.
;;
;; Algorithm (= literal transcription of the Rust body):
;;
;;   1. Check mirror entry existence via `nelisp_mirror_lookup_entry'.
;;      If miss (= 0), return 1 (= unbound-fn sentinel).
;;
;;   2. If hit: fill out-ptr via record-slot-ref (refcount-aware copy
;;      which delegates to `nl_sexp_clone_into' since Doc 111 §111.C v3),
;;      then test slot 1 for the unbound marker; return 1 (= miss) if
;;      the function slot is the unbound marker, else 0 (= found).
;;
;; Signature:
;;   (nelisp_env_lookup_function MIRROR-PTR UNBOUND-PTR NAME-PTR OUT-PTR)
;;     MIRROR-PTR  : *const Sexp — Env::globals_record.
;;     UNBOUND-PTR : *const Sexp — Env::unbound_marker (unused, for future
;;                                  use / arity padding to 4 = even).
;;     NAME-PTR    : *const Sexp — Sexp::Symbol name to look up.
;;     OUT-PTR     : *mut Sexp   — 32-byte caller-owned result slot.
;;   Returns: i64.  0 = found (function written to *out-ptr),
;;                  1 = unbound (out-ptr unchanged).
;;
;; ABI:
;;   4 args (even) — body-entry rsp ≡ 0 mod 16 ✓.
;;   Each defun has at most one extern-call in any execution path.
;;
;; ABI deps:
;;   nelisp_mirror_lookup_entry    — hit/miss check (0 = miss)
;;   nelisp_mirror_lookup_function — fills out-ptr (refcount-safe)

;;; Code:

(defconst nelisp-cc-env-lookup-function--source
  '(seq
    (defun nelisp_env_lookup_function_hop (mirror-ptr unbound-ptr name-ptr out-ptr depth pad)
      ;; One lookup step + bounded alias chase (arity 6, even).
      ;; DEPTH counts remaining hops; 0 = budget exhausted (cycle),
      ;; reported as miss.  PAD keeps the arity even.
      (let ((entry (extern-call nelisp_mirror_lookup_entry mirror-ptr name-ptr)))
        (if (= entry 0)
            1
          ;; Inspect the LIVE in-arena function-cell view first; the
          ;; mirror's valid-key guard (`nl_gc_in_arena') rejects any
          ;; out-of-arena pointer, so the caller's out slot (typically a
          ;; root-stack slot) must never be used as a chase key.  The
          ;; clone into out-ptr happens exactly once, on final success,
          ;; which also avoids per-hop refcount leaks.
          (let ((cell (record-slot-ref-ptr entry 1)))
            (if (= (symbol-name-eq cell "\001elisp--unbound-marker") 1)
                1
              (if (= (sexp-tag cell) 4)
                  (if (= depth 0)
                      1
                    (nelisp_env_lookup_function_hop
                     mirror-ptr unbound-ptr cell out-ptr (- depth 1) pad))
                (seq
                 (record-slot-ref entry 1 out-ptr)
                 0)))))))
    ;; PERMANENT INVARIANT CHECK, not scaffolding.
    ;;
    ;; Every Symbol Sexp is created with cap == len -- `nl_alloc_symbol_write'
    ;; writes cap<-alloc-n / len<-n and `nl_intern_write_sexp' writes cap<-n /
    ;; len<-n, and both are the only producers.  So cap /= len means the length
    ;; word was overwritten after creation, which is a memory-corruption defect
    ;; that has been open since 2026-08-05.
    ;;
    ;; Kept on permanently because it is two loads and a compare, on the miss
    ;; path only, and because as a temporary probe it answered in ONE run
    ;; (2026-08-12: cap=40, len=43) a question that a week of sampling had not.
    ;; A cheap invariant that fires where the corruption happens is worth more
    ;; than any amount of post-hoc profiling.
    ;;
    ;; Original note (2026-08-12, handoff §4.2 / the codex task's section 4).
    ;;
    ;; `nl_intern_write_sexp' writes cap == len when a Symbol Sexp is created,
    ;; so cap and len disagreeing proves the length word was overwritten AFTER
    ;; creation -- which decides the open question: producer side, or a later
    ;; 32-byte box reuse clobbering offset 24.  `nl_alloc_symbol''s NUL scan
    ;; already rules out creation with a too-long length (it is linked and has
    ;; never fired), so this is the other half of the same test.
    ;;
    ;; Reported on a lookup MISS only, and only when cap /= len, so the common
    ;; miss (every failing `fboundp') stays silent.  The name is printed at the
    ;; CAP length, i.e. what the name should have been; the error message that
    ;; follows prints it at the corrupted length.  The two together give both
    ;; numbers without a formatter.
    ;;
    ;; PRINT-ONLY: the return value is the hop helper's, unchanged.
    (defun nl_symcap_report (name-ptr pad)
      (let* ((msg (alloc-bytes 24 1)) (nl (alloc-bytes 1 1)))
        (seq
         (ptr-write-u64 msg 0 2322292198855173486)
         (ptr-write-u64 (+ msg 8) 0 5633246950142597459)
         (ptr-write-u64 (+ msg 16) 0 35520542158149)
         (ptr-write-u8 nl 0 10)
         (bf_report_eval_stack)
         (nl_os_write_stderr msg 22)
         (nl_os_write_stderr (ptr-read-u64 name-ptr 16) (ptr-read-u64 name-ptr 8))
         (nl_os_write_stderr nl 1)
         0)))
    (defun nl_symcap_check (name-ptr pad)
      (if (= (sexp-tag name-ptr) 4)
          (if (= (ptr-read-u64 name-ptr 8) (ptr-read-u64 name-ptr 24))
              0
            (nl_symcap_report name-ptr pad))
        0))
    (defun nelisp_env_lookup_function (mirror-ptr unbound-ptr name-ptr out-ptr)
      ;; ABI entry (arity 4, unchanged for existing callers): delegate
      ;; to the chase helper with an 8-hop budget.
      (let ((rc (nelisp_env_lookup_function_hop
                 mirror-ptr unbound-ptr name-ptr out-ptr 8 0)))
        (seq (if (= rc 1) (nl_symcap_check name-ptr 0) 0) rc))))
  "AOT source for `Env::lookup_function' with bounded alias chase.

The pre-chase unit returned the raw function-cell contents, so a cell
holding a SYMBOL (an Emacs-style function alias installed by
`fset'/`defalias') was handed to callers as the callable itself, and
`nl_apply_do_fset' compensated by eagerly resolving symbol definitions
at fset time -- which broke forward aliases (`(defalias 'a 'later)')
with a premature void-function.  The chase helper resolves alias
chains at LOOKUP time instead, up to 8 hops; exhaustion (cycles)
reports miss (1), which call sites surface as void-function.

Known divergences from Emacs, accepted for now: `fboundp' of an alias
whose target is still undefined answers nil (Emacs: t), and
`symbol-function' reports the chased final definition rather than the
alias symbol.

Contract change: on a miss discovered mid-chain, out-ptr may hold an
intermediate alias symbol rather than being untouched; callers must
not read out-ptr when the return is 1 (they already don't).")

(provide 'nelisp-cc-env-lookup-function)

;;; nelisp-cc-env-lookup-function.el ends here
