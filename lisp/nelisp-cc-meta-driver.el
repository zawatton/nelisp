;;; nelisp-cc-meta-driver.el --- Wave A25.2 AOT meta-driver kernel  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Wave A25.2 (AOT self-application — driver rewrite proof of
;; concept).  Provides the AOT-compilable kernel that the future
;; Wave A25.3 standalone bootstrap will call to drive
;; `scripts/compile-elisp-objects.el's per-entry up-to-date check.
;;
;; A25.0 audit (= a114d48af6c4 report) called out two structural
;; mismatches between the existing `compile-elisp-objects.el' driver
;; and AOT grammar:
;;
;;   1. The manifest is iterated via `dolist' + `plist-get' against a
;;      runtime alist of feature/symbol pairs.  AOT has no `dolist'
;;      / `plist-get' / hash-table access, and `(require feature)' +
;;      `(symbol-value src-var)' do not exist at all (= the standalone
;;      runtime resolves all `provide' calls at link time, not load
;;      time).
;;
;;   2. The per-entry up-to-date check chains `locate-file' +
;;      `file-attribute-modification-time' + `time-less-p' — all
;;      host-Emacs primitives with no AOT substrate.
;;
;; Wave A25.1-min shipped the foundation for (2): `nelisp_bi_getenv'
;; (read `NELISP_ELISP_OBJECTS_DIR') and `nelisp_bi_syscall_stat_mtime'
;; (read mtime as i64 seconds, returning -1 on stat error).  This file
;; adds the third foundational helper: `nelisp_meta_should_rebuild',
;; which composes two `nelisp_bi_syscall_stat_mtime' calls and returns
;; a 0/1 decision sufficient to drive the rebuild branch.
;;
;; Wave A29 — the stat-mtime helper's success-sentinel disambiguation
;; (= `(= rc 0)' vs the broken pre-A29 `(< rc 0)') makes the kernel
;; self-sufficient for missing-file detection: stat failure now
;; reliably surfaces as `Sexp::Int(-1)' in the mtime slot, and the
;; `nelisp_meta_decide_rebuild' arm `(if (= src-mtime -1) 1 ...)' /
;; `(if (= out-mtime -1) 1 ...)' fires as designed.  The elisp-side
;; pre-flight `compile-elisp-objects-meta--missing-file-mask-chunk'
;; the A26 walker carried as a work-around has been dropped.
;;
;; Static-link manifest plan (= the structural answer to mismatch #1).
;; In Wave A25.3 the manifest will be reified as a vector of Sexp::Str
;; pairs `(src-path-str . out-path-str)' bundled into the standalone
;; binary's read-only data section via the existing AOT rodata
;; emit path.  The driver then iterates `i = 0 .. N` via a tail-
;; recursive even-arity defun, reads `(vector-ref MANIFEST i)' to fetch
;; one entry, splits car/cdr via `cons-car' / `cons-cdr', invokes
;; `nelisp_meta_should_rebuild', and on rebuild=1 dispatches to a
;; per-entry compile kernel.  No `(require feature)' or
;; `(symbol-value src-var)' — every source defconst is already linked
;; into the same binary as a defconst-rooted Sexp tree.
;;
;; PoC scope (A25.2).  Only the `should_rebuild' kernel ships here.
;; The iteration loop + manifest vector + per-entry dispatch land in
;; A25.3 once the manifest reification design (vector vs. cons-list,
;; rodata emit shape, symbol-table layout for source defconst names)
;; is settled.  Today's PoC proves:
;;
;;   * The kernel composes only existing AOT grammar — no new
;;     opcode, no Rust extern added.
;;   * `nelisp-aot-compile-to-object' on the source defconst
;;     produces an ET_REL `.o' with the expected global STT_FUNC
;;     symbols (= `nelisp_meta_should_rebuild' + helpers).
;;   * Rust LOC delta = 0 (= no new shim, no manifest table generator;
;;     the existing `build-tool/build.rs' just gains one
;;     `manifest_sources' entry — pure data, not LOC).
;;
;; Substrate composition:
;;
;;   §125.A  `alloc-bytes' / `dealloc-bytes' — two 32-byte slots for
;;            the `_stat_mtime' result Sexps.
;;   §100.A  `extern-call'            — invoke the A25.1-min
;;            `nelisp_bi_syscall_stat_mtime' helper twice.
;;   §100    `sexp-int-unwrap'        — pull the i64 mtime payload
;;            out of each `Sexp::Int' result.
;;   §100    `sexp-int-make'          — write the 0/1 decision into
;;            the caller-owned result slot.
;;
;; Function contract:
;;   src-path:    *const Sexp — caller-validated `Sexp::Str' pointing
;;                at the source `.el' path (caller normalises).
;;   out-path:    *const Sexp — caller-validated `Sexp::Str' pointing
;;                at the destination `.o' path.
;;   result-slot: *mut Sexp  — caller-owned 32-byte slot.  Receives
;;                `Sexp::Int(1)' when the destination needs to be
;;                rebuilt (= out missing, out_mtime < src_mtime, or
;;                src missing — the last case bubbles to the caller
;;                which raises an error in the host-Emacs driver).
;;                `Sexp::Int(0)' otherwise (= up-to-date).
;;   returns:     result-slot pointer in rax.
;;
;; Decision table (matches the existing host-Emacs `--up-to-date-p'
;; semantics modulo the compiler-core-mtime check, which lives one
;; layer up and is computed once per batch):
;;
;;   src-mtime  out-mtime  rebuild?
;;   ---------- ---------- --------
;;        -1         *        1   (src missing — host driver will raise)
;;         *        -1        1   (out missing or stat error)
;;        S          O        1 if O < S else 0
;;
;; Linux-x86_64 only — same arch gate as the §122.I / A25.1-min
;; parents.  Composes only existing AOT grammar (no new opcode).

;;; Code:

(defconst nelisp-cc-meta-driver--source
  '(seq
    ;; Side-effect sequencer — 4-arg `(val _e1 _e2 _e3) -> val'.
    ;; Caches the decision while sequencing the two dealloc-bytes
    ;; cleanups of the temporary mtime-result slots and a final pad.
    ;; Even arity (4) — no Doc 124.F alignment workaround needed.
    (defun nelisp_meta_seq4 (val _e1 _e2 _e3) val)

    ;; Wave A25.6 — 2-arg discard-and-return sequencer used by the
    ;; public entry to swap the inner driver's slot-pointer return for
    ;; a TRAMPOLINE_OK (0) i64.  This makes the public symbol
    ;; `nelisp_meta_should_rebuild' directly addressable from the elisp
    ;; interpreter via the existing `nl-jit-call-out-2' bridge (which
    ;; expects the kernel to return 0 on success and reads the decision
    ;; from the caller's result slot).  Even arity (2) — no rsp pad
    ;; needed for the outer caller's dispatch into this entry.
    (defun nelisp_meta_zero2 (_x ret) ret)

    ;; 2-arg branch helper — given two i64 mtime values, decide whether
    ;; the destination is stale.  Pure arithmetic, no syscalls.  Returns
    ;; 1 (rebuild) or 0 (up-to-date).
    ;;
    ;;   src = -1                  → 1 (src missing; host driver raises)
    ;;   out = -1                  → 1 (out missing or stat error)
    ;;   out < src                 → 1 (destination is older)
    ;;   else                      → 0
    ;;
    ;; The three conditions collapse into a single `(or)' chain in
    ;; AOT grammar; `or' short-circuits to 1 on the first non-zero
    ;; arm, and the final `(if ... 0)' wraps the fallthrough.  Each
    ;; comparison op (`<' / `=') yields 0/1 in rax per the §97.c
    ;; convention, so the entire decision is one short-circuit chain.
    (defun nelisp_meta_decide_rebuild (src-mtime out-mtime)
      (if (= src-mtime -1)
          1
        (if (= out-mtime -1)
            1
          (if (< out-mtime src-mtime)
              1
            0))))

    ;; 4-arg inner driver — receives the caller-owned result slot, the
    ;; two source/output path pointers, and the two freshly-allocated
    ;; 32-byte scratch slots that will hold the `Sexp::Int(mtime)'
    ;; results from `nelisp_bi_syscall_stat_mtime'.  Sequences:
    ;;
    ;;   1. Call `nelisp_bi_syscall_stat_mtime(src-path, src-slot)'.
    ;;   2. Call `nelisp_bi_syscall_stat_mtime(out-path, out-slot)'.
    ;;   3. Unwrap both Sexp::Int payloads to i64.
    ;;   4. Dispatch through `nelisp_meta_decide_rebuild' to get 0/1.
    ;;   5. Write `Sexp::Int(decision)' into the caller's result slot.
    ;;   6. Free both scratch slots.
    ;;
    ;; The two stat calls are pure side-effects (results stored in
    ;; the scratch slots via the `nelisp_bi_syscall_stat_mtime'
    ;; contract).  The decision is computed last so the cleanup
    ;; dealloc-bytes effects can be sequenced behind it via the
    ;; `nelisp_meta_seq4' wrapper.
    ;;
    ;; Even arity (4 = src-slot + out-slot + src-path + out-path; the
    ;; result-slot is captured by the outer call via an extra layer
    ;; below — keeping each defun ≤ 6 args for SysV AMD64 GP-reg
    ;; passing).  Actually 5 args here — but AOT emits the rsp
    ;; alignment pad automatically for odd-arity defuns via the
    ;; `--needs-align' branch.
    (defun nelisp_meta_should_rebuild_inner
        (src-slot out-slot src-path out-path result-slot)
      (nelisp_meta_seq4
       (sexp-int-make
        result-slot
        (nelisp_meta_decide_rebuild
         (sexp-int-unwrap
          (extern-call nelisp_bi_syscall_stat_mtime src-path src-slot))
         (sexp-int-unwrap
          (extern-call nelisp_bi_syscall_stat_mtime out-path out-slot))))
       (dealloc-bytes src-slot 32 8)
       (dealloc-bytes out-slot 32 8)
       0))

    ;; Public 3-arg entry — allocates two 32-byte scratch slots for
    ;; the `Sexp::Int(mtime)' results, then dispatches to the inner
    ;; driver.  Odd arity (3) — AOT emits the rsp alignment pad
    ;; via the `--needs-align' branch around the outer caller's
    ;; dispatch into this entry.
    ;;
    ;; The 32-byte size matches `sizeof::<Sexp>()' (= 24-byte enum
    ;; discriminant + 8-byte payload + tail padding, see
    ;; `nelisp-sexp-layout.el`).  Align 8 matches the slot's
    ;; alignment requirement.
    ;; Wave A25.6 — public entry returns TRAMPOLINE_OK (0) instead of
    ;; the inner driver's slot-pointer.  The `nl-jit-call-out-2' bridge
    ;; in the standalone NeLisp interpreter casts the symbol's return
    ;; value to `i64' and treats anything other than 0 as
    ;; TRAMPOLINE_ERR; the actual rebuild decision is communicated via
    ;; the caller-owned RESULT-SLOT, which the bridge surfaces as the
    ;; `Sexp::Int(0|1)' return of `(nl-jit-call-out-2 "nelisp_meta
    ;; _should_rebuild" SRC-PATH OUT-PATH)'.  Wrapping the inner call
    ;; in `nelisp_meta_zero2 ... 0' threads the slot pointer through
    ;; (discarded) and emits `mov rax, 0' at function exit — no other
    ;; behavioural change.  Future AOT callers that want the slot
    ;; pointer back continue to dispatch into
    ;; `nelisp_meta_should_rebuild_inner' directly.
    (defun nelisp_meta_should_rebuild (src-path out-path result-slot)
      (nelisp_meta_zero2
       (nelisp_meta_should_rebuild_inner
        (alloc-bytes 32 8)
        (alloc-bytes 32 8)
        src-path
        out-path
        result-slot)
       0)))
  "AOT source for the Wave A25.2 meta-driver kernel
`(nelisp_meta_should_rebuild SRC-PATH OUT-PATH RESULT-SLOT)'.

Five-entry `(seq DEFUN ...)' manifest (Wave A25.6 + 1 sequencer):

- `nelisp_meta_seq4 (val _e1 _e2 _e3) -> val' — 4-arg side-effect
  sequencer (= result + 2 dealloc effects + pad).

- `nelisp_meta_zero2 (_x ret) -> ret' — 2-arg discard-and-return
  sequencer (Wave A25.6).  Wraps the inner driver's slot-pointer
  return so the public entry returns TRAMPOLINE_OK (0) for the
  elisp interpreter's `nl-jit-call-out-2' bridge.

- `nelisp_meta_decide_rebuild (src-mtime out-mtime) -> 0|1' —
  pure-arithmetic decision: src missing / out missing / out older →
  1; else 0.

- `nelisp_meta_should_rebuild_inner (src-slot out-slot src-path
  out-path result-slot) -> result-slot' — orchestrates two
  `extern-call nelisp_bi_syscall_stat_mtime' invocations + i64
  payload unwrap + decision + result write + scratch-slot cleanup.

- `nelisp_meta_should_rebuild (src-path out-path result-slot) -> 0'
  — public 3-arg entry; allocates the two 32-byte scratch slots,
  dispatches to the inner driver, then returns TRAMPOLINE_OK (0)
  so the elisp `nl-jit-call-out-2' bridge can surface the decision
  via the caller-owned RESULT-SLOT (= `Sexp::Int(0|1)').

Composes only existing AOT grammar — no new opcode:

- §125.A `alloc-bytes' / `dealloc-bytes' — scratch slot lifecycle.
- §100.A `extern-call' to the A25.1-min `nelisp_bi_syscall_stat_mtime'
  helper (= cross-`.o' AOT dispatch).
- §100 `sexp-int-unwrap' — pull i64 mtime out of `Sexp::Int' result.
- §100 `sexp-int-make' — write `Sexp::Int(decision)' into result slot.

Decision semantics match the existing host-Emacs
`compile-elisp-objects--up-to-date-p' modulo the compiler-core mtime
check (which lives one layer up in the iteration loop).  A future
Wave A25.3 iteration kernel will combine `nelisp_meta_should_rebuild'
with a static manifest vector (= read-only Sexp::Str pairs bundled
into the standalone binary's rodata section) to drive the full per-
entry walk without `(require feature)' or `(symbol-value src-var)'.

Linux-x86_64 only — same arch gate as the A25.1-min parents.")

(provide 'nelisp-cc-meta-driver)

;;; nelisp-cc-meta-driver.el ends here
