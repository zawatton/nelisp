;;; nelisp-cc-frame-ensure-capacity.el --- Doc 111 §111.E #20 ensure_capacity  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 111 §111.E Group E helper #20 — `frame_stack_ensure_capacity'.
;; Phase 47 reimplementation of the lexframe-stack capacity-doubling
;; grow that currently lives in Rust at
;; `build-tool/src/eval/env_lexframe.rs::Env::frame_stack_ensure_capacity'.
;;
;; Strategy: the elisp body checks `vector-len(backing) < needed' via
;; the §111.C `vector-len' grammar form, and on need-grow delegates to
;; the `nl_frame_stack_ensure_capacity' Rust shim (=
;; `build-tool/src/eval/env_lexframe_phase47_shims.rs').  The shim
;; allocates a fresh `NlVector' via `nl_alloc_vector' (Doc 111 §111.E),
;; copies live elements over, and installs it in `frames_record.slots[0]'
;; via `nl_record_set_slot' — matching the Rust impl bit-for-bit.
;;
;; The tight grow loop stays in Rust per Doc 111 §5.3's "first ship can
;; use the Rust helper" trade-off; a future swap can rewrite the copy
;; loop in elisp using §111.C `vector-ref-ptr' + a `nl_sexp_clone_into'
;; recursion (~50 LOC of arithmetic).
;;
;; ABI deps satisfied:
;;   §111.B  `record-slot-ref-ptr' — slot 0 pointer (= backing vector).
;;   §111.C  `vector-len'          — current backing capacity.
;;   §100.A  `extern-call'         — `nl_frame_stack_ensure_capacity'.

;;; Code:

(defconst nelisp-cc-frame-ensure-capacity--source
  '(defun nelisp_frame_stack_ensure_capacity (frames-ptr needed)
     ;; frames-ptr: *const Sexp pointing at Env::frames_record.
     ;; needed:     i64 — required minimum capacity.
     ;;
     ;; Returns: i64 — 1 when a grow happened (or no grow needed), 0
     ;; reserved for future "mirror unbuilt" gate (= currently the
     ;; Rust shim returns null which we coerce to 0 via the `if').
     ;;
     ;; The two-step body matches the Rust impl's early-return shape:
     ;;   1. If current cap >= needed, no-op (= return 1).
     ;;   2. Else call into the Rust grow path which allocates +
     ;;      copies + installs in slot 0.
     (if (< (vector-len (record-slot-ref-ptr frames-ptr 0)) needed)
         (extern-call nl_frame_stack_ensure_capacity frames-ptr needed)
       1))
  "Phase 47 source for Doc 111 §111.E #20 `frame_stack_ensure_capacity'.

Composes record-slot-ref-ptr (§111.B) + vector-len (§111.C) + extern-call
into `nl_frame_stack_ensure_capacity' (§100.A) to gate the grow path
on the cheap capacity comparison.  The Rust shim handles the grow
itself (= `nl_alloc_vector' + Sexp::clone over each live slot +
`nl_record_set_slot' on the frames-record's slot 0).")

(provide 'nelisp-cc-frame-ensure-capacity)

;;; nelisp-cc-frame-ensure-capacity.el ends here
