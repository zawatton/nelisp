;;; nelisp-cc-frame-push.el --- Doc 111 §111.E #21 frame_push_rust_direct  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 111 §111.E Group E helper #21 — `frame_push_rust_direct'.  Phase 47
;; reimplementation of the lexframe push currently in Rust at
;; `build-tool/src/eval/env_lexframe.rs::Env::frame_push_rust_direct'.
;;
;; The push operation combines three sub-steps that the design doc lists
;; under three different ABI groups: (a) §111.E `nl_alloc_record' for
;; the fresh `nelisp-lexframe' record, (b) §111.C `vector-ref' / -set
;; for the backing[depth] install, (c) `frame_stack_ensure_capacity'
;; (= helper #20) to grow if needed.  Each sub-step needs different
;; refcount discipline, so the Phase 47 first ship delegates the whole
;; bundle to the `nl_frame_push' Rust shim — same trade-off as Doc 111
;; §5.3 (= "first ship can use the Rust helper", deferred elisp
;; rewrite of the inner sequence to a follow-up).
;;
;; The elisp shape is intentionally minimal: it exists to register the
;; `nl_frame_push' extern symbol in the Phase 47 dispatcher and to
;; prove the round-trip end-to-end via the probe in
;; `tests/elisp_cc_frame_push_probe.rs'.
;;
;; ABI deps satisfied:
;;   §100.A  `extern-call' — `nl_frame_push'.

;;; Code:

(defconst nelisp-cc-frame-push--source
  '(defun nelisp_frame_push (frames-ptr)
     ;; frames-ptr: *const Sexp pointing at Env::frames_record.
     ;;
     ;; Returns: i64 — 1 on push, 0 when the mirror is unbuilt.
     ;;
     ;; Side effect: backing[old-depth] receives a fresh
     ;; `nelisp-lexframe' record (with one `fast-hash-table' slot
     ;; containing 16 empty buckets); depth slot incremented by 1.
     (extern-call nl_frame_push frames-ptr))
  "Phase 47 source for Doc 111 §111.E #21 `frame_push_rust_direct'.

Wraps the `nl_frame_push' Rust shim which composes `nl_alloc_record'
(§111.E) + `nl_alloc_vector' (§111.E, for the fast-hash-table
buckets) + `nl_record_set_slot' (§111.B) on the frames-record's
depth slot.  The full elisp rewrite of the push sequence is deferred
per Doc 111 §5.3.")

(provide 'nelisp-cc-frame-push)

;;; nelisp-cc-frame-push.el ends here
