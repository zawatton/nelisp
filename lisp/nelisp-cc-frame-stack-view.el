;;; nelisp-cc-frame-stack-view.el --- Doc 111 §111.E #19 frame_stack_view  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 111 §111.E Group E helper #19 — `frame_stack_view'.  AOT
;; reimplementation of the layout walker that currently lives in Rust
;; at `build-tool/src/eval/env_lexframe.rs::Env::frame_stack_view'.
;;
;; The Rust impl returns a `(stack_rec, backing, depth)' triple; Phase
;; 47 cannot easily produce tuples, and the consumers (#20 / #21 / #22
;; / #23 / #24) all only need the `depth' i64 — the `stack_rec' is the
;; same `frames_record' pointer the caller already has, and the
;; `backing' vector is one `(record-slot-ref-ptr ... 0)' hop away.
;;
;; So this object exposes the depth as a single i64 — the one piece of
;; info that requires `sexp-int-unwrap'.  The wider "view" composition
;; happens inline at call sites via the §111.B `record-slot-ref-ptr'
;; grammar form (= zero-cost in registers, no intermediate slot copies).
;;
;; Layout (= `lisp/nelisp-lexframe.el'):
;;   frames_record = Sexp::Record(`nelisp-lexframe-stack')
;;     slots[0] = Sexp::Vector(BACKING, length = INITIAL_CAPACITY,
;;                             all entries Sexp::Nil or Frame Record)
;;     slots[1] = Sexp::Int(DEPTH)
;;
;; ABI deps satisfied:
;;   §111.B  `record-slot-ref-ptr' — slot 1 pointer.
;;   §100    `sexp-int-unwrap'     — i64 payload read at offset 8.

;;; Code:

(defconst nelisp-cc-frame-stack-view--source
  '(defun nelisp_frame_stack_depth (frames-ptr)
     ;; frames-ptr: *const Sexp pointing at Env::frames_record (=
     ;;             either Sexp::Record(`nelisp-lexframe-stack') or
     ;;             Sexp::Nil before stage0 bootstrap).
     ;;
     ;; Returns: i64 — the current depth slot (= slots[1] payload).
     ;;
     ;; Caller's pre-condition: frames-ptr.tag = Sexp::Record AND
     ;; slots[1].tag = Sexp::Int.  This helper does not gate on
     ;; `sexp-tag' because the only legal frames_record shape the
     ;; bootstrap installs already meets the precondition; callers
     ;; that may see `Sexp::Nil' route through Rust until the §111.E
     ;; cumulative ship rewires the dispatcher.
     (sexp-int-unwrap (record-slot-ref-ptr frames-ptr 1)))
  "AOT source for Doc 111 §111.E #19 `frame_stack_view' depth read.")

(provide 'nelisp-cc-frame-stack-view)

;;; nelisp-cc-frame-stack-view.el ends here
