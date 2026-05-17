;;; nelisp-cc-frame-pop.el --- Doc 111 §111.E #22 frame_pop_rust_direct  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 111 §111.E Group E helper #22 — `frame_pop_rust_direct'.  Phase 47
;; reimplementation of the lexframe pop currently in Rust at
;; `build-tool/src/eval/env_lexframe.rs::Env::frame_pop_rust_direct'.
;;
;; The pop operation is two writes (= overwrite backing[depth-1] with
;; `Sexp::Nil' + decrement depth slot) but both need refcount-aware
;; semantics: the backing slot may hold a `nelisp-lexframe' record
;; whose drop releases its inner fast-hash-table (= 16 bucket cells +
;; the entry-count Int).  Phase 47 grammar has no vector-set or
;; record-slot-set-int form yet, so the body delegates to the
;; `nl_frame_pop' Rust shim.
;;
;; ABI deps satisfied:
;;   §100.A  `extern-call' — `nl_frame_pop'.

;;; Code:

(defconst nelisp-cc-frame-pop--source
  '(defun nelisp_frame_pop (frames-ptr)
     ;; frames-ptr: *const Sexp pointing at Env::frames_record.
     ;;
     ;; Returns: i64 — 1 on pop, 0 on no-op (= empty stack or unbuilt).
     ;;
     ;; Side effect: backing[depth-1] reset to `Sexp::Nil' (= the old
     ;; `nelisp-lexframe' record is dropped, refcounts adjusted),
     ;; depth slot decremented by 1.
     (extern-call nl_frame_pop frames-ptr))
  "Phase 47 source for Doc 111 §111.E #22 `frame_pop_rust_direct'.

Wraps the `nl_frame_pop' Rust shim.  Future swap (post-§111.E ship)
can rewrite the body in elisp using `nl_vector_set_slot' (§111.E) +
`nl_record_set_slot' (§111.B) for the depth-1 Sexp::Nil overwrite
+ depth decrement — currently deferred per Doc 111 §5.3.")

(provide 'nelisp-cc-frame-pop)

;;; nelisp-cc-frame-pop.el ends here
