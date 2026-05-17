;;; nelisp-cc-frame-stack-find.el --- Doc 111 §111.E #24 frame_stack_find  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 111 §111.E Group E helper #24 — `frame_stack_find_rust_direct'
;; with the §111.E #25 private helpers `frame_lookup_rust_direct' +
;; `frame_lookup_in' folded in.  Phase 47 reimplementation of the
;; innermost-first stack walk currently in Rust at
;; `build-tool/src/eval/env_lexframe.rs::Env::frame_stack_find_rust_direct'.
;;
;; The walk visits each frame from depth-1 down to 0 and inside each
;; frame hashes NAME via `mirror_fnv1a' + walks the matching bucket
;; alist.  The Phase 47 first ship dispatches the entire walk to the
;; `nl_frame_stack_find' Rust shim which folds in `frame_lookup_in'
;; (= bucket hash + cons-walk + symbol-eq).  Future elisp rewrite is
;; tractable using `mirror_lookup_entry' (= §111.E #1) as a template
;; — recursive walker with `record-slot-ref-ptr' + `vector-ref-ptr'
;; + cons-walk primitives.
;;
;; ABI deps satisfied:
;;   §100.A  `extern-call' — `nl_frame_stack_find' (composes §111.B/C + §101.B/C).

;;; Code:

(defconst nelisp-cc-frame-stack-find--source
  '(defun nelisp_frame_stack_find (frames-ptr name-ptr)
     ;; frames-ptr: *const Sexp pointing at Env::frames_record.
     ;; name-ptr:   *const Sexp pointing at Sexp::Str / Sexp::Symbol.
     ;;
     ;; Returns: i64 — the `*const Sexp' of the matching (NAME . CELL)
     ;; pair's CDR slot (= the cell), or 0 on miss.  The returned
     ;; pointer borrows the bucket-pair's slot owned by `*frames-ptr';
     ;; callers must not outlive that ownership (= same contract as
     ;; `nelisp_mirror_lookup_entry').
     (extern-call nl_frame_stack_find frames-ptr name-ptr))
  "Phase 47 source for Doc 111 §111.E #24 `frame_stack_find_rust_direct' +
folded private helpers `frame_lookup_rust_direct' / `frame_lookup_in'
(#25).

Wraps the `nl_frame_stack_find' Rust shim which composes mirror_fnv1a
(§111.E Group C) + per-frame hash walk + symbol-eq (§101.C).  The
return value semantically mirrors `mirror_lookup_entry' (= §111.E #1):
non-zero pointer to a borrowed Sexp slot, or 0 on miss.")

(provide 'nelisp-cc-frame-stack-find)

;;; nelisp-cc-frame-stack-find.el ends here
