;;; nelisp-cc-frame-bind.el --- Doc 111 §111.E #23 frame_bind_rust_direct  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 111 §111.E Group E helper #23 — `frame_bind_rust_direct' with
;; the §111.E #25 private helper `frame_bind_into' folded in.  Phase 47
;; reimplementation of the innermost-frame name → cell bind currently
;; in Rust at `build-tool/src/eval/env_lexframe.rs::Env::frame_bind_rust_direct'.
;;
;; The bind operation is a hash-bucket walk + update-in-place-or-prepend.
;; The hash is computed via `mirror_fnv1a'; the bucket walk needs a
;; cons-pair-cdr update via `NlConsBoxRef::set_cdr' (= reuses the
;; §101.D `nl_consbox_set_cdr' extern); the prepend allocates two
;; `NlConsBox' boxes via `nl_alloc_consbox' (= §101.D).  That sequence
;; is encoded in the `nl_frame_bind' Rust shim per the Doc 111 §5.3
;; "first ship can use the Rust helper" pattern; a later elisp
;; rewrite can compose §101.B/D ops with the §111.B `record-slot-ref-ptr'
;; + §111.C `vector-ref-ptr' walk.
;;
;; ABI deps satisfied:
;;   §100.A  `extern-call' — `nl_frame_bind' (composes §101.B/D + §111.B/C).

;;; Code:

(defconst nelisp-cc-frame-bind--source
  '(defun nelisp_frame_bind (frames-ptr name-ptr cell-ptr)
     ;; frames-ptr: *const Sexp pointing at Env::frames_record.
     ;; name-ptr:   *const Sexp pointing at Sexp::Str / Sexp::Symbol.
     ;; cell-ptr:   *const Sexp pointing at the new cell value
     ;;             (commonly Sexp::Cell(NlCellRef)).
     ;;
     ;; Returns: i64 — 1 on bind, 0 on no-op (= empty stack / unbuilt
     ;; mirror / malformed name).
     ;;
     ;; Side effect: innermost frame's hash table either gains a new
     ;; (NAME . CELL) pair or has the existing pair's cdr replaced
     ;; with CELL.
     (extern-call nl_frame_bind frames-ptr name-ptr cell-ptr))
  "Phase 47 source for Doc 111 §111.E #23 `frame_bind_rust_direct' +
folded private helper `frame_bind_into' (#25).

Wraps the `nl_frame_bind' Rust shim which composes mirror_fnv1a
(§111.E Group C) + cons-bucket walk (§101.B) + `NlConsBox::set_cdr'
(§101.D) for in-place update + `nl_alloc_consbox' (§101.D) for the
prepend path.  Future elisp swap is feasible once §101.D's
`cons-make' + `cons-set-cdr' ops + a `vector-set' grammar form
gel.")

(provide 'nelisp-cc-frame-bind)

;;; nelisp-cc-frame-bind.el ends here
