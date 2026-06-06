;;; nelisp-cc-frame-stack-install.el --- Wave i frame_stack_install_sexp → AOT .o  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Wave i — pure-elisp port of `nelisp_frame_stack_install_sexp'
;; (12 LOC `#[no_mangle] pub unsafe extern "C"' body in
;; `build-tool/src/eval/env_helpers.rs').
;;
;; The Rust body is DELETED; the `#[no_mangle]' symbol becomes a 3-line
;; thin wrapper in `env_helpers.rs' that allocates a scratch `Sexp::Nil'
;; and delegates to this AOT-compiled object.
;;
;; Algorithm implemented by `nelisp_frame_stack_install':
;;
;;   1. Ensure backing capacity >= depth+1 by calling
;;      `nelisp_frame_stack_ensure_capacity' (existing AOT .o).
;;      This is a no-op fast-path when depth < current capacity.
;;   2. Install FRAME-PTR into `backing[depth]' via `vector-slot-set'
;;      (refcount-aware clone into the NlVector slot).
;;   3. Materialise `Sexp::Int(depth+1)' into scratch-slot via
;;      `sexp-int-make'.
;;   4. Install the new depth into `frames-ptr.slot[1]' via
;;      `record-slot-set' (refcount-safe; returns 1 = sentinel).
;;
;; Scratch-slot layout:
;;   scratch-slot — caller-owned `*mut Sexp' initialised to `Sexp::Nil'.
;;     Reused twice: first as the scratch for `ensure_capacity' (which
;;     may write a `Sexp::Vector' into it if a grow happened), then
;;     overwritten with `Sexp::Int(depth+1)' for the depth bump.
;;     The raw overwrite of the Vector in step 3 causes a refcount LEAK
;;     if a grow occurred (new backing rc stays 2), but NOT a crash —
;;     same intentional pattern as Wave h scratch[4] reuse.
;;     When the Rust thin wrapper returns, it drops the scratch
;;     `Sexp::Nil' (= no-op), so no double-free.
;;
;; Arity: 3 (frames-ptr / frame-ptr / scratch-slot) — odd arity.
;; `extern-call' (steps 1) adds `sub rsp, 8' before each C call when
;; `--current-defun-arity' is odd, satisfying the SysV AMD64 16-byte
;; rsp alignment requirement at every call site.  `vector-slot-set'
;; (step 2) also goes through `extern-call' plumbing and inherits the
;; same alignment correction.
;;
;; Refcount discipline:
;;   After step 2, frame-ptr's referent has rc=N+1 (one extra from
;;   backing[depth]).  The caller (`push_captured') holds `frame' as a
;;   local Sexp::Record; that local is dropped when `push_captured'
;;   returns, leaving rc=N (= correct single owner in backing).
;;
;; ABI deps satisfied:
;;   §100.A  `extern-call'         — `nelisp_frame_stack_ensure_capacity'.
;;   §100    `sexp-int-make'       — i64 → Sexp::Int materialise.
;;   §100    `sexp-int-unwrap'     — i64 payload read for depth.
;;   §111.B  `record-slot-ref-ptr' — read frames-ptr.slot[1] (depth).
;;   §111.B  `record-slot-set'     — refcount-safe slot[1] write.
;;   §111.E  `vector-slot-set'     — refcount-safe backing[depth] write.

;;; Code:

(defconst nelisp-cc-frame-stack-install--source
  '(defun nelisp_frame_stack_install (frames-ptr frame-ptr scratch-slot)
     ;; frames-ptr:   *const Sexp — Env::frames_record (Sexp::Record).
     ;; frame-ptr:    *const Sexp — the lexframe to install (Sexp::Record).
     ;; scratch-slot: *mut Sexp   — caller-owned scratch (Sexp::Nil on entry).
     ;;
     ;; Returns: i64 — 1 (= record-slot-set sentinel).
     (and
      ;; Step 1: ensure backing capacity >= depth+1.
      (extern-call nelisp_frame_stack_ensure_capacity
                   frames-ptr
                   (+ (sexp-int-unwrap
                       (record-slot-ref-ptr frames-ptr 1))
                      1)
                   scratch-slot)
      ;; Step 2: install frame into backing[depth].
      (vector-slot-set
       (record-slot-ref-ptr frames-ptr 0)       ; backing NlVector ptr
       (sexp-int-unwrap
        (record-slot-ref-ptr frames-ptr 1))     ; depth (i64 index)
       frame-ptr)                               ; frame to clone in
      ;; Step 3: materialise Sexp::Int(depth+1) into scratch-slot.
      ;; NOTE: raw-overwrite of scratch-slot (may hold NlVector if grow
      ;; occurred); that NlVector's rc stays >=2 from backing slot[0].
      (sexp-int-make scratch-slot
                     (+ (sexp-int-unwrap
                         (record-slot-ref-ptr frames-ptr 1))
                        1))
      ;; Step 4: depth bump — frames.slot[1] = Sexp::Int(depth+1).
      (record-slot-set frames-ptr 1 scratch-slot)))
  "AOT source for Wave i `nelisp_frame_stack_install'.

Installs a lexframe into the frames-record backing vector and bumps
the depth counter.  Replaces the 12-LOC `#[no_mangle] pub unsafe
extern \"C\" fn nelisp_frame_stack_install_sexp' body deleted from
`build-tool/src/eval/env_helpers.rs'.

Net Rust delta: -12 LOC deleted + 3 LOC thin-wrapper = -9 LOC.

AOT ops consumed:
  `extern-call'        — §100.A nelisp_frame_stack_ensure_capacity.
  `sexp-int-unwrap'    — §100 depth i64 read (x3).
  `record-slot-ref-ptr' — §111.B slot 0 / slot 1 pointer reads (x3).
  `vector-slot-set'    — §111.E refcount-aware backing[depth] write.
  `sexp-int-make'      — §100 depth+1 materialise.
  `record-slot-set'    — §111.B refcount-safe slot[1] depth bump.")

(provide 'nelisp-cc-frame-stack-install)

;;; nelisp-cc-frame-stack-install.el ends here
