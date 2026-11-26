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
;;   2. If hit: compare slot 1 with UNBOUND-PTR.  A symbol created by a
;;      variable-only definition has a mirror entry whose function slot is
;;      this sentinel; treat it as a miss instead of exposing it as a
;;      callable function.
;;
;;   3. Otherwise fill out-ptr with the function Sexp (refcount-aware copy
;;      via record-slot-ref, which delegates to `nl_sexp_clone_into' since
;;      Doc 111 §111.C v3).  Return 0 (= found).
;;
;; Signature:
;;   (nelisp_env_lookup_function MIRROR-PTR UNBOUND-PTR NAME-PTR OUT-PTR)
;;     MIRROR-PTR  : *const Sexp — Env::globals_record.
;;     UNBOUND-PTR : *const Sexp — Env::unbound_marker.
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

(defun nelisp-cc-env-lookup-function--cell-form ()
  "Emit the live function-cell read shared by both lookup ABIs.
The result is the raw function WORD, or 2 for the internal void marker."
  (let ((general
         '(let* ((entry (nelisp_mirror_lookup_entry_owned mirror-ptr name-ptr)))
            (if (= entry 0) 2
              (let* ((w (nl_record_slot_word entry 1)))
                (if (if (= (logand w 1) 1) 0
                      (if (= (nl_val_tag w) 16)
                          (= (heap-sexp-payload w) (heap-sexp-payload unbound-ptr))
                        0))
                    2 w))))))
    (if (member (getenv "NELISP_SYMCELL_DISABLE") '("function" "all"))
        general
      `(if (if (= (logand name-ptr 7) 6)
               (let* ((kind (logand (ptr-read-u8 (- name-ptr 6) 0) 63)))
                 (if (= kind 4) 1 (= kind 16)))
             0)
           ;; The owned symbol itself identifies the cell, including a
           ;; private-obarray symbol and one retained after uninterning.
           ;; Qunbound is a legacy tag-16 view with a small identity.
           ;; Direct symbol words carry their object address as identity.
           (let* ((w (ptr-read-u64 name-ptr 18)))
             (if (if (= (logand w 7) 0)
                     (if (> w 4095)
                         (if (= (ptr-read-u8 w 0) 16)
                             (= (ptr-read-u64 w 8) (ptr-read-u64 unbound-ptr 8))
                           0)
                       0)
                   0)
                 2 w))
         ,general))))

(defun nelisp-cc-env-lookup-function--legacy-form (word)
  "Emit the public lookup representation for raw function WORD."
  `(if (= (logand ,word 7) 6)
       (if (= (logand (ptr-read-u8 (- ,word 6) 0) 63) 20)
           (ptr-read-u64 (- ,word 6) 8) ,word)
     ,word))

(defconst nelisp-cc-env-lookup-function--source
  `(seq
    (defun nelisp_env_lookup_function_w (mirror-ptr unbound-ptr name-ptr _pad)
      (let* ((w ,(nelisp-cc-env-lookup-function--cell-form)))
        ;; Legacy callers consume the builtin list carried by a Subr.
        ,(nelisp-cc-env-lookup-function--legacy-form 'w)))
    (defun nelisp_env_lookup_function (mirror-ptr unbound-ptr name-ptr out-ptr)
      (let* ((w ,(nelisp-cc-env-lookup-function--cell-form)))
        (if (= w 2) 1
          (nl_abi_word_to_rc ,(nelisp-cc-env-lookup-function--legacy-form 'w)
                             out-ptr)))))
  "AOT source for the environment function-cell lookup.")

(provide 'nelisp-cc-env-lookup-function)

;;; nelisp-cc-env-lookup-function.el ends here
