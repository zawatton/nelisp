;;; nelisp-cc-jit-make-symbol.el --- Native symbol constructor trampoline -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton
;; This file is not part of GNU Emacs.
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; C ABI: (ARG: *const Sexp, OUT: *mut Sexp) -> status, 0=OK / 1=ERR.
;; Accept inline and mutable multibyte/unibyte strings. Symbol arguments are
;; not strings. Failure leaves OUT untouched; the calling Lisp bridge owns
;; condition construction. Names are copied unchanged into tag-16 symbols.
;;
;; Use the same identity issuer and allocator as the standalone reader.
;; The former Rust counter getter is no longer a runtime dependency, and
;; mixing the two producers cannot restart an independent identity sequence.
;; The helpers reside in nelisp-cc-nlstr-direct-ops--alloc-str-source, which
;; scripts/compile-elisp-objects.el emits as nl_alloc_str.o. Boxed string
;; lengths additionally use nl_mut_str_len.o; str-len reads inline slots.

;;; Code:

(defconst nelisp-cc-jit-make-symbol--source
  '(seq
    (defun nl_jit_make_symbol_name_len (arg)
      (if (or (= (sexp-tag arg) 6) (= (sexp-tag arg) 15))
          (extern-call nl_mut_str_len arg)
        (str-len arg)))
    (defun nl_jit_make_symbol_with_id (arg out identity _pad)
      (if (> identity 0)
          (if (= (extern-call nl_alloc_uninterned_symbol
                              (str-bytes-ptr arg)
                              (nl_jit_make_symbol_name_len arg) identity out) 0)
              1
            0)
        1))
    (defun nl_jit_make_symbol (arg out)
      (if (or (= (sexp-tag arg) 5) (= (sexp-tag arg) 6)
              (= (sexp-tag arg) 14) (= (sexp-tag arg) 15))
          (nl_jit_make_symbol_with_id
           arg out (extern-call nl_next_symbol_identity) 0)
        1)))
  "Native constructor sharing the reader's tag-16 identity namespace.")

(provide 'nelisp-cc-jit-make-symbol)
;;; nelisp-cc-jit-make-symbol.el ends here
