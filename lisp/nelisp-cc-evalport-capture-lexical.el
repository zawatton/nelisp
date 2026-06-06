;;; nelisp-cc-evalport-capture-lexical.el --- AOT nl_env_capture_lexical  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 135 Stage 135.E -- nl_env_capture_lexical.
;; Lowered from packages/nelisp-sys/eval-port/nonenv-capture-lexical.nl via nelisp-sys-backend-lower-module (x86_64-unknown-linux-gnu).

;;; Code:

(defconst nelisp-cc-evalport-capture-lexical--source
  '(seq (defun nl_clx_write_nil (slot) (seq (ptr-write-u64 slot 0 0) (ptr-write-u64 (+ slot 8) 0 0) (ptr-write-u64 (+ slot 16) 0 0) (ptr-write-u64 (+ slot 24) 0 0) 0)) (defun nl_env_capture_lexical (env out) (let ((frames_sexp_ptr (+ env 32)) (alist_slot (alloc-bytes 32 8))) (seq (nl_clx_write_nil alist_slot) (nl_capture_descend_native frames_sexp_ptr alist_slot) (nl_sexp_clone_into out alist_slot) 0))))
  "Doc 135 AOT lowered source for nl_env_capture_lexical.")

(provide 'nelisp-cc-evalport-capture-lexical)

;;; nelisp-cc-evalport-capture-lexical.el ends here
