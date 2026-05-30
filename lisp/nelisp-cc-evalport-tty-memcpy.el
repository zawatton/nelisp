;;; nelisp-cc-evalport-tty-memcpy.el --- Phase 47 nl_tty_memcpy_to_saved  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 135 Stage 135.E -- nl_tty_memcpy_to_saved.
;; Lowered from packages/nelisp-sys/eval-port/nonenv-tty-memcpy.nl via nelisp-sys-backend-lower-module (x86_64-unknown-linux-gnu).

;;; Code:

(defconst nelisp-cc-evalport-tty-memcpy--source
  '(seq (defun nl_tmcs_copy8 (src dst off) (nelisp_ptr_write_u64 (+ dst off) (nelisp_ptr_read_u64 src off))) (defun nl_tmcs_copy1 (src dst off) (nelisp_ptr_write_u8 (+ dst off) 0 (nelisp_ptr_read_u8 src off))) (defun nl_tty_memcpy_to_saved (src) (let ((dst (nl_tty_saved_termios_ptr))) (seq (nl_tmcs_copy8 src dst 0) (nl_tmcs_copy8 src dst 8) (nl_tmcs_copy8 src dst 16) (nl_tmcs_copy8 src dst 24) (nl_tmcs_copy8 src dst 32) (nl_tmcs_copy8 src dst 40) (nl_tmcs_copy8 src dst 48) (nl_tmcs_copy1 src dst 56) (nl_tmcs_copy1 src dst 57) (nl_tmcs_copy1 src dst 58) (nl_tmcs_copy1 src dst 59)))))
  "Doc 135 Phase 47 lowered source for nl_tty_memcpy_to_saved.")

(provide 'nelisp-cc-evalport-tty-memcpy)

;;; nelisp-cc-evalport-tty-memcpy.el ends here
