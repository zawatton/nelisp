;;; nelisp-cc-evalport-tty-memcpy.el --- lowered -*- lexical-binding: t; -*-
;;; Code:
(defconst nelisp-cc-evalport-tty-memcpy--source
  (quote (seq (defun nl_tmcs_copy8 (src dst off) (nelisp_ptr_write_u64 (+ dst off) (nelisp_ptr_read_u64 src off))) (defun nl_tmcs_copy1 (src dst off) (nelisp_ptr_write_u8 (+ dst off) 0 (nelisp_ptr_read_u8 src off))) (defun nl_tty_memcpy_to_saved (src) (let* ((dst (nl_tty_saved_termios_ptr))) (seq (nl_tmcs_copy8 src dst 0) (nl_tmcs_copy8 src dst 8) (nl_tmcs_copy8 src dst 16) (nl_tmcs_copy8 src dst 24) (nl_tmcs_copy8 src dst 32) (nl_tmcs_copy8 src dst 40) (nl_tmcs_copy8 src dst 48) (nl_tmcs_copy1 src dst 56) (nl_tmcs_copy1 src dst 57) (nl_tmcs_copy1 src dst 58) (nl_tmcs_copy1 src dst 59)))))))
(provide (quote nelisp-cc-evalport-tty-memcpy))
