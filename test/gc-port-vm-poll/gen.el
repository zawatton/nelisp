;;; gen.el --- the allocating loop of the VM safepoint check, compiled by GNU Emacs  -*- lexical-binding: t; -*-
;; Usage: emacs -Q --batch -l test/gc-port-vm-poll/gen.el
;; funcall.eld and funcall-workload.el are the codegen lane's frozen S5.13 fixtures,
;; copied unchanged (sha256 of funcall.eld 8662cd5f..., of the workload 57c8337b...).
;; loop.eld is a call-free bytecode loop that allocates one cons per iteration and
;; drops it at the next: only the backward-branch poll (bytecode.c:861-866) can bound it.
(require 'bytecomp)
(let* ((dir (file-name-directory (or load-file-name buffer-file-name)))
       (print-length nil) (print-level nil) (print-escape-nonascii t)
       (functions
        `((loop . ,(byte-compile
                    '(lambda (n) (let ((i 0) (tmp nil))
                                   (while (< i n)
                                     (setq tmp (cons i i))
                                     (setq i (1+ i)))
                                   (car tmp))))))))
  (with-temp-file (expand-file-name "loop.eld" dir)
    (insert ";; GNU " emacs-version "; reproduce with gen.el\n")
    (prin1 functions (current-buffer)) (insert "\n")))
