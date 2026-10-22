;;; vm-gc-poll-loop-gen.el --- the allocating loop of the VM GC poll checks, compiled by GNU Emacs  -*- lexical-binding: t; -*-
;; Usage: emacs -Q --batch -l test/fixtures/vm-gc-poll-loop-gen.el
;; vm-gc-poll-loop.eld is a call-free bytecode loop that allocates one cons per iteration and
;; drops it at the next: only the backward-branch poll (bytecode.c:861-866) can bound it.  The
;; idea and the shape of the loop are gc-port's (tools/ai/gc-port-vm-poll.py, Doc 223).
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
  (with-temp-file (expand-file-name "vm-gc-poll-loop.eld" dir)
    (insert ";; GNU " emacs-version "; reproduce with vm-gc-poll-loop-gen.el\n")
    (prin1 functions (current-buffer)) (insert "\n")))
