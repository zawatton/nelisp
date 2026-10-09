;;; standalone-file-read-hook-arity.el --- Preserve text hook arity -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
(let ((saved (symbol-function 'nelisp--syscall-read-file))
      (coding-system-for-read nil)
      (calls 0))
  (unwind-protect
      (progn
        (fset 'nelisp--syscall-read-file
              (lambda (_filename) (setq calls (1+ calls)) "abc"))
        (with-temp-buffer
          (let ((result (insert-file-contents (getenv "NELISP_LITERAL_FIXTURE"))))
            (unless (and (= calls 1) (equal (buffer-string) "abc")
                         (= (cadr result) 3) (= (point) 1))
              (error "One-argument text hook mismatch"))))
        (princ "FILE-READ-HOOK-PASS cases=1\n"))
    (fset 'nelisp--syscall-read-file saved)))
