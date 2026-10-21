;;; compile-kernel.el --- build one codegen kernel as a freestanding ELF  -*- lexical-binding: t; -*-

;; Usage:
;;   emacs --batch -Q -L lisp -L src -l test/perf/codegen-kernels/compile-kernel.el \
;;     --eval '(codegen-kernel-compile "KERNEL.nl.el" "OUT")'
;;
;; A kernel file holds top-level `defun' forms in the AOT dialect and must
;; define `ck_main' taking the iteration count N.  The driver adds the
;; shared harness: `ck_mmap' (anonymous mmap) and `ck_read_n' (decimal N
;; from stdin), then exits with (logand (ck_main N) 127) so the result can
;; be checked against the C versions.

(require 'nelisp-aot-compiler)

(defconst codegen-kernel--harness
  '((defun ck_mmap (size)
      (syscall-direct 9 0 size 3 34 -1 0))
    (defun ck_read_n ()
      (let ((buf (ck_mmap 4096))
            (got 0)
            (i 0)
            (n 0)
            (c 0))
        (setq got (syscall-direct 0 0 buf 64 0 0 0))
        (while (< i got)
          (setq c (ptr-read-u8 buf i))
          (if (and (>= c 48) (<= c 57))
              (setq n (+ (* n 10) (- c 48)))
            0)
          (setq i (+ i 1)))
        n)))
  "Dialect harness shared by every kernel program.")

(defun codegen-kernel--read-forms (file)
  "Return the top-level forms of FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (let (forms form)
      (while (setq form (condition-case nil (read (current-buffer))
                          (end-of-file nil)))
        (push form forms))
      (nreverse forms))))

(defun codegen-kernel-compile (file out)
  "Compile kernel FILE with the harness into the ELF executable OUT."
  (let ((program `(seq ,@codegen-kernel--harness
                       ,@(codegen-kernel--read-forms file)
                       (exit (logand (ck_main (ck_read_n)) 127)))))
    (nelisp-aot-compile-sexp program out)
    out))

;;; compile-kernel.el ends here
