;;; compile-kernel.el --- build one codegen kernel as a freestanding ELF  -*- lexical-binding: t; -*-

;; Usage:
;;   emacs --batch -Q -L lisp -L src -l test/perf/codegen-kernels/compile-kernel.el \
;;     --eval '(codegen-kernel-compile "KERNEL.nl.el" "OUT")'
;;
;; A kernel file holds top-level `defun' forms in the AOT dialect and must
;; define `ck_main' taking the iteration count N and the repeat count R
;; (the kernel runs R times on the same data, for wall-clock timing).  The
;; driver adds the shared harness: `ck_mmap' (anonymous mmap) and
;; `ck_start', which reads "N [R]" from stdin and exits with
;; (logand (ck_main N R) 127) so the result can be checked against the C
;; versions.

(require 'nelisp-aot-compiler)

(defconst codegen-kernel--harness
  '((defun ck_mmap (size)
      (syscall-direct 9 0 size 3 34 -1 0))
    ;; The K-th (0 or 1) decimal number in BUF[0, GOT); a missing second
    ;; number is the repeat count 1.
    (defun ck_parse (buf got k)
      (let ((i 0) (field -1) (in 0) (n 0) (found 0) (c 0))
        (while (< i got)
          (setq c (ptr-read-u8 buf i))
          (if (and (>= c 48) (<= c 57))
              (seq
               (if (= in 0) (seq (setq field (+ field 1)) (setq in 1)) 0)
               (if (= field k) (seq (setq n (+ (* n 10) (- c 48))) (setq found 1)) 0))
            (setq in 0))
          (setq i (+ i 1)))
        (if (= found 1) n (if (= k 1) 1 0))))
    (defun ck_start ()
      (let ((buf (ck_mmap 4096))
            (got 0))
        (setq got (syscall-direct 0 0 buf 64 0 0 0))
        (ck_main (ck_parse buf got 0) (ck_parse buf got 1)))))
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
                       (exit (logand (ck_start) 127)))))
    (nelisp-aot-compile-sexp program out)
    out))

;;; compile-kernel.el ends here
