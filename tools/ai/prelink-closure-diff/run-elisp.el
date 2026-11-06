;;; run-elisp.el --- Run a prelink closure Elisp main on CLI arguments  -*- lexical-binding: t; -*-

;; Usage: emacs --batch -Q -L lisp -l run-elisp.el SCRIPT.el ARG...
;; Loads SCRIPT.el as interpreted source, calls <basename>-main on ARG...,
;; prints the returned digest line to stdout and the wall time to stderr.
;; Exit status 1 when the main function signals.

;;; Code:

(let* ((args command-line-args-left)
       (script (expand-file-name (car args)))
       (main (intern (concat (file-name-base script) "-main")))
       (t0 (float-time)))
  (setq command-line-args-left nil)
  (condition-case err
      (progn
        (load script nil t t)
        (let ((t1 (float-time)))
          (princ (funcall main (cdr args)))
          (message "elisp-seconds load=%.3f run=%.3f" (- t1 t0) (- (float-time) t1))))
    (error
     (message "ELISP-REFUSED: %S" err)
     (kill-emacs 1))))
