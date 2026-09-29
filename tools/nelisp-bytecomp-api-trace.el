;;; nelisp-bytecomp-api-trace.el --- trace API calls during S6 bytecomp -*- lexical-binding: t; -*-

;; Load before bytecomp.el on GNU Emacs or NeLisp.  The wrappers count calls
;; which pass through each candidate's function cell; native/internal calls
;; that bypass the Lisp function cell are outside this measurement.

(defvar nelisp-bytecomp-api-trace-counts nil)
(defvar nelisp-bytecomp-api-trace-output (getenv "NELISP_BYTECOMP_TRACE_OUT"))

(defun nelisp-bytecomp-api-trace-install ()
  ;; The shell passes names before this file loads, so tracer setup does not
  ;; itself call any wrapped API function.
  (dolist (name (split-string (getenv "NELISP_BYTECOMP_TRACE_NAMES") "," t))
    (let ((sym (intern name)))
      (when (and (fboundp sym) (not (assq sym nelisp-bytecomp-api-trace-counts)))
        (let ((original (symbol-function sym)))
          (push (cons sym 0) nelisp-bytecomp-api-trace-counts)
          (fset sym
                (eval (list 'lambda '(&rest args)
                            (list 'setq 'nelisp-bytecomp-api-trace-counts
                                  (list 'nelisp-bytecomp-api-trace-bump
                                        (list 'quote sym)))
                            (list 'apply (list 'quote original) 'args))))))))
  (add-hook 'kill-emacs-hook (quote nelisp-bytecomp-api-trace-write)))

(defun nelisp-bytecomp-api-trace-bump (sym)
  (let ((cell (assq sym nelisp-bytecomp-api-trace-counts)))
    (setcdr cell (1+ (cdr cell))))
  nelisp-bytecomp-api-trace-counts)

(defun nelisp-bytecomp-api-trace-write ()
  (when nelisp-bytecomp-api-trace-output
    (with-temp-file nelisp-bytecomp-api-trace-output
      (insert "name\tcount\n")
      (dolist (row (sort (copy-sequence nelisp-bytecomp-api-trace-counts)
                         (lambda (a b) (string< (symbol-name (car a)) (symbol-name (car b))))))
        (when (> (cdr row) 0)
          (insert (format "%s\t%d\n" (symbol-name (car row)) (cdr row))))))))

(nelisp-bytecomp-api-trace-install)
(provide 'nelisp-bytecomp-api-trace)
;;; nelisp-bytecomp-api-trace.el ends here
