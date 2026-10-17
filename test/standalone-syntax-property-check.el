;;; standalone-syntax-property-check.el --- scanners honour the syntax-table property -*- lexical-binding: t -*-

;; Usage: nelisp --load test/standalone-syntax-property-check.el
;;
;; With `parse-sexp-lookup-properties' non-nil, GNU's scanners classify a
;; position by its `syntax-table' text property when it has one.  The
;; runtime's scan-lists, scan-sexps, forward-sexp and parse-partial-sexp
;; looked at the character only.  CC Mode's load-time self-test in
;; cc-defs.el marks "<" and ">" as parens this way and refuses to load
;; without it, which blocked cc-mode, js and json-mode.  The expected
;; values are GNU Emacs 31.1's, with the variable on and off.  Prints one
;; SYNTAX-PROPERTY line and exits non-zero on failure.

(defun syntax-property-check-run ()
  (let (out)
    (dolist (lookup '(t nil))
      (with-temp-buffer
        (let ((parse-sexp-lookup-properties lookup) (parse-sexp-ignore-comments t))
          (set-syntax-table (make-syntax-table))
          (modify-syntax-entry ?< ".")
          (modify-syntax-entry ?> ".")
          (insert "<()> (a <b> c)")
          (put-text-property 1 2 'syntax-table (cons 4 ?>))
          (put-text-property 4 5 'syntax-table (cons 5 ?<))
          (put-text-property 9 10 'syntax-table (cons 2 nil))
          (push (list lookup
                      (progn (goto-char 1) (forward-sexp) (point))
                      (scan-lists 1 1 0)
                      (condition-case e (scan-sexps 6 1) (error (car e)))
                      (progn (goto-char 7) (forward-sexp) (point))
                      (car (parse-partial-sexp 1 3)))
                out))))
    (nreverse out)))

(let* ((got (syntax-property-check-run))
       (want '((t 5 5 15 8 2) (nil 4 4 15 8 1)))
       (ok (equal got want)))
  (unless ok (princ (format "  MISMATCH got=%S want=%S\n" got want)))
  (princ (format "SYNTAX-PROPERTY %s\n" (if ok "PASS" "FAIL")))
  (kill-emacs (if ok 0 1)))

;;; standalone-syntax-property-check.el ends here
