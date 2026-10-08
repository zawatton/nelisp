;;; standalone-match-state-origin.el --- Match owner/origin regression -*- lexical-binding: t; -*-
(require 'nelisp-stdlib-match-state)
(let ((installed (eq (nelisp-stdlib-match-state-install) 'installed)))
  (with-temp-buffer
    (insert "abcdef")
    (let ((begin (copy-marker 2)) (end (copy-marker 4)))
      (set-match-data (list begin end))
      (unless (and (= (marker-position begin) 2) (= (marker-position end) 4))
        (error "Default RESEAT moved original markers"))
      (string-match "b" "abc")
      (unless (equal (match-data t) '(1 2))
        (error "New string regex inherited stale marker-buffer origin"))))
  (set-match-data '(1 2 nil nil 4 5))
  (unless (equal (match-data t) '(1 2 nil nil 4 5))
    (error "Interior missing group was lost"))
  (when installed
    (let ((old (symbol-function 'nlre-match-beginning)) refused)
      (unwind-protect
          (progn
            (fset 'nlre-match-beginning (lambda (_n) 99))
            (condition-case nil (set-match-data nil) (error (setq refused t)))
            (unless refused (error "Changed regex owner was accepted")))
        (fset 'nlre-match-beginning old))))
  (let ((old (symbol-function 'nelisp-bytecode-compiler-input-dialect)) refused)
    (unwind-protect
        (progn
          (fset 'nelisp-bytecode-compiler-input-dialect (lambda () '(:status unknown)))
          (condition-case nil (nelisp-stdlib-match-state-install)
            (error (setq refused t)))
          (unless refused (error "Changed match evidence owner was accepted")))
      (fset 'nelisp-bytecode-compiler-input-dialect old))))
(princ "MATCH-ORIGIN-OWNER-PASS\n")
