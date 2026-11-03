;;; lisp-test.el --- ERT for lightweight lisp facade  -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)

(require 'lisp)

(ert-deftest lisp-test/require-loads-standard-feature ()
  (should (featurep 'lisp))
  (dolist (sym '(forward-sexp backward-sexp mark-sexp kill-sexp
                 beginning-of-defun end-of-defun insert-pair
                 insert-parentheses delete-pair check-parens))
    (should (fboundp sym))))

(ert-deftest lisp-test/internal-forward-scan-handles-nested-list ()
  (with-temp-buffer
    (insert "(foo [bar] \"baz\") tail")
    (goto-char (point-min))
    (should (= (lisp--scan-sexp-forward) 18))
    (should (string= (buffer-substring-no-properties (point) (point-max))
                     " tail"))))

(ert-deftest lisp-test/internal-forward-scan-handles-quoted-list ()
  (with-temp-buffer
    (insert "'(a b)")
    (goto-char (point-min))
    (should (= (lisp--scan-sexp-forward) (point-max)))))

(ert-deftest lisp-test/internal-backward-scan-includes-prefix ()
  (with-temp-buffer
    (insert "'foo")
    (goto-char (point-max))
    (should (= (lisp--scan-sexp-backward) (point-min)))))

(ert-deftest lisp-test/check-parens-range-detects-unmatched-close ()
  (with-temp-buffer
    (insert "(ok) )")
    (should-error
     (lisp--check-parens-range (point-min) (point-max))
     :type 'scan-error)))

(ert-deftest lisp-test/check-parens-range-accepts-comments-and-strings ()
  (with-temp-buffer
    (insert "(message \")\") ; ] ignored\n")
    (should-not (lisp--check-parens-range (point-min) (point-max)))))

(provide 'lisp-test)

;;; lisp-test.el ends here
