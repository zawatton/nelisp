;;; regi-test.el --- ERT for lightweight regi facade  -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)

(require 'regi)

(defvar regi-test--seen nil)

(ert-deftest regi-test/require-loads-standard-feature ()
  (should (featurep 'regi))
  (dolist (sym '(regi-pos regi-mapcar regi-interpret))
    (should (fboundp sym))))

(ert-deftest regi-test/regi-pos-line-positions ()
  (with-temp-buffer
    (insert "  alpha\nbeta\n")
    (goto-char (point-min))
    (forward-char 3)
    (should (= (regi-pos 'bol) 1))
    (should (= (regi-pos 'boi) 3))
    (should (= (regi-pos 'eol) 8))
    (should (= (regi-pos 'bonl) 9))
    (should (= (regi-pos 'boi t) 2))))

(ert-deftest regi-test/regi-mapcar-builds-frame ()
  (should (equal (regi-mapcar '("^a" "^b") '(push curline regi-test--seen)
                              t nil)
                 '(("^a" (push curline regi-test--seen) t)
                   ("^b" (push curline regi-test--seen) t)))))

(ert-deftest regi-test/interpret-matches-lines ()
  (let ((regi-test--seen nil))
    (with-temp-buffer
      (insert "foo\nbar\nfood\n")
      (regi-interpret
       '(("^foo" (push curline regi-test--seen)))
       (point-min) (point-max))
      (should (equal (nreverse regi-test--seen) '("foo" "food"))))))

(ert-deftest regi-test/interpret-continue-processes-next-entry-same-line ()
  (let ((regi-test--seen nil))
    (with-temp-buffer
      (insert "alpha\n")
      (regi-interpret
       '(("^alpha" (progn (push 'first regi-test--seen) '(continue)))
         ("^alpha" (push 'second regi-test--seen)))
       (point-min) (point-max))
      (should (equal (nreverse regi-test--seen) '(first second))))))

(ert-deftest regi-test/interpret-negate-case-fold-step-and-abort ()
  (let ((regi-test--seen nil))
    (with-temp-buffer
      (insert "One\nTwo\nskip\nthree\n")
      (regi-interpret
       '(("^two" (push curline regi-test--seen) nil t)
         ("^skip" '((step . 1)))
         ("^three" (progn (push 'stop regi-test--seen) '(abort))))
       (point-min) (point-max))
      (should (equal (nreverse regi-test--seen) '("Two" stop))))))

(provide 'regi-test)

;;; regi-test.el ends here
