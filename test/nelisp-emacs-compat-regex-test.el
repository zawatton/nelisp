;;; nelisp-emacs-compat-regex-test.el --- ERT tests for regex wiring  -*- lexical-binding: t; -*-

(require 'ert)
(require 'nelisp-emacs-compat)

(defmacro nelisp-ec-regex-test--with-fresh-world (&rest body)
  "Run BODY with fresh emacs-compat global state."
  (declare (indent 0) (debug (body)))
  `(let ((nelisp-ec--buffers nil)
         (nelisp-ec--current-buffer nil)
         (nelisp-ec--match-data nil))
     ,@body))

(defmacro nelisp-ec-regex-test--with-buffer (text &rest body)
  "Create a fresh current buffer containing TEXT, then run BODY."
  (declare (indent 1) (debug (form body)))
  `(nelisp-ec-regex-test--with-fresh-world
    (let ((buf (nelisp-ec-generate-new-buffer "rx")))
      (nelisp-ec-set-buffer buf)
      (nelisp-ec-insert ,text)
      (nelisp-ec-goto-char 1)
      ,@body)))

(ert-deftest nelisp-ec-re-search-forward-literal-equivalent ()
  (nelisp-ec-regex-test--with-buffer "alpha beta gamma"
    (let ((literal (nelisp-ec-search-forward "beta")))
      (nelisp-ec-goto-char 1)
      (let ((regex (nelisp-ec-re-search-forward "beta")))
        (should (= literal regex))
        (should (= 11 (nelisp-ec-point)))
        (should (equal '(7 11) (nelisp-ec-match-data)))))))

(ert-deftest nelisp-ec-re-search-forward-regex-anchor ()
  (nelisp-ec-regex-test--with-buffer "zzz\nabc\ntail"
    (nelisp-ec-goto-char 5)
    (should (= 8 (nelisp-ec-re-search-forward "^abc$")))
    (should (equal '(5 8) (nelisp-ec-match-data)))))

(ert-deftest nelisp-ec-re-search-forward-bound-respected ()
  (nelisp-ec-regex-test--with-buffer "abc def ghi"
    (should (null (nelisp-ec-re-search-forward "def" 6 t)))
    (should (= 1 (nelisp-ec-point)))
    (should-error (progn
                    (nelisp-ec-goto-char 1)
                    (nelisp-ec-re-search-forward "def" 6 nil))
                  :type 'nelisp-ec-error)))

(ert-deftest nelisp-ec-re-search-forward-zero-length-anchor ()
  (nelisp-ec-regex-test--with-buffer "abc\ndef"
    (nelisp-ec-goto-char 5)
    (should (= 5 (nelisp-ec-re-search-forward "^")))
    (should (equal '(5 5) (nelisp-ec-match-data)))))

(ert-deftest nelisp-ec-re-search-backward-finds-rightmost ()
  (nelisp-ec-regex-test--with-buffer "aa11 bb22 cc33"
    (nelisp-ec-goto-char (nelisp-ec-point-max))
    (should (= 11 (nelisp-ec-re-search-backward "[a-z]+[0-9]+")))
    (should (= 11 (nelisp-ec-point)))
    (should (equal '(11 15) (nelisp-ec-match-data)))))

(ert-deftest nelisp-ec-re-search-backward-respects-bound ()
  (nelisp-ec-regex-test--with-buffer "x1 y2 z3"
    (nelisp-ec-goto-char (nelisp-ec-point-max))
    (should (= 7 (nelisp-ec-re-search-backward "[a-z][0-9]")))
    (nelisp-ec-goto-char (nelisp-ec-point-max))
    (should (= 7 (nelisp-ec-re-search-backward "[a-z][0-9]" 4)))
    (nelisp-ec-goto-char (nelisp-ec-point-max))
    (should (null (nelisp-ec-re-search-backward "[a-z][0-9]" 8 t)))))

(ert-deftest nelisp-ec-looking-at-prefix-match ()
  (nelisp-ec-regex-test--with-buffer "foo-123 bar"
    (should (nelisp-ec-looking-at "foo-[0-9]+"))
    (should (equal '(1 8) (nelisp-ec-match-data)))
    (should-not (nelisp-ec-looking-at "bar"))))

(ert-deftest nelisp-ec-looking-at-sees-prior-context-for-boundary ()
  (nelisp-ec-regex-test--with-buffer "x cat"
    (nelisp-ec-goto-char 3)
    (should (nelisp-ec-looking-at "\\bcat\\b"))
    (should (equal '(3 6) (nelisp-ec-match-data)))))

(ert-deftest nelisp-ec-looking-at-p-remains-literal ()
  (nelisp-ec-regex-test--with-buffer "a.c"
    (should (nelisp-ec-looking-at-p "a.c"))
    (should-not (nelisp-ec-looking-at-p "a\\.c"))
    (should (equal '(1 4) (nelisp-ec-match-data)))))

(ert-deftest nelisp-ec-match-data-after-search-with-grouping ()
  (nelisp-ec-regex-test--with-buffer "key=val"
    (should (= 8 (nelisp-ec-re-search-forward "\\([a-z]+\\)=\\([a-z]+\\)")))
    (should (equal '(1 8 1 4 5 8) (nelisp-ec-match-data)))
    (should (= 1 (nelisp-ec-match-beginning 0)))
    (should (= 8 (nelisp-ec-match-end 0)))
    (should (= 1 (nelisp-ec-match-beginning 1)))
    (should (= 4 (nelisp-ec-match-end 1)))
    (should (= 5 (nelisp-ec-match-beginning 2)))
    (should (= 8 (nelisp-ec-match-end 2)))))

(ert-deftest nelisp-ec-re-search-with-nested-grouping ()
  (nelisp-ec-regex-test--with-buffer "abc"
    (should (= 4 (nelisp-ec-re-search-forward "\\(\\(a\\)bc\\)")))
    (should (equal '(1 4 1 4 1 2) (nelisp-ec-match-data)))))

(ert-deftest nelisp-ec-match-data-unmatched-optional-group ()
  (nelisp-ec-regex-test--with-buffer "b"
    (should (= 2 (nelisp-ec-re-search-forward "\\(a\\)?b")))
    (should (equal '(1 2 nil nil) (nelisp-ec-match-data)))
    (should (null (nelisp-ec-match-beginning 1)))
    (should (null (nelisp-ec-match-end 1)))))

(ert-deftest nelisp-ec-re-search-forward-respects-narrowing ()
  (nelisp-ec-regex-test--with-buffer "hdr body tail"
    (nelisp-ec-narrow-to-region 5 10)
    (nelisp-ec-goto-char 5)
    (should (= 9 (nelisp-ec-re-search-forward "body")))
    (should (equal '(5 9) (nelisp-ec-match-data)))
    (nelisp-ec-goto-char 5)
    (should (null (nelisp-ec-re-search-forward "tail" nil t)))))

(provide 'nelisp-emacs-compat-regex-test)
;;; nelisp-emacs-compat-regex-test.el ends here
