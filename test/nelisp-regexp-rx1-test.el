;;; nelisp-regexp-rx1-test.el --- rx1 regexp fast-path checks -*- lexical-binding: t; -*-

;; Differential checks against GNU Emacs for the rx1 changes to
;; lisp/nelisp-stdlib-regexp.el: the ASCII first-character map used by the
;; generic scan, the anchored `looking-at' matcher, and the public entry
;; defuns the build installs under string-match & co.  Runs in host Emacs.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'nelisp-stdlib-regexp
         (expand-file-name "../lisp/nelisp-stdlib-regexp.el"
                           (file-name-directory (or load-file-name buffer-file-name))))

(defconst nlre-rx1--patterns
  '("\\(zap\\|bar\\|foo\\)" "zap\\|bar\\|foo" "\\(?:ab\\|cd\\)+x" "[0-9]+\\|[a-c]x"
    "\\(a\\|b\\)\\(c\\|d\\)" "[^a-z]x\\|q" "K+z" "k\\|z" "\\<foo\\>" "x*foo"
    "\\(\\)foo" "foo\\|" "\\(foo\\)\\1" "[[:alpha:]]b\\|9" "[[:upper:]]a\\|zz"
    "\\(?:\\)b\\|c" "\\_<bar\\|foo" "é\\|foo" "\\`foo\\|bar" "bar\\'\\|foo"
    "\\(zap\\|bar\\|foo\\)\\{2\\}" "\\(a\\|b\\)+c" "[a-c]\\{2,3\\}d")
  "Patterns whose first-character map is built, rejected or irrelevant.")

(defconst nlre-rx1--subjects
  '("hello world foo bar" "ZAP BAR FOO" "xxbarxx" "" "f" "fo" "foo" "FOO"
    "KKz" "abx cdx" "99 a" "b.x" "Zap\nfoo" "éfoo" "xfoofoo" "abcd"
    "zzzzzzzzzzzzzzzzzzzz" "aaaaaabcd" "bbbd" "hello\nbar")
  "Subjects covering case, non-ASCII, empty and boundary text.")

(ert-deftest nelisp-regexp-rx1-first-map-matches-gnu ()
  (dolist (fold '(t nil))
    (let ((case-fold-search fold))
      (cl-letf (((symbol-function 'nlre--current-translate-table) (lambda () nil)))
        (nlre--compiled-cache-clear)
        (dolist (pattern nlre-rx1--patterns)
          (dolist (subject nlre-rx1--subjects)
            (dolist (start '(nil 0 1 3))
              (when (or (null start) (<= start (length subject)))
                (let ((expected (string-match pattern subject start))
                      (expected-data (and (string-match pattern subject start)
                                          (match-data)))
                      (actual (nlre-string-match pattern subject start)))
                  (should (equal actual expected))
                  (when expected
                    (should (equal (nlre-match-data) expected-data))))))))))))

(ert-deftest nelisp-regexp-rx1-first-map-shape ()
  (let ((case-fold-search t))
    (cl-letf (((symbol-function 'nlre--current-translate-table) (lambda () nil)))
      (nlre--compiled-cache-clear)
      (let ((map (aref (nlre--compiled-pattern "\\(zap\\|bar\\|foo\\)") 8)))
        (should (vectorp map))
        (should (equal (cl-loop for i below 128 when (eq (aref map i) 1) collect i)
                       (sort (append "zZbBfF" nil) #'<))))
      ;; Not fully described by ASCII literals/sets: no map.
      (dolist (pattern '("x*foo" "foo\\|" "é\\|foo" "\\<foo\\>"))
        (should-not (aref (nlre--compiled-pattern pattern) 8)))
      (let ((case-fold-search nil))
        (nlre--compiled-cache-clear)
        (should (equal (cl-loop for i below 128
                                when (eq (aref (aref (nlre--compiled-pattern "zap\\|Bar") 8) i) 1)
                                collect i)
                       '(66 122)))))))

(defvar nlre-rx1--mismatches nil)

(ert-deftest nelisp-regexp-rx1-looking-at-matches-gnu ()
  (setq nlre-rx1--mismatches nil)
  (dolist (fold '(t nil))
    (let ((case-fold-search fold))
      (dolist (narrow '(nil t))
        (with-temp-buffer
          (insert "abcdefghij0123456789ABCDEFGHIJ foo bar\nfoo")
          (when narrow (narrow-to-region 5 35))
          (cl-letf (((symbol-function 'nlre--current-translate-table) (lambda () nil)))
            (nlre--compiled-cache-clear)
            (dolist (pattern (append nlre-rx1--patterns
                                     '("[0-9]+" "[a-z]+[0-9]" "\\w+" "^foo" "foo$" "")))
              (save-excursion
                (let ((position (point-min)))
                  (while (<= position (point-max))
                    (goto-char position)
                    (let* ((expected (looking-at pattern))
                           (expected-data (and expected (match-data)))
                           (actual (nlre--looking-at pattern)))
                      (unless (eq (and actual t) (and expected t))
                        (push (list pattern position narrow fold actual expected) nlre-rx1--mismatches))
                      (when (and expected actual)
                        (should (equal (mapcar (lambda (m) (and m (if (markerp m) (marker-position m) m)))
                                               (nlre-match-data))
                                       (mapcar (lambda (m) (and m (if (markerp m) (marker-position m) m)))
                                               expected-data))))
                      (should (= (point) position)))
                    (setq position (1+ position)))))))))))
  (should (equal nlre-rx1--mismatches nil)))

(ert-deftest nelisp-regexp-rx1-entry-defuns-match-string-match ()
  (dolist (args '(("o" "foo") ("o" "foo" 1) ("o" "foo" -1) ("z" "foo")
                  ("\\(o\\)" "foo" 2) ("" "")))
    (let ((expected (apply #'string-match args))
          (actual (apply #'nlre--string-match-entry args)))
      (should (equal actual expected))
      (should (equal (nlre--string-match-p-entry (car args) (cadr args))
                     (string-match-p (car args) (cadr args))))))
  (dolist (bad '((1 "a") ("a" 1) ("a" "abc" 9) ("a" "abc" -9) ("a" "abc" x)))
    (should (equal (car (should-error (apply #'nlre--string-match-entry bad)))
                   (car (should-error (apply #'string-match bad)))))))

(ert-deftest nelisp-regexp-rx1-match-p-preserves-match-data ()
  (setq nlre--last-buffer nil)
  (nlre-string-match "\\(o\\)" "foo")
  (let ((before (nlre-match-data)))
    (should (nlre--string-match-p-entry "f" "foo"))
    (should (equal (nlre-match-data) before))))

(ert-deftest nelisp-regexp-rx1-set-match-data-roundtrip ()
  (setq nlre--last-buffer nil)
  (nlre-string-match "\\(o\\)\\(x\\)?" "foo")
  (let ((data (nlre-match-data)))
    (nlre-string-match "f" "foo")
    (nlre--set-match-data-entry data)
    (should (equal (nlre-match-data) data))
    (should (equal (nlre-match-beginning 1) 1))))

(ert-deftest nelisp-regexp-rx1-match-p-error-keeps-match-data ()
  (setq nlre--last-buffer nil)
  (nlre-string-match "\\(o\\)" "foo")
  (let ((before (nlre-match-data)))
    (should-error (nlre--string-match-p-entry "\\(" "foo"))
    (should (equal (nlre-match-data) before))
    (should-error (nlre--string-match-entry "\\(" "foo" nil t))
    (should (equal (nlre-match-data) before))))

(provide 'nelisp-regexp-rx1-test)
;;; nelisp-regexp-rx1-test.el ends here
