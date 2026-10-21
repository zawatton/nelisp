;;; nelisp-regex-replacement-test.el --- GNU replacement parity -*- lexical-binding: t; -*-

;;; Commentary:

;; Exercise the replacement helpers as authored in the standalone prelude
;; without loading that prelude over GNU Emacs's own primitive definitions.

;;; Code:

(require 'ert)
(require 'nelisp-stdlib-unicode
         (expand-file-name "../lisp/nelisp-stdlib-unicode.el"
                           (file-name-directory
                            (or load-file-name buffer-file-name))))

(defconst nelisp-regex-replacement-test--prelude
  (expand-file-name "../scripts/nelisp-stdlib-prelude.el"
                    (file-name-directory (or load-file-name buffer-file-name))))

(defun nelisp-regex-replacement-test--source-defun (name)
  "Read NAME's `defun' directly from the standalone prelude source."
  (with-temp-buffer
    (insert-file-contents nelisp-regex-replacement-test--prelude)
    (goto-char (point-min))
    (unless (re-search-forward
             (concat "^[[:space:]]*(defun[[:space:]]+"
                     (regexp-quote (symbol-name name)) "\\_>")
             nil t)
      (error "Prelude definition not found: %S" name))
    (goto-char (match-beginning 0))
    (read (current-buffer))))

(dolist (name '(nelisp--cc03-match-slice
                nelisp--cc03-replacement-case
                nelisp--cc03-expand-replacement
                nelisp--cc03-case-text
                nelisp--rris-expand
                nelisp--rris-case
                nelisp--rris))
  (eval (nelisp-regex-replacement-test--source-defun name)))

(let ((form (nelisp-regex-replacement-test--source-defun 'replace-match)))
  ;; Run the authored body without replacing GNU Emacs's golden primitive.
  (setcar (cdr form) 'nelisp-regex-replacement-test--replace-match)
  (eval form))

(ert-deftest nelisp-regex-replacement-unicode-data-is-intact ()
  "Generated categories and mappings retain their recorded semantic hash."
  (let ((payload
         (prin1-to-string
          ;; The generator's payload order (tools/nelisp-generate-unicode.el).
          (list nelisp--unicode-category-ranges
                nelisp--unicode-simple-upcase-ranges
                nelisp--unicode-simple-downcase-ranges
                nelisp--unicode-titlecase-property-ranges
                nelisp--unicode-full-upcase
                nelisp--unicode-full-downcase
                nelisp--unicode-full-titlecase
                nelisp--unicode-canonical-ranges
                nelisp--unicode-equivalence-ranges
                nelisp--unicode-syntax-ranges
                nelisp--unicode-syntax-entries
                nelisp--unicode-category-docstrings
                nelisp--unicode-category-sets
                nelisp--unicode-category-runs))))
    (should (equal (secure-hash 'sha256 payload)
                   nelisp--unicode-data-sha256)))
  (should (eq (nelisp-unicode-category ?A) 'Lu))
  (should (eq (nelisp-unicode-category ?ǅ) 'Lt))
  (should (eq (nelisp-unicode-category #x0301) 'Mn))
  (should (eq (nelisp-unicode-category ?中) 'Lo))
  (should (eq (nelisp-unicode-category #x216B) 'Nl))
  (should (eq (nelisp-unicode-category #x00BD) 'No))
  (should (eq (nelisp-unicode-category ?９) 'Nd))
  (should (eq (nelisp-unicode-category #x1F600) 'So))
  (should (= (nelisp-unicode-syntax #x00A0) ?\s))
  (should (= (nelisp-unicode-syntax #x200B) ?\s))
  (should (= (nelisp-unicode-syntax ?中) ?w))
  (should (= (nelisp-unicode-syntax #x00BD) ?_))
  (should (= (nelisp-unicode-canonical-character ?Σ) ?σ))
  (should (= (nelisp-unicode-equivalent-character ?Σ) ?σ))
  (should (= (nelisp-unicode-equivalent-character ?σ) ?ς))
  (should (= (nelisp-unicode-equivalent-character ?ς) ?Σ)))

(ert-deftest nelisp-regex-replacement-unicode-case-provider-matches-gnu ()
  "Private simple, full, and title mappings agree with GNU 31.1."
  (dolist (c '(?a ?A ?ä ?Ä ?ǆ ?ǅ ?ß ?ﬃ ?σ ?ς ?Ж ?𐐨))
    (should (= (nelisp-unicode-simple-upcase c) (upcase c)))
    (should (= (nelisp-unicode-simple-downcase c) (downcase c))))
  (dolist (string '("ǆ ﬃ ß" "Straße" "ὀδυσσεύς" "ΟΣ Σ ΣΑ ΟΣ́" "𐐨𐐀"
                    "中áⅻ½ ß ﬃ ǆ"))
    (should (equal (nelisp-unicode-case-string string 'upper)
                   (upcase string)))
    (should (equal (nelisp-unicode-case-string string 'lower)
                   (downcase string)))
    (should (equal (nelisp-unicode-case-string string 'title)
                   (upcase-initials string)))))

(ert-deftest nelisp-regex-replacement-case-flags-match-gnu-31 ()
  "Case flags follow GNU search.c's four-flag decision table."
  (dolist (case '((".A" nil) (" A" nil) ("1Ab" nil) ("x" nil)
                  ("Ab" capitalize) ("Ää" capitalize) ("ǅa" capitalize)
                  ("AB" upper) ("ÄÄ" upper) ("A" upper) ("ǅ" upper)))
    (should (eq (nelisp--cc03-replacement-case (car case)) (cadr case)))))

(ert-deftest nelisp-regex-replacement-case-conversion-matches-gnu-31 ()
  "Saved GNU 31.1 results preserve mixed-case tails under cap-initial."
  (dolist (case '((".A" "xY zQ")
                  (" A" "xY zQ")
                  ("1Ab" "xY zQ")
                  ("Ab" "XY ZQ")
                  ("Ää" "XY ZQ")
                  ("ǅa" "XY ZQ")
                  ("AB" "XY ZQ")
                  ("A" "XY ZQ")
                  ("ǅ" "XY ZQ")))
    (should (equal (nelisp--rris (regexp-quote (car case)) "xY zQ" (car case))
                   (cadr case)))))

(ert-deftest nelisp-regex-replacement-preserves-bytes-and-full-titlecase ()
  "No-action bytes and GNU full titlecase mappings retain their exact form."
  (let* ((rep (unibyte-string 255))
         (result (nelisp--rris "a" rep "a" nil t)))
    (should (equal result rep))
    (should-not (multibyte-string-p result)))
  (should (equal (nelisp--rris "Ab" "ǆ" "Ab" nil t) "ǅ"))
  (should (equal (nelisp--rris "Ab" "ﬃ" "Ab" nil t) "Ffi")))

(ert-deftest nelisp-regex-replacement-cases-expanded-backreferences ()
  "GNU cases the fully expanded replacement, including backreferences."
  (let ((case-fold-search nil))
    (string-match "A\\(b\\)" "Ab")
    (should (equal (nelisp-regex-replacement-test--replace-match
                    "\\1 zQ" nil nil "Ab")
                   "B ZQ"))
    ;; GNU full/titlecase mapping expands sharp-s during replacement.
    (string-match "A\\(ß\\)" "Aß")
    (should (equal (nelisp-regex-replacement-test--replace-match
                    "\\1" nil nil "Aß")
                   "Ss"))
    (string-match "Ab" "Ab")
    (should (equal (nelisp-regex-replacement-test--replace-match
                    "ﬃ" nil t "Ab")
                   "Ffi"))))

(ert-deftest nelisp-regex-replacement-retains-empty-and-subexp-tails ()
  "Advancing past empty matches and replacing a subgroup retain skipped text."
  (should (equal (nelisp--rris "" "X" "ab") "XaXb"))
  (let ((case-fold-search nil))
    (should (equal (nelisp--rris "\\(a\\)b" "X" "zabq" t t 1)
                   "zXbq"))
    ;; An empty subgroup does not make the whole match empty.  Resume at the
    ;; whole match's end so the following `a' remains eligible to match.
    (should (equal (nelisp--rris "a\\(\\)" "X" "aa" t t 1)
                   "aXaX"))))

(ert-deftest nelisp-regex-replacement-preserves-match-data ()
  "Replacement leaves its caller's match data intact, as GNU does."
  (string-match "\\(sent\\)" "sentinel")
  (let ((before (match-data)))
    (should (equal (nelisp--rris "a" "X" "banana") "bXnXnX"))
    (should (equal (match-data) before))))

(ert-deftest nelisp-regex-replacement-function-gets-local-match-data ()
  "Functional REP sees substring-relative data and returns replacement text."
  (let (seen)
    (should
     (equal
      (nelisp--rris
       "a" (lambda (matched)
             (setq seen (cons (list matched (match-data)) seen))
             (number-to-string (match-beginning 0)))
       "aba" t t)
      "0b0"))
    (should (equal (nreverse seen) '(("a" (0 1)) ("a" (0 1))))))
  (let ((case-fold-search nil))
    (should (equal (nelisp--rris "\\(a\\)" (lambda (_matched) "\\1X")
                                 "a" t nil)
                   "aX"))))

(provide 'nelisp-regex-replacement-test)
;;; nelisp-regex-replacement-test.el ends here
