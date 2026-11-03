;;; nelisp-regex-test.el --- ERT tests for nelisp-regex word boundaries -*- lexical-binding: t; -*-

;;; Commentary:

;; Doc 51 Track J (2026-05-04) — extends `nelisp-regex' to handle the
;; directional word-boundary anchors `\<' (word start) and `\>' (word
;; end).  These are independent of the existing `\b' (any boundary)
;; and `\B' (no boundary) which are tested for regression.

;;; Code:

(require 'ert)
(setq load-prefer-newer t)
(require 'rx)
(require 'nelisp-regex)

;;;; --- helpers ---------------------------------------------------------------

(defun nelisp-regex-test--matched (pat str)
  "Return the matched substring of PAT in STR or nil."
  (let ((m (nelisp-rx-string-match pat str)))
    (and m (substring str (plist-get m :start) (plist-get m :end)))))

(defun nelisp-regex-test--start (pat str)
  "Return the match start position of PAT in STR or nil."
  (let ((m (nelisp-rx-string-match pat str)))
    (and m (plist-get m :start))))

(defun nelisp-regex-test--equivalent-match-p (pattern string &optional start)
  "Assert string and compiled PATTERN paths match identically on STRING.
START is passed through to both implementations."
  (should (equal (nelisp-rx-string-match pattern string start)
                 (nelisp-rx-string-match (nelisp-rx-compile pattern)
                                         string
                                         start))))

(ert-deftest nelisp-regex-test/public-api-smoke ()
  (let ((pattern (nelisp-rx-compile "\\<foo\\>")))
    (should (nelisp-rx-string-match pattern "say foo"))
    (should (= 2 (length (nelisp-rx-string-match-all "foo" "foo foo"))))
    (should (equal "bar foo" (nelisp-rx-replace "foo" "foo foo" "bar")))
    (should (equal "bar bar" (nelisp-rx-replace-all "foo" "foo foo" "bar")))
    (dolist (case '(("foo\\'" "foo" nil)
                    ("foo\\'" "prefixfoo" nil)
                    ("foo\\'" "foox" nil)
                    ("foo\\'" "foo" 1)
                    ("foo\\'" "prefixfoo" 7)
                    ("foo\\'" "prefixfoo" 8)
                    ("\\.png\\'" "image.png" nil)
                    ("\\.png\\'" "image.png~" nil)
                    ("foo\\ bar\\'" "xfoo bar" nil)
                    ("\\\\tmp\\'" "path\\tmp" nil)
                    ("\\'" "" nil)
                    ("\\'" "abc" 3)
                    ("\\'" "abc" 4)))
      (apply #'nelisp-regex-test--equivalent-match-p case))
    (let ((m (nelisp-rx-string-match "\\.png\\'" "icons/image.png")))
      (should (equal '(:start 11 :end 15 :groups nil) m))
      (should (equal (list m)
                     (nelisp-rx-string-match-all "\\.png\\'"
                                                 "icons/image.png"))))))

(ert-deftest nelisp-regex-test/literal-nonmatch-does-not-return-empty-match ()
  "A failed literal match must return nil, not a zero-width match at 0."
  (should-not (nelisp-rx-string-match "org-fold-visible"
                                      "org-fold-outline"))
  (should-not (string-match-p "org-fold-visible"
                              "org-fold-outline")))

(ert-deftest nelisp-regex-test/rx-compatibility-surface-stays-working ()
  (let ((rx-constituents
         '((legacy (lambda (_form) "abc") 0 0 nil)))
        (saved (get 'nelisp-regex-test--rx-sample 'rx-definition)))
    (unwind-protect
        (progn
          (rx-define nelisp-regex-test--rx-sample "def")
          (should (equal "abc" (rx-to-string '(legacy) t)))
          (should (equal "def" (rx-to-string 'nelisp-regex-test--rx-sample t)))
          (should (equal "ghi" (rx-let-eval '((local "ghi"))
                                 (rx-to-string 'local t))))
          (should (string-match-p "jkl" (rx-let ((local "jkl"))
                                          (rx local))))
          (should (equal "def" (rx-to-string 'nelisp-regex-test--rx-sample t))))
      (put 'nelisp-regex-test--rx-sample 'rx-definition saved))))

;;;; --- character classes -----------------------------------------------------

(ert-deftest nelisp-regex-test/inverted-class-range-is-empty ()
  "GNU Emacs accepts inverted ranges in classes as empty ranges."
  (should-not (nelisp-rx-string-match "[z-a]" "a"))
  (should-not (nelisp-rx-string-match "[z-a]" "z"))
  (should-not (nelisp-rx-string-match "[z-a]" "-"))
  (should-not (nelisp-rx-string-match "[z-a]" "\n")))

(ert-deftest nelisp-regex-test/negated-inverted-class-range-is-anychar ()
  "`rx.el' emits `[^z-a]' for `anychar', so it must compile and match."
  (should (nelisp-rx-string-match "[^z-a]" "a"))
  (should (nelisp-rx-string-match "[^z-a]" "z"))
  (should (nelisp-rx-string-match "[^z-a]" "-"))
  (should (nelisp-rx-string-match "[^z-a]" "\n")))

;;;; --- \< (word start) -------------------------------------------------------

(ert-deftest nelisp-regex-test/backslash-is-literal-in-class ()
  "GNU Emacs closes `[^\\]' at the bracket after the literal backslash."
  (should (nelisp-rx-string-match "[^\\]" "a"))
  (should-not (nelisp-rx-string-match "[^\\]" "\\")))

(ert-deftest nelisp-regex-test/backslash-class-keeps-following-character ()
  "A backslash in a class is literal and does not escape the next character."
  (should (nelisp-rx-string-match "[\\w]" "\\"))
  (should (nelisp-rx-string-match "[\\w]" "w"))
  (should-not (nelisp-rx-string-match "[\\w]" "x")))

(ert-deftest nelisp-regex-test/syntax-errors-inherit-invalid-regexp ()
  "Substring validation can catch parser errors like GNU Emacs does."
  (dolist (regexp '("[abc" "\\("))
    (should
     (condition-case nil
         (progn (nelisp-rx-compile regexp) nil)
       (invalid-regexp t)))))

(ert-deftest nelisp-regex-test/subregexp-context-catches-standalone-syntax-errors ()
  "The standalone context probe catches only incomplete-prefix errors."
  (require 'emacs-string)
  (let* ((context-function (and (fboundp 'subregexp-context-p)
                                (symbol-function 'subregexp-context-p)))
         (match-function (and (fboundp 'string-match)
                              (symbol-function 'string-match)))
         (parity-file (locate-library "emacs-parity-regex-charclass")))
    (unwind-protect
        (progn
          (when (and parity-file (string-suffix-p ".elc" parity-file))
            (setq parity-file (substring parity-file 0 -1)))
          (load-file parity-file)
          (fset 'string-match
                (lambda (&rest _args)
                  (signal 'nelisp-rx-syntax-error
                          '("unterminated class"))))
          (should-not (subregexp-context-p "[abc" 4))
          (fset 'string-match
                (lambda (&rest _args)
                  (signal 'nelisp-rx-syntax-error
                          '("unexpected char"))))
          (should (subregexp-context-p "[abc" 4)))
      (if context-function
          (fset 'subregexp-context-p context-function)
        (fmakunbound 'subregexp-context-p))
      (if match-function
          (fset 'string-match match-function)
        (fmakunbound 'string-match)))))

(ert-deftest nelisp-regex-test/wbs-matches-at-start-of-line ()
  "`\\\\<foo' matches at BOS when followed by a word char."
  (should (equal "foo" (nelisp-regex-test--matched "\\<foo" "foobar"))))

(ert-deftest nelisp-regex-test/wbs-matches-after-space ()
  "`\\\\<foo' matches `foo' starting after a space (= word start)."
  (should (equal 4 (nelisp-regex-test--start "\\<foo" "abc foo bar"))))

(ert-deftest nelisp-regex-test/wbs-rejects-mid-word ()
  "`\\\\<bar' does NOT match the `bar' inside `foobar' (= no word
boundary preceding the b)."
  (should-not (nelisp-rx-string-match "\\<bar" "foobar")))

(ert-deftest nelisp-regex-test/wbs-skips-to-next-word-start ()
  "`\\\\<bar' inside `foo bar' must skip past the inner-word `bar'
candidate (there is none here) and find the real word-start `bar'."
  (should (equal 4 (nelisp-regex-test--start "\\<bar" "foo bar"))))

;;;; --- \> (word end) ---------------------------------------------------------

(ert-deftest nelisp-regex-test/wbe-matches-at-end-of-line ()
  "`foo\\\\>' matches at EOS."
  (should (equal "foo" (nelisp-regex-test--matched "foo\\>" "say foo"))))

(ert-deftest nelisp-regex-test/wbe-matches-before-space ()
  "`foo\\\\>' matches when followed by a space."
  (should (equal 4 (nelisp-regex-test--start "foo\\>" "abc foo bar"))))

(ert-deftest nelisp-regex-test/wbe-rejects-mid-word ()
  "`foo\\\\>' does NOT match `foo' inside `foobar' (= followed by
word char `b'; not a word end)."
  (should-not (nelisp-rx-string-match "foo\\>" "foobar")))

;;;; --- combined \<...\> ------------------------------------------------------

(ert-deftest nelisp-regex-test/wb-pair-matches-whole-word ()
  "`\\\\<word\\\\>' matches the standalone occurrence only."
  (should (equal "word"
                 (nelisp-regex-test--matched "\\<word\\>" "subword word foo"))))

(ert-deftest nelisp-regex-test/wb-pair-finds-the-isolated-word ()
  "`\\\\<word\\\\>' must skip the substring inside `subword' and
land on the standalone `word'."
  (should (equal 8 (nelisp-regex-test--start "\\<word\\>" "subword word foo"))))

;;;; --- regression for \b / \B (= the existing any-boundary forms) -----------

(ert-deftest nelisp-regex-test/wb-any-still-works ()
  "Pre-existing `\\\\b' (any word boundary) regression."
  (should (nelisp-rx-string-match "\\bfoo" "say foo")))

(ert-deftest nelisp-regex-test/nwb-still-works ()
  "Pre-existing `\\\\B' (no word boundary) regression."
  (should (nelisp-rx-string-match "f\\Bo" "foobar")))

;;;; --- scan candidate-position optimization ---------------------------------

(defun nelisp-regex-test--native-span (pattern string &optional start)
  "Return the native match span for PATTERN in STRING."
  (let ((m (nelisp-rx-string-match pattern string start)))
    (and m (list (plist-get m :start) (plist-get m :end)))))

(defun nelisp-regex-test--host-spans-all (pattern string &optional start)
  "Return all HOST match spans, including zero-width matches."
  (let ((pos (or start 0))
        (limit (length string))
        (spans nil))
    (while (and (<= pos limit)
                (string-match pattern string pos))
      (let ((beg (match-beginning 0))
            (end (match-end 0)))
        (push (list beg end) spans)
        (setq pos (if (= beg end) (1+ end) end))))
    (nreverse spans)))

(ert-deftest nelisp-regex-test/scan-compiled-and-uncompiled-preserve-captures ()
  "Candidate scanning keeps compiled parity and capture offsets."
  (let* ((pattern "\\(foo\\)-\\(bar\\)")
         (string "xfoo-bar")
         (uncompiled (nelisp-rx-string-match pattern string))
         (compiled (nelisp-rx-string-match (nelisp-rx-compile pattern)
                                           string)))
    (should (equal uncompiled compiled))
    (should (equal '(:start 1 :end 8
                     :groups ((:index 1 :start 1 :end 4)
                              (:index 2 :start 5 :end 8)))
                   uncompiled))
    (should (equal "foo"
                   (substring string
                              (plist-get (car (plist-get uncompiled :groups))
                                         :start)
                              (plist-get (car (plist-get uncompiled :groups))
                                         :end))))
    (should (equal "bar"
                   (substring string
                              (plist-get (cadr (plist-get uncompiled :groups))
                                         :start)
                              (plist-get (cadr (plist-get uncompiled :groups))
                                         :end))))))

(ert-deftest nelisp-regex-test/scan-match-all-zero-width-parity ()
  "Zero-width match-all advances and agrees for both pattern forms."
  (let* ((pattern "")
         (string "ab")
         (expected '((0 0) (1 1) (2 2)))
         (uncompiled (nelisp-rx-string-match-all pattern string))
         (compiled (nelisp-rx-string-match-all (nelisp-rx-compile pattern)
                                               string)))
    (should (equal uncompiled compiled))
    (should (equal expected
                   (mapcar (lambda (match)
                             (list (plist-get match :start)
                                   (plist-get match :end)))
                           uncompiled)))
    (should (equal expected
                   (nelisp-regex-test--host-spans-all pattern string)))))

(ert-deftest nelisp-regex-test/scan-match-all-multiline-anchor-parity ()
  "Multiline BOL match-all preserves offsets and zero-width progression."
  (dolist (start '(nil 1))
    (let* ((pattern "^")
           (string "a\nb\n")
           (uncompiled (nelisp-rx-string-match-all pattern string start))
           (compiled (nelisp-rx-string-match-all (nelisp-rx-compile pattern)
                                                 string start))
           (expected (nelisp-regex-test--host-spans-all pattern string start)))
      (should (equal uncompiled compiled))
      (should (equal expected
                     (mapcar (lambda (match)
                               (list (plist-get match :start)
                                     (plist-get match :end)))
                             uncompiled))))))

(ert-deftest nelisp-regex-test/scan-anchored-candidates-match-gnu ()
  "Literal BOL/BOS starts preserve GNU Emacs match results and offsets."
  (dolist (case '(("^foo" "x\nfoo" nil)
                  ("^foo" "x\nfoo" 2)
                  ("^foo" "xfoo" nil)
                  ("\\`foo" "xfoo" nil)
                  ("\\`foo" "foo" nil)
                  ("\\`foo" "foo" 1)
                  ("\\`foo" "x\nfoo" nil)
                  ("^" "" nil)
                  ("^" "x\n" 1)
                  ("^foo" "あ\nfoo" 1)
                  ("" "abc" 1)))
    (pcase-let ((`(,pattern ,string ,start) case))
      (should (equal (nelisp-regex-test--native-span pattern string start)
                     (let ((pos (string-match pattern string start)))
                       (and pos (list pos (match-end 0)))))))))

(ert-deftest nelisp-regex-test/scan-only-skips-literal-anchor-positions ()
  "A literal start anchor avoids futile --match-from calls."
  (let ((calls 0)
        (original (symbol-function 'nelisp-rx--match-from)))
    (cl-letf (((symbol-function 'nelisp-rx--match-from)
               (lambda (&rest args)
                 (setq calls (1+ calls))
                 (apply original args))))
      (should-not (nelisp-rx-string-match "^z" "xxxxxxxx"))
      (should (= calls 1)))
    (setq calls 0)
    (cl-letf (((symbol-function 'nelisp-rx--match-from)
               (lambda (&rest args)
                 (setq calls (1+ calls))
                 (apply original args))))
      (should (nelisp-rx-string-match "^x" "a\nx"))
      (should (= calls 2)))))

(ert-deftest nelisp-regex-test/scan-bos-does-not-search-later-positions ()
  "String-begin anchors cannot match a later line or nonzero offset."
  (let ((calls 0)
        (original (symbol-function 'nelisp-rx--match-from)))
    (cl-letf (((symbol-function 'nelisp-rx--match-from)
               (lambda (&rest args)
                 (setq calls (1+ calls))
                 (apply original args))))
      (should-not (nelisp-rx-string-match "\\`z" "a\nz"))
      (should (= calls 1))
      (setq calls 0)
      (should-not (nelisp-rx-string-match "\\`a" "abc" 1))
      (should (= calls 0)))))

(ert-deftest nelisp-regex-test/scan-does-not-infer-through-epsilon ()
  "Alternation and escaped carets retain ordinary scan behavior."
  (let ((calls 0)
        (original (symbol-function 'nelisp-rx--match-from)))
    (cl-letf (((symbol-function 'nelisp-rx--match-from)
               (lambda (&rest args)
                 (setq calls (1+ calls))
                 (apply original args))))
      (should (nelisp-rx-string-match "\\(?:^foo\\|bar\\)" "xxbar"))
      (should (= calls 3)))
    (setq calls 0)
    (cl-letf (((symbol-function 'nelisp-rx--match-from)
               (lambda (&rest args)
                 (setq calls (1+ calls))
                 (apply original args))))
      (should (nelisp-rx-string-match "\\^foo" "xx^foo"))
      (should (= calls 3)))))

(provide 'nelisp-regex-test)

;;; nelisp-regex-test.el ends here
