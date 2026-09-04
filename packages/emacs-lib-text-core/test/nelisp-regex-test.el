;;; nelisp-regex-test.el --- ERT tests for nelisp-regex word boundaries -*- lexical-binding: t; -*-

;;; Commentary:

;; Doc 51 Track J (2026-05-04) — extends `nelisp-regex' to handle the
;; directional word-boundary anchors `\<' (word start) and `\>' (word
;; end).  These are independent of the existing `\b' (any boundary)
;; and `\B' (no boundary) which are tested for regression.

;;; Code:

(require 'ert)
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

(provide 'nelisp-regex-test)

;;; nelisp-regex-test.el ends here
