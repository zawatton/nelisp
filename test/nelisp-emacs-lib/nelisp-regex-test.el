;;; nelisp-regex-test.el --- ERT tests for nelisp-regex word boundaries -*- lexical-binding: t; -*-

;;; Commentary:

;; Doc 51 Track J (2026-05-04) — extends `nelisp-regex' to handle the
;; directional word-boundary anchors `\<' (word start) and `\>' (word
;; end).  These are independent of the existing `\b' (any boundary)
;; and `\B' (no boundary) which are tested for regression.

;;; Code:

(require 'ert)
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

(ert-deftest nelisp-regex-test/public-api-smoke ()
  (let ((pattern (nelisp-rx-compile "\\<foo\\>")))
    (should (nelisp-rx-string-match pattern "say foo"))
    (should (= 2 (length (nelisp-rx-string-match-all "foo" "foo foo"))))
    (should (equal "bar foo" (nelisp-rx-replace "foo" "foo foo" "bar")))
    (should (equal "bar bar" (nelisp-rx-replace-all "foo" "foo foo" "bar")))))

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
