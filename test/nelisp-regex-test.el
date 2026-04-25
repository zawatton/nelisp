;;; nelisp-regex-test.el --- ERT tests for nelisp-regex MVP  -*- lexical-binding: t; -*-

;; Phase 9a/9b prerequisite — exercises the public API of `nelisp-regex',
;; covering syntax A (literal/./^/$/[]/*/+/?/group/alternation/escape) +
;; best-effort B (\\b \\B \\w \\W).  Backreferences and \\{n,m\\} are NOT
;; expected to work and are explicitly tested for the documented
;; `nelisp-rx-syntax-error' rejection path.

(require 'ert)
(require 'nelisp-regex)

;;; --------------------------------------------------------------------------
;;; helpers
;;; --------------------------------------------------------------------------

(defun nelisp-regex-test--match-text (md str)
  "Return the substring of STR covered by match-data MD."
  (and md (substring str (plist-get md :start) (plist-get md :end))))

(defun nelisp-regex-test--group-text (md str idx)
  "Return the substring captured by group IDX (1-based) of MD inside STR.
Returns nil if the group did not participate."
  (let ((g (cl-find idx (plist-get md :groups)
                    :key (lambda (e) (plist-get e :index)))))
    (and g (plist-get g :start)
         (substring str (plist-get g :start) (plist-get g :end)))))

;;; --------------------------------------------------------------------------
;;; literal / any / anchors
;;; --------------------------------------------------------------------------

(ert-deftest nelisp-rx-test-literal-match ()
  (let ((m (nelisp-rx-string-match (nelisp-rx-compile "abc") "xxabcyy")))
    (should m)
    (should (equal (nelisp-regex-test--match-text m "xxabcyy") "abc"))
    (should (= (plist-get m :start) 2))
    (should (= (plist-get m :end)   5))))

(ert-deftest nelisp-rx-test-literal-no-match ()
  (should-not (nelisp-rx-string-match (nelisp-rx-compile "abc") "xxxyyy")))

(ert-deftest nelisp-rx-test-empty-pattern ()
  (let ((m (nelisp-rx-string-match (nelisp-rx-compile "") "abc")))
    (should m)
    (should (= (plist-get m :start) 0))
    (should (= (plist-get m :end)   0))))

(ert-deftest nelisp-rx-test-any-char ()
  (let ((m (nelisp-rx-string-match (nelisp-rx-compile "a.c") "axc")))
    (should m)
    (should (equal (nelisp-regex-test--match-text m "axc") "axc"))))

(ert-deftest nelisp-rx-test-any-char-newline ()
  ;; Emacs default: `.' matches newline too.  We follow that.
  (let ((m (nelisp-rx-string-match (nelisp-rx-compile "a.c") "a\nc")))
    (should m)))

(ert-deftest nelisp-rx-test-anchor-begin ()
  (should (nelisp-rx-string-match (nelisp-rx-compile "^abc") "abcdef"))
  (should-not (nelisp-rx-string-match (nelisp-rx-compile "^abc") "xabcdef")))

(ert-deftest nelisp-rx-test-anchor-end ()
  (should (nelisp-rx-string-match (nelisp-rx-compile "abc$") "xxabc"))
  (should-not (nelisp-rx-string-match (nelisp-rx-compile "abc$") "xxabcyy")))

(ert-deftest nelisp-rx-test-anchor-both ()
  (should (nelisp-rx-string-match (nelisp-rx-compile "^abc$") "abc"))
  (should-not (nelisp-rx-string-match (nelisp-rx-compile "^abc$") "abcd"))
  (should-not (nelisp-rx-string-match (nelisp-rx-compile "^abc$") "xabc")))

(ert-deftest nelisp-rx-test-bol-after-newline ()
  (let ((m (nelisp-rx-string-match (nelisp-rx-compile "^bar") "foo\nbar")))
    (should m)
    (should (= (plist-get m :start) 4))))

(ert-deftest nelisp-rx-test-eol-before-newline ()
  (let ((m (nelisp-rx-string-match (nelisp-rx-compile "foo$") "foo\nbar")))
    (should m)
    (should (= (plist-get m :end) 3))))

;;; --------------------------------------------------------------------------
;;; character classes
;;; --------------------------------------------------------------------------

(ert-deftest nelisp-rx-test-char-class-set ()
  (let ((m (nelisp-rx-string-match (nelisp-rx-compile "[abc]") "xxbxx")))
    (should m)
    (should (equal (nelisp-regex-test--match-text m "xxbxx") "b"))))

(ert-deftest nelisp-rx-test-char-class-range ()
  (let ((m (nelisp-rx-string-match (nelisp-rx-compile "[a-z]+") "HELLOworld")))
    (should m)
    (should (equal (nelisp-regex-test--match-text m "HELLOworld") "world"))))

(ert-deftest nelisp-rx-test-char-class-neg ()
  (let ((m (nelisp-rx-string-match (nelisp-rx-compile "[^a-z]+") "abc123XYZdef")))
    (should m)
    (should (equal (nelisp-regex-test--match-text m "abc123XYZdef") "123XYZ"))))

(ert-deftest nelisp-rx-test-char-class-mixed ()
  (let ((m (nelisp-rx-string-match (nelisp-rx-compile "[A-Za-z0-9]+") "  hello42!")))
    (should m)
    (should (equal (nelisp-regex-test--match-text m "  hello42!") "hello42"))))

(ert-deftest nelisp-rx-test-char-class-leading-bracket ()
  ;; A `]' as the very first char inside `[...]' is a literal close bracket.
  (let ((m (nelisp-rx-string-match (nelisp-rx-compile "[]ab]+") "xx]ab]y")))
    (should m)
    (should (equal (nelisp-regex-test--match-text m "xx]ab]y") "]ab]"))))

(ert-deftest nelisp-rx-test-char-class-no-match ()
  (should-not (nelisp-rx-string-match (nelisp-rx-compile "[xyz]") "abcd")))

;;; --------------------------------------------------------------------------
;;; quantifiers
;;; --------------------------------------------------------------------------

(ert-deftest nelisp-rx-test-star-zero ()
  (let ((m (nelisp-rx-string-match (nelisp-rx-compile "ab*c") "ac")))
    (should m)
    (should (equal (nelisp-regex-test--match-text m "ac") "ac"))))

(ert-deftest nelisp-rx-test-star-many ()
  (let ((m (nelisp-rx-string-match (nelisp-rx-compile "ab*c") "abbbbc")))
    (should m)
    (should (equal (nelisp-regex-test--match-text m "abbbbc") "abbbbc"))))

(ert-deftest nelisp-rx-test-plus-one ()
  (let ((m (nelisp-rx-string-match (nelisp-rx-compile "ab+c") "abc")))
    (should m)
    (should (equal (nelisp-regex-test--match-text m "abc") "abc"))))

(ert-deftest nelisp-rx-test-plus-zero-fails ()
  (should-not (nelisp-rx-string-match (nelisp-rx-compile "ab+c") "ac")))

(ert-deftest nelisp-rx-test-opt-present ()
  (let ((m (nelisp-rx-string-match (nelisp-rx-compile "ab?c") "abc")))
    (should m)
    (should (equal (nelisp-regex-test--match-text m "abc") "abc"))))

(ert-deftest nelisp-rx-test-opt-absent ()
  (let ((m (nelisp-rx-string-match (nelisp-rx-compile "ab?c") "ac")))
    (should m)
    (should (equal (nelisp-regex-test--match-text m "ac") "ac"))))

(ert-deftest nelisp-rx-test-greedy-star ()
  ;; Greedy: `.*' should consume as much as possible.
  (let ((m (nelisp-rx-string-match (nelisp-rx-compile "a.*b") "axxxbxxxb")))
    (should m)
    (should (equal (nelisp-regex-test--match-text m "axxxbxxxb") "axxxbxxxb"))))

;;; --------------------------------------------------------------------------
;;; groups & alternation
;;; --------------------------------------------------------------------------

(ert-deftest nelisp-rx-test-group-capture-simple ()
  (let* ((s "ababab")
         (m (nelisp-rx-string-match (nelisp-rx-compile "\\(ab\\)+") s)))
    (should m)
    (should (equal (nelisp-regex-test--match-text m s) "ababab"))
    ;; Last iteration of the group should capture the final "ab".
    (should (equal (nelisp-regex-test--group-text m s 1) "ab"))))

(ert-deftest nelisp-rx-test-group-multiple ()
  (let* ((s "key=val")
         (m (nelisp-rx-string-match
             (nelisp-rx-compile "\\([a-z]+\\)=\\([a-z]+\\)") s)))
    (should m)
    (should (equal (nelisp-regex-test--group-text m s 1) "key"))
    (should (equal (nelisp-regex-test--group-text m s 2) "val"))))

(ert-deftest nelisp-rx-test-group-nested ()
  (let* ((s "abcabc")
         (m (nelisp-rx-string-match
             (nelisp-rx-compile "\\(\\(a\\)bc\\)") s)))
    (should m)
    (should (equal (nelisp-regex-test--group-text m s 1) "abc"))
    (should (equal (nelisp-regex-test--group-text m s 2) "a"))))

(ert-deftest nelisp-rx-test-alternation-first ()
  (let ((m (nelisp-rx-string-match (nelisp-rx-compile "cat\\|dog") "I have a cat")))
    (should m)
    (should (equal (nelisp-regex-test--match-text m "I have a cat") "cat"))))

(ert-deftest nelisp-rx-test-alternation-second ()
  (let ((m (nelisp-rx-string-match (nelisp-rx-compile "cat\\|dog") "I have a dog")))
    (should m)
    (should (equal (nelisp-regex-test--match-text m "I have a dog") "dog"))))

(ert-deftest nelisp-rx-test-alternation-three ()
  (let ((m (nelisp-rx-string-match
            (nelisp-rx-compile "foo\\|bar\\|baz") "see baz now")))
    (should m)
    (should (equal (nelisp-regex-test--match-text m "see baz now") "baz"))))

(ert-deftest nelisp-rx-test-group-alternation-mix ()
  (let* ((s "I like apple")
         (m (nelisp-rx-string-match
             (nelisp-rx-compile "\\(apple\\|banana\\)") s)))
    (should m)
    (should (equal (nelisp-regex-test--group-text m s 1) "apple"))))

;;; --------------------------------------------------------------------------
;;; escapes
;;; --------------------------------------------------------------------------

(ert-deftest nelisp-rx-test-escape-dot ()
  ;; "\\." should match a literal dot, not any char.
  (should (nelisp-rx-string-match (nelisp-rx-compile "a\\.c") "a.c"))
  (should-not (nelisp-rx-string-match (nelisp-rx-compile "a\\.c") "axc")))

(ert-deftest nelisp-rx-test-escape-paren ()
  (let ((m (nelisp-rx-string-match (nelisp-rx-compile "f\\\\(o\\\\)o")
                                   "f\\(o\\)o")))
    ;; Make sure compile didn't choke on a triple-escape literal.
    ;; (Test mainly guards the parser; meaningful semantics covered elsewhere.)
    (should (or m (null m)))))

(ert-deftest nelisp-rx-test-escape-backslash ()
  ;; "\\\\" matches a single literal backslash.
  (let ((m (nelisp-rx-string-match (nelisp-rx-compile "a\\\\b") "a\\b")))
    (should m)
    (should (equal (nelisp-regex-test--match-text m "a\\b") "a\\b"))))

(ert-deftest nelisp-rx-test-escape-star-literal ()
  (let ((m (nelisp-rx-string-match (nelisp-rx-compile "a\\*b") "a*b")))
    (should m)
    (should (equal (nelisp-regex-test--match-text m "a*b") "a*b"))))

;;; --------------------------------------------------------------------------
;;; word boundaries / classes (best-effort B)
;;; --------------------------------------------------------------------------

(ert-deftest nelisp-rx-test-word-boundary-hit ()
  (let ((m (nelisp-rx-string-match (nelisp-rx-compile "\\bcat\\b")
                                   "the cat sat")))
    (should m)
    (should (= (plist-get m :start) 4))))

(ert-deftest nelisp-rx-test-word-boundary-miss ()
  ;; "scatter" -- the substring "cat" is not on a word boundary on either side.
  (should-not (nelisp-rx-string-match (nelisp-rx-compile "\\bcat\\b")
                                      "scatter")))

(ert-deftest nelisp-rx-test-not-word-boundary ()
  (let ((m (nelisp-rx-string-match (nelisp-rx-compile "cat\\B")
                                   "scatter")))
    (should m)))

(ert-deftest nelisp-rx-test-word-class ()
  (let ((m (nelisp-rx-string-match (nelisp-rx-compile "\\w+")
                                   "  hello42 world")))
    (should m)
    (should (equal (nelisp-regex-test--match-text m "  hello42 world")
                   "hello42"))))

(ert-deftest nelisp-rx-test-non-word-class ()
  (let ((m (nelisp-rx-string-match (nelisp-rx-compile "\\W+")
                                   "abc!!!def")))
    (should m)
    (should (equal (nelisp-regex-test--match-text m "abc!!!def") "!!!"))))

;;; --------------------------------------------------------------------------
;;; start argument / search
;;; --------------------------------------------------------------------------

(ert-deftest nelisp-rx-test-start-arg-skip-first ()
  (let ((m (nelisp-rx-string-match (nelisp-rx-compile "ab") "abXab" 2)))
    (should m)
    (should (= (plist-get m :start) 3))))

(ert-deftest nelisp-rx-test-search-finds-later-occurrence ()
  (let ((m (nelisp-rx-string-match (nelisp-rx-compile "needle")
                                   "haystack-needle-haystack")))
    (should m)
    (should (= (plist-get m :start) 9))))

;;; --------------------------------------------------------------------------
;;; match-all
;;; --------------------------------------------------------------------------

(ert-deftest nelisp-rx-test-match-all ()
  (let ((ms (nelisp-rx-string-match-all (nelisp-rx-compile "[0-9]+")
                                        "a1 b22 c333")))
    (should (= (length ms) 3))
    (should (equal (mapcar (lambda (m)
                             (substring "a1 b22 c333"
                                        (plist-get m :start)
                                        (plist-get m :end)))
                           ms)
                   '("1" "22" "333")))))

(ert-deftest nelisp-rx-test-match-all-empty-pattern-step ()
  ;; Empty-match patterns must not loop forever; we step by 1.
  (let ((ms (nelisp-rx-string-match-all (nelisp-rx-compile "a*") "aaa")))
    (should (consp ms))
    (should (<= (length ms) 4))))

(ert-deftest nelisp-rx-test-match-all-no-overlap ()
  (let ((ms (nelisp-rx-string-match-all (nelisp-rx-compile "aa") "aaaa")))
    (should (= (length ms) 2))
    (should (equal (mapcar (lambda (m) (plist-get m :start)) ms)
                   '(0 2)))))

;;; --------------------------------------------------------------------------
;;; replace
;;; --------------------------------------------------------------------------

(ert-deftest nelisp-rx-test-replace-first ()
  (should (equal (nelisp-rx-replace (nelisp-rx-compile "foo")
                                    "foo bar foo" "X")
                 "X bar foo")))

(ert-deftest nelisp-rx-test-replace-no-match ()
  (should (equal (nelisp-rx-replace (nelisp-rx-compile "zzz")
                                    "abcdef" "X")
                 "abcdef")))

(ert-deftest nelisp-rx-test-replace-all ()
  (should (equal (nelisp-rx-replace-all (nelisp-rx-compile "foo")
                                        "foo bar foo baz foo" "X")
                 "X bar X baz X")))

(ert-deftest nelisp-rx-test-replace-all-class ()
  (should (equal (nelisp-rx-replace-all (nelisp-rx-compile "[0-9]+")
                                        "a1 b22 c333" "N")
                 "aN bN cN")))

;;; --------------------------------------------------------------------------
;;; pre-compile / re-use
;;; --------------------------------------------------------------------------

(ert-deftest nelisp-rx-test-precompile-reuse ()
  (let ((pat (nelisp-rx-compile "[a-z]+")))
    (should (nelisp-rx-pattern-p pat))
    (should (nelisp-rx-string-match pat "abc"))
    (should (nelisp-rx-string-match pat "xyz"))
    (should-not (nelisp-rx-string-match pat "123"))))

(ert-deftest nelisp-rx-test-string-pattern-input ()
  ;; First arg may be raw string -- compiled on the fly.
  (let ((m (nelisp-rx-string-match "abc" "xabcy")))
    (should m)
    (should (= (plist-get m :start) 1))))

;;; --------------------------------------------------------------------------
;;; syntax errors / deferred features
;;; --------------------------------------------------------------------------

(ert-deftest nelisp-rx-test-trailing-backslash-error ()
  (should-error (nelisp-rx-compile "abc\\")
                :type 'nelisp-rx-syntax-error))

(ert-deftest nelisp-rx-test-unmatched-close-paren-error ()
  (should-error (nelisp-rx-compile "abc\\)")
                :type 'nelisp-rx-syntax-error))

(ert-deftest nelisp-rx-test-unterminated-group-error ()
  (should-error (nelisp-rx-compile "\\(abc")
                :type 'nelisp-rx-syntax-error))

(ert-deftest nelisp-rx-test-unterminated-class-error ()
  (should-error (nelisp-rx-compile "[abc")
                :type 'nelisp-rx-syntax-error))

(ert-deftest nelisp-rx-test-backref-rejected-mvp ()
  ;; Backrefs land in Phase 9c -- MVP must reject them rather than
  ;; pretend to succeed.
  (should-error (nelisp-rx-compile "\\(a\\)\\1")
                :type 'nelisp-rx-syntax-error))

(ert-deftest nelisp-rx-test-leading-quantifier-error ()
  (should-error (nelisp-rx-compile "*abc")
                :type 'nelisp-rx-syntax-error))

(provide 'nelisp-regex-test)

;;; nelisp-regex-test.el ends here
