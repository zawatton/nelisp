;;; nelisp-regexp-diff-test.el --- differential test vs host string-match -*- lexical-binding: t; -*-

;; Doc 143: verify the pure-elisp regexp matcher (nelisp-stdlib-regexp.el)
;; against the REAL GNU Emacs `string-match' across a corpus of patterns.
;; Runs in host Emacs (the matcher is pure elisp).  This is the safety gate
;; that lets the matcher be wired into the reader prelude as `string-match'
;; without risking silent mis-matches.

;;; Code:

(require 'ert)
(require 'nelisp-stdlib-regexp
         (expand-file-name "../lisp/nelisp-stdlib-regexp.el"
                           (file-name-directory (or load-file-name buffer-file-name))))

(defconst nelisp-regexp-diff-cases
  '(;; literals / any / anchors
    ("abc" "xxabcyy") ("a.c" "xaqcy") ("a.c" "xabcy")
    ("^abc" "abc") ("^abc" "xabc") ("abc$" "xabc") ("abc$" "abcx") ("^$" "")
    ;; quantifiers
    ("ab*c" "ac") ("ab*c" "abbbc") ("ab+c" "ac") ("ab+c" "abc")
    ("ab?c" "ac") ("ab?c" "abc") ("a*" "") ("x*" "yyy") (".*" "anything") ("a.*z" "aXXz")
    ;; char classes
    ("[abc]+" "xxbcacyy") ("[a-z]+" "12abc34") ("[^0-9]+" "abc123")
    ("[A-Za-z_][A-Za-z0-9_]*" "_foo9 bar") ("[][]" "a]b") ("[.]el$" "x.el")
    ;; groups / alternation
    ("\\(ab\\)+" "ababx") ("a\\|b" "xby") ("foo\\|bar" "zzbarzz")
    ("\\(foo\\)\\(bar\\)" "xfoobary") ("a\\(b\\|c\\)d" "xacdy")
    ("\\(a+\\)\\(b+\\)" "xaaabbby") ("\\(foo\\)bar\\(baz\\)" "foobarbaz")
    ("\\([a-z]+\\)-\\([0-9]+\\)" "item-42") ("a\\(b\\(c\\)d\\)e" "abcde")
    ("\\(x\\|y\\)\\(z\\)" "yz") ("\\(\\)" "abc") ("\\(ab\\)*" "ababab")
    ;; escapes / classes
    ("\\w+" "  hello42 ") ("\\.el" "foo.el") ("a\\.b" "a.b") ("a\\.b" "axb")
    ;; braces
    ("ab\\{0\\}" "ab") ("ab\\{2\\}" "abbc") ("ab\\{2\\}" "abc") ("ab\\{2,3\\}" "abbbb")
    ("ab\\{1,\\}" "abbb") ("a\\{3\\}" "aaaa") ("[0-9]\\{2,4\\}" "12345")
    ("\\(ab\\)\\{2\\}" "abab") ("x\\{0,2\\}y" "xxy") ("x\\{0,2\\}y" "y")
    ;; misc
    ("colou?r" "color") ("colou?r" "colour") ("end$" "the end")))

(ert-deftest nelisp-regexp-matches-host-string-match ()
  "Every case must agree with host `string-match' on start index + group spans."
  (dolist (cs nelisp-regexp-diff-cases)
    (let* ((p (car cs)) (s (cadr cs))
           (ref (string-match p s))
           (ref-b (and ref (match-beginning 0))) (ref-e (and ref (match-end 0)))
           (ref-1b (and ref (ignore-errors (match-beginning 1))))
           (ref-1e (and ref (ignore-errors (match-end 1))))
           (mine (nlre-string-match p s))
           (mine-b (nlre-match-beginning 0)) (mine-e (nlre-match-end 0))
           (mine-1b (nlre-match-beginning 1)) (mine-1e (nlre-match-end 1)))
      (should (equal ref mine))
      (when ref
        (should (equal ref-b mine-b))
        (should (equal ref-e mine-e))
        (should (equal ref-1b mine-1b))
        (should (equal ref-1e mine-1e))))))

(ert-deftest nelisp-regexp-split-string-matches-host ()
  "nlre-split-string must agree with host `split-string'."
  (dolist (c '(("a b  c" nil nil) ("a,b,c" "," nil) ("  x y  " nil nil)
               ("a,,b" "," nil) ("a,,b" "," t) ("" nil nil)
               ("/a/b/c" "/" nil) ("/a/b/c" "/" t) ("x1y2z" "[0-9]" nil)))
    (should (equal (split-string (nth 0 c) (nth 1 c) (nth 2 c))
                   (nlre-split-string (nth 0 c) (nth 1 c) (nth 2 c))))))

(ert-deftest nelisp-regexp-replace-matches-host ()
  "nlre-replace-regexp-in-string must agree with host on string REP."
  (dolist (c '(("a" "X" "banana") ("[0-9]+" "#" "a12b345c") ("o" "0" "foobar")))
    (should (equal (replace-regexp-in-string (nth 0 c) (nth 1 c) (nth 2 c))
                   (nlre-replace-regexp-in-string (nth 0 c) (nth 1 c) (nth 2 c))))))

(provide 'nelisp-regexp-diff-test)
;;; nelisp-regexp-diff-test.el ends here
