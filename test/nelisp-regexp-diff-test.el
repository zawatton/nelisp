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
               ("/a/b/c" "/" nil) ("/a/b/c" "/" t) ("x1y2z" "[0-9]" nil)
               ;; SEPARATORS nil: `nlre--split-on-whitespace' fast path
               ;; (Doc 205 split-string follow-up) -- no separator at all,
               ;; leading/trailing-only runs, mixed whitespace kinds, and a
               ;; long non-whitespace run before the first separator (the
               ;; secure-hash-discovered shape: "<hex>  /path\n").
               ("nosep" nil nil) ("   " nil nil)
               ("   lead" nil nil) ("trail   " nil nil)
               ("a\f\t\n\r\vb" nil nil)))
    (should (equal (split-string (nth 0 c) (nth 1 c) (nth 2 c))
                   (nlre-split-string (nth 0 c) (nth 1 c) (nth 2 c)))))
  (let ((long-run (concat (make-string 96 ?a) "  " "/tmp/some/long/path" "\n")))
    (should (equal (split-string long-run) (nlre-split-string long-run)))))

(ert-deftest nelisp-regexp-split-string-nil-separators-match-data ()
  "SEPARATORS nil leaves match data at the LAST separator run consumed,
same as host `split-string' via its own repeated `string-match' calls;
when no separator is found, neither touches match data at all."
  (dolist (s '("abc def" "  lead trail  " "one" "" "a b" "a\nb\nc  " "   "))
    (let (host-md nl-md)
      (save-match-data
        (split-string s)
        (setq host-md (and (match-beginning 0)
                            (cons (match-beginning 0) (match-end 0)))))
      (save-match-data
        (nlre-split-string s nil nil)
        (setq nl-md (and (nlre-match-beginning 0)
                          (cons (nlre-match-beginning 0) (nlre-match-end 0)))))
      ;; Only assert when a real separator was consumed; with none, host's
      ;; match data is leftover global state from something unrelated, not
      ;; a value `split-string' itself set, so it is not a parity target.
      (when (string-match "[ \f\t\n\r\v]" s)
        (should (equal host-md nl-md))))))

(ert-deftest nelisp-regexp-leading-required-atom-matches-host ()
  "A leading `:plus'/`:lazyplus' of a single fixed-width atom (Doc 205
split-string follow-up's `nlre--leading-required-atom' filter) must still
agree with host `string-match' on start, end, and captures, including
inputs with a long run of characters that must all be rejected before the
first real match -- the case the filter exists to speed up."
  (dolist (case (list
                 (list "[ \t\n]+" "abc def")
                 (list "[ \t\n]+" (concat (make-string 64 ?a) "  tail"))
                 (list "[0-9]+" "abc123def")
                 (list "[a-z]+" "123abc456")
                 (list "\\sw+" "  word  ")
                 (list "\\s-+" "a   b")
                 (list "a+" "bbbaaab")
                 (list "\\(a+\\)b" "xxxaaab")))
    (let* ((regexp (nth 0 case)) (string (nth 1 case))
           (reference (string-match regexp string))
           (reference-data (and reference (match-data)))
           (actual (nlre-string-match regexp string)))
      (should (equal actual reference))
      (when reference
        (should (equal (nlre-match-beginning 0) (nth 0 reference-data)))
        (should (equal (nlre-match-end 0) (nth 1 reference-data)))))))

(ert-deftest nelisp-regexp-replace-matches-host ()
  "nlre-replace-regexp-in-string must agree with host on string REP."
  (dolist (c '(("a" "X" "banana") ("[0-9]+" "#" "a12b345c") ("o" "0" "foobar")))
    (should (equal (replace-regexp-in-string (nth 0 c) (nth 1 c) (nth 2 c))
                   (nlre-replace-regexp-in-string (nth 0 c) (nth 1 c) (nth 2 c))))))

(ert-deftest nelisp-regexp-compiled-cache-reuses-exact-key ()
  "A repeated pattern/fold/syntax-table triple is one cache entry."
  (let ((case-fold-search nil))
    (nlre--compiled-cache-clear)
    (setq nlre--compiled-cache-hits 0
          nlre--compiled-cache-misses 0)
    (let ((first (nlre--compiled-pattern "cache-literal"))
          (second (nlre--compiled-pattern "cache-literal")))
      (should (eq first second))
      (should (= nlre--compiled-cache-count 1))
      (should (= nlre--compiled-cache-hits 1))
      (should (= nlre--compiled-cache-misses 1)))))

(ert-deftest nelisp-regexp-compiled-cache-separates-fold-and-syntax-table ()
  "Case-fold values and syntax-table identities occupy separate entries."
  (nlre--compiled-cache-clear)
  (setq nlre--compiled-cache-hits 0
        nlre--compiled-cache-misses 0)
  (let ((case-fold-search nil))
    (nlre--compiled-pattern "cache-key"))
  (let ((case-fold-search t))
    (nlre--compiled-pattern "cache-key"))
  (let ((case-fold-search nil))
    (with-syntax-table (copy-syntax-table)
      (nlre--compiled-pattern "cache-key")))
  (should (= nlre--compiled-cache-count 3))
  (should (= nlre--compiled-cache-hits 0))
  (should (= nlre--compiled-cache-misses 3)))

(ert-deftest nelisp-regexp-compiled-cache-evicts-least-recently-used ()
  "The bounded cache evicts the LRU entry rather than clearing everything."
  (let ((case-fold-search nil)
        (nlre--compiled-cache-limit 3))
    (nlre--compiled-cache-clear)
    (setq nlre--compiled-cache-hits 0
          nlre--compiled-cache-misses 0)
    (nlre--compiled-pattern "cache-0")
    (nlre--compiled-pattern "cache-1")
    (nlre--compiled-pattern "cache-2")
    (nlre--compiled-pattern "cache-0") ; promote; cache-1 is now LRU
    (nlre--compiled-pattern "cache-3")
    (should (= nlre--compiled-cache-count 3))
    (should (= nlre--compiled-cache-hits 1))
    (nlre--compiled-pattern "cache-1")
    (should (= nlre--compiled-cache-misses 5))
    ;; cache-0 survived both evictions because it was touched before cache-3.
    (nlre--compiled-pattern "cache-0")
    (should (= nlre--compiled-cache-hits 2))))

(ert-deftest nelisp-regexp-finite-prefilter-preserves-edge-cases ()
  "Finite fast plans must not skip empty/anchored/folded valid matches."
  (dolist (case '(("a\\|" "bbb")
                  ("\\`foo\\|bar\\'" "xxbar")
                  ("[^x]foo" "xxyfoo")
                  ("a.*?b" "zaxxbxxb")
                  ("\\(a\\|ba\\)\\'" "xxba")
                  ("\\(za\\|a\\)\\'" "xxa")))
    (let* ((regexp (nth 0 case))
           (string (nth 1 case))
           (reference (string-match regexp string))
           (reference-end (and reference (match-end 0)))
           (actual (nlre-string-match regexp string)))
      (should (equal actual reference))
      (should (equal (and actual (nlre-match-end 0)) reference-end))))
  (let ((case-fold-search t))
    (should (= (nlre-string-match "ALPHA" "xxalpha") 2))
    (should (= (nlre-string-match "\\(A0\\|B0\\)\\'" "xxb0") 2))))

(ert-deftest nelisp-regexp-finite-prefilter-load-history-and-regexp-opt ()
  "The two hot finite-pattern shapes retain match and capture semantics."
  (let* ((load-re
          "\\(\\`\\|/\\)org\\(\\.elc\\|\\.el\\|\\.so\\|\\)\\(\\.gz\\)?\\'")
         (load-string "/usr/share/emacs/lisp/org.elc.gz")
         (extensions nil)
         (i 0))
    (while (< i 40)
      (setq extensions (cons (format "mode%02d" i) extensions)
            i (1+ i)))
    (let ((auto-re (concat "\\." (regexp-opt (nreverse extensions) t) "\\'")))
      (dolist (case (list (list load-re load-string)
                          (list auto-re "/tmp/example.mode39")))
        (let* ((regexp (nth 0 case))
               (string (nth 1 case))
               (reference (string-match regexp string))
               (reference-data (and reference (match-data)))
               (actual (nlre-string-match regexp string)))
          (should (aref (nlre--compiled-pattern regexp) 2))
          (should (equal actual reference))
          (should (= (length nlre--last-caps) (/ (length reference-data) 2)))
          ;; These shapes have at most three subexpressions.  Keep the
          ;; assertions explicit: this also leaves `nlre--last-caps' live
          ;; while checking it, just as the public match accessors do.
          (should (equal (nlre-match-beginning 0) (nth 0 reference-data)))
          (should (equal (nlre-match-end 0) (nth 1 reference-data)))
          (should (equal (nlre-match-beginning 1) (nth 2 reference-data)))
          (should (equal (nlre-match-end 1) (nth 3 reference-data)))
          (when (> (length nlre--last-caps) 2)
            (should (equal (nlre-match-beginning 2) (nth 4 reference-data)))
            (should (equal (nlre-match-end 2) (nth 5 reference-data))))
          (when (> (length nlre--last-caps) 3)
            (should (equal (nlre-match-beginning 3) (nth 6 reference-data)))
            (should (equal (nlre-match-end 3) (nth 7 reference-data)))))))))

(ert-deftest nelisp-regexp-finite-prefilter-bailout-classes-still-match ()
  "Patterns the finite-plan builder must decline still match correctly
via the backtracking slow path (Doc T48d)."
  ;; A mid-pattern anchor other than a leading `\\=\\`' or trailing `\\='':
  ;; `nlre--plan-expand' has no case for `:bol'/`:eol' and falls through
  ;; to its `t' clause, which fails the plan rather than mis-modeling it.
  (let* ((regexp "x\\(^y\\|z\\)\\'") (string "xz")
         (reference (string-match regexp string))
         (reference-data (match-data))
         (actual (nlre-string-match regexp string)))
    (should (null (aref (nlre--compiled-pattern regexp) 2)))
    (should (equal actual reference))
    (should (equal (nlre-match-beginning 0) (nth 0 reference-data)))
    (should (equal (nlre-match-end 0) (nth 1 reference-data)))
    (should (equal (nlre-match-beginning 1) (nth 2 reference-data)))
    (should (equal (nlre-match-end 1) (nth 3 reference-data))))
  ;; More alternation leaves than `nlre--plan-limit' allows: the builder
  ;; must give up once its atom-instance budget is spent instead of
  ;; finishing an unbounded cross-product.
  (let* ((branches nil) (i 0))
    (while (< i 300)
      (setq branches (cons (format "w%03d" i) branches) i (1+ i)))
    (let* ((regexp (concat "\\(" (mapconcat #'identity (nreverse branches) "\\|") "\\)\\'"))
           (string "xxw299")
           (reference (string-match regexp string))
           (reference-data (match-data))
           (actual (nlre-string-match regexp string)))
      (should (null (aref (nlre--compiled-pattern regexp) 2)))
      (should (equal actual reference))
      (should (equal (nlre-match-beginning 0) (nth 0 reference-data)))
      (should (equal (nlre-match-end 0) (nth 1 reference-data)))
      (should (equal (nlre-match-beginning 1) (nth 2 reference-data)))
      (should (equal (nlre-match-end 1) (nth 3 reference-data))))))

(ert-deftest nelisp-regexp-buffer-search-wrappers ()
  "Buffer searches share the string matcher and report buffer positions."
  (with-temp-buffer
    (insert "xxneedlezz")
    (goto-char 3)
    (should (nlre--looking-at "needle"))
    (should (= (nlre-match-beginning 0) 3))
    (should (= (nlre-match-end 0) 9))
    (goto-char 1)
    (should (= (nlre--re-search-forward "needle" nil nil) 9))
    (should (= (point) 9))
    (should (= (nlre-match-beginning 0) 3))))

(ert-deftest nelisp-regexp-single-atom-fast-plan-matches-host ()
  "A bare, quantifier-free, group-free single atom takes the `:atom' fast
plan and still agrees with host `string-match' on start and end."
  (dolist (case '(("[0-9]" "abc1" nil) ("[0-9]" "abc" nil) ("[^0-9]" "1a" nil)
                  ("." "x" nil) ("." "" nil) ("[a-z]" "9z" nil)
                  ("\\w" "  a1" nil) ("\\W" "ab!c" nil)
                  ("\\s-" "x y" nil) ("\\S-" "  x" nil)
                  ("[0-9]" "ABC1" t) ("[a-z]" "XYZ" t)))
    (let* ((regexp (nth 0 case)) (string (nth 1 case))
           (case-fold-search (nth 2 case))
           (reference (string-match regexp string))
           (reference-end (and reference (match-end 0)))
           (compiled (nlre--compiled-pattern regexp))
           (plan (aref compiled 2))
           (actual (nlre-string-match regexp string)))
      (should (and plan (eq (aref plan 0) :atom)))
      (should (equal actual reference))
      (should (equal (and actual (nlre-match-end 0)) reference-end)))))

(ert-deftest nelisp-regexp-single-atom-fast-plan-does-not-overreach ()
  "Groups, quantifiers, anchors and multi-atom patterns must NOT take the
single-atom fast plan -- it only ever applies to exactly one fixed-width,
capture-free, quantifier-free atom."
  (dolist (regexp '("\\(a\\)" "a*" "a+" "a?" "^" "$" "ab" "a\\|b" "[0-9]+"))
    (let ((plan (aref (nlre--compiled-pattern regexp) 2)))
      (should (not (and plan (eq (aref plan 0) :atom)))))))

(ert-deftest nelisp-regexp-single-char-literal-fold-matches-host ()
  "A one-character literal needle under `case-fold-search' must find the
same leftmost position as host Emacs, whether it needs no case flip (same
char), only the flipped case, or no match exists at all."
  (dolist (case '(("b" "abc" t) ("B" "abc" t) ("b" "ABC" t) ("Z" "xyz" t)
                  ("q" "xyz" t) ("b" "abc" nil) ("B" "abc" nil)
                  ("5" "abc123" t) ("5" "abc123" nil)))
    (let* ((regexp (nth 0 case)) (string (nth 1 case))
           (case-fold-search (nth 2 case))
           (reference (string-match regexp string))
           (reference-end (and reference (match-end 0)))
           (compiled (nlre--compiled-pattern regexp))
           (plan (aref compiled 2))
           (actual (nlre-string-match regexp string)))
      (should (and plan (eq (aref plan 0) :literal)))
      (should (equal actual reference))
      (should (equal (and actual (nlre-match-end 0)) reference-end)))))

(ert-deftest nelisp-regexp-match-end-ceiling-keeps-anchor-context ()
  "A match ceiling bounds consumption without becoming a new string end."
  (should (= (nlre-string-match "a.*" "axxx\n" 0 2) 0))
  (should (= (nlre-match-end 0) 2))
  (should (equal (substring "axxx\n" (nlre-match-beginning 0)
                            (nlre-match-end 0))
                 "ax"))
  ;; `^' and `$', unlike the ceiling, refer to the actual full string.
  (should (= (nlre-string-match "^" "axxx\n" 0 2) 0))
  (should (null (nlre-string-match "^" "axxx\n" 2 2)))
  (should (null (nlre-string-match "$" "axxx\n" 0 2)))
  (should (= (nlre-string-match "$" "axxx\n" 0 4) 4))
  (should (null (nlre-string-match "\\='" "axxx\n" 0 4))))

(ert-deftest nelisp-regexp-match-end-ceiling-backtracks-and-captures ()
  "The end ceiling applies through greedy backtracking and captures."
  (should (= (nlre-string-match "\\(a.*\\)" "axxx\n" 0 2) 0))
  (should (equal (cons (nlre-match-beginning 1) (nlre-match-end 1))
                 (cons 0 2)))
  (should (= (nlre-string-match "a$" "a\nx" 0 1) 0))
  (should (= (nlre-match-end 0) 1)))

(ert-deftest nelisp-regexp-match-end-ceiling-bypasses-unbounded-fast-plans ()
  "Literal and suffix plans respect the end ceiling and full-string anchors."
  (should (= (nlre-string-match "ax" "axxx" 0 2) 0))
  (should (= (nlre-match-end 0) 2))
  (should (null (nlre-string-match "a\\='" "axxx" 0 2)))
  (should (= (nlre-string-match "\\`a" "axxx" 0 2) 0)))

(ert-deftest nelisp-regexp-re-search-backward-bounds-greedy-match-at-origin ()
  "Backward search chooses the rightmost start and caps greedy matches."
  (with-temp-buffer
    (insert "axxx\n")
    (goto-char 3)
    (should (= (nlre--re-search-backward "a.*" nil nil) 1))
    (should (= (point) 1))
    (should (equal (buffer-substring (nlre-match-beginning 0)
                                     (nlre-match-end 0))
                   "ax"))
    (should (= (nlre-match-end 0) 3))))

(ert-deftest nelisp-regexp-re-search-backward-preserves-line-anchor-context ()
  "Backward anchors use buffer context, not the starting point as EOL/BOL."
  (with-temp-buffer
    (insert "prev\naxxx\n")
    (goto-char 7)
    (should (= (nlre--re-search-backward "^" nil nil) 6))
    (should (= (nlre-match-beginning 0) 6))
    (goto-char 8)
    (should (= (nlre--re-search-backward "$" nil nil) 5))
    (should (= (nlre-match-beginning 0) 5))
    (should (= (point) 5))))

(ert-deftest nelisp-regexp-re-search-backward-count-bound-and-noerror ()
  "Backward count and bound follow GNU point and failure behavior."
  (with-temp-buffer
    (insert "aXaXa")
    (goto-char 6)
    (should (= (nlre--re-search-backward "a" nil nil 2) 3))
    (should (equal (buffer-substring (nlre-match-beginning 0)
                                     (nlre-match-end 0))
                   "a"))
    (goto-char 6)
    (should (null (nlre--re-search-backward "a" 4 t 2)))
    (should (= (point) 6))
    (should (= (nlre--re-search-backward "a" 4 nil) 5))
    (goto-char 6)
    (should (null (nlre--re-search-backward "z" nil t)))
    (should (= (point) 6))
    (should (null (nlre--re-search-backward "z" 2 1)))
    (should (= (point) 2))
    (should-error (nlre--re-search-backward "z" nil nil) :type 'search-failed)))

(ert-deftest nelisp-regexp-re-search-backward-negative-count-searches-forward ()
  "A negative COUNT reverses direction as in GNU Emacs."
  (with-temp-buffer
    (insert "aXaXa")
    (goto-char 1)
    (should (= (nlre--re-search-backward "a" nil nil -2) 4))
    (should (= (point) 4))
    (should (= (nlre-match-beginning 0) 3))))

(ert-deftest nelisp-regexp-re-search-backward-agrees-with-host ()
  "Representative backward searches agree with GNU Emacs point and captures."
  (dolist (case '(("axxx\n" "a.*" 3 nil nil nil)
                  ("prev\naxxx\n" "^" 7 nil nil nil)
                  ("prev\naxxx\n" "$" 8 nil nil nil)
                  ("aXaXa" "a" 6 nil nil 2)
                  ("aXaXa" "a" 6 4 t 2)
                  ("aXaXa" "a" 6 4 nil nil)
                  ("aXaXa" "z" 6 2 1 nil)
                  ("a" "z" 2 nil nil 0)
                  ("a" "z" 2 nil t 0)
                  ("aXaXa" "a" 1 nil nil -2)))
    (let* ((text (nth 0 case)) (regexp (nth 1 case))
           (origin (nth 2 case)) (bound (nth 3 case))
           (noerror (nth 4 case)) (count (nth 5 case))
           (matched (not (= (or count 1) 0)))
           (host
            (with-temp-buffer
              (insert text)
              (goto-char origin)
              (let ((result (re-search-backward regexp bound noerror count)))
                (list result (point)
                      (and matched result (match-beginning 0))
                      (and matched result (match-end 0))
                      (and matched result (match-string 0))))))
           (mine
            (with-temp-buffer
              (insert text)
              (goto-char origin)
              (let ((result (nlre--re-search-backward regexp bound noerror count)))
                (list result (point)
                      (and matched result (nlre-match-beginning 0))
                      (and matched result (nlre-match-end 0))
                      (and matched result
                           (buffer-substring (nlre-match-beginning 0)
                                             (nlre-match-end 0)))))))
      (should (equal mine host))))))

(ert-deftest nelisp-regexp-re-search-backward-zero-count-preserves-state ()
  "COUNT zero returns point without searching or replacing match data."
  (let ((nlre--last-caps 'saved-match-data))
    (with-temp-buffer
      (insert "a")
      (goto-char 2)
      (should (= (nlre--re-search-backward "z" nil nil 0) 2))
      (should (= (point) 2))
      (should (eq nlre--last-caps 'saved-match-data))
      (should (= (nlre--re-search-backward "z" nil t 0) 2))
      (should (= (point) 2))
      (should (eq nlre--last-caps 'saved-match-data)))))

(provide 'nelisp-regexp-diff-test)
;;; nelisp-regexp-diff-test.el ends here
