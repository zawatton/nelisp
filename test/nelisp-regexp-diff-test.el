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

(ert-deftest nelisp-regexp-rejects-impossible-default-fold-path-before-backtracking ()
  "A repeated path component cannot consume a second slash."
  (let ((case-fold-search t))
    (cl-letf (((symbol-function 'nlre--current-translate-table) (lambda () nil))
              ((symbol-function 'nlre--match-list)
               (lambda (&rest _) (error "entered the backtracker"))))
      (nlre--compiled-cache-clear)
      (should-not (nlre-string-match "\\`/[[:alnum:]]*\\'" "/home/user/file.el")))))

(ert-deftest nelisp-regexp-ascii-class-bitmap-ignores-unicode-property-edits ()
  "GNU's ASCII class bitmap does not consult mutable Unicode categories."
  (let ((old (get-char-code-property ?/ 'general-category))
        (case-fold-search nil))
    (unwind-protect
        (progn
          (put-char-code-property ?/ 'general-category 'Lu)
          (dolist (pattern '("[[:alpha:]]" "[[:alnum:]]"))
            (should (equal (nlre-string-match pattern "/") (string-match pattern "/")))))
      (put-char-code-property ?/ 'general-category old))))

(ert-deftest nelisp-regexp-portable-prefilter-preserves-live-classes ()
  "A cached default-fold rejection must observe changed POSIX membership."
  (with-temp-buffer
    (set-syntax-table (copy-syntax-table))
    (modify-syntax-entry ?/ ".")
    (let ((case-fold-search t)
          (pattern "\\`/[[:word:]]*\\'")
          (text "/aa/a"))
      (cl-letf (((symbol-function 'nlre--current-translate-table) (lambda () nil)))
        (nlre--compiled-cache-clear)
        (should-not (nlre-string-match pattern text))
        (let ((compiled (nlre--compiled-pattern pattern)))
          (modify-syntax-entry ?/ "w")
          (should (eq compiled (nlre--compiled-pattern pattern)))
          (should (equal (nlre-string-match pattern text) (string-match pattern text))))))))

(ert-deftest nelisp-regexp-portable-ascii-sets-match-gnu ()
  "Exercise every ASCII byte through cached immutable-class plans."
  (cl-letf (((symbol-function 'nlre--current-translate-table) (lambda () nil)))
    (dolist (fold '(nil t))
      (let ((case-fold-search fold))
        (dolist (class '("alpha" "alnum" "digit" "xdigit" "ascii"
                         "blank" "cntrl" "graph" "print"))
          (dolist (neg '("" "^"))
            (let ((pattern (concat "\\`[" neg "[:" class ":]]\\'")))
              (dotimes (c 128)
                (let ((text (char-to-string c)))
                  (should (equal (list class neg fold c (nlre-string-match pattern text))
                                 (list class neg fold c (string-match pattern text)))))))))))))

(ert-deftest nelisp-regexp-portable-prefilter-follows-canonical-table ()
  "Installing a table after compilation must disable default-fold proofs."
  (let ((case-fold-search t)
        (pattern "\\`/:\\'")
        (table (make-vector 128 nil)) active-table)
    (cl-letf (((symbol-function 'nlre--current-translate-table) (lambda () active-table)))
      (nlre--compiled-cache-clear)
      (should-not (nlre-string-match pattern "/x"))
      (aset table ?x ?:)
      (setq active-table table)
      (should (eq (nlre-string-match pattern "/x") 0)))))

(ert-deftest nelisp-regexp-invalid-pattern-error-data-matches-host ()
  "Malformed patterns signal GNU's complete condition rather than matching."
  (dolist (pattern '("[" "[^" "[a" "\\(" "\\)" "\\" "\\1"))
    (let ((expected (condition-case e (string-match pattern "a") (error e)))
          (actual (condition-case e (nlre-string-match pattern "a") (error e))))
      (should (equal actual expected)))))

(ert-deftest nelisp-regexp-subexpression-error-data-matches-host ()
  "Invalid subexpression indices preserve GNU's condition and data."
  (string-match "x" "x")
  (nlre-string-match "x" "x")
  (dolist (index '(-1 1.5 bad 3))
    (dolist (pair '((match-beginning . nlre-match-beginning)
                    (match-end . nlre-match-end)))
      (let ((expected (condition-case e (funcall (car pair) index) (error e)))
            (actual (condition-case e (funcall (cdr pair) index) (error e))))
        (should (equal actual expected))))))

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

(ert-deftest nelisp-regexp-compiled-cache-separates-fold-not-irrelevant-syntax-table ()
  "Fold values differ, while literal patterns ignore syntax-table identity."
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
  (should (= nlre--compiled-cache-count 2))
  (should (= nlre--compiled-cache-hits 1))
  (should (= nlre--compiled-cache-misses 2)))

(ert-deftest nelisp-regexp-compiled-cache-invalidates-mutable-inputs ()
  "Mutating key inputs in place must never return a stale compilation."
  (let ((case-fold-search nil)
        (pattern (copy-sequence "alpha"))
        (search-spaces-regexp (copy-sequence " +")))
    (nlre--compiled-cache-clear)
    (let ((pattern-first (nlre--compiled-pattern pattern)))
      (aset pattern 0 ?o)
      (should-not (eq pattern-first (nlre--compiled-pattern pattern))))
    (let ((spaces-first (nlre--compiled-pattern "a b")))
      (aset search-spaces-regexp 0 ?x)
      (should-not (eq spaces-first (nlre--compiled-pattern "a b"))))))

(ert-deftest nelisp-regexp-compiled-cache-defers-case-table-contents ()
  "Cached plans stay valid because case translation happens at match time."
  (with-temp-buffer
    (let ((case-fold-search t)
          (table (copy-case-table (current-case-table))))
      (set-case-table table)
      (nlre--compiled-cache-clear)
      (let ((first (nlre--compiled-pattern "A")))
        (should (= (nlre-string-match "A" "a") 0))
        (set-char-table-range (char-table-extra-slot table 1) ?A ?z)
        (should (eq first (nlre--compiled-pattern "A")))
        (should-not (nlre-string-match "A" "a"))
        (should (= (nlre-string-match "A" "z") 0))))))

(ert-deftest nelisp-regexp-compiled-cache-tracks-only-needed-syntax-table ()
  "Syntax mutation invalidates syntax patterns but not literal patterns."
  (with-temp-buffer
    (let ((case-fold-search nil)
          (table (copy-syntax-table)))
      (set-syntax-table table)
      (nlre--compiled-cache-clear)
      (let ((syntax-first (nlre--compiled-pattern "\\sw"))
            (literal-first (nlre--compiled-pattern "literal")))
        (modify-syntax-entry ?_ "w" table)
        (should-not (eq syntax-first (nlre--compiled-pattern "\\sw")))
        (should (eq literal-first (nlre--compiled-pattern "literal")))))))

(ert-deftest nelisp-regexp-anchored-greedy-suffix-plan-matches-host ()
  "Anchored greedy-dot captures use the finite suffix plan with GNU spans."
  (dolist (case
           '(("\\`\\(.*\\)\\(\\)\\'" "alpha")
             ("\\`\\(.*\\)\\(\\.el\\)\\'" "alpha.el")
             ("\\`\\(.*\\)\\(a\\|ba\\)\\'" "ba")
             ("\\`\\(.*\\)\\(\\.so\\|\\.elc\\|\\.elc\\.gz\\|\\.el\\|\\.el\\.gz\\)\\'"
              "alpha.el.gz")
             ("\\`\\(.*\\)\\(\n\\.el\\)\\'" "alpha\n.el")
             ("\\`\\(.*\\)\\(\\.el\\)\\'" "alpha\nbeta.el")))
    (let* ((regexp (nth 0 case))
           (string (nth 1 case))
           (reference (string-match regexp string))
           (reference-data (and reference (match-data)))
           (compiled (nlre--compiled-pattern regexp))
           (plan (aref compiled 2))
           (actual (nlre-string-match regexp string)))
      (should (and plan (eq (aref plan 0) :greedy-suffix)))
      (should (equal actual reference))
      (when reference
        (should (equal (nlre-match-beginning 0) (nth 0 reference-data)))
        (should (equal (nlre-match-end 0) (nth 1 reference-data)))
        (should (equal (nlre-match-beginning 1) (nth 2 reference-data)))
        (should (equal (nlre-match-end 1) (nth 3 reference-data)))
        (should (equal (nlre-match-beginning 2) (nth 4 reference-data)))
        (should (equal (nlre-match-end 2) (nth 5 reference-data)))))))

(ert-deftest nelisp-regexp-search-spaces-and-syntax-table-match-host ()
  "Mutable search and syntax inputs affect matching exactly as on GNU."
  (dolist (spaces '("[ \t]+" "\\([ \t]+\\)"))
    (let* ((search-spaces-regexp spaces)
           (regexp "a b\\(c\\)") (string "a\t\tbc")
           (reference (string-match regexp string))
           (reference-data (and reference (match-data)))
           (actual (nlre-string-match regexp string)))
      (should (equal actual reference))
      (should (equal (and actual (nlre-match-beginning 1))
                     (and reference (nth 2 reference-data))))
      (when (> (length reference-data) 4)
        (should (equal (nlre-match-beginning 2) (nth 4 reference-data))))))
  (with-temp-buffer
    (let ((table (copy-syntax-table)))
      (set-syntax-table table)
      (modify-syntax-entry ?_ "w" table)
      (let* ((regexp "\\w+") (string "_x")
             (reference (string-match regexp string))
             (reference-end (and reference (match-end 0)))
             (actual (nlre-string-match regexp string)))
        (should (equal actual reference))
        (should (equal (and actual (nlre-match-end 0)) reference-end))))))

(ert-deftest nelisp-regexp-case-fold-unicode-and-suffix-match-host ()
  "Canonical Unicode case folding also applies to finite suffix plans."
  (let ((case-fold-search t))
    (dolist (case '(("σ" "ς") ("\\(ä\\)\\'" "Ä")))
      (let* ((regexp (nth 0 case)) (string (nth 1 case))
             (reference (string-match regexp string))
             (reference-data (and reference (match-data)))
             (actual (nlre-string-match regexp string)))
        (should (equal actual reference))
        (should (equal (and actual (nlre-match-end 0))
                       (and reference (nth 1 reference-data))))))))

(ert-deftest nelisp-regexp-posix-alpha-matches-unicode-letters ()
  "POSIX alphabetic classes classify Unicode letters at match time."
  (let* ((case-fold-search nil)
         (regexp "[[:alpha:]]+")
         (string "Ää σς")
         (reference (string-match regexp string))
         (reference-end (and reference (match-end 0)))
         (actual (nlre-string-match regexp string)))
    (should (equal actual reference))
    (should (equal (and actual (nlre-match-end 0)) reference-end))
    (should (= actual 0))
    (should (= (nlre-match-end 0) 2))))

(ert-deftest nelisp-regexp-raw-byte-literal-keeps-pattern-kind ()
  "A raw byte pattern remains distinct from the same-codepoint text pattern."
  (let* ((case-fold-search nil)
         (pattern (unibyte-string 233))
         (subjects (list pattern (string 233) (string-as-multibyte pattern))))
    (dolist (string subjects)
      (let ((reference (string-match pattern string))
            (actual (nlre-string-match pattern string)))
        (should (equal actual reference))))))

(ert-deftest nelisp-regexp-raw-byte-suffix-keeps-pattern-kind ()
  "Raw-byte finite suffixes remain distinct from Unicode text."
  (let* ((case-fold-search nil)
         (byte (unibyte-string 233))
         (open (unibyte-string ?\\ ?\())
         (close-eos (unibyte-string ?\\ ?\) ?\\ ?'))
         (suffix (concat open byte close-eos))
         (greedy (concat (unibyte-string ?\\ ?` ?\\ ?\( ?. ?* ?\\ ?\)
                                         ?\\ ?\()
                         byte close-eos))
         (subjects (list byte (string 233) (string-as-multibyte byte))))
    (dolist (regexp (list suffix greedy))
      (let ((plan (aref (nlre--compiled-pattern regexp) 2)))
        (should (memq (aref plan 0) '(:suffix :greedy-suffix))))
      (dolist (string subjects)
        (let ((reference (string-match regexp string))
              (reference-data (match-data))
              (nlre--last-buffer nil)
              (actual (nlre-string-match regexp string)))
          (should (equal actual reference))
          (when reference
            (should (equal (nlre-match-data t) reference-data))))))))

(ert-deftest nelisp-regexp-multichar-literal-prefix-scans-before-engine ()
  "A leading literal run locates candidates without changing full matching."
  (dolist (case '(("needle\\([0-9]+\\)" "xxxxneedle42" nil nil)
                  ("Needle\\(.\\)\\1" "xxneedleaa" t nil)
                  ("needleZ." "xxneedleZtail" nil 10)
                  ("needleZ." "xxneedleZtail" nil 9)))
    (let* ((regexp (nth 0 case)) (string (nth 1 case))
           (case-fold-search (nth 2 case)) (ceiling (nth 3 case))
           (reference (if ceiling
                          (string-match regexp (substring string 0 ceiling))
                        (string-match regexp string)))
           (compiled (nlre--compiled-pattern regexp))
           (prefix (aref compiled 4))
           (actual (nlre-string-match regexp string 0 ceiling)))
      (should (and prefix (eq (aref prefix 0) :literal)
                   (> (length (aref prefix 1)) 1)))
      (should (equal actual reference)))))

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
    (should (= (nlre-match-beginning 0) 3))
    (should (eq nlre--last-buffer (current-buffer)))
    (let ((integer-data (nlre-match-data t))
          (marker-data (nlre-match-data)))
      (should (equal (butlast integer-data) '(3 9)))
      (should (eq (car (last integer-data)) (current-buffer)))
      (should (markerp (car marker-data)))
      (should (= (marker-position (car marker-data)) 3)))
    (let ((old-caps nlre--last-caps) (old-buffer nlre--last-buffer))
      (goto-char 1)
      ;; The internal string scan finds NEEDLE later, but `looking-at' fails
      ;; and must restore the prior successful buffer match state.
      (should-not (nlre--looking-at "needle"))
      (should (eq nlre--last-caps old-caps))
      (should (eq nlre--last-buffer old-buffer))
      (should-not (nlre--re-search-forward "absent" nil t))
      (should (eq nlre--last-caps old-caps))
      (should (eq nlre--last-buffer old-buffer)))))

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

(ert-deftest nelisp-regexp-looking-back-matches-host-and-retains-point ()
  "Backward look checks exact end, greedy extension, limits and captures."
  (dolist (case '((".*" nil nil) (".*" 4 t) ("a.*" nil nil)
                  ("\\(a.*\\)" 3 t) ("z" nil nil)))
    (with-temp-buffer
      (insert "xxabc")
      (goto-char (point-max))
      (let* ((regexp (nth 0 case)) (limit (nth 1 case)) (greedy (nth 2 case))
             (origin (point))
             (reference (looking-back regexp limit greedy))
             (reference-start (and reference (match-beginning 0)))
             (reference-end (and reference (match-end 0)))
             (reference-group (and reference (> (length (match-data)) 2)
                                   (cons (match-beginning 1) (match-end 1))))
             (actual (nlre--looking-back regexp limit greedy)))
        (should (equal actual reference))
        (should (= (point) origin))
        (when reference
          (should (= (nlre-match-beginning 0) reference-start))
          (should (= (nlre-match-end 0) reference-end))
          (when reference-group
            (should (equal (cons (nlre-match-beginning 1) (nlre-match-end 1))
                           reference-group))))))))

(ert-deftest nelisp-regexp-looking-back-failure-preserves-match-state ()
  "A failed backward look preserves match data and buffer origin."
  (with-temp-buffer
    (insert "xxabc")
    (goto-char (point-max))
    (should (nlre--looking-back "a.*" nil nil))
    (let ((old-caps nlre--last-caps) (old-buffer nlre--last-buffer)
          (origin (point)))
      (should-not (nlre--looking-back "z" nil nil))
      (should (= (point) origin))
      (should (eq nlre--last-caps old-caps))
      (should (eq nlre--last-buffer old-buffer)))))

(defun nelisp-regexp-diff-tramp-regexps ()
  "Tramp's `file-name-handler-alist' regexps from the host Emacs, or nil.
These are the prefilter's motivating case: tried on every file name."
  (when (require 'tramp nil t)
    (delq nil (mapcar (lambda (handler)
                        (car (rassq handler file-name-handler-alist)))
                      '(tramp-file-name-handler tramp-completion-file-name-handler
                        tramp-archive-file-name-handler)))))

(defun nelisp-regexp-diff--groups (n)
  (let ((out nil) (k 0))
    (while (<= k n)
      (push (list (ignore-errors (match-beginning k)) (ignore-errors (match-end k))) out)
      (setq k (1+ k)))
    (nreverse out)))

(defun nelisp-regexp-diff--mine-groups (n)
  (let ((out nil) (k 0))
    (while (<= k n)
      (push (list (nlre-match-beginning k) (nlre-match-end k)) out)
      (setq k (1+ k)))
    (nreverse out)))

(ert-deftest nelisp-regexp-prefilter-matches-host ()
  "Anchoring found inside leading groups, the anchored literal prefix and the
required-character prefilter must agree with host `string-match': on the
start index, on every group span, with START, and under case folding."
  (let ((cases
         (list
          ;; \` inside leading groups and alternations, and a mixed alternation
          ;; that must not be treated as anchored.
          '("\\(?:\\`/\\)x" "/x") '("\\(?:\\`/\\)x" "a/x") '("\\(\\`a\\)b" "ab")
          '("\\(\\`a\\)b" "aab") '("\\(?:\\`a\\|\\`b\\)c" "bc") '("\\(?:\\`a\\|\\`b\\)c" "cbc")
          '("\\(?:\\`a\\|b\\)c" "xbc") '("\\`\\(ab\\)" "ab") '("\\(?:\\(?:\\`\\)ab\\)" "xab")
          '("\\(?:\\`/ssh:\\)" "/tmp/no-such-file-x.el") '("\\(?:\\`/ssh:\\)" "/ssh:host")
          ;; Required characters, including through alternations and options.
          '("a:b" "xa:by") '("a:b" "xab") '("x:\\|y:" "zy:") '("x:\\|y:" "zy;")
          '("x:\\|y" "zy") '("a\\(:\\)?b" "ab") '("a\\(?::\\)+b" "a::b") '("a\\(?::\\)+b" "ab")
          '("[:]x" "y:x") '("\\(:\\)\\{2\\}" "a::") '("\\(:\\)\\{0,2\\}x" "x")
          ;; Anchored at both ends: a probe character (/, space, newline, ...)
          ;; after the literal prefix that no remaining atom consumes rules the
          ;; string out; a back reference disables the probes.
          '("\\`/[a-z]+\\'" "/abc") '("\\`/[a-z]+\\'" "/ab/c") '("\\`/[a-z]+\\'" "/abC")
          '("\\`a\\(b\\|c\\)*\\'" "abcbc") '("\\`a\\(b\\|c\\)*\\'" "abd")
          '("\\`a[B-D]+\\'" "Abcd") '("\\`\\(/\\)a\\1\\'" "/a/") '("\\`\\(/\\)a\\1\\'" "/ab")
          '("\\`[a-z]+\\'" "ab\u00e9") '("\\`[^/]+\\'" "\u00e9t\u00e9") '("\\`.*\\'" "a\nb")
          '("\\`x\\w+\\'" "xab_c") '("\\`x\\s-*\\'" "x  ") '("\\(?:\\`/\\|\\`~\\)[a-z]*\\'" "~ab")
          '("\\(?:\\`/\\|\\`~\\)[a-z]*\\'" "~a/b") '("\\`\\(?:ab\\)?\\'" "") '("\\`\\'" "")
          '("\\`a[^ ]*\\'" "ab c") '("\\`a[^ ]*\\'" "ab/c") '("\\`ab\\'" "ab/")
          '("\\`a\\(?:/\\|x\\)*\\'" "a/x/") '("\\`a[[:alpha:]-]*\\'" "a-b_c"))))
    (dolist (cs cases)
      (dolist (fold '(nil t))
        (dolist (start (if (> (length (cadr cs)) 0) '(nil 1) '(nil)))
          (let* ((case-fold-search fold) (p (car cs)) (s (cadr cs))
                 (ref (string-match p s start))
                 (ref-groups (and ref (nelisp-regexp-diff--groups 2)))
                 (mine (nlre-string-match p s start))
                 (mine-groups (and mine (nelisp-regexp-diff--mine-groups 2))))
            (should (equal (list p s fold start ref ref-groups)
                           (list p s fold start mine mine-groups))))))))
  ;; Case folding: a letter prefix may not be used literally, nor a
  ;; character above ASCII, which can be the fold of another (ẞ -> ß).
  (let ((case-fold-search t))
    (dolist (c '(("\\`\u00dfx" "\u1e9ex") ("\u00df:" "a\u1e9e:") ("\\`\u1e9e" "\u00df")
                 ("\\`\u212a" "k") ("k:" "\u212a:")))
      (should (equal (list c (string-match (car c) (cadr c)))
                     (list c (nlre-string-match (car c) (cadr c)))))))
  (let ((case-fold-search t))
    (should (equal (string-match "\\(?:\\`Ab\\)c" "abc") (nlre-string-match "\\(?:\\`Ab\\)c" "abc")))
    (should (equal (string-match "\\(?:\\`ab\\)c" "ABC") (nlre-string-match "\\(?:\\`ab\\)c" "ABC")))
    (should (equal (string-match "Q:\\|q;" "xq:") (nlre-string-match "Q:\\|q;" "xq:"))))
  ;; Tramp's handlers on the file names `find-file-name-handler' sees.
  (dolist (re (nelisp-regexp-diff-tramp-regexps))
    (dolist (s '("/tmp/no-such-file-x.el" "/ssh:host:/path" "/ssh:user@host:/path"
                 "/sudo::/etc/hosts" "/ssh:a|sudo:b:/x" "/-:host:/x" "/" "/ssh" "/ssh:"
                 "/method:" "/ssh:host" "relative/path" "" "/x:y" "/ssh:host#22:/p"
                 "/ssh:[::1]:/p" "~/file" "/home/user/.emacs.d/elpa/x.el"))
      (dolist (fold '(nil t))
        (let* ((case-fold-search fold)
               (ref (string-match re s))
               (ref-groups (and ref (nelisp-regexp-diff--groups 5)))
               (mine (nlre-string-match re s))
               (mine-groups (and mine (nelisp-regexp-diff--mine-groups 5))))
          (should (equal (list s fold ref ref-groups) (list s fold mine mine-groups))))))))

(ert-deftest nelisp-regexp-posix-unicode-and-syntax-match-host ()
  "POSIX classes use GNU categories, case mappings and buffer syntax."
  (with-temp-buffer
    (dolist (syntax '("_" "w" " "))
      (set-syntax-table (copy-syntax-table))
      (modify-syntax-entry ?_ syntax)
      (dolist (fold '(nil t))
        (let ((case-fold-search fold))
          (dolist (class '("alpha" "alnum" "word" "upper" "lower" "space"
                           "blank" "punct" "cntrl" "graph" "print"))
            (dolist (char '(9 10 32 65 95 97 127 160 178 223 453 769
                           8544 19968 8203 128512 1114111))
              (let* ((regexp (concat "[[:" class ":]]"))
                     (text (char-to-string char))
                     (reference (string-match regexp text)))
                (should (equal (list class char fold reference)
                               (list class char fold
                                     (nlre-string-match regexp text))))))))))))

(ert-deftest nelisp-regexp-late-case-api-and-in-place-canonical-mutation ()
  "Discover a late case API and never assume the standard table is immutable."
  (with-temp-buffer
    (let* ((case-fold-search t)
           (table (copy-case-table (current-case-table)))
           (nlre--case-table-reader nil))
      (set-case-table table)
      (nlre--compiled-cache-clear)
      (should (= 0 (nlre-string-match "A" "a")))
      (should (eq nlre--case-table-reader 'current-case-table))
      (set-char-table-range (char-table-extra-slot table 1) ?A ?z)
      (should-not (nlre-string-match "A" "a"))
      (should (= 0 (nlre-string-match "A" "z"))))))

(ert-deftest nelisp-regexp-cached-prefilter-defers-case-and-posix-syntax ()
  "Forbidden probes must not freeze mutable case or syntax predicates."
  (with-temp-buffer
    (let ((case-fold-search t) (pattern "\\`A+\\'"))
      (nlre--compiled-cache-clear)
      (should (= 0 (nlre-string-match pattern "A")))
      (let ((table (copy-case-table (current-case-table))))
        (set-case-syntax-pair ?A ?/ table)
        (set-case-table table)
        (should (= 0 (string-match pattern "/")))
        (should (= 0 (nlre-string-match pattern "/")))))
    (let ((case-fold-search nil) (pattern "\\`[[:word:]]+\\'"))
      (set-syntax-table (copy-syntax-table))
      (modify-syntax-entry ?/ ".")
      (nlre--compiled-cache-clear)
      (should-not (nlre-string-match pattern "/"))
      (modify-syntax-entry ?/ "w")
      (should (= 0 (string-match pattern "/")))
      (should (= 0 (nlre-string-match pattern "/"))))
    (let ((case-fold-search nil) (search-spaces-regexp "\\w+")
          (pattern "\\` \\'"))
      (modify-syntax-entry ?/ ".")
      (nlre--compiled-cache-clear)
      (should-not (nlre-string-match pattern "/"))
      (modify-syntax-entry ?/ "w")
      (should (= 0 (string-match pattern "/")))
      (should (= 0 (nlre-string-match pattern "/"))))))

(ert-deftest nelisp-regexp-set-case-equivalence-cycles-match-host ()
  "General and finite set plans use full case equivalence, including custom pairs."
  (with-temp-buffer
    (let ((case-fold-search t))
      (dolist (pattern '("[σ]" "[σ]+" "[Σ]" "\\`[σ]\\'"))
        (should (equal (string-match pattern "ς")
                       (nlre-string-match pattern "ς"))))
      (let ((table (copy-case-table (current-case-table))))
        (set-case-syntax-pair ?A ?/ table)
        (set-case-table table)
        (dolist (pattern '("[A]" "[A]+" "\\`[A]\\'" "[[:upper:]]"))
          (should (equal (string-match pattern "/")
                         (nlre-string-match pattern "/"))))))))

(ert-deftest nelisp-regexp-dot-tests-canonical-newlines ()
  "GNU applies canonical case translation before dot's newline rejection."
  (with-temp-buffer
    (let ((case-fold-search t)
          (table (copy-case-table (current-case-table))))
      (set-case-table table)
      (aset (char-table-extra-slot table 1) ?B ?\n)
      (dolist (case '(("." "B") (".+" "B")
                      ("\\`\\(.*\\)\\(x\\)\\'" "Bx")))
        (should (equal (string-match (car case) (cadr case))
                       (nlre-string-match (car case) (cadr case))))))))

(ert-deftest nelisp-regexp-posix-canonical-bitmap-differs-from-ranges ()
  "GNU's class bitmap and multibyte range flags have distinct fold rules."
  (with-temp-buffer
    (let ((case-fold-search t)
          (table (copy-case-table (current-case-table))))
      (set-case-table table)
      (aset (char-table-extra-slot table 1) ?λ ?!)
      (aset (char-table-extra-slot table 1) ?1 ?x)
      (aset (char-table-extra-slot table 1) ?! ?1)
      (dolist (case '(("[[:alpha:]]" "λ") ("[[:lower:]]" "λ")
                      ("[[:punct:]]" "λ") ("[λ]" "Λ") ("[λ]+" "Λ")
                      ("\\`[λ]\\'" "Λ")
                      ("[[:digit:]]" "!") ("[0-9]" "!")))
        (should (equal (string-match (car case) (cadr case))
                       (nlre-string-match (car case) (cadr case)))))))
  (let ((case-fold-search nil) (raw (char-to-string #x3fff80)))
    (should (equal (string-match "[[:word:]]" raw)
                   (nlre-string-match "[[:word:]]" raw)))))

(ert-deftest nelisp-regexp-cache-separates-equal-pattern-byte-kinds ()
  "ASCII content can be equal while its compilation byte kind differs."
  (let ((case-fold-search t)
        (unibyte "A") (multibyte (substring (concat "λ" "A") 1)))
    (should (equal unibyte multibyte))
    (nlre--compiled-cache-clear)
    (let ((first (nlre--compiled-pattern unibyte)))
      (should-not (eq first (nlre--compiled-pattern multibyte)))
      (should (eq first (nlre--compiled-pattern unibyte))))))

(ert-deftest nelisp-regexp-spaces-preserve-quantified-runs-and-raw-bytes ()
  "Space substitution follows GNU's quantifier exception and byte kind."
  (let ((case-fold-search nil) (search-spaces-regexp "-"))
    (dolist (pattern '(" +" "  +" " *" " ?" " \\{2\\}"))
      (dolist (text '("   " "---"))
        (should (equal (string-match pattern text)
                       (nlre-string-match pattern text)))))
    (let ((pattern (unibyte-string ?\[ 255 ?\])))
      (dolist (text (list (unibyte-string 255) (string 255)))
        (should (equal (string-match pattern text)
                       (nlre-string-match pattern text)))))))

(ert-deftest nelisp-regexp-unibyte-space-substitution-keeps-encoded-bytes ()
  "GNU inserts SPACES bytes into a unibyte regexp without decoding them."
  (let ((case-fold-search nil) (nlre--last-buffer nil))
    (dolist (spaces (list (string 255) (unibyte-string 255)
                         (string-to-multibyte (unibyte-string 128))
                         (string-to-multibyte (unibyte-string 255))))
      (let ((search-spaces-regexp spaces))
        (dolist (pattern (list (unibyte-string 32)
                              (unibyte-string ?a 32 ?b)))
          (dolist (middle (list (unibyte-string 255) (string 255)
                                (unibyte-string 195 191)
                                (unibyte-string 192 128)
                                (unibyte-string 193 191)))
            (let* ((text (if (= (length pattern) 1) middle
                           (concat "a" middle "b")))
                   (expected (string-match pattern text))
                   (data (and expected (match-data t)))
                   (actual (nlre-string-match pattern text)))
              (should (equal actual expected))
              (when actual (should (equal (nlre-match-data t) data))))))))))

(ert-deftest nelisp-regexp-unibyte-raw-space-captures-match-host ()
  "Check every raw byte with GNU, including folded matching and captures."
  (dolist (fold '(nil t))
    (let ((case-fold-search fold) (nlre--last-buffer nil)
          (patterns (list (unibyte-string 92 40 32 92 41)
                          "\\`\\( \\)\\'" "\\`.* \\'")))
      (dolist (pattern patterns)
        (dotimes (offset 128)
          (let ((search-spaces-regexp (string (+ #x3fff80 offset))))
            (dolist (text (list (unibyte-string (+ 128 offset))
                                (unibyte-string (+ #xc0 (ash offset -6))
                                                (+ #x80 (logand offset 63)))
                                (unibyte-string (+ #xe0 (ash offset -6))
                                                (+ #x80 (logand offset 63)))))
              (let* ((expected (string-match pattern text))
                     (data (and expected (match-data t)))
                     (actual (nlre-string-match pattern text)))
                (should (equal actual expected))
                (when actual (should (equal (nlre-match-data t) data)))))))))))

(ert-deftest nelisp-regexp-cross-byte-canonical-mappings-match-host ()
  "Literal and set rules differ when ASCII maps to a nonbyte character."
  (with-temp-buffer
    (let ((case-fold-search t)
          (table (copy-case-table (current-case-table)))
          (multi-a (substring (concat "λ" "A") 1)))
      (set-case-table table)
      (aset (char-table-extra-slot table 1) ?A ?λ)
      (dolist (pattern (list "A" multi-a "[A]" "[A]+" "\\`[A]\\'"
                             "[[:alpha:]]" "[[:upper:]]"))
        (dolist (text '("A" "λ"))
          (should (equal (list pattern (multibyte-string-p pattern) text
                               (string-match pattern text))
                         (list pattern (multibyte-string-p pattern) text
                               (nlre-string-match pattern text)))))))))

(ert-deftest nelisp-regexp-raw-byte-backrefs-do-not-become-unicode-case-pairs ()
  "Raw bytes keep their byte identity while matching a folded backreference."
  (dolist (fold '(nil t))
    (let ((case-fold-search fold))
      (dolist (text (list (unibyte-string 201 233) (unibyte-string 233 233)
                          (unibyte-string ?A ?a)))
        (should (equal (string-match "\\(.\\)\\1" text)
                       (nlre-string-match "\\(.\\)\\1" text)))))))

(ert-deftest nelisp-regexp-custom-fold-search-bitmap-applies-only-at-start ()
  "GNU filters initial byte candidates separately from charset membership."
  (with-temp-buffer
    (let ((case-fold-search t)
          (table (copy-case-table (current-case-table))))
      (set-case-table table)
      (aset (char-table-extra-slot table 1) ?A ?λ)
      (dolist (case (list '("[[:lower:]]" "A") '("x[[:lower:]]" "xA")
                          '(".[[:lower:]]" "xA")
                          '("x*[[:lower:]]" "A") '("x?[[:lower:]]" "A")
                          '("\\(?:x\\|[[:lower:]]\\)" "A")
                          '("\\(?:x\\|\\)[[:lower:]]" "A")
                          (list "[[:lower:]]" (string-to-multibyte "A"))))
        (should (equal (string-match (car case) (cadr case))
                       (nlre-string-match (car case) (cadr case)))))
      (set-buffer-multibyte nil)
      (insert "A") (goto-char 1)
      (should (equal (looking-at "[[:lower:]]")
                     (nlre--looking-at "[[:lower:]]")))
      (erase-buffer) (insert "Aa") (goto-char 3)
      (dolist (greedy '(nil t))
        (let* ((expected (looking-back "[[:lower:]]+" nil greedy))
               (spans (match-data t)))
          (should (equal expected
                         (nlre--looking-back "[[:lower:]]+" nil greedy)))
          (should (equal spans (nlre-match-data t))))))))

(provide 'nelisp-regexp-diff-test)
;;; nelisp-regexp-diff-test.el ends here

(ert-deftest nelisp-regexp-repeated-alternatives-backtrack-with-all-captures ()
  "Repetition must retain alternatives until its continuation succeeds."
  (dolist (case '(("\\(ab\\|a\\)*b" "ab")
                  ("\\(ab\\|a\\)+b" "ab")
                  ("\\(ab\\|a\\)?b" "ab")
                  ("\\(ab\\|a\\)\\{1,2\\}b" "ab")
                  ("\\(ab\\|a\\)*?b" "ab")
                  ("\\(ab\\|a\\)+?b" "ab")
                  ("\\(\\(ab\\|a\\)\\)*b" "aab")
                  ("\\(\\(a\\|b\\)*\\)\\2" "ab")
                  ("\\(a\\|b\\)*\\1" "abb")
                  ("\\(a\\|b\\)*\\1" "aba")
                  ("\\(a?\\)*" "") ("\\(a?\\)*" "aa")
                  ("\\(a*\\)+" "aa")
                  ("\\(ab\\|a\\)?b" "b")
                  ("\\(\\(a\\)\\|\\(b\\)\\)*c" "abc")
                  ("\\(a\\(b\\)?\\)*" "aba")
                  ("\\(a?\\|b\\)\\{0,2\\}a" "ba")
                  ("\\(a?\\|b\\)\\{0,2\\}a" "baa")
                  ("\\(a?\\|b\\)\\{1,3\\}a" "ba")
                  ("\\(a?\\|b\\)\\{2,4\\}a" "ba")))
    (let* ((nlre--last-buffer nil)
           (case-fold-search nil)
           (expected (string-match (car case) (cadr case)))
           (expected-data (and expected (match-data t)))
           (actual (nlre-string-match (car case) (cadr case)))
           (actual-data (and actual (fboundp 'nlre-match-data) (nlre-match-data t))))
      (should (equal actual expected))
      (should (equal (list case actual actual-data)
                     (list case expected expected-data))))))
