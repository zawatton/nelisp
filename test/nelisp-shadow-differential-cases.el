;;; nelisp-shadow-differential-cases.el --- native vs prelude, same answers -*- lexical-binding: nil; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Expressions exercising names the standalone provides natively AND the
;; prelude redefines unconditionally, so loading the prelude replaces the
;; native implementation with an Elisp one.  `make
;; standalone-reader-shadow-smoke' evaluates this file twice -- once as-is,
;; once with the prelude loaded first -- and requires the two results to be
;; identical.
;;
;; Measured 2026-08-19: 70 of the 245 `shared-shadowing' names in
;; docs/emacs-compat-table.txt are also in the standalone's native builtin
;; list, and 10 of those are defined in the prelude, which is the set both
;; halves of a run can actually reach.  Nothing here diverged when the file
;; was written; the point is that it stays that way.
;;
;; The class is not hypothetical.  Three defects on 2026-08-19 were an
;; unconditional definition landing on a working one: `provide'/`featurep'
;; fset over the native builtins while `require' stayed native, a
;; `string-match-p' that recognised five literal regexps and answered nil to
;; everything else, and an `error-message-string' that dropped the error
;; symbol.  Each was found by hand, late, a long way from where it was
;; introduced.  A differential is how a machine finds the next one.
;;
;; When comparing this file's value against stock Emacs by hand, wrap it so
;; NeLisp prints through `format "%S"' as Emacs does:
;;
;;   (princ (format "%S\n" (progn <this file>)))
;;
;; Reading the runtime's own value echo instead compares Emacs's printer
;; against a DIFFERENT NeLisp printer -- the native `nelisp--repr' -- and
;; that one does not escape a backslash inside a nested string, so
;; (prin1-to-string (intern "12")) shows as "\\12" from one and "\\\\12"
;; from the other while the value is byte-identical.  An hour went into
;; that mirage on 2026-08-19.  The echo gap is a real divergence and is its
;; own item; it is just not what these cases are about.
;;
;; Add a case when a prelude definition starts covering more ground, and
;; keep every expression answerable by BOTH implementations -- an expression
;; only the prelude can evaluate proves nothing about agreement.

;;; Code:

(list
 ;; Doc 200 P2: the standalone must distinguish raw-byte strings from UTF-8
 ;; strings while keeping ASCII equality representation-independent.  Keep
 ;; every result derived: returning a raw-byte string here would compare the
 ;; two printers instead of the string semantics.
 (equal (unibyte-string 227 129 130) "あ")
 (append (unibyte-string 200 201 202) nil)
 (append "あ" nil)
 (list (multibyte-string-p (unibyte-string 200 201))
       (multibyte-string-p "abc")
       (multibyte-string-p "あ"))
 (equal "abc" (unibyte-string 97 98 99))
 (let ((u (unibyte-string 200 201 202)))
   (list (length u) (string-bytes u) (aref u 0) (elt u 1)))
 (append (concat (unibyte-string 200) "a") nil)
 (append (substring (unibyte-string 200 201) 0 1) nil)
 (append (copy-sequence (unibyte-string 200 201)) nil)
 (append (upcase (unibyte-string 200 97)) nil)
 (append (downcase (unibyte-string 200 65)) nil)
 (vconcat (unibyte-string 200 201))
 (append (format "%s" (unibyte-string 200)) nil)
 (mapcar #'identity (unibyte-string 200 201))
 (append (mapconcat (lambda (c) (unibyte-string c))
                    (unibyte-string 200 201)
                    (unibyte-string 44))
         nil)
 (mapcar (lambda (s) (append s nil))
         (split-string (concat (unibyte-string 200) ","
                               (unibyte-string 201))
                       ","))
 (string= (unibyte-string 227 129 130) "あ")
 (= (sxhash-equal (unibyte-string 97 98 99)) (sxhash-equal "abc"))
 (= (sxhash (unibyte-string 97 98 99)) (sxhash "abc"))
 (list (multibyte-string-p (substring (unibyte-string 200) 0 1))
       (multibyte-string-p (copy-sequence (unibyte-string 200)))
       (multibyte-string-p (upcase (unibyte-string 200 97)))
       (multibyte-string-p (concat (unibyte-string 200) "a"))
       (multibyte-string-p (format "%s" (unibyte-string 200))))
 (append (string-as-multibyte (unibyte-string 227 129 130)) nil)
 (append (string-as-unibyte "あ") nil)
 (append (string-to-unibyte "abc") nil)
 (append (string-make-unibyte "abc") nil)
 (list (multibyte-string-p
        (string-as-multibyte (unibyte-string 227 129 130)))
       (append (string-to-multibyte (unibyte-string 65)) nil)
       (append (string-make-multibyte (unibyte-string 65)) nil))
 (condition-case e
     (string-to-unibyte "あ")
   (error (list (car e) (cadr e))))
 ;; format
 (format "%s-%d" "x" 7) (format "%S" '(1 . 2)) (format "%5.2f" 1.5)
 (format "%c%%" 65) (format "%-4s|" "ab")
 ;; substring, including the negative and empty edges
 (substring "abcdef" 1 3) (substring "abcdef" -2) (substring "abc" 0 0)
 (substring "abcdef" 2) (substring "abcdef" 0 -3)
 ;; string=
 (string= "a" "a") (string= "a" "b") (string= "" "")
 ;; the rounding family: sign matters, and each rounds a different way
 (floor 7 2) (floor -7 2) (floor 7) (ceiling 7 2) (ceiling -7 2)
 (truncate 7 2) (truncate -7 2) (truncate 7) (mod 7 2) (mod -7 2) (mod 7 -2)
 ;; equal: structure, not identity, and 1 is not 1.0
 (equal '(1 (2 3)) '(1 (2 3))) (equal "a" "a") (equal [1 2] [1 2]) (equal 1 1.0)
 (equal nil nil) (equal '(1 . 2) '(1 . 2))
 ;; natnump
 (natnump 3) (natnump 0) (natnump -1) (natnump "x")
 ;; A leading string is a docstring only when something follows it.  When it
 ;; is the whole body it IS the body -- `(defun f () "hello")' answered nil
 ;; until 2026-08-19, in both the prelude stripper and the native `defun'
 ;; the evaluator actually dispatches.  All four edges, because getting one
 ;; right by breaking another is the easy failure here.
 (progn (defun nl-diff-a () "only") (nl-diff-a))
 (progn (defun nl-diff-b () "doc" "body") (nl-diff-b))
 (progn (defun nl-diff-c (x) "doc" x) (nl-diff-c 5))
 ;; Split rather than wrapped in `progn': a call to an empty-body function
 ;; does not write its output slot, so inside a `progn' it yields whatever
 ;; the previous form left there -- `(progn (defun d () (declare ...)) (d))'
 ;; answers `d' here and nil in Emacs.  Separate list elements get separate
 ;; slots, so these two are the honest test of the declare-only body; the
 ;; slot bug is its own defect and does not belong hidden in this one.
 (defun nl-diff-d () (declare (indent 1)))
 (nl-diff-d)
 (progn (defun nl-diff-e () "doc" (declare (indent 1)) 7) (nl-diff-e))
 (progn (defmacro nl-diff-m () "mac-only") (nl-diff-m))
 ;; regexp-quote escapes exactly the eight characters Emacs's regexp syntax
 ;; treats as special.  It used to escape six more, and ( ) { } | are LITERAL
 ;; in an Emacs regexp -- the constructs are the backslashed forms -- so
 ;; escaping them built the very syntax the caller asked to be quoted away.
 ;; The match cases matter more than the strings: they are what the function
 ;; is for, and a correct-looking escape set is worthless if the engine
 ;; disagrees.
 ;; Compared with `equal' rather than returned raw, so the case turns on the
 ;; escape set rather than on how the result prints.
 ;;
 ;; (The comment here used to say `prin1' does not escape a backslash inside
 ;; a string.  That was wrong, and measuring the bytes says so: both print
 ;; \"a\\.b\\*\" as 34 97 92 92 46 98 92 92 42 34, identical.  What does
 ;; differ is the other direction -- this prints \\n and \\t where Emacs
 ;; emits a raw newline and tab -- and since both read back to the same
 ;; string, print-then-read is intact.)
 (equal (regexp-quote "(a)") "(a)")
 (equal (regexp-quote "a|b") "a|b")
 (equal (regexp-quote "a{2}") "a{2}")
 (equal (regexp-quote "a.b*") "a\\.b\\*")
 (equal (regexp-quote "[x]") "\\[x]")
 (string-match-p (regexp-quote "(a)") "x(a)y")
 (string-match-p (regexp-quote "a|b") "za|by")
 (string-match-p (regexp-quote "a.b") "zaXby")
 (string-match-p (regexp-quote "a|b") "za")
 ;; The sequence functions take the sequences Emacs takes.  `reverse' used
 ;; to walk any argument as a list, so a vector answered (nil) -- one
 ;; element, and the wrong one; `mapconcat' answered the empty string for a
 ;; vector because the walk never entered; `nreverse' signalled on a vector;
 ;; and `nconc' skipped a non-cons argument instead of making it the tail,
 ;; so the dotted-tail idiom silently lost data.
 (reverse (list 1 2 3))
 (reverse [1 2 3])
 (reverse "abc")
 (reverse nil)
 (type-of (reverse [1 2]))
 (type-of (reverse "ab"))
 (nreverse (list 1 2 3))
 (nreverse (vector 1 2 3))
 (nreverse (copy-sequence "abc"))
 ;; in place for a vector, as in Emacs: the caller's object changes
 (let ((v (vector 1 2 3))) (nreverse v) v)
 ;; and NOT in place for `reverse'
 (let ((v (vector 1 2 3))) (reverse v) v)
 (mapconcat #'identity (list "a" "b") "-")
 (mapconcat #'identity ["a" "b"] "-")
 (mapconcat #'char-to-string "abc" "-")
 (mapconcat #'identity nil "-")
 (nconc (list 1 2) 3)
 (nconc (list 1) (list 2))
 (nconc nil 5)
 (nconc (list 1) nil)
 (nconc)
 ;; pcase.  The dispatcher used to build the test `t' for any pattern head
 ;; it did not recognise, so those matched EVERYTHING -- and it recognised
 ;; `backquote' while the reader spells the head `\=`', and `comma' while the
 ;; reader spells it `\=,'.  Between them, no backquote pattern was ever
 ;; matched and every backquote clause won regardless of the value.  There
 ;; are 47 of them in this tree.
 (pcase 5 (5 'five) (_ 'other))
 (pcase 6 (5 'five) (_ 'other))
 (pcase 5 (n (list 'bound n)))
 (pcase 'a ('a 'is-a) (_ 'other))
 (pcase "x" ("x" 'sx) (_ 'other))
 (pcase '(1 2) (`(,a ,b) (list a b)) (_ 'other))
 (pcase '(1) (`(,a ,b) (list a b)) (_ 'other))
 (pcase '(1 2 3) (`(,a . ,rest) (list a rest)) (_ 'other))
 (pcase 5 ((pred integerp) 'int) (_ 'other))
 (pcase "s" ((pred integerp) 'int) (_ 'other))
 ;; `app' applies and matches the result; a guard reads a binding made
 ;; earlier in the same `and'.
 (pcase 5 ((app 1+ 6) 'six) (_ 'other))
 (pcase 5 ((app 1+ 7) 'seven) (_ 'other))
 (pcase 5 ((and n (guard (> n 3))) 'big) (_ 'small))
 (pcase 2 ((and n (guard (> n 3))) 'big) (_ 'small))
 ;; File paths.  `expand-file-name' used to concatenate and stop: no `.',
 ;; no `..', no `~', no collapsing of doubled slashes, and an empty name
 ;; came back empty.  A path it produced could not be compared with `equal'
 ;; against one Emacs produced, which for a runtime meant to host an editor
 ;; is a daily defect.
 ;;
 ;; BASE is passed explicitly rather than bound with `let': `default-
 ;; directory' is not a special variable in this runtime, so a `let' around
 ;; it does not reach `expand-file-name' and the case would measure that
 ;; instead of what it is about.  (That gap is real and is its own item.)
 (expand-file-name "a/../b" "/base/dir/")
 (expand-file-name "/a/../b" "/base/dir/")
 (expand-file-name "./a" "/base/dir/")
 (expand-file-name "a" "/base/dir/")
 (expand-file-name "/x/y" "/base/dir/")
 (expand-file-name "" "/base/dir/")
 (expand-file-name "a/" "/base/dir/")
 (expand-file-name ".." "/base/dir/")
 (expand-file-name "a/./b" "/base/dir/")
 (expand-file-name "a//b" "/base/dir/")
 (expand-file-name "x" "/base/dir")
 (directory-file-name "a//")
 (directory-file-name "a/")
 (directory-file-name "/")
 (file-name-as-directory "")
 (file-name-as-directory "a")
 (file-name-as-directory "a/")
 (file-name-sans-versions "foo.txt~")
 (file-name-sans-versions "foo.txt.~1~")
 (file-name-sans-versions "a~b.txt")
 (file-name-sans-versions "foo.txt.~1~x")
 (file-name-extension "foo.txt~")
 (file-name-extension "foo.txt")
 (file-name-extension "foo")
 (file-name-extension "foo.~12~")
 ;; A call to a function with an EMPTY body used to leave its output slot
 ;; untouched, so it answered whatever the previous form had left there:
 ;; (progn 42 (f)) was 42.  Every context that reuses a slot was affected --
 ;; progn, a let body, an if branch, or, after a while -- and it was never
 ;; about `defun': an empty `lambda' did it too.  Only a top-level call and
 ;; `list', which gives each element its own slot, came out right.
 (progn (defun nl-diff-empty ()) 42 (nl-diff-empty))
 (progn "abc" (nl-diff-empty))
 (let ((x 9)) 7 (nl-diff-empty))
 (if t (progn 42 (nl-diff-empty)) 'no)
 (or nil (progn 42 (nl-diff-empty)))
 (progn 42 (funcall (lambda ())))
 (let ((n 0)) (while (< n 1) (setq n 1)) (nl-diff-empty))
 (list 42 (nl-diff-empty))
 ;; `length' used to answer a number for anything: an improper list got the
 ;; count of its cons cells, a SYMBOL got the length of its NAME -- (length
 ;; 'foo) was 3 -- and everything else got 0, so (length 5) was 0 rather
 ;; than an error.  A record was 0 too, where Emacs counts the type tag.
 ;; The tolerant counterparts Emacs provides did not exist, which is why
 ;; they are here: without them there is no way to ask about a dotted list.
 (length (list 1 2 3))
 (length nil)
 (length [1 2 3])
 (length "abc")
 (length (record 'a 1 2))
 (condition-case e (length '(1 2 . 3)) (error e))
 (condition-case e (length 'foo) (error e))
 (condition-case e (length 5) (error e))
 (safe-length '(1 2 . 3))
 (safe-length (list 1 2))
 (safe-length nil)
 (proper-list-p '(1 2 . 3))
 (proper-list-p (list 1 2))
 (proper-list-p nil)
 (proper-list-p 5)
 ;; `default-directory' is bound to the real working directory now.  It was
 ;; unbound, so a relative name resolved against nothing and
 ;; (expand-file-name "a") answered "/a".  The value itself cannot be a case
 ;; here -- it depends on where the test runs -- so these check the shape
 ;; and the relationship, which do not.
 ;; Only the two properties that hold in BOTH: Emacs keeps this variable in
 ;; abbreviated form, so it starts with ~ there and / here, and
 ;; (expand-file-name "a") therefore does not equal (concat
 ;; default-directory "a") in Emacs.  That the value tracks the real cwd is
 ;; checked directly instead, from four different directories, since it
 ;; cannot be a fixed case in a file.
 (and (boundp 'default-directory) (stringp default-directory))
 (eq (aref default-directory (1- (length default-directory))) ?/)
 ;; CASE-FOLD and IGNORE-CASE were accepted and ignored -- the parameters
 ;; were even named `_case-fold' and `_ignore-case' to say so -- so a caller
 ;; that asked for a case-insensitive lookup got a case-sensitive one and no
 ;; indication.  It failed on plain ASCII, not just on the non-ASCII these
 ;; still cannot fold.  `compare-strings' existed only in a file the
 ;; standalone does not load, so it was void-function.
 (assoc-string "abc" '("abc"))
 (assoc-string "ABC" '("abc") t)
 (assoc-string "ABC" '("abc") nil)
 (assoc-string "A" '((a . 1)) t)
 (assoc-string "KEY" '(("key" . 1) ("other" . 2)) t)
 (assoc-string "A" '("a" "A") t)
 (assoc-string "zzz" '("abc") t)
 (string-prefix-p "AB" "abcd" t)
 (string-prefix-p "ab" "abcd")
 (string-prefix-p "" "abc")
 (string-prefix-p "abcd" "abc")
 (string-suffix-p "CD" "abcd" t)
 (string-suffix-p "" "abc")
 (compare-strings "ABC" nil nil "abc" nil nil t)
 (compare-strings "abc" nil nil "abc" nil nil)
 (compare-strings "abd" nil nil "abc" nil nil)
 (compare-strings "xabcy" 1 4 "abc" nil nil)
 (compare-strings "ab" nil nil "abc" nil nil)
 (compare-strings "abc" nil nil "ab" nil nil)
 (compare-strings "" nil nil "" nil nil)
 (compare-strings "ABD" nil nil "abc" nil nil t)
 ;; A batch of small ones, each the same shape: a parameter accepted and
 ;; dropped, or a guard that never fired.  `delete' built a fresh list so it
 ;; removed nothing from the caller's; `nthcdr' walked past a negative N and
 ;; answered nil for every negative index; `make-list' of a negative length
 ;; answered nil rather than signalling; `string-equal' fell through to
 ;; `equal' so (string-equal 5 5) was t; `string-empty-p' called nil empty
 ;; because (length nil) is 0; `copy-sequence' returned a non-sequence
 ;; unchanged, so a caller copying in order to mutate mutated the original;
 ;; and `lsh' did not exist at all.
 (let ((l (list 1 2 3))) (delete 2 l) l)
 (delete 2 (list 1 2 3))
 (delete 1 (list 1 1 2))
 (delete 1 (vector 1 2 1))
 (delete ?a "aba")
 (nth -1 '(1 2 3))
 (nth 1 '(1 2 3))
 (nthcdr -1 '(1 2 3))
 (condition-case e (make-list -1 'a) (error (car e)))
 (make-list 0 'a)
 (make-list 3 'a)
 (condition-case e (string-equal 5 5) (error (car e)))
 (string-equal "a" "a")
 (string-equal 'a "a")
 (string-empty-p nil)
 (string-empty-p "")
 (string-empty-p "x")
 (condition-case e (copy-sequence 5) (error (car e)))
 (copy-sequence (list 1 2))
 (lsh -1 -1)
 (lsh -8 -2)
 (lsh 1 4)
 (lsh 16 -2)
 (alist-get 'a '((a . 1)))
 (alist-get "a" '(("a" . 1)) nil nil #'equal)
 (alist-get 'z '((a . 1)) 'dflt)
 ;; The printer.  `print-length' and `print-level' did not exist, so both
 ;; were ignored -- and they are the only bound on output size, so a
 ;; circular structure printed until something gave out.  Symbol escaping
 ;; was per-character only, so a symbol whose whole NAME reads as a number,
 ;; or as the dot of a dotted pair, printed as that: (intern "12") printed
 ;; 12, which reads back as the integer.  A print-then-read round trip
 ;; silently changed the type, which is what the round-trip cases below are
 ;; really testing.
 (let ((print-length 2)) (prin1-to-string '(1 2 3 4)))
 (let ((print-length 2)) (prin1-to-string [1 2 3 4]))
 (let ((print-length 2)) (prin1-to-string '((1 2 3) (4 5 6) (7 8 9))))
 (let ((print-length nil)) (prin1-to-string '(1 2 3)))
 (let ((print-level 2)) (prin1-to-string '(1 (2 (3 (4))))))
 ;; print-level bounds LIST nesting only; a vector prints in full
 (let ((print-level 2)) (prin1-to-string [1 [2 [3 [4]]]]))
 (prin1-to-string (intern "12"))
 (prin1-to-string (intern "."))
 (prin1-to-string (intern ""))
 (prin1-to-string (intern "a b"))
 (prin1-to-string 'abc)
 (let ((s (intern "12"))) (eq (car (read-from-string (prin1-to-string s))) s))
 (let ((s (intern "."))) (eq (car (read-from-string (prin1-to-string s))) s))
 (let ((s (intern "a b"))) (eq (car (read-from-string (prin1-to-string s))) s))
 (type-of (car (read-from-string (prin1-to-string (intern "12")))))
 (equal (car (read-from-string (prin1-to-string "a\"b"))) "a\"b")
 (equal (car (read-from-string (prin1-to-string '(1 "a" b)))) '(1 "a" b))
 ;; An index outside a sequence used to answer nil, which is
 ;; indistinguishable from a slot that genuinely holds nil -- so reading
 ;; past the end was a silent wrong answer and a handler written for
 ;; `args-out-of-range' never fired.  `intern' took a symbol and returned
 ;; it, because the name buffer of a Symbol reads just like a Str's.
 (condition-case e (aref [1 2 3] 5) (error e))
 (condition-case e (aref [1 2 3] -1) (error e))
 (condition-case e (aref "abc" 5) (error e))
 (condition-case e (elt "abc" 5) (error e))
 (condition-case e (elt [1 2 3] 5) (error e))
 (aref [1 2 3] 1)
 (aref "abc" 1)
 (condition-case e (intern 'foo) (error e))
 (intern "nl-diff-interned")
 ;; maphash reaches every entry
 (let ((h (make-hash-table :test 'equal)) (n 0))
   (puthash "b" 2 h) (puthash "a" 1 h) (puthash "c" 3 h)
   (maphash (lambda (_k v) (setq n (+ n v))) h)
   n)
 ;; --- 2026-08-19, found by an Emacs-parity sweep rather than by hand ---
 ;; Each of these answered something Emacs does not, and answered it
 ;; silently: no error, no warning, just a different value.
 (list (car nil) (cdr nil)
       (condition-case e (car 5) (wrong-type-argument (cdr e)))
       (condition-case e (cdr 5) (wrong-type-argument (cdr e))))
 (condition-case e (symbol-value 'nelisp-parity-unbound-zz) (error e))
 (list (zerop 0.0) (zerop -0.0) (zerop 0) (zerop 1.5)
       (condition-case e (zerop "a") (wrong-type-argument (cdr e))))
 (list (round 0.5) (round 1.5) (round 2.5) (round -0.5) (round -1.5)
       (round 2.4) (round 7 2) (round -7 2) (round 5))
 (list (isnan (/ 0.0 0.0)) (isnan 1.0))
 (let ((l (list 1 2 3))) (list (nbutlast l 1) l (nbutlast (list 1 2) 9)))
 (upcase-initials "hello wORLD")
 (list (string-trim-left "xxab" "x+") (string-trim-right "abxx" "x+")
       (string-trim-left "  ab") (string-trim-right "ab  ")
       (string-trim-left "ab" "x+"))
 (list (format-message "`%s'" "q") (format-message "no quotes"))
 (list (intern-soft "zz-never-interned-parity") (intern-soft 'car))
 ;; The regexp engine: shy groups, explicit numbering, word boundaries,
 ;; non-greedy quantifiers, folding, and \N in a replacement.
 (list (string-match "\\(?:x+\\)" "xxab")
       (string-match "\\`\\(?:x+\\)" "xxab")
       (progn (string-match "\\(?2:x\\)\\(?1:a\\)" "xxab") (match-string 1 "xxab"))
       (progn (string-match "a.*?b" "axbxb") (match-string 0 "axbxb"))
       (string-match "\\ba" " a")
       (let ((case-fold-search t)) (string-match "A" "a"))
       (let ((case-fold-search nil)) (string-match "A" "a"))
       (let ((case-fold-search t)) (string-match "[a-z]" "A"))
       (let ((case-fold-search t)) (string-match "[^a-z]" "A")))
 (list (replace-regexp-in-string "\\(a\\)" "[\\1]" "a")
       (replace-regexp-in-string "a" "[\\&]" "a")
       (replace-regexp-in-string "\\(a\\)" "[\\1]" "a" nil t))
 (list (error-message-string '(error "m"))
       (error-message-string '(wrong-type-argument listp 5))
       (error-message-string '(args-out-of-range "abc" 0 9))
       (error-message-string '(arith-error))
       (error-message-string '(user-error "u")))
 (list (macroexpand-1 '(when t 1)) (macroexpand-1 '(unless t 1)))
 (list (length= (list 1 2) 2) (length< (list 1 2) 3) (length> (list 1 2) 1)
       (file-name-concat "a" "b") (string-distance "ab" "ac")
       (let ((case-fold-search t)) (char-equal ?a ?A)))

 ;; --- 2026-08-19, second parity sweep ---
 (list (condition-case e (error "msg %d" 1) (error e))
       (condition-case e (error "n=%s" 'x) (error (error-message-string e))))
 (list (concat '(97 98)) (concat [97 98]) (concat "a" '(98) [99]) (concat))
 (list (condition-case e (substring "abc" 0 9) (error e))
       (condition-case e (substring "abc" 9) (error e))
       (condition-case e (substring "abc" 2 1) (error e))
       (substring "abc" -2) (substring "abc" 1 nil) (substring "abc" 3)
       (substring "abc" 1 -1) (substring "あいう" 1 2)
       (condition-case e (substring [1 2 3] 0 9) (error e)))
 ;; Case mapping over the ranges the prelude claims: ASCII, Latin-1, Latin
 ;; Extended-A, Greek, Cyrillic.  Outside them a character passes through,
 ;; which is a stated limit -- the CJK case is here to pin that, not to
 ;; claim coverage.
 (list (upcase "aé") (downcase "AÉ") (upcase "αβγ") (downcase "ΑΒΓ")
       (upcase "абв") (downcase "АБВ") (upcase "āăą") (downcase "ĀĂĄ")
       (upcase "ß") (upcase ?ß) (upcase "あい") (capitalize "éa bÉ")
       (upcase-initials "héllo wORLD"))
 (list (string-to-number "1.5") (string-to-number "ff" 16)
       (string-to-number "12abc") (string-to-number "") (string-to-number "-1.5e3")
       (string-to-number "  12") (string-to-number "101" 2) (string-to-number "1e3")
       (string-to-number ".5") (string-to-number "-.5") (string-to-number "+3")
       (string-to-number "1.") (string-to-number "-2.") (string-to-number "0x10"))
 (list (pcase 5 ((or (and (pred integerp) n) n) n))
       (pcase 3 ((or 1 2 n) n))
       (pcase "s" ((or (pred integerp) (pred stringp)) 'ok)))
 (list (key-description (kbd "C-x")) (key-description (kbd "C-x C-f"))
       (key-description (kbd "M-x")) (key-description (kbd "SPC"))
       (key-description (kbd "a b")))

 ;; `sxhash' values are explicitly unspecified by Emacs, so what is compared
 ;; is the contract -- `equal' objects hash equal, and the answer is an
 ;; integer -- not the numbers.
 (list (= (sxhash-equal (list 1 2)) (sxhash-equal (list 1 2)))
       (= (sxhash-equal "ab") (sxhash-equal "ab"))
       (= (sxhash-equal (vector 1 "a")) (sxhash-equal (vector 1 "a")))
       (integerp (sxhash 5)) (integerp (sxhash-eq 'a)) (integerp (sxhash-eql 1.5)))

 ;; `condition-case' :success -- the clause was inert, so the protected form's
 ;; value came back instead of the handler's, which is exactly what a
 ;; `condition-case' with no handler answers.
 (list (condition-case e 5 (:success (list 'ok e)))
       (condition-case nil 5 (:success 'ran))
       (condition-case e 5 (error 'err) (:success (list 'ok e)))
       (condition-case e 5 (:success (list 'ok e)) (error 'err))
       (condition-case e (error "x") (error 'err) (:success 'ok))
       (condition-case e 5 (:success 1 2 (list 'v e)))
       (condition-case a (condition-case b 7 (:success (* b 2))) (:success (+ a 1)))
       (let ((e 99)) (list (condition-case e 5 (:success e)) e)))

 ;; `regexp-opt' is Emacs's algorithm now, so the generated TEXT is compared,
 ;; not just what it matches.
 (list (regexp-opt '("ab" "ac")) (regexp-opt '("abc")) (regexp-opt '())
       (regexp-opt '("a" "b" "c")) (regexp-opt '("a" "b" "c" "d" "e" "f"))
       (regexp-opt '("a" "bc" "b" "cd")) (regexp-opt '("axz" "byz"))
       (regexp-opt '("" "a" "ab")) (regexp-opt '("ab" "ac") t)
       (regexp-opt '("ab" "ac") "\\(?1:") (regexp-opt '("if" "then" "else") 'words)
       (regexp-opt '("car" "cdr") 'symbols)
       (regexp-opt '("defun" "defvar" "defmacro" "defconst"))
       (regexp-opt '("a." "a*" "a+")) (regexp-opt '("]" "^" "-" "a"))
       (regexp-opt '("ad" "d")) (regexp-opt '("alpha" "alpine" "alps" "beta" "betamax"))
       (regexp-opt '("0" "1" "2" "3" "7" "8" "9")))
 ;; Word and symbol edges, and syntax classes: `_' is a symbol constituent,
 ;; not a word one, which moves every \b boundary that touches it.
 (list (string-match "\\bx" "_x") (string-match "\\bx" " x") (string-match "\\bx" "ax")
       (string-match "\\w" "_") (string-match "\\w" "a")
       (string-match "\\<x" "_x") (string-match "\\<x" " x") (string-match "\\<x" "ax")
       (string-match "x\\>" "x ") (string-match "x\\>" "xa") (string-match "x\\>" "x_")
       (string-match "\\_<x" "_x") (string-match "\\_<x" " x")
       (string-match "x\\_>" "x_") (string-match "x\\_>" "x ")
       (string-match "\\Sw" "a") (string-match "\\W" "a") (string-match "\\W" " ")
       (string-match (regexp-opt '("if" "then") 'words) "x then y"))

 ;; Arguments that were accepted and ignored -- the shape `make
 ;; partial-inventory' now enumerates.
 (list (string-trim "xxaxx" "x+" "x+") (string-trim "xxa" "x+") (string-trim "  a  ")
       (seq-mapcat #'list '(1 2) 'vector) (seq-mapcat #'list '(1 2))
       (split-string "a,,b" "," t) (split-string " a , b " "," nil " +"))

 ;; Calling a non-function ENDED THE PROCESS -- uncatchable, so nothing
 ;; downstream could defend against it.  Found by `make parity-fuzz'.
 (list (condition-case e (funcall "abc" 1) (error e))
       (condition-case e (funcall 1 1) (error e))
       (condition-case e (mapcar "abc" (list 1)) (error e))
       (condition-case e (apply "abc" (list 1)) (error e))
       (condition-case e (funcall 'no-such-fn-zz 1) (error e))
       (funcall 'car (list 1 2))
       (funcall (lambda (x) (* x 2)) 21))

 ;; Arithmetic read a non-number through the value word of its Sexp and
 ;; answered with whatever sat there: (* "" 0) was 0.  Every operator now
 ;; names `number-or-marker-p', as Emacs does.
 (list (condition-case e (* "" 0) (error e))
       (condition-case e (+ 1 "a") (error e))
       (condition-case e (- nil 1) (error e))
       (condition-case e (/ "a" 2) (error e))
       (condition-case e (/= 0 "a") (error e))
       (condition-case e (= 1 "a") (error e))
       (condition-case e (< 1 'x) (error e))
       (condition-case e (1+ "a") (error e))
       (condition-case e (mod "a" 2) (error e)))
 (list (+ 1 2 3) (+) (- 10 3) (- 5) (* 2 3 4) (*) (/ 7 2) (/ 7.0 2)
       (mod -7 2) (% -7 2) (1+ 5) (1- 5) (1+ 1.5) (= 1 1.0) (< 1 2 3)
       (>= 3 2) (/= 1 2) (+ 1 2.5) (* 2 1.5))

 ;; String functions signalled `sequencep' where Emacs signals `stringp',
 ;; because the first thing they touch is `length'.  A handler written for
 ;; the documented condition never fired.
 (list (condition-case e (string-replace "a" '(1) ["b"]) (error e))
       (string-replace "a" "b" "aaa")
       (condition-case e (base64-decode-string 0 "x" "y") (error e))
       (base64-decode-string "YWJj")
       (condition-case e (read-from-string "") (error (car e)))
       (condition-case e (read-from-string "  ") (error (car e)))
       (read-from-string "(1 2)") (read-from-string "nil") (read-from-string "xx(1)" 2)
       (list (sequencep 0) (sequencep "a") (sequencep '(1)) (sequencep [1]) (sequencep nil))
       (seq-reverse "ab") (seq-reverse '(1 2)) (seq-reverse [1 2])
       (last t) (last '(1 2 3)) (last '(1 2 3) 2) (last nil)
       (condition-case e (capitalize nil) (error e)) (capitalize "ab cd")
       (condition-case e (mapcan #'list :key) (error e)) (mapcan #'list '(1 2)))

 ;; Predicate names, from `make parity-fuzz': each of these answered rather
 ;; than signalling, or signalled a predicate a handler would not match.
 (list (condition-case e (elt 0 '(1 2)) (error e))
       (condition-case e (elt 'foo 0) (error e))
       (elt '(1 2 3) 1) (elt [1 2 3] 2) (elt "abc" 1) (elt nil 0)
       (condition-case e (concat t) (error e))
       (condition-case e (concat 'foo) (error e))
       (concat "a" '(98) [99]) (concat nil) (concat)
       (condition-case e (hash-table-count 0) (error e))
       (hash-table-count (make-hash-table))
       (condition-case e (string-as-multibyte 0) (error e))
       (string-as-multibyte "ab")
       (condition-case e (string-make-unibyte 0) (error e))
       (condition-case e (nthcdr "" '(1)) (error e))
       (condition-case e (nth "" '(1)) (error e))
       (nthcdr 1 '(1 2 3)) (nth 1 '(1 2 3)) (nthcdr -1 '(1 2)))

 ;; `symbolp' checks.  `get' and `boundp' answered nil, which is also the
 ;; answer for a symbol that simply has no such property or binding, and
 ;; `fmakunbound'/`defalias'/`fset' answered their argument.
 (list (condition-case e (fmakunbound "ABC") (error e))
       (condition-case e (get -7 'k) (error e))
       (condition-case e (put 1 'k 'v) (error e))
       (condition-case e (set "s" 1) (error e))
       (condition-case e (boundp 1) (error e))
       (condition-case e (defalias 100 'car) (error e))
       (condition-case e (fset 100 'car) (error e))
       (condition-case e (format 65) (error e))
       (condition-case e (file-name-directory 0) (error e))
       (condition-case e (string-trim-right nil) (error e))
       (condition-case e (string-trim-left nil) (error e)))
 (list (progn (put 'zzq 'k 'v) (get 'zzq 'k)) (get 'zzq-none 'k) (boundp 'features)
       (progn (set 'zzv 7) (symbol-value 'zzv)) (format "%s-%d" "a" 3)
       (file-name-directory "/a/b") (file-name-directory "b")
       (string-trim-right "ab  ") (string-trim-left "  ab")
       ;; `fmakunbound' unbinds by fsetting nil, and a nil function cell is
       ;; NOT fbound -- reporting t made it look like it had done nothing.
       (progn (defun zzf () 1) (fmakunbound 'zzf) (fboundp 'zzf))
       (let ((s 'zz-alias-a)) (defalias s 'car) (list (fboundp 'zz-alias-a) (fboundp 's)))
       (progn (defalias 'zz-alias-d (lambda (x) (* x 3))) (zz-alias-d 5)))

 ;; `plistp', `obarrayp', and a `locate-file' that was simply absent.
 ;; `plist-get' is deliberately the lenient one -- Emacs answers nil there
 ;; and signals for the other two.
 (list (condition-case e (plist-member "s" 'k) (error e))
       (plist-get "s" 'k)
       (condition-case e (plist-put "s" 'k 1) (error e))
       (plist-member '(:a 1 :b 2) :b) (plist-get '(:a 1) :a) (plist-get '(:a 1) :z)
       (plist-put (list :a 1) :b 2) (plist-get nil :a)
       (condition-case e (special-variable-p "s") (error e))
       (condition-case e (intern-soft "x" 65) (error e))
       (intern-soft "car") (intern-soft "zz-never-zz")
       (list (obarrayp 1) (obarrayp [1 2]))
       (condition-case e (locate-file 7 [] 1) (error e))
       (condition-case e (locate-file nil '("a")) (error e))
       (locate-file "zz-no-such-file" '("/tmp")))

 ;; Index arguments name `integerp', and the stubs for subsystems this
 ;; runtime does not have (timers, processes, coding systems) reject a
 ;; wrong-typed argument rather than answering as if they had worked.
 (list (condition-case e (read-from-string "abc" "x") (error e))
       (condition-case e (read-from-string "abc" 0 "x") (error e))
       (read-from-string "(1)") (read-from-string "xx(1)" 2)
       (condition-case e (memq 1 -1.5) (error e))
       (memq 2 '(1 2 3)) (memq 9 '(1 2)) (memq 1 nil)
       (condition-case e (encode-coding-string "a" 12354) (error e))
       (condition-case e (decode-coding-string "a" 12354) (error e))
       (encode-coding-string "ab" 'utf-8) (decode-coding-string "ab" 'utf-8)
       (condition-case e (copy-hash-table "x") (error e))
       (let ((h (make-hash-table))) (puthash 1 2 h) (gethash 1 (copy-hash-table h)))
       (condition-case e (cancel-timer "x") (error e))
       (condition-case e (process-put "x" 'k 1) (error e)))

 ;; One helper per PREDICATE, not one per function: Emacs names a specific
 ;; predicate for each requirement and a handler matches on that name, so
 ;; getting the name wrong is the same failure as not checking at all.
 (list (condition-case e (ash "" 3) (error e)) (ash 8 -1)
       (condition-case e (logand "x" 1) (error e)) (logand 12 10)
       (condition-case e (logior '("a") 1) (error e)) (logior 12 10)
       (condition-case e (string-to-char 65) (error e)) (string-to-char "abc")
       (condition-case e (char-to-string '(1 2)) (error e)) (char-to-string 97)
       (condition-case e (gethash 1 'foo) (error e))
       (condition-case e (string-empty-p 12354) (error e)) (string-empty-p "")
       (condition-case e (int-to-string '((a . 1))) (error e)) (int-to-string 42)
       (condition-case e (string nil) (error e)) (string 97 98)
       (condition-case e (fceiling ["a"]) (error e)) (fceiling 1.2)
       (condition-case e (file-name-absolute-p -1) (error e)) (file-name-absolute-p "/a")
       (condition-case e (string-pad "ab" -1.5) (error e)) (string-pad "ab" 4)
       (condition-case e (add-to-list 1 'x) (error e))
       (condition-case e (frame-height -1.5) (error e))
       (condition-case e (generate-new-buffer ["a"]) (error e))
       (condition-case e (set-file-modes "/tmp/x" '(1)) (error e))
       (condition-case e (rename-file '(1 2) "b") (error e))
       (condition-case e (seq-do #'identity 3) (error e)) (seq-do #'identity '(1 2))
       (condition-case e (seq-position 2 1) (error e)) (seq-position '(1 2 3) 2))

 ;; (symbol-name nil) answered "" -- nil and t are their own tags here, not
 ;; Symbols, so the name pointer and length are both zero.  It reaches
 ;; further than it looks: `string-equal' compares a symbol BY ITS NAME, so
 ;; (string= nil "") was t here and nil in Emacs, and `string-empty-p'
 ;; inherited that.
 (list (symbol-name nil) (symbol-name t) (symbol-name 'foo) (length (symbol-name nil))
       (string-equal nil "") (string-equal nil "nil") (string-equal 'foo "foo")
       (condition-case e (string-empty-p nil) (error e))
       (condition-case e (string-empty-p 12354) (error e))
       (string-empty-p "") (string-empty-p 'foo))

 ;; NOTE: `cl-defstruct' with a leading docstring is NOT a case here.  It was
 ;; taken for a slot name until today, and it only worked because
 ;; `symbol-name' answered for a string -- the moment that signalled
 ;; `symbolp', every `cl-defstruct' with a docstring stopped compiling, which
 ;; is how it was found.  It cannot be compared here because `cl-defstruct'
 ;; does not exist in `emacs -Q'; `standalone-reader-test' covers it instead.

 ;; More predicate names.  `%' is INTEGER remainder, so it names
 ;; `integer-or-marker-p' where the general arithmetic ops name
 ;; `number-or-marker-p' -- a float is rejected there and accepted by `mod'.
 (list (condition-case e (base64-encode-string 1) (error e)) (base64-encode-string "abc")
       (condition-case e (directory-file-name 1) (error e)) (directory-file-name "/a/b/")
       (condition-case e (file-name-as-directory 1) (error e)) (file-name-as-directory "a")
       (condition-case e (file-name-nondirectory 1) (error e)) (file-name-nondirectory "/a/b")
       (condition-case e (file-name-sans-extension 1) (error e)) (file-name-sans-extension "a.b")
       (condition-case e (regexp-quote 1) (error e)) (regexp-quote "a.b")
       (condition-case e (format-message 1) (error e)) (format-message "`a'")
       (condition-case e (string-blank-p 1) (error e)) (string-blank-p "  ")
       (condition-case e (assoc 1 "s") (error e)) (assoc 'a '((a . 1)))
       (condition-case e (member 1 "s") (error e)) (member 2 '(1 2))
       (condition-case e (expt "a" 2) (error e)) (expt 2 10)
       (condition-case e (floor '(1 2 3)) (error e)) (floor 1.7)
       (condition-case e (truncate t) (error e)) (truncate -1.5)
       (condition-case e (mod :key 2) (error e)) (mod -7 2)
       (condition-case e (% "a" 2) (error e)) (% -7 2)
       (condition-case e (round "a") (error e)) (round 2.5))

 ;; `process-status' is the lenient one -- Emacs accepts a buffer or a
 ;; process NAME, so "not a process object" is nil there, not an error.
 ;; `process-put' next to it signals.  Two contracts, not one.
 (list (condition-case e (seq-elt 3 0) (error e)) (seq-elt '(1 2 3) 1)
       (process-status "x")
       (condition-case e (mapcar #'identity 3) (error e)) (mapcar #'1+ '(1 2))
       (condition-case e (regexp-opt "s") (error e)) (regexp-opt '("ab" "ac"))
       (condition-case e (file-truename 1) (error e))
       (condition-case e (buffer-file-name "a") (error e)) (buffer-file-name)
       (condition-case e (make-bool-vector [1] t) (error e))
       (condition-case e (log "a") (error e))
       (< (abs (- (log 2.718281828459045) 1.0)) 1e-12)
       (< (abs (- (log 8 2) 3.0)) 1e-12)
       (condition-case e (generate-new-buffer ["a"]) (error e)))

 ;; `remove' is type-preserving: answering a list of character codes for a
 ;; string is a different TYPE flowing into whatever the caller does next.
 ;; `string-lessp' takes a string OR a symbol, like `string='.
 ;; NOTE: `bool-vector' is NOT compared here -- Emacs prints #&3"" for a
 ;; distinct object type this runtime does not have (recorded in
 ;; tools/partial-accepted.txt); the values agree under `aref' and `length'.
 (list (condition-case e (string-lessp 1 "a") (error e))
       (string-lessp 'a "b") (string-lessp "a" "b")
       (condition-case e (string-greaterp 1 "a") (error e)) (string-greaterp "b" "a")
       path-separator
       (remove ?a "abc") (remove 2 [1 2 3]) (remove 2 '(1 2 3)) (remove 9 '(1 2))
       (help-add-fundoc-usage "Doc." '(a b)) (help-add-fundoc-usage nil '(x)))

 ;; `fboundp' answered t for a name that has only a VARIABLE binding: the
 ;; mirror keeps a function-cell entry holding an unbound sentinel.  Anything
 ;; that probes before calling -- most defensive elisp -- then called
 ;; something that was never a function.
 (list (progn (defconst zz-par-dual "V") (defun zz-par-dual () "F")
              (list zz-par-dual (fboundp 'zz-par-dual) (zz-par-dual)))
       (progn (defconst zz-par-var "V2") (fboundp 'zz-par-var))
       (condition-case e (funcall nil) (error e))
       (condition-case e (funcall 'nosuch-zz-fn) (error e))
       (prin1-to-string nil 7) (prin1-to-string "a\"b") (prin1-to-string "ab" t)
       (terpri nil nil) (terpri)
       (condition-case e (memql -1 '(1 2 . 3)) (error e))
       (memql 2 '(1 2 3)) (memql 9 '(1 2))
       (condition-case e (memql 1 "s") (error e))
       (path-separator) path-separator)

 ;; Index arguments name `fixnump'/`integerp'; a STREAM that is not a
 ;; function is an error rather than being ignored -- output was going
 ;; somewhere the caller did not ask for and nothing said so.
 (list (condition-case e (elt "abc" '(1 2)) (error e)) (elt "abc" 1)
       (condition-case e (substring "abc" t) (error e)) (substring "abc" 1)
       (condition-case e (unibyte-string nil) (error e)) (unibyte-string 97 98)
       (condition-case e (symbol-value 2) (error e))
       (condition-case e (set-file-modes '(1 2) '(1)) (error e))
       (help-split-fundoc "abc" 'foo) (help-split-fundoc nil 'foo)
       (bool-vector-p :key) (point-min) (locate-library "zz-no-such-lib-zz")
       (condition-case e (seq-position '(1 . 2) 48) (error e)) (seq-position '(1 2 3) 2)
       (condition-case e (char-uppercase-p "a") (error e))
       (char-uppercase-p ?A) (char-uppercase-p ?a)
       (string-version-lessp nil "x") (string-version-lessp "a1" "a2")
       (set-buffer-multibyte "abc") (set-buffer-multibyte nil)
       (define-error 'zz-par-err "msg")
       (condition-case e (princ 97 "s") (error e))
       (condition-case e (prin1 1.5 -7) (error e))
       (condition-case e (print 32 0) (error e))
       (condition-case e (write-char 65 :key) (error e)))

 ;; The bitwise predicate is POSITION-dependent, not type-dependent:
 ;; (logand "x" 1) names `integer-or-marker-p' and (logior 1 "s") names
 ;; `number-or-marker-p'.  Reasoning from "which check fails first" gets
 ;; argument one wrong; only running it says so.
 (list (> '((a . 1))) (condition-case e (min '(1 2) '(1)) (error e))
       (last ["a"] -1) (condition-case e (plist-put -1 ["a"] "e") (error e))
       (kill-buffer) (assoc-string 0 0.0 65)
       (string-version-lessp nil "x")
       (condition-case e (string-split '(1 . 2) -1 :key) (error e))
       (condition-case e (regexp-opt 32) (error e))
       (condition-case e (mod :key 0) (error e))
       (condition-case e (mod 5 0) (error e)) (mod -7 2)
       (condition-case e (logior 12354 '("a")) (error e))
       (condition-case e (logand "x" 1) (error e))
       (condition-case e (logior 1 1.5) (error e))
       (condition-case e (% "a" 2) (error e))
       (locate-file "" '(1 2 3) "x")
       (condition-case e (char-to-string -1) (error e)) (char-to-string 97)
       (condition-case e (intern "x" 1) (error e)))

 ;; An improper list: `rassq' names the whole ARGUMENT and the seq- walkers
 ;; name the TAIL they hit.  Measured -- they genuinely differ, and one rule
 ;; for both would be wrong for one of them.
 (list (condition-case e (process-status -1) (error e)) (process-status "x")
       (condition-case e (json-serialize "s" "k") (error e)) (json-serialize "s")
       ;; `byte-compile-file' is NOT compared: src/nelisp-bytecode.el has a
       ;; real one that the standalone does not load, and a prelude stub
       ;; that answers nil would shadow it on the host.  Leaving it
       ;; void-function in the standalone is the honest state.
       (condition-case e (string-version-lessp 48 97) (error e))
       (string-version-lessp "a1" "a2")
       (condition-case e (plist-put -1 ["a"] "e") (error e))
       (plist-put (list :a 1 :b 2) :b 3)
       (condition-case e (rassq ["a"] '(1 . 2)) (error e)) (rassq 1 '((a . 1)))
       (condition-case e (seq-do #'identity '(1 2 . 3)) (error e))
       (seq-do #'identity '(1 2))
       (condition-case e (seq-position '(1 . 2) 48) (error e))
       (condition-case e (seq-reverse [] 1) (error (car e)))
       (condition-case e (seq-empty-p '(1 . 2) -1) (error (car e)))
       (condition-case e (file-truename nil 'x 0.0) (error e))
       (condition-case e (generate-new-buffer ["a"] nil) (error e)))

 ;; `(and (vectorp X) (eq (aref X 0) 'tag))' is unsafe for an EMPTY vector:
 ;; `aref' signals args-out-of-range, so six predicates written that way
 ;; reported a range error where they should have answered nil.
 (list (processp []) (process-live-p '(1 2 . 3))
       (condition-case e (process-status []) (error e))
       (condition-case e (maphash #'ignore 'x) (error e))
       (condition-case e (file-executable-p nil) (error e))
       (condition-case e (file-writable-p ["a"]) (error e))
       (condition-case e (make-list "x" []) (error e)) (make-list 2 'a)
       (condition-case e (make-string 3 -1) (error e)) (make-string 2 ?a)
       (condition-case e (exp '("a")) (error e))
       (< (abs (- (exp 1.0) 2.718281828459045)) 1e-12) (exp 0)
       (condition-case e (lsh 48 "a") (error e)) (lsh 8 -1)
       (condition-case e (symbol-plist "x") (error e))
       (condition-case e (setplist "a" 7) (error e))
       (condition-case e (make-temp-name nil) (error e))
       (functionp t) (functionp nil) (functionp 'car)
       (condition-case e (mapc #'identity 0) (error e)) (mapc #'identity '(1 2))
       (condition-case e (string-pad nil '(1 . 2)) (error e)))

 ;; `string-version-lessp' is NOT `string-lessp': digit runs compare as
 ;; NUMBERS, and the character order is `.' `~' 0-9 A-Z a-z then everything
 ;; else -- so an alphanumeric sorts below a control character.  Derived by
 ;; sorting 1..127 with Emacs and reading the order off; delegating to
 ;; `string-lessp' was wrong for every pair this function exists for.
 (list (string-version-lessp nil "\t x") (string-version-lessp "a2" "a10")
       (string-version-lessp "a10" "a2") (string-version-lessp "a1b2" "a1b10")
       (string-version-lessp "n" "\t") (string-version-lessp "A" "a")
       (condition-case e (split-string 'x 1 "") (error e)) (split-string "a,b" ",")
       (condition-case e (file-truename "a" "b" nil) (error e))
       (condition-case e (run-at-time "ABC" [] []) (error e))
       (condition-case e (decode-char "x" t) (error e))
       (condition-case e (make-symbolic-link "a" '("b") -7) (error e))
       ;; `insert-file-contents-literally' is NOT compared: in `emacs -Q'
       ;; it reaches `buffer-read-only' before the file check, so the two
       ;; disagree about which error comes first for reasons that have
       ;; nothing to do with the file.
       )

 ;; The `f' rounding family requires a FLOAT: (ffloor 65) signals while
 ;; (floor 65) is 65.  Accepting any number, which an earlier pass did,
 ;; is the loose reading of a strict contract.
 (list (condition-case e (ffloor 65) (error e)) (ffloor 1.7)
       (condition-case e (fceiling 65) (error e))
       (condition-case e (fround 65) (error e))
       (condition-case e (ftruncate 65) (error e))
       (condition-case e (number-to-string 'x) (error e)) (number-to-string 42)
       (condition-case e (hash-table-test 2) (error e))
       (hash-table-test (make-hash-table :test 'equal))
       (condition-case e (remq ["a"] 'foo) (error e)) (remq 2 '(1 2 3))
       (condition-case e (featurep "a" '(1 2 . 3)) (error e))
       (featurep 'zz-no-feature)
       (condition-case e (logxor "x") (error e)) (logxor 12 10))

 ;; A STREAM that is not a function is an error for `read' and `terpri' too;
 ;; an unknown coding system is `coding-system-error', not a generic one.
 (list (condition-case e (read '((a . 1))) (error e)) (read "(1 2)")
       ;; `terpri' with ENSURE is NOT a case here: ENSURE means "only if not
       ;; already at column 0", so whether the STREAM is reached at all
       ;; depends on what was printed before it.  Emacs answers nil inside
       ;; this file and `invalid-function' on its own, for that reason.  The
       ;; one-argument form below is deterministic.
       (condition-case e (terpri '(1 . 2)) (error e))
       (condition-case e (run-at-time '(1) 48 :key) (error e))
       (condition-case e (decode-coding-string "ABC" t "x") (error e))
       (encode-coding-string "ABC" 'latin-1) (decode-coding-string "ab" 'utf-8)
       (condition-case e (buffer-substring-no-properties '(1 2 3) "x") (error e))
       (condition-case e (plist-put -1 ["a" "b"] "e") (error e))
       (condition-case e (lsh [] 97) (error e)) (condition-case e (lsh 1 1.5) (error e)))

 ;; `featurep' takes nil and t -- they ARE symbols, and rejecting them made
 ;; a feature probe signal.  `file-missing' reports the ABSOLUTE name; a
 ;; relative one leaves the reader guessing which directory the call was in.
 (list (condition-case e (featurep t 1) (error e)) (featurep 'zz-no-feature)
       ;; `insert-file-contents-literally' is NOT a case: in `emacs -Q' the
       ;; scratch buffer is read-only, so Emacs raises `buffer-read-only'
       ;; before it looks at the file.  The two disagree about which error
       ;; comes first for a reason that has nothing to do with the file, and
       ;; the file-missing data itself is checked by hand above.
       )

 ;; `upcase'/`downcase'/`capitalize' take a CHARACTER or a string, so a
 ;; float or a negative is neither -- answering the argument hid that.
 (list (condition-case e (abs nil) (error e)) (abs -3)
       (condition-case e (float [1 2 3]) (error e)) (float 3)
       (condition-case e (frame-width 0.0) (error e))
       (condition-case e (char-equal 7 "e") (error e)) (char-equal ?a ?a)
       (condition-case e (assq-delete-all t "ab") (error e))
       (condition-case e (rassoc ["a"] "x") (error e)) (rassoc 1 '((a . 1)))
       (condition-case e (assq 32 [1 2 3]) (error e)) (assq 'a '((a . 1)))
       (condition-case e (process-get nil 1) (error e))
       (condition-case e (take "ABC" "x") (error e)) (take 2 '(1 2 3))
       (condition-case e (butlast 97 '(1 2 . 3)) (error e)) (butlast '(1 2 3))
       (condition-case e (make-temp-file 'foo) (error e))
       (condition-case e (capitalize -1) (error e))
       (condition-case e (downcase 1.5) (error e))
       (condition-case e (upcase 1.5) (error e)))

 ;; `string-trim-left' and `-right' disagree about which predicate a bad
 ;; REGEXP names -- LEFT walks it and reports the cdr as `listp', RIGHT
 ;; reports the regexp itself as `sequencep'.  One rule for both is wrong
 ;; for one of them.
 (list (condition-case e (error-message-string 1.5) (error e))
       (error-message-string '(error "m"))
       (condition-case e (macroexp--fgrep "x" :key) (error e))
       (condition-case e (fmakunbound nil) (error e))
       (condition-case e (fmakunbound t) (error e))
       (prefix-numeric-value "e") (prefix-numeric-value 3) (prefix-numeric-value '(4))
       (equal-including-properties 32 -1.5) (equal-including-properties "a" "a")
       (condition-case e (string-equal-ignore-case "e" '(1 2 . 3)) (error e))
       (string-equal-ignore-case "AB" "ab")
       (condition-case e (seq-into '(1 2 . 3) -7) (error e)) (seq-into '(1 2) 'vector)
       (condition-case e (seq-concatenate "x") (error e))
       (condition-case e (string-trim-left '(1 2 3) '(1 . 2)) (error e))
       (condition-case e (string-trim-right 7 2) (error e)))

 ;; Arithmetic that walked off the end of its own argument list.  (/ 7) is
 ;; the RECIPROCAL, and reading a second argument that was not there
 ;; segfaulted -- an abort no `condition-case' could see, from a call Emacs
 ;; answers.  `/' also folds over EVERY divisor, and integer division by
 ;; zero traps in hardware unless it is checked first.
 (list (/ 7) (/ 2.0) (/ 100 5 2) (/ 100.0 5 2) (/ 1.0 0)
       (condition-case e (/ 1 0) (error e))
       (condition-case e (% 1 0) (error e))
       (condition-case e (mod 1 0) (error e))
       (symbol-value nil) (symbol-value t)
       (condition-case e (lognot "x") (error e))
       (condition-case e (logand nil 0.0) (error e))
       (condition-case e (logand 1 nil) (error e))
       (condition-case e (logand "x" 1) (error e))
       (condition-case e (ash 97 0.0) (error e)))

 ;; Predicate selection measured one function at a time: `sequencep' for a
 ;; non-sequence, `listp' for the tail an improper list stops on, and
 ;; `integerp' vs `fixnump' by what the sequence turned out to be.
 (list (condition-case e (string< t ["a"]) (error e))
       (condition-case e (string< 1 "a") (error e))
       (string< 'abc "abd") (string> "b" "a")
       (condition-case e (seq-elt "ab" t) (error e))
       (condition-case e (seq-elt '("a") 1.5) (error e))
       (condition-case e (seq-elt '(1 2 . 3) 5) (error e))
       (condition-case e (seq-count 'identity '(1 2 . 3)) (error e))
       (condition-case e (seq-map 'identity '(1 . 2)) (error e))
       (condition-case e (seq-map 'identity 1) (error e))
       (condition-case e (elt '(1 2) 1.5) (error e))
       (condition-case e (elt [1 2] 1.5) (error e))
       (condition-case e (memq 9 '(1 2 . 3)) (error e))
       (condition-case e (assoc "" '(1 . 2)) (error e))
       (condition-case e (nth 7 '(1 . 2)) (error e))
       (condition-case e (nthcdr 5 '(1 2 . 3)) (error e))
       (nthcdr 1 '(1 . 2)) (nthcdr 3 '(1 2 3 . 4)))

 ;; `seq-take'/`seq-drop' preserve the sequence type, check N before SEQ,
 ;; and carry the arity of the generic they are.
 (list (seq-take "abcd" 2) (seq-take [1 2 3] 2) (seq-take '(1 2 3) 2)
       (seq-take "a" -1) (seq-drop "abcd" 2) (seq-drop [1 2 3] 1)
       (seq-drop "abc" -1) (seq-subseq "abcd" 1 3) (seq-subseq [1 2 3] 1)
       (condition-case e (seq-take 12354 '(1 2 3)) (error e))
       (condition-case e (seq-take 12354 5) (error e))
       (condition-case e (seq-take "ab" 1.5) (error e))
       (condition-case e (seq-take 48) (error e))
       (condition-case e (seq-drop 12354) (error e))
       (condition-case e (seq-reverse 'foo -7) (error e))
       (condition-case e (seq-subseq 5 1) (error e)))

 ;; The rest of one afternoon's measurements: each of these answered
 ;; something plausible before.
 (list (condition-case e (make-hash-table nil '(1 2 . 3)) (error e))
       (condition-case e (make-hash-table :bogus 1) (error e))
       (condition-case e (make-hash-table :size) (error e))
       (condition-case e (make-hash-table :test 'nosuch) (error e))
       (condition-case e (defalias :key 'no-such-target-fn) (error e))
       (condition-case e (write-char '(1 2 . 3) t) (error e))
       (condition-case e (substring '(1) 32 'foo) (error e))
       (condition-case e (string-match-p "x" "a" 1.5) (error e))
       (condition-case e (macroexpand-all '(1 2 . 3) 32) (error e))
       (macroexpand-all 48 -1.5)
       (condition-case e (make-string "x" 65) (error e))
       (condition-case e (make-string 2 "x") (error e))
       (condition-case e (set t '(1)) (error e))
       (condition-case e (set :k 1) (error e))
       (max-char 97) (max-char) (frame-height) (frame-width)
       (multibyte-string-p "abc") (multibyte-string-p "\303\251")
       (condition-case e (concat "x" '(1 . 2)) (error e))
       (condition-case e (string-trim-left "abc" '(1 . 2)) (error e))
       (plist-member '(1 2 3) 97) (plist-member '(1 2 3) 3)
       (condition-case e (plist-member '(1 2 . 3) 97) (error e))
       (condition-case e (plist-member '(1 . 2) 97) (error e))
       (condition-case e (file-attribute-size '(1 . 2)) (error e))
       (condition-case e (byte-compile-file 7 "x") (error e))
       (condition-case e (byte-compile-file 'foo) (error e))
       (condition-case e (string-replace "" "a" "b") (error e))
       (sqrt 4) (sqrt 0)
       (funcall (apply-partially '+ 1 2) 3)
       (replace-regexp-in-string "a" "b" "xax")
       (replace-regexp-in-string nil "b" "")
       (replace-regexp-in-string "\\([a-z]\\)" "<\\1>" "ab")
       (replace-regexp-in-string "b" "\\&\\&" "abc")
       (replace-regexp-in-string "b" "z" "abc" nil t)
       (condition-case e (replace-regexp-in-string 0 t "ab") (error e)))

 ;; Objects that print as themselves.  A buffer came out as its backing
 ;; vector, a hash table as 2048 nils, and a closure as the list it is made
 ;; of -- each of them readable Lisp that means something else.  The last one
 ;; could not be printed at all: the printer's fallback formatted the object
 ;; with %S, which re-entered the printer on the same object.
 (list (make-hash-table) (make-hash-table :test 'equal)
       (hash-table-p '(1)) (hash-table-p (make-hash-table))
       (condition-case e (gethash 1 '(1 2 3)) (error e))
       (condition-case e (hash-table-test '(1)) (error e))
       (let ((h (make-hash-table))) (puthash 1 2 h) (list (gethash 1 h) (hash-table-count h)))
       (generate-new-buffer "x") (generate-new-buffer " a b ")
       ;; Forced lexical: this file is loaded with the default binding mode,
       ;; and a DYNAMIC closure captures nothing -- so the env would be nil
       ;; on the host and the case would say nothing about printing one.
       (eval '(let ((a 1)) (lambda () a)) t)
       (eval '(let ((a (list 1))) (lambda () a)) t))

 ;; Numerics that were close but not equal.  A float literal with more than
 ;; eighteen digits OVERFLOWED the reader's mantissa and came back about ten
 ;; times too small; sqrt and exp were each one ULP out.
 (list 1.44269504088896338700e+00 6.93147180369123816490e-01
       1.90821492927058770002e-10 1.66666666666666019037e-01
       (sqrt 2) (sqrt 32) (sqrt 4) (sqrt 123456.789) (sqrt 1e10)
       (exp -1.5) (exp 0) (exp -0.1)
       (condition-case e (sqrt t) (error e))
       (condition-case e (unibyte-string 300) (error e))
       (condition-case e (ash 97 0.0) (error e)))

 ;; One more afternoon of measurements, each one its own rule.
 (list (condition-case e (seq-partition '(1 2 3) 0) (error e))
       (seq-partition '(1 2 3) 2)
       (condition-case e (string-chop-newline -7) (error e))
       (condition-case e (string-chop-newline ["a" "b"]) (error e))
       (macroexpand-all '(1 2 . 3))
       (condition-case e (macroexpand-all '(1 2 . 3) 32) (error e))
       (macroexpand "aXbXc" "x") (macroexpand-1 0.0 -1)
       (condition-case e (number-sequence nil [1 2 3] "") (error e))
       (condition-case e (seq-mapn 1.5 '(1 . 2)) (error e))
       (condition-case e (string-match-p "ABC" "ab" 7) (error e))
       (condition-case e (string-match-p nil "ab" 0.0) (error e))
       (condition-case e (decode-char 'foo -7) (error e))
       (condition-case e (prin1-to-string "x" nil 3) (error e))
       (condition-case e (prin1-to-string "x" nil '(quote sym)) (error e))
       (condition-case e (prin1-to-string "x" nil '((a . 1))) (error e))
       (condition-case e (string-to-number "a" "b") (error e))
       (condition-case e (string-to-number "ABC" 97) (error e))
       (condition-case e (string-trim 65 nil '("a" "b")) (error e))
       (condition-case e (copy-sequence '(1 . 2)) (error e))
       (condition-case e (delete 1 '(1 . 2)) (error e))
       (condition-case e (plist-put '(1 . 2) 1 2) (error e))
       (condition-case e (alist-get 1 '(1 . 2)) (error e))
       (condition-case e (concat "x" '("a")) (error e))
       (condition-case e (string-to-list 5) (error e))
       (string-to-list '(1)) (string-to-list [1 2]) (string-to-list "ab")
       (condition-case e (string-suffix-p 1.5 0.0) (error e))
       (help-add-fundoc-usage 0 '("a" "b")) (help-add-fundoc-usage t t)
       (help-add-fundoc-usage "d" '(a &optional b))
       (json-serialize nil) (json-serialize '((a . 1) (b . "x")))
       (json-serialize '(:a 1 :b 2)) (json-serialize [1 "a" t])
       (condition-case e (json-serialize '(1 2 . 3)) (error e))
       (condition-case e (seq-empty-p "x" 0) (error e))
       (seq-sort #'< '(3 1 2)) (seq-sort #'< [3 1 2]) (seq-sort #'< "cab")
       (condition-case e (base64-decode-string "ABC") (error e))
       (condition-case e (base64-decode-string "A!BC") (error e))
       (base64-decode-string "AB==")
       ;; Raw-byte round trip.  Every base64 case above this line is ASCII,
       ;; which is exactly why a real defect survived here: the decoder built
       ;; each output byte with `char-to-string', so a byte >= 128 came back
       ;; as its two-byte UTF-8 form and the pair did not round-trip at all
       ;; for binary input.  ASCII never notices.  These do.
       ;;
       ;; Only DERIVED values are compared -- an encode result, an `equal', a
       ;; byte count -- never a decoded binary string: this gate diffs printed
       ;; output, and a raw byte string prints differently on the two sides for
       ;; reasons unrelated to the decoder.  A byte-by-byte list is also NOT
       ;; taken: `string-byte' does not exist in a real Emacs, and `append' on
       ;; a unibyte string answers (521 640 1 1835048 0 0) on this runtime
       ;; against (200 201 202 0 1 255) in Emacs -- a separate, real defect in
       ;; string-to-list conversion, out of scope here.  The `equal' case below
       ;; already fails if a single byte is wrong.
       (base64-encode-string (unibyte-string 200 201 202))
       (equal (unibyte-string 200 201 202)
              (base64-decode-string (base64-encode-string
                                     (unibyte-string 200 201 202))))
       (string-bytes (base64-decode-string
                      (base64-encode-string (unibyte-string 200 201 202 0 1 255))))
       (equal (apply #'unibyte-string (number-sequence 0 255))
              (base64-decode-string
               (base64-encode-string (apply #'unibyte-string
                                            (number-sequence 0 255)))))
       (condition-case e (seq-concatenate '(1 2 3) 'foo) (error e))
       (condition-case e (mapc t '(1 2 . 3)) (error e))
       (condition-case e (mapcar 7 '(1 . 2)) (error e))
       (condition-case e (assoc-string "" '(1) '(1)) (error e))
       (condition-case e (error-message-string nil) (error e))
       (condition-case e (error-message-string "") (error e))
       (error-message-string '(error "x"))
       (error-message-string '(wrong-type-argument stringp 1))
       (condition-case e (string-search "a" "bab" 1.5) (error e))
       (condition-case e (string-search "a" "bab" 9) (error e))
       (condition-case e (aref [1] "x") (error e))
       (condition-case e (seq-min []) (error e))
       (condition-case e (seq-max "") (error e))
       (condition-case e (min) (error e))
       (substring "abcd") (substring [1 2])
       (condition-case e (substring -7) (error e))
       (condition-case e (substring "abc" 1 "z") (error e)))

 ;; Two more seeds of the same sweep.  Several of these are the second half
 ;; of a rule whose first half was already fixed -- the predicate for
 ;; `seq-take''s N depends on whether SEQ is a list, `butlast' answers its
 ;; argument unlooked-at for a non-positive N, and nil and t are symbols
 ;; that `fset' has to accept even though they carry their own tags.
 (list (condition-case e (seq-take '((a . 1)) :key) (error e))
       (condition-case e (seq-take '(1) "abc") (error e))
       (condition-case e (seq-take 12354 '(1 2 3)) (error e))
       (condition-case e (seq-take 12354 5) (error e))
       (condition-case e (seq-drop '(1) :key) (error e))
       (butlast -7 -7) (condition-case e (butlast -7) (error e))
       (butlast '(1 2 3) 1)
       (buffer-live-p []) (buffer-live-p 5)
       (boundp nil) (boundp t) (boundp :k) (boundp 'no-such-var-xyz)
       (condition-case e (ceiling 65 0.0) (error e))
       (condition-case e (floor 1 0) (error e))
       (condition-case e (truncate 1 0) (error e))
       (condition-case e (round 1 0) (error e))
       (floor 7 2) (round 7 2) (ceiling 7 2)
       (assoc-string t '(1 . 2)) (assoc-string '(1) '(1) "")
       (assoc-string "a" '("a" "b"))
       (condition-case e (seq-mapcat 7 [] "abc") (error e))
       (seq-mapcat #'list '(1 2) 'vector)
       (condition-case e (json-serialize (list 'quote 'sym)) (error e))
       ;; No (fset t ...) here: it ASSIGNS t's function cell, and a later
       ;; case in this same list then calls t and reports a different error.
       ;; The cases have to be independent of the order they are read in.
       (condition-case e (fset nil 1) (error e))
       (condition-case e (fset 5 1) (error e))
       (condition-case e (defalias nil "x") (error e))
       (condition-case e (vconcat '(1 . 2)) (error e)) (vconcat '(1 2) [3])
       (condition-case e (string-to-list '(1 2 . 3)) (error e))
       (condition-case e (file-truename "abc" nil "[a-z]+") (error e))
       (condition-case e (file-truename "abc" nil '(1)) (error e))
       (condition-case e (lsh -1 t) (error e))
       (condition-case e (lsh 1 t) (error e))
       (condition-case e (lsh "a" 1) (error e))
       (condition-case e (delete 1 '(1 . 2)) (error e))
       (condition-case e (delete nil '(1 . 2)) (error e))
       (condition-case e (/ 1.5 nil) (error e))
       (condition-case e (macroexp--fgrep '(1 2 . 3) 48) (error e))
       (executable-find "")
       (string-replace "a.b*c" 7 "ABC") (string-replace "abc" '(1 2 . 3) "a")
       (condition-case e (string-suffix-p [1 2 3] "ABC" "x") (error e))
       (log 0.0) (log 2 -7) (log 8 2)
       (condition-case e (seq-mapn 't '(1 2 . 3)) (error e))
       (seq-mapn 48 nil) (seq-mapn #'+ '(1 2) '(3 4))
       (condition-case e (memql 1 '(1 2 . 3)) (error e))
       (memql 2 '(1 2 3))
       (condition-case e (number-sequence 1 '(1 2 3) ["a"]) (error e))
       (number-sequence '(quote sym)) (number-sequence 1 3)
       (special-variable-p nil) (special-variable-p 'foo)
       (prefix-numeric-value '((a . 1))) (prefix-numeric-value '(4))
       (condition-case e (plist-put '(1) 100 32) (error e))
       (plist-member '(1) 100)
       (seq-partition "abc" 2) (seq-partition [1 2 3] 2)
       (condition-case e (unibyte-string -1 (list 'quote 'sym)) (error e))
       (condition-case e (string-chop-newline -7) (error e))
       (string-chop-newline nil) (string-chop-newline "a\n"))

 ;; The long tail, one seed at a time.  Several are the SECOND half of a
 ;; rule: `fround' rounds a half to even like `round' (not away from zero),
 ;; `seq-take' checks N differently depending on whether SEQ is a list, and
 ;; `fset' may set nil's function cell to nil but not to anything else.
 (list (fround -1.5) (fround 1.5) (fround 2.5) (fround -2.5) (fround 1.4)
       (seq-empty-p '(1 . 2)) (seq-empty-p nil) (seq-empty-p "") (seq-empty-p "a")
       (condition-case e (delete-directory "no-such-dir-xyz") (error e))
       (delete-directory "no-such-dir-xyz" t)
       (help-add-fundoc-usage "x" '(a)) (help-add-fundoc-usage "x\n" '(a))
       (help-add-fundoc-usage t '((a . 1) (b . 2)))
       (help-add-fundoc-usage nil '(a (b c)))
       (condition-case e (assoc :key '((a . 1)) 0.0) (error e))
       (condition-case e (assoc " a b " '(1 2 . 3) "[a-z]+") (error e))
       (assoc '(1 2 3) '(1 2 3) "ABC") (assoc "a" '(("a" . 1)))
       (condition-case e (append '(1 . 2) []) (error e)) (append '(1 2) '(3))
       (condition-case e (delq 'foo '(1 2 . 3)) (error e))
       (condition-case e (remq t '(1 . 2)) (error e))
       (condition-case e (generate-new-buffer "") (error e))
       (condition-case e (read-from-string "abc" 100) (error e))
       (read-from-string "abc" -1) (read-from-string "abc" nil)
       (condition-case e (concat '("a" "b") 'foo) (error e))
       (condition-case e (concat ["a" "b"] 7) (error e))
       (condition-case e (concat "x" 'foo) (error e))
       (condition-case e (concat "x" '(1 . 2)) (error e))
       (concat "ab" "cd") (concat '(97 98))
       (mod 0 0.0) (mod 32 0.0) (mod 5.5 2.0) (mod -5.5 2.0)
       (condition-case e (mod 5 0) (error e))
       (sort (list 3 1 2) nil) (sort [3 1 2] nil)
       (condition-case e (sort "cab" nil) (error e))
       (seq-sort nil "a.b*c") (seq-sort #'< '(3 1 2))
       (condition-case e (regexp-opt [1 2 3]) (error e))
       (regexp-opt '("a" "b"))
       (elt '(a b) -1) (nth -1 '(a b))
       (condition-case e (elt "ab" -1) (error e))
       (expt 4 1.5) (expt 2.0 3.0) (expt 2 0.5) (expt 0 1.5) (expt 0 0.0)
       (condition-case e (fset nil 1) (error e)) (fset nil nil)
       (condition-case e (defalias nil 1) (error e)) (defalias nil nil)
       (string-prefix-p " a b " nil) (string-prefix-p "a" nil)
       (condition-case e (string-prefix-p nil "a") (error e))
       (macroexp--fgrep '(1 2 3) '(1)) (macroexp--fgrep '((a . 1)) '(a))
       (macroexp--fgrep '((a . 1)) '(f)) (macroexp--fgrep '(a b) '(a))
       (seq-drop :key 0.0) (condition-case e (seq-drop :key 1) (error e))
       (locate-file "x" '(1 . 2))
       (macroexp-parse-body '("a" "b")) (macroexp-parse-body '((f) (g)))
       (macroexp-parse-body '("only"))
       (macroexp-parse-body '("d" (declare (x)) (f)))
       (boundp nil) (boundp t) (boundp :k) (boundp 'no-such-var-xyz)
       (buffer-live-p []) (buffer-live-p 5)
       (butlast -7 -7) (condition-case e (butlast -7) (error e))
       (condition-case e (seq-mapcat "" t 't) (error e))
       (condition-case e (seq-mapcat -1.5 "[a-z]+" 0) (error e))
       (condition-case e (seq-mapcat 7 [] "abc") (error e))
       (seq-mapcat #'list '(1 2) 'vector)
       (macroexpand '(1) "abc") (condition-case e (macroexpand '(a) "abc") (error e))
       (condition-case e (string-suffix-p [1 2 3] "ABC" "x") (error e))
       (log 0.0) (log 2 -7) (log 8 2) (log 7 1.5) (log 2.0) (log 10.0)
       (exp -1.5) (exp 0) (exp 1) (exp 2.5) (exp -0.1)
       (sqrt 2) (sqrt 32) (sqrt 123456.789)
       (special-variable-p nil) (special-variable-p 'foo)
       (prefix-numeric-value '((a . 1))) (prefix-numeric-value '(4))
       (condition-case e (plist-put '(1) 100 32) (error e))
       (plist-put (list 1 2 3) 1 9) (plist-member '(1) 100)
       (plist-get '(1 . 2) 1) (plist-get '(a 1) 'a)
       (seq-partition "abc" 2) (seq-partition [1 2 3] 2)
       (condition-case e (unibyte-string -1 (list 'quote 'sym)) (error e))
       (condition-case e (string-chop-newline -7) (error e))
       (condition-case e (vconcat '(1 . 2)) (error e))
       (condition-case e (string-to-list '(1 2 . 3)) (error e))
       (condition-case e (file-truename "abc" nil "[a-z]+") (error e))
       (condition-case e (lsh -1 t) (error e)) (condition-case e (lsh 1 t) (error e))
       (condition-case e (/ 1.5 nil) (error e))
       (condition-case e (seq-mapn 't '(1 2 . 3)) (error e))
       (seq-mapn 48 nil) (seq-mapn #'+ '(1 2) '(3 4))
       (condition-case e (memql 1 '(1 2 . 3)) (error e)) (memql 2 '(1 2 3))
       (condition-case e (number-sequence 1 '(1 2 3) ["a"]) (error e))
       (number-sequence '(quote sym)) (number-sequence 1 3)
       (executable-find "")
       (string-replace "a.b*c" 7 "ABC") (string-replace "abc" '(1 2 . 3) "a")
       (error-message-string (list 'quote 'sym))
       (error-message-string '(nosuchcond 1 2)))

 ;; The last of the long tail.  Two of these are the same shape twice over:
 ;; a NEGATIVE count asks for nothing rather than for everything (`last',
 ;; `butlast', `seq-take'), and "cannot read" is a different condition from
 ;; "is not there" (`byte-compile-file').
 (list (last '(1 2 . 3) -1) (last '(1 2 . 3) 1) (last '(1 2 3) 1) (last '(1 2 . 3))
       (help-split-fundoc "abc" "a" t) (help-split-fundoc "abc" "a")
       (condition-case e (seq-partition '(1 2 . 3) 1) (error e))
       (seq-partition '(1 2 3) 2)
       (locate-file "" '(1) 32) (locate-file "x" '("a") '("b"))
       (condition-case e (json-serialize '(1)) (error e))
       (condition-case e (json-serialize '(1 . 2)) (error e))
       (condition-case e (json-serialize '(1 2 . 3)) (error e))
       (json-serialize '((a . 1))) (json-serialize '(:a 1))
       (condition-case e (seq-drop ["a" "b"] nil) (error e))
       (condition-case e (seq-drop nil nil) (error e))
       (seq-drop '(1 2 3) 1) (seq-drop :key 0.0) (seq-take '(1 2 3) 2)
       (condition-case e (number-sequence -7 1.5 0.0) (error e))
       (condition-case e (define-error ["a" "b"] "a" '((a . 1) (b . 2))) (error e))
       (condition-case e (define-error 'e-unknown-parent "m" '(no-such-signal)) (error e))
       (condition-case e (defalias 'cyc-a 'cyc-a) (error e))
       (condition-case e (fset 'cyc-b 'cyc-b) (error e))
       (condition-case e (file-truename "x" 1) (error e))
       (condition-case e (file-truename "x" '(quote sym)) (error e))
       (condition-case e (lsh -1.5 [1 2 3]) (error e))
       (condition-case e (lsh 1.5 1) (error e))
       (condition-case e (elt '(1 . 2) 12354) (error e))
       (elt '(a b) -1)
       ;; `fmakunbound' is `(fset SYM nil)', so a nil function cell IS the
       ;; unbound state: the call must name the SYMBOL.  Measured 2026-08-20 --
       ;; the direct-call path answered `(void-function nil)' while `funcall'
       ;; and `apply' answered correctly, so all three are pinned here.
       (progn (fset 'par-fn-1 (lambda (y) y)) (fmakunbound 'par-fn-1)
              (condition-case e (par-fn-1 1) (error e)))
       (progn (fset 'par-fn-2 (lambda (y) y)) (fmakunbound 'par-fn-2)
              (condition-case e (funcall 'par-fn-2 1) (error e)))
       (progn (fset 'par-fn-3 (lambda (y) y)) (fmakunbound 'par-fn-3)
              (condition-case e (apply 'par-fn-3 '(1)) (error e)))
       ;; A void function cell reads back as nil; it does not signal.
       (symbol-function 'par-never-defined)
       (progn (fset 'par-fn-4 nil)
              (list (fboundp 'par-fn-4) (symbol-function 'par-fn-4)))
       (fmakunbound 'par-fn-5)
       (condition-case e (fmakunbound "s") (error e))
       (progn (fset 'par-fn-6 (lambda (y) (* y 3))) (par-fn-6 4))
       ;; `format' flags.  The zero-pad guard asked whether the first character
       ;; was a DECIMAL digit, which is false for hex output beginning a-f, so
       ;; "%02x" of 10 space-padded to " a".  `#' was scanned and thrown away.
       ;; `+' and a leading space were wired to d/f/e/g only.
       (format "%02x" 10) (format "%04X" 255) (format "%05x" -255)
       (format "%#x" 10) (format "%#X" 255) (format "%#o" 8)
       (format "%#x" 0) (format "%#o" 0) (format "%#05x" 10) (format "%#x" -10)
       (format "%#5x" 10) (format "%#-5x|" 10)
       (format "%+05x" 10) (format "% 05x" 10) (format "%+i" 5)
       (format "%#d" 5) (format "%05s" "ab") (format "%08.3f" 1.5)
       ;; A wrong-typed argument reached the native delegate, which formatted
       ;; whatever bits it was handed: (format "%o" [1]) answered a raw pointer
       ;; value and (format "%d" nil) answered 0.  Emacs signals for both.
       (condition-case e (format "%d" "s") (error e))
       (condition-case e (format "%d" nil) (error e))
       (condition-case e (format "%o" [1]) (error e))
       (condition-case e (format "%e" "s") (error e))
       (condition-case e (format "%c" "a") (error e))
       (condition-case e (format "%c" 65.0) (error e))
       (condition-case e (format "%c" -1) (error e))
       (condition-case e (format "%x" t) (error e))
       (format "%c" 65) (format "%d" 2.0)
       ;; `#' on %g keeps the trailing zeros %g strips, and keeps the point
       ;; even when nothing follows it.
       (format "%#g" 1.0) (format "%#g" 0.0) (format "%#g" 0.0001)
       (format "%#g" 0.00001) (format "%#.3g" 1.0) (format "%#.1g" 1.0)
       (format "%#.1g" 1.0e20) (format "%#010.3g" 1.0) (format "%#g" 100000.0)
       (format "%#g" 1000000.0) (format "%#.2g" 0.000123) (format "%#g" -2.5)
       ;; Emacs accepts s S d i o x X e f g c and %% only.  The C uppercase
       ;; spellings got through because the conversion character was handed
       ;; straight to the native delegate, and an unknown one answered the
       ;; SPEC STRING itself.  (%a / %A stay accepted -- deliberate NeLisp
       ;; superset, Doc 159 sec 11.)
       (condition-case e (format "%G" 1.0) (error e))
       (condition-case e (format "%E" 1.0) (error e))
       (condition-case e (format "%F" 1.0) (error e))
       (condition-case e (format "%b" 5) (error e))
       (condition-case e (format "%z" 5) (error e))
       ;; A format string that stops inside a specifier is an error, not a
       ;; literal "%".
       (condition-case e (format "%") (error e))
       (condition-case e (format "%5") (error e))
       (condition-case e (format "%.") (error e))
       (format "%i" 5) (format "a%%b")
       ;; %a / %A were a deliberate NeLisp superset (C99 hex-float, Doc 159
       ;; sec 11) until 2026-08-21.  Emacs rejects them, so a program that
       ;; formats here and signals there was a divergence too; parity won.
       (condition-case e (format "%a" 1.0) (error e))
       (condition-case e (format "%A" 1.0) (error e))
       (condition-case e (format "%.3a" 1.5) (error e))
       (condition-case e (format "%#a" 1.0) (error e))
       ;; Doc 188 §4.2 (buffer unification P1).  `with-temp-buffer' uses an
       ;; auto-named, caller-invisible buffer, so it cannot collide with
       ;; the `generate-new-buffer "x"'/`" a b "' calls earlier in this
       ;; file.  `insert'/`buffer-string' round-tripped "" before this
       ;; phase (Doc 188 §1.3's disconnected-variable bug); `point-max'
       ;; was hardcoded to 1 regardless of content.  `point-min' is
       ;; already-correct anchor: must not regress the one thing that was
       ;; already right.
       (with-temp-buffer (insert "abc") (buffer-string))
       (with-temp-buffer (insert "abcdef") (goto-char 3) (point))
       (with-temp-buffer (insert "ab") (point-min))
       (with-temp-buffer (insert "hi") (point-max))
       ;; Doc 188 §4.2 (buffer unification P2).  `current-buffer'/
       ;; `set-buffer'/`buffer-substring'/`erase-buffer' were all
       ;; void-function before this phase.  Each form is self-contained
       ;; inside its own `with-temp-buffer', so it cannot disturb the
       ;; ambient current buffer for anything else in this file (same
       ;; discipline as the P1 forms just above).
       (with-temp-buffer
         (let ((outer (current-buffer)))
           (with-temp-buffer (set-buffer outer) (eq (current-buffer) outer))))
       ;; `buffer-substring' accepts START/END in either order -- both
       ;; answers collected into one form so a single printed value
       ;; covers both edges.
       (with-temp-buffer
         (insert "hello")
         (list (buffer-substring 2 4) (buffer-substring 4 2)))
       (with-temp-buffer (insert "abc") (erase-buffer) (buffer-string))
       ;; Doc 186 P0/P1: char-table constructor/accessor layer.  Not a
       ;; native/prelude shadow case (there is no prelude definition to
       ;; shadow -- these two names live only in the standalone's own
       ;; `bf_*' dispatch), but the gate's own contract is "byte-identical
       ;; printed output on both sides" regardless, and these are the
       ;; doc's own §6.2 parity forms.
       (char-table-p (make-char-table 'test))
       (aref (make-char-table 'test 'D) ?a))
)

;;; nelisp-shadow-differential-cases.el ends here
