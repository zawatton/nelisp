;;; emacs-string-test.el --- ERT tests for emacs-string  -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for string utility polyfills used by vendored Emacs Lisp.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'emacs-string)

(ert-deftest emacs-string-test/require-loads-cleanly ()
  (should (featurep 'emacs-string))
  (dolist (sym '(string-empty-p string-blank-p string-prefix-p
                 string-suffix-p string-trim-left string-trim-right
                 string-trim string-lines string< char-equal string-width
                 int-to-string assoc-string))
    (should (fboundp sym))))

(ert-deftest emacs-string-test/string-lines-basic-shape ()
  (should (equal (string-lines "") '("")))
  (should (equal (string-lines "a") '("a")))
  (should (equal (string-lines "a\n") '("a")))
  (should (equal (string-lines "a\n\n") '("a" "")))
  (should (equal (string-lines "\na") '("" "a"))))

(ert-deftest emacs-string-test/string-lines-omit-nulls ()
  (should (equal (string-lines "" t) nil))
  (should (equal (string-lines "a\n\nb" t) '("a" "b")))
  (should (equal (string-lines "\na" t) '("a"))))

(ert-deftest emacs-string-test/string-lines-keep-newlines ()
  (should (equal (string-lines "a\n" nil t) '("a\n")))
  (should (equal (string-lines "a\n\n" nil t) '("a\n" "\n")))
  (should (equal (string-lines "\na" nil t) '("\n" "a")))
  (should (equal (string-lines "a\n\n" t t) '("a\n"))))

(ert-deftest emacs-string-test/runtime-callable-fallbacks ()
  (should (string< "a" "b"))
  (should-not (string< "b" "a"))
  (should-not (string< "a" "a"))
  (should (char-equal ?x ?x))
  (should (= 3 (string-width "abc")))
  (should (string-equal "-42" (int-to-string -42)))
  (should (equal '("A" . 1)
                 (assoc-string "a" '(("A" . 1)) t)))
  (should (equal '(A . 1)
                 (assoc-string "a" '((A . 1)) t)))
  (should-not (assoc-string 'a '((A . 1)) nil)))

(ert-deftest emacs-string-test/casefiddle-ascii-polyfill ()
  "ASCII upcase/downcase polyfills cover character and string objects.
Host Emacs' C builtins shadow the `unless (fboundp ...)' gates, so pin literal
copies of the polyfill bodies (Doc 16 residuals-test parity pattern)."
  (cl-letf (((symbol-function 'upcase)
             (lambda (object)
               (cond
                ((integerp object)
                 (if (and (>= object ?a) (<= object ?z)) (- object 32) object))
                ((stringp object)
                 (let ((res (copy-sequence object)) (i 0) (n (length object)))
                   (while (< i n)
                     (let ((c (aref object i)))
                       (when (and (>= c ?a) (<= c ?z)) (aset res i (- c 32))))
                     (setq i (1+ i)))
                   res))
                (t object))))
            ((symbol-function 'downcase)
             (lambda (object)
               (cond
                ((integerp object)
                 (if (and (>= object ?A) (<= object ?Z)) (+ object 32) object))
                ((stringp object)
                 (let ((res (copy-sequence object)) (i 0) (n (length object)))
                   (while (< i n)
                     (let ((c (aref object i)))
                       (when (and (>= c ?A) (<= c ?Z)) (aset res i (+ c 32))))
                     (setq i (1+ i)))
                   res))
                (t object)))))
    (should (= ?A (upcase ?a)))
    (should (= ?A (upcase ?A)))
    (should (= ?5 (upcase ?5)))
    (should (equal "ABC123" (upcase "abc123")))
    (should (= ?z (downcase ?Z)))
    (should (= ?z (downcase ?z)))
    (should (equal "abc123" (downcase "ABC123")))
    (should (equal "" (upcase "")))))

(ert-deftest emacs-string-test/doc16-breadth-string-builtins ()
  "Doc 16 breadth: string-equal-ignore-case / string-clean-whitespace /
string-split were void in the standalone runtime."
  ;; string-equal-ignore-case
  (should (string-equal-ignore-case "ABc" "abC"))
  (should-not (string-equal-ignore-case "abc" "abd"))
  ;; string-clean-whitespace (collapse runs + trim ends).
  ;; On host Emacs this name is an *autoload* into subr-x, and the test
  ;; load-path (-L src) shadows subr-x with the partial NeLisp port, so
  ;; calling it live would mis-resolve.  Pin a literal copy of the
  ;; standalone polyfill body (residuals-test parity pattern) instead.
  (cl-letf (((symbol-function 'string-clean-whitespace)
             (lambda (string)
               (string-trim
                (replace-regexp-in-string "[ \t\r\n]+" " " string t t)))))
    (should (equal "a b c" (string-clean-whitespace "  a   b\tc \n")))
    (should (equal "single" (string-clean-whitespace "single")))
    (should (equal "" (string-clean-whitespace "   "))))
  ;; string-split is an alias of split-string
  (should (equal '("a" "b" "c") (string-split "a,b,c" ",")))
  (should (equal '("a" "b") (string-split "  a b  "))))

(ert-deftest emacs-string-test/doc16-round11-compare-convert ()
  "Doc 16 round 11: string-distance / string-version-lessp /
string-collate-lessp / string-collate-equalp / string-to-vector."
  ;; string-distance (Levenshtein)
  (should (equal 0 (string-distance "abc" "abc")))
  (should (equal 1 (string-distance "ab" "abc")))
  (should (equal 3 (string-distance "kitten" "sitting")))
  (should (equal 3 (string-distance "" "abc")))
  (should (equal 2 (string-distance "flaw" "lawn")))
  ;; string-version-lessp (digit runs by value)
  (should (string-version-lessp "foo2" "foo10"))
  (should-not (string-version-lessp "foo10" "foo2"))
  (should (string-version-lessp "a" "b"))
  (should-not (string-version-lessp "abc" "abc"))
  ;; string-collate-lessp / -equalp (byte-order fallback + ignore-case)
  (should (string-collate-lessp "a" "b"))
  (should (string-collate-lessp "A" "b" nil t))
  (should (string-collate-equalp "a" "a"))
  (should (string-collate-equalp "ABC" "abc" nil t))
  ;; string-to-vector
  (should (equal [97 98 99] (string-to-vector "abc"))))

(ert-deftest emacs-string-test/doc16-round12-subr-string-utils ()
  "Doc 16 round 12: subst-char-in-string / combine-and-quote-strings /
split-string-and-unquote."
  (should (equal "bXnXnX" (subst-char-in-string ?a ?X "banana")))
  (should (equal "banana" (subst-char-in-string ?z ?Q "banana")))
  (should (equal "a \"b c\" d" (combine-and-quote-strings '("a" "b c" "d"))))
  (should (equal '("a" "b c" "d") (split-string-and-unquote "a \"b c\" d")))
  ;; round trip
  (should (equal '("x" "y z" "q")
                 (split-string-and-unquote
                  (combine-and-quote-strings '("x" "y z" "q"))))))

(provide 'emacs-string-test)

;;; emacs-string-test.el ends here

(ert-deftest emacs-string-test/char-width-matches-host ()
  "emacs-string--char-width matches host char-width on representative codepoints.
Host's C `char-width' shadows the install gate, so the private policy helper is
exercised directly and compared to host."
  (dolist (cp (list 1 8 9 10 12 13 27 31 32 65 97 126 127 160
                    #x301 #x3042 #x65e5 #xFF21 #xAC00 #x1100 #x1F600 #x2003))
    (should (= (char-width cp) (emacs-string--char-width cp))))
  ;; summed string width, including a wide CJK codepoint
  (should (= 3 (emacs-string--string-width "abc")))
  (should (= 4 (emacs-string--string-width (string ?a #x65e5 ?b))))
  (should (= 0 (emacs-string--string-width ""))))

(ert-deftest emacs-string-test/propertize-returns-string-content ()
  "propertize returns the string content (MVP drops properties)."
  (should (string= "abc" (propertize "abc" 'face 'bold)))
  (should (stringp (propertize "x")))
  (should (string= "" (propertize ""))))
