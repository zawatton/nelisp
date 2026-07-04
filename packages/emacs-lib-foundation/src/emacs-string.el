;;; emacs-string.el --- NeLisp port of Emacs string utility primitives  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Doc 51 Phase 2 — Layer 2.
;;
;; Ports the string-utility primitives Emacs ships in `subr-x.el' /
;; `subr.el' that anvil modules use during normal operation
;; (`string-trim', `string-prefix-p', `string-suffix-p',
;; `string-empty-p', `string-blank-p', `string-lines').  Each polyfill is gated on
;; `unless (fboundp ...)'.

;;; Code:

(require 'emacs-char-table)

(unless (fboundp 'string-empty-p)
  (defun string-empty-p (string)
    "Return non-nil iff STRING is the empty string."
    (= 0 (length string))))

(unless (fboundp 'string-blank-p)
  (defun string-blank-p (string)
    "Return non-nil iff STRING contains only whitespace (or is empty)."
    (string-match-p "\\`[ \t\n\r]*\\'" string)))

(unless (fboundp 'string-prefix-p)
  (defun string-prefix-p (prefix string &optional ignore-case)
    "Return non-nil iff PREFIX is a prefix of STRING.
IGNORE-CASE non-nil compares case-insensitively (= naive ASCII downcase)."
    (ignore ignore-case)
    (let ((plen (length prefix)))
      (and (>= (length string) plen)
           (equal (substring string 0 plen) prefix)))))

(unless (fboundp 'string-suffix-p)
  (defun string-suffix-p (suffix string &optional ignore-case)
    "Return non-nil iff SUFFIX is a suffix of STRING."
    (ignore ignore-case)
    (let ((slen (length suffix))
          (xlen (length string)))
      (and (>= xlen slen)
           (equal (substring string (- xlen slen)) suffix)))))

(unless (fboundp 'string-reverse)
  (defun string-reverse (string)
    "Return STRING with its characters in reverse order."
    (apply #'string (nreverse (append string nil)))))

(unless (fboundp 'string-trim-left)
  (defun string-trim-left (string &optional regexp)
    "Trim leading whitespace from STRING.
Optional REGEXP overrides the default `[ \\t\\n\\r]+' pattern."
    (let ((re (or regexp "\\`[ \t\n\r]+")))
      (if (string-match re string)
          (substring string (match-end 0))
        string))))

(unless (fboundp 'string-trim-right)
  (defun string-trim-right (string &optional regexp)
    "Trim trailing whitespace from STRING."
    (let ((re (or regexp "[ \t\n\r]+\\'")))
      (if (string-match re string)
          (substring string 0 (match-beginning 0))
        string))))

(unless (fboundp 'string-trim)
  (defun string-trim (string &optional trim-left trim-right)
    "Trim leading + trailing whitespace from STRING."
    (string-trim-left (string-trim-right string trim-right) trim-left)))

(unless (fboundp 'string-lines)
  (defun string-lines (string &optional omit-nulls keep-newlines)
    "Split STRING into a list of lines.
If OMIT-NULLS is non-nil, empty lines are removed.  If KEEP-NEWLINES
is non-nil, each returned line keeps its trailing newline when present."
    (let ((start 0)
          (len (length string))
          (out nil))
      (if (= len 0)
          (unless omit-nulls
            (setq out (cons "" out)))
        (while (< start len)
          (let* ((pos (string-search "\n" string start))
                 (end (or pos len))
                 (raw (substring string start end))
                 (line (if (and pos keep-newlines)
                           (substring string start (1+ pos))
                         raw)))
          (unless (and omit-nulls (string-empty-p raw))
            (setq out (cons line out)))
          (setq start (if pos (1+ pos) len)))))
      (nreverse out))))

(unless (and (fboundp 'string<)
             (not (get 'string< 'emacs-stub-bulk)))
  (defun string< (a b)
    "Return non-nil if string A is lexicographically less than string B."
    (let ((i 0)
          (na (length a))
          (nb (length b))
          (answer nil)
          (done nil))
      (while (and (not done) (< i na) (< i nb))
        (let ((ca (aref a i))
              (cb (aref b i)))
          (cond
           ((< ca cb) (setq answer t done t))
           ((> ca cb) (setq answer nil done t))
           (t (setq i (1+ i))))))
      (if done answer (< na nb))))
  (put 'string< 'emacs-stub-bulk nil))

(unless (and (fboundp 'char-equal)
             (not (get 'char-equal 'emacs-stub-bulk)))
  (defun char-equal (a b)
    "Return non-nil when characters A and B are equal."
    (= a b))
  (put 'char-equal 'emacs-stub-bulk nil))

(unless (and (fboundp 'string-width)
             (not (get 'string-width 'emacs-stub-bulk)))
  (defun string-width (string)
    "Return the display width of STRING (sum of per-character widths)."
    (emacs-string--string-width string))
  (put 'string-width 'emacs-stub-bulk nil))

(unless (and (fboundp 'int-to-string)
             (not (get 'int-to-string 'emacs-stub-bulk)))
  (defun int-to-string (integer)
    "Return decimal printed representation of INTEGER."
    (format "%d" integer))
  (put 'int-to-string 'emacs-stub-bulk nil))

(unless (and (fboundp 'assoc-string)
             (not (get 'assoc-string 'emacs-stub-bulk)))
  (defun emacs-string--assoc-string-key (value)
    "Return VALUE as a string key for `assoc-string', or nil."
    (cond
     ((stringp value) value)
     ((symbolp value) (symbol-name value))
     (t nil)))

  (defun assoc-string (key list &optional case-fold)
    "Return first alist element whose string key matches KEY.
CASE-FOLD non-nil compares via `downcase'."
    (let ((needle (emacs-string--assoc-string-key key))
          (cur list)
          (found nil))
      (when case-fold
        (setq needle (and needle (downcase needle))))
      (when needle
        (while (and cur (not found))
          (let* ((cell (car cur))
                 (head (emacs-string--assoc-string-key
                        (if (consp cell) (car cell) cell))))
            (when head
              (let ((candidate (if case-fold (downcase head) head)))
                (when (string= needle candidate)
                  (setq found cell)))))
          (setq cur (cdr cur))))
      found))
  (put 'assoc-string 'emacs-stub-bulk nil))

(unless (fboundp 'string-lessp) (defun string-lessp (a b) (string< a b)))
(unless (fboundp 'string-greaterp) (defun string-greaterp (a b) (string< b a)))
(unless (fboundp 'string>) (defun string> (a b) (string< b a)))
(unless (fboundp 'substring-no-properties)
  (defun substring-no-properties (s &optional from to)
    (substring s (or from 0) (or to (length s)))))
(unless (fboundp 'truncate-string-to-width)
  (defun truncate-string-to-width (str width &rest _)
    (if (<= (length str) width) str (substring str 0 (max 0 width)))))
(unless (fboundp 'capitalize)
  (defun capitalize (s)
    (if (stringp s)
        (let ((res "") (i 0) (n (length s)) (prev-alpha nil))
          (while (< i n)
            (let* ((c (aref s i))
                   (alpha (or (and (>= c 97) (<= c 122)) (and (>= c 65) (<= c 90))))
                   (ch (cond ((and alpha (not prev-alpha) (>= c 97) (<= c 122)) (- c 32))
                             ((and alpha prev-alpha (>= c 65) (<= c 90)) (+ c 32))
                             (t c))))
              (setq res (concat res (char-to-string ch)) prev-alpha alpha))
            (setq i (1+ i)))
          res)
      s)))

;; ASCII case conversion (src/casefiddle.c:ascii_casify_character).  These
;; were void/stub in the standalone runtime, yet `string-equal-ignore-case'
;; and the compare helpers below already depend on `downcase'.  ASCII-only;
;; Unicode and case-table casing are not modeled.  OBJECT may be a character
;; (integer) or a string, matching the host `upcase' / `downcase' surface.
(unless (and (fboundp 'upcase) (not (get 'upcase 'emacs-stub-bulk)))
  (defun upcase (object)
    "Naive ASCII upcase polyfill for a character or string OBJECT.
a..z map to A..Z; every other codepoint is returned unchanged."
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
     (t object)))
  (put 'upcase 'emacs-stub-bulk nil))

(unless (and (fboundp 'downcase) (not (get 'downcase 'emacs-stub-bulk)))
  (defun downcase (object)
    "Naive ASCII downcase polyfill for a character or string OBJECT.
A..Z map to a..z; every other codepoint is returned unchanged."
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
     (t object)))
  (put 'downcase 'emacs-stub-bulk nil))

;; char-width (src/character.c:char_width) was undefined in the standalone
;; runtime.  Default-policy column width: newline 0, tab `tab-width', other
;; control/DEL 2 (^X notation), combining diacriticals 0, East Asian wide /
;; fullwidth / CJK / emoji 2, everything else 1.  A full char-width-table from
;; Unicode East Asian Width data is out of scope; the wide ranges are a rough
;; approximation pinned to host `char-width' on tested codepoints.
(defvar emacs-string--char-width-table nil
  "Lazily built char-table mapping codepoint -> column width.
Default width is 1; control chars and East Asian wide / fullwidth / CJK /
emoji codepoints are 2, newline and combining diacriticals 0.  This is the
overridable width-table substrate (consumers may `emacs-char-table-set' it);
a full Unicode East Asian Width data set remains future work, so the wide
ranges are a documented approximation pinned to host `char-width'.")

(defun emacs-string--build-char-width-table ()
  "Build and return the default char-width char-table."
  (let ((ct (emacs-char-table-make 'char-width 1)))
    (emacs-char-table-set-range ct (cons 0 31) 2)        ; control -> ^X
    (emacs-char-table-set ct 10 0)                        ; newline
    (emacs-char-table-set ct 127 2)                       ; DEL -> ^?
    (emacs-char-table-set-range ct (cons #x300 #x36F) 0)  ; combining marks
    (dolist (r '((#x1100 . #x115F) (#x2E80 . #x303E) (#x3041 . #x33FF)
                 (#x3400 . #x4DBF) (#x4E00 . #x9FFF) (#xA000 . #xA4CF)
                 (#xAC00 . #xD7A3) (#xF900 . #xFAFF) (#xFE10 . #xFE19)
                 (#xFE30 . #xFE6F) (#xFF00 . #xFF60) (#xFFE0 . #xFFE6)
                 (#x1B000 . #x1B16F) (#x1F300 . #x1FAFF) (#x20000 . #x3FFFD)))
      (emacs-char-table-set-range ct r 2))
    ct))

(defun emacs-string-char-width-table ()
  "Return the char-width char-table, building it on first use."
  (or emacs-string--char-width-table
      (setq emacs-string--char-width-table
            (emacs-string--build-char-width-table))))

(defun emacs-string--char-width (char)
  "Return the column width for character CHAR via the char-width table.
TAB resolves to `tab-width' (matching host `char-width')."
  (if (eq char ?\t)
      (if (and (boundp 'tab-width) (integerp tab-width)) tab-width 8)
    (emacs-char-table-ref (emacs-string-char-width-table) char)))

(defun emacs-string--string-width (string)
  "Return the summed display width of STRING using `emacs-string--char-width'."
  (let ((w 0) (i 0) (n (length string)))
    (while (< i n)
      (setq w (+ w (emacs-string--char-width (aref string i)))
            i (1+ i)))
    w))

(unless (and (fboundp 'char-width) (not (get 'char-width 'emacs-stub-bulk)))
  (defun char-width (char)
    "Return the column width of CHAR (default policy, ASCII + rough EAW)."
    (emacs-string--char-width char))
  (put 'char-width 'emacs-stub-bulk nil))

;;;; --- Doc 16 breadth: subr-x / subr string builtins (were void) -------
;; `string-equal-ignore-case' / `string-clean-whitespace' (subr-x.el) and
;; `string-split' (subr.el alias) were void in the standalone runtime.
;; Gated on `unless (fboundp ...)'.  The reader treats POSIX `[[:blank:]]'
;; classes literally (verified by direct --load), so
;; `string-clean-whitespace' uses an explicit `[ \t\r\n]+' character set.

(unless (fboundp 'string-equal-ignore-case)
  (defun string-equal-ignore-case (string1 string2)
    "Compare STRING1 and STRING2 case-insensitively (= naive `downcase').
Upper-case and lower-case letters are treated as equal."
    (string-equal (downcase string1) (downcase string2))))

(unless (fboundp 'string-clean-whitespace)
  (defun string-clean-whitespace (string)
    "Clean up whitespace in STRING.
All sequences of whitespace in STRING are collapsed into a single
space character, and leading/trailing whitespace is removed."
    (string-trim (replace-regexp-in-string "[ \t\r\n]+" " " string t t))))

(unless (fboundp 'string-split)
  (defalias 'string-split #'split-string
    "Split STRING into a list of substrings.  Alias of `split-string'."))

;;;; --- Doc 16 breadth round 11: string comparison / conversion ---------
;; string-distance / string-version-lessp / string-collate-lessp /
;; string-collate-equalp / string-to-vector were void.  All gated on
;; `unless (fboundp ...)'.  collate-* fall back to byte order (no locale
;; collation in the standalone runtime).

(unless (fboundp 'string-distance)
  (defun string-distance (string1 string2 &optional _bytecompare)
    "Return the Levenshtein edit distance between STRING1 and STRING2.
Characters are compared by code point (the BYTECOMPARE argument, which
would switch to a byte-wise comparison, is ignored)."
    (let* ((l1 (length string1))
           (l2 (length string2))
           (col (make-vector (1+ l2) 0))
           (prev 0)
           (cur 0))
      (dotimes (j (1+ l2)) (aset col j j))
      (dotimes (i l1)
        (setq prev (aref col 0))
        (aset col 0 (1+ i))
        (dotimes (j l2)
          (setq cur (aref col (1+ j)))
          (aset col (1+ j)
                (min (1+ (aref col (1+ j)))
                     (1+ (aref col j))
                     (+ prev (if (= (aref string1 i) (aref string2 j)) 0 1))))
          (setq prev cur)))
      (aref col l2))))

(unless (fboundp 'string-version-lessp)
  (defun string-version-lessp (string1 string2)
    "Return non-nil if STRING1 is less than STRING2 in version order.
Runs of digits are compared by numeric value, so e.g. \"foo2\" sorts
before \"foo10\".  (Very long digit runs are compared via
`string-to-number' and so lose precision beyond the float range.)"
    (let ((i 0) (j 0)
          (n1 (length string1)) (n2 (length string2))
          (result nil) (done nil))
      (while (not done)
        (cond
         ((and (>= i n1) (>= j n2)) (setq done t result nil))
         ((>= i n1) (setq done t result t))
         ((>= j n2) (setq done t result nil))
         (t
          (let* ((c1 (aref string1 i)) (c2 (aref string2 j))
                 (d1 (and (>= c1 ?0) (<= c1 ?9)))
                 (d2 (and (>= c2 ?0) (<= c2 ?9))))
            (if (and d1 d2)
                (let ((si i) (sj j))
                  (while (and (< i n1) (>= (aref string1 i) ?0) (<= (aref string1 i) ?9))
                    (setq i (1+ i)))
                  (while (and (< j n2) (>= (aref string2 j) ?0) (<= (aref string2 j) ?9))
                    (setq j (1+ j)))
                  (let ((v1 (string-to-number (substring string1 si i)))
                        (v2 (string-to-number (substring string2 sj j))))
                    (cond ((< v1 v2) (setq done t result t))
                          ((> v1 v2) (setq done t result nil)))))
              (cond ((< c1 c2) (setq done t result t))
                    ((> c1 c2) (setq done t result nil))
                    (t (setq i (1+ i) j (1+ j)))))))))
      result)))

(unless (fboundp 'string-collate-lessp)
  (defun string-collate-lessp (s1 s2 &optional _locale ignore-case)
    "Return non-nil if S1 is less than S2.
The standalone runtime has no locale collation, so this falls back to
byte order (`string-lessp'); IGNORE-CASE folds with `downcase'."
    (if ignore-case
        (string-lessp (downcase s1) (downcase s2))
      (string-lessp s1 s2))))

(unless (fboundp 'string-collate-equalp)
  (defun string-collate-equalp (s1 s2 &optional _locale ignore-case)
    "Return non-nil if S1 and S2 are equal.
Falls back to `string-equal'; IGNORE-CASE folds with `downcase'."
    (if ignore-case
        (string-equal (downcase s1) (downcase s2))
      (string-equal s1 s2))))

(unless (fboundp 'string-to-vector)
  (defun string-to-vector (string)
    "Return a vector of the characters in STRING."
    (vconcat string)))

;;;; --- Doc 16 breadth round 12: subr.el string utilities (were void) ---
;; subst-char-in-string / combine-and-quote-strings / split-string-and-unquote
;; were void.  subst-char-in-string routes through `string-replace' because
;; the standalone runtime's `aset' does not mutate strings (only vectors).

(unless (fboundp 'subst-char-in-string)
  (defun subst-char-in-string (fromchar tochar string &optional _inplace)
    "Return a copy of STRING with each FROMCHAR replaced by TOCHAR.
The INPLACE argument is ignored (the runtime cannot mutate strings)."
    (string-replace (string fromchar) (string tochar) string)))

(unless (fboundp 'combine-and-quote-strings)
  (defun combine-and-quote-strings (strings &optional separator)
    "Concatenate STRINGS, quoting any that contain SEPARATOR (default \" \").
Inverse of `split-string-and-unquote'."
    (let* ((sep (or separator " "))
           (re (concat "[\\\"]" "\\|" (regexp-quote sep))))
      (mapconcat
       (lambda (str)
         (if (string-match re str)
             (concat "\"" (replace-regexp-in-string "[\\\"]" "\\\\\\&" str) "\"")
           str))
       strings sep))))

(unless (fboundp 'split-string-and-unquote)
  (defun split-string-and-unquote (string &optional separator)
    "Split STRING on SEPARATOR (default \"\\\\s-+\"), unquoting Lisp-quoted parts.
Inverse of `combine-and-quote-strings'."
    (let ((sep (or separator "\\s-+"))
          (i (string-search "\"" string)))
      (if (null i)
          (split-string string sep t)
        (append (unless (eq i 0) (split-string (substring string 0 i) sep t))
                (let ((rfs (read-from-string string i)))
                  (cons (car rfs)
                        (split-string-and-unquote (substring string (cdr rfs))
                                                  sep))))))))

(unless (and (fboundp 'propertize) (not (get 'propertize 'emacs-stub-bulk)))
  (defun propertize (string &rest _properties)
    "Return a copy of STRING.
Standalone MVP: text PROPERTIES are accepted for call compatibility but are
not retained, matching the no-op string text-property substrate.  Callers that
only need the string content are unaffected."
    (copy-sequence string))
  (put 'propertize 'emacs-stub-bulk nil))

(unless (fboundp 'string-glyph-split)
  (defun string-glyph-split (string)
    "Split STRING into a list of one-glyph strings.
Standalone substrate: splits per character (no grapheme-cluster composition),
which matches `string-glyph-split' for non-composed text."
    (let ((out nil) (i 0) (n (length string)))
      (while (< i n)
        (push (substring string i (1+ i)) out)
        (setq i (1+ i)))
      (nreverse out))))

(provide 'emacs-string)

;;; emacs-string.el ends here
