;;; nelisp-stdlib-prn.el --- elisp Sexp printer / serializer  -*- lexical-binding: t; -*-

;; Phase 7 Stage 7.1.2 (2026-05-07, Doc 64).
;;
;; elisp re-implementation of `prin1-to-string' / `prin1' / `terpri'.
;; `princ' / `print' already live in `lisp/nelisp-stdlib-misc.el'
;; (= batch 6e/6i) on top of `prin1-to-string'; promoting
;; `prin1-to-string' to elisp here also routes those two through the
;; pure-elisp printer.  The Rust dispatch arm + `bi_prin1_to_string'
;; function body in `build-tool/src/eval/builtins.rs' are removed in
;; the same commit (= Stage 7.1.4 in Doc 64).
;;
;; Float formatting matches the prior Rust `Sexp' Display closely
;; enough for substrate use: `(number-to-string X)' (= `%g')
;; followed by a `.0' suffix when the result lacks `.', `e' or `E'.
;; Edge cases (very large / NaN / inf) are passed through unchanged.
;;
;; Reader-macro abbreviation: a 2-element cons `(QUOTE-TAG ARG)' whose
;; head is one of `quote' / `function' or the punctuation-named symbols
;; `\`' / `,' / `,@' is rendered with the corresponding prefix (`\''
;; / `#\'' / `\`' / `,' / `,@').
;;
;; The MVP omits cycle detection (`#1=...#1#'); circular structures
;; recurse infinitely and abort via `max-lisp-eval-depth', matching
;; the prior Rust impl.  Cycle-safe printing is Stage 7.1.5 follow-up.

;; ---- core dispatcher ----

(defun nelisp--prn-chunks-add (state chunk)
  "Append CHUNK to STATE without reversing the accumulated chunk list."
  (let ((cell (cons chunk nil)))
    (if (car state)
        (setcdr (cdr state) cell)
      (setcar state cell))
    (setcdr state cell)
    state))

(defun nelisp--prn-chunks-string (state)
  "Return the concatenation of chunks held in STATE."
  (apply #'concat (car state)))

(defun nelisp--prn-string-escaped (s)
  "Return S with `\"' and `\\' escaped, which is what Emacs `prin1' escapes.
Measured on Emacs 30.1 rather than assumed: a newline, tab, carriage return,
BEL and NUL all pass through VERBATIM inside a printed string -- only the
two characters that would end the literal or start an escape are doubled.
Escaping \\n as well made every printed string containing a newline differ
from Emacs, which `make emacs-parity' caught the first time a case had one.
Char comparisons use raw integer codepoints (34 / 92) to sidestep any
difference in how `?\\X' literals get parsed by the bundled reader vs the
host.  For a tag-14/15 unibyte string, Doc 200 additionally requires every
byte >= 128 to print as octal, never as a raw byte mistaken for UTF-8."
  (let ((chunks (cons nil nil))
        (i 0)
        (n (length s))
        (unibyte (and (fboundp 'unibyte-string-p)
                      (unibyte-string-p s))))
    (while (< i n)
      (let ((c (aref s i)))
        (cond
         ((= c 34) (nelisp--prn-chunks-add chunks "\\\"")) ; ?\"
         ((= c 92) (nelisp--prn-chunks-add chunks "\\\\")) ; ?\\
         ((and unibyte (>= c 128))
          (nelisp--prn-chunks-add
           chunks
           (concat "\\"
                   (char-to-string (+ 48 (/ c 64)))
                   (char-to-string (+ 48 (logand (/ c 8) 7)))
                   (char-to-string (+ 48 (logand c 7))))))
         (t        (nelisp--prn-chunks-add chunks (char-to-string c)))))
      (setq i (1+ i)))
    (nelisp--prn-chunks-string chunks)))

(defun nelisp--prn-symbol-char-needs-escape-p (c)
  "Return non-nil when C terminates or escapes a reader symbol atom.
This mirrors the reader atom-terminator predicate.  A backslash also
needs escaping because the reader consumes it as an escape prefix."
  (or (= c 92) (= c 40) (= c 41) (= c 91) (= c 93) (= c 39)
      (= c 96) (= c 44) (= c 59) (= c 34) (= c 32) (= c 9)
      (= c 10) (= c 13) (= c 11) (= c 12)))

(defun nelisp--prn-symbol-escaped (s)
  "Return S with reader atom terminators escaped for readable printing."
  (if (= (length s) 0)
      "##"
    (let ((chunks (cons nil nil)) (i 0) (n (length s)))
      (when (nelisp--prn-symbol-needs-leading-escape-p s)
        (nelisp--prn-chunks-add chunks "\\"))
      (while (< i n)
        (let ((c (aref s i)))
          (when (nelisp--prn-symbol-char-needs-escape-p c)
            (nelisp--prn-chunks-add chunks "\\"))
          (nelisp--prn-chunks-add chunks (char-to-string c)))
        (setq i (1+ i)))
      (nelisp--prn-chunks-string chunks))))

(defun nelisp--prn-float (x)
  "Return a compact, round-trip-safe string for float X.
Built on `(number-to-string X)' (= `%g' which in NeLisp is fixed
6-decimal `%f', e.g. `1.5' → `1.500000').  Trims trailing zeros
after the decimal point — `1.500000' → `1.5' — keeping at least one
digit so the form re-reads as a float (= `1.0' stays `1.0', not `1').
Integer-valued bodies without `.' / `e' / `E' get `.0' appended for
round-trip identity.  `inf' / `-inf' / `NaN' pass through unchanged."
  (let ((s (number-to-string x)))
    (cond
     ((string= s "inf") s)
     ((string= s "-inf") s)
     ((string= s "NaN") s)
     (t
      (let ((dot (string-search "." s))
            (eee (or (string-search "e" s) (string-search "E" s))))
        (cond
         ;; Exponent form passes through (= already minimal).
         (eee s)
         ;; No `.' and no exponent → append `.0' for round-trip.
         ((null dot) (concat s ".0"))
         (t
          ;; Trim trailing zeros after `.', keep at least 1 digit.
          (let ((i (1- (length s))))
            (while (and (> i (1+ dot)) (eq (aref s i) ?0))
              (setq i (1- i)))
            (substring s 0 (1+ i))))))))))

(defun nelisp--prn-reader-macro-abbrev (lst escape)
  "Return abbreviated form for `(TAG ARG)' reader-macro shapes, or nil.
TAG is quote, function, or an Emacs-compatible punctuation-named symbol.
Legacy convenience names remain ordinary symbols so readable output
round-trips without changing the list head.  ARG is printed recursively
via `nelisp--prn-to-string' under ESCAPE."
  (when (and (consp lst)
             (symbolp (car lst))
             (consp (cdr lst))
             (null (cdr (cdr lst))))
    (let* ((tag-name (symbol-name (car lst)))
           (arg (car (cdr lst)))
           (prefix (cond ((string= tag-name "quote")     "'")
                         ((string= tag-name "function")  "#'")
                         ((string= tag-name "`")         "`")
                         ((string= tag-name ",")         ",")
                         ((string= tag-name ",@")        ",@")
                         (t nil))))
      (when prefix
        (concat prefix (nelisp--prn-to-string arg escape))))))

(defun nelisp--prn-list-body (lst escape &optional depth)
  (setq depth (or depth 0))
  (let ((chunks (cons nil nil)) (cur lst) (first t) (count 0))
    (while (and (consp cur)
                (if print-length (< count print-length) t))
      (unless first (nelisp--prn-chunks-add chunks " "))
      (nelisp--prn-chunks-add chunks
                              (nelisp--prn-to-string (car cur) escape depth))
      (setq first nil)
      (setq count (1+ count))
      (setq cur (cdr cur)))
    (when (and (consp cur) print-length)
      (nelisp--prn-chunks-add chunks " ...")
      (setq cur nil))
    (unless (null cur)
      (nelisp--prn-chunks-add chunks " . ")
      (nelisp--prn-chunks-add chunks (nelisp--prn-to-string cur escape depth)))
    (nelisp--prn-chunks-string chunks)))

(defun nelisp--prn-vector (vec escape &optional depth)
  (setq depth (or depth 0))
  (let ((n (length vec)) (chunks (cons nil nil)))
    (nelisp--prn-chunks-add chunks "[")
    (let ((i 0) (lim (if print-length (if (< print-length n) print-length n) n)))
      (while (< i lim)
        (when (> i 0) (nelisp--prn-chunks-add chunks " "))
        (nelisp--prn-chunks-add chunks
                                (nelisp--prn-to-string (aref vec i) escape depth))
        (setq i (1+ i)))
      (when (< lim n)
        (when (> lim 0) (nelisp--prn-chunks-add chunks " "))
        (nelisp--prn-chunks-add chunks "...")))
    (nelisp--prn-chunks-add chunks "]")
    (nelisp--prn-chunks-string chunks)))

(defun nelisp--prn-record (rec escape)
  "Print RECORD as `#s(TYPE-TAG SLOT0 SLOT1 ...)'."
  (let ((tag  (nelisp--record-type rec))
        (n    (nelisp--record-length rec))
        (chunks (cons nil nil)))
    (nelisp--prn-chunks-add chunks "#s(")
    (nelisp--prn-chunks-add chunks (nelisp--prn-to-string tag escape))
    (let ((i 0))
      (while (< i n)
        (nelisp--prn-chunks-add chunks " ")
        (nelisp--prn-chunks-add
         chunks (nelisp--prn-to-string (nelisp--record-ref rec i) escape))
        (setq i (1+ i))))
    (nelisp--prn-chunks-add chunks ")")
    (nelisp--prn-chunks-string chunks)))

;; Kept in step with scripts/nelisp-stdlib-prelude.el, the copy the
;; standalone runs; `make ns-gate' reports any drift.
(defvar print-length nil)
(defvar print-level nil)

(defun nelisp--prn-symbol-needs-leading-escape-p (s)
  (let ((n (length s)))
    (cond
     ((= n 0) nil)                      ; handled by the ## case
     ((string= s ".") t)
     (t
      ;; Would the reader take this whole name for a number?
      (let ((i 0) (seen-digit nil) (ok t))
        (while (and ok (< i n))
          (let ((c (aref s i)))
            (cond
             ((and (>= c ?0) (<= c ?9)) (setq seen-digit t))
             ((and (= i 0) (or (eq c ?-) (eq c ?+))) nil)
             ((or (eq c ?.) (eq c ?e) (eq c ?E)) nil)
             (t (setq ok nil))))
          (setq i (1+ i)))
        (and ok seen-digit))))))

(defun nelisp--prn-to-string (obj escape &optional depth)
  (setq depth (or depth 0))
  (cond
   ((null obj) "nil")
   ((eq obj t) "t")
   ((integerp obj) (number-to-string obj))
   ((floatp obj)   (nelisp--prn-float obj))
   ((symbolp obj)
    (if escape
        (nelisp--prn-symbol-escaped (symbol-name obj))
      (symbol-name obj)))
   ((stringp obj)
    (if escape (concat "\"" (nelisp--prn-string-escaped obj) "\"") obj))
   ((consp obj)
    ;; Depth is a PARAMETER, not a special variable.  A free
    ;; `nelisp--prn-depth' worked in the standalone and broke
    ;; test/nelisp-stdlib-test.el, which evaluates only the `defun' forms of
    ;; lisp/nelisp-stdlib-prn.el -- so the defvar never ran and the counter
    ;; was void.  Threading it also means a caller cannot forget to rebind.
    (if (and print-level (>= depth print-level))
        "..."
      (or (nelisp--prn-reader-macro-abbrev obj escape)
          (concat "(" (nelisp--prn-list-body obj escape (1+ depth)) ")"))))
   ;; `print-level' bounds LIST nesting only -- Emacs prints
   ;; [1 [2 [3 [4]]]] in full at print-level 2, and only the list arm above
   ;; counts depth.  Measured rather than assumed; the first cut guarded
   ;; both and truncated vectors Emacs does not.
   ((vectorp obj) (nelisp--prn-vector obj escape (1+ depth)))
   ((recordp obj) (nelisp--prn-record obj escape))
   (t (format "#<unprintable %S>" obj))))

(defun prin1-to-string (object &optional noescape _overrides)
    (nelisp--prn-to-string object (not noescape)))

(defun terpri (&optional stream)
  "Output a newline to STREAM or `standard-output' (Doc 22 A9)."
  (let ((s (or stream standard-output)))
    (if (or (null s) (eq s t))
        (nelisp--write-stdout-bytes "\n")
      (nelisp--emit-to-stream "\n" s)))
  t)

(defun prin1 (object &optional stream)
  "Print OBJECT in read syntax to STREAM or `standard-output' (Doc 22 A9)."
  (let ((s (or stream standard-output)))
    (if (or (null s) (eq s t))
        (nelisp--write-stdout-bytes (nelisp--prn-to-string object t))
      (nelisp--emit-to-stream (nelisp--prn-to-string object t) s)))
  object)

(provide 'nelisp-stdlib-prn)

;;; nelisp-stdlib-prn.el ends here
