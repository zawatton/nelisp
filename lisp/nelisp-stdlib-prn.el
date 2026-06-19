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
;; head is one of `quote' / `function' / `backquote' / `comma' /
;; `comma-at' is rendered with the corresponding prefix (`\''
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
  "Return S with `\"' / `\\' / `\\n' / `\\r' / `\\t' escaped per Emacs prin1.
Other characters pass through verbatim, matching the Rust printer.
Char comparisons use raw integer codepoints (34 / 92 / 10 / 13 / 9)
to sidestep any difference in how `?\\X' literals get parsed by the
bundled reader vs the host."
  (let ((chunks (cons nil nil))
        (i 0)
        (n (length s)))
    (while (< i n)
      (let ((c (aref s i)))
        (cond
         ((= c 34) (nelisp--prn-chunks-add chunks "\\\"")) ; ?\"
         ((= c 92) (nelisp--prn-chunks-add chunks "\\\\")) ; ?\\
         ((= c 10) (nelisp--prn-chunks-add chunks "\\n"))  ; ?\n
         ((= c 13) (nelisp--prn-chunks-add chunks "\\r"))  ; ?\r
         ((= c 9)  (nelisp--prn-chunks-add chunks "\\t"))  ; ?\t
         (t        (nelisp--prn-chunks-add chunks (char-to-string c)))))
      (setq i (1+ i)))
    (nelisp--prn-chunks-string chunks)))

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
TAG ∈ {quote, function, backquote, comma, comma-at}; ARG is printed
recursively via `nelisp--prn-to-string' under ESCAPE.
We dispatch on `symbol-name' string equality to avoid any reader-side
re-interning of `backquote' / `comma' / `comma-at' under abbrev forms."
  (when (and (consp lst)
             (symbolp (car lst))
             (consp (cdr lst))
             (null (cdr (cdr lst))))
    (let* ((tag-name (symbol-name (car lst)))
           (arg (car (cdr lst)))
           (prefix (cond ((string= tag-name "quote")     "'")
                         ((string= tag-name "function")  "#'")
                         ((string= tag-name "backquote") "`")
                         ((string= tag-name "comma")     ",")
                         ((string= tag-name "comma-at")  ",@")
                         (t nil))))
      (when prefix
        (concat prefix (nelisp--prn-to-string arg escape))))))

(defun nelisp--prn-list-body (lst escape)
  "Print the body of LST (= cons cell) without enclosing parens.
Handles proper / dotted lists.  Element separator is a single space;
a non-nil non-cons tail prints as ` . TAIL'."
  (let ((chunks (cons nil nil))
        (cur lst)
        (first t))
    (while (consp cur)
      (unless first
        (nelisp--prn-chunks-add chunks " "))
      (nelisp--prn-chunks-add chunks
                              (nelisp--prn-to-string (car cur) escape))
      (setq first nil)
      (setq cur (cdr cur)))
    (unless (null cur)
      (nelisp--prn-chunks-add chunks " . ")
      (nelisp--prn-chunks-add chunks (nelisp--prn-to-string cur escape)))
    (nelisp--prn-chunks-string chunks)))

(defun nelisp--prn-vector (vec escape)
  "Print VEC as `[E0 E1 ...]'."
  (let ((n (length vec))
        (chunks (cons nil nil)))
    (nelisp--prn-chunks-add chunks "[")
    (let ((i 0))
      (while (< i n)
        (when (> i 0)
          (nelisp--prn-chunks-add chunks " "))
        (nelisp--prn-chunks-add chunks
                                (nelisp--prn-to-string (aref vec i) escape))
        (setq i (1+ i))))
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

(defun nelisp--prn-to-string (obj escape)
  "Convert OBJ to its printed representation.
ESCAPE = t  → readable form (= prin1).
ESCAPE = nil → non-readable form (= princ): strings without quotes /
escaping; everything else identical."
  (cond
   ((null obj) "nil")
   ((eq obj t) "t")
   ((integerp obj) (number-to-string obj))
   ((floatp obj)   (nelisp--prn-float obj))
   ((symbolp obj)  (symbol-name obj))
   ((stringp obj)
    (if escape
        (concat "\"" (nelisp--prn-string-escaped obj) "\"")
      obj))
   ((consp obj)
    (or (nelisp--prn-reader-macro-abbrev obj escape)
        (concat "(" (nelisp--prn-list-body obj escape) ")")))
   ((vectorp obj) (nelisp--prn-vector obj escape))
   ((recordp obj) (nelisp--prn-record obj escape))
   (t (format "#<unprintable %S>" obj))))

;; ---- user-visible functions ----

;; Override the function-cell installed by `install_builtins' (= the
;; `bi_prin1_to_string' Rust dispatcher).  Stage 7.1.4 also removes
;; the Rust arm so this elisp defun is the sole implementation.

(defun prin1-to-string (object)
  "Return a string containing the printed representation of OBJECT.
Strings, symbols and characters are escaped so the result reads back."
  (nelisp--prn-to-string object t))

(defun terpri (&optional _stream)
  "Write a newline character to STREAM.
STREAM defaults to stdout; non-nil values that are not functions
signal `error' (= MVP scope: only stdout / function streams supported,
matching the Rust princ sliver)."
  (nelisp--write-stdout-bytes "\n")
  nil)

(defun prin1 (object &optional _stream)
  "Output the printed representation of OBJECT to STREAM.
STREAM defaults to stdout.  Like `prin1-to-string' but also writes
the result to the stream and returns OBJECT."
  (nelisp--write-stdout-bytes (nelisp--prn-to-string object t))
  object)

(provide 'nelisp-stdlib-prn)

;;; nelisp-stdlib-prn.el ends here
