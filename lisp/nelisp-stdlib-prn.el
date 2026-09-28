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

(defun nelisp--prn-control-char-p (c)
  "Return non-nil when C is a C-locale control character.
Mirrors GNU `c_iscntrl' (lib/c-ctype.h) for the ASCII case this runtime
always uses: codepoints 0x00-0x1F and 0x7F (DEL)."
  (or (< c 32) (= c 127)))

(defun nelisp--prn-octal-digit-count (c next-is-octal-digit)
  "Return how many octal digits GNU `octalout' (src/print.c) emits for C.
3 when C >= 0o100 (64) or NEXT-IS-OCTAL-DIGIT is non-nil -- the latter so a
reader re-scanning `\\N' followed by a literal 0-7 digit cannot absorb that
digit into the escape and misread it as one longer number; else 2 when
C >= 8; else 1."
  (cond ((or (> c 63) next-is-octal-digit) 3)
        ((> c 7) 2)
        (t 1)))

(defun nelisp--prn-octal-escape (c &optional digits)
  "Return the octal escape for codepoint C (0-255).
DIGITS fixes the width (1, 2, or 3); omit it for the always-safe 3-digit
form the pre-existing Doc 200 unibyte-byte escape below uses
unconditionally.  A caller that must match GNU's variable width exactly
(see `nelisp--prn-octal-digit-count') passes it explicitly.  Built with one
fixed-arity `concat' call per width rather than an accumulating
setq/concat loop, which the source-scanning perf guard
`nelisp-stdlib-printer-bounds-concat-calls' (test/nelisp-stdlib-test.el)
would flag."
  (cond
   ((eq digits 1) (concat "\\" (char-to-string (+ 48 (logand c 7)))))
   ((eq digits 2) (concat "\\"
                          (char-to-string (+ 48 (logand (ash c -3) 7)))
                          (char-to-string (+ 48 (logand c 7)))))
   (t             (concat "\\"
                          (char-to-string (+ 48 (logand (ash c -6) 7)))
                          (char-to-string (+ 48 (logand (ash c -3) 7)))
                          (char-to-string (+ 48 (logand c 7)))))))

(defun nelisp--prn-hex-digit-char-p (c)
  "Non-nil when C is an ASCII hex digit: 0-9, a-f, or A-F."
  (or (and (>= c 48) (<= c 57))
      (and (>= c 97) (<= c 102))
      (and (>= c 65) (<= c 70))))

(defun nelisp--prn-string-escaped (s)
  "Return S with characters escaped the way Emacs `prin1' escapes them.
`\"' and `\\' are always doubled.  The remaining escapes are controlled by
the standard `print-escape-*' variables (src/print.c `print_object', GNU
31.1): `print-escape-newlines' turns a literal newline/formfeed into the
two-character sequence `\\n'/`\\f'; `print-escape-control-characters' turns
any other C0/DEL control character into an octal escape, GNU's exact
variable width (see `nelisp--prn-octal-digit-count') -- when
`print-escape-newlines' is nil, a newline/formfeed still falls under this
one, since both variables gate against the SAME literal character and
`print-escape-newlines' is checked first; `print-escape-multibyte' turns a
non-ASCII character of a multibyte string into a `\\xXXXX' hex escape,
followed by a `\\ ' separator (GNU's `need_nonhex') before the NEXT
character if that character would otherwise read as more hex digits of the
same escape.  All four default to nil, matching GNU's C defaults, so
unbound callers see exactly the previous fixed behavior: control
characters other than the quote/backslash pair pass through verbatim.

Char comparisons use raw integer codepoints (34 / 92 / 10 / 12) to sidestep
any difference in how `?\\X' literals get parsed by the bundled reader vs
the host.  For a tag-14/15 unibyte string, Doc 200 additionally requires
every byte >= 128 to print as octal, never as a raw byte mistaken for
UTF-8 -- that rule is unconditional and independent of
`print-escape-nonascii' (which this runtime binds for `boundp' parity but
does not yet gate any behavior on, since the Doc 200 rule already forces
the stricter octal form GNU's flag would only opt into)."
  (let ((chunks (cons nil nil))
        (i 0)
        (n (length s))
        (need-nonhex nil)
        (unibyte (and (fboundp 'unibyte-string-p)
                      (unibyte-string-p s))))
    (while (< i n)
      (let ((c (aref s i)))
        (cond
         ((= c 34) (setq need-nonhex nil) (nelisp--prn-chunks-add chunks "\\\"")) ; ?\"
         ((= c 92) (setq need-nonhex nil) (nelisp--prn-chunks-add chunks "\\\\")) ; ?\\
         ((and unibyte (>= c 128))
          (setq need-nonhex nil)
          (nelisp--prn-chunks-add chunks (nelisp--prn-octal-escape c)))
         ((and print-escape-newlines (= c 10))
          (setq need-nonhex nil) (nelisp--prn-chunks-add chunks "\\n"))
         ((and print-escape-newlines (= c 12))
          (setq need-nonhex nil) (nelisp--prn-chunks-add chunks "\\f"))
         ((and print-escape-control-characters (nelisp--prn-control-char-p c))
          (setq need-nonhex nil)
          (nelisp--prn-chunks-add
           chunks
           (nelisp--prn-octal-escape
            c (nelisp--prn-octal-digit-count
               c (and (< (1+ i) n)
                      (let ((nc (aref s (1+ i)))) (and (>= nc 48) (<= nc 55))))))))
         ((and print-escape-multibyte (not unibyte) (>= c 128))
          (setq need-nonhex t)
          (nelisp--prn-chunks-add chunks (format "\\x%04x" c)))
         (t
          (when (and need-nonhex (nelisp--prn-hex-digit-char-p c))
            (nelisp--prn-chunks-add chunks "\\ "))
          (setq need-nonhex nil)
          (nelisp--prn-chunks-add chunks (char-to-string c)))))
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
  "Return the printed representation of float X.
`number-to-string' already produces Emacs's exact `prin1'/`princ'
spelling for every float -- the trailing `.0', the `e+NN'/`e-NN'
exponent form, and the `1.0e+INF' / `-1.0e+INF' / `0.0e+NaN' special
forms included (Doc 159 SS10 forward; that is where `number-to-string'
became a real shortest-round-trip printer rather than the %g-plus-one-
digit stub the old version of this docstring described).  This used to
locate `.'/`e' with `string-search' and hand-trim trailing zeros to
patch up that stub's output; Doc 159 SS15 found the patch redundant
and, worse, ~200x the cost of the single `number-to-string' call it
wrapped -- `string-search' has no native form here and its interpreted
cost dominates every other part of printing a float."
  (number-to-string x))

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
;; The four escape toggles below were previously unbound on the standalone
;; (`(boundp 'print-escape-newlines)' => nil), so binding one around a
;; `prin1'/`format "%S"' call had no effect: the printer never looked at
;; them.  Defaults match GNU src/print.c (all nil).  See
;; `nelisp--prn-string-escaped' for what each one does.
(defvar print-escape-newlines nil
  "Non-nil means print newlines in strings as `\\n'.
Also print formfeeds as `\\f'.")
(defvar print-escape-control-characters nil
  "Non-nil means print control characters in strings as `\\OOO'.
\(OOO is the octal representation of the character code.)")
(defvar print-escape-nonascii nil
  "Non-nil means print unibyte non-ASCII chars in strings as \\OOO.
\(OOO is the octal representation of the character code.)
Only single-byte characters are affected, and only in `prin1'.
Bound for `boundp' parity; this runtime's Doc 200 unibyte-string rule
already forces the octal form unconditionally, so this variable does
not yet gate anything (see `nelisp--prn-string-escaped').")
(defvar print-escape-multibyte nil
  "Non-nil means print multibyte characters in strings as \\xXXXX.
\(XXXX is the hex representation of the character code.)
This affects only `prin1'.")

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
   ((and (fboundp 'byte-code-function-p) (byte-code-function-p obj))
    (if (fboundp 'nelisp--repr) (nelisp--repr obj) (format "%S" obj)))
   ;; `print-level' bounds LIST nesting only -- Emacs prints
   ;; [1 [2 [3 [4]]]] in full at print-level 2, and only the list arm above
   ;; counts depth.  Measured rather than assumed; the first cut guarded
   ;; both and truncated vectors Emacs does not.
   ;; Overlay: printed opaquely, matching Emacs's own `print.c' and the
   ;; equivalent clause in `scripts/nelisp-stdlib-prelude.el' (the copy
   ;; the standalone actually runs).  Without this, an overlay falls
   ;; through to the generic `nelisp--prn-record' arm below, which
   ;; recurses into every slot with no cycle guard -- and button.el's
   ;; `make-button' stores the overlay in its own `button' property
   ;; (`(overlay-put overlay 'button overlay)'), so printing it hangs
   ;; forever instead of erroring.  See the prelude's copy of this
   ;; clause for the confirmed repro and Emacs-parity rendering.
   ((and (fboundp 'nelisp-overlay-p) (nelisp-overlay-p obj))
    (if (nelisp-overlay-buffer obj)
        (concat "#<overlay from " (number-to-string (nelisp-overlay-start obj))
                " to " (number-to-string (nelisp-overlay-end obj))
                " in " (nelisp-buffer-name (nelisp-overlay-buffer obj)) ">")
      "#<overlay in no buffer>"))
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
