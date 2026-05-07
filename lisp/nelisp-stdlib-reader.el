;;; nelisp-stdlib-reader.el --- elisp Sexp reader (Stage 7.2.a lexer)  -*- lexical-binding: t; -*-

;; Phase 7 Stage 7.2.a (2026-05-07, Doc 66).
;;
;; Tokenizer (= lexer) for the elisp port of `build-tool/src/reader/'.
;; Stage 7.2.a is the parallel-impl phase: the Rust reader still drives
;; every `read-from-string' call, the elisp tokenizer runs alongside
;; with its own ERT coverage.  Stage 7.2.b will add the parser + the
;; takeover hook in `bi_read_from_string'; Stage 7.2.d will remove the
;; Rust fallback once the round-trip property test gates have soaked.
;;
;; Token shape (= Doc 66 §2.2):
;;   (:type TYPE :value VALUE :line LINE :col COL)
;;
;; TYPE is one of:
;;   lparen rparen lbracket rbracket
;;   quote backquote comma comma-at function-quote
;;   dot int float str symbol sharps-paren
;;
;; The lexer covers the Doc 44 §3.2 LOCKED subset that the Rust lexer
;; covers; deferred features (= byte-code literal `#[...]', meta char
;; modifiers `?\M-X' beyond the ASCII case) signal a `nelisp-read-error'
;; with a descriptive message identical in spirit to the Rust
;; `ReadError::NotYetImplemented' variant.
;;
;; The lexer state is a 5-element vector `[STRING I LINE COL LEN]'
;; mutated through `nelisp--read-tok-bump'.  Vector slots are accessed
;; positionally rather than through `cl-defstruct' to keep dependency
;; surface small (= reader runs at substrate boot time before
;; `cl-defstruct' macro is defined).

;;; Code:

;; Stage 7.2.a uses plain `error' for reader failures.  Stage 7.2.b will
;; introduce a `nelisp-read-error' symbol once the parser surface needs
;; programmatic discrimination from generic `error' (= currently every
;; failure is a hard top-level error from ERT's perspective).

;; ---------------------------------------------------------------------------
;; Lexer state.  The vector layout is:
;;   slot 0 = STRING (the input)
;;   slot 1 = I       (current byte index)
;;   slot 2 = LINE    (1-based line counter)
;;   slot 3 = COL     (1-based column counter)
;;   slot 4 = LEN     (cached length of STRING)
;; ---------------------------------------------------------------------------

(defun nelisp--read-tok-state (string)
  "Build a fresh lexer state vector for STRING."
  (vector string 0 1 1 (length string)))

(defun nelisp--read-tok-peek (lx)
  "Char at the current position of LX, or nil at EOF."
  (let ((i (aref lx 1))
        (len (aref lx 4)))
    (when (< i len)
      (aref (aref lx 0) i))))

(defun nelisp--read-tok-peek-at (lx offset)
  "Char at position I+OFFSET in LX, or nil if past end."
  (let ((i (+ (aref lx 1) offset))
        (len (aref lx 4)))
    (when (< i len)
      (aref (aref lx 0) i))))

(defun nelisp--read-tok-bump (lx)
  "Consume the current char of LX, advance line/col, return the char."
  (let* ((i (aref lx 1))
         (c (aref (aref lx 0) i)))
    (aset lx 1 (1+ i))
    (if (eq c ?\n)
        (progn (aset lx 2 (1+ (aref lx 2)))
               (aset lx 3 1))
      (aset lx 3 (1+ (aref lx 3))))
    c))

(defun nelisp--read-tok-snapshot-pos (lx)
  "Return a (:line L :col C) plist for the current cursor in LX."
  (list :line (aref lx 2) :col (aref lx 3)))

;; ---------------------------------------------------------------------------
;; Char categorization predicates.  Mirrors `is_whitespace' /
;; `is_atom_terminator' / `hex_digit' in the Rust lexer.
;; ---------------------------------------------------------------------------

(defun nelisp--read-tok-whitespace-p (c)
  "Match `is_whitespace': space / tab / LF / CR / VT (11) / FF (12)."
  (or (eq c ?\s) (eq c ?\t) (eq c ?\n) (eq c ?\r)
      (eq c 11) (eq c 12)))

(defun nelisp--read-tok-atom-terminator-p (c)
  "Match `is_atom_terminator'."
  (or (nelisp--read-tok-whitespace-p c)
      (eq c ?\() (eq c ?\)) (eq c ?\[) (eq c ?\])
      (eq c ?\') (eq c ?`) (eq c ?,) (eq c ?\;) (eq c ?\")))

(defun nelisp--read-tok-hex-digit (c)
  "Return 0..15 if C is a hex digit, else nil."
  (cond
   ((null c) nil)
   ((and (>= c ?0) (<= c ?9)) (- c ?0))
   ((and (>= c ?a) (<= c ?f)) (+ 10 (- c ?a)))
   ((and (>= c ?A) (<= c ?F)) (+ 10 (- c ?A)))
   (t nil)))

(defun nelisp--read-tok-digit-p (c radix)
  "Non-nil if C is a digit valid in base RADIX."
  (let ((v (nelisp--read-tok-hex-digit c)))
    (and v (< v radix))))

;; ---------------------------------------------------------------------------
;; Skip whitespace + line comments.  Identical state machine to Rust's
;; `skip_whitespace_and_comments': loop alternating ws-run and `;'-to-EOL,
;; without consuming the newline that ends the comment.
;; ---------------------------------------------------------------------------

(defun nelisp--read-tok-skip-ws-comments (lx)
  "Advance LX past whitespace and `;'-line-comments to the next token."
  (let ((done nil))
    (while (not done)
      (setq done t)
      (while (let ((c (nelisp--read-tok-peek lx)))
               (and c (nelisp--read-tok-whitespace-p c)))
        (nelisp--read-tok-bump lx))
      (when (eq (nelisp--read-tok-peek lx) ?\;)
        (setq done nil)
        (while (let ((c (nelisp--read-tok-peek lx)))
                 (and c (not (eq c ?\n))))
          (nelisp--read-tok-bump lx))))))

;; ---------------------------------------------------------------------------
;; Token construction.
;; ---------------------------------------------------------------------------

(defun nelisp--read-tok-make (type value pos)
  "Build a token plist of TYPE / VALUE at POS = (:line L :col C)."
  (list :type type
        :value value
        :line (plist-get pos :line)
        :col (plist-get pos :col)))

(defun nelisp--read-tok-error (msg pos)
  "Raise a reader error for MSG at POS."
  (error "nelisp-read-error: %s (line %d col %d)"
         msg
         (plist-get pos :line)
         (plist-get pos :col)))

;; ---------------------------------------------------------------------------
;; String literal (`"..."').  Mirrors Rust's `read_string': escape set
;; is the union of \n \t \r \\ \" \0 plus the named control escapes (\e
;; \s \b \d \a \f \v) and \xNN; unknown \X falls through as literal X
;; per Emacs reader compatibility.
;; ---------------------------------------------------------------------------

(defun nelisp--read-tok-string (lx pos)
  "Tokenize a string literal at LX, opening `\"' at POS."
  (nelisp--read-tok-bump lx) ; consume opening "
  (let ((parts nil) (done nil))
    (while (not done)
      (let ((c (nelisp--read-tok-peek lx)))
        (cond
         ((null c)
          (nelisp--read-tok-error "unterminated string literal" pos))
         ((eq c ?\")
          (nelisp--read-tok-bump lx)
          (setq done t))
         ((eq c ?\\)
          (nelisp--read-tok-bump lx)
          (let ((esc-pos (nelisp--read-tok-snapshot-pos lx))
                (esc (nelisp--read-tok-peek lx)))
            (when (null esc)
              (nelisp--read-tok-error "unterminated escape in string" esc-pos))
            (nelisp--read-tok-bump lx)
            (cond
             ((eq esc ?\n) nil) ; line continuation, no output
             ((eq esc ?\r)
              (when (eq (nelisp--read-tok-peek lx) ?\n)
                (nelisp--read-tok-bump lx)))
             ((eq esc ?n) (push 10  parts))
             ((eq esc ?t) (push 9   parts))
             ((eq esc ?r) (push 13  parts))
             ((eq esc ?\\) (push 92 parts))
             ((eq esc ?\") (push 34 parts))
             ((eq esc ?0) (push 0   parts))
             ((eq esc ?e) (push 27  parts))
             ((eq esc ?s) (push 32  parts))
             ((eq esc ?b) (push 8   parts))
             ((eq esc ?d) (push 127 parts))
             ((eq esc ?a) (push 7   parts))
             ((eq esc ?f) (push 12  parts))
             ((eq esc ?v) (push 11  parts))
             ((eq esc ?x)
              (let ((h1 (nelisp--read-tok-peek lx)))
                (when (null h1)
                  (nelisp--read-tok-error
                   "\\x needs 2 hex digits in string" esc-pos))
                (nelisp--read-tok-bump lx)
                (let ((h2 (nelisp--read-tok-peek lx)))
                  (when (null h2)
                    (nelisp--read-tok-error
                     "\\x needs 2 hex digits in string" esc-pos))
                  (nelisp--read-tok-bump lx)
                  (let ((hv1 (nelisp--read-tok-hex-digit h1))
                        (hv2 (nelisp--read-tok-hex-digit h2)))
                    (when (or (null hv1) (null hv2))
                      (nelisp--read-tok-error
                       "invalid hex digit in \\x escape" esc-pos))
                    (push (logior (ash hv1 4) hv2) parts)))))
             ;; Unknown \X — Emacs compat: drop the backslash, emit X.
             (t (push esc parts)))))
         (t
          (nelisp--read-tok-bump lx)
          (push c parts)))))
    (nelisp--read-tok-make 'str (apply 'string (nreverse parts)) pos)))

;; ---------------------------------------------------------------------------
;; Radix integer (`#xNN', `#oNN', `#bNN').
;; ---------------------------------------------------------------------------

(defun nelisp--read-tok-radix-int (lx radix pos)
  "Read a radix-RADIX integer; `#X' prefix already consumed."
  (let ((sign 1) (val 0) (digits-seen nil))
    (when (memq (nelisp--read-tok-peek lx) '(?+ ?-))
      (when (eq (nelisp--read-tok-peek lx) ?-)
        (setq sign -1))
      (nelisp--read-tok-bump lx))
    (let ((done nil))
      (while (not done)
        (let ((c (nelisp--read-tok-peek lx)))
          (cond
           ((null c) (setq done t))
           ((eq c ?_) (nelisp--read-tok-bump lx))
           ((nelisp--read-tok-digit-p c radix)
            (setq val (+ (* val radix) (nelisp--read-tok-hex-digit c)))
            (setq digits-seen t)
            (nelisp--read-tok-bump lx))
           ((nelisp--read-tok-atom-terminator-p c) (setq done t))
           (t (nelisp--read-tok-error
               (format "invalid digit `%c' for radix %d integer" c radix)
               pos))))))
    (unless digits-seen
      (nelisp--read-tok-error
       (format "radix-%d integer requires at least one digit" radix)
       pos))
    (nelisp--read-tok-make 'int (* sign val) pos)))

;; ---------------------------------------------------------------------------
;; Sharpsign (`#x' / `#o' / `#b' / `#'' / `#s(' / `#[' deferred).
;; ---------------------------------------------------------------------------

(defun nelisp--read-tok-sharpsign (lx pos)
  "Tokenize a `#'-prefixed form; `#' at LX has not been consumed."
  (nelisp--read-tok-bump lx) ; consume `#'
  (let ((c (nelisp--read-tok-peek lx)))
    (cond
     ((null c)
      (nelisp--read-tok-error "unexpected EOF after `#'" pos))
     ((or (eq c ?x) (eq c ?X))
      (nelisp--read-tok-bump lx)
      (nelisp--read-tok-radix-int lx 16 pos))
     ((or (eq c ?o) (eq c ?O))
      (nelisp--read-tok-bump lx)
      (nelisp--read-tok-radix-int lx 8 pos))
     ((or (eq c ?b) (eq c ?B))
      (nelisp--read-tok-bump lx)
      (nelisp--read-tok-radix-int lx 2 pos))
     ((eq c ?\')
      (nelisp--read-tok-bump lx)
      (nelisp--read-tok-make 'function-quote nil pos))
     ((eq c ?\[)
      (nelisp--read-tok-error
       "byte-code literal #[...] is deferred (Doc 44 §3.2)" pos))
     ((eq c ?s)
      (nelisp--read-tok-bump lx)
      (let ((next (nelisp--read-tok-peek lx)))
        (cond
         ((eq next ?\()
          (nelisp--read-tok-bump lx)
          (nelisp--read-tok-make 'sharps-paren nil pos))
         ((null next)
          (nelisp--read-tok-error "unexpected EOF after `#s'" pos))
         (t
          (nelisp--read-tok-error
           (format "expected `(' after `#s', got `%c'" next) pos)))))
     (t
      (nelisp--read-tok-error
       (format "unsupported sharpsign form: #%c" c) pos)))))

;; ---------------------------------------------------------------------------
;; Number parsing (try-int / try-float).  These operate on the
;; concatenated atom text (= the full byte run before the next
;; atom-terminator).
;; ---------------------------------------------------------------------------

(defun nelisp--read-tok-strip-underscores (text)
  "Strip `_' digit separators from TEXT."
  (let ((parts nil) (i 0) (len (length text)))
    (while (< i len)
      (let ((c (aref text i)))
        (unless (eq c ?_)
          (push c parts)))
      (setq i (1+ i)))
    (apply 'string (nreverse parts))))

(defun nelisp--read-tok-text-has-fp-marker-p (text)
  "Non-nil if TEXT contains `.' / `e' / `E' (= forces float interpretation)."
  (or (string-search "." text)
      (string-search "e" text)
      (string-search "E" text)))

(defun nelisp--read-tok-try-int (text)
  "Parse TEXT as an integer (with optional sign), or return nil."
  (let ((s (nelisp--read-tok-strip-underscores text)))
    (cond
     ((string= s "") nil)
     ((nelisp--read-tok-text-has-fp-marker-p s) nil)
     (t
      (let ((i 0) (len (length s)) (all-digits t))
        ;; optional sign
        (when (and (> len 0)
                   (or (eq (aref s 0) ?+) (eq (aref s 0) ?-)))
          (setq i 1))
        (when (= i len) (setq all-digits nil))
        (while (and (< i len) all-digits)
          (let ((c (aref s i)))
            (if (and (>= c ?0) (<= c ?9))
                (setq i (1+ i))
              (setq all-digits nil))))
        (when all-digits
          (string-to-number s)))))))

(defun nelisp--read-tok-try-float (text)
  "Parse TEXT as a float if it looks like one, else nil.

Bootstrap subset: rely on `string-to-number' for the actual parse and
require the result to be a `floatp' value.  `string-to-number' returns
0 (integer) for non-numeric text and a float for valid float text;
treating only `floatp' results as success rejects the symbol shape
`1.x' that the Rust `try_parse_float' also rejects.  NaN / Inf
rejection is left to Stage 7.2.b parser-side validation since the
substrate has no `isnan' primitive yet."
  (when (nelisp--read-tok-text-has-fp-marker-p text)
    (let ((f (string-to-number text)))
      (and (floatp f) f))))

;; ---------------------------------------------------------------------------
;; Atom (= non-prefix run).  Scan all non-terminator bytes, then try to
;; interpret as `.', integer, float, or symbol.
;; ---------------------------------------------------------------------------

(defun nelisp--read-tok-atom (lx pos)
  "Tokenize a non-prefixed atom run starting at LX."
  (let ((parts nil) (done nil))
    (while (not done)
      (let ((c (nelisp--read-tok-peek lx)))
        (cond
         ((null c) (setq done t))
         ((nelisp--read-tok-atom-terminator-p c) (setq done t))
         (t
          (nelisp--read-tok-bump lx)
          (push c parts)))))
    (let ((text (apply 'string (nreverse parts))))
      (cond
       ((string= text "")
        (nelisp--read-tok-error "unexpected character" pos))
       ((string= text ".")
        (nelisp--read-tok-make 'dot nil pos))
       (t
        (let ((iv (nelisp--read-tok-try-int text)))
          (if iv
              (nelisp--read-tok-make 'int iv pos)
            (let ((fv (nelisp--read-tok-try-float text)))
              (if fv
                  (nelisp--read-tok-make 'float fv pos)
                (nelisp--read-tok-make 'symbol text pos))))))))))

;; ---------------------------------------------------------------------------
;; Char literal (`?X' / `?\X' / `?\xNN' / `?\C-X' / `?\M-X').  The
;; emitted token type is `int' (= same as numeric literals); the
;; downstream parser does not distinguish.
;; ---------------------------------------------------------------------------

(defun nelisp--read-tok-char-literal (lx pos)
  "Tokenize a `?X' char literal; `?' has not been consumed."
  (nelisp--read-tok-bump lx) ; consume `?'
  (let ((c (nelisp--read-tok-peek lx)))
    (when (null c)
      (nelisp--read-tok-error "unterminated char literal after `?'" pos))
    (cond
     ((not (eq c ?\\))
      ;; Plain `?X'
      (nelisp--read-tok-bump lx)
      (if (< c 128)
          (nelisp--read-tok-make 'int c pos)
        (nelisp--read-tok-error "multibyte char literal is deferred" pos)))
     (t
      ;; `?\X'
      (nelisp--read-tok-bump lx)            ; consume `\'
      (let ((esc-pos (nelisp--read-tok-snapshot-pos lx))
            (next (nelisp--read-tok-peek lx)))
        (when (null next)
          (nelisp--read-tok-error
           "unterminated escape in char literal" esc-pos))
        (nelisp--read-tok-bump lx)
        (cond
         ((eq next ?n) (nelisp--read-tok-make 'int 10  pos))
         ((eq next ?t) (nelisp--read-tok-make 'int 9   pos))
         ((eq next ?r) (nelisp--read-tok-make 'int 13  pos))
         ((eq next ?\\) (nelisp--read-tok-make 'int 92 pos))
         ((eq next ?\') (nelisp--read-tok-make 'int 39 pos))
         ((eq next ?\") (nelisp--read-tok-make 'int 34 pos))
         ((eq next ?s) (nelisp--read-tok-make 'int 32  pos))
         ((eq next ?e) (nelisp--read-tok-make 'int 27  pos))
         ((eq next ?b) (nelisp--read-tok-make 'int 8   pos))
         ((eq next ?d) (nelisp--read-tok-make 'int 127 pos))
         ((eq next ?a) (nelisp--read-tok-make 'int 7   pos))
         ((eq next ?f) (nelisp--read-tok-make 'int 12  pos))
         ((eq next ?v) (nelisp--read-tok-make 'int 11  pos))
         ((eq next ?0) (nelisp--read-tok-make 'int 0   pos))
         ((eq next ?x)
          (let ((h1 (nelisp--read-tok-peek lx)))
            (when (null h1)
              (nelisp--read-tok-error "\\x needs 2 hex digits" esc-pos))
            (nelisp--read-tok-bump lx)
            (let ((h2 (nelisp--read-tok-peek lx)))
              (when (null h2)
                (nelisp--read-tok-error "\\x needs 2 hex digits" esc-pos))
              (nelisp--read-tok-bump lx)
              (let ((hv1 (nelisp--read-tok-hex-digit h1))
                    (hv2 (nelisp--read-tok-hex-digit h2)))
                (when (or (null hv1) (null hv2))
                  (nelisp--read-tok-error
                   "invalid hex digit in char literal" esc-pos))
                (nelisp--read-tok-make 'int (logior (ash hv1 4) hv2) pos)))))
         ((eq next ?C)
          (unless (eq (nelisp--read-tok-peek lx) ?-)
            (nelisp--read-tok-error
             "expected `-' after \\C in char literal" esc-pos))
          (nelisp--read-tok-bump lx)
          (let ((ctrl (nelisp--read-tok-peek lx)))
            (when (null ctrl)
              (nelisp--read-tok-error
               "missing control target in char literal" esc-pos))
            (nelisp--read-tok-bump lx)
            (when (>= ctrl 128)
              (nelisp--read-tok-error
               "multibyte control char literal is deferred" esc-pos))
            (nelisp--read-tok-make 'int (logand ctrl 31) pos)))
         ((eq next ?M)
          ;; `\M-X' = X | 0x8000000.  Only support a single `-X' or
          ;; nested `-\C-X' / `-\n' / `-\t' / `-\r' for the bootstrap
          ;; subset (= matches Rust lexer Phase 3-A''-2 coverage).
          (unless (eq (nelisp--read-tok-peek lx) ?-)
            (nelisp--read-tok-error
             "expected `-' after \\M in char literal" esc-pos))
          (nelisp--read-tok-bump lx)
          (let ((nb (nelisp--read-tok-peek lx)))
            (when (null nb)
              (nelisp--read-tok-error
               "missing meta target in char literal" esc-pos))
            (nelisp--read-tok-bump lx)
            (let ((base
                   (cond
                    ((eq nb ?\\)
                     (let ((inner (nelisp--read-tok-peek lx)))
                       (when (null inner)
                         (nelisp--read-tok-error
                          "unterminated nested escape after \\M-\\"
                          esc-pos))
                       (nelisp--read-tok-bump lx)
                       (cond
                        ((eq inner ?C)
                         (unless (eq (nelisp--read-tok-peek lx) ?-)
                           (nelisp--read-tok-error
                            "expected `-' after nested \\C" esc-pos))
                         (nelisp--read-tok-bump lx)
                         (let ((cc (nelisp--read-tok-peek lx)))
                           (when (null cc)
                             (nelisp--read-tok-error
                              "missing control target after \\M-\\C-"
                              esc-pos))
                           (nelisp--read-tok-bump lx)
                           (logand cc 31)))
                        ((eq inner ?n) 10)
                        ((eq inner ?t) 9)
                        ((eq inner ?r) 13)
                        (t inner))))
                    (t nb))))
              (nelisp--read-tok-make 'int (logior base 134217728) pos))))
         ;; Unknown `?\X' — Emacs compat: literal char.
         (t (nelisp--read-tok-make 'int next pos))))))))

;; ---------------------------------------------------------------------------
;; Top-level dispatch.
;; ---------------------------------------------------------------------------

(defun nelisp--read-next-token (lx)
  "Return the next token from LX, or nil at EOF."
  (nelisp--read-tok-skip-ws-comments lx)
  (let ((c (nelisp--read-tok-peek lx)))
    (when c
      (let ((pos (nelisp--read-tok-snapshot-pos lx)))
        (cond
         ((eq c ?\() (nelisp--read-tok-bump lx)
                     (nelisp--read-tok-make 'lparen nil pos))
         ((eq c ?\)) (nelisp--read-tok-bump lx)
                     (nelisp--read-tok-make 'rparen nil pos))
         ((eq c ?\[) (nelisp--read-tok-bump lx)
                     (nelisp--read-tok-make 'lbracket nil pos))
         ((eq c ?\]) (nelisp--read-tok-bump lx)
                     (nelisp--read-tok-make 'rbracket nil pos))
         ((eq c ?\') (nelisp--read-tok-bump lx)
                     (nelisp--read-tok-make 'quote nil pos))
         ((eq c ?`)  (nelisp--read-tok-bump lx)
                     (nelisp--read-tok-make 'backquote nil pos))
         ((eq c ?,)
          (nelisp--read-tok-bump lx)
          (if (eq (nelisp--read-tok-peek lx) ?@)
              (progn (nelisp--read-tok-bump lx)
                     (nelisp--read-tok-make 'comma-at nil pos))
            (nelisp--read-tok-make 'comma nil pos)))
         ((eq c ?\") (nelisp--read-tok-string lx pos))
         ((eq c ?#)  (nelisp--read-tok-sharpsign lx pos))
         ((eq c ?\?)
          ;; Doc 51 Phase 3-A''-3: bare `?' followed by whitespace or EOF
          ;; is the symbol `?', everything else is a char literal.
          (let ((nx (nelisp--read-tok-peek-at lx 1)))
            (cond
             ((null nx) (nelisp--read-tok-atom lx pos))
             ((nelisp--read-tok-whitespace-p nx) (nelisp--read-tok-atom lx pos))
             (t (nelisp--read-tok-char-literal lx pos)))))
         (t (nelisp--read-tok-atom lx pos)))))))

(defun nelisp--read-tokenize (string)
  "Tokenize STRING, returning a list of token plists.
Each token has shape `(:type TYPE :value VALUE :line L :col C)'.
Stage 7.2.a — Doc 66 §2.2."
  (let ((lx (nelisp--read-tok-state string))
        (tokens nil))
    (let ((tok (nelisp--read-next-token lx)))
      (while tok
        (push tok tokens)
        (setq tok (nelisp--read-next-token lx))))
    (nreverse tokens)))

(provide 'nelisp-stdlib-reader)

;;; nelisp-stdlib-reader.el ends here
