;;; emacs-subr-extras.el --- subr.el primitive shims for cl-lib bootstrap  -*- lexical-binding: t; -*-

;; Phase B2 (= Doc anvil-runtime pure-elisp roadmap, 2026-05-09)
;;
;; Standalone NeLisp ships nelisp-stdlib's string-prefix-p /
;; string-suffix-p / split-string / plist-get etc., but four
;; primitives that vendor `cl-lib.el' / `subr-x.el' reach for at
;; load time are missing:
;;
;;   `number-sequence' / `assoc-default' / `string-join' /
;;   `member-ignore-case'
;;
;; Plus the four-level `caaaar' .. `cddddr' family — fifteen entries
;; (`cadddr' is provided by NeLisp itself; the other 15 are absent).
;; `cl-lib.el' line 424 onwards does `(defalias 'cl-caaaar 'caaaar)'
;; etc. and trips `void-function caaaar' on standalone NeLisp without
;; them.
;;
;; Those nineteen `defun's are gathered here so `(require 'cl-lib)' /
;; `(require 'subr-x)' load cleanly.  Once that is in place, the rest
;; of the `anvil-server.el' load chain (= json + anvil-server-metrics
;; + anvil-server itself) goes through.  See memory entry
;; `project_anvil_runtime_phase_b1_breakthroughs' for the full audit.
;;
;; Each shim is gated on `unless (fboundp ...)' so loading under
;; host Emacs is a cheap no-op.

;;; Code:

;; ---- subr.el primitives missing from nelisp-stdlib ----

(unless (fboundp 'number-sequence)
  (defun number-sequence (from &optional to inc)
    "Return a sequence of numbers from FROM to TO (inclusive) by INC.
INC defaults to 1.  TO can be nil (= return single-element list).
Negative INC is supported when FROM > TO."
    (let ((step (or inc 1))
          (acc nil)
          (cur from))
      (cond
       ((null to) (list from))
       ((> step 0)
        (while (<= cur to)
          (setq acc (cons cur acc))
          (setq cur (+ cur step)))
        (nreverse acc))
       (t
        (while (>= cur to)
          (setq acc (cons cur acc))
          (setq cur (+ cur step)))
        (nreverse acc))))))

(unless (fboundp 'assoc-default)
  (defun assoc-default (key alist &optional test default)
    "Find object KEY in pseudo-alist ALIST.
Each ALIST entry is either a cons (KEY . VALUE) or a bare KEY.
TEST is called with the element (or its car) and KEY; defaults
to `equal'.  When a match is found, return the cdr if the
element is a cons, otherwise DEFAULT.  When no element matches,
return nil."
    (let (found
          (tail alist)
          value)
      (while (and tail (not found))
        (let ((elt (car tail)))
          (when (and elt
                     (funcall (or test #'equal)
                              (if (consp elt) (car elt) elt)
                              key))
            (setq found t)
            (setq value (if (consp elt) (cdr elt) default))))
        (setq tail (cdr tail)))
      value)))

(unless (fboundp 'string-join)
  (defun string-join (strings &optional separator)
    "Join all STRINGS using SEPARATOR (default empty string)."
    (mapconcat #'identity strings (or separator ""))))

(unless (fboundp 'member-ignore-case)
  (defun member-ignore-case (elt list)
    "Like `member', but case-insensitive on string elements."
    (while (and list
                (not (eq t (compare-strings elt 0 nil (car list) 0 nil t))))
      (setq list (cdr list)))
    list))

;; ---- length comparison primitives (Emacs 29+ C builtins) ----
;; Vendor packages (and modern subr-x users) call `length<' / `length=' /
;; `length>' pervasively.  For lists these walk at most LENGTH cells instead
;; of computing the full length, so they stay correct on long lists; arrays
;; fall back to `length'.

(unless (fboundp 'length=)
  (defun length= (sequence length)
    "Return non-nil if SEQUENCE has exactly LENGTH elements."
    (if (listp sequence)
        (let ((n length))
          (while (and (consp sequence) (> n 0))
            (setq sequence (cdr sequence) n (1- n)))
          (and (= n 0) (null sequence)))
      (= (length sequence) length))))

(unless (fboundp 'length<)
  (defun length< (sequence length)
    "Return non-nil if SEQUENCE is shorter than LENGTH."
    (if (listp sequence)
        (let ((n length))
          (while (and (consp sequence) (> n 0))
            (setq sequence (cdr sequence) n (1- n)))
          (and (null sequence) (> n 0)))
      (< (length sequence) length))))

(unless (fboundp 'length>)
  (defun length> (sequence length)
    "Return non-nil if SEQUENCE is longer than LENGTH."
    (if (listp sequence)
        (let ((n length))
          (while (and (consp sequence) (> n 0))
            (setq sequence (cdr sequence) n (1- n)))
          (consp sequence))
      (> (length sequence) length))))

;; ---- four-level caaaar..cddddr (cadddr is NeLisp builtin) ----

(unless (fboundp 'caaaar) (defun caaaar (x) (car (car (car (car x))))))
(unless (fboundp 'caaadr) (defun caaadr (x) (car (car (car (cdr x))))))
(unless (fboundp 'caadar) (defun caadar (x) (car (car (cdr (car x))))))
(unless (fboundp 'caaddr) (defun caaddr (x) (car (car (cdr (cdr x))))))
(unless (fboundp 'cadaar) (defun cadaar (x) (car (cdr (car (car x))))))
(unless (fboundp 'cadadr) (defun cadadr (x) (car (cdr (car (cdr x))))))
(unless (fboundp 'caddar) (defun caddar (x) (car (cdr (cdr (car x))))))
(unless (fboundp 'cdaaar) (defun cdaaar (x) (cdr (car (car (car x))))))
(unless (fboundp 'cdaadr) (defun cdaadr (x) (cdr (car (car (cdr x))))))
(unless (fboundp 'cdadar) (defun cdadar (x) (cdr (car (cdr (car x))))))
(unless (fboundp 'cdaddr) (defun cdaddr (x) (cdr (car (cdr (cdr x))))))
(unless (fboundp 'cddaar) (defun cddaar (x) (cdr (cdr (car (car x))))))
(unless (fboundp 'cddadr) (defun cddadr (x) (cdr (cdr (car (cdr x))))))
(unless (fboundp 'cdddar) (defun cdddar (x) (cdr (cdr (cdr (car x))))))
(unless (fboundp 'cddddr) (defun cddddr (x) (cdr (cdr (cdr (cdr x))))))

;; ---- if-let* / when-let* / if-let / when-let (subr.el 2626+) ----
;;
;; In modern Emacs these macros live in `subr.el' (NOT `subr-x.el')
;; and are preloaded at dump time so callers never `require' them
;; explicitly.  Standalone NeLisp does not preload `subr.el', so
;; `(void-function if-let*)' fires for any anvil-*.el module that
;; uses the macro.  The minimal expansion below is sufficient for
;; the spec form `((SYM VALFORM) ...)' that anvil-server.el and
;; friends actually use; the obsolete bare-symbol shorthand is not
;; supported.

(defun emacs-subr-extras--build-if-let (varlist then else)
  "Build the expansion for `if-let*'.  VARLIST is a list of
`(SYM VALFORM)' entries (or bare SYM, treated as `(SYM SYM)').
Returns a sexp that evaluates THEN when every binding is non-nil
and ELSE (a list of forms to splice into a `progn') otherwise."
  (let ((else-form (if else (cons 'progn else) nil))
        (continuation then)
        (entries (reverse varlist)))
    (while entries
      (let* ((entry (car entries))
             (sym (if (consp entry) (car entry) entry))
             (val (if (consp entry) (cadr entry) entry)))
        (setq continuation
              `(let ((,sym ,val))
                 (if ,sym ,continuation ,else-form))))
      (setq entries (cdr entries)))
    continuation))

(unless (fboundp 'if-let*)
  (defmacro if-let* (varlist then &rest else)
    "If all bindings in VARLIST evaluate non-nil, eval THEN, else ELSE.
Each VARLIST entry is `(SYMBOL VALUEFORM)' or just `SYMBOL'.  Bindings
are sequential — later forms see earlier symbols."
    (declare (indent 2))
    (emacs-subr-extras--build-if-let varlist then else)))

(unless (fboundp 'when-let*)
  (defmacro when-let* (varlist &rest body)
    "Bind variables sequentially per VARLIST and eval BODY when all bindings are non-nil."
    (declare (indent 1))
    `(if-let* ,varlist (progn ,@body))))

(unless (fboundp 'if-let)
  (defalias 'if-let 'if-let*
    "Compatibility alias for `if-let*' (= deprecated bare-symbol form
not supported in this minimal port)."))

(unless (fboundp 'when-let)
  (defalias 'when-let 'when-let*
    "Compatibility alias for `when-let*'."))

;; ---- interactive-context primitives ----
;;
;; Standalone NeLisp has no Emacs UI, so `called-interactively-p'
;; always returns nil — every call is a programmatic call.  This
;; matches the safe behaviour for batch-mode Emacs.

(unless (fboundp 'called-interactively-p)
  (defun called-interactively-p (&optional _kind)
    "Return non-nil when the calling command was invoked interactively.
Reads the interactive-call flag set by `call-interactively' /
`funcall-interactively' (Doc 06 A5); see
`emacs-command-loop--called-interactively' for the approximation's limits.
KIND is accepted for API parity but not distinguished."
    (and (boundp 'emacs-command-loop--called-interactively)
         emacs-command-loop--called-interactively)))

(unless (fboundp 'apply-partially)
  (defun apply-partially (fun &rest args)
    "Return a function that calls FUN with leading ARGS plus its callers' args."
    (lambda (&rest more) (apply fun (append args more)))))

(unless (fboundp 'ignore-errors)
  (defmacro ignore-errors (&rest body)
    "Eval BODY, returning nil if any error is signalled."
    (declare (indent 0))
    `(condition-case nil (progn ,@body) (error nil))))

(unless (fboundp 'ignore)
  (defun ignore (&rest _ignored)
    "Do nothing and return nil — for use as a callback that disregards args."
    nil))

;; ---- subr.el property-helper primitives ----
;;
;; Vendor `cl-macs.el' uses `define-symbol-prop' to attach metadata
;; (= cl-deftype, cl-typep, compiler-macro etc.) to symbols.  Minimal
;; port matches host Emacs `put' semantics; the current-load-list
;; bookkeeping that real `define-symbol-prop' performs is irrelevant
;; on standalone (= unload-feature is not used).

(unless (fboundp 'define-symbol-prop)
  (defun define-symbol-prop (symbol prop val)
    "Define the property PROP of SYMBOL to be VAL.
Minimal subr.el port — matches host Emacs `put' behaviour but
skips the `current-load-list' bookkeeping (= unused on standalone)."
    (put symbol prop val)))

;; ---- byte-compile-only metadata declarations ----
;;
;; `declare-function' is a byte-compiler hint in stock Emacs and a
;; runtime no-op.  Vendor `anvil-server.el' / `cl-extra.el' and many
;; subr-derived files put these at top level; standalone NeLisp has
;; no byte compiler, so the simplest thing is to register a no-op
;; macro that swallows the args.

(unless (fboundp 'declare-function)
  (defmacro declare-function (&rest _args)
    "No-op stub for the byte-compiler hint `declare-function'."
    nil))

;; ---- Compiler-macro helper for `cXXr' family ----
;;
;; Vendor `cl-macs.el' references `internal--compiler-macro-cXXr'
;; (= subr.el line 598) as a compiler-macro for the entire car/cdr
;; composition family.  Without it `(load cl-macs.el)' silently
;; truncates partway through — `cl-letf' / `cl-defstruct' register
;; but `cl-callf' / `cl-callf2' (= line 2886+) never get defined,
;; which then breaks `cl-incf' expansion at runtime.  Port verbatim
;; from subr.el so the cXXr forms collapse to nested car/cdr at
;; compile time.

(unless (fboundp 'internal--compiler-macro-cXXr)
  (defun internal--compiler-macro-cXXr (form x)
    (let* ((head (car form))
           (n (symbol-name head))
           (i (- (length n) 2)))
      (if (not (string-match "c[ad]+r\\'" n))
          (if (and (fboundp head) (symbolp (symbol-function head)))
              (internal--compiler-macro-cXXr
               (cons (symbol-function head) (cdr form)) x)
            (error "Compiler macro for cXXr applied to non-cXXr form"))
        (while (> i (match-beginning 0))
          (setq x (list (if (eq (aref n i) ?a) 'car 'cdr) x))
          (setq i (1- i)))
        x))))

;; Doc 06 B3: event-modifiers / event-basic-type / event-convert-list.
;; Modifier bit values (low to high); control on a letter and an uppercase
;; letter are encoded specially as in Emacs.  Modifier-list *order* may differ
;; from the host (callers use `memq'); values and basic types match.
(defconst emacs-subr-extras--event-modifier-bits
  '((alt . 4194304) (super . 8388608) (hyper . 16777216)
    (shift . 33554432) (control . 67108864) (meta . 134217728)))

(defconst emacs-subr-extras--event-modifier-prefixes
  '((?A . alt) (?C . control) (?H . hyper) (?M . meta) (?S . shift) (?s . super)))

(defun emacs-subr-extras--split-symbol-modifiers (sym)
  "Return (MODS . BASE-STRING) by stripping leading X- prefixes from SYM."
  (let ((name (symbol-name sym)) (mods nil) (done nil))
    (while (not done)
      (if (and (>= (length name) 2)
               (eq (aref name 1) ?-)
               (assq (aref name 0) emacs-subr-extras--event-modifier-prefixes))
          (progn
            (push (cdr (assq (aref name 0)
                             emacs-subr-extras--event-modifier-prefixes))
                  mods)
            (setq name (substring name 2)))
        (setq done t)))
    (cons (nreverse mods) name)))

(defun emacs-subr-extras-event-modifiers (event)
  "Return the list of modifier symbols of EVENT (a char or key symbol)."
  (cond
   ((symbolp event) (car (emacs-subr-extras--split-symbol-modifiers event)))
   ((integerp event)
    (let ((mods nil) (base event))
      (dolist (cell emacs-subr-extras--event-modifier-bits)
        (when (/= 0 (logand event (cdr cell)))
          (push (car cell) mods)
          (setq base (- base (cdr cell)))))
      (cond
       ((and (>= base 1) (<= base 31)) (push 'control mods))
       ((and (>= base ?A) (<= base ?Z)) (push 'shift mods)))
      mods))
   (t nil)))

(defun emacs-subr-extras-event-basic-type (event)
  "Return the base type of EVENT (modifiers stripped)."
  (cond
   ((symbolp event)
    (intern (cdr (emacs-subr-extras--split-symbol-modifiers event))))
   ((integerp event)
    (let ((base event))
      (dolist (cell emacs-subr-extras--event-modifier-bits)
        (when (/= 0 (logand event (cdr cell)))
          (setq base (- base (cdr cell)))))
      (cond
       ((and (>= base 1) (<= base 26)) (+ base 96))
       ((and (>= base 27) (<= base 31)) (+ base 64))
       ((and (>= base ?A) (<= base ?Z)) (+ base 32))
       (t base))))
   (t event)))

(defun emacs-subr-extras-event-convert-list (list)
  "Convert LIST (MODIFIERS... BASE) into an event (char or key symbol)."
  (let ((base (car (last list)))
        (mods (butlast list)))
    (if (integerp base)
        (let ((c base) (m (copy-sequence mods)))
          (when (and (memq 'control m) (>= c ?a) (<= c ?z))
            (setq c (- c 96) m (delq 'control m)))
          (when (and (memq 'control m) (>= c ?A) (<= c ?Z))
            (setq c (- c 64) m (delq 'control m)))
          (dolist (mod m)
            (let ((bit (cdr (assq mod emacs-subr-extras--event-modifier-bits))))
              (when bit (setq c (logior c bit)))))
          c)
      (let ((prefix ""))
        (dolist (cell '((alt . "A-") (control . "C-") (hyper . "H-")
                        (meta . "M-") (shift . "S-") (super . "s-")))
          (when (memq (car cell) mods)
            (setq prefix (concat prefix (cdr cell)))))
        (intern (concat prefix (symbol-name base)))))))

(unless (fboundp 'event-modifiers)
  (defun event-modifiers (event) (emacs-subr-extras-event-modifiers event)))
(unless (fboundp 'event-basic-type)
  (defun event-basic-type (event) (emacs-subr-extras-event-basic-type event)))
(unless (and (fboundp 'event-convert-list)
             (not (get 'event-convert-list 'emacs-stub-bulk)))
  (defun event-convert-list (list) (emacs-subr-extras-event-convert-list list))
  (put 'event-convert-list 'emacs-stub-bulk nil))

;; Doc 06 A2: GC statistics API.  The standalone runtime collects automatically
;; at form boundaries and exposes no manual-collect-with-stats primitive, so
;; these provide the host-shaped return structure (counts are placeholders) to
;; unblock callers that depend on the API shape.  Under host Emacs the C
;; builtins win.  `garbage-collect' is stub-aware (it ships as a nil stub).
(unless (and (fboundp 'garbage-collect)
             (not (get 'garbage-collect 'emacs-stub-bulk)))
  (defun garbage-collect ()
    "Return a host-shaped garbage-collection statistics list.
The standalone nelisp runtime collects automatically; counts here are
placeholders, but the list structure matches Emacs so callers that index it
work."
    (list '(conses 16 0 0)
          '(symbols 48 0 0)
          '(strings 32 0 0)
          '(string-bytes 1 0)
          '(vectors 16 0)
          '(vector-slots 8 0 0)
          '(floats 8 0 0)
          '(intervals 56 0 0)
          '(buffers 1000 0)))
  (put 'garbage-collect 'emacs-stub-bulk nil))

(unless (fboundp 'memory-use-counts)
  (defun memory-use-counts ()
    "Return a host-shaped list of seven consing counters (placeholders).
Per-type totals are not exposed by the standalone runtime:
\(CONSES FLOATS VECTOR-CELLS SYMBOLS STRING-CHARS INTERVALS STRINGS)."
    (list 0 0 0 0 0 0 0)))

(provide 'emacs-subr-extras)
;;; emacs-subr-extras.el ends here
