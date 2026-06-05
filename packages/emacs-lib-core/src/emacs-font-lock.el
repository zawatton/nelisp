;;; emacs-font-lock.el --- Prefixed font-lock substrate  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Doc 51 Track K (2026-05-03) — Layer 2 γ-deeper.
;;
;; Minimum-viable font-lock implementation.  Sits on top of the
;; existing text-property MVP in `emacs-buffer.el' (= the
;; `emacs-buffer-put-text-property' family) and the regex search
;; bridges in `emacs-search-builtins.el'.
;;
;; Supported keyword forms (subset of upstream
;; `font-lock-keywords' grammar):
;;
;;   STRING                      ; regexp, applies font-lock-keyword-face
;;   (REGEXP . SYMBOL)           ; SYMBOL is a face name (or face-var)
;;   (REGEXP . NUMBER)           ; numeric subexp -> font-lock-keyword-face
;;   (REGEXP (SUBEXP FACE [OVERRIDE [LAXMATCH]]) ...)   ; multi-highlight
;;
;; Shipped (Track D, 2026-05-04):
;;   - (eval . FORM) keyword form
;;   - font-lock-defaults slot 3 = CASE-FOLD honoured
;;   - override = `keep' / `prepend' / `append' with real list-merge
;; Shipped (Track G, 2026-05-04):
;;   - anchored matcher  (REGEX PRE POST HIGHLIGHTS...)
;;   - PRE-FORM evaluated as bound for the inner search
;;   - POST-FORM evaluated for state-restore (result ignored)
;;
;; Still deferred:
;;   - syntactic-keywords / syntax-table fontification (= Track F char-table dep)
;;   - jit-lock incremental fontification (= we do whole-region only)
;;   - font-lock-extend-region-functions hook
;;
;; Standard faces (`font-lock-keyword-face' etc) are defined in this
;; module so callers / modes can reference them at load-time without
;; pulling the host Emacs's `font-lock.el'.

;;; Code:

(require 'cl-lib)
(require 'emacs-buffer)
(require 'emacs-faces)
(require 'emacs-syntax-table)

;;;; --- standard faces ------------------------------------------------

(defconst emacs-font-lock--standard-faces
  '(font-lock-keyword-face
    font-lock-function-name-face
    font-lock-string-face
    font-lock-comment-face
    font-lock-comment-delimiter-face
    font-lock-doc-face
    font-lock-type-face
    font-lock-variable-name-face
    font-lock-constant-face
    font-lock-builtin-face
    font-lock-warning-face
    font-lock-preprocessor-face
    font-lock-negation-char-face
    font-lock-regexp-grouping-construct
    font-lock-regexp-grouping-backslash)
  "List of standard font-lock face names registered by Track K.")

(dolist (face emacs-font-lock--standard-faces)
  (unless (emacs-faces-facep face)
    (emacs-faces-make-face face)))

;;;; --- per-buffer state ----------------------------------------------

(defvar emacs-font-lock--state (make-hash-table :test 'eq :weakness 'key)
  "Per-buffer font-lock state, keyed by `nelisp-ec-buffer' object.
Value is a plist with keys
  :keywords  -- the active KEYWORDS list (post-compile)
  :defaults  -- the raw `font-lock-defaults' value
  :enabled   -- t when font-lock-mode is on for this buffer.")

(defun emacs-font-lock--ensure-state (buf)
  "Return the state plist for BUF, creating an empty one if absent."
  (or (gethash buf emacs-font-lock--state)
      (puthash buf (list :keywords nil :defaults nil :enabled nil)
               emacs-font-lock--state)))

(defun emacs-font-lock--state-get (buf prop)
  (plist-get (emacs-font-lock--ensure-state buf) prop))

(defun emacs-font-lock--state-set (buf prop val)
  (let ((p (emacs-font-lock--ensure-state buf)))
    (puthash buf (plist-put p prop val) emacs-font-lock--state)))

(defun emacs-font-lock--current-buffer ()
  (or (and (fboundp 'nelisp-ec--current-buffer)
           (boundp 'nelisp-ec--current-buffer)
           nelisp-ec--current-buffer)
      (and (fboundp 'emacs-buffer--current)
           (emacs-buffer--current))))

;;;; --- keyword compilation -------------------------------------------

(defun emacs-font-lock--compile-keyword (kw)
  "Normalise a font-lock KEYWORD to canonical form.
Returns a list of (REGEXP HIGHLIGHTS...) where each HIGHLIGHT is a
4-list (SUBEXP FACE OVERRIDE LAXMATCH).

`(eval . FORM)' is evaluated lazily — we wrap it in a sentinel
`(:eval . FORM)' so `--fontify-one-keyword' can call `eval' the
first time it runs (caching the materialised keyword in the
sentinel cdr after expansion).  This matches upstream Emacs's
`font-lock-keywords' semantics where the FORM is evaluated once
per buffer when fontification first runs."
  (cond
   ;; (eval . FORM) — defer materialisation to fontify-time.
   ;; Use `cons' (not `list') to keep the FORM dotted onto the cdr;
   ;; that way `--materialise-eval-cell' can do `(eval (cdr cell) t)'
   ;; without unwrapping an extra list level.
   ((and (consp kw) (eq (car kw) 'eval))
    (cons :eval (cdr kw)))
   ((stringp kw)
    (list kw (list 0 'font-lock-keyword-face nil nil)))
   ((and (consp kw) (stringp (car kw)) (symbolp (cdr kw)))
    (list (car kw) (list 0 (cdr kw) nil nil)))
   ((and (consp kw) (stringp (car kw)) (numberp (cdr kw)))
    (list (car kw) (list (cdr kw) 'font-lock-keyword-face nil nil)))
   ((and (consp kw) (stringp (car kw)) (listp (cdr kw)))
    (cons (car kw)
          (mapcar
           (lambda (h)
             (cond
              ;; Plain face symbol → (0 SYMBOL nil nil)
              ((symbolp h) (list 0 h nil nil))
              ;; SUBEXP-HIGHLIGHT (= numeric subexp + face/spec)
              ((and (consp h) (numberp (car h)))
               (let ((subexp (car h))
                     (face (cadr h))
                     (override (and (cddr h) (nth 2 h)))
                     (laxmatch (and (nthcdr 3 h) (nth 3 h))))
                 (list subexp face override laxmatch)))
              ;; ANCHORED-MATCHER:  (REGEXP PRE-FORM POST-FORM HIGHLIGHTS...)
              ;; Recognise by `stringp (car h)' — the inner regex.
              ((and (consp h) (stringp (car h)))
               (let* ((inner-regex (car h))
                      (pre-form    (and (cdr h) (nth 1 h)))
                      (post-form   (and (cddr h) (nth 2 h)))
                      (inner-highs (nthcdr 3 h))
                      ;; Normalise inner highlights via the same
                      ;; SUBEXP-HIGHLIGHT canonicalisation we use
                      ;; above (= recursive on each anchored sub).
                      (inner-canon
                       (mapcar
                        (lambda (ih)
                          (cond
                           ((symbolp ih) (list 0 ih nil nil))
                           ((and (consp ih) (numberp (car ih)))
                            (list (car ih)
                                  (cadr ih)
                                  (and (cddr ih) (nth 2 ih))
                                  (and (nthcdr 3 ih) (nth 3 ih))))
                           (t (list 0 'font-lock-keyword-face nil nil))))
                        inner-highs)))
                 (list :anchored inner-regex pre-form post-form inner-canon)))
              (t (list 0 'font-lock-keyword-face nil nil))))
           (cdr kw))))
   (t
    (list ".\\`" (list 0 'font-lock-keyword-face nil nil)))))

(defun emacs-font-lock--case-insensitive-regexp (regexp)
  "Convert REGEXP into a case-insensitive equivalent.
Replaces each ASCII letter with a `[Xx]' character class.  Skips
`\\\\' escape sequences and `[...]' character classes (= they are
copied through).  Used because `nelisp-regex' does not honour any
external `case-fold-search' switch."
  (let ((i 0)
        (n (length regexp))
        (out (make-string 0 0)))
    (while (< i n)
      (let ((c (aref regexp i)))
        (cond
         ;; Pass-through escape sequence.
         ((eq c ?\\)
          (setq out (concat out (substring regexp i (min n (+ i 2))))
                i (+ i 2)))
         ;; Pass-through char class.
         ((eq c ?\[)
          (let ((end (1+ i)))
            (while (and (< end n) (not (eq (aref regexp end) ?\])))
              (when (eq (aref regexp end) ?\\) (setq end (1+ end)))
              (setq end (1+ end)))
            (setq out (concat out (substring regexp i (min n (1+ end))))
                  i (1+ end))))
         ;; ASCII letter → [Xx]
         ((or (and (>= c ?a) (<= c ?z))
              (and (>= c ?A) (<= c ?Z)))
          (let ((lower (downcase c))
                (upper (upcase c)))
            (setq out (concat out (string ?\[ upper lower ?\]))
                  i (1+ i))))
         (t
          (setq out (concat out (string c))
                i (1+ i))))))
    out))

(defun emacs-font-lock--materialise-eval-cell (cell)
  "If CELL is an `(:eval . FORM)' sentinel, eval FORM and recompile.
Returns the materialised cell (a regular `(REGEXP HIGHLIGHTS...)' list)
or CELL itself when no expansion is needed.

The cell shape is `(:eval . FORM)' (= dotted cons), so `(cdr cell)'
yields the FORM directly without an extra list-level unwrap."
  (cond
   ((and (consp cell) (eq (car cell) :eval))
    (let ((form (cdr cell)))
      (condition-case _
          (let ((expanded (eval form t)))
            (when expanded
              (emacs-font-lock--compile-keyword expanded)))
        (error nil))))
   (t cell)))

(defun emacs-font-lock--compile-keywords (keywords)
  (mapcar #'emacs-font-lock--compile-keyword keywords))

;;;; --- defaults / add-keywords ---------------------------------------

(defun emacs-font-lock-set-defaults (&optional buf)
  "Initialise font-lock state from `font-lock-defaults' for BUF.
Defaults to the current buffer.  If `font-lock-defaults' is nil
or unbound, leaves :keywords as nil.

Slots honoured (Doc 51 Track D hardening):
  1. KEYWORDS         — keyword list (or symbol → variable lookup)
  2. KEYWORDS-ONLY    — when t, syntactic fontification is skipped.
                        Our MVP does not do syntactic fontification
                        anyway, so this slot is recorded but a no-op.
  3. CASE-FOLD        — when non-nil, regex matching is case-insensitive.
                        Captured into :case-fold and consulted in
                        `--fontify-one-keyword'.
Slots 4 (SYNTAX-ALIST) and 5+ (OTHER) are still deferred."
  (let* ((b (or buf (emacs-font-lock--current-buffer)))
         (defaults (and (boundp 'font-lock-defaults) font-lock-defaults))
         (slot0 (and (listp defaults) (car defaults)))
         (kw (cond
              ((null defaults) nil)
              ((symbolp slot0)
               (and (boundp slot0) (symbol-value slot0)))
              ((listp slot0) slot0)
              (t nil)))
         (case-fold (and (listp defaults)
                         (cdr (cdr defaults))
                         (nth 2 defaults))))
    (when b
      (emacs-font-lock--state-set b :defaults defaults)
      (emacs-font-lock--state-set b :keywords
                                  (emacs-font-lock--compile-keywords kw))
      (emacs-font-lock--state-set b :case-fold case-fold))
    kw))

(defun emacs-font-lock-add-keywords (mode keywords &optional how)
  "Append KEYWORDS to the current buffer's font-lock keyword set.
MODE is accepted for API parity with upstream and is ignored
(= we always update the current buffer).  HOW = `set' replaces,
HOW = t or `append' appends, default prepends."
  (ignore mode)
  (let* ((b (emacs-font-lock--current-buffer)))
    (when b
      (let ((compiled (emacs-font-lock--compile-keywords keywords))
            (existing (emacs-font-lock--state-get b :keywords)))
        (emacs-font-lock--state-set
         b :keywords
         (cond
          ((eq how 'set) compiled)
          ((or (eq how t) (eq how 'append))
           (append existing compiled))
          (t (append compiled existing))))))
    nil))

(defun emacs-font-lock-remove-keywords (mode keywords)
  "Remove KEYWORDS from the current buffer's font-lock keyword set.
MODE is accepted for API parity and ignored."
  (ignore mode)
  (let* ((b (emacs-font-lock--current-buffer)))
    (when b
      (let* ((to-remove (mapcar #'car (emacs-font-lock--compile-keywords keywords)))
             (existing (emacs-font-lock--state-get b :keywords))
             (filtered (cl-remove-if
                        (lambda (cell) (member (car cell) to-remove))
                        existing)))
        (emacs-font-lock--state-set b :keywords filtered)))
    nil))

;;;; --- fontification core --------------------------------------------

(defun emacs-font-lock-unfontify-region (start end &optional buf)
  "Remove the `face' text property on [START, END) in BUF."
  (let ((b (or buf (emacs-font-lock--current-buffer))))
    (when (and b (< start end))
      (emacs-buffer-remove-text-properties start end '(face) b))))

(defun emacs-font-lock-unfontify-buffer (&optional buf)
  "Remove the `face' text property over the whole buffer."
  (let* ((b (or buf (emacs-font-lock--current-buffer))))
    (when b
      (let* ((nelisp-ec--current-buffer b)
             (e (1+ (nelisp-ec-buffer-size))))
        (when (> e 1)
          (emacs-font-lock-unfontify-region 1 e b))))))

(defun emacs-font-lock--merge-face (existing new strategy)
  "Combine EXISTING and NEW face values per STRATEGY.
STRATEGY is one of `prepend' / `append' (= cons-into-list) or nil
(= replace).  EXISTING may be nil, a face symbol, or a list of
faces.  NEW is a face symbol / list / face-spec.  Returns a value
suitable for the `face' text-property."
  (cond
   ((null strategy) new)
   ((null existing) new)
   (t
    (let* ((ex-list (if (listp existing) existing (list existing)))
           (new-list (if (listp new) new (list new))))
      (cond
       ((eq strategy 'prepend) (append new-list ex-list))
       ((eq strategy 'append)  (append ex-list new-list))
       (t new))))))

(defun emacs-font-lock--apply-highlight (highlight buf)
  "Apply one HIGHLIGHT (SUBEXP FACE OVERRIDE LAXMATCH) to current match in BUF.

OVERRIDE semantics (Doc 51 Track D — full upstream parity):
  t          replace any existing face property
  `keep'     only set when no face is currently set
  `prepend'  cons NEW onto the front of the existing face list
  `append'   cons NEW onto the back of the existing face list
  nil        replace (= same as t in MVP — upstream uses this for
             non-overlapping keywords; our matcher already rewinds)"
  (let* ((subexp (nth 0 highlight))
         (face (nth 1 highlight))
         (override (nth 2 highlight))
         (laxmatch (nth 3 highlight))
         (m-beg (nelisp-ec-match-beginning subexp))
         (m-end (nelisp-ec-match-end subexp))
         (face-val (cond
                    ((symbolp face) face)
                    ((and (boundp face) (symbol-value face)))
                    (t face))))
    (cond
     ((and m-beg m-end (< m-beg m-end))
      (let ((cur (emacs-buffer-get-text-property m-beg 'face buf)))
        (cond
         ((eq override 'keep)
          (unless cur
            (emacs-buffer-put-text-property m-beg m-end 'face face-val buf)))
         ((eq override 'prepend)
          (emacs-buffer-put-text-property
           m-beg m-end 'face
           (emacs-font-lock--merge-face cur face-val 'prepend) buf))
         ((eq override 'append)
          (emacs-buffer-put-text-property
           m-beg m-end 'face
           (emacs-font-lock--merge-face cur face-val 'append) buf))
         (t
          (emacs-buffer-put-text-property m-beg m-end 'face face-val buf)))))
     ((not laxmatch)
      ;; subexp didn't match and laxmatch=nil → would error in upstream; we ignore.
      nil))))

(defun emacs-font-lock--fontify-one-keyword (cell start end buf)
  "Run REGEXP from CELL over [START, END) in BUF, applying highlights.

Uses the prefixed `nelisp-ec-re-search-forward' substrate directly
so the search runs against BUF rather than whatever `current-buffer'
the host Emacs would otherwise see.

Honours the per-buffer `:case-fold' state captured in
`emacs-font-lock-set-defaults' (slot 3 of `font-lock-defaults');
when non-nil the regex is wrapped with `(?i)' equivalent by
binding `case-fold-search' for the duration of the search.

`(:eval . FORM)' sentinel cells are materialised lazily on first
fontification: `--materialise-eval-cell' calls `eval' on the FORM
and the produced regular `(REGEXP HIGHLIGHTS...)' cell is used
for the rest of the call.  Failed evaluation returns nil and the
cell is skipped silently (= same behaviour as upstream)."
  (let ((real-cell (emacs-font-lock--materialise-eval-cell cell)))
    (when (and real-cell (consp real-cell)
               (not (eq (car real-cell) :eval)))
      (let* ((raw-regexp (car real-cell))
             (case-fold (emacs-font-lock--state-get buf :case-fold))
             (regexp (if case-fold
                         (emacs-font-lock--case-insensitive-regexp raw-regexp)
                       raw-regexp))
             (highlights (cdr real-cell))
             (nelisp-ec--current-buffer buf)
             (case-fold-search case-fold))
        (nelisp-ec-goto-char start)
        (while (and (< (nelisp-ec-point) end)
                    (nelisp-ec-re-search-forward regexp end t))
          (let ((mb (nelisp-ec-match-beginning 0))
                (me (nelisp-ec-match-end 0)))
            (dolist (h highlights)
              (cond
               ;; Anchored matcher: nested search after the outer match.
               ((and (consp h) (eq (car h) :anchored))
                (emacs-font-lock--apply-anchored h buf mb me end))
               ;; Plain SUBEXP-HIGHLIGHT.
               (t
                (emacs-font-lock--apply-highlight h buf))))
            ;; Advance past zero-width matches to prevent infinite loop.
            (when (and mb me (= mb me))
              (if (< (nelisp-ec-point) end)
                  (nelisp-ec-forward-char 1)
                (nelisp-ec-goto-char end)))))))))

(defun emacs-font-lock--eval-bound-form (form default-bound)
  "Evaluate FORM as an anchored-matcher PRE/POST sentinel.
Returns an integer BOUND for the inner search.  When FORM is nil we
return DEFAULT-BOUND.  When evaluation fails we silently fall back
to DEFAULT-BOUND so a buggy mode definition cannot crash fontify."
  (cond
   ((null form) default-bound)
   ((integerp form) form)
   (t
    (condition-case _
        (let ((v (eval form t)))
          (if (integerp v) v default-bound))
      (error default-bound)))))

(defun emacs-font-lock--apply-anchored (cell buf outer-mb outer-me region-end)
  "Run the anchored-matcher CELL after the outer match in BUF.
CELL is `(:anchored INNER-REGEX PRE-FORM POST-FORM HIGHLIGHTS-LIST)'.
OUTER-MB / OUTER-ME bound the just-matched outer range; REGION-END
is the overall fontify region's end (= upper search ceiling)."
  (let* ((inner-regex (nth 1 cell))
         (pre-form    (nth 2 cell))
         (post-form   (nth 3 cell))
         (highs       (nth 4 cell))
         (case-fold   (emacs-font-lock--state-get buf :case-fold))
         (inner-rx    (if case-fold
                          (emacs-font-lock--case-insensitive-regexp inner-regex)
                        inner-regex))
         ;; PRE-FORM result = bound for the inner search.  Default
         ;; to REGION-END so a nil pre-form still has a real ceiling.
         (bound       (let ((nelisp-ec--current-buffer buf))
                        (nelisp-ec-goto-char outer-me)
                        (emacs-font-lock--eval-bound-form pre-form region-end)))
         ;; Clamp bound into the outer fontify region.
         (effective-bound (min bound region-end)))
    (let ((nelisp-ec--current-buffer buf)
          (case-fold-search case-fold))
      (nelisp-ec-goto-char outer-me)
      (while (and (< (nelisp-ec-point) effective-bound)
                  (nelisp-ec-re-search-forward inner-rx effective-bound t))
        (let ((mb (nelisp-ec-match-beginning 0))
              (me (nelisp-ec-match-end 0)))
          (dolist (h highs)
            (emacs-font-lock--apply-highlight h buf))
          (when (and mb me (= mb me))
            (if (< (nelisp-ec-point) effective-bound)
                (nelisp-ec-forward-char 1)
              (nelisp-ec-goto-char effective-bound)))))
      ;; POST-FORM: opportunity to restore state.  Result is ignored.
      (when post-form
        (condition-case _
            (eval post-form t)
          (error nil))))
    ;; Suppress unused-variable warning under byte-compile.
    (ignore outer-mb)))

(defun emacs-font-lock-default-fontify-region (start end &optional _loudly buf)
  "Fontify [START, END) in BUF using the buffer's compiled keywords.

Doc 51 Track R (2026-05-04): after the keyword pass we do a
syntactic pre-pass via `emacs-syntax-apply-faces-region' that
overwrites string + line-comment ranges with the dedicated
`font-lock-string-face' / `font-lock-comment-face'.  Running it
after the keyword loop means syntactic faces always win over a
keyword that happened to fire inside a string / comment — which
is the upstream Emacs convention."
  (let* ((b (or buf (emacs-font-lock--current-buffer)))
         (kws (and b (emacs-font-lock--state-get b :keywords))))
    (when (and b (< start end))
      (let ((saved-point (let ((nelisp-ec--current-buffer b)) (nelisp-ec-point))))
        (unwind-protect
            (progn
              (when kws
                (dolist (cell kws)
                  (emacs-font-lock--fontify-one-keyword cell start end b)))
              ;; Syntactic post-pass — strings / comments win.
              (emacs-syntax-apply-faces-region start end b))
          (let ((nelisp-ec--current-buffer b))
            (nelisp-ec-goto-char saved-point)))
        t))))

(defun emacs-font-lock-fontify-region (start end &optional loudly)
  "Fontify [START, END) in the current buffer."
  (emacs-font-lock-default-fontify-region start end loudly
                                          (emacs-font-lock--current-buffer)))

(defun emacs-font-lock-fontify-buffer ()
  "Fontify the entire current buffer."
  (let* ((b (emacs-font-lock--current-buffer)))
    (when b
      (let* ((nelisp-ec--current-buffer b)
             (e (1+ (nelisp-ec-buffer-size))))
        (when (> e 1)
          (emacs-font-lock-default-fontify-region 1 e nil b))))))

;;;; --- mode toggle ---------------------------------------------------

(defun emacs-font-lock-mode (&optional arg)
  "Toggle font-lock-mode for the current buffer.
With ARG > 0 enables; ARG ≤ 0 disables; nil toggles."
  (let* ((b (emacs-font-lock--current-buffer)))
    (when b
      (let* ((cur (emacs-font-lock--state-get b :enabled))
             (new (cond
                   ((null arg) (not cur))
                   ((and (numberp arg) (> arg 0)) t)
                   ((and (numberp arg) (<= arg 0)) nil)
                   (arg t)
                   (t nil))))
        (emacs-font-lock--state-set b :enabled new)
        (cond
         (new
          ;; Pull defaults from font-lock-defaults if not yet set.
          (unless (emacs-font-lock--state-get b :keywords)
            (emacs-font-lock-set-defaults b))
          (emacs-font-lock-fontify-buffer))
         (t
          (emacs-font-lock-unfontify-buffer b)))
        new))))

(defun emacs-font-lock-mode-enabled-p (&optional buf)
  "Return non-nil when font-lock-mode is on in BUF (default: current)."
  (let ((b (or buf (emacs-font-lock--current-buffer))))
    (and b (emacs-font-lock--state-get b :enabled))))

;;;; --- introspection -------------------------------------------------

(defun emacs-font-lock-keywords (&optional buf)
  "Return the compiled keywords list for BUF (default: current)."
  (let ((b (or buf (emacs-font-lock--current-buffer))))
    (and b (emacs-font-lock--state-get b :keywords))))

;;;; --- jit-lock primitives (Doc 51 Track S) -------------------------------

;; Doc 51 Track S (2026-05-04) — incremental fontification surface.
;;
;; The model is intentionally simple: each buffer carries a single
;; *dirty-interval* (a cons (BEG . END) on the font-lock state plist).
;; Edit ops (= self-insert-command / newline / delete-region / load /
;; etc.) call `emacs-font-lock-mark-dirty-region' which UNIONS the
;; new range into the existing one — multiple dirty marks coalesce
;; into one interval.
;;
;; `emacs-font-lock-flush-pending' clears the marker and re-fontifies
;; just that interval (= the whole-buffer pass at the end of every
;; redisplay flush is O(n) on every keystroke without this, which
;; gets unusable past a few hundred lines).
;;
;; `emacs-font-lock-after-change-handler' is the canonical hook
;; handler shape (BEG END LEN → mark dirty).  Edit primitives plug
;; this directly into their post-edit recordings.
;;
;; Caveat: a multi-line string / comment that spans the dirty
;; boundary is NOT auto-expanded — we take the dirty interval as-is
;; and rely on the syntactic post-pass walking from the interval
;; start.  Editing the interior of a triple-quoted string can leave
;; stale faces above the cursor until a wider region is fontified
;; (= e.g. by `emacs-font-lock-fontify-buffer').  Acceptable MVP
;; trade-off; expansion can land as a follow-up once
;; `emacs-syntax-state-at' grows a "find-enclosing-string-start"
;; helper.

(defun emacs-font-lock-mark-dirty-region (start end &optional buf)
  "Record [START, END) as needing re-fontification on the next flush.
Multiple calls union into a single interval — minimum-START to
maximum-END.  No-op when font-lock state is unavailable for BUF.
Returns the new (BEG . END) interval."
  (let* ((b (or buf (emacs-font-lock--current-buffer))))
    (when b
      (let ((existing (emacs-font-lock--state-get b :dirty)))
        (if (consp existing)
            (progn
              (when (< start (car existing))
                (setcar existing start))
              (when (> end (cdr existing))
                (setcdr existing end))
              existing)
          (let ((interval (cons start end)))
            (emacs-font-lock--state-set b :dirty interval)
            interval))))))

(defun emacs-font-lock-pending-dirty-region (&optional buf)
  "Return the pending dirty (BEG . END) interval for BUF, or nil.
Read-only — does NOT clear the marker."
  (let ((b (or buf (emacs-font-lock--current-buffer))))
    (and b (emacs-font-lock--state-get b :dirty))))

(defun emacs-font-lock-flush-pending (&optional buf)
  "Re-fontify the dirty interval recorded for BUF and clear it.
Returns the (BEG . END) interval that was fontified, or nil if
nothing was pending.  Skipped when font-lock-mode is off for BUF
(= the dirty marker is still cleared so it doesn't leak)."
  (let* ((b (or buf (emacs-font-lock--current-buffer)))
         (dirty (and b (emacs-font-lock--state-get b :dirty))))
    (when dirty
      (emacs-font-lock--state-set b :dirty nil)
      (when (emacs-font-lock-mode-enabled-p b)
        (emacs-font-lock-default-fontify-region (car dirty) (cdr dirty)
                                                nil b))
      dirty)))

(defun emacs-font-lock-after-change-handler (beg end _len &optional buf)
  "After-change-functions-shaped handler: mark [BEG, END) dirty.
LEN (= the length of the deleted text) is currently ignored — we
only care about the *post-change* interval that needs re-fontify.
Suitable for direct registration on `after-change-functions' once
the substrate fires that hook, or for explicit invocation by the
edit primitive (= the existing pattern for
`emacs-buffer-record-insertion')."
  (emacs-font-lock-mark-dirty-region beg end buf))

(defun emacs-font-lock--remove-eq (item list)
  "Return LIST without elements `eq' to ITEM."
  (let ((out nil))
    (while list
      (unless (eq item (car list))
        (setq out (cons (car list) out)))
      (setq list (cdr list)))
    (nreverse out)))

(defun emacs-font-lock-jit-lock-register (function &optional _contextual)
  "Register FUNCTION in the lightweight jit-lock function list.

Standalone NeLisp does not implement lazy redisplay fontification yet,
but packages such as visual-wrap expect `jit-lock-register' to exist and
to remember registered functions."
  (unless (boundp 'jit-lock-functions)
    (defvar jit-lock-functions nil))
  (unless (memq function jit-lock-functions)
    (setq jit-lock-functions (append jit-lock-functions (list function))))
  function)

(defun emacs-font-lock-jit-lock-unregister (function)
  "Remove FUNCTION from the lightweight jit-lock function list."
  (when (boundp 'jit-lock-functions)
    (setq jit-lock-functions
          (emacs-font-lock--remove-eq function jit-lock-functions)))
  function)

(defun emacs-font-lock-jit-lock-functions (&optional buf)
  "Return the lightweight jit-lock functions registered for BUF.
BUF is accepted for API symmetry with other font-lock introspection
helpers; the current fallback stores jit-lock registrations in the
buffer-local-compatible `jit-lock-functions' variable."
  (ignore buf)
  (and (boundp 'jit-lock-functions) jit-lock-functions))

(provide 'emacs-font-lock)

;;; emacs-font-lock.el ends here
