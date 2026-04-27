;;; nelisp-textprop-display.el --- Display property + spec validator (Phase 9c.2)  -*- lexical-binding: t; -*-

;; Phase 9c.2 per Doc 41 LOCKED-2026-04-25-v2 §3.2 / §2.4 / §2.7.
;; Layer: Phase 9c text-property advanced (sister module of T138
;; `nelisp-emacs-compat-face', SHIPPED Doc 41 §3.1).
;;
;; Goal: provide the *display property pure-data layer* — spec
;; validator, accessor, attribute-query, two-spec merge — so that
;; `put-text-property' can carry display values, and a future Phase 11
;; display backend can pull *resolved spec payloads* without rebuilding
;; the validator.
;;
;; Contract: =DISPLAY_PROPERTY_CONTRACT_VERSION = 1= (Doc 41 §2.4 + §4.2).
;;
;; Public API (12 functions, all `nelisp-display-' prefix except `nelisp-displayp'):
;;
;;   Spec classification / predicates:
;;     `nelisp-display-spec-p'        OBJECT -> bool
;;     `nelisp-display-type-p'        SYM    -> bool
;;
;;   Validator (ship-gated by Doc 41 §3.2.1):
;;     `nelisp-display-spec-validate' SPEC   -> t | (signal nelisp-display-bad-spec)
;;
;;   Accessor (ship-gated by Doc 41 §3.2.2):
;;     `nelisp-display-spec-type'         SPEC -> SYMBOL
;;     `nelisp-display-spec-payload'      SPEC -> ANY
;;     `nelisp-display-spec-replace-spec' SPEC -> SUB-SPEC | nil
;;     `nelisp-display-spec-attribute'    SPEC ATTR -> VALUE
;;
;;   Query / merge convenience (Doc 41 §2.7 pull contract):
;;     `nelisp-display-attribute'         SPEC ATTR -> VALUE
;;     `nelisp-display-resolve'           SPEC FRAME -> SPEC
;;     `nelisp-display-merge'             SPEC1 SPEC2 -> LIST
;;     `nelisp-display-known-types'       () -> (SYMBOL ...)
;;
;; Display spec types (Doc 41 §2.4 LOCKED, the v0 set of 8):
;;
;;   string    — literal replacement text (any STRING)
;;   image     — (image . PROPS)  where PROPS is a plist of image attrs
;;   space     — (space . PROPS)  where PROPS carries :width / :height etc
;;   height    — (height HEIGHT)  HEIGHT = number, (NUM . NUM), `+N', or symbol
;;   raise     — (raise FACTOR)   FACTOR = number
;;   margin    — (margin LEFT-OR-RIGHT)  symbol = `left-margin' | `right-margin'
;;   slice     — (slice X Y W H)  4 numbers (or 4-element list inside `image')
;;   when      — (when CONDITION SPEC)  CONDITION = any form, SPEC = nested
;;   replace   — (replace SUB-SPEC) or extensible-alist form (Doc 41 §2.4 row 8)
;;
;; Note: a *display value* in Emacs is either a single spec or a list of
;; specs (Emacs `display' property value semantics).  This validator
;; accepts both: a top-level list whose head is *not* a known type
;; symbol is treated as a *cascade* of specs (each individually validated).
;;
;; Storage / hook integration (Doc 41 §3.2.3):
;;   - The validator is a pure function; the call into
;;     `put-text-property' / `add-text-properties' is wired in at the
;;     text-property primitive layer.  This module only owns the spec
;;     vocabulary; it does *not* touch interval-tree storage.
;;   - `nelisp-display-spec-validate' signals `nelisp-display-bad-spec'
;;     on rejection so the caller can re-signal `wrong-type-argument'
;;     with full backtrace info.
;;
;; Resolver model (Doc 41 §2.7 + §3.2 LOCKED, pull-on-demand):
;;   - `nelisp-display-resolve' is the *backend pull entrypoint* — it
;;     returns the spec normalized into list form (= every input is
;;     turned into the same shape Phase 11 backend can iterate).
;;     FRAME is currently informational only (= reserved for v2
;;     contract increment when frame-specific specs become legal).
;;   - `nelisp-display-merge' implements the *cascade overlay rule*
;;     used by Phase 11 when two specs apply to the same range
;;     (semantic = right-most spec wins per type, matching Emacs
;;     precedent for the `display' property).
;;
;; Non-goals (deferred):
;;   - actual *display backend rendering* (= Phase 11)
;;   - actual image file loading / decode (= Phase 11)
;;   - frame-specific spec selection beyond `:when' (= Phase 11)

;;; Code:

(require 'cl-lib)

;;; Errors

(define-error 'nelisp-display-error "NeLisp display property error")
(define-error 'nelisp-display-bad-spec
  "Invalid display spec value" 'nelisp-display-error)
(define-error 'nelisp-display-bad-type
  "Unknown display spec type" 'nelisp-display-error)
(define-error 'nelisp-display-bad-attribute
  "Invalid display spec attribute" 'nelisp-display-error)

;;; Contract version (Doc 41 §2.4 + §4.2 LOCKED v2)

(defconst nelisp-display-property-contract-version 1
  "Doc 41 §2.4 + §4.2 LOCKED contract version for the display property layer.
Increment only via a new Doc 41 LOCKED revision.")

;;; Tunables

(defconst nelisp-display-spec-nesting-limit 8
  "Max nesting depth for `when' / `replace' specs before truncation.
Used as a cycle guard during validate / resolve recursion.")

(defconst nelisp-display--known-types
  '(string image space height raise margin slice when replace)
  "Doc 41 §2.4 LOCKED v0 display type set (8 types + replace).
Order is informational; lookup is via `memq'.")

(defconst nelisp-display--margin-keywords
  '(left-margin right-margin)
  "Allowed symbols inside a (margin SYMBOL) spec.")

;;; Predicates

(defun nelisp-display-known-types ()
  "Return the list of recognised display spec type symbols (fresh copy).

MCP Parameters: (none)"
  (copy-sequence nelisp-display--known-types))

(defun nelisp-display-type-p (sym)
  "Return non-nil if SYM is one of the Doc 41 §2.4 LOCKED display types.

MCP Parameters: SYM — symbol to test."
  (and (symbolp sym)
       (memq sym nelisp-display--known-types)
       t))

(defun nelisp-display-spec-p (object)
  "Return non-nil if OBJECT is a syntactically valid display spec.
This is a *predicate* form of `nelisp-display-spec-validate' that
suppresses the signal and returns nil instead.

MCP Parameters: OBJECT — anything."
  (condition-case nil
      (progn (nelisp-display-spec-validate object) t)
    (nelisp-display-error nil)))

;;; Internal helpers

(defun nelisp-display--plist-p (plist)
  "Return non-nil if PLIST is an even-length keyword-keyed list."
  (and (listp plist)
       (zerop (mod (length plist) 2))
       (cl-loop for k in plist by #'cddr
                always (keywordp k))))

(defun nelisp-display--number-or-nil-p (x)
  "Return non-nil if X is a number or nil."
  (or (null x) (numberp x)))

(defun nelisp-display--height-form-p (h)
  "Return non-nil if H is a valid (height HEIGHT) HEIGHT form.
Accepts: number, (NUM . NUM) ratio, symbol, or function-call form
\(per Emacs precedent for the `height' display attribute)."
  (cond
   ((numberp h) t)
   ((symbolp h) t)
   ((and (consp h) (numberp (car h)) (numberp (cdr h))) t)
   ;; Function call form like (+ 2) or (* 1.5) — accept any cons
   ;; whose head is a symbol; payload is opaque to this layer.
   ((and (consp h) (symbolp (car-safe h))) t)
   (t nil)))

(defun nelisp-display--validate-1 (spec depth)
  "Validate SPEC at recursion DEPTH; signal on rejection.
Returns t on success.  See `nelisp-display-spec-validate' for the
public contract; this internal arm carries the depth counter so
nested `when' / `replace' specs can be bounded."
  (when (>= depth nelisp-display-spec-nesting-limit)
    (signal 'nelisp-display-bad-spec
            (list 'nesting-limit-exceeded spec)))
  (cond
   ;; ── 1. literal string is always valid (= `string' type sugar).
   ((stringp spec) t)
   ;; ── 2. nil is the empty-display sentinel; allowed.
   ((null spec) t)
   ;; ── 3. typed cons / list form.
   ((consp spec)
    (let ((head (car spec))
          (tail (cdr spec)))
      (cond
       ;; (image . PROPS)
       ((eq head 'image)
        (unless (nelisp-display--plist-p tail)
          (signal 'nelisp-display-bad-spec (list 'image-needs-plist spec)))
        t)
       ;; (space . PROPS)
       ((eq head 'space)
        (unless (nelisp-display--plist-p tail)
          (signal 'nelisp-display-bad-spec (list 'space-needs-plist spec)))
        t)
       ;; (height HEIGHT)
       ((eq head 'height)
        (unless (and (consp tail)
                     (null (cdr tail))
                     (nelisp-display--height-form-p (car tail)))
          (signal 'nelisp-display-bad-spec (list 'bad-height spec)))
        t)
       ;; (raise FACTOR)
       ((eq head 'raise)
        (unless (and (consp tail)
                     (null (cdr tail))
                     (numberp (car tail)))
          (signal 'nelisp-display-bad-spec (list 'bad-raise spec)))
        t)
       ;; (margin SYMBOL)
       ((eq head 'margin)
        (unless (and (consp tail)
                     (null (cdr tail))
                     (memq (car tail) nelisp-display--margin-keywords))
          (signal 'nelisp-display-bad-spec (list 'bad-margin spec)))
        t)
       ;; (slice X Y W H) — exactly 4 numbers.
       ((eq head 'slice)
        (unless (and (consp tail)
                     (= (length tail) 4)
                     (cl-every #'numberp tail))
          (signal 'nelisp-display-bad-spec (list 'bad-slice spec)))
        t)
       ;; (when CONDITION SPEC) — CONDITION opaque, SPEC recursive.
       ((eq head 'when)
        (unless (and (consp tail)
                     (consp (cdr tail))
                     (null (cddr tail)))
          (signal 'nelisp-display-bad-spec (list 'bad-when spec)))
        ;; CONDITION is opaque (any form); recurse into the nested SPEC.
        (nelisp-display--validate-1 (cadr tail) (1+ depth))
        t)
       ;; (replace SUB-SPEC) — recursive sub-spec.
       ((eq head 'replace)
        (unless (and (consp tail)
                     (null (cdr tail)))
          (signal 'nelisp-display-bad-spec (list 'bad-replace spec)))
        (nelisp-display--validate-1 (car tail) (1+ depth))
        t)
       ;; ── Cascade form: top-level list of specs.
       ;; Heuristic: if HEAD is a string or a known-type cons / nil,
       ;; treat the whole list as a cascade and validate each element.
       ((or (stringp head)
            (null head)
            (and (consp head)
                 (symbolp (car-safe head))
                 (nelisp-display-type-p (car-safe head)))
            (and (symbolp head)
                 (not (nelisp-display-type-p head))))
        (cond
         ;; Cascade arm: every element validates as a spec.
         ((cl-every (lambda (e)
                      (or (stringp e)
                          (null e)
                          (and (consp e)
                               (symbolp (car-safe e))
                               (nelisp-display-type-p (car-safe e)))))
                    spec)
          (dolist (e spec)
            (nelisp-display--validate-1 e (1+ depth)))
          t)
         (t
          (signal 'nelisp-display-bad-type (list head spec)))))
       (t
        (signal 'nelisp-display-bad-type (list head spec))))))
   ;; ── 4. anything else = invalid.
   (t
    (signal 'nelisp-display-bad-spec (list 'unrecognized-form spec)))))

;;; Validator (Doc 41 §3.2.1)

(defun nelisp-display-spec-validate (spec)
  "Validate SPEC as a Doc 41 §2.4 LOCKED display spec.
Returns t on success; on failure signals `nelisp-display-bad-spec'
or `nelisp-display-bad-type' with REASON details (suitable for
re-signal as `wrong-type-argument' at the text-property primitive
boundary, per §3.2.3).

Accepts:
  - nil                            (empty display)
  - STRING                         (literal text)
  - (image . PROPS)
  - (space . PROPS)
  - (height HEIGHT)
  - (raise FACTOR)
  - (margin SYMBOL)
  - (slice X Y W H)
  - (when CONDITION SPEC)
  - (replace SUB-SPEC)
  - (SPEC1 SPEC2 ...) cascade list of any of the above

MCP Parameters: SPEC — display spec value to validate."
  (nelisp-display--validate-1 spec 0))

;;; Accessors (Doc 41 §3.2.2)

(defun nelisp-display-spec-type (spec)
  "Return the type symbol of SPEC (Doc 41 §3.2.2).

Mapping:
  STRING                       -> `string'
  (TYPE . _) where TYPE known   -> TYPE
  (SPEC1 SPEC2 ...) cascade     -> `cascade'
  nil                           -> nil
  unrecognized                  -> signal `nelisp-display-bad-spec'

MCP Parameters: SPEC — display spec value."
  (cond
   ((null spec) nil)
   ((stringp spec) 'string)
   ((consp spec)
    (let ((head (car spec)))
      (cond
       ((nelisp-display-type-p head) head)
       ;; cascade form
       ((or (stringp head) (null head) (consp head))
        'cascade)
       (t (signal 'nelisp-display-bad-spec (list spec))))))
   (t (signal 'nelisp-display-bad-spec (list spec)))))

(defun nelisp-display-spec-payload (spec)
  "Return the payload of SPEC (= cdr of typed cons, or SPEC itself for sugar).

Per-type payload shape:
  string                -> the string itself
  (image . PROPS)        -> PROPS (plist)
  (space . PROPS)        -> PROPS (plist)
  (height H)             -> H
  (raise F)              -> F
  (margin S)             -> S
  (slice X Y W H)        -> (X Y W H)
  (when COND SPEC)       -> (COND SPEC)
  (replace SUB)          -> SUB
  cascade                -> the spec list itself
  nil                    -> nil

MCP Parameters: SPEC — display spec value."
  (cond
   ((null spec) nil)
   ((stringp spec) spec)
   ((consp spec)
    (let ((head (car spec))
          (tail (cdr spec)))
      (cond
       ((memq head '(image space)) tail)
       ((memq head '(height raise margin replace)) (car tail))
       ((eq head 'slice) tail)
       ((eq head 'when) tail)
       ;; cascade
       (t spec))))
   (t (signal 'nelisp-display-bad-spec (list spec)))))

(defun nelisp-display-spec-replace-spec (spec)
  "Return the inner SUB-SPEC of a (replace SUB-SPEC) or (when COND SUB) form.
Returns nil for any other spec shape (= caller can use nil to mean
\"no recursive sub-spec\", per Doc 41 §3.2.2).

MCP Parameters: SPEC — display spec value."
  (when (consp spec)
    (let ((head (car spec)))
      (cond
       ((eq head 'replace) (cadr spec))
       ((eq head 'when)    (caddr spec))
       (t nil)))))

(defun nelisp-display-spec-attribute (spec attribute)
  "Return ATTRIBUTE from SPEC's payload plist if applicable.
Only meaningful for `image' and `space' typed specs (which carry a
plist payload); for other types returns nil.

ATTRIBUTE must be a keyword.

MCP Parameters:
  SPEC      — display spec value.
  ATTRIBUTE — keyword."
  (unless (keywordp attribute)
    (signal 'nelisp-display-bad-attribute (list attribute)))
  (when (and (consp spec)
             (memq (car spec) '(image space)))
    (plist-get (cdr spec) attribute)))

;;; Query / merge convenience (Doc 41 §2.7 pull-on-demand)

(defun nelisp-display-attribute (spec attribute)
  "Return the resolved ATTRIBUTE of SPEC (or nil if unset).

For typed specs (`image', `space') ATTRIBUTE is a plist key.
For cascade specs the *first* contributing spec wins (left-to-right
search), matching Emacs `display' property precedence.

MCP Parameters:
  SPEC      — display spec value.
  ATTRIBUTE — keyword."
  (unless (keywordp attribute)
    (signal 'nelisp-display-bad-attribute (list attribute)))
  (cond
   ((or (null spec) (stringp spec)) nil)
   ((and (consp spec) (nelisp-display-type-p (car spec)))
    (nelisp-display-spec-attribute spec attribute))
   ((consp spec)
    ;; cascade — return first non-nil attribute hit
    (cl-loop for s in spec
             for v = (nelisp-display-attribute s attribute)
             when v return v))
   (t nil)))

(defun nelisp-display-resolve (spec frame)
  "Resolve SPEC into the canonical list form for FRAME (Doc 41 §2.7).

The current LOCKED v1 contract treats FRAME as *informational only*
- the validator + accessor layer is frame-agnostic.  Phase 11 backend
will use FRAME to evaluate (when CONDITION SPEC) clauses; at this
layer we just return the spec re-shaped as a list (cascade form):

  nil           -> nil
  STRING        -> (STRING)
  typed-cons    -> (SPEC)
  cascade list  -> SPEC unchanged (but freshly copied)

The output is always a *fresh list* the caller may mutate without
corrupting upstream storage.

MCP Parameters:
  SPEC  — display spec value.
  FRAME — frame object or nil (reserved, currently unused)."
  (ignore frame)
  (cond
   ((null spec) nil)
   ((stringp spec) (list spec))
   ((and (consp spec) (nelisp-display-type-p (car spec)))
    (list (copy-tree spec)))
   ((consp spec)
    (mapcar #'copy-tree spec))
   (t (signal 'nelisp-display-bad-spec (list spec)))))

(defun nelisp-display-merge (spec1 spec2)
  "Merge SPEC1 and SPEC2 into a single cascade list.
Both inputs are first resolved via `nelisp-display-resolve' (FRAME=nil)
so the result is a flat cascade.  The merge order is *left-to-right*
(SPEC1 first), matching Emacs `display' property merge semantics: the
first occurrence of a given type *wins*, later occurrences contribute
only if their type was not already represented.

Example:
  (nelisp-display-merge \\='\"X\" \\='(height 1.2))
  => (\"X\" (height 1.2))

  (nelisp-display-merge \\='((height 1.2) \"X\") \\='((height 0.8)))
  => ((height 1.2) \"X\")    ; later (height 0.8) suppressed

MCP Parameters:
  SPEC1, SPEC2 — display spec values."
  (let* ((left  (nelisp-display-resolve spec1 nil))
         (right (nelisp-display-resolve spec2 nil))
         (seen-types (mapcar (lambda (s)
                               (cond
                                ((stringp s) 'string)
                                ((and (consp s)
                                      (nelisp-display-type-p (car s)))
                                 (car s))
                                (t nil)))
                             left))
         (result (copy-sequence left)))
    (dolist (s right)
      (let ((ty (cond
                 ((stringp s) 'string)
                 ((and (consp s) (nelisp-display-type-p (car s)))
                  (car s))
                 (t nil))))
        (unless (and ty (memq ty seen-types))
          (setq result (nconc result (list s)))
          (when ty (push ty seen-types)))))
    result))

(provide 'nelisp-textprop-display)

;;; nelisp-textprop-display.el ends here
