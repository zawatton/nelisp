;;; nelisp-emacs-compat-face.el --- Face property + spec resolver (Phase 9c.1)  -*- lexical-binding: t; -*-

;; Phase 9c.1 per Doc 41 LOCKED-2026-04-25-v2 §3.1 / §4.1.
;; Layer: Phase 9c text-property advanced (Layer 2 extension on top of
;; Doc 34 §4.6 text-property MVP, SHIPPED).
;;
;; Goal: provide the *face property pure-data layer* — registry,
;; spec normalize, attribute merge, `:inherit' chain expansion — so that
;; `put-text-property' can carry face values, and a future Phase 11
;; display backend can pull *resolved attribute plists* without rebuilding
;; the spec layer.
;;
;; Contract: =FACE_PROPERTY_CONTRACT_VERSION = 1= (Doc 41 §4.1).
;;
;; Public API (10 functions, all `nelisp-face-' prefix):
;;
;;   Core resolver / merge (Doc 41 §4.1, ship-gated 7 = §4.1 + 1 alias):
;;     `nelisp-face-resolve'           FACE-VALUE -> ATTR-PLIST
;;     `nelisp-face-attribute-merge'   FACE-LIST  -> ATTR-PLIST
;;     `nelisp-face-define'            NAME ATTR-PLIST
;;     `nelisp-face-attributes'        NAME -> ATTR-PLIST | nil
;;     `nelisp-face-list'              () -> (NAME ...)
;;     `nelisp-face-equal-p'           F1 F2 -> bool
;;     `nelisp-facep'                  OBJ -> bool
;;
;;   Convenience wrappers (Emacs precedent names, all delegate to the 7):
;;     `nelisp-face-foreground'        FACE -> STR | nil
;;     `nelisp-face-background'        FACE -> STR | nil
;;     `nelisp-face-attribute'         FACE ATTR -> VALUE
;;
;; Storage model (Doc 41 §2.1 + §2.2 LOCKED):
;;   - face value is opaque payload riding on Doc 34 §2.6 interval tree
;;     (= no parallel face-only structure; we just normalize on PUT).
;;   - face spec is one of three normalized forms (per §2.2 LOCKED):
;;       face-symbol               -> registry lookup
;;       (face-symbol ...)         -> cascade list, left-to-right merge
;;       (:foreground STR ...)     -> raw plist, used as-is
;;       nil                       -> empty merge (no contribution)
;;   - registry is *per-runtime global* (per §4.1 invariants);
;;     not per-buffer, not frame-aware (Phase 11 deals with frame).
;;
;; Resolver model (Doc 41 §2.3 LOCKED = pure left-to-right merge):
;;   - left attribute wins on conflict (Emacs semantic)
;;   - `:inherit' triggers chain expansion, depth-bounded at 16 (cycle-safe)
;;   - `unspecified' values (= the symbol `unspecified') are *kept*
;;     in the result so the backend can decide fallback
;;
;; Non-goals (deferred):
;;   - actual *rendering* (= Phase 11 display backend)
;;   - frame-specific overrides (= MVP is global only, per T138 spec)
;;   - face-spec-set / defface macro (deferred per T138 non-goals;
;;     the 7-API contract above is what Doc 41 §4.1 ship-gates)

;;; Code:

(require 'cl-lib)

;;; Errors

(define-error 'nelisp-face-error "NeLisp face error")
(define-error 'nelisp-face-bad-spec
  "Invalid face spec value" 'nelisp-face-error)
(define-error 'nelisp-face-bad-name
  "Invalid face name" 'nelisp-face-error)
(define-error 'nelisp-face-bad-attribute
  "Invalid face attribute plist" 'nelisp-face-error)

;;; Contract version (Doc 41 §4.1 LOCKED v2)

(defconst nelisp-face-property-contract-version 1
  "Doc 41 §4.1 LOCKED contract version for the face property layer.
Increment only via a new Doc 41 LOCKED revision.")

;;; Tunables

(defconst nelisp-face-inherit-depth-limit 16
  "Max `:inherit' chain depth before truncation (Doc 41 §2.3 LOCKED).
Also used as a cycle guard: when expansion would exceed this depth the
remainder of the chain is dropped silently, matching Emacs precedent.")

(defconst nelisp-face-known-attributes
  '(:family :foundry :width :height :weight :slant
    :foreground :background :underline :overline :strike-through
    :box :inverse-video :stipple :font :inherit :extend
    :distant-foreground)
  "Set of face attribute keys recognised by the validator.
Non-listed keys still pass validation (forward-compat) but a strict
caller can rebind this constant in tests.")

;;; Registry: per-runtime global

(defvar nelisp-face--registry (make-hash-table :test 'eq)
  "Hash: face NAME (symbol) -> attribute plist.
Owned by `nelisp-face-define', read by `nelisp-face-attributes' and
`nelisp-face-resolve' when expanding a face symbol.  Per Doc 41 §4.1
this is per-runtime global.")

;;; Predicates

(defun nelisp-facep (object)
  "Return non-nil if OBJECT is a registered face name (symbol).
Mirrors Emacs `facep' for the symbol arm only — face-spec lists
and attribute plists are *not* faces in their own right.

MCP Parameters: OBJECT — anything."
  (and (symbolp object)
       object
       (not (eq object t))
       (gethash object nelisp-face--registry nil)
       t))

(defun nelisp-face--valid-attr-plist-p (plist)
  "Return non-nil if PLIST is a well-formed attribute plist (even length)."
  (and (listp plist)
       (zerop (mod (length plist) 2))
       (cl-loop for k in plist by #'cddr
                always (keywordp k))))

;;; Registry mutation

(defun nelisp-face-define (name attr-plist)
  "Register face NAME with ATTR-PLIST in the global face registry.
NAME must be a non-nil symbol; ATTR-PLIST must be an even-length list
whose keys are keywords.  Re-defining an existing face replaces its
attribute plist outright (= no merge).  Returns NAME.

MCP Parameters:
  NAME       — face name symbol (non-nil, not t).
  ATTR-PLIST — keyword/value plist of face attributes."
  (unless (and (symbolp name) name (not (eq name t)))
    (signal 'nelisp-face-bad-name (list name)))
  (unless (nelisp-face--valid-attr-plist-p attr-plist)
    (signal 'nelisp-face-bad-attribute (list attr-plist)))
  (puthash name (copy-sequence attr-plist) nelisp-face--registry)
  name)

(defun nelisp-face-attributes (name)
  "Return the attribute plist registered for face NAME, or nil.
The returned list is a *fresh* copy — callers may mutate it without
corrupting the registry.

MCP Parameters:
  NAME — face name symbol."
  (let ((stored (and (symbolp name)
                     (gethash name nelisp-face--registry nil))))
    (and stored (copy-sequence stored))))

(defun nelisp-face-list ()
  "Return the list of registered face names (symbols), in insertion order
of their last definition.  The list is a fresh copy; callers may sort
or filter freely.

MCP Parameters: (none)"
  (let (acc)
    (maphash (lambda (k _v) (push k acc)) nelisp-face--registry)
    (nreverse acc)))

(defun nelisp-face--registry-clear ()
  "Internal: drop every face from the registry.
Used by tests; not part of the public Doc 41 §4.1 contract."
  (clrhash nelisp-face--registry)
  nil)

;;; Spec normalization (Doc 41 §2.2 LOCKED, 3 + nil forms)

(defun nelisp-face--spec-form (spec)
  "Classify SPEC as one of (`:nil', `:symbol', `:cascade', `:plist', `:bad').
Used internally by the resolver and the put-text-property normalizer."
  (cond
   ((null spec) :nil)
   ((and (symbolp spec) (not (eq spec t))) :symbol)
   ((and (listp spec)
         (keywordp (car-safe spec))
         (nelisp-face--valid-attr-plist-p spec))
    :plist)
   ((and (listp spec)
         (cl-every (lambda (x) (or (null x) (symbolp x) (consp x)))
                   spec))
    :cascade)
   (t :bad)))

(defun nelisp-face-normalize-spec (spec)
  "Return SPEC after light normalization, or signal `nelisp-face-bad-spec'.

Normalization rules (Doc 41 §2.2 LOCKED):
- nil           -> nil
- symbol        -> symbol (kept as-is for late lookup)
- (:k v ...)    -> a *fresh copy* of the plist (so callers cannot
                   mutate the stored value out from under us)
- (s1 s2 ...)   -> a fresh list with leading nil stripped from each
                   element so the resolver does not have to skip
- otherwise     -> signal `nelisp-face-bad-spec'

Used by `put-text-property' (face) callers to validate before storage,
*and* by `nelisp-face-resolve' as the entry sanitizer.

MCP Parameters: SPEC — a face spec value (3 forms + nil)."
  (pcase (nelisp-face--spec-form spec)
    (:nil      nil)
    (:symbol   spec)
    (:plist    (copy-sequence spec))
    (:cascade  (copy-sequence spec))
    (_         (signal 'nelisp-face-bad-spec (list spec)))))

;;; Attribute merge (left-to-right, Doc 41 §2.3 LOCKED)

(defun nelisp-face--plist-put-keep-left (acc key value)
  "Insert KEY=VALUE into ACC (a plist) only if KEY is not already present.
Returns the (possibly extended) plist."
  (if (plist-member acc key)
      acc
    (nconc acc (list key value))))

(defun nelisp-face--merge-plists (left right)
  "Return a new plist = LEFT overlaid on RIGHT (LEFT wins on conflict).
Both inputs must be valid plists; output is a fresh list."
  (let ((result (copy-sequence left)))
    (cl-loop for (k v) on right by #'cddr
             do (setq result
                      (nelisp-face--plist-put-keep-left result k v)))
    result))

(defun nelisp-face--expand-symbol (sym depth seen)
  "Expand registered face SYM into its attribute plist with `:inherit' chain.
DEPTH is the current recursion depth (bounded by
`nelisp-face-inherit-depth-limit').  SEEN is a list of already-visited
face symbols on this path (cycle guard).

Returns a fresh attribute plist with `:inherit' stripped (= chain fully
flattened).  Unknown faces and over-depth lookups return nil (= no
contribution), matching Emacs precedent."
  (cond
   ((>= depth nelisp-face-inherit-depth-limit) nil)
   ((memq sym seen) nil)
   (t
    (let ((own (nelisp-face-attributes sym)))
      (if (null own)
          nil
        (let ((inherit (plist-get own :inherit))
              (base (cl-loop for (k v) on own by #'cddr
                             unless (eq k :inherit)
                             nconc (list k v))))
          (if (null inherit)
              base
            ;; Recurse into the inherit chain (symbol or list of symbols)
            ;; and merge BASE on top (= base wins, parents are fallback).
            (let ((parents
                   (nelisp-face--resolve-1 inherit (1+ depth)
                                           (cons sym seen))))
              (nelisp-face--merge-plists base parents)))))))))

(defun nelisp-face--resolve-1 (spec depth seen)
  "Internal recursive resolver — see `nelisp-face-resolve'.
DEPTH/SEEN form the cycle guard."
  (pcase (nelisp-face--spec-form spec)
    (:nil      nil)
    (:symbol   (nelisp-face--expand-symbol spec depth seen))
    (:plist
     ;; Raw plist may itself carry :inherit, expand it.
     (let ((inherit (plist-get spec :inherit))
           (base (cl-loop for (k v) on spec by #'cddr
                          unless (eq k :inherit)
                          nconc (list k v))))
       (if (null inherit)
           base
         (nelisp-face--merge-plists
          base
          (nelisp-face--resolve-1 inherit (1+ depth) seen)))))
    (:cascade
     ;; Left-to-right: head wins, tail is fallback chain.
     (let (acc)
       (dolist (entry spec)
         (let ((piece (nelisp-face--resolve-1 entry depth seen)))
           (when piece
             (setq acc (nelisp-face--merge-plists acc piece)))))
       acc))
    (_ (signal 'nelisp-face-bad-spec (list spec)))))

(defun nelisp-face-resolve (spec)
  "Resolve face SPEC into a flat attribute plist (`:inherit' expanded).

Inputs (Doc 41 §2.2 LOCKED, 3 + nil forms):
  nil                       — empty plist
  FACE-SYMBOL               — registry lookup, then `:inherit' chain
  (FACE-SYMBOL ...)         — cascade list, left-to-right merge
  (:foreground STR ...)     — raw plist, used directly

Output: a fresh attribute plist.  `:inherit' is fully expanded out;
`unspecified' values are *kept* so a downstream backend can fall back.
On a malformed SPEC, signals `nelisp-face-bad-spec'.

MCP Parameters: SPEC — a face spec value (3 forms + nil)."
  (nelisp-face--resolve-1 spec 0 nil))

(defun nelisp-face-attribute-merge (face-list)
  "Resolve FACE-LIST and return the merged attribute plist.
FACE-LIST may be any cascade-form face spec — a list of face names,
plists, or a mix (per §2.2).  This is just `nelisp-face-resolve' but
*requires* a list-form input so callers can be explicit when they have
a cascade.

Examples:
  (nelisp-face-attribute-merge \\='(bold (:foreground \"red\")))
  => (:weight bold :foreground \"red\")

MCP Parameters: FACE-LIST — list of face specs."
  (unless (listp face-list)
    (signal 'nelisp-face-bad-spec (list face-list)))
  (nelisp-face-resolve face-list))

;;; Attribute query helpers

(defun nelisp-face-attribute (face attribute)
  "Return the resolved value of ATTRIBUTE for FACE (or nil if unset).
FACE is any spec form accepted by `nelisp-face-resolve'.  ATTRIBUTE
must be a keyword.  Note: returns the literal value as merged (which
may be `unspecified').

MCP Parameters:
  FACE      — face spec value.
  ATTRIBUTE — keyword."
  (unless (keywordp attribute)
    (signal 'nelisp-face-bad-attribute (list attribute)))
  (plist-get (nelisp-face-resolve face) attribute))

(defun nelisp-face-foreground (face)
  "Return the resolved `:foreground' of FACE, or nil if unset.

MCP Parameters: FACE — face spec value."
  (nelisp-face-attribute face :foreground))

(defun nelisp-face-background (face)
  "Return the resolved `:background' of FACE, or nil if unset.

MCP Parameters: FACE — face spec value."
  (nelisp-face-attribute face :background))

;;; Equality (Doc 41 §4.1: equal-p over normalized attributes)

(defun nelisp-face--canonicalize (plist)
  "Return PLIST sorted by key for stable comparison."
  (let ((pairs (cl-loop for (k v) on plist by #'cddr collect (cons k v))))
    (cl-loop for (k . v) in (cl-sort pairs #'string<
                                     :key (lambda (p) (symbol-name (car p))))
             append (list k v))))

(defun nelisp-face-equal-p (face1 face2)
  "Return non-nil if FACE1 and FACE2 resolve to equal attribute plists.
Order of keys in the source spec is irrelevant; comparison is over the
canonical (sorted-by-key) resolved form.

Defined to satisfy reflexivity / symmetry / transitivity over the set
of valid Doc 41 §2.2 spec values (per §3.1.4 ERT gate).

MCP Parameters:
  FACE1, FACE2 — face spec values."
  (equal (nelisp-face--canonicalize (nelisp-face-resolve face1))
         (nelisp-face--canonicalize (nelisp-face-resolve face2))))

(provide 'nelisp-emacs-compat-face)

;;; nelisp-emacs-compat-face.el ends here
