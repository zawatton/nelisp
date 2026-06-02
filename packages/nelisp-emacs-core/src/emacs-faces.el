;;; emacs-faces.el --- Face attribute API (Track F)  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Doc 51 Track F (2026-05-03) — Layer 2.
;;
;; Substrate for the user-facing face API.  Shares the face registry
;; variable with `emacs-redisplay', but does not require redisplay at
;; load time: batch/bootstrap paths need face definitions without paying
;; the full renderer load cost.
;;
;; The data model:
;; - Each face is a symbol whose attribute plist lives in
;;   `emacs-redisplay--face-registry'.
;; - Attribute keys are Emacs-standard keywords:
;;     :foreground :background :weight :slant :underline
;;     :overline :strike-through :inverse-video :inherit :family
;;     :height :box.
;; - Unset attributes return the symbol `unspecified' (= matches
;;   `face-attribute' contract).
;;
;; The bridge layer (`emacs-faces-builtins') exposes the
;; conventional unprefixed names — `face-attribute',
;; `set-face-attribute', `face-foreground', `defface' (macro),
;; etc. — gated on `unless (fboundp ...)' so loading inside a
;; host Emacs is a no-op.
;;
;; Out of scope (= deferred to later γ phases): face inheritance
;; resolution at attribute-read time, frame parameter integration,
;; X-resource fallback, `face-spec-set-2'-style display-class
;; precedence (= only the catch-all entry is honoured).

;;; Code:

(require 'cl-lib)

(defvar emacs-redisplay--face-registry (make-hash-table :test 'eq)
  "Shared face registry used by `emacs-faces' and `emacs-redisplay'.")

(defvar emacs-redisplay--face-cache (make-hash-table :test 'equal)
  "Shared realized-face cache used when `emacs-redisplay' is loaded.")

(unless (fboundp 'emacs-redisplay-face-cache-clear)
  (defun emacs-redisplay-face-cache-clear ()
    "Clear the shared face realization cache."
    (clrhash emacs-redisplay--face-cache)))

(define-error 'emacs-faces-error "Face error")

;;;; --- predicates / lifecycle -----------------------------------------

(defconst emacs-faces--unset (make-symbol "emacs-faces--unset")
  "Sentinel returned by `gethash' when a face is not registered.")

(defun emacs-faces-facep (x)
  "Return X (= face symbol) if X names a registered face, else nil.
A face with no attributes (= empty plist) still counts as registered;
we distinguish via a `gethash' sentinel rather than the value, since
the empty plist is `nil'."
  (and (symbolp x)
       (not (eq emacs-faces--unset
                (gethash x emacs-redisplay--face-registry
                         emacs-faces--unset)))
       x))

(defun emacs-faces-make-face (name)
  "Define a new face NAME with no attributes (= empty plist).
Returns NAME.  Idempotent: re-registering an existing face is a
no-op; existing attributes are preserved."
  (unless (symbolp name)
    (signal 'wrong-type-argument (list 'symbolp name)))
  (when (eq emacs-faces--unset
            (gethash name emacs-redisplay--face-registry
                     emacs-faces--unset))
    (puthash name nil emacs-redisplay--face-registry))
  name)

;;;; --- attribute accessors --------------------------------------------

(defun emacs-faces-attribute (face attribute &optional _frame _inherit)
  "Return the value of FACE's ATTRIBUTE, or `unspecified' if not set.
FRAME and INHERIT are accepted for API parity but ignored in the
MVP.  Inheritance resolution is deferred."
  (let ((plist (and (boundp 'emacs-redisplay--face-registry)
                    (gethash face emacs-redisplay--face-registry))))
    (if (and plist (plist-member plist attribute))
        (plist-get plist attribute)
      'unspecified)))

(defun emacs-faces-set-attribute (face _frame &rest props)
  "Update FACE's plist with PROPS (= alternating keyword/value).
Returns FACE.  Invalidates the realization cache so subsequent
lookups via `emacs-redisplay-realize-face' see the new state."
  (unless (symbolp face)
    (signal 'wrong-type-argument (list 'symbolp face)))
  (unless (zerop (mod (length props) 2))
    (signal 'emacs-faces-error
            (list 'odd-length-attribute-list props)))
  (emacs-faces-make-face face)
  (let ((plist (gethash face emacs-redisplay--face-registry)))
    (while props
      (let ((k (car props))
            (v (cadr props)))
        (setq plist (plist-put plist k v)))
      (setq props (cddr props)))
    (puthash face plist emacs-redisplay--face-registry)
    (emacs-redisplay-face-cache-clear)
    face))

;;;; --- convenience accessors -----------------------------------------

(defun emacs-faces-foreground (face &optional _frame _inherit)
  "Return FACE's :foreground, or nil when unspecified."
  (let ((v (emacs-faces-attribute face :foreground)))
    (and (not (eq v 'unspecified)) v)))

(defun emacs-faces-background (face &optional _frame _inherit)
  "Return FACE's :background, or nil when unspecified."
  (let ((v (emacs-faces-attribute face :background)))
    (and (not (eq v 'unspecified)) v)))

(defun emacs-faces-set-foreground (face color &optional frame)
  "Set FACE's :foreground to COLOR."
  (emacs-faces-set-attribute face frame :foreground color))

(defun emacs-faces-set-background (face color &optional frame)
  "Set FACE's :background to COLOR."
  (emacs-faces-set-attribute face frame :background color))

;;;; --- enumeration ----------------------------------------------------

(defun emacs-faces-list ()
  "Return all registered face names as a list (= unsorted)."
  (let ((out nil))
    (maphash (lambda (k _v) (push k out))
             emacs-redisplay--face-registry)
    out))

;;;; --- defface macro --------------------------------------------------

(defun emacs-faces--entry-attrs (entry)
  "Return the attribute plist stored in a face spec ENTRY.
Emacs accepts both `(t :foreground \"red\")' and
`(t (:foreground \"red\"))' shapes in `defface' specs.  The
substrate stores the normalized flat plist."
  (let ((attrs (cdr entry)))
    (if (and (= (length attrs) 1)
             (listp (car attrs)))
        (car attrs)
      attrs)))

(defun emacs-faces--default-attrs-from-spec (spec)
  "Extract a flat attribute plist from a SPEC value.

SPEC is the value handed to `defface' — a list of entries
`(DISPLAY . ATTRS)' where ATTRS is a flat plist.  We honour:

  default     →  always-applied attributes
  t           →  catch-all
  (((class color))) etc. → conditional (= ignored for MVP, only
                            checked if no t / default entry)

Returns a flat plist or nil."
  (let ((entries (cond
                  ((and (consp spec) (eq (car spec) 'quote))
                   (cadr spec))
                  ((listp spec) spec)
                  (t nil)))
        (default-entry nil)
        (t-entry nil)
        (first-entry nil))
    (dolist (e entries)
      (when (consp e)
        (cond
         ((eq (car e) 'default) (setq default-entry e))
         ((eq (car e) t)        (setq t-entry e))
         ((null first-entry)    (setq first-entry e)))))
    (let ((entry (or default-entry t-entry first-entry)))
      (and entry (emacs-faces--entry-attrs entry)))))

(defmacro emacs-faces-defface (name spec _doc &rest _opts)
  "Register face NAME with the SPEC's catch-all attributes.
DOC and OPTS (= :group / :version / :package-version) are
accepted for API parity but ignored in the MVP."
  (let ((attrs (emacs-faces--default-attrs-from-spec spec)))
    `(progn
       (emacs-faces-make-face ',name)
       ,@(when attrs
           `((apply #'emacs-faces-set-attribute
                    ',name nil ',attrs)))
       ',name)))

;;;; --- reset (test helper) -------------------------------------------

(defun emacs-faces-reset ()
  "Drop every face from the registry + invalidate realize cache.
Test helper — production code shouldn't call this."
  (clrhash emacs-redisplay--face-registry)
  (emacs-redisplay-face-cache-clear))

(provide 'emacs-faces)

;;; emacs-faces.el ends here
