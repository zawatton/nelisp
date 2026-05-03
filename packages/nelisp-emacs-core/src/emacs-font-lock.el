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
;; Out of scope (= deferred to Track K' / γ+):
;;   - (eval . FORM) keyword form
;;   - anchored multi-line highlights
;;   - syntactic-keywords / syntax-table fontification
;;   - jit-lock incremental fontification (= we do whole-region only)
;;   - font-lock-extend-region-functions hook
;;   - font-lock-defaults beyond the KEYWORDS slot
;;
;; Standard faces (`font-lock-keyword-face' etc) are defined in this
;; module so callers / modes can reference them at load-time without
;; pulling the host Emacs's `font-lock.el'.

;;; Code:

(require 'cl-lib)
(require 'emacs-buffer)
(require 'emacs-faces)

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
4-list (SUBEXP FACE OVERRIDE LAXMATCH)."
  (cond
   ((stringp kw)
    (list kw (list 0 'font-lock-keyword-face nil nil)))
   ((and (consp kw) (stringp (car kw)) (symbolp (cdr kw)))
    (list (car kw) (list 0 (cdr kw) nil nil)))
   ((and (consp kw) (stringp (car kw)) (numberp (cdr kw)))
    (list (car kw) (list (cdr kw) 'font-lock-keyword-face nil nil)))
   ((and (consp kw) (stringp (car kw)) (listp (cdr kw)))
    (cons (car kw)
          (mapcar (lambda (h)
                    (cond
                     ((symbolp h) (list 0 h nil nil))
                     ((and (consp h) (numberp (car h)))
                      (let ((subexp (car h))
                            (face (cadr h))
                            (override (and (cddr h) (nth 2 h)))
                            (laxmatch (and (nthcdr 3 h) (nth 3 h))))
                        (list subexp face override laxmatch)))
                     (t (list 0 'font-lock-keyword-face nil nil))))
                  (cdr kw))))
   (t
    (list ".\\`" (list 0 'font-lock-keyword-face nil nil)))))

(defun emacs-font-lock--compile-keywords (keywords)
  (mapcar #'emacs-font-lock--compile-keyword keywords))

;;;; --- defaults / add-keywords ---------------------------------------

(defun emacs-font-lock-set-defaults (&optional buf)
  "Initialise font-lock state from `font-lock-defaults' for BUF.
Defaults to the current buffer.  If `font-lock-defaults' is nil
or unbound, leaves :keywords as nil."
  (let* ((b (or buf (emacs-font-lock--current-buffer)))
         (defaults (and (boundp 'font-lock-defaults) font-lock-defaults))
         (kw (cond
              ((null defaults) nil)
              ((listp defaults)
               (let ((spec (car defaults)))
                 (cond
                  ((symbolp spec)
                   (and (boundp spec) (symbol-value spec)))
                  ((listp spec) spec)
                  (t nil))))
              (t nil))))
    (when b
      (emacs-font-lock--state-set b :defaults defaults)
      (emacs-font-lock--state-set b :keywords
                                  (emacs-font-lock--compile-keywords kw)))
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

(defun emacs-font-lock--apply-highlight (highlight buf)
  "Apply one HIGHLIGHT (SUBEXP FACE OVERRIDE LAXMATCH) to current match in BUF."
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
      (cond
       ((eq override 'prepend)
        ;; prepend: only set if not already set
        (let ((cur (emacs-buffer-get-text-property m-beg 'face buf)))
          (unless cur
            (emacs-buffer-put-text-property m-beg m-end 'face face-val buf))))
       ((eq override 'append)
        ;; same as prepend in MVP (no list-merge)
        (let ((cur (emacs-buffer-get-text-property m-beg 'face buf)))
          (unless cur
            (emacs-buffer-put-text-property m-beg m-end 'face face-val buf))))
       (t
        (emacs-buffer-put-text-property m-beg m-end 'face face-val buf))))
     ((not laxmatch)
      ;; subexp didn't match and laxmatch=nil → would error in upstream; we ignore.
      nil))))

(defun emacs-font-lock--fontify-one-keyword (cell start end buf)
  "Run REGEXP from CELL over [START, END) in BUF, applying highlights.

Uses the prefixed `nelisp-ec-re-search-forward' substrate directly
so the search runs against BUF rather than whatever `current-buffer'
the host Emacs would otherwise see."
  (let ((regexp (car cell))
        (highlights (cdr cell))
        (nelisp-ec--current-buffer buf))
    (nelisp-ec-goto-char start)
    (while (and (< (nelisp-ec-point) end)
                (nelisp-ec-re-search-forward regexp end t))
      (let ((mb (nelisp-ec-match-beginning 0))
            (me (nelisp-ec-match-end 0)))
        (dolist (h highlights)
          (emacs-font-lock--apply-highlight h buf))
        ;; Advance past zero-width matches to prevent infinite loop.
        (when (and mb me (= mb me))
          (if (< (nelisp-ec-point) end)
              (nelisp-ec-forward-char 1)
            (nelisp-ec-goto-char end)))))))

(defun emacs-font-lock-default-fontify-region (start end &optional _loudly buf)
  "Fontify [START, END) in BUF using the buffer's compiled keywords."
  (let* ((b (or buf (emacs-font-lock--current-buffer)))
         (kws (and b (emacs-font-lock--state-get b :keywords))))
    (when (and b kws (< start end))
      (let ((saved-point (let ((nelisp-ec--current-buffer b)) (nelisp-ec-point))))
        (unwind-protect
            (dolist (cell kws)
              (emacs-font-lock--fontify-one-keyword cell start end b))
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

(provide 'emacs-font-lock)

;;; emacs-font-lock.el ends here
