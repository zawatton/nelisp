;;; emacs-textmodes-stub.el --- fill-region / count-matches polyfills  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Phase 4 'C' (lisp/textmodes uptake, 2026-05-06): minimal polyfills
;; for `fill-region' and `count-matches' so MELPA packages that route
;; word-wrap / regex counting through buffer ops (s.el's `s-word-wrap'
;; and `s-count-matches' being canonical examples) work end-to-end on
;; the nelisp driver without dragging in upstream `lisp/textmodes/
;; fill.el' (~1800 LOC paragraph + justify logic) or `lisp/replace.el'
;; (~3000 LOC interactive query-replace stack).
;;
;; The shipped helpers (`emacs-textmodes--word-wrap',
;; `emacs-textmodes-fill-region', `emacs-textmodes-count-matches')
;; are named so they can be unit-tested directly even under host
;; Emacs (= where `fill-region' / `count-matches' are already bound
;; and our `unless (fboundp ...)' aliases skip).  Under the nelisp
;; driver the aliases bind these names to the unprefixed contracts.
;;
;; NOT a full `fill.el' substitute: paragraph detection, justify,
;; adaptive prefix, refill-mode integration are all out of scope.
;; If a downstream package needs those, it should depend on the real
;; vendor file (= future Phase 4 'C+' = vendor lisp/textmodes/fill.el).

;;; Code:

(require 'nelisp-emacs-compat)

;; -- fill-column default -----------------------------------------------
;; Some MELPA packages let-bind `fill-column' before calling
;; `fill-region' (= s.el's `s-word-wrap' is a textbook example).  Make
;; sure the variable exists at top level so the let-binding is legal.

(unless (boundp 'fill-column) (defvar fill-column 70))

;; -- paragraph defaults -----------------------------------------------
;; Upstream `textmodes/paragraphs.el' defines these standard variables.
;; Vendor `outline-mode' and `org-mode' read them during mode activation
;; before loading the full paragraphs command surface.

(unless (boundp 'paragraph-start)
  (defvar paragraph-start "\f\\|[ \t]*$"
    "Regexp for beginning of a line that starts or separates paragraphs."))

(unless (boundp 'paragraph-separate)
  (defvar paragraph-separate "[ \t\f]*$"
    "Regexp for beginning of a line that separates paragraphs."))

(when (and (fboundp 'make-variable-buffer-local)
           (or (not (boundp 'emacs-version))
               (fboundp 'nelisp--write-stdout-bytes)))
  (make-variable-buffer-local 'paragraph-start)
  (make-variable-buffer-local 'paragraph-separate))

;; -- word-wrap (pure-string helper) ------------------------------------

(defun emacs-textmodes--word-wrap (s width)
  "Word-wrap string S to WIDTH columns, returning the wrapped string.
Greedy: each output line packs as many whitespace-separated tokens
as fit, breaking at the next token boundary.  Tokens longer than
WIDTH go on a line of their own (= no mid-token break).  Existing
newlines are treated as token separators (= paragraph fold)."
  (let* ((tokens (split-string s "[ \t\n\r\f\v]+" t))
         (out nil)
         (col 0)
         (line nil))
    (dolist (w tokens)
      (let ((wlen (length w)))
        (cond
         ((null line)
          (setq line (list w))
          (setq col wlen))
         ((<= (+ col 1 wlen) width)
          (setq line (cons w (cons " " line)))
          (setq col (+ col 1 wlen)))
         (t
          (setq out (cons (apply #'concat (nreverse line)) out))
          (setq line (list w))
          (setq col wlen)))))
    (when line
      (setq out (cons (apply #'concat (nreverse line)) out)))
    (mapconcat #'identity (nreverse out) "\n")))

;; -- fill-region (greedy word-wrap of a buffer region) -----------------

(defun emacs-textmodes-fill-region (start end &optional _justify _nosqueeze _eop)
  "Phase 4 'C' polyfill: greedy word-wrap of the current-buffer region
between START and END to `fill-column'.  JUSTIFY / NOSQUEEZE / TO-EOP
are accepted for API parity but ignored (= no justify / paragraph
detect)."
  (let* ((s (buffer-substring start end))
         (wrapped (emacs-textmodes--word-wrap s (or fill-column 70))))
    (delete-region start end)
    (goto-char start)
    (insert wrapped)
    nil))

(unless (fboundp 'fill-region)
  (defalias 'fill-region #'emacs-textmodes-fill-region))

;; -- count-matches (non-overlapping regex count between BEG/END) -------

(defun emacs-textmodes-count-matches (regexp &optional rstart rend &rest _)
  "Phase 4 'C' polyfill: count non-overlapping matches of REGEXP in
the current buffer between RSTART (default `point-min') and REND
(default `point-max'), returning the integer count.

Mirrors the non-interactive contract of `lisp/replace.el's
`count-matches' — no echo-area report, no INTERACTIVE branch."
  (save-excursion
    (goto-char (or rstart (point-min)))
    (let ((bound (or rend (point-max)))
          (count 0))
      (while (re-search-forward regexp bound t)
        (setq count (1+ count))
        ;; Advance past a zero-length match to avoid infinite loop.
        (when (= (match-beginning 0) (match-end 0))
          (if (< (point) bound)
              (forward-char 1)
            ;; Empty match at end-of-region — nothing more to scan.
            (goto-char bound))))
      count)))

(unless (fboundp 'count-matches)
  (defalias 'count-matches #'emacs-textmodes-count-matches))

(provide 'emacs-textmodes-stub)

;;; emacs-textmodes-stub.el ends here
