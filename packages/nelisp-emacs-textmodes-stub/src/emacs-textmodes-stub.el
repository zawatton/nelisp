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

;; -- standard text/editing variable defaults --------------------------
;; Some MELPA packages let-bind `fill-column' before calling
;; `fill-region' (= s.el's `s-word-wrap' is a textbook example).  Vendor
;; `outline-mode' and `org-mode' additionally write a broad standard set
;; via `setq-local' while activating.  The standalone `setq-local' bridge
;; currently expects those variables to be bound already, so keep the
;; low-cost standard defaults available from this textmodes shim.

(unless (boundp 'tab-width)
  (defvar tab-width 8
    "Distance between tab stops in columns."))

(unless (boundp 'fill-column)
  (defvar fill-column 70
    "Column beyond which automatic line-wrapping should happen."))

(unless (boundp 'indent-tabs-mode)
  (defvar indent-tabs-mode t
    "Non-nil means indentation can insert tabs."))

(unless (boundp 'left-margin)
  (defvar left-margin 0
    "Column for the left margin in the current buffer."))

(unless (boundp 'fill-prefix)
  (defvar fill-prefix nil
    "String inserted at the front of new lines during filling, or nil."))

(unless (boundp 'truncate-lines)
  (defvar truncate-lines nil
    "Non-nil means display lines should not wrap."))

(unless (boundp 'word-wrap)
  (defvar word-wrap nil
    "Non-nil means display line wrapping should happen at word boundaries."))

(unless (boundp 'case-fold-search)
  (defvar case-fold-search t
    "Non-nil means searches and matches should ignore case by default."))

(when (and (boundp 'case-fold-search)
           (null case-fold-search)
           (or (not (boundp 'emacs-version))
               (fboundp 'nelisp--write-stdout-bytes)))
  (setq case-fold-search t))

(unless (boundp 'selective-display)
  (defvar selective-display nil
    "Non-nil means hide lines according to selective display rules."))

(unless (boundp 'cursor-type)
  (defvar cursor-type t
    "Default cursor shape for the selected buffer."))

(unless (boundp 'font-lock-unfontify-region-function)
  (defvar font-lock-unfontify-region-function nil
    "Function used by Font Lock to unfontify a region."))

(unless (boundp 'indent-line-function)
  (defvar indent-line-function nil
    "Function called to indent the current line."))

(unless (boundp 'indent-region-function)
  (defvar indent-region-function nil
    "Function called to indent a region, or nil for the default."))

(unless (boundp 'beginning-of-defun-function)
  (defvar beginning-of-defun-function nil
    "Function used by `beginning-of-defun' when non-nil."))

(unless (boundp 'end-of-defun-function)
  (defvar end-of-defun-function nil
    "Function used by `end-of-defun' when non-nil."))

(unless (boundp 'next-error-function)
  (defvar next-error-function nil
    "Buffer-local function used to jump to the next match or error."))

(unless (boundp 'add-log-current-defun-function)
  (defvar add-log-current-defun-function nil
    "Function used to find the current defun for change-log entries."))

(unless (boundp 'align-mode-rules-list)
  (defvar align-mode-rules-list nil
    "Mode-specific alignment rules."))

(unless (boundp 'calc-embedded-open-mode)
  (defvar calc-embedded-open-mode nil
    "Prefix used by Calc embedded mode when opening formulas."))

(unless (boundp 'buffer-face-mode-face)
  (defvar buffer-face-mode-face nil
    "Face used by `buffer-face-mode'."))

(unless (boundp 'pcomplete-command-completion-function)
  (defvar pcomplete-command-completion-function nil
    "Function used by pcomplete to complete command names."))

(unless (boundp 'pcomplete-command-name-function)
  (defvar pcomplete-command-name-function nil
    "Function used by pcomplete to find the command name at point."))

(unless (boundp 'pcomplete-default-completion-function)
  (defvar pcomplete-default-completion-function nil
    "Default pcomplete completion function."))

(unless (boundp 'pcomplete-parse-arguments-function)
  (defvar pcomplete-parse-arguments-function nil
    "Function used by pcomplete to parse arguments."))

(unless (boundp 'pcomplete-termination-string)
  (defvar pcomplete-termination-string nil
    "String inserted by pcomplete after a completion."))

(unless (boundp 'minor-mode-overriding-map-alist)
  (defvar minor-mode-overriding-map-alist nil
    "Alist of buffer-local minor-mode keymap overrides."))

(unless (boundp 'auto-fill-function)
  (defvar auto-fill-function nil
    "Function run by self-insert when Auto Fill mode is active."))

(unless (boundp 'normal-auto-fill-function)
  (defvar normal-auto-fill-function 'do-auto-fill
    "Function used as `auto-fill-function' when Auto Fill mode is active."))

(unless (boundp 'filter-buffer-substring-functions)
  (defvar filter-buffer-substring-functions nil
    "Obsolete wrapper hook around `buffer-substring--filter'."))

(unless (boundp 'filter-buffer-substring-function)
  (defvar filter-buffer-substring-function #'buffer-substring--filter
    "Function used by `filter-buffer-substring' to filter copied text."))

(unless (fboundp 'delete-and-extract-region)
  (defun delete-and-extract-region (beg end)
    "Delete text between BEG and END and return it."
    (let ((text (buffer-substring beg end)))
      (delete-region beg end)
      text)))

(unless (fboundp 'buffer-substring--filter)
  (defun buffer-substring--filter (beg end &optional delete)
    "Default function for `filter-buffer-substring-function'."
    (if delete
        (delete-and-extract-region beg end)
      (buffer-substring beg end))))

(unless (fboundp 'filter-buffer-substring)
  (defun filter-buffer-substring (beg end &optional delete)
    "Return filtered buffer text between BEG and END.
When DELETE is non-nil, delete the source text after copying."
    (funcall filter-buffer-substring-function beg end delete)))

(unless (boundp 'fill-nobreak-predicate)
  (defvar fill-nobreak-predicate nil
    "Hook of predicates preventing line breaks during filling."))

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

(unless (boundp 'sentence-end-double-space)
  (defvar sentence-end-double-space t
    "Non-nil means a single space does not end a sentence."))

(unless (boundp 'sentence-end-without-period)
  (defvar sentence-end-without-period nil
    "Non-nil means a sentence can end without a period."))

(unless (boundp 'sentence-end-without-space)
  (defvar sentence-end-without-space "。．？！"
    "String of characters that end sentences without following spaces."))

(unless (boundp 'sentence-end)
  (defvar sentence-end nil
    "Regexp describing the end of a sentence, or nil for the default."))

(unless (boundp 'sentence-end-base)
  (defvar sentence-end-base "[.?!…‽][]\"'”’)}»›]*"
    "Regexp matching the basic end of a sentence."))

(unless (boundp 'page-delimiter)
  (defvar page-delimiter "^\014"
    "Regexp describing line beginnings that separate pages."))

(unless (boundp 'paragraph-ignore-fill-prefix)
  (defvar paragraph-ignore-fill-prefix nil
    "Non-nil means paragraph commands ignore `fill-prefix'."))

(unless (boundp 'colon-double-space)
  (defvar colon-double-space nil
    "Non-nil means filling inserts two spaces after a colon."))

(unless (boundp 'adaptive-fill-mode)
  (defvar adaptive-fill-mode t
    "Non-nil means choose a paragraph fill prefix from its text."))

(unless (boundp 'adaptive-fill-regexp)
  (defvar adaptive-fill-regexp "[-–!|#%;>*·•‣⁃◦ \t]*"
    "Regexp matching indentation used as adaptive fill prefix."))

(unless (boundp 'adaptive-fill-first-line-regexp)
  (defvar adaptive-fill-first-line-regexp "\\`[ \t]*\\'"
    "Regexp deciding whether one-line paragraphs keep adaptive prefix."))

(unless (boundp 'adaptive-fill-function)
  (defvar adaptive-fill-function #'ignore
    "Function called to choose an adaptive fill prefix."))

(unless (boundp 'fill-paragraph-function)
  (defvar fill-paragraph-function nil
    "Mode-specific function used by `fill-paragraph'."))

(unless (boundp 'fill-paragraph-handle-comment)
  (defvar fill-paragraph-handle-comment t
    "Non-nil means paragraph filling pays attention to comments."))

(unless (boundp 'fill-forward-paragraph-function)
  (defvar fill-forward-paragraph-function 'forward-paragraph
    "Function used by filling code to move by paragraphs."))

(unless (boundp 'auto-fill-inhibit-regexp)
  (defvar auto-fill-inhibit-regexp nil
    "Regexp matching lines that should not be auto-filled."))

(unless (boundp 'comment-use-syntax)
  (defvar comment-use-syntax 'undecided
    "Non-nil means comment commands can use syntax tables."))

(unless (boundp 'comment-fill-column)
  (defvar comment-fill-column nil
    "Column used for comment indentation, or nil for `fill-column'."))

(unless (boundp 'comment-column)
  (defvar comment-column 32
    "Column to indent right-margin comments to."))

(unless (boundp 'comment-start)
  (defvar comment-start nil
    "String used to start a comment, or nil if comments are unavailable."))

(unless (boundp 'comment-start-skip)
  (defvar comment-start-skip nil
    "Regexp matching the start of a comment and its leading padding."))

(unless (boundp 'comment-end-skip)
  (defvar comment-end-skip nil
    "Regexp matching the end of a comment and its trailing padding."))

(unless (boundp 'comment-end)
  (defvar comment-end ""
    "String used to end a comment."))

(unless (boundp 'comment-indent-function)
  (defvar comment-indent-function 'comment-indent-default
    "Function computing comment indentation."))

(unless (boundp 'comment-insert-comment-function)
  (defvar comment-insert-comment-function nil
    "Function used to insert a new comment."))

(unless (boundp 'comment-region-function)
  (defvar comment-region-function 'comment-region-default
    "Function used to comment a region."))

(unless (boundp 'uncomment-region-function)
  (defvar uncomment-region-function 'uncomment-region-default
    "Function used to uncomment a region."))

(unless (boundp 'comment-continue)
  (defvar comment-continue nil
    "Continuation string for multi-line comments."))

(unless (boundp 'comment-add)
  (defvar comment-add 0
    "How many extra comment characters `comment-region' should insert."))

(unless (boundp 'comment-style)
  (defvar comment-style 'indent
    "Commenting style used by `comment-region'."))

(unless (boundp 'comment-padding)
  (defvar comment-padding " "
    "Padding inserted between comment delimiters and comment text."))

(unless (boundp 'comment-inline-offset)
  (defvar comment-inline-offset 1
    "Minimum spacing before inline comments."))

(unless (boundp 'comment-multi-line)
  (defvar comment-multi-line nil
    "Non-nil means comment line breaks continue comments."))

(unless (boundp 'comment-empty-lines)
  (defvar comment-empty-lines nil
    "Non-nil means comment commands also affect empty lines."))

(unless (boundp 'comment-line-break-function)
  (defvar comment-line-break-function 'comment-indent-new-line
    "Function used to break lines inside comments."))

(when (and (fboundp 'make-variable-buffer-local)
           (or (not (boundp 'emacs-version))
               (fboundp 'nelisp--write-stdout-bytes)))
  (dolist (sym '(tab-width fill-column indent-tabs-mode left-margin
                 fill-prefix truncate-lines word-wrap case-fold-search
                 selective-display auto-fill-function paragraph-start
                 paragraph-separate paragraph-ignore-fill-prefix
                 adaptive-fill-function fill-paragraph-function
                 comment-column comment-start comment-start-skip
                 completion-at-point-functions
                 minor-mode-overriding-map-alist
                 add-log-current-defun-function next-error-function
                 indent-line-function indent-region-function))
    (make-variable-buffer-local sym)))

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
