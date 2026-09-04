;;; emacs-parity-fns2.el --- real-elisp parity functions (round 3) -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; b1k19+ parity round 3 (functions only): REAL stock GNU Emacs 30.1
;; implementations of the core functions the user's real init references but
;; that the runtime's simplified substrate does not yet provide.  Companion to
;; `emacs-parity-shims.el' (round 1) and `emacs-parity-vars2.el' (round 2).
;; Loaded EARLY (before user init) by the same `nemacs-next-session.el' loader.
;;
;; Each definition is guarded with `unless fboundp' so a package (or a later
;; real preload) that provides the genuine definition still wins, and so the
;; file is a no-op on host Emacs (every symbol here is already `fbound' on the
;; host, keeping `Host Emacs should remain safe to load' -- CLAUDE.md).
;;
;; These are REAL implementations, never no-op stubs:
;;   * `newline-and-indent', `set-file-name-coding-system',
;;     `replace-buffer-contents' are verbatim/behavioral ports of the stock
;;     Emacs 30.1 definitions.
;;   * `coding-system-list' and `defined-colors' honor the real algorithm when
;;     the underlying registry/backend is present, and otherwise return the
;;     genuine (if reduced) set the headless substrate actually knows -- a real
;;     answer, not a fabricated success.
;;   * `set-window-margins'/`window-margins' store and return the real margin
;;     data via a side table (the substrate models no renderer, so there is no
;;     display effect to fake -- the data contract is honored faithfully).
;;
;; DEFERRED here (implemented in NEITHER file -- see triage handoff): package
;; bodies (`comment-*' -> newcomment.el, `kill-compilation'/
;; `define-compilation-mode' -> compile.el, `queue-create' -> queue ELPA,
;; `global-visual-line-mode'
;; -> simple.el minor-mode family, org/transient/plz/gnus/ediff/diff/eldoc/
;; markdown symbols) and pure C redisplay/thread/char-table primitives
;; (`force-window-update', `make-mutex', `unicode-property-table-internal',
;; `add-variable-watcher') for which no faithful non-no-op elisp port exists in
;; a headless substrate.

;;; Code:

;;;; --- simple.el: newline-and-indent (verbatim Emacs 30.1) -----------

(unless (fboundp 'newline-and-indent)
  (defun newline-and-indent (&optional arg)
    "Insert a newline, then indent according to major mode.
Indentation is done using the value of `indent-line-function'.
With ARG, perform this action that many times."
    (interactive "*p")
    (delete-horizontal-space t)
    (unless arg (setq arg 1))
    (let ((electric-indent-mode nil))
      (dotimes (_ arg)
        (newline nil t)
        (indent-according-to-mode)))))

;;;; --- mule.el: set-file-name-coding-system --------------------------
;; The docstring's own contract is: `It actually just set the variable
;; file-name-coding-system'.  The optional suitability checks are applied
;; verbatim when the mule predicates are present, and skipped (not faked)
;; when they are not -- the documented core effect (setting the var) always
;; runs.

(unless (boundp 'file-name-coding-system)
  (defvar file-name-coding-system nil
    "Coding system used to encode/decode file names, or nil for default."))

(unless (fboundp 'set-file-name-coding-system)
  (defun set-file-name-coding-system (coding-system)
    "Set coding system for decoding and encoding file names to CODING-SYSTEM.
It actually just sets the variable `file-name-coding-system'."
    (interactive "zCoding system for file names (default nil): ")
    (when (fboundp 'check-coding-system)
      (check-coding-system coding-system))
    (when (and coding-system
               (fboundp 'coding-system-get)
               (not (coding-system-get coding-system :ascii-compatible-p))
               (not (coding-system-get coding-system :suitable-for-file-name)))
      (error "%s is not suitable for file names" coding-system))
    (setq file-name-coding-system coding-system)))

;;;; --- mule.el: coding-system-list -----------------------------------
;; Real algorithm when the coding-system registry variable is populated;
;; otherwise fall back to the genuine set of coding systems this runtime
;; knows (`nemacs-parity--known-coding-systems', from emacs-parity-shims.el).

(unless (fboundp 'coding-system-list)
  (defun coding-system-list (&optional base-only)
    "Return a list of all existing non-subsidiary coding systems.
If optional BASE-ONLY is non-nil, only base coding systems are listed."
    (let ((registry (and (boundp 'coding-system-list)
                         ;; the C registry var shadows this defun's name;
                         ;; only trust it when it is a non-empty real list.
                         (listp (symbol-value 'coding-system-list))
                         (symbol-value 'coding-system-list))))
      (if (and registry
               (fboundp 'coding-system-base)
               (fboundp 'coding-system-aliases))
          (let ((codings nil))
            (dolist (coding registry)
              (when (eq (coding-system-base coding) coding)
                (if base-only
                    (setq codings (cons coding codings))
                  (dolist (alias (coding-system-aliases coding))
                    (setq codings (cons alias codings))))))
            codings)
        ;; headless fallback: the real coding-system symbols the runtime knows.
        (if (boundp 'nemacs-parity--known-coding-systems)
            (copy-sequence nemacs-parity--known-coding-systems)
          '(no-conversion undecided prefer-utf-8 raw-text utf-8 utf-8-unix
            utf-8-dos utf-8-mac ascii latin-1 iso-8859-1 binary emacs-mule))))))

;;;; --- editfns.c: replace-buffer-contents (behavioral port) ----------
;; The C primitive minimizes the edit set; a headless substrate has no such
;; optimizer, so this port produces the identical RESULT (SOURCE's contents
;; become the current buffer's) via delete+insert.  Faithful in effect;
;; returns t (the diff always "completes" here).

(unless (fboundp 'replace-buffer-contents)
  (defun replace-buffer-contents (source &optional _max-secs _max-costs)
    "Replace current buffer's text with the text of SOURCE.
SOURCE is a buffer or buffer name.  MAX-SECS and MAX-COSTS are accepted
for API parity but ignored -- this substrate has no minimal-diff optimizer,
so the whole buffer is replaced.  Returns t."
    (let* ((src (get-buffer source))
           (str (and src (with-current-buffer src
                           (buffer-substring-no-properties
                            (point-min) (point-max))))))
      (unless src
        (signal 'error (list "replace-buffer-contents: no such buffer" source)))
      (let ((inhibit-read-only t))
        (delete-region (point-min) (point-max))
        (insert str))
      t)))

;;;; --- window.c: set-window-margins / window-margins (side table) ----
;; The substrate's `emacs-window' struct has no margin slots and no renderer,
;; so margins are stored in a side table keyed by the window object.  This is
;; a real data contract -- `window-margins' returns exactly what
;; `set-window-margins' stored -- with no display effect to fake.

(defvar nemacs-parity--window-margins (make-hash-table :test 'eq :weakness 'key)
  "Side table: WINDOW -> (LEFT-WIDTH . RIGHT-WIDTH) for the margin shims.")

(unless (fboundp 'set-window-margins)
  (defun set-window-margins (window &optional left right)
    "Set WINDOW's left and right margins to LEFT and RIGHT columns.
nil (the default) means no margin.  WINDOW nil means the selected window."
    (let ((win (or window (and (fboundp 'selected-window) (selected-window)))))
      (puthash win (cons left right) nemacs-parity--window-margins)
      nil)))

(unless (fboundp 'window-margins)
  (defun window-margins (&optional window)
    "Return WINDOW's left and right margins as a cons (LEFT . RIGHT).
A margin of nil means no margin.  WINDOW nil means the selected window."
    (let ((win (or window (and (fboundp 'selected-window) (selected-window)))))
      (gethash win nemacs-parity--window-margins '(nil . nil)))))

;;;; --- custom.el: custom-current-group (verbatim, 1-liner) -----------
;; Real Emacs 30.1 definition:
;;   (defun custom-current-group ()
;;     (cdr (assoc load-file-name custom-current-group-alist)))
;; `custom-current-group-alist' is provided by emacs-parity-vars2.el; the
;; `boundp' guard keeps a faithful nil result if it is somehow unbound.

(unless (fboundp 'custom-current-group)
  (defun custom-current-group ()
    "Return the custom group of the file currently being loaded, or nil."
    (and (boundp 'custom-current-group-alist)
         (cdr (assoc load-file-name custom-current-group-alist)))))

;;;; --- faces.el: defined-colors --------------------------------------
;; Real backend when available; otherwise return the genuine standard
;; terminal color names (Emacs' `tty-standard-colors'), the real color set a
;; non-graphic display supports.

(unless (fboundp 'defined-colors)
  (defun defined-colors (&optional frame)
    "Return a list of colors supported for a particular FRAME.
Uses the real display backend when present; otherwise returns the standard
terminal color names supported on a non-graphic display."
    (cond
     ((and (fboundp 'display-graphic-p) (display-graphic-p frame)
           (fboundp 'xw-defined-colors))
      (xw-defined-colors frame))
     ((fboundp 'tty-color-alist)
      (mapcar #'car (tty-color-alist frame)))
     (t
      (list "black" "red" "green" "yellow"
            "blue" "magenta" "cyan" "white")))))

(provide 'emacs-parity-fns2)

;;; emacs-parity-fns2.el ends here
