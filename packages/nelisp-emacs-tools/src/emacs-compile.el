;;; emacs-compile.el --- minimal compile / grep + next-error  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Layer 2 (nelisp-emacs) compilation command semantics: run a shell command,
;; capture its output into a `*compilation*' buffer, parse `FILE:LINE[:COL]:'
;; diagnostics, and navigate them with `next-error' / `previous-error'.
;;
;; Boundary (../nelisp-gui/docs/design/00-three-layer-architecture.org,
;; 04-completion-boundary-plan.org):
;;   - nelisp-emacs OWNS: compile/grep command invocation (via the
;;     `call-process' substrate), *compilation* buffer construction, error
;;     location parsing, and next-error navigation state.
;;   - nelisp-gui OWNS: rendering of the compilation buffer + key transport.
;; This module only computes state / fills buffers; no X11 decisions.

;;; Code:

(require 'emacs-process-builtins)

(defvar emacs-compile-buffer-name "*compilation*"
  "Buffer name used by `emacs-compile-run'.")

(defvar emacs-compile-shell-program "/bin/sh"
  "Shell used to run compile / grep command lines.")

(defvar emacs-compile-error-regexp
  "^\\([^ \t\n:]+\\):\\([0-9]+\\)\\(:\\([0-9]+\\)\\)?:"
  "Regexp matching a `FILE:LINE[:COL]:' diagnostic at the start of a line.
Group 1 = file, 2 = line, 4 = optional column.  A plain capturing group
\(not a shy `\\(?:...\\)' group) wraps the column so the group numbering
stays deterministic on the standalone reader, whose regexp engine
miscounts shy groups as capturing.")

(defvar emacs-compile--errors nil
  "Parsed error plists (:file :line :col :text) from the last run.")

(defvar emacs-compile--error-index -1
  "Cursor into `emacs-compile--errors' for next/previous navigation.")

(defvar emacs-compile--last-command nil
  "The most recent command passed to `emacs-compile-run', for `recompile'.")

;;;; --- parsing ------------------------------------------------------

(defun emacs-compile--parse-errors (text)
  "Parse TEXT for `FILE:LINE[:COL]:' diagnostics.
Return a list of plists (:file :line :col :text) in source order."
  (let ((pos 0) (out nil) (str (or text "")))
    (while (string-match emacs-compile-error-regexp str pos)
      (let* ((mb (match-beginning 0))
             (me (match-end 0))
             (file (match-string 1 str))
             (line (string-to-number (match-string 2 str)))
             ;; group 4 (column) -- group 3 is the optional ":COL" wrapper.
             (col (and (match-string 4 str)
                       (string-to-number (match-string 4 str))))
             (eol (or (string-match "\n" str me) (length str))))
        (push (list :file file :line line :col col
                    :text (substring str mb eol))
              out)
        (setq pos (if (> me pos) me (1+ pos)))))
    (nreverse out)))

;;;; --- run ----------------------------------------------------------

(defun emacs-compile--call (command)
  "Run COMMAND through `emacs-compile-shell-program'.
Return (EXIT-CODE . OUTPUT-STRING); EXIT-CODE is nil with no substrate."
  (if (not (fboundp 'call-process))
      (cons nil "")
    (let ((dir default-directory))
      (with-temp-buffer
        ;; propagate the caller's working directory: `call-process' runs in the
        ;; current buffer's `default-directory', and a temp buffer would
        ;; otherwise reset it.
        (setq default-directory dir)
        (let ((code (call-process emacs-compile-shell-program nil t nil
                                  "-c" command)))
          (cons code (buffer-string)))))))

(defun emacs-compile-run (command)
  "Run COMMAND, capture output in `*compilation*', parse diagnostics.
Resets next-error navigation.  Returns the compilation buffer."
  (let* ((res (emacs-compile--call command))
         (output (cdr res)))
    (setq emacs-compile--last-command command
          emacs-compile--errors (emacs-compile--parse-errors output)
          emacs-compile--error-index -1)
    (let ((buf (get-buffer-create emacs-compile-buffer-name)))
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (format "-*- compilation -*-\n%s\n\n" command))
          (insert output)
          (insert (format "\nCompilation exited with code %s\n" (car res))))
        (goto-char (point-min)))
      (when (fboundp 'display-buffer)
        (display-buffer buf))
      buf)))

(defun emacs-compile-recompile ()
  "Re-run the most recent `emacs-compile-run' command.
Signals an error when no command has been run yet."
  (interactive)
  (unless emacs-compile--last-command
    (error "emacs-compile-recompile: no previous compile command"))
  (emacs-compile-run emacs-compile--last-command))

(defun emacs-compile-errors ()
  "Return the parsed error plists from the last `emacs-compile-run'."
  emacs-compile--errors)

;;;; --- navigation ---------------------------------------------------

(defun emacs-compile--visit (err)
  "Best-effort: open ERR's :file and move to its :line.  Return the buffer."
  (when (and err (fboundp 'find-file-noselect))
    (let ((file (plist-get err :file))
          (line (plist-get err :line)))
      (when (and file (file-exists-p file))
        (let ((buf (find-file-noselect file)))
          (with-current-buffer buf
            (goto-char (point-min))
            (forward-line (1- (or line 1))))
          (when (fboundp 'display-buffer)
            (display-buffer buf))
          buf)))))

(defun emacs-compile-next-error (&optional n)
  "Advance N (default 1) diagnostics and visit the target.
Returns the current error plist, or nil when there are none."
  (interactive)
  (let ((n (or n 1))
        (len (length emacs-compile--errors)))
    (when (> len 0)
      (setq emacs-compile--error-index
            (max 0 (min (1- len) (+ emacs-compile--error-index n))))
      (let ((err (nth emacs-compile--error-index emacs-compile--errors)))
        (emacs-compile--visit err)
        err))))

(defun emacs-compile-previous-error (&optional n)
  "Move back N (default 1) diagnostics.  See `emacs-compile-next-error'."
  (interactive)
  (emacs-compile-next-error (- (or n 1))))

;;;; --- standard-name facade + bindings ------------------------------

(defun emacs-compile-install ()
  "Bind the standard `compile' / `grep' / `next-error' / `previous-error'
command names to the `emacs-compile-*' implementations.  Not run on
`require' (keeps a bare load from touching shared command symbols)."
  (defalias 'compile #'emacs-compile-run)
  (defalias 'recompile #'emacs-compile-recompile)
  (defalias 'grep #'emacs-compile-run)
  (defalias 'next-error #'emacs-compile-next-error)
  (defalias 'previous-error #'emacs-compile-previous-error))

(defun emacs-compile--install-bindings ()
  "Bind `C-x `' to `emacs-compile-next-error' on the global map."
  (let ((map (and (fboundp 'current-global-map) (current-global-map))))
    (when (and map (fboundp 'define-key) (fboundp 'kbd))
      (define-key map (kbd "C-x `") #'emacs-compile-next-error))))

(emacs-compile--install-bindings)

;; T62: `compilation-error-regexp-alist' (compile.el) was void anywhere in
;; src/ -- the load matrix hit `(void-variable compilation-error-regexp-alist)'
;; for `powershell' (which does `(setq compilation-error-regexp-alist (cons
;; NEW-ENTRY compilation-error-regexp-alist))' at top level).  Real Emacs's
;; own default is the ~66-entry `compilation-error-regexp-alist-alist'
;; catalogue (gcc/gnu/msft/... regexps) from the full `progmodes/compile.el',
;; which `src/compile.el' deliberately does NOT load on the standalone
;; runtime (see its own commentary: this substrate installs the much
;; smaller `emacs-compile' read-only engine instead).  Replicating the full
;; real catalogue here would be fabricating fidelity this substrate's
;; next-error engine cannot back up, so -- matching `coding-system-list' /
;; `defined-colors''s own precedent in `emacs-parity-fns2.el' ("a real
;; answer, not a fabricated success") -- this binds one real, honest entry:
;; the exact `FILE:LINE[:COL]:' pattern `emacs-compile--parse-errors' itself
;; actually parses, in the real `(REGEXP FILE LINE COLUMN)' shape.  That is
;; enough for `(cons NEW-ENTRY compilation-error-regexp-alist)'-style
;; top-level `setq' forms to succeed, matching `button-buffer-map''s "kind,
;; not content" precedent (Doc 51 T52) for a reduced value.
(unless (boundp 'compilation-error-regexp-alist)
  (defvar compilation-error-regexp-alist
    (list (list emacs-compile-error-regexp 1 2 4))
    "Reduced `compilation-error-regexp-alist'.
See the comment above this defvar for why this holds one real entry
(this substrate's own next-error pattern) instead of the full GNU
catalogue."))

;;;; --- keymaps other packages bind into -------------------------------

;; T102: `compilation-minor-mode-map' (compile.el) was void anywhere in
;; src/ -- the load matrix hit `(void-variable compilation-minor-mode-map)'
;; for `magit-todos' (whose dependency `grep.el' does `(set-keymap-parent
;; map compilation-minor-mode-map)' at top level while building
;; `grep-mode-map').  `src/compile.el' provides the `compile' feature
;; directly on the standalone runtime (see its own commentary) without
;; ever loading real Emacs's `vendor/emacs-lisp/progmodes/compile.el', so
;; that vendor file's `compilation-minor-mode-map' defvar never runs.
;;
;; This substrate does not implement `compilation-mode'/
;; `define-compilation-mode' (the reduced `emacs-compile' engine above
;; intentionally does not replicate that machinery -- see the
;; `compilation-error-regexp-alist' comment above for the same
;; "reduced, honest substrate" precedent), but the KEYMAP data other
;; packages bind into at top level is cheap, static, and faithfully
;; portable.  Bindings and parentage below are copied verbatim from
;; real Emacs's `progmodes/compile.el' `compilation-menu-map' and
;; `compilation-minor-mode-map' (`special-mode-map' parentage matches;
;; see `emacs-mode-builtins.el' for this substrate's `special-mode-map').
;;
;; The bare forward `defvar' below only marks `compilation-menu-map'
;; special for this file's byte-compilation (it gives no value); the
;; conditional block right after gives the real value exactly once.
(defvar compilation-menu-map)

(unless (boundp 'compilation-menu-map)
  (defvar compilation-menu-map
    (let ((map (make-sparse-keymap "Errors"))
          (opt-map (make-sparse-keymap "Skip")))
      (define-key map [stop-subjob]
        '(menu-item "Stop Compilation" kill-compilation
                    :help "Kill the process made by the M-x compile or M-x grep commands"))
      (define-key map [compilation-mode-separator3]
        '("----" . nil))
      (define-key map [compilation-next-error-follow-minor-mode]
        '(menu-item
          "Auto Error Display" next-error-follow-minor-mode
          :help "Display the error under cursor when moving the cursor"
          :button (:toggle . next-error-follow-minor-mode)))
      (define-key map [compilation-skip]
        (cons "Skip Less Important Messages" opt-map))
      (define-key opt-map [compilation-skip-none]
        '(menu-item "Don't Skip Any Messages"
                    (lambda ()
                      (interactive)
                      (customize-set-variable 'compilation-skip-threshold 0))
                    :help "Do not skip any type of messages"
                    :button (:radio . (eq compilation-skip-threshold 0))))
      (define-key opt-map [compilation-skip-info]
        '(menu-item "Skip Info"
                    (lambda ()
                      (interactive)
                      (customize-set-variable 'compilation-skip-threshold 1))
                    :help "Skip anything less than warning"
                    :button (:radio . (eq compilation-skip-threshold 1))))
      (define-key opt-map [compilation-skip-warning-and-info]
        '(menu-item "Skip Warnings and Info"
                    (lambda ()
                      (interactive)
                      (customize-set-variable 'compilation-skip-threshold 2))
                    :help "Skip over Warnings and Info, stop for errors"
                    :button (:radio . (eq compilation-skip-threshold 2))))
      (define-key map [compilation-mode-separator2]
        '("----" . nil))
      (define-key map [compilation-first-error]
        '(menu-item "First Error" first-error
                    :help "Restart at the first error, visit corresponding source code"))
      (define-key map [compilation-previous-error]
        '(menu-item "Previous Error" previous-error
                    :help "Visit previous `next-error' message and corresponding source code"))
      (define-key map [compilation-next-error]
        '(menu-item "Next Error" next-error
                    :help "Visit next `next-error' message and corresponding source code"))
      map)
    "Keymap for `compilation-mode'/`compilation-minor-mode' error menus.
Ported from real Emacs `progmodes/compile.el' -- see the comment above
this block."))

(unless (boundp 'compilation-minor-mode-map)
  (defvar compilation-minor-mode-map
    (let ((map (make-sparse-keymap)))
      (set-keymap-parent map special-mode-map)
      (define-key map [mouse-2] 'compile-goto-error)
      (define-key map [follow-link] 'mouse-face)
      (define-key map "\C-c\C-c" 'compile-goto-error)
      (define-key map "\C-m" 'compile-goto-error)
      (define-key map "\C-o" 'compilation-display-error)
      (define-key map "\C-c\C-k" 'kill-compilation)
      (define-key map "\M-n" 'compilation-next-error)
      (define-key map "\M-p" 'compilation-previous-error)
      (define-key map "\M-{" 'compilation-previous-file)
      (define-key map "\M-}" 'compilation-next-file)
      (define-key map "n" 'next-error-no-select)
      (define-key map "p" 'previous-error-no-select)
      (define-key map "l" 'recenter-current-error)
      (define-key map "g" 'recompile)
      (define-key map [menu-bar compilation]
        (cons "Errors" compilation-menu-map))
      map)
    "Keymap for `compilation-minor-mode'.
Ported from real Emacs `progmodes/compile.el' -- see the comment above
this block."))

(provide 'emacs-compile)

;;; emacs-compile.el ends here
