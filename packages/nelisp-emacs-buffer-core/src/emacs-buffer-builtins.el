;;; emacs-buffer-builtins.el --- Unprefixed Emacs C-core buffer builtins  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Doc 51 Phase 9 — Layer 2.
;;
;; Bridges the Emacs C-core *unprefixed* buffer builtins (= the names
;; that vanilla Elisp code expects: `generate-new-buffer',
;; `with-current-buffer', `point-min', `buffer-substring-no-properties',
;; ...) to NeLisp's `nelisp-emacs-compat' (= `nelisp-ec-*') primitives.
;;
;; Phase 8 shipped a pragmatic accumulator-string approximation for
;; `with-temp-buffer' / `insert' / `buffer-string' inside `emacs-stub.el'.
;; That sufficed to unblock anvil-memory tokenizer + worklog write paths
;; but failed once a caller wanted to manipulate two buffers at once
;; (the accumulator was a single global string), or wanted the natural
;; `(buffer-substring-no-properties (point-min) (point-max))' pattern.
;;
;; Phase 9 replaces the accumulator with the real `nelisp-ec-*' buffer
;; substrate (T39, ~31 APIs), which already implements multi-buffer
;; current-buffer dispatch, narrow/widen, markers, and search.  This
;; file is purely a *naming bridge* — every definition is gated on
;; `unless (fboundp ...)' so loading inside a host Emacs is a cheap
;; no-op and the host's own C builtins win.
;;
;; What this module unblocks (= deferred from Phase 8 commit):
;;
;;   - `anvil-worklog-export-org' (= multi-buffer; uses
;;     `generate-new-buffer' + `with-current-buffer' + `kill-buffer'
;;     in `unwind-protect' shape).
;;   - any future MCP tool that wants `buffer-substring-no-properties'
;;     of a non-temp buffer.
;;
;; Non-goals (= still deferred):
;;
;;   - `make-network-process' / `memory-serve-start' (Phase 10
;;     candidate, requires socket primitive separate from buffer).
;;   - file-coding handling beyond UTF-8 default
;;     (= `coding-system-for-write' is read but not enforced).
;;   - hooks like `before-change-functions' / `after-change-functions'
;;     (= callers in the 22/27 working set don't depend on them).

;;; Code:

(require 'nelisp-emacs-compat)

;;;; --- creation / liveness -----------------------------------------------

(unless (fboundp 'generate-new-buffer)
  (defalias 'generate-new-buffer #'nelisp-ec-generate-new-buffer))

(unless (fboundp 'kill-buffer)
  (defalias 'kill-buffer #'nelisp-ec-kill-buffer))

(unless (fboundp 'bufferp)
  (defalias 'bufferp #'nelisp-ec-buffer-p))

(unless (fboundp 'buffer-live-p)
  (defun buffer-live-p (object)
    "Return non-nil when OBJECT is a live (non-killed) buffer."
    (and (nelisp-ec-buffer-p object)
         (not (nelisp-ec-buffer-killed-p object)))))

(unless (fboundp 'buffer-name)
  (defun buffer-name (&optional buffer)
    "Return the name of BUFFER (default = current buffer)."
    (cond
     ((null buffer)
      (let ((b (nelisp-ec-current-buffer)))
        (and b (nelisp-ec-buffer-name b))))
     ((nelisp-ec-buffer-p buffer)
      (nelisp-ec-buffer-name buffer))
     (t nil))))

;;;; --- registry lookup (Phase L1, 2026-05-03) --------------------------

(unless (fboundp 'get-buffer)
  (defun get-buffer (buffer-or-name)
    "Phase L1 polyfill: look BUFFER-OR-NAME up in the `nelisp-ec' registry.
When BUFFER-OR-NAME is a buffer object, return it if live else nil.
When it is a string, return the matching buffer record or nil."
    (cond
     ((null buffer-or-name) nil)
     ((nelisp-ec-buffer-p buffer-or-name)
      (if (nelisp-ec-buffer-killed-p buffer-or-name)
          nil
        buffer-or-name))
     ((stringp buffer-or-name)
      (cdr (assoc buffer-or-name nelisp-ec--buffers)))
     (t nil))))

(unless (fboundp 'get-buffer-create)
  (defun get-buffer-create (buffer-or-name &optional inhibit-buffer-hooks)
    "Phase L1 polyfill: get an existing buffer or create a fresh one.
INHIBIT-BUFFER-HOOKS is accepted for API parity but no buffer-hook
subsystem exists yet to honor it."
    (ignore inhibit-buffer-hooks)
    (or (get-buffer buffer-or-name)
        (nelisp-ec-generate-new-buffer
         (cond
          ((stringp buffer-or-name) buffer-or-name)
          ((nelisp-ec-buffer-p buffer-or-name)
           (nelisp-ec-buffer-name buffer-or-name))
          (t " *unnamed*"))))))

(unless (fboundp 'buffer-list)
  (defun buffer-list (&optional frame)
    "Phase L1 polyfill: return a list of every live buffer in the registry.
FRAME is accepted for API parity (host filters by frame) but the
prefixed substrate has no per-frame buffer affinity, so all live
buffers are returned regardless."
    (ignore frame)
    (let ((acc nil))
      (dolist (cell nelisp-ec--buffers)
        (let ((buf (cdr cell)))
          (when (and buf (not (nelisp-ec-buffer-killed-p buf)))
            (setq acc (cons buf acc)))))
      ;; Reverse for registry-insertion order (= push above prepended).
      (let ((rev nil))
        (while acc
          (setq rev (cons (car acc) rev))
          (setq acc (cdr acc)))
        rev))))

;;;; --- current buffer ---------------------------------------------------

(unless (fboundp 'current-buffer)
  (defalias 'current-buffer #'nelisp-ec-current-buffer))

(unless (fboundp 'set-buffer)
  (defalias 'set-buffer #'nelisp-ec-set-buffer))

(unless (fboundp 'with-current-buffer)
  (defmacro with-current-buffer (buf &rest body)
    "Phase 9 polyfill: forward to `nelisp-ec-with-current-buffer'."
    (declare (indent 1) (debug (form body)))
    (cons 'nelisp-ec-with-current-buffer (cons buf body))))

;;;; --- positions ---------------------------------------------------------

(unless (fboundp 'point)
  (defalias 'point #'nelisp-ec-point))

(unless (fboundp 'point-min)
  (defalias 'point-min #'nelisp-ec-point-min))

(unless (fboundp 'point-max)
  (defalias 'point-max #'nelisp-ec-point-max))

(unless (fboundp 'goto-char)
  (defalias 'goto-char #'nelisp-ec-goto-char))

(unless (fboundp 'forward-char)
  (defun forward-char (&optional n)
    "Phase 9 polyfill: move point N (default 1) characters forward.
Bound to C-f / <right>.

Matches the real Emacs C `forward-char' end-of-buffer semantics: when
the target lies past the accessible end, point is clamped to point-max
and `end-of-buffer' is signaled (not `nelisp-ec-args-out-of-range' from
the underlying primitive).  The command loop catches the signal as a
soft non-fatal end-of-buffer message; non-loop callers can wrap in
`condition-case' against `end-of-buffer'."
    (interactive "p")
    (let* ((n (or n 1))
           (p (nelisp-ec-point))
           (lo (nelisp-ec-point-min))
           (hi (nelisp-ec-point-max))
           (target (+ p n)))
      (cond
       ((< target lo)
        (nelisp-ec-goto-char lo)
        (signal 'beginning-of-buffer nil))
       ((> target hi)
        (nelisp-ec-goto-char hi)
        (signal 'end-of-buffer nil))
       (t
        (nelisp-ec-goto-char target)
        t)))))

(unless (fboundp 'backward-char)
  (defun backward-char (&optional n)
    "Phase 9 polyfill: move point N (default 1) characters backward.
Bound to C-b / <left>.

Symmetric to `forward-char' for `beginning-of-buffer' / `end-of-buffer'
clamp + signal semantics."
    (interactive "p")
    (forward-char (- (or n 1)))))

(unless (fboundp 'buffer-size)
  (defalias 'buffer-size #'nelisp-ec-buffer-size))

;;;; --- text mutation + accessors ----------------------------------------

(unless (fboundp 'insert)
  (defalias 'insert #'nelisp-ec-insert))

(unless (fboundp 'erase-buffer)
  (defalias 'erase-buffer #'nelisp-ec-erase-buffer))

(unless (fboundp 'delete-region)
  (defalias 'delete-region #'nelisp-ec-delete-region))

(unless (fboundp 'delete-char)
  (defun delete-char (n &optional killflag)
    "Phase 9 polyfill: delete N characters forward (negative = backward).
KILLFLAG accepted for host API parity but ignored in MVP.
Forwards to `nelisp-ec-delete-char'.  Bound to C-d.

The `(interactive \"p\")' form supplies N from the prefix-arg, so a
keymap dispatch with no prefix passes N=1.  Without this form,
`call-interactively' would build an empty arg list and crash on the
required N parameter (= the same lambda-arity-mismatch that bit
`delete-backward-char' before its 2026-05-04 fix)."
    (interactive "p")
    (ignore killflag)
    (nelisp-ec-delete-char n)))

(unless (fboundp 'buffer-string)
  (defalias 'buffer-string #'nelisp-ec-buffer-string))

(unless (fboundp 'buffer-substring)
  (defalias 'buffer-substring #'nelisp-ec-buffer-substring))

(unless (fboundp 'buffer-substring-no-properties)
  ;; Phase 9 MVP: text properties are not yet stored on
  ;; `nelisp-ec-buffer'; the substring already carries no properties.
  (defalias 'buffer-substring-no-properties #'nelisp-ec-buffer-substring))

;;;; --- save-* family ----------------------------------------------------

(unless (fboundp 'save-excursion)
  (defmacro save-excursion (&rest body)
    "Phase 9 polyfill: forward to `nelisp-ec-save-excursion'."
    (declare (indent 0) (debug (body)))
    (cons 'nelisp-ec-save-excursion body)))

(unless (fboundp 'save-restriction)
  (defmacro save-restriction (&rest body)
    "Phase 9 polyfill: forward to `nelisp-ec-save-restriction'."
    (declare (indent 0) (debug (body)))
    (cons 'nelisp-ec-save-restriction body)))

(unless (fboundp 'save-current-buffer)
  (defmacro save-current-buffer (&rest body)
    "Phase 9 polyfill: forward to `nelisp-ec-save-current-buffer'."
    (declare (indent 0) (debug (body)))
    (cons 'nelisp-ec-save-current-buffer body)))

;;;; --- narrow / widen ---------------------------------------------------

(unless (fboundp 'narrow-to-region)
  (defalias 'narrow-to-region #'nelisp-ec-narrow-to-region))

(unless (fboundp 'widen)
  (defalias 'widen #'nelisp-ec-widen))

;;;; --- markers ----------------------------------------------------------

(unless (fboundp 'make-marker)
  (defalias 'make-marker #'nelisp-ec-make-marker))

(unless (fboundp 'set-marker)
  (defalias 'set-marker #'nelisp-ec-set-marker))

(unless (fboundp 'marker-position)
  (defalias 'marker-position #'nelisp-ec-marker-position))

(unless (fboundp 'marker-buffer)
  (defalias 'marker-buffer #'nelisp-ec-marker-buffer))

(unless (fboundp 'point-marker)
  (defalias 'point-marker #'nelisp-ec-point-marker))

;;;; --- with-temp-buffer / with-temp-file (Phase 9 rewrite) -------------

;; Phase 8 used a global string accumulator (`emacs-stub--current-temp-buffer')
;; which collapsed under multi-buffer scenarios.  Phase 9 replaces the body
;; with a real `nelisp-ec' buffer that participates in the current-buffer
;; dispatch and respects narrow / point.

(unless (fboundp 'with-temp-buffer)
  (defmacro with-temp-buffer (&rest body)
    "Phase 9 polyfill: real-buffer rewrite of `with-temp-buffer'.
A fresh `nelisp-ec' buffer named ` *temp*' is created, made current
for BODY, then killed unconditionally on exit (= via `unwind-protect')."
    (declare (indent 0) (debug (body)))
    (let ((buf (make-symbol "buf")))
      (list 'let (list (list buf (list 'nelisp-ec-generate-new-buffer
                                       " *temp*")))
            (list 'unwind-protect
                  (cons 'nelisp-ec-with-current-buffer (cons buf body))
                  (list 'nelisp-ec-kill-buffer buf))))))

(unless (fboundp 'with-temp-file)
  (defmacro with-temp-file (path &rest body)
    "Phase 9 polyfill: real-buffer rewrite of `with-temp-file'.
BODY runs inside a fresh `nelisp-ec' buffer; on normal exit the buffer
contents are written to PATH via `nl-write-file' (when available),
falling back to `write-region' under host Emacs."
    (declare (indent 1) (debug (form body)))
    (let ((buf (make-symbol "buf"))
          (p (make-symbol "p"))
          (s (make-symbol "s")))
      (list 'let (list (list p path)
                       (list buf (list 'nelisp-ec-generate-new-buffer
                                       " *temp-file*")))
            (list 'unwind-protect
                  (list 'progn
                        (cons 'nelisp-ec-with-current-buffer (cons buf body))
                        (list 'let (list (list s
                                               (list
                                                'nelisp-ec-with-current-buffer
                                                buf
                                                '(nelisp-ec-buffer-string))))
                              (list 'cond
                                    (list (list 'fboundp (list 'quote
                                                               'nl-write-file))
                                          (list 'nl-write-file p s))
                                    (list (list 'fboundp (list 'quote
                                                               'write-region))
                                          (list 'write-region s nil p)))))
                  (list 'nelisp-ec-kill-buffer buf))))))

(provide 'emacs-buffer-builtins)

;;; emacs-buffer-builtins.el ends here
