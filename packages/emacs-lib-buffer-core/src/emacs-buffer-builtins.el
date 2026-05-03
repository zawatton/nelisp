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
  (defalias 'forward-char #'nelisp-ec-forward-char))

(unless (fboundp 'backward-char)
  (defalias 'backward-char #'nelisp-ec-backward-char))

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
  (defalias 'delete-char #'nelisp-ec-delete-char))

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
