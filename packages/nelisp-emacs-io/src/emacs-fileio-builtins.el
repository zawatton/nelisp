;;; emacs-fileio-builtins.el --- File I/O bridges + find-file / save-buffer  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Doc 51 Track D Phase D (2026-05-03) — Layer 2.
;;
;; Bridges the unprefixed file-I/O primitives (= `expand-file-name',
;; `file-name-directory', `file-exists-p', `insert-file-contents',
;; `write-region', ...) to the existing `nelisp-emacs-compat-fileio'
;; (= `nelisp-ec-*') substrate, AND derives the high-level commands
;; the user-facing edit cycle needs (= `find-file-noselect',
;; `find-file', `save-buffer', `write-file', `revert-buffer',
;; `buffer-file-name', `set-visited-file-name').
;;
;; Architecture:
;;
;;   - 1:1 defalias bridges for the substrate-direct primitives.
;;   - A global alist `emacs-fileio--buffer-files' keyed by buffer
;;     record (= eq) maps each visited buffer to its filename.  This
;;     stands in for Emacs' buffer-local `buffer-file-name' until
;;     `nelisp-ec-buffer' grows a real per-buffer slot.
;;   - High-level commands compose the bridges: e.g. `find-file-noselect'
;;     = `(get-buffer-create NAME)' + `(insert-file-contents)' +
;;     `(set-visited-file-name)'.
;;
;; Each definition is gated on `unless (fboundp ...)' / `unless
;; (boundp ...)' so loading inside a host Emacs is a cheap no-op
;; (= host's C builtins win).
;;
;; Phase D unblocks the β-stage edit cycle: `nemacs hello.txt' ->
;; `(insert "world")' -> `(save-buffer)' -> `(kill-buffer)'.
;; Reading and writing UTF-8 files is the whole substrate target;
;; locking, auto-save, backup files, and coding-system selection
;; remain L1.5 / future-phase work.

;;; Code:

(require 'nelisp-emacs-compat)
(require 'nelisp-emacs-compat-fileio)
(require 'emacs-buffer-builtins)

;;;; --- file-name parsing ----------------------------------------------

(unless (fboundp 'expand-file-name)
  (defalias 'expand-file-name #'nelisp-ec-expand-file-name))

(unless (fboundp 'file-name-absolute-p)
  (defalias 'file-name-absolute-p #'nelisp-ec-file-name-absolute-p))

(unless (fboundp 'file-name-directory)
  (defalias 'file-name-directory #'nelisp-ec-file-name-directory))

(unless (fboundp 'file-name-nondirectory)
  (defalias 'file-name-nondirectory #'nelisp-ec-file-name-nondirectory))

(unless (fboundp 'file-name-as-directory)
  (defalias 'file-name-as-directory #'nelisp-ec-file-name-as-directory))

;;;; --- predicates / attributes ----------------------------------------

(unless (fboundp 'file-exists-p)
  (defalias 'file-exists-p #'nelisp-ec-file-exists-p))

(unless (fboundp 'file-readable-p)
  (defalias 'file-readable-p #'nelisp-ec-file-readable-p))

(unless (fboundp 'file-directory-p)
  (defalias 'file-directory-p #'nelisp-ec-file-directory-p))

(unless (fboundp 'file-attributes)
  (defalias 'file-attributes #'nelisp-ec-file-attributes))

(unless (fboundp 'directory-files)
  (defalias 'directory-files #'nelisp-ec-directory-files))

(unless (fboundp 'executable-find)
  (defalias 'executable-find #'nelisp-ec-executable-find))

;;;; --- mutation ------------------------------------------------------

(unless (fboundp 'delete-file)
  (defalias 'delete-file #'nelisp-ec-delete-file))

(unless (fboundp 'rename-file)
  (defalias 'rename-file #'nelisp-ec-rename-file))

;;;; --- read / write ---------------------------------------------------

(unless (fboundp 'insert-file-contents)
  (defalias 'insert-file-contents #'nelisp-ec-insert-file-contents))

(unless (fboundp 'write-region)
  (defun write-region (start end filename &optional append visit lockname mustbenew)
    "Phase D polyfill: forward to `nelisp-ec-write-region'.
LOCKNAME / MUSTBENEW are accepted for API parity but ignored —
the substrate has no file-locking subsystem yet."
    (ignore lockname mustbenew)
    (nelisp-ec-write-region start end filename append visit)))

;;;; --- visited-file state ---------------------------------------------

(defvar emacs-fileio--buffer-files nil
  "Phase D state: alist mapping `nelisp-ec-buffer' record → filename.
Stands in for Emacs' buffer-local `buffer-file-name' until the
substrate adds a real per-buffer slot.  Entries for killed buffers
are cleaned up by `find-file-noselect' on next visit.")

(defun emacs-fileio--clean-killed ()
  "Drop entries from `emacs-fileio--buffer-files' whose buffer is killed."
  (let ((live nil))
    (dolist (cell emacs-fileio--buffer-files)
      (when (and (nelisp-ec-buffer-p (car cell))
                 (not (nelisp-ec-buffer-killed-p (car cell))))
        (setq live (cons cell live))))
    (setq emacs-fileio--buffer-files (nreverse live))))

(unless (fboundp 'buffer-file-name)
  (defun buffer-file-name (&optional buffer)
    "Phase D polyfill: read the visited filename of BUFFER (default = current).
Returns nil when the buffer is not visiting a file."
    (let ((b (or buffer (nelisp-ec-current-buffer))))
      (when (and b (nelisp-ec-buffer-p b)
                 (not (nelisp-ec-buffer-killed-p b)))
        (cdr (assq b emacs-fileio--buffer-files))))))

(unless (fboundp 'set-visited-file-name)
  (defun set-visited-file-name (filename &optional no-query along-with-file)
    "Phase D polyfill: associate the current buffer with FILENAME.
NO-QUERY / ALONG-WITH-FILE are accepted for API parity but ignored —
the substrate has no rename-on-visit / lockfile interaction yet."
    (ignore no-query along-with-file)
    (let ((b (nelisp-ec-current-buffer)))
      (when (and b (nelisp-ec-buffer-p b))
        (setq emacs-fileio--buffer-files
              (cons (cons b filename)
                    (assq-delete-all b emacs-fileio--buffer-files)))
        filename))))

;;;; --- find-file / save-buffer / write-file / revert-buffer ----------

(unless (fboundp 'find-file-noselect)
  (defun find-file-noselect (filename &optional nowarn rawfile wildcards)
    "Phase D polyfill: return a buffer visiting FILENAME, loading it if needed.
NOWARN / RAWFILE / WILDCARDS are accepted for API parity but ignored
(= no warning system, no raw-byte mode, no glob expansion in MVP)."
    (ignore nowarn rawfile wildcards)
    (emacs-fileio--clean-killed)
    (let* ((abs (nelisp-ec-expand-file-name filename))
           (existing
            (catch 'found
              (dolist (cell emacs-fileio--buffer-files)
                (when (equal abs (cdr cell))
                  (throw 'found (car cell))))
              nil)))
      (cond
       (existing existing)
       (t
        (let* ((bname (nelisp-ec-file-name-nondirectory abs))
               (buf (nelisp-ec-generate-new-buffer
                     (if (and bname (> (length bname) 0)) bname " *find-file*"))))
          (nelisp-ec-with-current-buffer buf
            (when (nelisp-ec-file-exists-p abs)
              (nelisp-ec-insert-file-contents abs))
            (setq emacs-fileio--buffer-files
                  (cons (cons buf abs)
                        (assq-delete-all buf emacs-fileio--buffer-files))))
          buf))))))

(unless (fboundp 'find-file)
  (defun find-file (filename &optional wildcards)
    "Phase D polyfill: visit FILENAME and make it the current buffer."
    (ignore wildcards)
    (let ((buf (find-file-noselect filename)))
      (nelisp-ec-set-buffer buf)
      buf)))

(unless (fboundp 'save-buffer)
  (defun save-buffer (&optional arg)
    "Phase D polyfill: write the current buffer to its visited file.
ARG is accepted for API parity but ignored — the host's prefix-arg
disambiguation (= multiple backup levels) has no MVP equivalent."
    (ignore arg)
    (let* ((b (nelisp-ec-current-buffer))
           (f (and b (buffer-file-name b))))
      (cond
       ((null f)
        (signal 'error '("save-buffer: buffer is not visiting a file")))
       (t
        (write-region (nelisp-ec-point-min) (nelisp-ec-point-max) f)
        f)))))

(unless (fboundp 'write-file)
  (defun write-file (filename &optional confirm)
    "Phase D polyfill: write the current buffer to FILENAME and visit it.
CONFIRM is accepted for API parity but ignored (= no interactive
confirmation prompt in MVP)."
    (ignore confirm)
    (let ((abs (nelisp-ec-expand-file-name filename)))
      (write-region (nelisp-ec-point-min) (nelisp-ec-point-max) abs)
      (set-visited-file-name abs)
      abs)))

(unless (fboundp 'revert-buffer)
  (defun revert-buffer (&optional ignore-auto noconfirm preserve-modes)
    "Phase D polyfill: reload the visited file into the current buffer.
IGNORE-AUTO / NOCONFIRM / PRESERVE-MODES are accepted for API parity
but ignored (= no auto-save subsystem, no confirm prompt, no major
mode rerun yet)."
    (ignore ignore-auto noconfirm preserve-modes)
    (let* ((b (nelisp-ec-current-buffer))
           (f (and b (buffer-file-name b))))
      (when f
        (nelisp-ec-erase-buffer)
        (nelisp-ec-insert-file-contents f)
        f))))

(provide 'emacs-fileio-builtins)

;;; emacs-fileio-builtins.el ends here
