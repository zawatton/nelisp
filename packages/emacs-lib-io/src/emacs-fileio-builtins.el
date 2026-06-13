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
;; Loading inside a host Emacs is a cheap no-op (= host's C builtins
;; win).  Standalone NeLisp deliberately overwrites the earlier
;; `emacs-stub-bulk.el' no-op shims.
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
(require 'emacs-string)

(defconst emacs-fileio-builtins--standalone-overrides
  '(insert-file-contents
    write-region
    file-name-quoted-p
    file-name-quote
    file-name-unquote
    buffer-file-name
    set-visited-file-name
    locate-library
    executable-find
    substitute-in-file-name
    find-file-noselect
    find-file
    save-buffer
    write-file
    revert-buffer)
  "Functions this bridge may overwrite under standalone NeLisp.
Path parsing, predicates, and directory/syscall primitives are left to
the runtime when they already exist because `load' / `require' depend
on those semantics during bootstrap.")

(defun emacs-fileio-builtins--install-function-p (symbol)
  "Return non-nil when SYMBOL should be installed by this bridge."
  (or (not (emacs-fileio-builtins--function-cell-live-p symbol))
      (and (not (boundp 'emacs-version))
           (memq symbol emacs-fileio-builtins--standalone-overrides))))

(defun emacs-fileio-builtins--function-cell-live-p (symbol)
  "Return non-nil when SYMBOL has a usable function cell."
  (and (fboundp symbol)
       (condition-case nil
           (symbol-function symbol)
         (error nil))))

;;;; --- batched trivial defaliases (Doc 51 Phase 5 boot perf) -----------
;;
;; Pattern source: commit d3c17fa (emacs-stub-bulk Phase 11.D batch).  The
;; nelisp standalone interpreter charges ~47ms per top-level form for the
;; original `(unless (fboundp X) (defalias X #'nelisp-ec-Y))' idiom — 14
;; clauses below + 23 in `emacs-buffer-builtins.el' add ~1.8s on every
;; bootstrap.  Collapsing through one dolist body keeps the gate semantics
;; identical (= each entry still does exactly one fboundp test) while
;; paying the per-form interpreter overhead only once.  Under host Emacs
;; the C subr wins fboundp so this is a no-op either way.
;;
;; All 14 substrate-direct file-I/O bridges in this file map uniformly to
;; `nelisp-ec-<same-name>', so we use a bare symbol list (no pairs) and
;; synthesize the target name with `(intern (concat "nelisp-ec-" ...))'.

(dolist (--name--
         '(;; --- file-name parsing
           expand-file-name
           file-name-absolute-p
           file-name-directory
           file-name-nondirectory
           file-name-as-directory
           substitute-in-file-name
           ;; --- predicates / attributes
           file-exists-p
           file-readable-p
           file-executable-p
           file-directory-p
           file-attributes
           directory-files
           executable-find
           ;; --- mutation
           delete-file
           rename-file
           ;; --- read / write
           insert-file-contents))
  (when (emacs-fileio-builtins--install-function-p --name--)
    (defalias --name--
      (intern (concat "nelisp-ec-" (symbol-name --name--))))))

;; --- access(2)-backed predicates for the standalone reader ----------
;; The `emacs-fileio-builtins--install-function-p' standalone clause keys
;; off `(not (boundp 'emacs-version))', but nemacs binds `emacs-version'
;; (for vendor compatibility), so that clause never fires here.  Its
;; native `file-exists-p' meanwhile resolves to a stat path that
;; misreports on the reader (always nil), and `file-executable-p' is
;; absent.  When the verified access(2) primitive `nelisp--syscall-path-int'
;; is present we therefore force-install the `nelisp-ec-*' access-backed
;; predicates over whatever the runtime bound.  This block is inert under
;; host Emacs (no `nelisp--syscall-path-int'), so the C builtins stand.
(when (fboundp 'nelisp--syscall-path-int)
  (defalias 'file-exists-p #'nelisp-ec-file-exists-p)
  (defalias 'file-readable-p #'nelisp-ec-file-readable-p)
  (defalias 'file-executable-p #'nelisp-ec-file-executable-p)
  ;; `executable-find' is in `--standalone-overrides' but, like the
  ;; predicates above, that clause does not fire on nemacs; an earlier
  ;; `emacs-stub' stub otherwise shadows the working `nelisp-ec' PATH walk.
  (defalias 'executable-find #'nelisp-ec-executable-find))

;;;; --- file-name parsing ----------------------------------------------

;; expand-file-name / file-name-absolute-p / file-name-directory /
;; file-name-nondirectory / file-name-as-directory batched into the dolist
;; near the top.

(defun emacs-fileio--directory-file-names (dir)
  "Return the non-full file names in DIR, or nil if DIR cannot be read."
  (condition-case nil
      (directory-files dir nil nil t)
    (error nil)))

(defun emacs-fileio--safe-directory-probe-p ()
  "Return non-nil when `locate-library' may inspect directories.
The legacy standalone `nelisp--syscall-readdir' / stat predicates can
stop the evaluator on some failure paths.  Until the newer opendir API
is present, standalone `locate-library' must prefer a safe nil result
over probing the filesystem."
  (or (not (fboundp 'nl-write-file))
      (fboundp 'nl-syscall-opendir)))

(defun emacs-fileio--locate-library-candidate (base name)
  "Return absolute path for NAME under BASE when the directory lists it."
  (let* ((relative-dir (file-name-directory name))
         (leaf (file-name-nondirectory name))
         (search-dir (if relative-dir
                         (expand-file-name relative-dir base)
                       base))
         (entries (emacs-fileio--directory-file-names search-dir)))
    (and (member leaf entries)
         (expand-file-name name base))))

(defun emacs-fileio-locate-library (library &optional nosuffix path interactive-call)
  "Return the absolute path of LIBRARY found on PATH or `load-path'.
This standalone implementation searches plain LIBRARY first, then
LIBRARY.el unless NOSUFFIX is non-nil.  INTERACTIVE-CALL is accepted for
Emacs API parity and ignored."
  (ignore interactive-call)
  (when (emacs-fileio--safe-directory-probe-p)
    (let ((dirs (or path load-path))
          (names (if nosuffix
                     (list library)
                   (list library (concat library ".el"))))
          found)
      (catch 'done
        (dolist (dir dirs)
          (let ((base (if dir
                          (file-name-as-directory dir)
                        (or (and (boundp 'default-directory) default-directory)
                            ""))))
            (dolist (name names)
              (let ((candidate (emacs-fileio--locate-library-candidate base name)))
                (when candidate
                  (setq found candidate)
                  (throw 'done nil)))))))
      found)))

(when (emacs-fileio-builtins--install-function-p 'locate-library)
  (defalias 'locate-library #'emacs-fileio-locate-library))

(when (emacs-fileio-builtins--install-function-p 'file-name-quoted-p)
  (defun file-name-quoted-p (name &optional top)
    "Return non-nil when NAME starts with the Emacs quote prefix `/:'."
    (ignore top)
    (and (stringp name)
         (string-prefix-p "/:" name))))

(when (emacs-fileio-builtins--install-function-p 'file-name-quote)
  (defun file-name-quote (name &optional top)
    "Add the Emacs file-name quote prefix `/:` to NAME if needed."
    (ignore top)
    (if (file-name-quoted-p name)
        name
      (concat "/:" name))))

(when (emacs-fileio-builtins--install-function-p 'file-name-unquote)
  (defun file-name-unquote (name &optional top)
    "Remove the Emacs file-name quote prefix `/:` from NAME when present."
    (ignore top)
    (if (file-name-quoted-p name)
        (substring name 2)
      name)))

;;;; --- predicates / attributes ----------------------------------------

;; file-exists-p / file-readable-p / file-directory-p / file-attributes /
;; directory-files / executable-find batched into the dolist near the top.

;;;; --- mutation ------------------------------------------------------

;; delete-file / rename-file batched into the dolist near the top.

;;;; --- read / write ---------------------------------------------------

;; insert-file-contents batched into the dolist near the top.

(when (emacs-fileio-builtins--install-function-p 'write-region)
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

(when (emacs-fileio-builtins--install-function-p 'buffer-file-name)
  (defun buffer-file-name (&optional buffer)
    "Phase D polyfill: read the visited filename of BUFFER (default = current).
Returns nil when the buffer is not visiting a file."
    (let ((b (or buffer (nelisp-ec-current-buffer))))
      (when (and b (nelisp-ec-buffer-p b)
                 (not (nelisp-ec-buffer-killed-p b)))
        (cdr (assq b emacs-fileio--buffer-files))))))

(when (emacs-fileio-builtins--install-function-p 'set-visited-file-name)
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

(when (emacs-fileio-builtins--install-function-p 'find-file-noselect)
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

(when (emacs-fileio-builtins--install-function-p 'find-file)
  (defun find-file (filename &optional wildcards)
    "Phase D polyfill: visit FILENAME and make it the current buffer."
    (interactive
     (list (if (fboundp 'read-file-name)
               (read-file-name
                "Find file: "
                (and (boundp 'default-directory) default-directory)
                nil nil)
             (if (fboundp 'read-string)
                 (read-string "Find file: ")
               ""))
           nil))
    (ignore wildcards)
    (let ((buf (find-file-noselect filename)))
      (nelisp-ec-set-buffer buf)
      buf)))

(when (emacs-fileio-builtins--install-function-p 'save-buffer)
  (defun save-buffer (&optional arg)
    "Phase D polyfill: write the current buffer to its visited file.
ARG is accepted for API parity but ignored — the host's prefix-arg
disambiguation (= multiple backup levels) has no MVP equivalent.

Clears `(buffer-modified-p)' on success so the GUI mode-line `**'
indicator drops back to `--' after a save."
    (interactive "P")
    (ignore arg)
    (let* ((b (nelisp-ec-current-buffer))
           (f (and b (buffer-file-name b))))
      (cond
       ((null f)
        (signal 'error '("save-buffer: buffer is not visiting a file")))
       (t
        (write-region (nelisp-ec-point-min) (nelisp-ec-point-max) f)
        (when (fboundp 'set-buffer-modified-p)
          (set-buffer-modified-p nil))
        f)))))

(when (emacs-fileio-builtins--install-function-p 'write-file)
  (defun write-file (filename &optional confirm)
    "Phase D polyfill: write the current buffer to FILENAME and visit it.
CONFIRM is accepted for API parity but ignored (= no interactive
confirmation prompt in MVP).

Clears `(buffer-modified-p)' on success — `set-visited-file-name'
takes the buffer's contents to be in-sync with the new path."
    (interactive
     (list (if (fboundp 'read-file-name)
               (read-file-name
                "Write file: "
                (and (boundp 'default-directory) default-directory)
                nil nil)
             (if (fboundp 'read-string)
                 (read-string "Write file: ")
               ""))
           nil))
    (ignore confirm)
    (let ((abs (nelisp-ec-expand-file-name filename)))
      (write-region (nelisp-ec-point-min) (nelisp-ec-point-max) abs)
      (set-visited-file-name abs)
      (when (fboundp 'set-buffer-modified-p)
        (set-buffer-modified-p nil))
      abs)))

(when (emacs-fileio-builtins--install-function-p 'revert-buffer)
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
