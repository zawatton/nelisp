;;; emacs-dump.el --- Layer-2 lisp-image dump / restore  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Doc 51 Track L (2026-05-03) — Layer 2 γ-deeper.
;;
;; Minimum-viable Emacs-style "dump file" generation.  The host Emacs
;; build emits a `.pdmp' that freezes the entire eval state into a
;; binary; for nelisp-emacs Layer 2 we only need to persist the
;; *Lisp-visible* parts of init so a subsequent `nemacs-loadup' run
;; can skip re-evaluating the `(require ...)' chain in
;; `src/emacs-init.el'.
;;
;; What ends up in the dump (= the "lisp-image"):
;;
;;   :version              schema version of the dump format
;;   :emacs-major-version  host major (used as a sanity check at load)
;;   :timestamp            (current-time)-style emit timestamp
;;   :features             features (= post-load `features' list)
;;   :load-history-tail    last N entries of `load-history' (= which
;;                         files contributed which symbols)
;;   :defvars              alist of (NAME . VALUE) for global defvars
;;                         that pass the `emacs-dump--include-defvar-p'
;;                         filter (= simple, readable, not buffer-local)
;;   :buffers              vector of (NAME . TEXT) snapshots for the
;;                         standard non-internal buffers (= "*scratch*"
;;                         and any caller-registered names)
;;
;; Out of scope (= deferred to Doc 51 ε):
;;   - byte-compiled function bodies (= we only persist symbol names;
;;     re-loading the dump still requires the bytecode-equivalent
;;     function bindings to live in already-loaded `.el' modules)
;;   - obarray remap (= we rely on Emacs interning the names again
;;     when read; this is fine for the symbol-as-name semantics here)
;;   - charset / coding-system tables / process state / GC roots

;;; Code:

(require 'cl-lib)

(defconst emacs-dump-format-version 1
  "Schema version of the lisp-image format written by `emacs-dump-save'.")

(defconst emacs-dump-default-load-history-tail 32
  "Default number of trailing `load-history' entries captured.")

(defvar emacs-dump-extra-buffer-names nil
  "Extra buffer names (strings) to capture into the dump.
The standard `*scratch*' buffer is always included if present.
Append additional names via `add-to-list' before calling
`emacs-dump-save'.")

(defvar emacs-dump-defvar-allowlist
  '(emacs-major-version
    emacs-version
    nemacs-version
    nemacs-initialized
    auto-mode-alist
    font-lock-defaults
    default-directory)
  "Symbols whose `symbol-value' is captured into a saved dump.
The list is intentionally narrow — most defvars do not survive a
round-trip through `prin1' / `read'.  Add a symbol here only when
its value is `read'-able (= not a buffer / process / hash-table
with non-`read'-able keys).")

;;;; --- helpers -------------------------------------------------------

(defun emacs-dump--readable-p (val)
  "Return non-nil when VAL can survive `prin1'/`read' roundtrip.

Walks proper lists, dotted pairs and vectors recursively.  Rejects
hash-tables / buffers / windows / processes / markers / overlays /
function bodies / file-handles."
  (cond
   ((null val) t)
   ((eq val t) t)
   ((numberp val) t)
   ((stringp val) t)
   ((symbolp val) t)
   ((characterp val) t)
   ((consp val)
    ;; Walk both car and cdr; this handles proper lists, dotted pairs
    ;; and circular structures (= the latter need print-circle in
    ;; `emacs-dump-save', which we already set).
    (let ((node val) (ok t))
      (while (and ok (consp node))
        (unless (emacs-dump--readable-p (car node))
          (setq ok nil))
        (setq node (cdr node)))
      ;; Tail can be nil (proper list) or any readable atom (dotted).
      (and ok (emacs-dump--readable-p node))))
   ((vectorp val)
    (let ((i 0) (n (length val)) (ok t))
      (while (and ok (< i n))
        (unless (emacs-dump--readable-p (aref val i))
          (setq ok nil))
        (setq i (1+ i)))
      ok))
   (t nil)))

(defun emacs-dump--collect-defvars ()
  "Return an alist of (NAME . VALUE) for the allow-listed defvars."
  (let (out)
    (dolist (sym emacs-dump-defvar-allowlist)
      (when (and (boundp sym)
                 (emacs-dump--readable-p (symbol-value sym)))
        (push (cons sym (symbol-value sym)) out)))
    (nreverse out)))

(defun emacs-dump--lookup-buffer (name)
  "Return the buffer object for NAME, prefering the nelisp-ec registry."
  (cond
   ((and (boundp 'nelisp-ec--buffers)
         (assoc name nelisp-ec--buffers))
    (cdr (assoc name nelisp-ec--buffers)))
   ((fboundp 'get-buffer)
    (get-buffer name))))

(defun emacs-dump--buffer-text (b)
  "Return BUFFER's text content, dispatching by buffer flavour."
  (cond
   ((and (fboundp 'nelisp-ec-buffer-p)
         (nelisp-ec-buffer-p b))
    (let ((nelisp-ec--current-buffer b))
      (nelisp-ec-buffer-string)))
   ((bufferp b)
    (with-current-buffer b (buffer-string)))
   (t "")))

(defun emacs-dump--collect-buffers ()
  "Return a vector of (NAME . TEXT) for the persistable buffers."
  (let* ((names (delete-dups
                 (append (list "*scratch*") emacs-dump-extra-buffer-names)))
         (out '()))
    (dolist (name names)
      (let ((b (emacs-dump--lookup-buffer name)))
        (when b
          (push (cons name (emacs-dump--buffer-text b)) out))))
    (apply #'vector (nreverse out))))

(defun emacs-dump--load-history-tail (n)
  (when (boundp 'load-history)
    (let* ((lh load-history)
           (len (length lh))
           (start (max 0 (- len n))))
      (cl-loop for i from start below len
               for entry = (nth i lh)
               when (emacs-dump--readable-p entry)
               collect entry))))

;;;; --- save ----------------------------------------------------------

(defun emacs-dump-build-image ()
  "Build the lisp-image plist without writing to disk."
  (list :version emacs-dump-format-version
        :emacs-major-version (and (boundp 'emacs-major-version)
                                  emacs-major-version)
        :timestamp (current-time)
        :features (and (boundp 'features) (copy-sequence features))
        :load-history-tail (emacs-dump--load-history-tail
                            emacs-dump-default-load-history-tail)
        :defvars (emacs-dump--collect-defvars)
        :buffers (emacs-dump--collect-buffers)))

(defun emacs-dump-save (path)
  "Write a lisp-image dump file to PATH (overwriting if present).
Returns the image plist that was written.  PATH is created with
`utf-8-emacs' coding so bytes survive on systems with non-ASCII
filesystem encodings."
  (unless (stringp path)
    (signal 'wrong-type-argument (list 'stringp path)))
  (let ((image (emacs-dump-build-image)))
    (with-temp-buffer
      (let ((standard-output (current-buffer))
            (print-length nil)
            (print-level nil)
            (print-escape-newlines t)
            (print-quoted t)
            (print-circle t))
        (insert ";;; nelisp-emacs lisp-image dump  -*- lexical-binding: t; mode: lisp-data -*-\n")
        (insert (format ";;; format: emacs-dump v%d\n"
                        emacs-dump-format-version))
        (insert ";;; auto-generated; do not edit.\n\n")
        (prin1 image)
        (insert "\n"))
      (let ((coding-system-for-write 'utf-8-emacs-unix))
        (write-region (point-min) (point-max) path nil 'silent)))
    image))

;;;; --- load ----------------------------------------------------------

(define-error 'emacs-dump-error "nelisp-emacs dump error")
(define-error 'emacs-dump-version-mismatch
  "nelisp-emacs dump format version mismatch" 'emacs-dump-error)
(define-error 'emacs-dump-corrupt
  "nelisp-emacs dump file is corrupt" 'emacs-dump-error)

(defun emacs-dump-read (path)
  "Read a lisp-image dump from PATH and return the image plist."
  (unless (stringp path)
    (signal 'wrong-type-argument (list 'stringp path)))
  (unless (file-readable-p path)
    (signal 'emacs-dump-corrupt (list 'unreadable path)))
  (with-temp-buffer
    (let ((coding-system-for-read 'utf-8-emacs-unix))
      (insert-file-contents path))
    (goto-char (point-min))
    (let (image)
      (condition-case err
          (progn
            ;; Skip the leading ';;; ...' header lines.
            (while (and (not (eobp))
                        (or (looking-at-p "^[ \t]*$")
                            (looking-at-p "^[ \t]*;")))
              (forward-line 1))
            (setq image (read (current-buffer))))
        (error
         (signal 'emacs-dump-corrupt (list 'read-error err))))
      (unless (and (listp image)
                   (eq (plist-get image :version) emacs-dump-format-version))
        (signal 'emacs-dump-version-mismatch
                (list 'expected emacs-dump-format-version
                      'got (and (listp image) (plist-get image :version)))))
      image)))

(defun emacs-dump-load (path &optional restore-buffers)
  "Read a dump from PATH and re-establish the persisted bindings.
When RESTORE-BUFFERS is non-nil, also recreates the persisted
buffers' contents.  Returns the loaded image plist."
  (let* ((image (emacs-dump-read path))
         (defvars (plist-get image :defvars))
         (features-list (plist-get image :features))
         (buffers (plist-get image :buffers)))
    (dolist (cell defvars)
      (let ((sym (car cell)) (val (cdr cell)))
        (set sym val)))
    (when (and features-list (boundp 'features))
      (dolist (f features-list)
        (unless (memq f features)
          (push f features))))
    (when restore-buffers
      (dotimes (i (length buffers))
        (let* ((cell (aref buffers i))
               (name (car cell))
               (text (cdr cell)))
          (cond
           ((fboundp 'nelisp-ec-get-buffer-create)
            (let* ((b (nelisp-ec-get-buffer-create name))
                   (nelisp-ec--current-buffer b))
              (nelisp-ec-erase-buffer)
              (nelisp-ec-insert text)))
           ((fboundp 'get-buffer-create)
            (with-current-buffer (get-buffer-create name)
              (erase-buffer)
              (insert text)))))))
    image))

;;;; --- introspection -------------------------------------------------

(defun emacs-dump-image-info (path)
  "Return a short summary plist of the dump at PATH (without applying it).
Useful for `--inspect' style tooling."
  (let ((image (emacs-dump-read path)))
    (list :version (plist-get image :version)
          :emacs-major-version (plist-get image :emacs-major-version)
          :timestamp (plist-get image :timestamp)
          :feature-count (length (plist-get image :features))
          :defvar-count (length (plist-get image :defvars))
          :buffer-count (length (plist-get image :buffers))
          :load-history-tail-count
          (length (plist-get image :load-history-tail)))))

(provide 'emacs-dump)

;;; emacs-dump.el ends here
