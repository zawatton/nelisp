;;; files-standalone-buffer.el --- fallback buffer substrate for files.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Standalone NeLisp can expose file syscalls before the full
;; nelisp-emacs buffer bridge is loaded.  This tiny current-buffer model
;; keeps the first `find-file' / `insert' / `save-buffer' cycle useful
;; without forcing the heavier bootstrap path.

;;; Code:

(defvar files--buffer-string ""
  "Fallback current-buffer contents for the lightweight files shim.")

(defvar files--buffer-strings nil
  "Alist of (BUFFER . STRING) for lightweight fallback buffers.")

(defvar files--point 1
  "Fallback point for `files--buffer-string'.")

(defvar files--buffer-points nil
  "Alist of (BUFFER . POINT) for lightweight fallback buffers.")

(defvar files--buffer-modified-p nil
  "Fallback modified flag for `files--buffer-string'.")

(defvar files--buffer-modified-flags nil
  "Alist of (BUFFER . MODIFIED-P) for lightweight fallback buffers.")

(defvar files--current-file-name nil
  "Fallback visited file name for the lightweight files shim.")

(defvar files--buffer-file-names nil
  "Alist of (BUFFER . FILENAME) for lightweight file-visiting buffers.")

(defvar files--standalone-runtime-p (not (boundp 'emacs-version))
  "Non-nil when this fallback is loaded in standalone NeLisp.")

(defvar files--native-write-region
  (and (fboundp 'write-region) (symbol-function 'write-region))
  "Native `write-region' captured before this fallback overrides it.")

(defvar files--native-insert-file-contents
  (and (fboundp 'insert-file-contents)
       (symbol-function 'insert-file-contents))
  "Native `insert-file-contents' captured before this fallback overrides it.")

(defvar files--native-buffer-string
  (and (fboundp 'buffer-string) (symbol-function 'buffer-string))
  "Native `buffer-string' captured before this fallback overrides it.")

(defun files--expand-file-name (filename)
  "Expand FILENAME when the runtime supplies `expand-file-name'."
  (if (fboundp 'expand-file-name)
      (expand-file-name filename)
    filename))

(defun files--buffer-key (&optional buffer)
  "Return BUFFER or the current buffer when buffer primitives exist."
  (or buffer
      (and (fboundp 'current-buffer)
           (current-buffer))))

(defun files--buffer-file-cell (&optional buffer)
  "Return the visited-file alist cell for BUFFER."
  (let ((key (files--buffer-key buffer)))
    (and key (assq key files--buffer-file-names))))

(defun files--buffer-state-cell (alist &optional buffer)
  "Return BUFFER's cell in ALIST."
  (let ((key (files--buffer-key buffer)))
    (and key (assq key (symbol-value alist)))))

(defun files--buffer-live-or-unknown-p (buffer)
  "Return non-nil when BUFFER is live or liveness cannot be checked."
  (or (not buffer)
      (not (fboundp 'buffer-live-p))
      (buffer-live-p buffer)))

(defun files--live-buffer-cells (cells)
  "Return CELLS without entries whose buffer key is known dead."
  (let ((live nil))
    (dolist (cell cells)
      (when (files--buffer-live-or-unknown-p (car cell))
        (setq live (cons cell live))))
    (nreverse live)))

(defun files--prune-dead-buffer-state ()
  "Drop fallback state for buffers that are known dead."
  (setq files--buffer-file-names
        (files--live-buffer-cells files--buffer-file-names))
  (setq files--buffer-strings
        (files--live-buffer-cells files--buffer-strings))
  (setq files--buffer-points
        (files--live-buffer-cells files--buffer-points))
  (setq files--buffer-modified-flags
        (files--live-buffer-cells files--buffer-modified-flags)))

(defun files--set-buffer-state-cell (alist value &optional buffer)
  "Set BUFFER's ALIST cell to VALUE when buffer primitives exist."
  (let* ((key (files--buffer-key buffer))
         (cell (and key (assq key (symbol-value alist)))))
    (if cell
        (setcdr cell value)
      (when key
        (set alist (cons (cons key value) (symbol-value alist)))))))

(defun files--buffer-string-value (&optional buffer)
  "Return BUFFER's fallback text."
  (let ((cell (files--buffer-state-cell 'files--buffer-strings buffer)))
    (if cell (cdr cell) files--buffer-string)))

(defun files--set-buffer-string-value (string &optional buffer)
  "Set BUFFER's fallback text to STRING."
  (setq files--buffer-string string)
  (files--set-buffer-state-cell 'files--buffer-strings string buffer))

(defun files--buffer-point-value (&optional buffer)
  "Return BUFFER's fallback point."
  (let ((cell (files--buffer-state-cell 'files--buffer-points buffer)))
    (if cell (cdr cell) files--point)))

(defun files--set-buffer-point-value (point &optional buffer)
  "Set BUFFER's fallback point to POINT."
  (setq files--point point)
  (files--set-buffer-state-cell 'files--buffer-points point buffer))

(defun files--buffer-modified-value (&optional buffer)
  "Return BUFFER's fallback modified flag."
  (let ((cell (files--buffer-state-cell 'files--buffer-modified-flags
                                        buffer)))
    (if cell (cdr cell) files--buffer-modified-p)))

(defun files--set-buffer-modified-value (flag &optional buffer)
  "Set BUFFER's fallback modified flag to FLAG."
  (setq files--buffer-modified-p flag)
  (files--set-buffer-state-cell 'files--buffer-modified-flags flag buffer))

(defun files--buffer-file-name (&optional buffer)
  "Return BUFFER's fallback visited file name."
  (files--prune-dead-buffer-state)
  (cond
   ((and buffer (not (files--buffer-live-or-unknown-p buffer)))
    nil)
   (buffer
    (let ((cell (files--buffer-file-cell buffer)))
      (and cell (cdr cell))))
   (t
    (let ((key (files--buffer-key nil)))
      (if key
          (let ((cell (files--buffer-file-cell key)))
            (and cell (cdr cell)))
        files--current-file-name)))))

(defun files--set-buffer-file-name (buffer filename)
  "Record FILENAME as BUFFER's fallback visited file name."
  (let ((cell (files--buffer-file-cell buffer)))
    (if cell
        (setcdr cell filename)
      (let ((key (files--buffer-key buffer)))
        (when key
          (setq files--buffer-file-names
                (cons (cons key filename) files--buffer-file-names)))))))

(defun files--set-visited-file-name (filename &optional _no-query _along)
  "Set the current fallback buffer's visited file name to FILENAME."
  (setq files--current-file-name filename)
  (files--set-buffer-file-name nil filename)
  filename)

(defun files--file-name-equal-p (left right)
  "Return non-nil when LEFT and RIGHT name the same expanded file."
  (equal (files--expand-file-name left)
         (files--expand-file-name right)))

(defun files--visited-buffer-for-file (filename)
  "Return a live fallback buffer already visiting FILENAME."
  (files--prune-dead-buffer-state)
  (let ((found nil))
    (dolist (cell files--buffer-file-names found)
      (when (and (not found)
                 (cdr cell)
                 (files--file-name-equal-p (cdr cell) filename))
        (setq found (car cell))))))

(defun files--file-buffer-name (filename)
  "Return the preferred buffer name for FILENAME."
  (if (fboundp 'file-name-nondirectory)
      (file-name-nondirectory filename)
    filename))

(defun files--create-buffer-for-file (filename)
  "Create a fresh buffer for FILENAME when buffer primitives exist."
  (let ((name (files--file-buffer-name filename)))
    (cond
     ((fboundp 'generate-new-buffer)
      (generate-new-buffer name))
     ((fboundp 'get-buffer-create)
      (get-buffer-create name))
     (t nil))))

(defun files--buffer-for-file (filename)
  "Return or create a buffer for FILENAME when buffer primitives exist."
  (or (files--visited-buffer-for-file filename)
      (let ((name (files--file-buffer-name filename)))
        (cond
         ((and (fboundp 'get-buffer)
               (get-buffer name))
          (files--create-buffer-for-file filename))
         ((fboundp 'get-buffer-create)
          (get-buffer-create name))
         (t
          (files--create-buffer-for-file filename))))))

(defun files--string-length (string)
  "Return STRING length, treating nil as the empty string."
  (if string (length string) 0))

(defun files--concat-strings (strings)
  "Return STRINGS concatenated in order."
  (let ((text ""))
    (dolist (string strings)
      (setq text (concat text string)))
    text))

(defun files--clip-point (pos &optional buffer)
  "Clip POS to the fallback buffer's valid point range."
  (let ((min 1)
        (max (1+ (files--string-length
                  (files--buffer-string-value buffer)))))
    (cond
     ((< pos min) min)
     ((> pos max) max)
     (t pos))))

(defun files--buffer-substring (start end)
  "Return fallback buffer text from START to END using Emacs positions."
  (let* ((from (1- (files--clip-point start)))
         (to (1- (files--clip-point end)))
         (text (files--buffer-string-value)))
    (substring text from to)))

(defun files--install-fallback-function-p (symbol)
  "Return non-nil when fallback SYMBOL should be installed."
  (or files--standalone-runtime-p
      (not (fboundp symbol))))

(when (files--install-fallback-function-p 'point-min)
  (defun point-min ()
    "Return the first valid fallback buffer position."
    1))

(when (files--install-fallback-function-p 'point-max)
  (defun point-max ()
    "Return one past the last fallback buffer position."
    (1+ (files--string-length (files--buffer-string-value)))))

(when (files--install-fallback-function-p 'point)
  (defun point ()
    "Return fallback point."
    (files--buffer-point-value)))

(when (files--install-fallback-function-p 'goto-char)
  (defun goto-char (position)
    "Set fallback point to POSITION, clipped to the current buffer."
    (files--set-buffer-point-value
     (files--clip-point
      (if (and (fboundp 'markerp) (markerp position))
          (marker-position position)
        position)))))

(when (files--install-fallback-function-p 'buffer-string)
  (defun buffer-string ()
    "Return the fallback current buffer contents."
    (files--buffer-string-value)))

(when (files--install-fallback-function-p 'erase-buffer)
  (defun erase-buffer ()
    "Erase the fallback current buffer."
    (files--set-buffer-string-value "")
    (files--set-buffer-point-value 1)
    (files--set-buffer-modified-value t)
    nil))

(when (files--install-fallback-function-p 'insert)
  (defun insert (&rest strings)
    "Insert STRINGS at fallback point."
    (let ((text (files--concat-strings strings)))
      (let* ((buffer-text (files--buffer-string-value))
             (point (files--buffer-point-value))
             (pos (1- (files--clip-point point)))
             (before (substring buffer-text 0 pos))
             (after (substring buffer-text pos)))
        (files--set-buffer-string-value (concat before text after))
        (files--set-buffer-point-value (+ point (length text)))
        (files--set-buffer-modified-value t)))
    nil))

(when (files--install-fallback-function-p 'buffer-modified-p)
  (defun buffer-modified-p (&optional _buffer)
    "Return the fallback modified flag."
    (files--buffer-modified-value)))

(when (files--install-fallback-function-p 'set-buffer-modified-p)
  (defun set-buffer-modified-p (flag)
    "Set the fallback modified flag to FLAG."
    (files--set-buffer-modified-value flag)))

(defun files--read-file-text (filename)
  "Return FILENAME contents as a string, or nil when no reader exists."
  (cond
   ((fboundp 'nelisp--syscall-read-file)
    (nelisp--syscall-read-file filename))
   ((and files--native-insert-file-contents files--native-buffer-string)
    (with-temp-buffer
      (funcall files--native-insert-file-contents filename)
      (funcall files--native-buffer-string)))
   (t nil)))

(when (files--install-fallback-function-p 'insert-file-contents)
  (defun insert-file-contents (filename &optional _visit _beg _end _replace)
    "Insert FILENAME into the fallback current buffer."
    (let ((text (files--read-file-text filename)))
      (unless text
        (signal 'file-error (list "Cannot read file" filename)))
      (insert text)
      (list filename (length text)))))

(defun files--region-text (start end)
  "Return the fallback text selected by START and END."
  (if (stringp start)
      start
    (files--buffer-substring (or start (point-min))
                             (or end (point-max)))))

(defun files--write-file-text (filename text append visit lockname mustbenew)
  "Write TEXT to FILENAME through the best available backend."
  (cond
   ((fboundp 'nl-syscall-write-file)
    (nl-syscall-write-file filename text (if append 1 0)))
   ((and (fboundp 'nl-write-file) (not append))
    (nl-write-file filename text))
   (files--native-write-region
    (funcall files--native-write-region text nil filename append visit
             lockname mustbenew))
   (t
    (signal 'file-error (list "Cannot write file" filename)))))

(when (files--install-fallback-function-p 'write-region)
  (defun write-region
      (start end filename &optional append visit lockname mustbenew)
    "Write text between START and END to FILENAME."
    (files--write-file-text filename (files--region-text start end)
                            append visit lockname mustbenew)))

(defun files--current-buffer-if-available ()
  "Return the current buffer when buffer primitives exist."
  (and (fboundp 'current-buffer) (current-buffer)))

(defun files--set-buffer-if-available (buffer)
  "Select BUFFER when `set-buffer' is available."
  (when (and buffer (fboundp 'set-buffer))
    (set-buffer buffer)))

(defun files--file-readable-or-unknown-p (filename)
  "Return non-nil if FILENAME exists, or no existence predicate exists."
  (or (not (fboundp 'file-exists-p))
      (file-exists-p filename)))

(defun files--insert-file-if-readable (filename)
  "Insert FILENAME when it exists or existence cannot be checked."
  (when (files--file-readable-or-unknown-p filename)
    (insert-file-contents filename)))

(defun files--load-file-into-buffer (filename buffer)
  "Load FILENAME into BUFFER or into the fallback current buffer."
  (if buffer
      (progn
        (files--set-buffer-if-available buffer)
        (erase-buffer)
        (files--insert-file-if-readable filename))
    (erase-buffer)
    (files--insert-file-if-readable filename)))

(defun files-standalone-find-file-noselect (filename)
  "Return a buffer visiting FILENAME, or the fallback current file name."
  (files--prune-dead-buffer-state)
  (let* ((abs (files--expand-file-name filename))
         (existing-buffer (files--visited-buffer-for-file abs))
         (buffer (or existing-buffer (files--buffer-for-file abs)))
         (old-buffer (files--current-buffer-if-available)))
    (unless existing-buffer
      (files--load-file-into-buffer abs buffer)
      (files--set-visited-file-name abs)
      (set-buffer-modified-p nil)
      (files--set-buffer-if-available old-buffer))
    (or buffer abs)))

(defun files-standalone-find-file (filename)
  "Visit FILENAME and return its buffer when one can be created."
  (let ((buffer (files-standalone-find-file-noselect filename)))
    (when (and buffer (not (stringp buffer)) (fboundp 'set-buffer))
      (set-buffer buffer))
    buffer))

(defun files-standalone-save-buffer ()
  "Write the fallback current buffer to its visited file when possible."
  (let ((filename (and (fboundp 'buffer-file-name)
                       (buffer-file-name))))
    (if filename
        (progn
          (write-region (point-min) (point-max) filename)
          (set-buffer-modified-p nil)
          filename)
      nil)))

(defun files--save-current-buffer-if-needed ()
  "Save the current fallback buffer when it visits a modified file."
  (let ((filename (and (fboundp 'buffer-file-name)
                       (buffer-file-name))))
    (if (and filename
             (or (not (fboundp 'buffer-modified-p))
                 (buffer-modified-p)))
        (progn
          (files-standalone-save-buffer)
          t)
      nil)))

(defun files--buffer-modified-for-save-p (buffer)
  "Return non-nil when BUFFER should be saved."
  (or (files--buffer-modified-value buffer)
      (and buffer
           (fboundp 'buffer-modified-p)
           (buffer-modified-p buffer))))

(defun files--save-buffer-entry-if-needed (entry)
  "Save file-visiting fallback buffer ENTRY when it is modified."
  (let ((buffer (car entry)))
    (when (and (cdr entry)
               (files--buffer-modified-for-save-p buffer))
      (files--set-buffer-if-available buffer)
      (files-standalone-save-buffer)
      t)))

(defun files--save-buffer-entries-if-needed ()
  "Save all modified file-visiting fallback buffer entries."
  (files--prune-dead-buffer-state)
  (let ((saved nil)
        (old-buffer (files--current-buffer-if-available)))
    (unwind-protect
        (dolist (entry files--buffer-file-names saved)
          (when (files--save-buffer-entry-if-needed entry)
            (setq saved t)))
      (files--set-buffer-if-available old-buffer))))

(defun files-standalone-write-file (filename)
  "Write the fallback current buffer to FILENAME and visit it."
  (set-visited-file-name (files--expand-file-name filename))
  (files-standalone-save-buffer))

(defun files-standalone-find-file-read-only (filename &optional _wildcards)
  "Visit FILENAME and mark the selected fallback buffer read-only."
  (let ((buffer (files-standalone-find-file filename)))
    (setq buffer-read-only t)
    buffer))

(defun files-standalone-find-alternate-file (filename &optional _wildcards)
  "Visit FILENAME in place of the current fallback buffer."
  (files-standalone-find-file filename))

(defun files-standalone-save-some-buffers ()
  "Save modified fallback file-visiting buffers."
  (if files--buffer-file-names
      (files--save-buffer-entries-if-needed)
    (files--save-current-buffer-if-needed)))

(defun files-standalone-insert-file (filename)
  "Insert the contents of FILENAME at fallback point."
  (insert-file-contents filename)
  filename)

(defun files-standalone-list-directory (dirname)
  "Return the names in DIRNAME."
  (cond
   ((fboundp 'nelisp-ec-directory-files)
    (nelisp-ec-directory-files dirname))
   ((fboundp 'directory-files)
    (directory-files dirname))
   ((fboundp 'nelisp--syscall-readdir)
    (cdr (nelisp--syscall-readdir dirname)))
   (t nil)))

;; --- file-system predicates / mkdir (standalone has no stat / mkdir syscall) -
(when (files--install-fallback-function-p 'file-exists-p)
  (defun file-exists-p (filename)
    "Approximate existence check for the standalone reader, which exposes no
stat syscall: a non-empty read counts as existing.  Adequate for the
key/value store files this substrate serves (they are never legitimately
empty -- they always hold at least \"{}\")."
    (let ((s (condition-case nil
                 (and (fboundp 'rdf) (rdf (expand-file-name filename)))
               (error nil))))
      (and (stringp s) (> (length s) 0)))))
(when (files--install-fallback-function-p 'file-readable-p)
  (defun file-readable-p (filename) (file-exists-p filename)))
(when (files--install-fallback-function-p 'file-regular-p)
  (defun file-regular-p (filename)
    "No directory detection on the standalone reader, so an existing path is a
regular file."
    (file-exists-p filename)))
(when (files--install-fallback-function-p 'char-before)
  (defun char-before (&optional pos)
    "Character before POS (or point) in the fallback current buffer, or nil."
    (let* ((p (or pos (files--buffer-point-value)))
           (content (files--buffer-string-value))
           (idx (- p 2)))
      (if (and (>= idx 0) (< idx (files--string-length content)))
          (aref content idx) nil))))
(when (files--install-fallback-function-p 'char-after)
  (defun char-after (&optional pos)
    "Character at POS (or point) in the fallback current buffer, or nil."
    (let* ((p (or pos (files--buffer-point-value)))
           (content (files--buffer-string-value))
           (idx (- p 1)))
      (if (and (>= idx 0) (< idx (files--string-length content)))
          (aref content idx) nil))))
(when (files--install-fallback-function-p 'following-char)
  (defun following-char ()
    "Character after point as a number, or 0 at end of the fallback buffer."
    (or (char-after) 0)))
(when (files--install-fallback-function-p 'preceding-char)
  (defun preceding-char ()
    "Character before point as a number, or 0 at start of the fallback buffer."
    (or (char-before) 0)))
(when (files--install-fallback-function-p 'file-directory-p)
  (defun file-directory-p (filename)
    "Heuristic directory test for the standalone reader (no stat syscall): a
path ending in a slash -- e.g. the result of `file-name-directory' -- is
treated as an existing directory; a path that exists as a regular file is not."
    (cond
     ((not (stringp filename)) nil)
     ((let ((n (length filename))) (and (> n 0) (eq (aref filename (1- n)) ?/))) t)
     (t nil))))
(when (files--install-fallback-function-p 'make-directory)
  (defun make-directory (_dir &optional _parents)
    "No-op: the standalone reader has no mkdir syscall; the parent directory is
assumed to already exist."
    nil))

;; Bridge the file reader to the standalone reader's `rdf' primitive (the only
;; file-read entry point baked into target/nelisp).  files--read-file-text
;; prefers `nelisp--syscall-read-file'; provide it on top of `rdf'.  `rdf'
;; returns "" for a missing OR empty file, so map empty -> nil to let
;; insert-file-contents signal `file-error' for a genuinely absent file.
(when (and (fboundp 'rdf) (fboundp 'nelisp--write-stderr-line))
  ;; The reader's baked nelisp--syscall-read-file throws uncatchably when
  ;; called with a path; redefine it on top of rdf (the working file-read
  ;; primitive).  Gated on the standalone marker so host Emacs is untouched.
  (defun nelisp--syscall-read-file (filename)
    (let ((s (rdf (expand-file-name filename))))
      (and (stringp s) (> (length s) 0) s))))

(provide 'files-standalone-buffer)

;;; files-standalone-buffer.el ends here
