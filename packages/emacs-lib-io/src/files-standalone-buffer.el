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

;; --- file-system predicates -------------------------------------------------
;; The reader exposes `nelisp--syscall-stat' (path -> absent/file/directory)
;; and access(2) via `nelisp--syscall-path-int'; these predicates use them when
;; present and fall back to a read-based approximation otherwise.
(defconst files--syscall-access 21 "Linux x86_64 access(2) syscall number.")
(defconst files--ok-exist 0 "access(2) F_OK: test for existence.")
(defconst files--ok-exec 1 "access(2) X_OK: test for execute/search permission.")
(defconst files--ok-write 2 "access(2) W_OK: test for write permission.")
(defconst files--ok-read 4 "access(2) R_OK: test for read permission.")

(defun files--rdf-nonempty-p (filename)
  "Fallback existence check: a non-empty `rdf' read counts as existing."
  (let ((s (condition-case nil
               (and (fboundp 'rdf) (rdf (files--expand-file-name filename)))
             (error nil))))
    (and (stringp s) (> (length s) 0))))

(defun files--access-ok-p (filename mode)
  "Return non-nil when access(2) on FILENAME with MODE succeeds (rc 0).
Falls back to a read-based existence check when the reader exposes no
`nelisp--syscall-path-int'."
  (if (fboundp 'nelisp--syscall-path-int)
      (= 0 (nelisp--syscall-path-int files--syscall-access
                                     (files--expand-file-name filename) mode))
    (files--rdf-nonempty-p filename)))

(when (files--install-fallback-function-p 'file-exists-p)
  (defun file-exists-p (filename)
    "Return non-nil if FILENAME exists, via access(2) F_OK when available."
    (files--access-ok-p filename files--ok-exist)))
(when (files--install-fallback-function-p 'file-readable-p)
  (defun file-readable-p (filename)
    "Return non-nil if FILENAME is readable, via access(2) R_OK."
    (files--access-ok-p filename files--ok-read)))
(when (files--install-fallback-function-p 'file-writable-p)
  (defun file-writable-p (filename)
    "Return non-nil if FILENAME is writable, via access(2) W_OK."
    (files--access-ok-p filename files--ok-write)))
(when (files--install-fallback-function-p 'file-executable-p)
  (defun file-executable-p (filename)
    "Return non-nil if FILENAME is executable/searchable, via access(2) X_OK."
    (files--access-ok-p filename files--ok-exec)))
(when (files--install-fallback-function-p 'file-accessible-directory-p)
  (defun file-accessible-directory-p (filename)
    "Return non-nil if FILENAME is a directory that can be searched."
    (and (file-directory-p filename) (file-executable-p filename))))
(when (files--install-fallback-function-p 'file-regular-p)
  (defun file-regular-p (filename)
    "Return non-nil if FILENAME exists and is not a directory."
    (and (file-exists-p filename) (not (file-directory-p filename)))))

;; --- stat(2) metadata: file-modes / file-attributes -------------------------
;; Built on the reader's generic stat primitives (stat(2) + read struct stat
;; fields).  Linux x86_64 struct stat byte offsets:
;; `ptr-read-u64' is a NeLisp standalone-reader primitive (absent from host
;; Emacs); declared so byte-compilation stays warning-free.
(declare-function ptr-read-u64 "ext:nelisp-reader")
(defconst files--stat-off-dev 0 "struct stat st_dev offset.")
(defconst files--stat-off-ino 8 "struct stat st_ino offset.")
(defconst files--stat-off-nlink 16 "struct stat st_nlink offset.")
(defconst files--stat-off-mode 24 "struct stat st_mode offset.")
(defconst files--stat-off-uid 28 "struct stat st_uid offset.")
(defconst files--stat-off-gid 32 "struct stat st_gid offset.")
(defconst files--stat-off-size 48 "struct stat st_size offset.")
(defconst files--stat-off-atime 72 "struct stat st_atim.tv_sec offset.")
(defconst files--stat-off-atime-nsec 80 "struct stat st_atim.tv_nsec offset.")
(defconst files--stat-off-mtime 88 "struct stat st_mtim.tv_sec offset.")
(defconst files--stat-off-mtime-nsec 96 "struct stat st_mtim.tv_nsec offset.")
(defconst files--stat-off-ctime 104 "struct stat st_ctim.tv_sec offset.")
(defconst files--stat-off-ctime-nsec 112 "struct stat st_ctim.tv_nsec offset.")
(defconst files--nsec-per-sec 1000000000 "Nanoseconds per second (timestamp HZ).")

(defun files--stat-field (filename offset)
  "Return the struct stat u64 at OFFSET for FILENAME, or nil when stat fails
or the reader provides no `nelisp--syscall-stat-field'."
  (when (fboundp 'nelisp--syscall-stat-field)
    (let ((v (nelisp--syscall-stat-field (files--expand-file-name filename)
                                         offset)))
      (and (>= v 0) v))))

(defun files--stat-buf (filename)
  "stat(2) FILENAME into a fresh struct stat buffer and return its pointer,
or nil when stat fails or the reader provides no `nelisp--syscall-stat-buf'.
Read individual fields from the pointer with `ptr-read-u64' -- one stat for
the whole struct."
  (when (fboundp 'nelisp--syscall-stat-buf)
    (let ((p (nelisp--syscall-stat-buf (files--expand-file-name filename))))
      (and (> p 0) p))))

(defun files--lstat-buf (filename)
  "lstat(2) FILENAME (without following symlinks) into a struct stat buffer and
return its pointer, or nil on failure / when the reader provides no
`nelisp--syscall-lstat-buf'."
  (when (fboundp 'nelisp--syscall-lstat-buf)
    (let ((p (nelisp--syscall-lstat-buf (files--expand-file-name filename))))
      (and (> p 0) p))))

(defun files--lstat-field (filename offset)
  "Return the lstat(2) struct stat u64 at OFFSET for FILENAME (without
following a symbolic link), or nil on failure."
  (let ((buf (files--lstat-buf filename)))
    (and buf (ptr-read-u64 buf offset))))

(defun files--stat-time (buf sec-offset nsec-offset)
  "Return a (TICKS . HZ) Lisp timestamp from the struct stat at BUF, reading
seconds at SEC-OFFSET and nanoseconds at NSEC-OFFSET (HZ = 1e9)."
  (let ((sec (ptr-read-u64 buf sec-offset))
        (nsec (logand (ptr-read-u64 buf nsec-offset) #xFFFFFFFF)))
    (cons (+ (* sec files--nsec-per-sec) nsec) files--nsec-per-sec)))

;; struct statx (statx(2)) offsets / flags for birth time.
(defconst files--statx-flag-nofollow 256 "AT_SYMLINK_NOFOLLOW (statx flags).")
(defconst files--statx-mask-btime 2048 "STATX_BTIME bit in struct statx stx_mask.")
(defconst files--statx-off-mask 0 "struct statx stx_mask offset.")
(defconst files--statx-off-btime-sec 80 "struct statx stx_btime.tv_sec offset.")
(defconst files--statx-off-btime-nsec 88 "struct statx stx_btime.tv_nsec offset.")

(defun files--statx-btime (filename)
  "Return the birth (creation) time of FILENAME as a (TICKS . HZ) timestamp via
statx(2), or nil when statx or birth time is unavailable (some filesystems do
not record it)."
  (when (fboundp 'nelisp--syscall-statx-buf)
    (let ((buf (nelisp--syscall-statx-buf (files--expand-file-name filename)
                                          files--statx-flag-nofollow)))
      (when (and (integerp buf) (> buf 0)
                 (> (logand (ptr-read-u64 buf files--statx-off-mask)
                            files--statx-mask-btime)
                    0))
        (files--stat-time buf files--statx-off-btime-sec
                          files--statx-off-btime-nsec)))))

(defun files--readlink (filename)
  "Return the symbolic-link target of FILENAME as a string, or nil when it is
not a symlink / on error / when the reader provides no
`nelisp--syscall-readlink'."
  (and (fboundp 'nelisp--syscall-readlink)
       (nelisp--syscall-readlink (files--expand-file-name filename))))

(when (files--install-fallback-function-p 'file-symlink-p)
  (defun file-symlink-p (filename)
    "Return the target of symbolic link FILENAME (a string), or nil when
FILENAME is not a symbolic link (via readlink(2))."
    (files--readlink filename)))

(defun files--truename-walk (path depth)
  "Resolve symbolic links in absolute PATH component by component.
DEPTH guards against symlink loops; relative link targets resolve against
the directory built so far."
  (if (> depth 100)
      path
    (let ((true ""))
      (dolist (comp (split-string path "/" t))
        (let* ((cand (concat true "/" comp))
               (link (files--readlink cand)))
          (setq true
                (if link
                    (files--truename-walk
                     (files--expand-file-name
                      (if (and (> (length link) 0) (eq (aref link 0) ?/))
                          link
                        (concat true "/" link)))
                     (1+ depth))
                  cand))))
      (if (= (length true) 0) "/" true))))

(when (files--install-fallback-function-p 'file-truename)
  (defun file-truename (filename &optional _counter _prev-dirs)
    "Return the canonical name of FILENAME, resolving all symbolic links via
readlink(2), component by component (interior links included).  COUNTER and
PREV-DIRS are accepted for call compatibility and ignored.  `..' is only
collapsed as far as the reader's `expand-file-name' does."
    (files--truename-walk (files--expand-file-name filename) 0)))

(defun files--toggle-case (str)
  "Return STR with the case of each ASCII letter toggled.
Built with `concat'/`char-to-string' since reader strings are immutable."
  (let ((out "") (i 0) (n (length str)))
    (while (< i n)
      (let ((c (aref str i)))
        (setq out (concat out (char-to-string
                               (cond ((and (>= c ?a) (<= c ?z)) (- c 32))
                                     ((and (>= c ?A) (<= c ?Z)) (+ c 32))
                                     (t c))))))
      (setq i (1+ i)))
    out))

(defun files--has-letter-p (str)
  "Return non-nil if STR contains an ASCII letter."
  (let ((i 0) (n (length str)) (found nil))
    (while (and (< i n) (not found))
      (let ((c (aref str i)))
        (when (or (and (>= c ?a) (<= c ?z)) (and (>= c ?A) (<= c ?Z)))
          (setq found t)))
      (setq i (1+ i)))
    found))

(when (files--install-fallback-function-p 'file-name-case-insensitive-p)
  (defun file-name-case-insensitive-p (filename)
    "Return t if FILENAME is on a case-insensitive filesystem.
Probe: toggle the case of an existing path component and check whether the
toggled name refers to the same file (same inode).  Returns nil on a
case-sensitive filesystem (the Linux default).  FILENAME need not exist; the
nearest existing ancestor with a letter in its name is probed."
    (let ((path (directory-file-name (files--expand-file-name filename)))
          (depth 0))
      (catch 'done
        (while (and (stringp path) (> (length path) 1) (< depth 64))
          (let ((base (file-name-nondirectory path)))
            (when (and (files--has-letter-p base) (file-exists-p path))
              (let* ((dir (file-name-directory path))
                     (toggled (concat dir (files--toggle-case base)))
                     (ino1 (files--stat-field path files--stat-off-ino))
                     (ino2 (and (file-exists-p toggled)
                                (files--stat-field toggled files--stat-off-ino))))
                (throw 'done (and ino1 ino2 (= ino1 ino2))))))
          (setq path (directory-file-name (file-name-directory path))
                depth (1+ depth)))
        nil))))

(defun files--mode-rwx (bits)
  "Return the 3-char rwx string for the low 3 BITS."
  (concat (if (= (logand bits 4) 0) "-" "r")
          (if (= (logand bits 2) 0) "-" "w")
          (if (= (logand bits 1) 0) "-" "x")))

(defun files--mode-string (mode)
  "Return the 10-char ls-style mode string for integer MODE (st_mode).
Omits the setuid/setgid/sticky special display."
  (let ((ifmt (logand mode #o170000)))
    (concat (cond ((= ifmt #o040000) "d")
                  ((= ifmt #o120000) "l")
                  ((= ifmt #o020000) "c")
                  ((= ifmt #o060000) "b")
                  ((= ifmt #o010000) "p")
                  ((= ifmt #o140000) "s")
                  (t "-"))
            (files--mode-rwx (logand (ash mode -6) 7))
            (files--mode-rwx (logand (ash mode -3) 7))
            (files--mode-rwx (logand mode 7)))))

(when (files--install-fallback-function-p 'file-modes)
  (defun file-modes (filename &optional flag)
    "Return the permission bits of FILENAME (st_mode masked to #o7777), or nil.
With FLAG `nofollow', use lstat(2) -- the symbolic link's own mode -- instead
of following the link."
    (let ((m (if (eq flag 'nofollow)
                 (files--lstat-field filename files--stat-off-mode)
               (files--stat-field filename files--stat-off-mode))))
      (and m (logand m #o7777)))))

(when (files--install-fallback-function-p 'file-attributes)
  (defun file-attributes (filename &optional _id-format)
    "Return the stat(2) attribute list of FILENAME, or nil when it is absent.
Order matches Emacs: (TYPE NLINK UID GID ATIME MTIME CTIME SIZE MODES
GID-CHANGE INODE DEVICE).  TYPE is the link target string for a symbolic
link, t for a directory, nil otherwise.  Uses lstat(2) (the link itself) with
a single stat via `nelisp--syscall-lstat-buf' + `ptr-read-u64'.  Times are
(TICKS . HZ) nanosecond timestamps.  A non-standard 13th element holds the
birth (creation) time as a (TICKS . HZ) timestamp via statx(2), or nil when
unavailable.  Degraded: the MODES string omits setuid/setgid/sticky display.
ID-FORMAT is ignored."
    (let ((buf (or (files--lstat-buf filename) (files--stat-buf filename))))
      (when buf
        (let* ((mode (ptr-read-u64 buf files--stat-off-mode))
               (ifmt (logand mode #o170000)))
          (list
           (cond ((= ifmt #o120000) (or (files--readlink filename) t))
                 ((= ifmt #o040000) t)
                 (t nil))
           (ptr-read-u64 buf files--stat-off-nlink)
           (logand (ptr-read-u64 buf files--stat-off-uid) #xFFFFFFFF)
           (logand (ptr-read-u64 buf files--stat-off-gid) #xFFFFFFFF)
           (files--stat-time buf files--stat-off-atime files--stat-off-atime-nsec)
           (files--stat-time buf files--stat-off-mtime files--stat-off-mtime-nsec)
           (files--stat-time buf files--stat-off-ctime files--stat-off-ctime-nsec)
           (ptr-read-u64 buf files--stat-off-size)
           (files--mode-string (logand mode #xFFFF))
           nil
           (ptr-read-u64 buf files--stat-off-ino)
           (ptr-read-u64 buf files--stat-off-dev)
           (files--statx-btime filename)))))))

(defun files--time-to-seconds (time)
  "Convert a Lisp TIME value to integer seconds since the epoch.
nil means now (via `nl-current-unix-time'); an integer is used as-is; a float
is truncated; a (TICKS . HZ) timestamp folds to TICKS/HZ; a (HIGH LOW . _)
timestamp folds to HIGH*65536+LOW."
  (cond
   ((null time) (if (fboundp 'nl-current-unix-time) (nl-current-unix-time) 0))
   ((integerp time) time)
   ((floatp time) (truncate time))
   ((consp time)
    (let ((d (cdr time)))
      (if (consp d)
          (+ (* (car time) 65536) (car d))
        (/ (car time) d))))
   (t 0)))

(when (files--install-fallback-function-p 'set-file-times)
  (defun set-file-times (filename &optional time _flag)
    "Set the access and modification times of FILENAME to TIME (default now)
via utimes(2).  Signals `file-error' on kernel failure (rc < 0)."
    (if (fboundp 'nelisp--syscall-utimes)
        (let* ((secs (files--time-to-seconds time))
               (rc (nelisp--syscall-utimes (files--expand-file-name filename)
                                           secs secs)))
          (when (< rc 0)
            (signal 'file-error (list "Setting file times" filename rc))))
      (signal 'file-error
              (list "set-file-times unavailable (no nelisp--syscall-utimes)"
                    filename)))
    nil))
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

;; --- column / line geometry on the fallback buffer (vendor-coverage batch3) --
;; Self-contained on the buffer content string + point (no dependency on the
;; other line-builtins layers), honouring tab stops.

(defun files--tab-width ()
  "Effective tab width for column math (default 8)."
  (if (and (boundp 'tab-width) (integerp tab-width) (> tab-width 0))
      tab-width
    8))

(defun files--bol-position (&optional pos)
  "Return the 1-indexed beginning-of-line position for POS (or point)."
  (let* ((content (files--buffer-string-value))
         (p (files--clip-point (or pos (files--buffer-point-value))))
         (i (1- p)))                    ; 0-indexed char at/after point
    (while (and (> i 0) (not (eq (aref content (1- i)) ?\n)))
      (setq i (1- i)))
    (1+ i)))

(defun files--column-at (pos)
  "Return the zero-based display column of POS honouring tab stops."
  (let* ((content (files--buffer-string-value))
         (bol (1- (files--bol-position pos)))   ; 0-indexed BOL
         (end (1- (files--clip-point pos)))     ; 0-indexed target
         (tw (files--tab-width))
         (col 0)
         (i bol))
    (while (< i end)
      (setq col (if (eq (aref content i) ?\t)
                    (* (1+ (/ col tw)) tw)
                  (1+ col)))
      (setq i (1+ i)))
    col))

(when (files--install-fallback-function-p 'current-indentation)
  (defun current-indentation ()
    "Return the indentation column of the current line (leading whitespace)."
    (let* ((content (files--buffer-string-value))
           (len (files--string-length content))
           (tw (files--tab-width))
           (i (1- (files--bol-position)))       ; 0-indexed BOL
           (col 0))
      (while (and (< i len)
                  (let ((ch (aref content i))) (or (eq ch ?\s) (eq ch ?\t))))
        (setq col (if (eq (aref content i) ?\t)
                      (* (1+ (/ col tw)) tw)
                    (1+ col)))
        (setq i (1+ i)))
      col)))

(when (files--install-fallback-function-p 'move-to-column)
  (defun move-to-column (column &optional force)
    "Move point to COLUMN on the current line; return the column reached.
With FORCE t, pad a too-short line with spaces to reach COLUMN."
    (let* ((content (files--buffer-string-value))
           (len (files--string-length content))
           (tw (files--tab-width))
           (pos (1- (files--bol-position)))      ; 0-indexed scan cursor
           (col 0))
      (while (and (< pos len)
                  (< col column)
                  (not (eq (aref content pos) ?\n)))
        (setq col (if (eq (aref content pos) ?\t)
                      (* (1+ (/ col tw)) tw)
                    (1+ col)))
        (setq pos (1+ pos)))
      (files--set-buffer-point-value (1+ pos))
      (when (and (eq force t) (< col column))
        (let ((n (- column col)))
          (when (> n 0)
            (insert (make-string n ?\s))
            (setq col column))))
      col)))

(when (files--install-fallback-function-p 'indent-to)
  (defun indent-to (column &optional minimum)
    "Indent from point to COLUMN with spaces, at least MINIMUM (default 0).
Return the column reached."
    (let* ((cur (files--column-at (files--buffer-point-value)))
           (target (max column (+ cur (max 0 (or minimum 0)))))
           (n (- target cur)))
      (when (> n 0)
        (insert (make-string n ?\s)))
      target)))

(when (files--install-fallback-function-p 'count-lines)
  (defun count-lines (start end)
    "Return the number of lines between START and END (Emacs semantics)."
    (let* ((content (files--buffer-string-value))
           (len (files--string-length content))
           (lo (max 0 (1- (min start end))))     ; 0-indexed first char
           (hi (min len (1- (max start end))))   ; 0-indexed end (exclusive)
           (n 0)
           (i lo))
      (while (< i hi)
        (when (eq (aref content i) ?\n) (setq n (1+ n)))
        (setq i (1+ i)))
      (if (and (/= start end) (> hi lo) (not (eq (aref content (1- hi)) ?\n)))
          (1+ n)
        n))))

(when (files--install-fallback-function-p 'file-directory-p)
  (defun file-directory-p (filename)
    "Return non-nil if FILENAME is an existing directory.
Detected with access(2) on FILENAME with a `.' component appended -- only a
directory has a `.' entry -- and falls back to the trailing-slash heuristic
when access(2) is unavailable."
    (cond
     ((not (stringp filename)) nil)
     ((fboundp 'nelisp--syscall-path-int)
      (let* ((d (files--expand-file-name filename))
             (n (length d))
             (probe (concat d (if (and (> n 0) (eq (aref d (1- n)) ?/)) "." "/."))))
        (= 0 (nelisp--syscall-path-int files--syscall-access probe files--ok-exist))))
     ((let ((n (length filename))) (and (> n 0) (eq (aref filename (1- n)) ?/))) t)
     (t nil))))
(when (files--install-fallback-function-p 'make-directory)
  (defun make-directory (_dir &optional _parents)
    "No-op: the standalone reader has no mkdir syscall; the parent directory is
assumed to already exist."
    nil))

;; --- file / directory removal via the reader's `nelisp--syscall-path' -------
;; Linux x86_64 syscall numbers (the standalone reader's only target).
(defconst files--syscall-unlink 87 "Linux x86_64 unlink(2) syscall number.")
(defconst files--syscall-rmdir 84 "Linux x86_64 rmdir(2) syscall number.")
(defconst files--syscall-rename 82 "Linux x86_64 rename(2) syscall number.")
(defconst files--syscall-link 86 "Linux x86_64 link(2) syscall number.")
(defconst files--syscall-symlink 88 "Linux x86_64 symlink(2) syscall number.")
(defconst files--syscall-chmod 90 "Linux x86_64 chmod(2) syscall number.")

(when (files--install-fallback-function-p 'delete-file)
  (defun delete-file (filename &optional _trash)
    "Delete FILENAME via the reader's `nelisp--syscall-path' unlink(2).
Signals `file-error' on kernel failure (rc < 0).  TRASH is ignored."
    (if (fboundp 'nelisp--syscall-path)
        (let ((rc (nelisp--syscall-path files--syscall-unlink
                                        (files--expand-file-name filename))))
          (when (< rc 0)
            (signal 'file-error (list "Removing old name" filename rc))))
      (signal 'file-error
              (list "delete-file unavailable (no nelisp--syscall-path)" filename)))
    nil))

(when (files--install-fallback-function-p 'delete-directory)
  (defun delete-directory (directory &optional _recursive _trash)
    "Delete the empty DIRECTORY via the reader's `nelisp--syscall-path' rmdir(2).
Signals `file-error' on kernel failure (rc < 0).  RECURSIVE and TRASH are
ignored -- only an empty directory can be removed."
    (if (fboundp 'nelisp--syscall-path)
        (let ((rc (nelisp--syscall-path files--syscall-rmdir
                                        (files--expand-file-name directory))))
          (when (< rc 0)
            (signal 'file-error (list "Removing directory" directory rc))))
      (signal 'file-error
              (list "delete-directory unavailable (no nelisp--syscall-path)"
                    directory)))
    nil))

(when (files--install-fallback-function-p 'rename-file)
  (defun rename-file (file newname &optional _ok-if-already-exists)
    "Rename FILE to NEWNAME via the reader's `nelisp--syscall-path2' rename(2).
Signals `file-error' on kernel failure (rc < 0).  OK-IF-ALREADY-EXISTS is
ignored -- rename(2) overwrites an existing NEWNAME (unless it is a
non-empty directory)."
    (if (fboundp 'nelisp--syscall-path2)
        (let ((rc (nelisp--syscall-path2 files--syscall-rename
                                         (files--expand-file-name file)
                                         (files--expand-file-name newname))))
          (when (< rc 0)
            (signal 'file-error (list "Renaming" file newname rc))))
      (signal 'file-error
              (list "rename-file unavailable (no nelisp--syscall-path2)" file)))
    nil))

(when (files--install-fallback-function-p 'add-name-to-file)
  (defun add-name-to-file (oldname newname &optional _ok-if-already-exists)
    "Make a hard link NEWNAME to OLDNAME via the reader's `nelisp--syscall-path2'
link(2).  Signals `file-error' on kernel failure (rc < 0).
OK-IF-ALREADY-EXISTS is ignored."
    (if (fboundp 'nelisp--syscall-path2)
        (let ((rc (nelisp--syscall-path2 files--syscall-link
                                         (files--expand-file-name oldname)
                                         (files--expand-file-name newname))))
          (when (< rc 0)
            (signal 'file-error (list "Adding new name" oldname newname rc))))
      (signal 'file-error
              (list "add-name-to-file unavailable (no nelisp--syscall-path2)"
                    oldname)))
    nil))

(when (files--install-fallback-function-p 'make-symbolic-link)
  (defun make-symbolic-link (target linkname &optional _ok-if-already-exists)
    "Make a symbolic link LINKNAME pointing at TARGET via the reader's
`nelisp--syscall-path2' symlink(2).  TARGET is stored verbatim (not
expanded -- a symlink target may legitimately be relative); LINKNAME is
the path where the link is created.  Signals `file-error' on kernel
failure (rc < 0).  OK-IF-ALREADY-EXISTS is ignored."
    (if (fboundp 'nelisp--syscall-path2)
        (let ((rc (nelisp--syscall-path2 files--syscall-symlink
                                         target
                                         (files--expand-file-name linkname))))
          (when (< rc 0)
            (signal 'file-error (list "Making symbolic link" target linkname rc))))
      (signal 'file-error
              (list "make-symbolic-link unavailable (no nelisp--syscall-path2)"
                    linkname)))
    nil))

(when (files--install-fallback-function-p 'set-file-modes)
  (defun set-file-modes (filename mode &optional _flag)
    "Set the permission bits of FILENAME to MODE via the reader's
`nelisp--syscall-path-int' chmod(2).  MODE is the integer permission bits
\(e.g. #o644).  Signals `file-error' on kernel failure (rc < 0).  FLAG is
ignored."
    (if (fboundp 'nelisp--syscall-path-int)
        (let ((rc (nelisp--syscall-path-int files--syscall-chmod
                                            (files--expand-file-name filename)
                                            mode)))
          (when (< rc 0)
            (signal 'file-error (list "Setting file modes" filename rc))))
      (signal 'file-error
              (list "set-file-modes unavailable (no nelisp--syscall-path-int)"
                    filename)))
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
