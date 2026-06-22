;;; emacs-dired-min.el --- Minimal dired for nelisp-emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Doc 02 `docs/design/02-v01-daily-driver.org' §3.1 / item 6
;; (= dired-min) asks for the smallest directory browser needed by
;; the v0.1 daily-driver gate: enter a directory, move by line, RET
;; into files/subdirs, go up, quit, and rescan.
;;
;; This module intentionally keeps its own per-buffer side table for
;; listing state so it can operate on the `nelisp-ec-*' buffer
;; substrate without depending on host Emacs window/buffer objects.

;;; Code:

(require 'cl-lib)
(require 'emacs-buffer-builtins)
(require 'emacs-error)
(require 'emacs-keymap)
(require 'emacs-line-builtins)
(require 'emacs-minibuffer-builtins)
(require 'emacs-mode)
(require 'nelisp-emacs-compat)
(require 'nelisp-emacs-compat-fileio)

(defvar dired-mode-map nil
  "Keymap for `dired-mode'.")

(defun emacs-dired-min--ensure-mode-map ()
  "Build and return `dired-mode-map', constructing it on first use.
The standalone NeLisp reader aborts an eager top-level keymap
initializer at bundle-load time (a keymap primitive such as `kbd' is
not yet in its final binding while the bundle is still loading),
leaving the variable unbound.  Building the map lazily on the first
`dired-mode' entry sidesteps that load-order fragility — by then every
keymap primitive is fully installed."
  (unless dired-mode-map
    (let ((map (emacs-keymap-make-sparse-keymap)))
      (emacs-keymap-define-key map (kbd "RET") #'dired-find-file)
      (emacs-keymap-define-key map (kbd "n") #'dired-next-line)
      (emacs-keymap-define-key map (kbd "p") #'dired-previous-line)
      (emacs-keymap-define-key map (kbd "q") #'emacs-dired-min-quit-window)
      (emacs-keymap-define-key map (kbd "g") #'emacs-dired-min-revert-buffer)
      (emacs-keymap-define-key map (kbd "^") #'dired-up-directory)
      (emacs-keymap-define-key map (kbd "m") #'dired-mark)
      (emacs-keymap-define-key map (kbd "u") #'dired-unmark)
      (emacs-keymap-define-key map (kbd "d") #'dired-flag-file-deletion)
      (emacs-keymap-define-key map (kbd "x") #'dired-do-flagged-delete)
      (emacs-keymap-define-key map (kbd "R") #'dired-do-rename)
      (emacs-keymap-define-key map (kbd "C") #'dired-do-copy)
      (setq dired-mode-map map)))
  dired-mode-map)

(defvar emacs-dired-min--state (make-hash-table :test 'eq :weakness nil)
  "Hash table mapping dired buffers to listing metadata.
Each value is a plist with keys:
- `:directory'       current expanded directory (with trailing slash)
- `:entries'         listing entries in display order
- `:line-starts'     1-based buffer positions for each entry line
- `:previous-buffer' previously-selected buffer, if any")

(defun emacs-dired-min--buffer-by-name (name)
  "Return the live nelisp buffer named NAME, or nil."
  (cdr (assoc name nelisp-ec--buffers)))

(defun emacs-dired-min--dired-buffer-name (directory)
  "Return the dired buffer name for DIRECTORY."
  (format "*Dired %s*" directory))

(defun emacs-dired-min--normalize-directory (directory)
  "Return DIRECTORY as an expanded directory name."
  (file-name-as-directory (nelisp-ec-expand-file-name directory)))

(defun emacs-dired-min--parent-directory (directory)
  "Return the parent directory of DIRECTORY."
  (let* ((dir (directory-file-name
               (emacs-dired-min--normalize-directory directory)))
         (parent (file-name-directory dir)))
    (if (or (null parent) (equal parent ""))
        (emacs-dired-min--normalize-directory directory)
      (file-name-as-directory (nelisp-ec-expand-file-name parent)))))

(defun emacs-dired-min--directory-files-and-attributes (directory)
  "Return `(NAME . ATTRS)' pairs for DIRECTORY in alphabetical order."
  (let ((entries nil))
    (dolist (name (nelisp-ec-directory-files directory nil nil nil nil))
      (let* ((path (nelisp-ec-expand-file-name name directory))
             (attrs (nelisp-ec-file-attributes path)))
        (when attrs
          (push (cons name attrs) entries))))
    (nreverse entries)))

(defun emacs-dired-min--mark-for (marks name)
  "Return the mark char recorded for NAME in MARKS, or ?\\s when unmarked."
  (or (cdr (assoc name marks)) ?\s))

(defun emacs-dired-min--remove-mark (marks name)
  "Return MARKS with any entry for NAME removed."
  (let (kept)
    (dolist (cell marks)
      (unless (equal (car cell) name)
        (push cell kept)))
    (nreverse kept)))

(defun emacs-dired-min--format-entry (entry mark-char)
  "Return the display line for ENTRY prefixed with MARK-CHAR + a space.
The mark column is a fixed width (two characters), so line offsets are
independent of which mark is shown."
  (let* ((name (plist-get entry :name))
         (attrs (plist-get entry :attributes))
         (size (or (nth 7 attrs) 0))
         (modes (or (nth 8 attrs) "----------")))
    (format "%c %s\t%s\t%s\n" mark-char name size modes)))

(defun emacs-dired-min--render-text (entries marks)
  "Return listing text for ENTRIES using MARKS."
  (let ((text ""))
    (dolist (entry entries)
      (setq text
            (concat text
                    (emacs-dired-min--format-entry
                     entry
                     (emacs-dired-min--mark-for
                      marks (plist-get entry :name))))))
    text))

(defun emacs-dired-min--entries (directory)
  "Return dired entry plists for DIRECTORY."
  (let ((dir (emacs-dired-min--normalize-directory directory))
        (entries nil))
    (dolist (cell (emacs-dired-min--directory-files-and-attributes directory))
      (push (list :name (car cell)
                  :path (nelisp-ec-expand-file-name (car cell) dir)
                  :attributes (cdr cell))
            entries))
    (nreverse entries)))

(defun emacs-dired-min--line-starts-for-entries (entries)
  "Return 1-based line starts for ENTRIES."
  (let ((pos 1)
        (starts nil))
    (dolist (entry entries)
      (push pos starts)
      (setq pos (+ pos (length (emacs-dired-min--format-entry entry ?\s)))))
    (nreverse starts)))

(defun emacs-dired-min--line-index-at-point (line-starts point)
  "Return the line index in LINE-STARTS containing POINT."
  (let ((index 0)
        (count (length line-starts)))
    (while (and (< index count)
                (let ((next (nth (1+ index) line-starts)))
                  (and next (>= point next))))
      (setq index (1+ index)))
    index))

(defun emacs-dired-min--current-state ()
  "Return dired state for the current buffer, or signal `user-error'."
  (let* ((buffer (nelisp-ec-current-buffer))
         (state (and buffer (gethash buffer emacs-dired-min--state))))
    (or state
        (user-error "Current buffer is not a dired buffer"))))

(defun emacs-dired-min--current-entry ()
  "Return the dired entry at point, or nil when the listing is empty."
  (let* ((state (emacs-dired-min--current-state))
         (entries (plist-get state :entries))
         (line-starts (plist-get state :line-starts)))
    (when entries
      (nth (emacs-dired-min--line-index-at-point line-starts (nelisp-ec-point))
           entries))))

(defun emacs-dired-min--render-current-buffer (directory)
  "Render DIRECTORY into the current dired buffer."
  (let* ((dir (emacs-dired-min--normalize-directory directory))
         (entries (emacs-dired-min--entries dir))
         (line-starts (emacs-dired-min--line-starts-for-entries entries))
         (buffer (nelisp-ec-current-buffer))
         (old (gethash buffer emacs-dired-min--state))
         (previous (plist-get old :previous-buffer))
         (old-marks (plist-get old :marks))
         (marks (let (kept)
                  (dolist (entry entries)
                    (let ((cell (assoc (plist-get entry :name) old-marks)))
                      (when cell (push cell kept))))
                  (nreverse kept))))
    (nelisp-ec-erase-buffer)
    (let ((text (emacs-dired-min--render-text entries marks)))
      (nelisp-ec-insert text)
      (emacs-dired-min--mirror-host-buffer
       (emacs-dired-min--dired-buffer-name dir) text))
    (puthash buffer
             (list :directory dir
                   :entries entries
                   :line-starts line-starts
                   :previous-buffer previous
                   :marks marks)
             emacs-dired-min--state)
    (when (> (length entries) 0)
      (nelisp-ec-goto-char 1))
    buffer))

(defun emacs-dired-min--host-interactive-p ()
  "Return non-nil when a host Emacs window should mirror Dired text."
  (and (boundp 'noninteractive)
       (not noninteractive)
       (not (fboundp 'nl-write-file))
       (fboundp 'get-buffer-create)
       (fboundp 'selected-window)
       (fboundp 'set-window-buffer)))

(defun emacs-dired-min--mirror-host-buffer (name text)
  "Mirror Dired listing TEXT into host buffer NAME when running interactively.
The canonical Dired state remains the `nelisp-ec' buffer.  This mirror is
only for host `-nw' visibility, where Emacs redisplay paints host buffers."
  (when (emacs-dired-min--host-interactive-p)
    (let ((host-buffer (get-buffer-create name)))
      (with-current-buffer host-buffer
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert text)
          (goto-char (point-min))
          (setq major-mode 'dired-mode
                mode-name "Dired"
                buffer-read-only t)))
      (set-window-buffer (selected-window) host-buffer)
      host-buffer)))

(defun emacs-dired-min--goto-entry-index (index)
  "Move point to entry INDEX in the current dired buffer."
  (let* ((state (emacs-dired-min--current-state))
         (line-starts (plist-get state :line-starts))
         (count (length line-starts)))
    (when (> count 0)
      (nelisp-ec-goto-char
       (nth (max 0 (min index (1- count))) line-starts)))))

(defun emacs-dired-min--move-line (delta)
  "Move point DELTA lines in the current dired buffer."
  (let* ((state (emacs-dired-min--current-state))
         (line-starts (plist-get state :line-starts)))
    (when line-starts
      (emacs-dired-min--goto-entry-index
       (+ (emacs-dired-min--line-index-at-point line-starts (nelisp-ec-point))
          delta)))))

(defun emacs-dired-min--create-buffer (directory previous-buffer)
  "Return the dired buffer for DIRECTORY.
PREVIOUS-BUFFER is remembered for quit behaviour."
  (let* ((dir (emacs-dired-min--normalize-directory directory))
         (name (emacs-dired-min--dired-buffer-name dir))
         (buffer (or (emacs-dired-min--buffer-by-name name)
                     (nelisp-ec-generate-new-buffer name))))
    (puthash buffer
             (list :directory dir
                   :entries nil
                   :line-starts nil
                   :previous-buffer previous-buffer)
             emacs-dired-min--state)
    buffer))

;;;###autoload
(defun dired-mode ()
  "Major mode for the minimal directory browser."
  (interactive)
  (emacs-mode-kill-all-local-variables)
  (emacs-mode-set-major-mode 'dired-mode "Dired")
  (setq major-mode 'dired-mode)
  (setq mode-name "Dired")
  (emacs-keymap-use-local-map (emacs-dired-min--ensure-mode-map))
  nil)

;;;###autoload
(defun dired (directory)
  "Open DIRECTORY in a minimal dired buffer."
  (interactive (list (read-directory-name "Dired (directory): ")))
  (let* ((dir (emacs-dired-min--normalize-directory directory))
         (previous (nelisp-ec-current-buffer))
         (buffer (emacs-dired-min--create-buffer dir previous)))
    (nelisp-ec-with-current-buffer buffer
      (dired-mode)
      (emacs-dired-min--render-current-buffer dir))
    (nelisp-ec-set-buffer buffer)
    buffer))

;;;###autoload
(defun dired-find-file ()
  "Open the file or directory at point."
  (interactive)
  (let* ((entry (or (emacs-dired-min--current-entry)
                    (user-error "No dired entry on this line")))
         (path (plist-get entry :path)))
    (if (nelisp-ec-file-directory-p path)
        (dired path)
      (if (fboundp 'find-file)
          (find-file path)
        (user-error "find-file not available")))))

;;;###autoload
(defun dired-next-line (&optional n)
  "Move to the next dired line."
  (interactive "p")
  (emacs-dired-min--move-line (or n 1))
  nil)

;;;###autoload
(defun dired-previous-line (&optional n)
  "Move to the previous dired line."
  (interactive "p")
  (emacs-dired-min--move-line (- (or n 1)))
  nil)

;;;###autoload
(defun dired-up-directory ()
  "Visit the parent directory of the current dired buffer."
  (interactive)
  (dired (emacs-dired-min--parent-directory
          (plist-get (emacs-dired-min--current-state) :directory))))

;;;###autoload
(defun emacs-dired-min-revert-buffer ()
  "Rescan the current dired buffer."
  (interactive)
  (emacs-dired-min--render-current-buffer
   (plist-get (emacs-dired-min--current-state) :directory))
  nil)

;;;###autoload
(defun emacs-dired-min-quit-window ()
  "Leave the current dired buffer and restore the previous buffer if known."
  (interactive)
  (let* ((buffer (nelisp-ec-current-buffer))
         (state (emacs-dired-min--current-state))
         (previous (plist-get state :previous-buffer)))
    (when (and previous (buffer-live-p previous))
      (nelisp-ec-set-buffer previous))
    (when buffer
      (remhash buffer emacs-dired-min--state))
    nil))

(defun emacs-dired-min--set-mark-and-advance (mark-char)
  "Set the mark of the entry at point to MARK-CHAR, then move to the next line.
A MARK-CHAR of ?\\s clears any existing mark."
  (let* ((state (emacs-dired-min--current-state))
         (entries (plist-get state :entries))
         (line-starts (plist-get state :line-starts))
         (index (emacs-dired-min--line-index-at-point
                 line-starts (nelisp-ec-point)))
         (entry (and entries (nth index entries))))
    (when entry
      (let* ((name (plist-get entry :name))
             (marks (emacs-dired-min--remove-mark (plist-get state :marks) name)))
        (unless (eq mark-char ?\s)
          (setq marks (cons (cons name mark-char) marks)))
        (puthash (nelisp-ec-current-buffer)
                 (plist-put state :marks marks)
                 emacs-dired-min--state)
        (emacs-dired-min--render-current-buffer (plist-get state :directory))
        (emacs-dired-min--goto-entry-index
         (min (1+ index) (max 0 (1- (length entries)))))))
    nil))

;;;###autoload
(defun dired-mark (&optional _arg)
  "Mark the file on the current line with `*' and move to the next line."
  (interactive "p")
  (emacs-dired-min--set-mark-and-advance ?*))

;;;###autoload
(defun dired-unmark (&optional _arg)
  "Remove any mark on the current line and move to the next line."
  (interactive "p")
  (emacs-dired-min--set-mark-and-advance ?\s))

;;;###autoload
(defun dired-flag-file-deletion (&optional _arg)
  "Flag the file on the current line for deletion (`D') and move down."
  (interactive "p")
  (emacs-dired-min--set-mark-and-advance ?D))

;;;###autoload
(defun dired-do-flagged-delete ()
  "Delete the files flagged for deletion (marked with `D').
Directories are skipped (the minimal browser deletes regular files only).
Returns the number of files deleted."
  (interactive)
  (let* ((state (emacs-dired-min--current-state))
         (marks (plist-get state :marks))
         (entries (plist-get state :entries))
         (dir (plist-get state :directory))
         (deleted 0))
    (dolist (entry entries)
      (let ((name (plist-get entry :name))
            (path (plist-get entry :path)))
        (when (and (eq (emacs-dired-min--mark-for marks name) ?D)
                   (not (nelisp-ec-file-directory-p path)))
          (nelisp-ec-delete-file path)
          (setq deleted (1+ deleted)))))
    (emacs-dired-min--render-current-buffer dir)
    deleted))

;;;###autoload
(defun dired-do-rename (&optional _arg)
  "Rename the file at point to a destination read from the minibuffer."
  (interactive "p")
  (let* ((entry (or (emacs-dired-min--current-entry)
                    (user-error "No dired entry on this line")))
         (name (plist-get entry :name))
         (path (plist-get entry :path))
         (state (emacs-dired-min--current-state))
         (dir (plist-get state :directory))
         (target (read-file-name (format "Rename %s to: " name) dir))
         (new-path (nelisp-ec-expand-file-name target dir)))
    (nelisp-ec-rename-file path new-path)
    (emacs-dired-min--render-current-buffer dir)
    new-path))

(defun emacs-dired-min--copy-file (src dest)
  "Copy file SRC to DEST through a temporary buffer.
Content round-trips through the buffer encoder, which is UTF-8 oriented;
this minimal browser targets text files."
  (let ((buf (nelisp-ec-generate-new-buffer " *dired-copy*")))
    (unwind-protect
        (nelisp-ec-with-current-buffer buf
          (nelisp-ec-insert-file-contents src)
          (nelisp-ec-write-region (nelisp-ec-point-min)
                                  (nelisp-ec-point-max)
                                  dest))
      (nelisp-ec-kill-buffer buf))))

;;;###autoload
(defun dired-do-copy (&optional _arg)
  "Copy the file at point to a destination read from the minibuffer."
  (interactive "p")
  (let* ((entry (or (emacs-dired-min--current-entry)
                    (user-error "No dired entry on this line")))
         (name (plist-get entry :name))
         (path (plist-get entry :path))
         (state (emacs-dired-min--current-state))
         (dir (plist-get state :directory))
         (target (read-file-name (format "Copy %s to: " name) dir))
         (new-path (nelisp-ec-expand-file-name target dir)))
    (emacs-dired-min--copy-file path new-path)
    (emacs-dired-min--render-current-buffer dir)
    new-path))

(provide 'emacs-dired-min)

;;; emacs-dired-min.el ends here
