;;; emacs-completion-ui.el --- Minibuffer completion UI layer  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Doc 02 v0.1 Daily-Driver §3.4.2 M4.2.
;;
;; Adds the UI layer that sits on top of `emacs-minibuffer.el'
;; completion primitives: TAB completion from the active minibuffer,
;; a plain-text *Completions* buffer with column rendering, and
;; navigation / selection commands over that buffer.
;;
;; Scope for v0.1:
;; - prefix-match completion from `minibuffer-completion-table'
;; - unique hit => replace minibuffer contents directly
;; - multiple hits => render `*Completions*'
;; - RET confirmation path via `minibuffer-complete-and-exit'
;;
;; Out of scope here: icomplete-style live updates.

;;; Code:

(require 'cl-lib)
(require 'emacs-keymap-builtins)
(require 'emacs-minibuffer)
(require 'emacs-window)
(require 'nelisp-emacs-compat)

(defconst emacs-completion-ui--buffer-name "*Completions*"
  "Buffer name used for the completion candidate list.")

(defvar emacs-completion-ui--completion-state nil
  "Plist describing the active `*Completions*' buffer state.

Keys:
- :buffer       `nelisp-ec-buffer' used for the list.
- :origin       source minibuffer buffer.
- :candidates   list of candidate strings.
- :entries      list of (START END CANDIDATE) cell spans.
- :index        selected entry index.")

(defun emacs-completion-ui--find-buffer (name)
  "Return the live NeLisp buffer named NAME, or nil."
  (cl-find-if (lambda (buf)
                (equal name (nelisp-ec-buffer-name buf)))
              (mapcar #'cdr nelisp-ec--buffers)))

(defun emacs-completion-ui--completion-list (table)
  "Normalize TABLE into a list of strings."
  (if (fboundp 'emacs-minibuffer--collection->list)
      (emacs-minibuffer--collection->list table)
    (cond
     ((null table) nil)
     ((listp table) table)
     (t nil))))

(defun emacs-completion-ui--active-minibuffer ()
  "Return the active minibuffer buffer, or signal on missing context."
  (let ((buf (emacs-minibuffer--current-buffer)))
    (unless buf
      (signal 'emacs-minibuffer-error '("No active minibuffer")))
    buf))

(defun emacs-completion-ui--minibuffer-contents ()
  "Return the active minibuffer contents."
  (emacs-minibuffer-minibuffer-contents))

(defun emacs-completion-ui--set-minibuffer-contents (string)
  "Replace the active minibuffer contents with STRING."
  (let ((buf (emacs-completion-ui--active-minibuffer))
        (prompt-end (emacs-minibuffer-minibuffer-prompt-end)))
    (nelisp-ec-with-current-buffer buf
      (nelisp-ec-delete-region prompt-end (nelisp-ec-point-max))
      (nelisp-ec-goto-char prompt-end)
      (nelisp-ec-insert string))
    string))

(defun emacs-completion-ui--matches (input)
  "Return completion candidates matching INPUT by prefix."
  (let ((table (emacs-completion-ui--completion-list
                minibuffer-completion-table)))
    (cl-remove-if-not (lambda (cand)
                        (string-prefix-p input cand))
                      table)))

(defun emacs-completion-ui--column-count (candidates)
  "Choose a conservative column count for CANDIDATES."
  (let* ((max-width (max 1 (apply #'max (mapcar #'length candidates))))
         (cell-width (+ max-width 2))
         (total-width (if (fboundp 'emacs-window-window-width)
                          (emacs-window-window-width
                           (emacs-window-selected-window))
                        80)))
    (max 1 (/ (max total-width cell-width) cell-width))))

(defun emacs-completion-ui--entry-at-point ()
  "Return the completion entry covering point in the current buffer."
  (let ((pt (nelisp-ec-point)))
    (cl-find-if (lambda (entry)
                  (and (<= (nth 0 entry) pt)
                       (< pt (nth 1 entry))))
                (plist-get emacs-completion-ui--completion-state :entries))))

(defun emacs-completion-ui--goto-entry (index)
  "Move point in `*Completions*' to entry INDEX and persist the selection."
  (let* ((entries (plist-get emacs-completion-ui--completion-state :entries))
         (count (length entries)))
    (when (> count 0)
      (let* ((normalized (mod index count))
             (entry (nth normalized entries)))
        (setq emacs-completion-ui--completion-state
              (plist-put emacs-completion-ui--completion-state :index normalized))
        (nelisp-ec-goto-char (nth 0 entry))
        (nth 2 entry)))))

(defun emacs-completion-ui--render-completions (origin candidates)
  "Render CANDIDATES into the reusable `*Completions*' buffer."
  (let* ((buf (or (emacs-completion-ui--find-buffer
                   emacs-completion-ui--buffer-name)
                  (nelisp-ec-generate-new-buffer
                   emacs-completion-ui--buffer-name)))
         (columns (emacs-completion-ui--column-count candidates))
         (max-width (max 1 (apply #'max (mapcar #'length candidates))))
         (cell-width (+ max-width 2))
         (entries nil))
    (nelisp-ec-with-current-buffer buf
      (nelisp-ec-erase-buffer)
      (let ((idx 0))
        (dolist (cand candidates)
          (let ((start (nelisp-ec-point))
                (cell (format (format "%%-%ds" cell-width) cand)))
            (nelisp-ec-insert cell)
            (push (list start (nelisp-ec-point) cand) entries)
            (when (= (mod (1+ idx) columns) 0)
              (nelisp-ec-insert "\n")))
          (setq idx (1+ idx))))
      (unless (or (null candidates)
                  (string-suffix-p "\n" (nelisp-ec-buffer-string)))
        (nelisp-ec-insert "\n"))
      (nelisp-ec-goto-char (if entries (caar (last entries)) 1)))
    (setq emacs-completion-ui--completion-state
          (list :buffer buf
                :origin origin
                :candidates candidates
                :entries (nreverse entries)
                :index 0))
    (when entries
      (nelisp-ec-with-current-buffer buf
        (emacs-completion-ui--goto-entry 0)))
    buf))

(defun emacs-completion-ui--show-completions (origin candidates)
  "Render and return a `*Completions*' buffer for CANDIDATES."
  (emacs-completion-ui--render-completions origin candidates))

(defun emacs-completion-ui--confirm-and-exit ()
  "Exit the current minibuffer via the existing primitive."
  (emacs-minibuffer-exit-minibuffer))

(defun emacs-completion-ui--completion-target ()
  "Return the selected completion string in the current list buffer."
  (let ((entry (emacs-completion-ui--entry-at-point)))
    (and entry (nth 2 entry))))

;;;###autoload
(defun minibuffer-complete ()
  "Complete the active minibuffer contents by prefix match.
Unique matches replace the minibuffer contents directly.  Multiple
matches render the `*Completions*' buffer."
  (interactive)
  (let* ((origin (emacs-completion-ui--active-minibuffer))
         (input (emacs-completion-ui--minibuffer-contents))
         (matches (emacs-completion-ui--matches input)))
    (cond
     ((null matches)
      (emacs-minibuffer-minibuffer-message "No completions for %s" input)
      nil)
     ((= (length matches) 1)
      (emacs-completion-ui--set-minibuffer-contents (car matches)))
     (t
      (emacs-completion-ui--show-completions origin matches)))))

;;;###autoload
(defun minibuffer-complete-and-exit ()
  "Confirm the active minibuffer completion and close the minibuffer."
  (interactive)
  (let* ((input (emacs-completion-ui--minibuffer-contents))
         (matches (emacs-completion-ui--matches input))
         (exact (member input matches)))
    (cond
     (exact
      (emacs-completion-ui--confirm-and-exit))
     ((= (length matches) 1)
      (emacs-completion-ui--set-minibuffer-contents (car matches))
      (emacs-completion-ui--confirm-and-exit))
     ((and (null matches) minibuffer-completion-confirm)
      (signal 'emacs-minibuffer-error (list "Match required" input)))
     ((null matches)
      (emacs-completion-ui--confirm-and-exit))
     (t
      (emacs-completion-ui--show-completions
       (emacs-completion-ui--active-minibuffer) matches)
      nil))))

;;;###autoload
(defun next-completion (&optional n)
  "Move point to the next completion in `*Completions*'."
  (interactive)
  (unless (eq (nelisp-ec-current-buffer)
              (plist-get emacs-completion-ui--completion-state :buffer))
    (signal 'emacs-minibuffer-error '("Not in *Completions* buffer")))
  (emacs-completion-ui--goto-entry
   (+ (or (plist-get emacs-completion-ui--completion-state :index) 0)
      (or n 1))))

;;;###autoload
(defun previous-completion (&optional n)
  "Move point to the previous completion in `*Completions*'."
  (interactive)
  (unless (eq (nelisp-ec-current-buffer)
              (plist-get emacs-completion-ui--completion-state :buffer))
    (signal 'emacs-minibuffer-error '("Not in *Completions* buffer")))
  (emacs-completion-ui--goto-entry
   (- (or (plist-get emacs-completion-ui--completion-state :index) 0)
      (or n 1))))

;;;###autoload
(defun switch-to-completions ()
  "Select the current `*Completions*' buffer."
  (interactive)
  (let ((buf (plist-get emacs-completion-ui--completion-state :buffer)))
    (unless buf
      (signal 'emacs-minibuffer-error '("No active *Completions* buffer")))
    (emacs-window-set-window-buffer (emacs-window-selected-window) buf)
    (nelisp-ec-set-buffer buf)
    (emacs-completion-ui--goto-entry
     (or (plist-get emacs-completion-ui--completion-state :index) 0))
    buf))

;;;###autoload
(defun choose-completion (&optional _event _buffer _base-size)
  "Choose the completion at point and confirm it in the minibuffer."
  (interactive)
  (let* ((choice (emacs-completion-ui--completion-target))
         (origin (plist-get emacs-completion-ui--completion-state :origin)))
    (unless choice
      (signal 'emacs-minibuffer-error '("No completion at point")))
    (unless origin
      (signal 'emacs-minibuffer-error '("No minibuffer origin for completion")))
    (nelisp-ec-with-current-buffer origin
      (emacs-completion-ui--set-minibuffer-contents choice))
    (emacs-completion-ui--confirm-and-exit)
    choice))

(defun emacs-completion-ui-reset ()
  "Reset module state for tests."
  (setq emacs-completion-ui--completion-state nil)
  nil)

(defun emacs-completion-ui--install-keybindings ()
  "Install completion bindings onto the available keymaps."
  (when (and (boundp 'minibuffer-local-completion-map)
             minibuffer-local-completion-map
             (fboundp 'define-key)
             (fboundp 'kbd))
    (define-key minibuffer-local-completion-map (kbd "TAB") #'minibuffer-complete))
  (when (and (boundp 'completion-list-mode-map)
             completion-list-mode-map
             (fboundp 'define-key)
             (fboundp 'kbd))
    (define-key completion-list-mode-map (kbd "RET") #'choose-completion)
    (define-key completion-list-mode-map (kbd "n") #'next-completion)
    (define-key completion-list-mode-map (kbd "p") #'previous-completion)))

(emacs-completion-ui--install-keybindings)

(provide 'emacs-completion-ui)

;;; emacs-completion-ui.el ends here
