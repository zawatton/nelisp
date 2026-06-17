;;; emacs-xref.el --- minimal Elisp jump-to-definition (xref)  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Layer 2 (nelisp-emacs) cross-reference semantics: find where an Elisp symbol
;; is defined and jump there (`M-.'), then return (`M-,').  The definition scan
;; reuses `emacs-imenu' so both subsystems agree on what a "definition" is.
;;
;; Boundary (../nelisp-gui/docs/design/00-three-layer-architecture.org):
;;   - nelisp-emacs OWNS: the definition search across the current buffer and a
;;     candidate file set, the jump-back marker stack, and the goto semantics.
;;   - nelisp-gui OWNS: rendering and key transport.
;;
;; Search scope is intentionally small for the dev daily-driver: the current
;; buffer first, then a caller-supplied file list (default: the `.el' files in
;; `default-directory').  No tags database, no project-wide async index yet.

;;; Code:

(require 'emacs-imenu)

(defvar emacs-xref--marker-stack nil
  "Stack of (BUFFER . POSITION) jump-back locations, most recent first.")

(defun emacs-xref--symbol-at-point ()
  "Return the Elisp symbol name around point as a string, or nil."
  (save-excursion
    (let ((constituent "[^ \t\n()'\"`,;]"))
      (skip-chars-backward "^ \t\n()'\"`,;")
      (let ((start (point)))
        (skip-chars-forward "^ \t\n()'\"`,;")
        (when (> (point) start)
          (let ((s (buffer-substring-no-properties start (point))))
            (and (string-match-p constituent s) s)))))))

(defun emacs-xref--find-in-current-buffer (name)
  "Return the position of NAME's definition in the current buffer, or nil."
  (let ((cell (assoc name (emacs-imenu-create-index))))
    (and cell (cdr cell))))

(defun emacs-xref--find-in-file (name file)
  "Return (FILE . POSITION) when NAME is defined in FILE, else nil."
  (when (and file (file-exists-p file))
    (let ((buf (and (fboundp 'find-file-noselect)
                    (find-file-noselect file))))
      (when buf
        (let ((pos (with-current-buffer buf
                     (emacs-xref--find-in-current-buffer name))))
          (and pos (cons file pos)))))))

(defun emacs-xref--default-files ()
  "Return the `.el' files in `default-directory' (best effort)."
  ;; No `condition-case' here: it is unavailable on the standalone reader,
  ;; so guard with `file-directory-p' instead of catching `directory-files'.
  (when (and (boundp 'default-directory)
             default-directory
             (fboundp 'directory-files)
             (fboundp 'file-directory-p)
             (file-directory-p default-directory))
    (let ((dir default-directory))
      (mapcar (lambda (f) (expand-file-name f dir))
              (directory-files dir nil "\\.el\\'")))))

(defun emacs-xref--record (&optional buffer)
  "Push BUFFER (default current) and point onto the jump-back stack."
  (push (cons (or buffer (current-buffer)) (point))
        emacs-xref--marker-stack))

(defun emacs-xref-find-definitions (name &optional files)
  "Jump to the definition of NAME (a string or symbol).
Searches the current buffer first, then FILES (default: the `.el'
files in `default-directory').  On a hit, the current location is
pushed onto the jump-back stack.  Returns the target (BUFFER . POS)
or (FILE . POS), or signals when nothing is found."
  (interactive
   (list (or (emacs-xref--symbol-at-point)
             (and (fboundp 'read-string) (read-string "Find definition: ")))))
  (let* ((name (if (symbolp name) (symbol-name name) name))
         (here-pos (emacs-xref--find-in-current-buffer name)))
    (cond
     (here-pos
      (emacs-xref--record)
      (goto-char here-pos)
      (cons (current-buffer) here-pos))
     (t
      (let ((hit nil)
            (candidates (or files (emacs-xref--default-files))))
        (while (and candidates (not hit))
          (setq hit (emacs-xref--find-in-file name (car candidates)))
          (setq candidates (cdr candidates)))
        (if (not hit)
            (error "emacs-xref: no definition found for %s" name)
          (emacs-xref--record)
          (let ((buf (find-file-noselect (car hit))))
            (set-buffer buf)
            (goto-char (cdr hit))
            (when (fboundp 'display-buffer) (display-buffer buf))
            (cons buf (cdr hit)))))))))

(defun emacs-xref-pop-marker-stack ()
  "Return to the location before the last `emacs-xref-find-definitions' jump."
  (interactive)
  (let ((loc (pop emacs-xref--marker-stack)))
    (unless loc
      (error "emacs-xref: jump-back stack is empty"))
    (let ((buf (car loc)))
      (when (buffer-live-p buf)
        (set-buffer buf)
        (when (fboundp 'switch-to-buffer) (switch-to-buffer buf))
        (goto-char (cdr loc)))
      loc)))

;;;; --- standard-name facade + bindings ------------------------------

(defun emacs-xref-install ()
  "Bind the standard xref command names to the `emacs-xref-*' implementations.
Not run on `require' (keeps a bare load from touching shared command
symbols)."
  (defalias 'xref-find-definitions #'emacs-xref-find-definitions)
  (defalias 'xref-pop-marker-stack #'emacs-xref-pop-marker-stack)
  (emacs-xref--install-bindings))

(defun emacs-xref--install-bindings ()
  "Bind `M-.' / `M-,' to the xref commands on the global map."
  (let ((map (and (fboundp 'current-global-map) (current-global-map))))
    (when (and map (fboundp 'define-key) (fboundp 'kbd))
      (define-key map (kbd "M-.") #'emacs-xref-find-definitions)
      (define-key map (kbd "M-,") #'emacs-xref-pop-marker-stack))))

(provide 'emacs-xref)

;;; emacs-xref.el ends here
