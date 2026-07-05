;;; emacs-imenu.el --- minimal Elisp symbol index (imenu)  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Layer 2 (nelisp-emacs) symbol-index semantics: scan an Emacs Lisp buffer for
;; top-level definitions (`defun', `defvar', `defmacro', `cl-defstruct', ...)
;; and build a NAME -> position index that drives `imenu' jump-to-symbol.
;;
;; Boundary (../nelisp-gui/docs/design/00-three-layer-architecture.org):
;;   - nelisp-emacs OWNS: the definition scan, the index alist, and the
;;     goto-position command semantics.
;;   - nelisp-gui OWNS: rendering the completion prompt + key transport.
;; This module only computes the index and moves point; no X11 decisions.
;;
;; Scope is deliberately Elisp-first (the dev daily-driver workflow).  Other
;; major modes get an index only once their definition shapes are promoted.

;;; Code:

(unless (boundp 'imenu-generic-expression)
  (defvar imenu-generic-expression nil
    "Mode-local generic imenu expression list."))

(defvar emacs-imenu-elisp-definition-regexp
  "^[ \t]*\\((\\)\\(cl-def[a-z-]*\\|def[a-z-]*\\)[ \t\n]+'?\\([^ \t\n()'\"]+\\)"
  "Regexp matching a candidate top-level Elisp definition.
Group 1 is the opening paren (the jump target); group 2 is the
definition head (validated against `emacs-imenu-elisp-definition-heads');
group 3 is the defined symbol name.  `defalias' / quoted names are
handled by the optional leading quote.

Deliberately avoids shy groups `\\(?:...\\)' and the `\\>' word
boundary: the standalone reader's regexp engine miscounts shy groups
as capturing and does not support word boundaries.  The head is matched
broadly (`def...') and filtered in Lisp so a single deterministic group
numbering holds on both host Emacs and the reader.")

(defvar emacs-imenu-elisp-definition-heads
  '("defun" "defmacro" "defvar" "defvar-local" "defconst" "defcustom"
    "defsubst" "defgroup" "defface" "defalias" "defgeneric" "defmethod"
    "define-derived-mode" "define-minor-mode"
    "define-globalized-minor-mode" "define-global-minor-mode"
    "cl-defun" "cl-defmacro" "cl-defstruct" "cl-defgeneric" "cl-defmethod"
    "cl-defsubst" "cl-deftype")
  "Definition heads accepted by the Elisp index scan.
Heads matched by `emacs-imenu-elisp-definition-regexp' but absent here
\(notably `define-key') are skipped.")

(defun emacs-imenu--scan-elisp ()
  "Scan the current buffer for Elisp definitions.
Return a list of (NAME . POSITION) in buffer order, where POSITION is
the opening paren of the defining form.  Duplicate names are kept (a
symbol can be defined more than once); callers may de-duplicate.

The scan runs `string-match' over the buffer text rather than
`re-search-forward': the standalone reader's buffer-search primitives
are limited, but its string regexp engine works.  Buffer position is
`point-min' plus the string index."
  (let ((base (point-min))
        (text (buffer-string))
        (pos 0)
        (out nil))
    (while (string-match emacs-imenu-elisp-definition-regexp text pos)
      ;; group 1 = `(' (string index), group 2 = head, group 3 = name.
      (when (member (match-string 2 text) emacs-imenu-elisp-definition-heads)
        (push (cons (match-string 3 text)
                    (+ base (match-beginning 1)))
              out))
      (setq pos (match-end 0)))
    (nreverse out)))

(defun emacs-imenu-create-index (&optional buffer)
  "Return the Elisp definition index for BUFFER (default current).
The result is an alist of (NAME . POSITION) in buffer order."
  (if buffer
      (with-current-buffer buffer
        (emacs-imenu--scan-elisp))
    (emacs-imenu--scan-elisp)))

(defun emacs-imenu--names (index)
  "Return the unique names in INDEX, first occurrence wins, in order."
  (let ((seen (make-hash-table :test 'equal))
        (out nil))
    (dolist (cell index)
      (unless (gethash (car cell) seen)
        (puthash (car cell) t seen)
        (push (car cell) out)))
    (nreverse out)))

(defun emacs-imenu-goto (name &optional buffer)
  "Move point to the definition of NAME in BUFFER (default current).
Return the position, or signal an error when NAME is not indexed."
  (let* ((index (emacs-imenu-create-index buffer))
         (cell (assoc name index)))
    (unless cell
      (error "emacs-imenu: no definition named %s" name))
    (when buffer (set-buffer buffer))
    (goto-char (cdr cell))
    (cdr cell)))

(defun emacs-imenu (&optional name)
  "Jump to an Elisp definition in the current buffer.
With NAME nil, prompt with completion over the indexed names.
Returns the target position."
  (interactive)
  (let* ((index (emacs-imenu-create-index))
         (names (emacs-imenu--names index)))
    (unless names
      (error "emacs-imenu: no Elisp definitions in this buffer"))
    (let ((pick (or name
                    (if (fboundp 'completing-read)
                        (completing-read "Definition: " names nil t)
                      (car names)))))
      (let ((pos (emacs-imenu-goto pick)))
        (when (fboundp 'push-mark) (push-mark))
        pos))))

;;;; --- standard-name facade -----------------------------------------

(defun emacs-imenu-install ()
  "Bind the standard `imenu' command name to the `emacs-imenu' implementation.
Not run on `require' (keeps a bare load from touching shared command
symbols)."
  (defalias 'imenu #'emacs-imenu)
  (defalias 'imenu--make-index-alist #'emacs-imenu-create-index))

(provide 'emacs-imenu)

;;; emacs-imenu.el ends here
