;;; emacs-undo-ui.el --- Interactive undo / redo UI layer  -*- lexical-binding: t; -*-

;;; Commentary:

;; Thin interactive layer on top of `emacs-undo.el'.  This file keeps
;; the existing undo primitive / list format intact and adds user-facing
;; `undo', `undo-redo', `undo-boundary', plus the global key bindings.

;;; Code:

(require 'cl-lib)
(require 'emacs-keymap-builtins)
(require 'emacs-undo)
(require 'nelisp-emacs-compat)

(defvar emacs-undo-ui--redos nil
  "Alist (BUFFER-RECORD . REDO-STACK) for `undo-redo'.
Each redo stack element is one undo-group in `primitive-undo' record form.")

(defun emacs-undo-ui--cell-for (buf)
  "Return the redo alist cell for BUF, creating it if needed."
  (or (assq buf emacs-undo-ui--redos)
      (let ((cell (cons buf nil)))
        (push cell emacs-undo-ui--redos)
        cell)))

(defun emacs-undo-ui--current-buffer ()
  "Return the current nelisp buffer record, or nil."
  (or (and (fboundp 'emacs-undo--current-buffer)
           (emacs-undo--current-buffer))
      (and (fboundp 'nelisp-ec-current-buffer)
           (nelisp-ec-current-buffer))))

(defun emacs-undo-ui--redo-stack ()
  "Return the redo stack for the current buffer."
  (let ((buf (emacs-undo-ui--current-buffer)))
    (when buf
      (cdr (emacs-undo-ui--cell-for buf)))))

(defun emacs-undo-ui--set-redo-stack (stack)
  "Set the current buffer's redo STACK and return it."
  (let* ((buf (emacs-undo-ui--current-buffer))
         (cell (and buf (emacs-undo-ui--cell-for buf))))
    (when cell
      (setcdr cell stack)))
  stack)

(defun emacs-undo-ui--clear-redo ()
  "Clear redo history for the current buffer."
  (emacs-undo-ui--set-redo-stack nil))

(defun emacs-undo-ui--skip-boundaries (list)
  "Return LIST with leading nil boundaries removed."
  (while (and (consp list) (null (car list)))
    (setq list (cdr list)))
  list)

(defun emacs-undo-ui--take-group (list)
  "Split LIST into (GROUP . REST), where GROUP is one undo-group."
  (let (group)
    (while (and list (car list))
      (push (car list) group)
      (setq list (cdr list)))
    (cons (nreverse group) list)))

(defun emacs-undo-ui--inverse-record (record)
  "Return the redo/undo inverse of RECORD for the current buffer."
  (cond
   ((and (consp record) (integerp (car record)) (integerp (cdr record)))
    (cons (nelisp-ec-buffer-substring (car record) (cdr record))
          (car record)))
   ((and (consp record) (stringp (car record)) (integerp (cdr record)))
    (cons (cdr record) (+ (cdr record) (length (car record)))))
   (t nil)))

(defun emacs-undo-ui--apply-record (record)
  "Apply one primitive undo RECORD."
  (emacs-undo-primitive-undo 1 (list record nil))
  nil)

(defun emacs-undo-ui--count (arg)
  "Normalize ARG to a positive repetition count."
  (max 1 (prefix-numeric-value (or arg 1))))

;;;###autoload
(defun undo (&optional arg)
  "Undo the last ARG undo-groups."
  (interactive "*p")
  (dotimes (_ (emacs-undo-ui--count arg))
    (let* ((list (emacs-undo-ui--skip-boundaries (emacs-undo-buffer-undo-list)))
           (split (emacs-undo-ui--take-group list))
           (group (car split))
           (rest (cdr split))
           redo-group)
      (when (null group)
        (user-error "No further undo information"))
      (dolist (record group)
        (let ((inverse (emacs-undo-ui--inverse-record record)))
          (emacs-undo-ui--apply-record record)
          (when inverse
            (push inverse redo-group))))
      (emacs-undo-ui--set-redo-stack
       (cons redo-group (emacs-undo-ui--redo-stack)))
      (emacs-undo-set-buffer-undo-list (cons nil rest))))
  nil)

;;;###autoload
(defun undo-redo (&optional arg)
  "Redo the last ARG undone undo-groups."
  (interactive "*p")
  (dotimes (_ (emacs-undo-ui--count arg))
    (let* ((stack (emacs-undo-ui--redo-stack))
           (group (car stack))
           undo-group)
      (when (null group)
        (user-error "No further redo information"))
      (dolist (record group)
        (let ((inverse (emacs-undo-ui--inverse-record record)))
          (emacs-undo-ui--apply-record record)
          (when inverse
            (push inverse undo-group))))
      (emacs-undo-ui--set-redo-stack (cdr stack))
      (let ((rest (emacs-undo-ui--skip-boundaries (emacs-undo-buffer-undo-list))))
        (emacs-undo-set-buffer-undo-list
         (append undo-group (cons nil rest))))))
  nil)

;;;###autoload
(defun undo-boundary ()
  "Close the current undo-group and drop redo on fresh edits."
  (let ((list (emacs-undo-buffer-undo-list)))
    (when (and (consp list) (car list))
      (emacs-undo-ui--clear-redo)))
  (emacs-undo-undo-boundary))

(defun emacs-undo-ui--install-bindings ()
  "Install the undo UI bindings into `current-global-map'."
  (let ((map (and (fboundp 'current-global-map) (current-global-map))))
    (when (and map (fboundp 'define-key) (fboundp 'kbd))
      (define-key map (kbd "C-/") #'undo)
      (define-key map (kbd "C-_") #'undo)
      (define-key map (kbd "C-x u") #'undo-redo))))

(emacs-undo-ui--install-bindings)

(provide 'emacs-undo-ui)

;;; emacs-undo-ui.el ends here
