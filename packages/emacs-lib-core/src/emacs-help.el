;;; emacs-help.el --- Help system for nelisp-emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Doc 02 `docs/design/02-v01-daily-driver.org' §3.2.2 asks for the
;; smallest useful help subsystem for the v0.1 daily-driver gate:
;; `describe-function', `describe-variable', `describe-key', and a
;; `help-mode' buffer with quit / rerender bindings.
;;
;; The implementation deliberately stays narrow:
;; - render into a single `*Help*' buffer
;; - keep per-buffer rerender state in a side table
;; - reuse existing runtime primitives (`documentation',
;;   `documentation-property', `key-binding', `read-key-sequence',
;;   `symbol-value', `symbol-file') rather than reimplementing them
;;
;; History navigation (`l') is intentionally out of scope for v0.1.

;;; Code:

(require 'cl-lib)
(require 'emacs-buffer-builtins)
(require 'emacs-command-loop-builtins)
(require 'emacs-keymap)
(require 'emacs-mode)
(require 'pp)

(defvar help-mode-map nil
  "Keymap for `help-mode'.")

(setq help-mode-map
      (let ((map (emacs-keymap-make-sparse-keymap)))
        (emacs-keymap-define-key map (kbd "q") #'emacs-help-quit-window)
        (emacs-keymap-define-key map (kbd "g") #'emacs-help-revert-buffer)
        map))

(defvar emacs-help--state (make-hash-table :test 'eq :weakness nil)
  "Hash table mapping help buffers to render metadata.
Each value is a plist with keys:
- `:rerender'  thunk that redraws the current help topic
- `:subject'   symbol or key description for the rendered topic
- `:kind'      one of `function', `variable', or `key'")

(defconst emacs-help--buffer-name "*Help*"
  "Name of the shared help buffer.")

(defun emacs-help--buffer ()
  "Return the shared help buffer, creating it when needed."
  (get-buffer-create emacs-help--buffer-name))

(defun emacs-help--show-buffer (buffer)
  "Display BUFFER in the current window and return it."
  (if (fboundp 'switch-to-buffer)
      (switch-to-buffer buffer)
    (set-buffer buffer))
  buffer)

(defun emacs-help--quit-window ()
  "Dismiss the current help buffer."
  (cond
   ((fboundp 'quit-window)
    (quit-window))
   ((fboundp 'bury-buffer)
    (bury-buffer))
   (t nil)))

(defun emacs-help--docstring (symbol kind)
  "Return SYMBOL's documentation string for KIND, or a fallback.
KIND is either `function' or `variable'."
  (let ((doc
         (pcase kind
           ('function
            (and (fboundp 'documentation)
                 (documentation symbol t)))
           ('variable
            (and (fboundp 'documentation-property)
                 (documentation-property symbol 'variable-documentation t))))))
    (if (and (stringp doc) (> (length doc) 0))
        doc
      "Not documented.")))

(defun emacs-help--symbol-file (symbol kind)
  "Return a source-file string for SYMBOL of KIND, or nil."
  (when (fboundp 'symbol-file)
    (condition-case nil
        (symbol-file symbol kind)
      (error nil))))

(defun emacs-help--arglist-from-definition (definition)
  "Extract an arglist from function DEFINITION when possible."
  (cond
   ((and (consp definition) (eq (car definition) 'lambda))
    (nth 1 definition))
   ((and (consp definition) (eq (car definition) 'closure))
    (nth 2 definition))
   ((and (consp definition) (eq (car definition) 'macro))
    (let ((inner (cdr definition)))
      (cond
       ((and (consp inner) (eq (car inner) 'lambda))
        (nth 1 inner))
       ((and (consp inner) (eq (car inner) 'closure))
        (nth 2 inner))
       (t nil))))
   (t nil)))

(defun emacs-help--function-signature (symbol)
  "Return a display signature string for function SYMBOL."
  (let* ((arglist
          (or (and (fboundp 'help-function-arglist)
                   (help-function-arglist symbol t))
              (and (fboundp 'symbol-function)
                   (emacs-help--arglist-from-definition
                    (symbol-function symbol)))))
         (signature
          (cond
           ((listp arglist)
            (cons symbol arglist))
           ((and (fboundp 'func-arity)
                 (ignore-errors (func-arity symbol)))
            (let ((arity (func-arity symbol)))
              (list symbol
                    (format "min=%s max=%s" (car arity) (cdr arity)))))
           (t
            (list symbol "ARGS")))))
    (prin1-to-string signature)))

(defun emacs-help--function-candidates ()
  "Return a list of function symbols for minibuffer completion."
  (or (and (fboundp 'apropos-internal)
           (apropos-internal "" #'fboundp))
      (let (acc)
        (mapatoms
         (lambda (sym)
           (when (fboundp sym)
             (push sym acc))))
        acc)))

(defun emacs-help--variable-candidates ()
  "Return a list of bound variable symbols for minibuffer completion."
  (or (and (fboundp 'apropos-internal)
           (apropos-internal "" #'boundp))
      (let (acc)
        (mapatoms
         (lambda (sym)
           (when (boundp sym)
             (push sym acc))))
        acc)))

(defun emacs-help--read-symbol (prompt candidates predicate)
  "Read a symbol with PROMPT from CANDIDATES satisfying PREDICATE."
  (let* ((choice (completing-read prompt candidates predicate t nil nil))
         (symbol (if (symbolp choice) choice (intern choice))))
    (unless (funcall predicate symbol)
      (user-error "%s is not available" choice))
    symbol))

(defun emacs-help--read-function ()
  "Read a defined function symbol from the minibuffer."
  (emacs-help--read-symbol "Describe function: "
                           (emacs-help--function-candidates)
                           #'fboundp))

(defun emacs-help--read-variable ()
  "Read a bound variable symbol from the minibuffer."
  (emacs-help--read-symbol "Describe variable: "
                           (emacs-help--variable-candidates)
                           #'boundp))

(defun emacs-help--insert-section (title body)
  "Insert TITLE followed by BODY and a blank line."
  (insert title "\n")
  (insert body)
  (unless (string-suffix-p "\n" body)
    (insert "\n"))
  (insert "\n"))

(defun emacs-help--render-buffer (kind subject rerender renderer)
  "Render help content into `*Help*'.
KIND and SUBJECT describe the current topic.
RERENDER is a thunk stored for `g'.  RENDERER inserts the content."
  (let ((buffer (emacs-help--buffer)))
    (with-current-buffer buffer
      (help-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (funcall renderer)
        (goto-char (point-min))
        (setq buffer-read-only t))
      (puthash buffer
               (list :kind kind :subject subject :rerender rerender)
               emacs-help--state))
    (emacs-help--show-buffer buffer)))

(defun emacs-help--render-function (symbol)
  "Render function help for SYMBOL into `*Help*'."
  (unless (fboundp symbol)
    (user-error "%s is not a defined function" symbol))
  (emacs-help--render-buffer
   'function
   symbol
   (lambda () (emacs-help--render-function symbol))
   (lambda ()
     (insert (format "%s is a function.\n\n" symbol))
     (emacs-help--insert-section "Signature:"
                                 (emacs-help--function-signature symbol))
     (let ((file (emacs-help--symbol-file symbol 'defun)))
       (when file
         (emacs-help--insert-section "Defined in:" file)))
     (insert (emacs-help--docstring symbol 'function) "\n"))))

(defun emacs-help--render-variable (symbol)
  "Render variable help for SYMBOL into `*Help*'."
  (unless (boundp symbol)
    (user-error "%s is not a bound variable" symbol))
  (emacs-help--render-buffer
   'variable
   symbol
   (lambda () (emacs-help--render-variable symbol))
   (lambda ()
     (insert (format "%s is a variable.\n\n" symbol))
     (emacs-help--insert-section "Value:"
                                 (pp-to-string (symbol-value symbol)))
     (insert (emacs-help--docstring symbol 'variable) "\n"))))

(defun emacs-help--render-key (key)
  "Render key help for KEY into `*Help*'."
  (let* ((binding (key-binding key))
         (desc (if (fboundp 'key-description)
                   (key-description key)
                 (format "%S" key))))
    (unless (and binding (symbolp binding) (fboundp binding))
      (user-error "%s is not bound to a command" desc))
    (emacs-help--render-buffer
     'key
     desc
     (lambda () (emacs-help--render-key key))
     (lambda ()
       (insert (format "%s runs the command %s.\n\n" desc binding))
       (emacs-help--insert-section "Signature:"
                                   (emacs-help--function-signature binding))
       (let ((file (emacs-help--symbol-file binding 'defun)))
         (when file
           (emacs-help--insert-section "Defined in:" file)))
       (insert (emacs-help--docstring binding 'function) "\n")))))

;;;###autoload
(defun help-mode ()
  "Major mode for the shared `*Help*' buffer."
  (interactive)
  (emacs-mode-kill-all-local-variables)
  (emacs-mode-set-major-mode 'help-mode "Help")
  (setq major-mode 'help-mode)
  (setq mode-name "Help")
  (use-local-map help-mode-map)
  (setq truncate-lines t)
  nil)

;;;###autoload
(defun emacs-help-quit-window ()
  "Quit or bury the current help buffer."
  (interactive)
  (emacs-help--quit-window))

;;;###autoload
(defun emacs-help-revert-buffer ()
  "Re-render the current help topic."
  (interactive)
  (let* ((buffer (current-buffer))
         (state (and buffer (gethash buffer emacs-help--state)))
         (rerender (plist-get state :rerender)))
    (unless rerender
      (user-error "Current buffer is not a help buffer"))
    (funcall rerender)))

;;;###autoload
(defun describe-function (function)
  "Render help for FUNCTION in the shared `*Help*' buffer."
  (interactive (list (emacs-help--read-function)))
  (emacs-help--render-function function))

;;;###autoload
(defun describe-variable (variable)
  "Render help for VARIABLE in the shared `*Help*' buffer."
  (interactive (list (emacs-help--read-variable)))
  (emacs-help--render-variable variable))

;;;###autoload
(defun describe-key (key &optional buffer)
  "Render help for KEY and its bound command in the shared `*Help*' buffer."
  (interactive (list (read-key-sequence "Describe key: ")))
  (ignore buffer)
  (emacs-help--render-key key))

(defun emacs-help--ensure-global-bindings ()
  "Install the M2.2 help bindings into the global map."
  (let ((map (or (and (fboundp 'current-global-map) (current-global-map))
                 (and (fboundp 'make-sparse-keymap) (make-sparse-keymap)))))
    (when (and map (fboundp 'use-global-map))
      (use-global-map map)
      (define-key map (kbd "C-h f") #'describe-function)
      (define-key map (kbd "C-h v") #'describe-variable)
      (define-key map (kbd "C-h k") #'describe-key))))

(emacs-help--ensure-global-bindings)

(provide 'emacs-help)

;;; emacs-help.el ends here
