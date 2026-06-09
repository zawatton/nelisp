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

(defvar describe-symbol-backends nil
  "Backends consulted by callers that extend `describe-symbol'.")

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
  "Display BUFFER in a help window and return it.
Prefer `pop-to-buffer' so the help buffer appears in a separate window and
the editing buffer stays visible (M3 help window rule); fall back to
`switch-to-buffer' then `set-buffer' when those are unavailable."
  (cond
   ((fboundp 'pop-to-buffer) (pop-to-buffer buffer))
   ((fboundp 'switch-to-buffer) (switch-to-buffer buffer))
   (t (set-buffer buffer)))
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

(defun help-go-back ()
  "Report that help history navigation is not implemented yet."
  (interactive)
  (user-error "Help history is not available"))

(defun help-go-forward ()
  "Report that help history navigation is not implemented yet."
  (interactive)
  (user-error "Help history is not available"))

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
(defun describe-symbol (symbol)
  "Render help for SYMBOL as a function or variable."
  (interactive
   (list (emacs-help--read-symbol
          "Describe symbol: "
          (append (emacs-help--function-candidates)
                  (emacs-help--variable-candidates))
          (lambda (sym) (or (fboundp sym) (boundp sym))))))
  (cond
   ((fboundp symbol)
    (describe-function symbol))
   ((boundp symbol)
    (describe-variable symbol))
   (t
    (user-error "%s is not a defined function or bound variable" symbol))))

;;;###autoload
(defun describe-key (key &optional buffer)
  "Render help for KEY and its bound command in the shared `*Help*' buffer."
  (interactive (list (read-key-sequence "Describe key: ")))
  (ignore buffer)
  (emacs-help--render-key key))

;; Snapshot our `describe-*' implementations at load time so we can
;; reassert them later.  Host Emacs's `help-fns' / `help.el' install
;; their own definitions whenever something autoload-loads them (e.g.
;; `find-function-library' loads `find-func' which loads `help-fns'),
;; silently overwriting our polyfills via plain `defun'.  Tests after
;; that point would otherwise route through host help, breaking the
;; *Help*-buffer rendering contract this module owns.
(defvar emacs-help--describe-function-impl
  (symbol-function 'describe-function)
  "Captured nelisp-emacs `describe-function' implementation.")

(defvar emacs-help--describe-variable-impl
  (symbol-function 'describe-variable)
  "Captured nelisp-emacs `describe-variable' implementation.")

(defvar emacs-help--describe-key-impl
  (symbol-function 'describe-key)
  "Captured nelisp-emacs `describe-key' implementation.")

(defvar emacs-help--describe-symbol-impl
  (symbol-function 'describe-symbol)
  "Captured nelisp-emacs `describe-symbol' implementation.")

(defun emacs-help--reassert-overrides ()
  "Reinstall our `describe-*' implementations.
Run from `emacs-help--ensure-global-bindings' so any host library that
re-defined these symbols (via `help-fns' autoload, `find-func' load,
etc.) is silently re-shadowed before the binding step."
  (fset 'describe-function emacs-help--describe-function-impl)
  (fset 'describe-variable emacs-help--describe-variable-impl)
  (fset 'describe-symbol emacs-help--describe-symbol-impl)
  (fset 'describe-key emacs-help--describe-key-impl))

(defun emacs-help--ensure-global-bindings ()
  "Install the M2.2 help bindings into the global map."
  (emacs-help--reassert-overrides)
  (let ((map (or (and (fboundp 'current-global-map) (current-global-map))
                 (and (fboundp 'make-sparse-keymap) (make-sparse-keymap)))))
    (when (and map (fboundp 'use-global-map))
      (use-global-map map)
      (define-key map (kbd "C-h f") #'describe-function)
      (define-key map (kbd "C-h v") #'describe-variable)
      (define-key map (kbd "C-h k") #'describe-key))))

(emacs-help--ensure-global-bindings)

(unless (fboundp 'documentation)
  (defun documentation (function &optional _raw)
    (let ((f (if (symbolp function) (and (fboundp function) function) nil)))
      (and f (get f 'function-documentation)))))
(unless (fboundp 'help-function-arglist)
  (defun help-function-arglist (def &optional _preserve-names)
    (let ((f (cond ((symbolp def) (and (fboundp def) (symbol-function def))) (t def))))
      (cond ((null f) nil)
            ((and (consp f) (eq (car f) 'lambda)) (car (cdr f)))
            ((and (consp f) (eq (car f) 'closure)) (car (cdr (cdr f))))
            ((and (consp f) (eq (car f) 'macro)) (help-function-arglist (cdr f)))
            (t nil)))))

(provide 'emacs-help)

;;; emacs-help.el ends here
