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

(defvar emacs-help-gui-backend nil
  "Plist of GUI bridge callbacks used by the help runtime.")

(defvar emacs-help-gui-arg ""
  "Current GUI bridge argument for help commands.")

(defvar emacs-help-gui-current-file-name ""
  "Current GUI bridge file name used by help descriptions.")

(defvar emacs-help-gui-buffer-name ""
  "Current GUI bridge buffer name used by help descriptions.")

(defvar emacs-help-gui-buffer-read-only-p nil
  "Current GUI bridge read-only flag used by help descriptions.")

(defvar emacs-help-gui-window-layout ""
  "Current GUI bridge window layout used by help descriptions.")

(defvar emacs-help-gui-keymap-source ""
  "Tab-separated keymap source for GUI help lookup.")

(defvar emacs-help-gui-user-keymap-source ""
  "Tab-separated user keymap source for GUI help lookup.")

(defvar emacs-help-gui-minibuffer-keymap-source ""
  "Tab-separated minibuffer keymap source for GUI help lookup.")

(defvar emacs-help-gui-status "ok"
  "Last GUI help command status.")

(defun emacs-help-gui-register-backend (&rest backend)
  "Register BACKEND plist for GUI help display."
  (setq emacs-help-gui-backend backend))

(defun emacs-help-gui--backend-call (key &rest args)
  "Call GUI help backend KEY with ARGS when available."
  (let ((fn (and emacs-help-gui-backend
                 (plist-get emacs-help-gui-backend key))))
    (when fn
      (apply fn args))))

(defun emacs-help-gui--backend-function (key)
  "Return GUI help backend function KEY, or nil."
  (and emacs-help-gui-backend
       (plist-get emacs-help-gui-backend key)))

(defun emacs-help-gui-set-context (&rest plist)
  "Set GUI help context from PLIST."
  (when (plist-member plist :arg)
    (setq emacs-help-gui-arg (plist-get plist :arg)))
  (when (plist-member plist :current-file-name)
    (setq emacs-help-gui-current-file-name
          (plist-get plist :current-file-name)))
  (when (plist-member plist :buffer-name)
    (setq emacs-help-gui-buffer-name (plist-get plist :buffer-name)))
  (when (plist-member plist :buffer-read-only-p)
    (setq emacs-help-gui-buffer-read-only-p
          (plist-get plist :buffer-read-only-p)))
  (when (plist-member plist :window-layout)
    (setq emacs-help-gui-window-layout (plist-get plist :window-layout)))
  (when (plist-member plist :keymap-source)
    (setq emacs-help-gui-keymap-source (plist-get plist :keymap-source)))
  (when (plist-member plist :user-keymap-source)
    (setq emacs-help-gui-user-keymap-source
          (plist-get plist :user-keymap-source)))
  (when (plist-member plist :minibuffer-keymap-source)
    (setq emacs-help-gui-minibuffer-keymap-source
          (plist-get plist :minibuffer-keymap-source)))
  (when (plist-member plist :status)
    (setq emacs-help-gui-status (plist-get plist :status)))
  emacs-help-gui-buffer-name)

(defun emacs-help-gui-refresh-context-from-backend ()
  "Refresh GUI help context from registered backend callbacks."
  (let ((arg (emacs-help-gui--backend-function :current-arg))
        (current-file-name
         (emacs-help-gui--backend-function :current-file-name))
        (buffer-name (emacs-help-gui--backend-function :buffer-name))
        (buffer-read-only-p
         (emacs-help-gui--backend-function :buffer-read-only-p))
        (window-layout (emacs-help-gui--backend-function :window-layout))
        (keymap-source (emacs-help-gui--backend-function :keymap-source))
        (user-keymap-source
         (emacs-help-gui--backend-function :user-keymap-source))
        (minibuffer-keymap-source
         (emacs-help-gui--backend-function :minibuffer-keymap-source))
        (status (emacs-help-gui--backend-function :current-status)))
    (when arg
      (setq emacs-help-gui-arg (funcall arg)))
    (when current-file-name
      (setq emacs-help-gui-current-file-name
            (funcall current-file-name)))
    (when buffer-name
      (setq emacs-help-gui-buffer-name (funcall buffer-name)))
    (when buffer-read-only-p
      (setq emacs-help-gui-buffer-read-only-p
            (funcall buffer-read-only-p)))
    (when window-layout
      (setq emacs-help-gui-window-layout (funcall window-layout)))
    (when keymap-source
      (setq emacs-help-gui-keymap-source (funcall keymap-source)))
    (when user-keymap-source
      (setq emacs-help-gui-user-keymap-source
            (funcall user-keymap-source)))
    (when minibuffer-keymap-source
      (setq emacs-help-gui-minibuffer-keymap-source
            (funcall minibuffer-keymap-source)))
    (when status
      (setq emacs-help-gui-status (funcall status))))
  (list :arg emacs-help-gui-arg
        :current-file-name emacs-help-gui-current-file-name
        :buffer-name emacs-help-gui-buffer-name
        :buffer-read-only-p emacs-help-gui-buffer-read-only-p
        :window-layout emacs-help-gui-window-layout
        :keymap-source emacs-help-gui-keymap-source
        :user-keymap-source emacs-help-gui-user-keymap-source
        :minibuffer-keymap-source emacs-help-gui-minibuffer-keymap-source
        :status emacs-help-gui-status))

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

(defvar emacs-help--nav-back nil
  "Stack of previous help rerender thunks (most recent first).")
(defvar emacs-help--nav-forward nil
  "Stack of forward help rerender thunks (next first).")
(defvar emacs-help--nav-current nil
  "Rerender thunk for the help topic currently on display.")
(defvar emacs-help--navigating nil
  "Non-nil while replaying a history entry, so it is not re-recorded.")

(defun emacs-help--history-record (rerender)
  "Record RERENDER in the help navigation history.
A normal topic push moves the current topic onto the back stack and
clears the forward stack; replays driven by `help-go-back' /
`help-go-forward' (when `emacs-help--navigating' is non-nil) are not
recorded so navigation stays stable."
  (unless emacs-help--navigating
    (when emacs-help--nav-current
      (push emacs-help--nav-current emacs-help--nav-back))
    (setq emacs-help--nav-current rerender
          emacs-help--nav-forward nil)))

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
    (emacs-help--history-record rerender)
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

(defun emacs-help-gui--show-help-buffer (title body)
  "Show GUI help buffer with TITLE and BODY."
  (setq emacs-help-gui-status "ok")
  (or (emacs-help-gui--backend-call :show-help-buffer title body)
      (let ((buffer (emacs-help--buffer)))
        (with-current-buffer buffer
          (help-mode)
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert title "\n\n" body)
            (unless (string-suffix-p "\n" body)
              (insert "\n"))
            (goto-char (point-min))
            (setq buffer-read-only t)))
        (emacs-help--show-buffer buffer)
        emacs-help--buffer-name)))

(defun emacs-help-gui-show-help-buffer (title body)
  "Show GUI help buffer with TITLE and BODY.
This is the stable bridge-facing wrapper around the runtime Help buffer
display core."
  (emacs-help-gui--show-help-buffer title body))

(defun emacs-help-gui--run-core-current-context (core)
  "Refresh GUI help context, run CORE, and render its title/body pair."
  (emacs-help-gui-refresh-context-from-backend)
  (let ((entry (funcall core)))
    (emacs-help-gui--show-help-buffer (car entry) (cdr entry))))

(defun emacs-help-gui--normalize-arg (&optional arg)
  "Return ARG or the current GUI help argument, defaulting to unknown."
  (let ((value (or arg emacs-help-gui-arg)))
    (if (equal value "") "unknown" value)))

(defun emacs-help-gui--line-field (line index)
  "Return field INDEX from tab-separated LINE."
  (let ((i 0)
        (start 0)
        (field 0)
        (out ""))
    (while (<= i (length line))
      (if (or (= i (length line))
              (= (aref line i) 9))
          (progn
            (when (= field index)
              (setq out (substring line start i)))
            (setq field (1+ field))
            (setq start (1+ i)))
        nil)
      (setq i (1+ i)))
    out))

(defun emacs-help-gui--each-keymap-line (source fn)
  "Call FN for each non-empty line in tab-separated keymap SOURCE."
  (let ((i 0)
        (start 0))
    (while (<= i (length source))
      (if (or (= i (length source))
              (= (aref source i) 10))
          (let ((line (substring source start i)))
            (unless (equal line "")
              (funcall fn line))
            (setq start (1+ i)))
        nil)
      (setq i (1+ i)))))

(defun emacs-help-gui--lookup-key-command (source key)
  "Return command bound to KEY in tab-separated SOURCE."
  (let ((backend-result
         (emacs-help-gui--backend-call :lookup-key-command source key)))
    (if backend-result
        backend-result
      (let ((found "")
        (index 0)
        (start 0)
        (source-len (length source))
        (key-len (length key))
        (match nil)
        (j 0)
        (command-start 0)
        (command-end 0))
    (while (and (equal found "")
                (<= index source-len))
      (if (or (= index source-len)
              (= (aref source index) 10))
          (progn
            (setq match nil)
            (if (and (< (+ start key-len) index)
                     (= (aref source (+ start key-len)) 9))
                (progn
                  (setq match t)
                  (setq j 0)
                  (while (and match (< j key-len))
                    (if (= (aref source (+ start j)) (aref key j))
                        nil
                      (setq match nil))
                    (setq j (1+ j)))))
            (when match
              (setq command-start (1+ (+ start key-len)))
              (setq command-end command-start)
              (while (and (< command-end index)
                          (not (= (aref source command-end) 9)))
                (setq command-end (1+ command-end)))
              (setq found (substring source command-start command-end)))
            (setq start (1+ index)))
        nil)
      (setq index (1+ index)))
        found))))

(defun emacs-help-gui--binding-list (source)
  "Return binding list text from tab-separated SOURCE."
  (let ((out ""))
    (emacs-help-gui--each-keymap-line
     source
     (lambda (line)
       (let ((key (emacs-help-gui--line-field line 0))
             (command (emacs-help-gui--line-field line 1)))
         (unless (equal key "")
           (setq out (concat out key "\t" command "\n"))))))
    out))

(defun emacs-help-gui--keys-for-command (source command)
  "Return comma-separated keys for COMMAND in tab-separated SOURCE."
  (let ((out ""))
    (emacs-help-gui--each-keymap-line
     source
     (lambda (line)
       (let ((key (emacs-help-gui--line-field line 0))
             (bound (emacs-help-gui--line-field line 1)))
         (when (equal bound command)
           (setq out (concat out (if (equal out "") "" ", ") key))))))
    out))

(defun emacs-help-gui--command-doc (name)
  "Return short bridge-compatible documentation for command NAME."
  (cond
   ((equal name "forward-char")
    "Move point one character forward in the current buffer.")
   ((equal name "backward-char")
    "Move point one character backward in the current buffer.")
   ((equal name "find-file")
    "Visit the file named by the bridge argument and make it the current buffer.")
   ((equal name "find-file-read-only")
    "Visit the file named by the bridge argument and mark the buffer read-only.")
   ((equal name "save-buffer")
    "Save the current buffer to its visited file.")
   ((equal name "execute-extended-command")
    "Read a command name from the minibuffer argument and execute that command.")
   ((equal name "goto-line")
    "Move point to the beginning of the requested line.")
   ((equal name "switch-to-buffer")
    "Select the buffer named by the bridge argument, creating it if needed.")
   ((equal name "kill-buffer")
    "Remove the buffer named by the bridge argument from the buffer list.")
   ((equal name "list-buffers")
    "Display the current bridge buffer list.")
   ((equal name "sort-lines")
    "Sort the lines in the active region alphabetically.")
   ((equal name "kill-whole-line")
    "Kill the entire current line, including its trailing newline when present.")
   (t
    "This function is known to the GUI bridge runtime, but no detailed documentation is available yet.")))

(defun emacs-help-gui--variable-info (name)
  "Return cons of value string and documentation string for variable NAME."
  (cond
   ((equal name "buffer-file-name")
    (cons (if (equal emacs-help-gui-current-file-name "")
              "nil"
            emacs-help-gui-current-file-name)
          "The visited file name of the current buffer, or nil when the buffer is not visiting a file."))
   ((equal name "buffer-read-only")
    (cons (if emacs-help-gui-buffer-read-only-p "t" "nil")
          "Non-nil means the current buffer rejects editing commands."))
   ((equal name "files--buffer-name")
    (cons emacs-help-gui-buffer-name
          "Bridge runtime name of the current buffer."))
   ((equal name "files--current-file-name")
    (cons (if (equal emacs-help-gui-current-file-name "")
              "nil"
            emacs-help-gui-current-file-name)
          "Bridge runtime visited file name of the current buffer."))
   ((equal name "files--buffer-read-only-p")
    (cons (if emacs-help-gui-buffer-read-only-p "t" "nil")
          "Bridge runtime read-only flag for the current buffer."))
   ((equal name "files--window-layout")
    (cons emacs-help-gui-window-layout
          "Bridge runtime window layout state returned to the GUI."))
   (t
    (cons "void"
          "This variable is known to the GUI bridge runtime, but no detailed documentation is available yet."))))

(defun emacs-help-gui-describe-function-core (&optional name)
  "Return GUI help title/body pair for function NAME."
  (let* ((fname (emacs-help-gui--normalize-arg name))
         (body (concat fname
                       " is a function.\n\n"
                       "Documentation:\n"
                       (emacs-help-gui--command-doc fname)
                       "\n")))
    (cons fname body)))

(defun emacs-help-gui-describe-function (&optional name)
  "Render GUI help for function NAME."
  (let ((entry (emacs-help-gui-describe-function-core name)))
    (emacs-help-gui--show-help-buffer (car entry) (cdr entry))))

(defun emacs-help-gui-describe-variable-core (&optional name)
  "Return GUI help title/body pair for variable NAME."
  (let* ((vname (emacs-help-gui--normalize-arg name))
         (info (emacs-help-gui--variable-info vname)))
    (cons vname
          (concat vname
                  " is a variable.\n\n"
                  "Value: "
                  (car info)
                  "\n\n"
                  "Documentation:\n"
                  (cdr info)
                  "\n"))))

(defun emacs-help-gui-describe-variable (&optional name)
  "Render GUI help for variable NAME."
  (let ((entry (emacs-help-gui-describe-variable-core name)))
    (emacs-help-gui--show-help-buffer (car entry) (cdr entry))))


(defun emacs-help-gui-describe-key-core (&optional key)
  "Return GUI help title/body pair for KEY."
  (let* ((k (emacs-help-gui--normalize-arg key))
         (source (concat emacs-help-gui-user-keymap-source
                         emacs-help-gui-keymap-source))
         (command (emacs-help-gui--lookup-key-command source k))
         (doc "This key is not bound to a bridge command in the current GUI runtime."))
    (when (equal command "")
      (setq command
            (emacs-help-gui--lookup-key-command
             emacs-help-gui-minibuffer-keymap-source k)))
    (unless (equal command "")
      (setq doc "This key is resolved through the GUI bridge keymap."))
    (cons k
          (if (equal command "")
              (concat k
                      " is not bound to a bridge command.\n\n"
                      "Documentation:\n"
                      doc
                      "\n")
            (concat k
                    " runs the command "
                    command
                    ".\n\n"
                    "Documentation:\n"
                    doc
                    "\n")))))

(defun emacs-help-gui-describe-key (&optional key)
  "Render GUI help for KEY."
  (let ((entry (emacs-help-gui-describe-key-core key)))
    (emacs-help-gui--show-help-buffer (car entry) (cdr entry))))

(defun emacs-help-gui-describe-key-briefly-core (&optional key)
  "Return short GUI help title/body pair for KEY."
  (let* ((k (emacs-help-gui--normalize-arg key))
         (command (emacs-help-gui--lookup-key-command
                   emacs-help-gui-keymap-source k)))
    (when (equal command "")
      (setq command
            (emacs-help-gui--lookup-key-command
             emacs-help-gui-minibuffer-keymap-source k)))
    (cons k
          (if (equal command "")
              (concat k " is undefined\n")
            (concat k " runs the command " command "\n")))))

(defun emacs-help-gui-describe-key-briefly (&optional key)
  "Render short GUI help for KEY."
  (let ((entry (emacs-help-gui-describe-key-briefly-core key)))
    (emacs-help-gui--show-help-buffer (car entry) (cdr entry))))

(defun emacs-help-gui-describe-bindings-core ()
  "Return GUI help title/body pair for current key bindings."
  (let ((bindings (concat
                   (emacs-help-gui--binding-list
                    (concat emacs-help-gui-user-keymap-source
                            emacs-help-gui-keymap-source))
                   (emacs-help-gui--binding-list
                    emacs-help-gui-minibuffer-keymap-source))))
    (cons "Key Bindings"
          (concat "Key bindings in the current GUI runtime:\n\n" bindings))))

(defun emacs-help-gui-describe-bindings ()
  "Render GUI help for current key bindings."
  (let ((entry (emacs-help-gui-describe-bindings-core)))
    (emacs-help-gui--show-help-buffer (car entry) (cdr entry))))

(defun emacs-help-gui-help-for-help ()
  "Render GUI Help-for-Help buffer."
  (emacs-help-gui--show-help-buffer
   "Help"
   (concat "Help commands in the current GUI runtime:\n\n"
           "C-h b\tdescribe-bindings\n"
           "C-h c\tdescribe-key-briefly\n"
           "C-h f\tdescribe-function\n"
           "C-h k\tdescribe-key\n"
           "C-h v\tdescribe-variable\n"
           "C-h w\twhere-is\n"
           "C-h x\tdescribe-command\n"
           "C-h C-a\tabout-emacs\n"
           "C-h C-c\tdescribe-copying\n"
           "C-h C-n\tview-emacs-news\n"
           "C-h n\tview-emacs-news\n"
           "C-h C-f\tview-emacs-FAQ\n"
           "C-h g\tdescribe-gnu-project\n"
           "C-h ?\thelp-for-help\n"
           "C-h C-h\thelp-for-help\n")))

(defun emacs-help-gui-where-is-core (&optional command)
  "Return GUI `where-is' title/body pair for COMMAND."
  (let* ((cmd (emacs-help-gui--normalize-arg command))
         (primary (emacs-help-gui--keys-for-command
                   (concat emacs-help-gui-user-keymap-source
                           emacs-help-gui-keymap-source)
                   cmd))
         (minor (emacs-help-gui--keys-for-command
                 emacs-help-gui-minibuffer-keymap-source cmd))
         (keys (concat primary
                       (if (or (equal primary "") (equal minor ""))
                           ""
                         ", ")
                       minor)))
    (cons cmd
          (if (equal keys "")
              (concat cmd " is not on any key\n")
            (concat cmd " is on " keys "\n")))))

(defun emacs-help-gui-where-is (&optional command)
  "Render GUI `where-is' result for COMMAND."
  (let ((entry (emacs-help-gui-where-is-core command)))
    (emacs-help-gui--show-help-buffer (car entry) (cdr entry))))

(defun emacs-help-gui-describe-command (&optional command)
  "Render GUI help for command COMMAND."
  (emacs-help-gui-describe-function command))

(defun emacs-help-gui-describe-symbol (&optional symbol)
  "Render GUI help for SYMBOL."
  (let ((name (emacs-help-gui--normalize-arg symbol)))
    (emacs-help-gui--show-help-buffer
     "Describe Symbol"
     (concat name
             " is a symbol known to the GUI bridge help surface.  Detailed function/variable lookup is provided by describe-function and describe-variable where implemented."))))

(defun emacs-help-gui-describe-package (&optional package)
  "Render GUI help for PACKAGE."
  (let ((name (emacs-help-gui--normalize-arg package)))
    (emacs-help-gui--show-help-buffer
     (concat "Package: " name)
     (concat name
             " is a package name requested through the Help surface.\nFull package metadata lookup is not yet connected."))))

(defun emacs-help-gui-static-command (command)
  "Render static GUI help for COMMAND."
  (let ((title "")
        (body ""))
    (pcase command
      ('about-emacs
       (setq title "About GNU Emacs")
       (setq body "GNU Emacs is the extensible, customizable, self-documenting editor.  This nemacs bridge runtime provides an Emacs-compatible help buffer for the native GUI replacement path."))
      ('describe-copying
       (setq title "GNU Emacs Copying Conditions")
       (setq body "GNU Emacs is free software.  You may redistribute and/or modify it under the terms of the GNU General Public License.  This bridge help text is a compact compatibility summary."))
      ('view-emacs-debugging
       (setq title "GNU Emacs Debugging")
       (setq body "Emacs provides debugging tools such as backtraces, debuggers, and bug reporting support.  The nemacs GUI bridge keeps command semantics in nelisp-emacs so failures can be isolated there."))
      ('view-external-packages
       (setq title "External Packages")
       (setq body "External packages extend Emacs.  Package management UI is not yet implemented in this GUI bridge runtime; this command records the expected Help buffer behavior."))
      ('view-emacs-FAQ
       (setq title "GNU Emacs FAQ")
       (setq body "The GNU Emacs FAQ answers common questions about using and configuring Emacs.  Full Info/manual navigation is a future nelisp-emacs feature."))
      ('view-emacs-news
       (setq title "GNU Emacs News")
       (setq body "Emacs news normally lists recent user-visible changes.  This runtime exposes the Help command path while detailed release notes are not yet loaded."))
      ('describe-distribution
       (setq title "GNU Emacs Distribution")
       (setq body "GNU Emacs is distributed by the GNU Project.  The nemacs replacement path keeps distribution/help command semantics in nelisp-emacs."))
      ('view-emacs-problems
       (setq title "GNU Emacs Known Problems")
       (setq body "Known problems are normally documented with the Emacs distribution.  This bridge command opens a read-only Help buffer as the compatibility surface."))
      ('view-emacs-todo
       (setq title "GNU Emacs TODO")
       (setq body "The Emacs TODO file tracks planned work.  Full distribution file viewing is not yet implemented in this GUI bridge runtime."))
      ('describe-no-warranty
       (setq title "GNU Emacs No Warranty")
       (setq body "GNU Emacs is distributed in the hope that it will be useful, but without warranty.  See the GNU General Public License for the complete terms."))
      ('describe-gnu-project
       (setq title "About the GNU Project")
       (setq body "The GNU Project develops the GNU operating system and free software, including GNU Emacs."))
      ('view-hello-file
       (setq title "Hello")
       (setq body "Hello from GNU Emacs.  Multilingual hello text is not yet bundled in this bridge runtime."))
      ('describe-coding-system
       (setq title "Coding System")
       (setq body "Coding system inspection is not yet connected to the full Emacs coding database.  This bridge command records the Help buffer behavior in nelisp-emacs."))
      ('describe-input-method
       (setq title "Input Method")
       (setq body "Input method descriptions are not yet backed by the full Emacs input method registry.  GUI input decoding remains in nelisp-gui; input method semantics belong in nelisp-emacs."))
      ('describe-language-environment
       (setq title "Language Environment")
       (setq body "Language environment details are not yet loaded from Emacs data files.  This command provides the expected read-only Help buffer surface."))
      ('view-lossage
       (setq title "Recent Keys")
       (setq body "Recent key lossage is not yet persisted by the GUI bridge runtime."))
      ('describe-mode
       (setq title "Mode Help")
       (setq body (concat "Major mode: Fundamental\nBuffer: "
                          emacs-help-gui-buffer-name
                          "\nThe current GUI bridge runtime exposes a minimal mode description.")))
      ('help-quit
       (setq title "Help Quit")
       (setq body "Help quit was requested.  Window closing is not modeled here; the command is represented as a read-only Help buffer update."))
      ('describe-syntax
       (setq title "Syntax Table")
       (setq body "Syntax table details are not yet backed by full Emacs syntax table data.  Word/symbol movement currently uses the bridge runtime character predicates."))
      ('help-with-tutorial
       (setq title "Emacs Tutorial")
       (setq body "The full Emacs tutorial is not yet bundled in this bridge runtime.  This command opens the expected read-only Help buffer."))
      ('display-local-help
       (setq title "Local Help")
       (setq body "Local contextual help was requested.  The GUI bridge runtime represents the request as a read-only Help buffer; widget and text-property help lookup remains a nelisp-emacs task."))
      ('help-find-source
       (setq title "Find Source")
       (setq body "Source lookup for the current help target is not yet backed by full symbol-to-source metadata.  This command opens the expected Help surface without adding GUI-side command semantics."))
      ('help-quick-toggle
       (setq title "Quick Help Toggle")
       (setq body "Quick help display toggling is represented in the bridge runtime as a Help buffer update.  Help window display policy remains owned by nelisp-emacs."))
      ('search-forward-help-for-help
       (setq title "Search Help")
       (setq body "Search within the Help-for-Help buffer was requested.  Full incremental help search is not yet implemented in the bridge runtime."))
      ('finder-by-keyword
       (setq title "Package Finder")
       (setq body "Package keyword browsing is not yet backed by the full package index.  This command opens the expected read-only Help buffer."))
      (_
       (setq title (symbol-name command))
       (setq body "This help command is recognized but does not have detailed text yet.")))
    (emacs-help-gui--show-help-buffer title body)))

(defun emacs-help-gui-apropos-command (&optional pattern)
  "Render GUI apropos-command help for PATTERN."
  (let ((arg (or pattern emacs-help-gui-arg)))
    (emacs-help-gui--show-help-buffer
     "Apropos Commands"
     (concat "Apropos command search is not yet backed by the full command index.\nPattern: "
             arg))))

(defun emacs-help-gui-apropos-documentation (&optional pattern)
  "Render GUI apropos-documentation help for PATTERN."
  (let ((arg (or pattern emacs-help-gui-arg)))
    (emacs-help-gui--show-help-buffer
     "Apropos Documentation"
     (concat "Apropos documentation search is not yet backed by the full documentation index.\nPattern: "
             arg))))

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
  "Go back to the previously shown help topic."
  (interactive)
  (unless emacs-help--nav-back
    (user-error "No previous help"))
  (push emacs-help--nav-current emacs-help--nav-forward)
  (setq emacs-help--nav-current (pop emacs-help--nav-back))
  (let ((emacs-help--navigating t))
    (funcall emacs-help--nav-current)))

(defun help-go-forward ()
  "Go forward to the next help topic in the history."
  (interactive)
  (unless emacs-help--nav-forward
    (user-error "No next help"))
  (push emacs-help--nav-current emacs-help--nav-back)
  (setq emacs-help--nav-current (pop emacs-help--nav-forward))
  (let ((emacs-help--navigating t))
    (funcall emacs-help--nav-current)))

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

(defun emacs-help-gui-describe-function-current-context-command ()
  "Refresh GUI help context, then run `describe-function'."
  (emacs-help-gui--run-core-current-context
   'emacs-help-gui-describe-function-core))

(defun emacs-help-gui-describe-variable-current-context-command ()
  "Refresh GUI help context, then run `describe-variable'."
  (emacs-help-gui--run-core-current-context
   'emacs-help-gui-describe-variable-core))

(defun emacs-help-gui-describe-key-current-context-command ()
  "Refresh GUI help context, then run `describe-key'."
  (emacs-help-gui--run-core-current-context
   'emacs-help-gui-describe-key-core))

(defun emacs-help-gui-describe-key-briefly-current-context-command ()
  "Refresh GUI help context, then run `describe-key-briefly'."
  (emacs-help-gui--run-core-current-context
   'emacs-help-gui-describe-key-briefly-core))

(defun emacs-help-gui-describe-bindings-current-context-command ()
  "Refresh GUI help context, then run `describe-bindings'."
  (emacs-help-gui--run-core-current-context
   'emacs-help-gui-describe-bindings-core))

(defun emacs-help-gui-where-is-current-context-command ()
  "Refresh GUI help context, then run `where-is'."
  (emacs-help-gui--run-core-current-context
   'emacs-help-gui-where-is-core))

(defun emacs-help-gui-help-for-help-current-context-command ()
  "Refresh GUI help context, then run `help-for-help'."
  (emacs-help-gui-refresh-context-from-backend)
  (emacs-help-gui-help-for-help))

(defun emacs-help-gui-describe-command-current-context-command ()
  "Refresh GUI help context, then run `describe-command'."
  (emacs-help-gui-refresh-context-from-backend)
  (emacs-help-gui-describe-command))

(defun emacs-help-gui-describe-package-current-context-command ()
  "Refresh GUI help context, then run `describe-package'."
  (emacs-help-gui-refresh-context-from-backend)
  (emacs-help-gui-describe-package))

(defun emacs-help-gui-static-current-context-command (command)
  "Refresh GUI help context, then render static help COMMAND."
  (emacs-help-gui-refresh-context-from-backend)
  (emacs-help-gui-static-command command))

(defun emacs-help-gui-apropos-command-current-context-command ()
  "Refresh GUI help context, then run `apropos-command'."
  (emacs-help-gui-refresh-context-from-backend)
  (emacs-help-gui-apropos-command))

(defun emacs-help-gui-apropos-documentation-current-context-command ()
  "Refresh GUI help context, then run `apropos-documentation'."
  (emacs-help-gui-refresh-context-from-backend)
  (emacs-help-gui-apropos-documentation))

(defun emacs-help-gui-writeback-spec (&optional command)
  "Return GUI transport writeback spec for Help COMMAND.
This describes the Help buffer state; the GUI bridge remains responsible
for concrete transport file writes."
  (let ((command (if (symbolp command)
                     (symbol-name command)
                   (if (stringp command) command "")))
        (result nil))
    (when (member command
                  '("describe-function" "describe-variable"
                    "describe-key" "describe-key-briefly"
                    "describe-bindings" "help-for-help"
                    "where-is" "describe-command"
                    "what-cursor-position"))
      (setq result
            '(:buffer t :file t :buffer-name t :read-only t
              :point t :mark t :window-start t)))
    (when (member command
                  '("about-emacs" "describe-copying"
                    "view-emacs-debugging" "view-external-packages"
                    "view-emacs-FAQ" "view-emacs-news"
                    "describe-distribution" "view-emacs-problems"
                    "view-emacs-todo" "describe-no-warranty"
                    "describe-gnu-project" "view-hello-file"
                    "describe-coding-system" "describe-input-method"
                    "describe-language-environment" "apropos-command"
                    "apropos-documentation" "view-echo-area-messages"
                    "view-lossage" "describe-mode" "describe-symbol"
                    "help-quit" "describe-syntax"
                    "help-with-tutorial" "display-local-help"
                    "help-find-source" "help-quick-toggle"
                    "search-forward-help-for-help"
                    "xref-go-back" "xref-go-forward"
                    "xref-find-definitions" "xref-find-references"
                    "xref-find-apropos"
                    "xref-find-definitions-other-window"
                    "xref-find-definitions-other-frame"
                    "repeat-complex-command" "describe-package"
                    "finder-by-keyword"))
      (setq result
            '(:buffer t :file t :buffer-name t :read-only t
              :window t :point t :mark t :window-start t)))
    result))

;;;###autoload
(defun emacs-help-gui-writeback-spec-flag (spec key)
  "Return non-nil when Help GUI writeback SPEC enables KEY."
  (and spec (plist-get spec key)))

;;;###autoload
(defun emacs-help-gui-writeback-state (&optional command)
  "Write GUI transport state for Help COMMAND.
The Help runtime owns writeback spec interpretation and callback ordering.
The registered backend owns concrete transport writes.  Return non-nil when
COMMAND has a Help writeback spec."
  (let ((spec (emacs-help-gui-writeback-spec command)))
    (when spec
      (when (emacs-help-gui-writeback-spec-flag spec :buffer)
        (emacs-help-gui--backend-call :write-buffer-state))
      (when (emacs-help-gui-writeback-spec-flag spec :file)
        (emacs-help-gui--backend-call :write-file-state))
      (when (emacs-help-gui-writeback-spec-flag spec :buffer-name)
        (emacs-help-gui--backend-call :write-buffer-name-state))
      (when (emacs-help-gui-writeback-spec-flag spec :read-only)
        (emacs-help-gui--backend-call :write-read-only-state))
      (when (emacs-help-gui-writeback-spec-flag spec :window)
        (emacs-help-gui--backend-call :write-window-state))
      (when (emacs-help-gui-writeback-spec-flag spec :point)
        (emacs-help-gui--backend-call :write-point-state))
      (when (emacs-help-gui-writeback-spec-flag spec :mark)
        (emacs-help-gui--backend-call :write-mark-state))
      (when (emacs-help-gui-writeback-spec-flag spec :window-start)
        (emacs-help-gui--backend-call :write-window-start-state))
      (emacs-help-gui--backend-call :mark-written-state)
      t)))

;;;###autoload
(defun emacs-help-gui-current-context-command
    (command &optional static-command)
  "Refresh GUI help context and run Help COMMAND.
STATIC-COMMAND, when non-nil, is rendered through
`emacs-help-gui-static-command'."
  (cond
   ((eq command 'describe-function)
    (emacs-help-gui-describe-function-current-context-command))
   ((eq command 'describe-variable)
    (emacs-help-gui-describe-variable-current-context-command))
   ((eq command 'describe-key)
    (emacs-help-gui-describe-key-current-context-command))
   ((eq command 'describe-key-briefly)
    (emacs-help-gui-describe-key-briefly-current-context-command))
   ((eq command 'describe-bindings)
    (emacs-help-gui-describe-bindings-current-context-command))
   ((eq command 'where-is)
    (emacs-help-gui-where-is-current-context-command))
   ((eq command 'help-for-help)
    (emacs-help-gui-help-for-help-current-context-command))
   ((eq command 'describe-command)
    (emacs-help-gui-describe-command-current-context-command))
   ((eq command 'describe-package)
    (emacs-help-gui-describe-package-current-context-command))
   ((eq command 'describe-symbol)
    (emacs-help-gui-refresh-context-from-backend)
    (emacs-help-gui-describe-symbol))
   ((eq command 'apropos-command)
    (emacs-help-gui-apropos-command-current-context-command))
   ((eq command 'apropos-documentation)
    (emacs-help-gui-apropos-documentation-current-context-command))
   (static-command
    (emacs-help-gui-static-current-context-command static-command))
   (t nil)))

(provide 'emacs-help)

;;; emacs-help.el ends here
