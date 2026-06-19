;;; emacs-help-gui.el --- GUI bridge Help adapter  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Lightweight GUI bridge adapter for Help commands.  The full Help runtime
;; depends on keymap, mode, and buffer libraries; this adapter intentionally
;; keeps only the transport-facing command surface so it can be baked into the
;; standalone GUI bridge image.

;;; Code:

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

;;;###autoload
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

;;;###autoload
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

;;;###autoload
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

(defun emacs-help-gui--show-help-buffer (title body)
  "Show GUI help buffer with TITLE and BODY."
  (setq emacs-help-gui-status "ok")
  (or (emacs-help-gui--backend-call :show-help-buffer title body)
      (progn
        (setq emacs-help-gui-status "unsupported")
        nil)))

;;;###autoload
(defun emacs-help-gui-show-help-buffer (title body)
  "Show GUI help buffer with TITLE and BODY."
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

;;;###autoload
(defun emacs-help-gui-describe-function-core (&optional name)
  "Return GUI help title/body pair for function NAME."
  (let* ((fname (emacs-help-gui--normalize-arg name))
         (body (concat fname
                       " is a function.\n\n"
                       "Documentation:\n"
                       (emacs-help-gui--command-doc fname)
                       "\n")))
    (cons fname body)))

;;;###autoload
(defun emacs-help-gui-describe-function (&optional name)
  "Render GUI help for function NAME."
  (let ((entry (emacs-help-gui-describe-function-core name)))
    (emacs-help-gui--show-help-buffer (car entry) (cdr entry))))

;;;###autoload
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

;;;###autoload
(defun emacs-help-gui-describe-variable (&optional name)
  "Render GUI help for variable NAME."
  (let ((entry (emacs-help-gui-describe-variable-core name)))
    (emacs-help-gui--show-help-buffer (car entry) (cdr entry))))

;;;###autoload
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

;;;###autoload
(defun emacs-help-gui-describe-key (&optional key)
  "Render GUI help for KEY."
  (let ((entry (emacs-help-gui-describe-key-core key)))
    (emacs-help-gui--show-help-buffer (car entry) (cdr entry))))

;;;###autoload
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

;;;###autoload
(defun emacs-help-gui-describe-key-briefly (&optional key)
  "Render short GUI help for KEY."
  (let ((entry (emacs-help-gui-describe-key-briefly-core key)))
    (emacs-help-gui--show-help-buffer (car entry) (cdr entry))))

;;;###autoload
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

;;;###autoload
(defun emacs-help-gui-describe-bindings ()
  "Render GUI help for current key bindings."
  (let ((entry (emacs-help-gui-describe-bindings-core)))
    (emacs-help-gui--show-help-buffer (car entry) (cdr entry))))

;;;###autoload
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

;;;###autoload
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

;;;###autoload
(defun emacs-help-gui-where-is (&optional command)
  "Render GUI `where-is' result for COMMAND."
  (let ((entry (emacs-help-gui-where-is-core command)))
    (emacs-help-gui--show-help-buffer (car entry) (cdr entry))))

;;;###autoload
(defun emacs-help-gui-describe-command (&optional command)
  "Render GUI help for command COMMAND."
  (emacs-help-gui-describe-function command))

;;;###autoload
(defun emacs-help-gui-describe-symbol (&optional symbol)
  "Render GUI help for SYMBOL."
  (let ((name (emacs-help-gui--normalize-arg symbol)))
    (emacs-help-gui--show-help-buffer
     "Describe Symbol"
     (concat name
             " is a symbol known to the GUI bridge help surface.  Detailed function/variable lookup is provided by describe-function and describe-variable where implemented."))))

;;;###autoload
(defun emacs-help-gui-describe-package (&optional package)
  "Render GUI help for PACKAGE."
  (let ((name (emacs-help-gui--normalize-arg package)))
    (emacs-help-gui--show-help-buffer
     (concat "Package: " name)
     (concat name
             " is a package name requested through the Help surface.\nFull package metadata lookup is not yet connected."))))

(defun emacs-help-gui--static-text (command)
  "Return cons of title and body for static help COMMAND."
  (cond
   ((eq command 'about-emacs)
    (cons "About GNU Emacs"
          "GNU Emacs is the extensible, customizable, self-documenting editor.  This nemacs bridge runtime provides an Emacs-compatible help buffer for the native GUI replacement path."))
   ((eq command 'describe-copying)
    (cons "GNU Emacs Copying Conditions"
          "GNU Emacs is free software.  You may redistribute and/or modify it under the terms of the GNU General Public License.  This bridge help text is a compact compatibility summary."))
   ((eq command 'view-emacs-debugging)
    (cons "GNU Emacs Debugging"
          "Emacs provides debugging tools such as backtraces, debuggers, and bug reporting support.  The nemacs GUI bridge keeps command semantics in nelisp-emacs so failures can be isolated there."))
   ((eq command 'view-external-packages)
    (cons "External Packages"
          "External packages extend Emacs.  Package management UI is not yet implemented in this GUI bridge runtime; this command records the expected Help buffer behavior."))
   ((eq command 'view-emacs-FAQ)
    (cons "GNU Emacs FAQ"
          "The GNU Emacs FAQ answers common questions about using and configuring Emacs.  Full Info/manual navigation is a future nelisp-emacs feature."))
   ((eq command 'view-emacs-news)
    (cons "GNU Emacs News"
          "Emacs news normally lists recent user-visible changes.  This runtime exposes the Help command path while detailed release notes are not yet loaded."))
   ((eq command 'describe-distribution)
    (cons "GNU Emacs Distribution"
          "GNU Emacs is distributed by the GNU Project.  The nemacs replacement path keeps distribution/help command semantics in nelisp-emacs."))
   ((eq command 'view-emacs-problems)
    (cons "GNU Emacs Known Problems"
          "Known problems are normally documented with the Emacs distribution.  This bridge command opens a read-only Help buffer as the compatibility surface."))
   ((eq command 'view-emacs-todo)
    (cons "GNU Emacs TODO"
          "The Emacs TODO file tracks planned work.  Full distribution file viewing is not yet implemented in this GUI bridge runtime."))
   ((eq command 'describe-no-warranty)
    (cons "GNU Emacs No Warranty"
          "GNU Emacs is distributed in the hope that it will be useful, but without warranty.  See the GNU General Public License for the complete terms."))
   ((eq command 'describe-gnu-project)
    (cons "About the GNU Project"
          "The GNU Project develops the GNU operating system and free software, including GNU Emacs."))
   ((eq command 'view-hello-file)
    (cons "Hello"
          "Hello from GNU Emacs.  Multilingual hello text is not yet bundled in this bridge runtime."))
   ((eq command 'describe-coding-system)
    (cons "Coding System"
          "Coding system inspection is not yet connected to the full Emacs coding database.  This bridge command records the Help buffer behavior in nelisp-emacs."))
   ((eq command 'describe-input-method)
    (cons "Input Method"
          "Input method descriptions are not yet backed by the full Emacs input method registry.  GUI input decoding remains in nelisp-gui; input method semantics belong in nelisp-emacs."))
   ((eq command 'describe-language-environment)
    (cons "Language Environment"
          "Language environment details are not yet loaded from Emacs data files.  This command provides the expected read-only Help buffer surface."))
   ((eq command 'view-lossage)
    (cons "Recent Keys"
          "Recent key lossage is not yet persisted by the GUI bridge runtime."))
   ((eq command 'describe-mode)
    (cons "Mode Help"
          (concat "Major mode: Fundamental\nBuffer: "
                  emacs-help-gui-buffer-name
                  "\nThe current GUI bridge runtime exposes a minimal mode description.")))
   ((eq command 'help-quit)
    (cons "Help Quit"
          "Help quit was requested.  Window closing is not modeled here; the command is represented as a read-only Help buffer update."))
   ((eq command 'describe-syntax)
    (cons "Syntax Table"
          "Syntax table details are not yet backed by full Emacs syntax table data.  Word/symbol movement currently uses the bridge runtime character predicates."))
   ((eq command 'help-with-tutorial)
    (cons "Emacs Tutorial"
          "The full Emacs tutorial is not yet bundled in this bridge runtime.  This command opens the expected read-only Help buffer."))
   ((eq command 'display-local-help)
    (cons "Local Help"
          "Local contextual help was requested.  The GUI bridge runtime represents the request as a read-only Help buffer; widget and text-property help lookup remains a nelisp-emacs task."))
   ((eq command 'help-find-source)
    (cons "Find Source"
          "Source lookup for the current help target is not yet backed by full symbol-to-source metadata.  This command opens the expected Help surface without adding GUI-side command semantics."))
   ((eq command 'help-quick-toggle)
    (cons "Quick Help Toggle"
          "Quick help display toggling is represented in the bridge runtime as a Help buffer update.  Help window display policy remains owned by nelisp-emacs."))
   ((eq command 'search-forward-help-for-help)
    (cons "Search Help"
          "Search within the Help-for-Help buffer was requested.  Full incremental help search is not yet implemented in the bridge runtime."))
   ((eq command 'finder-by-keyword)
    (cons "Package Finder"
          "Package keyword browsing is not yet backed by the full package index.  This command opens the expected read-only Help buffer."))
   (t
    (cons (symbol-name command)
          "This help command is recognized but does not have detailed text yet."))))

;;;###autoload
(defun emacs-help-gui-static-command (command)
  "Render static GUI help for COMMAND."
  (let ((entry (emacs-help-gui--static-text command)))
    (emacs-help-gui--show-help-buffer (car entry) (cdr entry))))

;;;###autoload
(defun emacs-help-gui-apropos-command (&optional pattern)
  "Render GUI apropos-command help for PATTERN."
  (let ((arg (or pattern emacs-help-gui-arg)))
    (emacs-help-gui--show-help-buffer
     "Apropos Commands"
     (concat "Apropos command search is not yet backed by the full command index.\nPattern: "
             arg))))

;;;###autoload
(defun emacs-help-gui-apropos-documentation (&optional pattern)
  "Render GUI apropos-documentation help for PATTERN."
  (let ((arg (or pattern emacs-help-gui-arg)))
    (emacs-help-gui--show-help-buffer
     "Apropos Documentation"
     (concat "Apropos documentation search is not yet backed by the full documentation index.\nPattern: "
             arg))))

;;;###autoload
(defun emacs-help-gui-describe-function-current-context-command ()
  "Refresh GUI help context, then run `describe-function'."
  (emacs-help-gui--run-core-current-context
   'emacs-help-gui-describe-function-core))

;;;###autoload
(defun emacs-help-gui-describe-variable-current-context-command ()
  "Refresh GUI help context, then run `describe-variable'."
  (emacs-help-gui--run-core-current-context
   'emacs-help-gui-describe-variable-core))

;;;###autoload
(defun emacs-help-gui-describe-key-current-context-command ()
  "Refresh GUI help context, then run `describe-key'."
  (emacs-help-gui--run-core-current-context
   'emacs-help-gui-describe-key-core))

;;;###autoload
(defun emacs-help-gui-describe-key-briefly-current-context-command ()
  "Refresh GUI help context, then run `describe-key-briefly'."
  (emacs-help-gui--run-core-current-context
   'emacs-help-gui-describe-key-briefly-core))

;;;###autoload
(defun emacs-help-gui-describe-bindings-current-context-command ()
  "Refresh GUI help context, then run `describe-bindings'."
  (emacs-help-gui--run-core-current-context
   'emacs-help-gui-describe-bindings-core))

;;;###autoload
(defun emacs-help-gui-where-is-current-context-command ()
  "Refresh GUI help context, then run `where-is'."
  (emacs-help-gui--run-core-current-context
   'emacs-help-gui-where-is-core))

;;;###autoload
(defun emacs-help-gui-help-for-help-current-context-command ()
  "Refresh GUI help context, then run `help-for-help'."
  (emacs-help-gui-refresh-context-from-backend)
  (emacs-help-gui-help-for-help))

;;;###autoload
(defun emacs-help-gui-describe-command-current-context-command ()
  "Refresh GUI help context, then run `describe-command'."
  (emacs-help-gui-refresh-context-from-backend)
  (emacs-help-gui-describe-command))

;;;###autoload
(defun emacs-help-gui-describe-package-current-context-command ()
  "Refresh GUI help context, then run `describe-package'."
  (emacs-help-gui-refresh-context-from-backend)
  (emacs-help-gui-describe-package))

;;;###autoload
(defun emacs-help-gui-static-current-context-command (command)
  "Refresh GUI help context, then render static help COMMAND."
  (emacs-help-gui-refresh-context-from-backend)
  (emacs-help-gui-static-command command))

;;;###autoload
(defun emacs-help-gui-apropos-command-current-context-command ()
  "Refresh GUI help context, then run `apropos-command'."
  (emacs-help-gui-refresh-context-from-backend)
  (emacs-help-gui-apropos-command))

;;;###autoload
(defun emacs-help-gui-apropos-documentation-current-context-command ()
  "Refresh GUI help context, then run `apropos-documentation'."
  (emacs-help-gui-refresh-context-from-backend)
  (emacs-help-gui-apropos-documentation))

;;;###autoload
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

(provide 'emacs-help-gui)

;;; emacs-help-gui.el ends here
