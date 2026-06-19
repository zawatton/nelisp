;;; emacs-info.el --- Minimal Info runtime for nelisp-emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Minimal Info command semantics shared by TUI/batch and GUI adapters.
;; The GUI bridge can register a transport-backed backend so the command
;; sequencing and Info node parsing live here while buffer stores remain
;; bridge-specific.

;;; Code:

(defvar emacs-info-gui-backend nil
  "PLIST of GUI bridge Info backend callbacks.")

(defvar emacs-info-gui-arg ""
  "Argument string supplied by the GUI bridge Info command.")

(defvar emacs-info-gui-status "ok"
  "Status string returned by the GUI bridge Info command.")

(defvar emacs-info-gui-buffer-name ""
  "Buffer name reported by the GUI bridge Info backend.")

(defvar emacs-info-gui-file ""
  "Current Info file path for GUI bridge Info navigation.")

(defvar emacs-info-gui-node ""
  "Current Info node name for GUI bridge Info navigation.")

(defvar emacs-info-gui-scan-cap 65536
  "Maximum number of bytes scanned when parsing an Info file.")

(defun emacs-info-gui-register-backend (&rest backend)
  "Register BACKEND as the GUI bridge Info adapter.
BACKEND is a plist.  Passing nil clears the adapter."
  (setq emacs-info-gui-backend backend))

(defun emacs-info-gui--backend-call (key &rest args)
  "Call GUI Info backend function KEY with ARGS, if registered."
  (let ((fn (and emacs-info-gui-backend
                 (plist-get emacs-info-gui-backend key))))
    (when fn
      (apply fn args))))

(defun emacs-info-gui--backend-function (key)
  "Return GUI Info backend function KEY, or nil."
  (and emacs-info-gui-backend
       (plist-get emacs-info-gui-backend key)))

(defun emacs-info-gui-set-context (&rest plist)
  "Update GUI bridge Info context from PLIST.
Recognized keys are `:arg', `:status', `:buffer-name', `:file',
and `:node'."
  (when (plist-member plist :arg)
    (setq emacs-info-gui-arg (plist-get plist :arg)))
  (when (plist-member plist :status)
    (setq emacs-info-gui-status (plist-get plist :status)))
  (when (plist-member plist :buffer-name)
    (setq emacs-info-gui-buffer-name (plist-get plist :buffer-name)))
  (when (plist-member plist :file)
    (setq emacs-info-gui-file (plist-get plist :file)))
  (when (plist-member plist :node)
    (setq emacs-info-gui-node (plist-get plist :node)))
  plist)

;;;###autoload
(defun emacs-info-gui-refresh-context-from-backend ()
  "Refresh GUI bridge Info context from registered backend callbacks."
  (let ((arg (emacs-info-gui--backend-function :current-arg))
        (status (emacs-info-gui--backend-function :current-status))
        (buffer-name (emacs-info-gui--backend-function :buffer-name))
        (file (emacs-info-gui--backend-function :current-file))
        (node (emacs-info-gui--backend-function :current-node)))
    (when arg
      (setq emacs-info-gui-arg (funcall arg)))
    (when status
      (setq emacs-info-gui-status (funcall status)))
    (when buffer-name
      (setq emacs-info-gui-buffer-name (funcall buffer-name)))
    (when file
      (setq emacs-info-gui-file (funcall file)))
    (when node
      (setq emacs-info-gui-node (funcall node))))
  (list :arg emacs-info-gui-arg
        :status emacs-info-gui-status
        :buffer-name emacs-info-gui-buffer-name
        :file emacs-info-gui-file
        :node emacs-info-gui-node))

(defun emacs-info-gui--set-status (status)
  "Set GUI Info STATUS locally and in the backend."
  (setq emacs-info-gui-status status)
  (emacs-info-gui--backend-call :set-status status)
  status)

(defun emacs-info-gui--set-buffer-name-from-backend (&optional result)
  "Refresh GUI Info buffer name from backend, falling back to RESULT."
  (setq emacs-info-gui-buffer-name
        (or (emacs-info-gui--backend-call :buffer-name)
            result
            emacs-info-gui-buffer-name)))

(defun emacs-info-gui--apply-display-prefix (action)
  "Ask the backend to apply display ACTION."
  (emacs-info-gui--backend-call :apply-display-prefix action))

(defun emacs-info-gui--host-interactive-p ()
  "Return non-nil when a host Emacs window should mirror Info text."
  (and (boundp 'noninteractive)
       (not noninteractive)
       (not (fboundp 'nl-write-file))
       (fboundp 'get-buffer-create)
       (fboundp 'selected-window)
       (fboundp 'set-window-buffer)))

(defun emacs-info-gui--show-host-buffer (title body)
  "Mirror Info TITLE and BODY into the visible host `*info*' buffer."
  (when (emacs-info-gui--host-interactive-p)
    (let ((host-buffer (get-buffer-create "*info*")))
      (with-current-buffer host-buffer
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert title)
          (insert "\n\n")
          (insert body)
          (unless (and (> (point) (point-min))
                       (= (char-before) ?\n))
            (insert "\n"))
          (goto-char (point-min))
          (setq major-mode 'Info-mode
                mode-name "Info"
                buffer-read-only t)))
      (set-window-buffer (selected-window) host-buffer)
      "*info*")))

(defun emacs-info-gui--token-at (text idx token)
  "Return non-nil if TEXT has TOKEN at IDX."
  (let ((k 0)
        (ok t)
        (n (length token)))
    (if (> (+ idx n) (length text))
        (setq ok nil)
      (while (< k n)
        (if (= (aref text (+ idx k)) (aref token k))
            nil
          (setq ok nil)
          (setq k n))
        (setq k (1+ k))))
    ok))

(defun emacs-info-gui-header-field (header field)
  "Return FIELD value from an Info HEADER line, or empty string."
  (let ((i 0)
        (n (length header))
        (start -1)
        (end 0)
        (out ""))
    (while (< i n)
      (if (emacs-info-gui--token-at header i field)
          (setq start (+ i (length field))
                i n)
        (setq i (1+ i))))
    (when (>= start 0)
      (setq end start)
      (while (and (< end n)
                  (not (= (aref header end) ?,)))
        (setq end (1+ end)))
      (setq out (substring header start end)))
    out))

(defun emacs-info-gui--read-current-file ()
  "Return the current Info file contents through the backend."
  (or (emacs-info-gui--backend-call :read-file emacs-info-gui-file)
      ""))

(defun emacs-info-gui--show-info-buffer (title body)
  "Render TITLE and BODY through the GUI backend or host TUI mirror."
  (emacs-info-gui--set-buffer-name-from-backend
   (or (emacs-info-gui--backend-call :show-info-buffer title body)
       (emacs-info-gui--show-host-buffer title body)
       "*info*"))
  emacs-info-gui-buffer-name)

(defun emacs-info-gui-render-node (target)
  "Render TARGET node from `emacs-info-gui-file'.
An empty TARGET selects the first node found."
  (let ((text (emacs-info-gui--read-current-file))
        (i 0)
        (n 0)
        (hs 0)
        (he 0)
        (name "")
        (node-start -1)
        (node-end -1))
    (setq n (length text))
    (when (> n emacs-info-gui-scan-cap)
      (setq n emacs-info-gui-scan-cap))
    (while (< i n)
      (if (= (aref text i) 31)
          (if (>= node-start 0)
              (setq node-end i
                    i n)
            (setq hs (1+ i))
            (when (and (< hs n) (= (aref text hs) ?\n))
              (setq hs (1+ hs)))
            (setq he hs)
            (while (and (< he n)
                        (not (= (aref text he) ?\n)))
              (setq he (1+ he)))
            (setq name (emacs-info-gui-header-field
                        (substring text hs he)
                        "Node: "))
            (if (if (equal target "")
                    (> (length name) 0)
                  (equal name target))
                (setq node-start hs
                      emacs-info-gui-node name)
              nil)
            (setq i he))
        (setq i (1+ i))))
    (if (>= node-start 0)
        (progn
          (when (< node-end 0)
            (setq node-end n))
          (emacs-info-gui--show-info-buffer
           (substring text node-start node-end)
           (concat "[Info file: " emacs-info-gui-file "]"))
          t)
      nil)))

(defun emacs-info-gui--write-state ()
  "Persist current Info file/node state through the backend."
  (emacs-info-gui--backend-call
   :write-state emacs-info-gui-file emacs-info-gui-node))

(defun emacs-info-gui--current-header ()
  "Return the current Info buffer header through the backend."
  (or (emacs-info-gui--backend-call :current-header) ""))

(defun emacs-info-gui--file-exists-p (path)
  "Return non-nil if PATH exists according to the backend."
  (emacs-info-gui--backend-call :file-exists-p path))

(defun emacs-info-gui-info-core ()
  "Open Info using `emacs-info-gui-arg' without display-prefix policy.
Command variants layer window/tab/frame placement on top of this core."
  (if (and (not (equal emacs-info-gui-arg ""))
           (emacs-info-gui--file-exists-p emacs-info-gui-arg))
      (progn
        (setq emacs-info-gui-file emacs-info-gui-arg)
        (setq emacs-info-gui-node "")
        (if (emacs-info-gui-render-node "Top")
            nil
          (if (emacs-info-gui-render-node "")
              nil
            (emacs-info-gui--set-status "unsupported")))
        (emacs-info-gui--write-state))
    (emacs-info-gui--show-info-buffer
     "Info Directory"
     "Info directory navigation is represented by this read-only Info buffer.  Pass an .info file path argument for node parsing; /usr/share/info dir aggregation remains future work."))
  emacs-info-gui-buffer-name)

(defun emacs-info-gui-info (&optional action)
  "Open Info using `emacs-info-gui-arg'.
ACTION is nil/same or a display target such as `other'."
  (emacs-info-gui-info-core)
  (when (equal emacs-info-gui-status "ok")
    (emacs-info-gui--apply-display-prefix (or action "same")))
  emacs-info-gui-buffer-name)

(defun emacs-info-gui-goto-pointer (field)
  "Navigate to FIELD pointer from the current Info node header."
  (if (and (equal emacs-info-gui-buffer-name "*info*")
           (not (equal emacs-info-gui-file "")))
      (let ((name (emacs-info-gui-header-field
                   (emacs-info-gui--current-header)
                   field)))
        (if (equal name "")
            (emacs-info-gui--set-status "unsupported")
          (if (emacs-info-gui-render-node name)
              (emacs-info-gui--write-state)
            (emacs-info-gui--set-status "unsupported"))))
    (emacs-info-gui--set-status "unsupported"))
  emacs-info-gui-buffer-name)

(defun emacs-info-gui-next ()
  "Navigate to the current Info node's Next pointer."
  (emacs-info-gui-goto-pointer "Next: "))

(defun emacs-info-gui-prev ()
  "Navigate to the current Info node's Prev pointer."
  (emacs-info-gui-goto-pointer "Prev: "))

(defun emacs-info-gui-up ()
  "Navigate to the current Info node's Up pointer."
  (emacs-info-gui-goto-pointer "Up: "))

(defun emacs-info-gui-emacs-manual ()
  "Show the compact Emacs manual placeholder."
  (emacs-info-gui--show-info-buffer
   "Emacs Manual"
   "The Emacs manual explains editing, files, buffers, windows, commands, customization, and Lisp.  Full manual navigation is not yet bundled in this runtime."))

(defun emacs-info-gui-display-manual ()
  "Show the requested manual placeholder."
  (let ((manual emacs-info-gui-arg))
    (when (equal manual "")
      (setq manual "emacs"))
    (emacs-info-gui--show-info-buffer
     (concat "Info Manual: " manual)
     (concat "Requested manual: "
             manual
             "\nFull Info manual lookup is not yet backed by parsed Info files."))))

(defun emacs-info-gui-view-order-manuals ()
  "Show the GNU manual ordering placeholder."
  (emacs-info-gui--show-info-buffer
   "Ordering GNU Manuals"
   "GNU manuals are available from the GNU project and its documentation mirrors.  This command opens the expected read-only Info buffer surface."))

(defun emacs-info-gui-goto-emacs-command-node ()
  "Show the requested Emacs command manual-node placeholder."
  (let ((command emacs-info-gui-arg))
    (when (equal command "")
      (setq command "unknown"))
    (emacs-info-gui--show-info-buffer
     (concat "Emacs Command: " command)
     (concat "Requested Emacs command manual node for "
             command
             ".\nFull Info command-node resolution is not yet implemented."))))

(defun emacs-info-gui-goto-emacs-key-command-node ()
  "Show the requested Emacs key manual-node placeholder."
  (let ((key emacs-info-gui-arg))
    (when (equal key "")
      (setq key "unknown"))
    (emacs-info-gui--show-info-buffer
     (concat "Emacs Key: " key)
     (concat "Requested Emacs manual node for key "
             key
             ".\nFull Info key-node resolution is not yet implemented."))))

(defun emacs-info-gui-lookup-symbol ()
  "Show the requested Info symbol lookup placeholder."
  (let ((symbol emacs-info-gui-arg))
    (when (equal symbol "")
      (setq symbol "unknown"))
    (emacs-info-gui--show-info-buffer
     (concat "Info Lookup Symbol: " symbol)
     (concat "Requested Info lookup for symbol "
             symbol
             ".\nThe runtime has not yet loaded language-specific Info lookup indexes."))))

(defun emacs-info-gui-info-command (&optional action)
  "Run the GUI bridge `info' command variant."
  (emacs-info-gui-info (or action "same")))

(defun emacs-info-gui-info-current-context-command (&optional action)
  "Refresh GUI Info context from backend, then run `info'."
  (emacs-info-gui-refresh-context-from-backend)
  (emacs-info-gui-info-command action))

(defun emacs-info-gui-next-command ()
  "Run the GUI bridge `Info-next' command."
  (emacs-info-gui-next))

(defun emacs-info-gui-prev-command ()
  "Run the GUI bridge `Info-prev' command."
  (emacs-info-gui-prev))

(defun emacs-info-gui-up-command ()
  "Run the GUI bridge `Info-up' command."
  (emacs-info-gui-up))

(defun emacs-info-gui-next-current-context-command ()
  "Refresh GUI Info context from backend, then run `Info-next'."
  (emacs-info-gui-refresh-context-from-backend)
  (emacs-info-gui-next-command))

(defun emacs-info-gui-prev-current-context-command ()
  "Refresh GUI Info context from backend, then run `Info-prev'."
  (emacs-info-gui-refresh-context-from-backend)
  (emacs-info-gui-prev-command))

(defun emacs-info-gui-up-current-context-command ()
  "Refresh GUI Info context from backend, then run `Info-up'."
  (emacs-info-gui-refresh-context-from-backend)
  (emacs-info-gui-up-command))

(defun emacs-info-gui-emacs-manual-command ()
  "Run the GUI bridge `info-emacs-manual' command."
  (emacs-info-gui-emacs-manual))

(defun emacs-info-gui-display-manual-command ()
  "Run the GUI bridge `info-display-manual' command."
  (emacs-info-gui-display-manual))

(defun emacs-info-gui-view-order-manuals-command ()
  "Run the GUI bridge `view-order-manuals' command."
  (emacs-info-gui-view-order-manuals))

(defun emacs-info-gui-goto-emacs-command-node-command ()
  "Run the GUI bridge `Info-goto-emacs-command-node' command."
  (emacs-info-gui-goto-emacs-command-node))

(defun emacs-info-gui-goto-emacs-key-command-node-command ()
  "Run the GUI bridge `Info-goto-emacs-key-command-node' command."
  (emacs-info-gui-goto-emacs-key-command-node))

(defun emacs-info-gui-lookup-symbol-command ()
  "Run the GUI bridge `info-lookup-symbol' command."
  (emacs-info-gui-lookup-symbol))

(defun emacs-info-gui-emacs-manual-current-context-command ()
  "Refresh GUI Info context from backend, then run `info-emacs-manual'."
  (emacs-info-gui-refresh-context-from-backend)
  (emacs-info-gui-emacs-manual-command))

(defun emacs-info-gui-display-manual-current-context-command ()
  "Refresh GUI Info context from backend, then run `info-display-manual'."
  (emacs-info-gui-refresh-context-from-backend)
  (emacs-info-gui-display-manual-command))

(defun emacs-info-gui-view-order-manuals-current-context-command ()
  "Refresh GUI Info context from backend, then run `view-order-manuals'."
  (emacs-info-gui-refresh-context-from-backend)
  (emacs-info-gui-view-order-manuals-command))

(defun emacs-info-gui-goto-emacs-command-node-current-context-command ()
  "Refresh GUI Info context, then run `Info-goto-emacs-command-node'."
  (emacs-info-gui-refresh-context-from-backend)
  (emacs-info-gui-goto-emacs-command-node-command))

(defun emacs-info-gui-goto-emacs-key-command-node-current-context-command ()
  "Refresh GUI Info context, then run `Info-goto-emacs-key-command-node'."
  (emacs-info-gui-refresh-context-from-backend)
  (emacs-info-gui-goto-emacs-key-command-node-command))

(defun emacs-info-gui-lookup-symbol-current-context-command ()
  "Refresh GUI Info context from backend, then run `info-lookup-symbol'."
  (emacs-info-gui-refresh-context-from-backend)
  (emacs-info-gui-lookup-symbol-command))

;;;###autoload
(defun emacs-info-gui-current-context-command (command &optional action)
  "Refresh backend context and run GUI Info COMMAND.
ACTION is nil/same or `other' for display variants."
  (emacs-info-gui-refresh-context-from-backend)
  (cond
   ((eq command 'info)
    (emacs-info-gui-info-command (or action "same")))
   ((eq command 'info-other-window)
    (emacs-info-gui-info-command (or action "other")))
   ((eq command 'Info-next)
    (emacs-info-gui-next-command))
   ((eq command 'Info-prev)
    (emacs-info-gui-prev-command))
   ((eq command 'Info-up)
    (emacs-info-gui-up-command))
   ((eq command 'info-emacs-manual)
    (emacs-info-gui-emacs-manual-command))
   ((eq command 'info-display-manual)
    (emacs-info-gui-display-manual-command))
   ((eq command 'view-order-manuals)
    (emacs-info-gui-view-order-manuals-command))
   ((eq command 'Info-goto-emacs-command-node)
    (emacs-info-gui-goto-emacs-command-node-command))
   ((eq command 'Info-goto-emacs-key-command-node)
    (emacs-info-gui-goto-emacs-key-command-node-command))
   ((eq command 'info-lookup-symbol)
    (emacs-info-gui-lookup-symbol-command))
   (t nil)))

;;;###autoload
(defun emacs-info-gui-writeback-spec (&optional command)
  "Return GUI transport writeback spec for Info COMMAND.
This describes Emacs-compatible Info buffer results; the bridge remains
responsible for the concrete transport files."
  (let ((command (if (symbolp command)
                     (symbol-name command)
                   (if (stringp command) command "")))
        (result nil))
    (when (member command
                  '("info" "info-other-window" "Info-next" "Info-prev"
                    "Info-up" "info-emacs-manual" "info-display-manual"
                    "view-order-manuals" "Info-goto-emacs-command-node"
                    "Info-goto-emacs-key-command-node"
                    "info-lookup-symbol"))
      (setq result
            '(:buffer t :file t :buffer-name t :read-only t
              :window t :point t :mark t :window-start t)))
    result))

;;;###autoload
(defun emacs-info-gui-writeback-spec-flag (spec key)
  "Return non-nil when Info GUI writeback SPEC enables KEY."
  (and spec (plist-get spec key)))

;;;###autoload
(defun emacs-info-gui-writeback-state (&optional command)
  "Write GUI transport state for Info COMMAND.
The Info runtime owns writeback spec interpretation and callback ordering.
The registered backend owns concrete transport writes.  Return non-nil when
COMMAND has an Info writeback spec."
  (let ((spec (emacs-info-gui-writeback-spec command)))
    (when spec
      (when (emacs-info-gui-writeback-spec-flag spec :buffer)
        (emacs-info-gui--backend-call :write-buffer-state))
      (when (emacs-info-gui-writeback-spec-flag spec :file)
        (emacs-info-gui--backend-call :write-file-state))
      (when (emacs-info-gui-writeback-spec-flag spec :buffer-name)
        (emacs-info-gui--backend-call :write-buffer-name-state))
      (when (emacs-info-gui-writeback-spec-flag spec :read-only)
        (emacs-info-gui--backend-call :write-read-only-state))
      (when (emacs-info-gui-writeback-spec-flag spec :window)
        (emacs-info-gui--backend-call :write-window-state))
      (when (emacs-info-gui-writeback-spec-flag spec :point)
        (emacs-info-gui--backend-call :write-point-state))
      (when (emacs-info-gui-writeback-spec-flag spec :mark)
        (emacs-info-gui--backend-call :write-mark-state))
      (when (emacs-info-gui-writeback-spec-flag spec :window-start)
        (emacs-info-gui--backend-call :write-window-start-state))
      (emacs-info-gui--backend-call :mark-written-state)
      t)))

;;;###autoload
(defun info (&optional file-or-node _buffer)
  "Minimal Info entry point.
When a GUI backend is registered, FILE-OR-NODE is forwarded through it.
Without a backend this opens the compact Info directory placeholder."
  (interactive)
  (when file-or-node
    (setq emacs-info-gui-arg file-or-node))
  (emacs-info-gui-info-command "same"))

;;;###autoload
(defun info-other-window (&optional file-or-node)
  "Minimal `info' variant targeting another window."
  (interactive)
  (when file-or-node
    (setq emacs-info-gui-arg file-or-node))
  (emacs-info-gui-info-command "other"))

;;;###autoload
(defun Info-next ()
  "Navigate to the next Info node."
  (interactive)
  (emacs-info-gui-next-command))

;;;###autoload
(defun Info-prev ()
  "Navigate to the previous Info node."
  (interactive)
  (emacs-info-gui-prev-command))

;;;###autoload
(defun Info-up ()
  "Navigate to the parent Info node."
  (interactive)
  (emacs-info-gui-up-command))

;;;###autoload
(defun Info-directory ()
  "Open the compact Info directory."
  (interactive)
  (let ((emacs-info-gui-arg ""))
    (emacs-info-gui-info-command "same")))

;;;###autoload
(defun Info-goto-node (&optional node)
  "Show NODE through the compact Info surface."
  (interactive)
  (when node
    (setq emacs-info-gui-arg node))
  (emacs-info-gui-info-command "same"))

;;;###autoload
(defun Info-find-node (&optional file node)
  "Show FILE or NODE through the compact Info surface."
  (interactive)
  (setq emacs-info-gui-arg (or file node ""))
  (emacs-info-gui-info-command "same"))

;;;###autoload
(defun Info-mode ()
  "Minimal Info mode marker for compact Info buffers."
  (interactive)
  (setq major-mode 'Info-mode
        mode-name "Info")
  nil)

;;;###autoload
(defun info-emacs-manual ()
  "Show the compact Emacs manual placeholder."
  (interactive)
  (emacs-info-gui-emacs-manual-command))

;;;###autoload
(defun info-display-manual (&optional manual)
  "Show MANUAL through the compact Info surface."
  (interactive)
  (when manual
    (setq emacs-info-gui-arg manual))
  (emacs-info-gui-display-manual-command))

;;;###autoload
(defun view-order-manuals ()
  "Show the GNU manual ordering placeholder."
  (interactive)
  (emacs-info-gui-view-order-manuals-command))

;;;###autoload
(defun Info-goto-emacs-command-node (&optional command)
  "Show the Info command-node placeholder for COMMAND."
  (interactive)
  (when command
    (setq emacs-info-gui-arg command))
  (emacs-info-gui-goto-emacs-command-node-command))

;;;###autoload
(defun Info-goto-emacs-key-command-node (&optional key)
  "Show the Info key-node placeholder for KEY."
  (interactive)
  (when key
    (setq emacs-info-gui-arg key))
  (emacs-info-gui-goto-emacs-key-command-node-command))

;;;###autoload
(defun info-lookup-symbol (&optional symbol)
  "Show the Info symbol lookup placeholder for SYMBOL."
  (interactive)
  (when symbol
    (setq emacs-info-gui-arg symbol))
  (emacs-info-gui-lookup-symbol-command))

(provide 'emacs-info)

;;; emacs-info.el ends here
