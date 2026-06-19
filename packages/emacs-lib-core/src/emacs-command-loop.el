;;; emacs-command-loop.el --- Command-loop substrate (Phase B.1)  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Doc 51 Track B (2026-05-03) — Layer 2.
;;
;; Phase B.1 = the foundation half of the command loop: the unread
;; event queue, the command-bookkeeping state vars (`this-command' /
;; `last-command' / `last-{input,command}-event' / `quit-flag' /
;; `inhibit-quit' / `real-this-command'), and the basic readers
;; (`read-event' / `read-char') that drain the queue.
;;
;; Higher-level pieces — `read-key-sequence' (B.2),
;; `call-interactively' (B.3), `command-loop-1' (B.4),
;; `execute-extended-command' (B.5), keyboard-quit / `recursive-edit'
;; (B.6) — build on this substrate.
;;
;; Test injection: callers (= ERTs and the future
;; `command-loop-1' driver) push events with
;; `emacs-command-loop-feed-events'.  Under standalone NeLisp the
;; bridge `emacs-command-loop-builtins' also exposes the
;; conventional `unread-command-events' defvar, and
;; `emacs-command-loop-read-event' will drain that as a fallback so
;; the standard `(setq unread-command-events ...)' idiom keeps
;; working without going through the prefixed feed.

;;; Code:

(require 'cl-lib)

;;;; --- error symbols --------------------------------------------------

(define-error 'emacs-command-loop-error "Command-loop error")
(define-error 'emacs-command-loop-no-input
  "No input event available" 'emacs-command-loop-error)
(define-error 'emacs-command-loop-quit
  "Quit during command loop" 'emacs-command-loop-error)

;;;; --- state ----------------------------------------------------------

(defvar emacs-command-loop--unread-events nil
  "Substrate-internal pending event queue, head consumed first.")

(defvar emacs-command-loop--this-command nil
  "Command currently being executed.  Mirrors `this-command'.")

(defvar emacs-command-loop--last-command nil
  "Command executed by the most recent complete command-loop iteration.")

(defvar emacs-command-loop--real-this-command nil
  "Command actually dispatched, even if `this-command' was overwritten
mid-execution by a remap.")

(defvar emacs-command-loop--this-command-keys ""
  "String of raw key events that triggered the current command.
Cleared on every fresh read-key-sequence iteration.")

(defvar emacs-command-loop--last-command-event nil
  "Last event in the key sequence that triggered the current command.")

(defvar emacs-command-loop--last-input-event nil
  "Most recent event read by `emacs-command-loop-read-event'.")

(defvar emacs-command-loop--last-nonmenu-event nil
  "Most recent input event that did not come from a menu bar.")

(defvar emacs-command-loop--quit-flag nil
  "Non-nil = quit was requested; checked at safe points by the loop.")

(defvar emacs-command-loop--inhibit-quit nil
  "Non-nil = `quit-flag' is not honoured (= protects critical sections).")

(defvar emacs-command-loop--throw-on-input nil
  "Tag to `throw' to when the next event is read; used for nested loops.")

;;;; --- GUI bridge command context ------------------------------------

(defvar emacs-command-loop-gui-backend nil
  "PLIST of GUI bridge command-loop backend callbacks.
The core command loop owns command/key dispatch sequencing; the backend
owns transport-specific state changes.")

(defvar emacs-command-loop-gui-command nil
  "Command symbol currently selected by the GUI bridge.")

(defvar emacs-command-loop-gui-effective-command ""
  "User-visible command string currently selected by the GUI bridge.")

(defvar emacs-command-loop-gui-keys ""
  "Raw key sequence string currently selected by the GUI bridge.")

(defvar emacs-command-loop-gui-arg ""
  "Command argument string currently selected by the GUI bridge.")

(defvar emacs-command-loop-gui-status "ok"
  "Status string returned to the GUI bridge.")

(defvar emacs-command-loop-gui-prefix-arg ""
  "Bridge textual prefix arg pending for the next GUI command.")

;;;; --- reset / lifecycle ---------------------------------------------

(defun emacs-command-loop-reset ()
  "Reset all substrate state.  Tests call this in `unwind-protect' tails."
  (setq emacs-command-loop--unread-events       nil
        emacs-command-loop--this-command        nil
        emacs-command-loop--last-command        nil
        emacs-command-loop--real-this-command   nil
        emacs-command-loop--this-command-keys   ""
        emacs-command-loop--last-command-event  nil
        emacs-command-loop--last-input-event    nil
        emacs-command-loop--last-nonmenu-event  nil
        emacs-command-loop--quit-flag           nil
        emacs-command-loop--inhibit-quit        nil
        emacs-command-loop--throw-on-input      nil
        emacs-command-loop-gui-backend          nil
        emacs-command-loop-gui-command          nil
        emacs-command-loop-gui-effective-command ""
        emacs-command-loop-gui-keys             ""
        emacs-command-loop-gui-arg              ""
        emacs-command-loop-gui-status           "ok"
        emacs-command-loop-gui-prefix-arg       "")
  ;; Phase B.5 additions — declared below this defun, so guarded
  ;; (= avoids a forward-reference void-variable).
  (when (boundp 'emacs-command-loop--prefix-arg)
    (setq emacs-command-loop--prefix-arg nil))
  (when (boundp 'emacs-command-loop--current-prefix-arg)
    (setq emacs-command-loop--current-prefix-arg nil)))

;;;; --- GUI bridge helpers --------------------------------------------

;;;###autoload
(defun emacs-command-loop-gui-register-backend (&rest backend)
  "Register BACKEND as the GUI bridge command-loop adapter.
BACKEND is a plist.  Passing nil clears the adapter."
  (setq emacs-command-loop-gui-backend backend))

(defun emacs-command-loop-gui--backend-call (key &rest args)
  "Call GUI backend function KEY with ARGS, if it is registered."
  (let ((fn (and emacs-command-loop-gui-backend
                 (plist-get emacs-command-loop-gui-backend key))))
    (when fn
      (apply fn args))))

(defun emacs-command-loop-gui--backend-function (key)
  "Return GUI backend function KEY, or nil."
  (and emacs-command-loop-gui-backend
       (plist-get emacs-command-loop-gui-backend key)))

;;;###autoload
(defun emacs-command-loop-gui-refresh-context-from-backend ()
  "Refresh GUI command context from backend current-value callbacks.
This lets the command-loop runtime own context ingestion while the GUI
bridge remains a transport adapter.  Missing callbacks leave the
existing in-memory context unchanged."
  (when (emacs-command-loop-gui--backend-function :current-command)
    (emacs-command-loop-gui--set-command
     (emacs-command-loop-gui--backend-call :current-command)))
  (when (emacs-command-loop-gui--backend-function :current-effective-command)
    (emacs-command-loop-gui--set-effective-command
     (emacs-command-loop-gui--backend-call :current-effective-command)))
  (when (emacs-command-loop-gui--backend-function :current-keys)
    (setq emacs-command-loop-gui-keys
          (emacs-command-loop-gui--backend-call :current-keys)))
  (when (emacs-command-loop-gui--backend-function :current-arg)
    (emacs-command-loop-gui--set-arg
     (emacs-command-loop-gui--backend-call :current-arg)))
  (when (emacs-command-loop-gui--backend-function :current-status)
    (emacs-command-loop-gui--set-status
     (emacs-command-loop-gui--backend-call :current-status)))
  (when (emacs-command-loop-gui--backend-function :current-prefix-arg)
    (emacs-command-loop-gui--set-prefix-arg
     (emacs-command-loop-gui--backend-call :current-prefix-arg)))
  (list :command emacs-command-loop-gui-command
        :effective-command emacs-command-loop-gui-effective-command
        :keys emacs-command-loop-gui-keys
        :arg emacs-command-loop-gui-arg
        :status emacs-command-loop-gui-status
        :prefix-arg emacs-command-loop-gui-prefix-arg))

;;;###autoload
(defun emacs-command-loop-gui-keymap-command (source key)
  "Return the command name for KEY in GUI keymap SOURCE.
SOURCE is a newline-separated KEY<TAB>COMMAND table.  Return the empty
string when KEY is not present."
  (let ((source (or source ""))
        (key (or key ""))
        (found ""))
    (if (fboundp 'str-kv-line)
        (str-kv-line source key)
      (let ((index 0)
            (start 0))
        (while (and (<= index (length source)) (equal found ""))
          (if (or (= index (length source))
                  (= (aref source index) 10))
              (let ((line (substring source start index))
                    (tab 0))
                (while (and (< tab (length line))
                            (not (= (aref line tab) 9)))
                  (setq tab (+ tab 1)))
                (when (and (< tab (length line))
                           (equal key (substring line 0 tab)))
                  (setq found (substring line (+ tab 1))))
                (setq start (+ index 1)))
            nil)
          (setq index (+ index 1)))
        found))))

;;;###autoload
(defun emacs-command-loop-gui-lookup-key-sequence-from-sources (key &rest sources)
  "Return the first command name bound to KEY in SOURCES.
Each source is a newline-separated KEY<TAB>COMMAND table.  Sources are
checked in order, so callers can preserve user, mode-local, then global
precedence without concatenating transport data in the bridge adapter."
  (let ((found ""))
    (while (and sources (equal found ""))
      (setq found
            (emacs-command-loop-gui-keymap-command
             (or (car sources) "")
             (or key "")))
      (setq sources (cdr sources)))
    found))

(defun emacs-command-loop-gui--set-command (command)
  "Set the active GUI bridge COMMAND."
  (setq emacs-command-loop-gui-command command)
  (emacs-command-loop-gui--backend-call :set-command command)
  command)

(defun emacs-command-loop-gui--set-effective-command (name)
  "Set the active GUI bridge effective command NAME."
  (setq emacs-command-loop-gui-effective-command name)
  (emacs-command-loop-gui--backend-call :set-effective-command name)
  name)

(defun emacs-command-loop-gui--set-status (status)
  "Set the active GUI bridge STATUS."
  (setq emacs-command-loop-gui-status status)
  (emacs-command-loop-gui--backend-call :set-status status)
  status)

(defun emacs-command-loop-gui--set-arg (arg)
  "Set the active GUI bridge argument ARG."
  (setq emacs-command-loop-gui-arg arg)
  (emacs-command-loop-gui--backend-call :set-arg arg)
  arg)

(defun emacs-command-loop-gui--set-keys (keys)
  "Set the active GUI bridge key sequence KEYS."
  (setq emacs-command-loop-gui-keys keys)
  (emacs-command-loop-gui--backend-call :set-keys keys)
  keys)

(defun emacs-command-loop-gui--set-prefix-arg (arg)
  "Set the active GUI bridge textual prefix ARG."
  (setq emacs-command-loop-gui-prefix-arg arg)
  (emacs-command-loop-gui--backend-call :set-prefix-arg arg)
  arg)

;;;###autoload
(defun emacs-command-loop-gui-set-context (&rest plist)
  "Update the GUI bridge command context from PLIST.
Recognized keys are `:command', `:effective-command', `:keys', `:arg',
`:status', and `:prefix-arg'."
  (when (plist-member plist :command)
    (emacs-command-loop-gui--set-command (plist-get plist :command)))
  (when (plist-member plist :effective-command)
    (emacs-command-loop-gui--set-effective-command
     (plist-get plist :effective-command)))
  (when (plist-member plist :keys)
    (emacs-command-loop-gui--set-keys (plist-get plist :keys)))
  (when (plist-member plist :arg)
    (emacs-command-loop-gui--set-arg (plist-get plist :arg)))
  (when (plist-member plist :status)
    (emacs-command-loop-gui--set-status (plist-get plist :status)))
  (when (plist-member plist :prefix-arg)
    (emacs-command-loop-gui--set-prefix-arg (plist-get plist :prefix-arg)))
  plist)

;;;###autoload
(defun emacs-command-loop-gui-ingest-request-context (&rest plist)
  "Normalize one GUI bridge request into command-loop context.
The bridge adapter owns transport reads.  This helper owns command-loop
request semantics: command-name interning, raw-key requests clearing the
direct command lane, minibuffer text promotion, prefix arg import, and
initial status.  Recognized keys are `:command-name', `:keys', `:arg',
`:minibuffer-text', `:prefix-arg', and `:status'."
  (let* ((command-name (or (plist-get plist :command-name) ""))
         (keys (or (plist-get plist :keys) ""))
         (arg (or (plist-get plist :arg) ""))
         (minibuffer-text (or (plist-get plist :minibuffer-text) ""))
         (prefix-arg (or (plist-get plist :prefix-arg) ""))
         (status (or (plist-get plist :status) "ok"))
         (raw-key-request-p (not (equal keys "")))
         (command (cond
                   (raw-key-request-p nil)
                   ((symbolp command-name) command-name)
                   ((and (stringp command-name)
                         (not (equal command-name "")))
                    (intern command-name))
                   (t nil)))
         (effective-command (cond
                             (raw-key-request-p "")
                             ((symbolp command-name)
                              (symbol-name command-name))
                             ((stringp command-name) command-name)
                             (t ""))))
    (emacs-command-loop-gui--set-command command)
    (emacs-command-loop-gui--set-effective-command effective-command)
    (emacs-command-loop-gui--set-keys keys)
    (emacs-command-loop-gui--set-arg arg)
    (emacs-command-loop-gui--set-prefix-arg prefix-arg)
    (emacs-command-loop-gui--set-status status)
    (if (not raw-key-request-p)
        nil
      (progn
        (emacs-command-loop-gui--backend-call :clear-command-request)
        (emacs-command-loop-gui--set-command nil)
        (emacs-command-loop-gui--set-effective-command "")))
    (if (if (not raw-key-request-p)
            nil
          (not (equal minibuffer-text "")))
        (emacs-command-loop-gui--set-arg minibuffer-text)
      nil)
    (list :command emacs-command-loop-gui-command
          :effective-command emacs-command-loop-gui-effective-command
          :keys emacs-command-loop-gui-keys
          :arg emacs-command-loop-gui-arg
          :status emacs-command-loop-gui-status
          :prefix-arg emacs-command-loop-gui-prefix-arg)))

(defconst emacs-command-loop-gui-benign-status-command-names
  '("org-metaright" "org-metaleft" "ignore")
  "GUI bridge command names whose writeback status should remain ok.")

;;;###autoload
(defun emacs-command-loop-gui-finalize-status (&rest plist)
  "Return the writeback lane for the current GUI bridge status.
The command-loop runtime owns status classification.  The bridge backend
owns transport writes for each lane.  Recognized PLIST keys are
`:command', `:effective-command', and `:status'.  Return one of
`read-only', `unsupported', `error', `minibuffer', `prefix-arg', or
`normal'."
  (let* ((command (if (plist-member plist :command)
                      (plist-get plist :command)
                    emacs-command-loop-gui-command))
         (effective-command
          (if (plist-member plist :effective-command)
              (plist-get plist :effective-command)
            emacs-command-loop-gui-effective-command))
         (status (if (plist-member plist :status)
                     (plist-get plist :status)
                   emacs-command-loop-gui-status))
         (command-name (cond
                        ((symbolp command) (symbol-name command))
                        ((stringp command) command)
                        (t "")))
         (effective-name (if (stringp effective-command)
                             effective-command
                           "")))
    (when (or (member command-name
                      emacs-command-loop-gui-benign-status-command-names)
              (member effective-name
                      emacs-command-loop-gui-benign-status-command-names))
      (setq status "ok")
      (emacs-command-loop-gui--set-status status))
    (cond
     ((equal status "read-only") 'read-only)
     ((equal status "unsupported") 'unsupported)
     ((or (equal status "file-not-found")
          (equal status "permission-denied")
          (equal status "error")
          (emacs-command-loop-gui--backend-call :error-status-p status))
      'error)
     ((equal status "minibuffer") 'minibuffer)
     ((equal status "prefix-arg") 'prefix-arg)
     (t 'normal))))

;;;###autoload
(defun emacs-command-loop-gui-finalize-status-current-context ()
  "Refresh GUI context and return the current writeback lane."
  (emacs-command-loop-gui-refresh-context-from-backend)
  (emacs-command-loop-gui-finalize-status))

;;;###autoload
(defun emacs-command-loop-gui-writeback-command-name
    (&optional command effective-command)
  "Return the GUI bridge command name used for post-command writeback.
COMMAND is the requested command symbol.  EFFECTIVE-COMMAND is the
string command name after key/minibuffer dispatch.  Most requests use
EFFECTIVE-COMMAND directly, but command-loop owns the minibuffer followup
normalization for commands whose writeback must be attributed to the
original command."
  (cond
   ((and (equal effective-command "minibuffer")
         (eq command 'project-query-replace-regexp))
    "project-query-replace-regexp")
   ((stringp effective-command)
    effective-command)
   ((and effective-command (symbolp effective-command))
    (symbol-name effective-command))
   ((and command (symbolp command))
    (symbol-name command))
   ((stringp command)
    command)
   (t "")))

;;;###autoload
(defun emacs-command-loop-gui-writeback-command-name-current-context ()
  "Refresh GUI context and return its post-command writeback command name."
  (emacs-command-loop-gui-refresh-context-from-backend)
  (emacs-command-loop-gui-writeback-command-name
   emacs-command-loop-gui-command
   emacs-command-loop-gui-effective-command))

;;;###autoload
(defun emacs-command-loop-gui-write-post-command-state
    (&optional command effective-command status)
  "Flush GUI bridge post-command state through backend callbacks.
The command-loop runtime owns the ordering and status lane decision.
The GUI bridge backend owns the transport writes behind each callback.
Return a plist with `:command-name' and `:lane'."
  (let* ((command (or command emacs-command-loop-gui-command))
         (effective-command
          (or effective-command emacs-command-loop-gui-effective-command))
         (status (or status emacs-command-loop-gui-status))
         (command-name
          (emacs-command-loop-gui-writeback-command-name
           command effective-command)))
    (dolist (callback '(:clear-display-prefix-after-command
                        :write-minibuffer-state
                        :write-redisplay-state
                        :write-prefix-arg-state
                        :write-kmacro-state
                        :write-last-command-state
                        :write-kill-ring-state
                        :write-window-split-delta
                        :write-window-dedicated-state
                        :write-side-windows-state
                        :write-frame-state))
      (emacs-command-loop-gui--backend-call callback))
    (list :command-name command-name
          :lane (emacs-command-loop-gui-finalize-status
                 :command command
                 :effective-command effective-command
                 :status status))))

;;;###autoload
(defun emacs-command-loop-gui-write-post-command-state-current-context ()
  "Refresh GUI context and flush post-command state through the backend."
  (emacs-command-loop-gui-refresh-context-from-backend)
  (emacs-command-loop-gui-write-post-command-state
   emacs-command-loop-gui-command
   emacs-command-loop-gui-effective-command
   emacs-command-loop-gui-status))

;;;###autoload
(defun emacs-command-loop-gui-lane-writeback-spec (&optional lane)
  "Return GUI transport writeback spec for post-command LANE.
The command loop owns which pieces of state a status lane exposes; the
GUI bridge backend owns the concrete transport writes."
  (let ((lane (cond
               ((symbolp lane) lane)
               ((stringp lane) (intern lane))
               (t 'normal))))
    (cond
     ((eq lane 'read-only)
      '(:status t :buffer t :read-only-one t
        :point t :mark t :window-start t :written t))
     ((eq lane 'unsupported)
      '(:status t))
     ((eq lane 'error)
      '(:status t :buffer t :file t :read-only t
        :point t :mark t :window-start t :written t))
     ((eq lane 'minibuffer)
      '(:status t :minibuffer t :buffer t
        :point t :mark t :window-start t :written t))
     ((eq lane 'prefix-arg)
      '(:status t :point t :mark t :window-start t
        :prefix-arg t :written t))
     (t nil))))

;;;###autoload
(defun emacs-command-loop-gui-writeback-spec-flag (spec key)
  "Return non-nil when GUI writeback SPEC enables KEY.
SPEC is a plist returned by `emacs-command-loop-gui-lane-writeback-spec'."
  (and spec (plist-get spec key)))

;;;###autoload
(defun emacs-command-loop-gui-write-lane-state (&optional lane)
  "Write GUI bridge transport state for post-command LANE.
The command-loop runtime owns the lane spec and callback ordering.  The
registered GUI backend owns concrete transport writes.  Return non-nil
when LANE has a writeback spec."
  (let ((spec (emacs-command-loop-gui-lane-writeback-spec lane)))
    (when spec
      (when (emacs-command-loop-gui-writeback-spec-flag spec :status)
        (emacs-command-loop-gui--backend-call :write-status-state))
      (when (emacs-command-loop-gui-writeback-spec-flag spec :minibuffer)
        (emacs-command-loop-gui--backend-call :write-minibuffer-state))
      (when (emacs-command-loop-gui-writeback-spec-flag spec :buffer)
        (emacs-command-loop-gui--backend-call :write-buffer-state))
      (when (emacs-command-loop-gui-writeback-spec-flag spec :file)
        (emacs-command-loop-gui--backend-call :write-file-state))
      (when (emacs-command-loop-gui-writeback-spec-flag spec :read-only-one)
        (emacs-command-loop-gui--backend-call :write-read-only-one-state))
      (when (emacs-command-loop-gui-writeback-spec-flag spec :read-only)
        (emacs-command-loop-gui--backend-call :write-read-only-state))
      (when (emacs-command-loop-gui-writeback-spec-flag spec :point)
        (emacs-command-loop-gui--backend-call :write-point-state))
      (when (emacs-command-loop-gui-writeback-spec-flag spec :mark)
        (emacs-command-loop-gui--backend-call :write-mark-state))
      (when (emacs-command-loop-gui-writeback-spec-flag spec :window-start)
        (emacs-command-loop-gui--backend-call :write-window-start-state))
      (when (emacs-command-loop-gui-writeback-spec-flag spec :prefix-arg)
        (emacs-command-loop-gui--backend-call :write-prefix-arg-state))
      (when (emacs-command-loop-gui-writeback-spec-flag spec :written)
        (emacs-command-loop-gui--backend-call :mark-written-state))
      t)))

(defun emacs-command-loop-gui--commandp (command)
  "Return non-nil when COMMAND is executable in the GUI bridge context."
  (or (emacs-command-loop-gui--backend-call :commandp command)
      (emacs-command-loop-gui-command-accepted-p command)))

(defun emacs-command-loop-gui--call-command (command)
  "Call COMMAND through the backend or by direct `funcall'."
  (let ((adapter-kind (emacs-command-loop-gui-command-adapter-kind command)))
    (if adapter-kind
        (emacs-command-loop-gui--backend-call :call-adapted-command
                                              command adapter-kind)
      (if (emacs-command-loop-gui--backend-function :call-command)
          (emacs-command-loop-gui--backend-call :call-command command)
        (if (and (symbolp command) (fboundp command))
            (funcall command)
          (if (functionp command)
              (funcall command)
            nil))))))

;;;###autoload
(defun emacs-command-loop-gui-before-command (&optional command)
  "Run GUI bridge pre-command policy for COMMAND.
The command loop owns the policy: `cycle-spacing' preserves its own
cycle state, while other commands clear it through the transport backend."
  (let ((command (or command emacs-command-loop-gui-command)))
    (if (equal (cond
                ((symbolp command) (symbol-name command))
                ((stringp command) command)
                (t ""))
               "cycle-spacing")
        nil
      (or (emacs-command-loop-gui--backend-call
           :clear-cycle-spacing-state)
          (emacs-command-loop-gui--backend-call
           :before-command command)))))

;;;###autoload
(defun emacs-command-loop-gui-self-insert-key-text (&optional keys)
  "Return text inserted by an unbound GUI bridge KEYS sequence, or nil.
Single-character keys insert themselves.  The bridge spelling `SPC'
inserts a literal space."
  (let ((keys (or keys emacs-command-loop-gui-keys "")))
    (cond
     ((equal keys "SPC") " ")
     ((= (length keys) 1) keys)
     (t nil))))

;;;###autoload
(defun emacs-command-loop-gui-minibuffer-active-p ()
  "Return non-nil when the GUI bridge minibuffer is active."
  (emacs-command-loop-gui--backend-call :minibuffer-active-p))

;;;###autoload
(defun emacs-command-loop-gui-minibuffer-key ()
  "Return the current GUI bridge minibuffer key sequence."
  (or (emacs-command-loop-gui--backend-call :minibuffer-key)
      emacs-command-loop-gui-keys
      ""))

;;;###autoload
(defun emacs-command-loop-gui-minibuffer-initial-input ()
  "Return initial input used when a GUI key starts the minibuffer."
  (or (emacs-command-loop-gui--backend-call :minibuffer-initial-input)
      emacs-command-loop-gui-arg
      ""))

;;;###autoload
(defun emacs-command-loop-gui-finish-command ()
  "Finish GUI bridge command bookkeeping.
This promotes `this-command' to `last-command' without clearing the GUI
bridge key/status transport.  The regular command-loop finisher also
clears `this-command-keys', which is not always appropriate for the
source-v1 bridge image."
  (setq emacs-command-loop--last-command emacs-command-loop--this-command
        emacs-command-loop--this-command nil
        emacs-command-loop--real-this-command nil)
  emacs-command-loop--last-command)

;;;###autoload
(defun emacs-command-loop-gui-minibuffer-handle-key ()
  "Handle the current GUI key when the minibuffer is already active.
The command-loop owns the dispatch branch.  `emacs-minibuffer' owns
minibuffer key semantics; the backend remains a transport adapter."
  (let ((explicit-context
         (or (emacs-command-loop-gui--backend-function :minibuffer-key)
             (emacs-command-loop-gui--backend-function :minibuffer-purpose))))
    (cond
     ((and explicit-context
           (fboundp 'emacs-minibuffer-gui-handle-key))
      (emacs-minibuffer-gui-handle-key
       (emacs-command-loop-gui-minibuffer-key)
       (or (emacs-command-loop-gui--backend-call :minibuffer-purpose)
           nil)))
     ((fboundp 'emacs-minibuffer-gui-handle-key-current-context)
      (emacs-minibuffer-gui-handle-key-current-context))
     ((fboundp 'emacs-minibuffer-gui-handle-key)
      (emacs-minibuffer-gui-handle-key
       (emacs-command-loop-gui-minibuffer-key)
       (or (emacs-command-loop-gui--backend-call :minibuffer-purpose)
           nil)))
     (t
      (emacs-command-loop-gui--backend-call :minibuffer-handle-key)))))

;;;###autoload
(defun emacs-command-loop-gui-maybe-start-minibuffer ()
  "Start a GUI minibuffer for the current key sequence when appropriate.
Mode-local minibuffer keymap entries are checked before the global GUI
minibuffer keymap when `emacs-minibuffer' provides the runtime helper."
  (let ((explicit-context
         (or (emacs-command-loop-gui--backend-function
              :minibuffer-mode-keymap-source)
             (emacs-command-loop-gui--backend-function
              :minibuffer-keymap-source)
             (emacs-command-loop-gui--backend-function :minibuffer-key)
             (emacs-command-loop-gui--backend-function
              :minibuffer-initial-input))))
    (cond
     ((and explicit-context
           (fboundp 'emacs-minibuffer-gui-maybe-start-from-keymaps))
      (emacs-minibuffer-gui-maybe-start-from-keymaps
       (or (emacs-command-loop-gui--backend-call
            :minibuffer-mode-keymap-source)
           "")
       (or (emacs-command-loop-gui--backend-call
            :minibuffer-keymap-source)
           "")
       (emacs-command-loop-gui-minibuffer-key)
       (emacs-command-loop-gui-minibuffer-initial-input)))
     ((fboundp 'emacs-minibuffer-gui-maybe-start-current-context)
      (emacs-minibuffer-gui-maybe-start-current-context))
     ((fboundp 'emacs-minibuffer-gui-maybe-start-from-keymaps)
      (emacs-minibuffer-gui-maybe-start-from-keymaps
       (or (emacs-command-loop-gui--backend-call
            :minibuffer-mode-keymap-source)
           "")
       (or (emacs-command-loop-gui--backend-call
            :minibuffer-keymap-source)
           "")
       (emacs-command-loop-gui-minibuffer-key)
       (emacs-command-loop-gui-minibuffer-initial-input)))
     (t
      (emacs-command-loop-gui--backend-call :maybe-start-minibuffer)))))

;;;###autoload
(defun emacs-command-loop-gui-call-interactively (&optional command)
  "Call COMMAND in the GUI bridge context.
When COMMAND is nil, use `emacs-command-loop-gui-command'."
  (let ((resolved-command (or command emacs-command-loop-gui-command)))
    (when resolved-command
      (emacs-command-loop-gui--set-command resolved-command)
      (emacs-command-loop-set-this-command resolved-command)
      (let ((result (emacs-command-loop-gui--call-command resolved-command)))
        ;; Source-v1 bridge images currently route
        ;; `emacs-command-loop-clear-this-command-keys' through transport
        ;; status state.  Preserve command bookkeeping here without letting
        ;; a failed bridge command be reset to "ok".
        (emacs-command-loop-gui-finish-command)
        result))))

;;;###autoload
(defun emacs-command-loop-gui-call-interactively-context (&rest plist)
  "Set GUI bridge command context from PLIST and call it interactively."
  (apply #'emacs-command-loop-gui-set-context plist)
  (emacs-command-loop-gui-call-interactively
   emacs-command-loop-gui-command))

;;;###autoload
(defun emacs-command-loop-gui-call-interactively-current-context ()
  "Refresh GUI context from the backend and call its current command."
  (emacs-command-loop-gui-refresh-context-from-backend)
  (emacs-command-loop-gui-call-interactively
   emacs-command-loop-gui-command))

;;;###autoload
(defun emacs-command-loop-gui-command-execute-call (&optional command)
  "Run COMMAND after command-execute admission checks.
This helper owns the final GUI command execution choice: run normal
commands through `emacs-command-loop-gui-call-interactively', or use
prefix-arg execution when a textual GUI prefix arg is pending.  COMMAND
defaults to `emacs-command-loop-gui-command'."
  (let ((command (or command emacs-command-loop-gui-command)))
    (when command
      (emacs-command-loop-gui--set-command command))
    (when (emacs-command-loop-gui--backend-function :current-prefix-arg)
      (emacs-command-loop-gui--set-prefix-arg
       (emacs-command-loop-gui--backend-call :current-prefix-arg)))
    (emacs-command-loop-gui-before-command command)
    (if (or (not (emacs-command-loop-gui--backend-function
                  :prefix-arg-empty-p))
            (emacs-command-loop-gui--backend-call :prefix-arg-empty-p))
        (emacs-command-loop-gui-call-interactively command)
      (if (emacs-command-loop-gui--backend-function :execute-with-prefix-arg)
          (emacs-command-loop-gui--backend-call :execute-with-prefix-arg)
        (emacs-command-loop-gui-execute-with-prefix-arg)))))

;;;###autoload
(defun emacs-command-loop-gui-command-execute (&optional command)
  "Execute COMMAND using GUI bridge command semantics.
This mirrors the bridge runtime's transport-aware `command-execute':
known commands run through `gui-call-interactively', read-only buffers
can reject mutating commands, prefix-arg commands update prefix state,
and unknown unbound commands become `unsupported'."
  (let ((command (or command emacs-command-loop-gui-command)))
    (emacs-command-loop-gui--set-command command)
    (cond
     ((not (emacs-command-loop-gui--commandp command))
      (emacs-command-loop-gui--set-status "unsupported")
      nil)
     ((and (emacs-command-loop-gui--backend-call :read-only-p)
           (or (emacs-command-loop-gui--backend-call
                :read-only-command-p command)
               (emacs-command-loop-gui-read-only-command-p command)))
      (emacs-command-loop-gui--set-status "read-only")
      nil)
     ((or (emacs-command-loop-gui--backend-call :prefix-command-p command)
          (emacs-command-loop-gui-prefix-command-p command))
      (emacs-command-loop-gui-call-interactively command))
     (t
      (emacs-command-loop-gui-command-execute-call command)))))

;;;###autoload
(defun emacs-command-loop-gui-command-execute-context (&rest plist)
  "Set GUI bridge command context from PLIST and execute its command."
  (apply #'emacs-command-loop-gui-set-context plist)
  (emacs-command-loop-gui-command-execute
   emacs-command-loop-gui-command))

;;;###autoload
(defun emacs-command-loop-gui-command-execute-current-context ()
  "Refresh GUI context from the backend and execute its current command."
  (emacs-command-loop-gui-refresh-context-from-backend)
  (emacs-command-loop-gui-command-execute
   emacs-command-loop-gui-command))

;;;###autoload
(defun emacs-command-loop-gui-execute-extended-command
    (&optional requested minibuffer-arg)
  "Execute REQUESTED as a GUI bridge M-x command.
REQUESTED defaults to `emacs-command-loop-gui-arg'.  MINIBUFFER-ARG
defaults to the backend's `:current-minibuffer-arg' callback.  The
runtime owns M-x command selection and status policy; the bridge backend
owns transport storage and command-specific execution."
  (let ((requested (or requested emacs-command-loop-gui-arg ""))
        (minibuffer-arg
         (or minibuffer-arg
             (emacs-command-loop-gui--backend-call
              :current-minibuffer-arg)
             "")))
    (if (or (equal requested "")
            (equal requested "execute-extended-command"))
        (progn
          (emacs-command-loop-gui--set-status "unsupported")
          nil)
      (emacs-command-loop-gui--set-effective-command requested)
      (emacs-command-loop-gui--set-command (intern requested))
      (when (not (equal minibuffer-arg ""))
        (emacs-command-loop-gui--set-arg minibuffer-arg))
      (let ((result (emacs-command-loop-gui-command-execute)))
        (emacs-command-loop-gui--set-command 'execute-extended-command)
        result))))

;;;###autoload
(defun emacs-command-loop-gui-execute-extended-command-current-context ()
  "Refresh GUI context and execute the current GUI bridge M-x request."
  (emacs-command-loop-gui-refresh-context-from-backend)
  (emacs-command-loop-gui-execute-extended-command))

;;;###autoload
(defun emacs-command-loop-gui-project-command
    (&optional requested command-arg wrapper-command)
  "Execute a GUI project command selected by REQUESTED.
REQUESTED defaults to `emacs-command-loop-gui-arg'.  Empty REQUESTED
defaults to \"project-dired\", matching Emacs project command prompts in
the GUI bridge.  COMMAND-ARG defaults to the backend's
`:current-minibuffer-arg' callback.  WRAPPER-COMMAND is restored as the
current GUI command after the selected command runs."
  (let ((requested (or requested emacs-command-loop-gui-arg ""))
        (command-arg
         (or command-arg
             (emacs-command-loop-gui--backend-call
              :current-minibuffer-arg)
             ""))
        (wrapper-command
         (or wrapper-command emacs-command-loop-gui-command
             'project-any-command)))
    (when (equal requested "")
      (setq requested "project-dired"))
    (emacs-command-loop-gui--set-effective-command requested)
    (emacs-command-loop-gui--set-command (intern requested))
    (emacs-command-loop-gui--set-arg command-arg)
    (let ((result (emacs-command-loop-gui-command-execute)))
      (emacs-command-loop-gui--set-command wrapper-command)
      result)))

;;;###autoload
(defun emacs-command-loop-gui-project-command-current-context
    (&optional wrapper-command)
  "Refresh GUI context and execute a project command request."
  (emacs-command-loop-gui-refresh-context-from-backend)
  (emacs-command-loop-gui-project-command
   nil nil (or wrapper-command emacs-command-loop-gui-command)))

(defconst emacs-command-loop-gui-undo-save-command-names
  '("kill-word"
    "kill-sexp"
    "backward-kill-word"
    "zap-to-char"
    "expand-abbrev"
    "dabbrev-expand"
    "dabbrev-completion"
    "complete-symbol"
    "transpose-words"
    "transpose-sexps"
    "insert-parentheses"
    "move-past-close-and-reindent"
    "fill-paragraph"
    "kill-sentence"
    "backward-kill-sentence"
    "transpose-chars"
    "delete-horizontal-space"
    "cycle-spacing"
    "just-one-space"
    "delete-indentation"
    "comment-line"
    "comment-dwim"
    "upcase-word"
    "downcase-word"
    "capitalize-word"
    "upcase-region"
    "downcase-region"
    "capitalize-region"
    "sort-lines"
    "delete-char"
    "backward-delete-char"
    "delete-backward-char"
    "self-insert-command"
    "insert-char"
    "quoted-insert"
    "indent-for-tab-command"
    "tab-to-tab-stop"
    "indent-region"
    "indent-rigidly"
    "newline"
    "electric-newline-and-maybe-indent"
    "default-indent-new-line"
    "open-line"
    "split-line"
    "delete-blank-lines"
    "kill-line"
    "kill-whole-line"
    "yank"
    "yank-pop"
    "delete-region"
    "kill-region"
    "kill-rectangle"
    "rectangle-number-lines"
    "delete-rectangle"
    "clear-rectangle"
    "open-rectangle"
    "string-rectangle"
    "yank-rectangle"
    "replace-string"
    "replace-regexp"
    "query-replace"
    "query-replace-regexp"
    "project-query-replace-regexp"
    "delete-trailing-whitespace"
    "untabify"
    "insert-file"
    "insert-buffer"
    "insert-register"
    "increment-register")
  "GUI bridge commands that require an undo snapshot before execution.")

;;;###autoload
(defun emacs-command-loop-gui-undo-save-command-p (&optional command)
  "Return non-nil when COMMAND should save GUI undo state before dispatch.
When COMMAND is nil, use `emacs-command-loop-gui-command'."
  (let* ((command (or command emacs-command-loop-gui-command))
         (name (cond
                ((symbolp command) (symbol-name command))
                ((stringp command) command)
                (t nil))))
    (and name
         (member name emacs-command-loop-gui-undo-save-command-names)
         t)))

;;;###autoload
(defun emacs-command-loop-gui-save-undo-if-needed (&optional command)
  "Save GUI undo state for COMMAND when command-loop policy requires it."
  (let ((command (or command emacs-command-loop-gui-command)))
    (cond
     ((emacs-command-loop-gui-undo-save-command-p command)
      (or (emacs-command-loop-gui--backend-call :save-undo-state)
          (emacs-command-loop-gui--backend-call :save-undo-if-needed)))
     (t nil))))

;;;###autoload
(defun emacs-command-loop-gui-key-dispatch-spec
    (&optional resolved keys arg)
  "Return normalized GUI key dispatch data.
RESOLVED is the command name returned by key lookup.  When nil, the GUI
backend's `:lookup-key-sequence' callback is used.  KEYS and ARG default
to the active GUI bridge context.  The returned plist contains
`:command', `:effective-command', `:arg', and `:status'."
  (let* ((resolved (or resolved
                       (emacs-command-loop-gui--backend-call
                        :lookup-key-sequence)
                       ""))
         (keys (or keys emacs-command-loop-gui-keys ""))
         (arg (or arg emacs-command-loop-gui-arg ""))
         (self-insert-text
          (emacs-command-loop-gui-self-insert-key-text keys)))
    (cond
     ((and (equal resolved "") self-insert-text)
      (list :command 'self-insert-command
            :effective-command "self-insert-command"
            :arg (if (equal arg "") self-insert-text arg)
            :status "ok"
            :self-insert-text self-insert-text))
     ((equal resolved "")
      (list :command nil
            :effective-command keys
            :arg arg
            :status "unsupported"))
     (t
      (list :command (intern resolved)
            :effective-command resolved
            :arg arg
            :status "ok")))))

;;;###autoload
(defun emacs-command-loop-gui-dispatch-key-sequence ()
  "Dispatch `emacs-command-loop-gui-keys' in the GUI bridge context.
The backend supplies transport-specific key lookup and minibuffer
operations; this function owns the command selection/status flow."
  (cond
   ((emacs-command-loop-gui--backend-call :quoted-insert-p)
    (emacs-command-loop-gui--backend-call :clear-quoted-insert)
    (let ((arg (or (emacs-command-loop-gui--backend-call
                    :quoted-insert-key-text)
                   "")))
      (emacs-command-loop-gui--set-arg arg)
      (if (equal arg "")
          (progn
            (emacs-command-loop-gui--set-effective-command
             emacs-command-loop-gui-keys)
            (emacs-command-loop-gui--set-status "unsupported")
            nil)
        (emacs-command-loop-gui--set-effective-command "quoted-insert")
        (emacs-command-loop-gui--set-command 'quoted-insert)
        (emacs-command-loop-gui-save-undo-if-needed)
        (emacs-command-loop-gui-command-execute))))
   ((emacs-command-loop-gui-minibuffer-active-p)
    (emacs-command-loop-gui-minibuffer-handle-key))
   ((emacs-command-loop-gui-maybe-start-minibuffer)
    nil)
   (t
    (let* ((spec (emacs-command-loop-gui-key-dispatch-spec))
           (command (plist-get spec :command))
           (effective-command (plist-get spec :effective-command))
           (status (plist-get spec :status))
           (arg (plist-get spec :arg)))
      (when (plist-member spec :self-insert-text)
        (emacs-command-loop-gui--set-arg arg))
      (if (not command)
          (progn
            (emacs-command-loop-gui--set-effective-command effective-command)
            (emacs-command-loop-gui--set-status status)
            nil)
        (emacs-command-loop-gui--set-effective-command effective-command)
        (emacs-command-loop-gui--set-command command)
        (emacs-command-loop-gui-save-undo-if-needed)
        (emacs-command-loop-gui-command-execute))))))

;;;###autoload
(defun emacs-command-loop-gui-dispatch-context (&rest plist)
  "Set GUI bridge command context from PLIST and dispatch the key sequence."
  (apply #'emacs-command-loop-gui-set-context plist)
  (emacs-command-loop-gui-dispatch-key-sequence))

;;;###autoload
(defun emacs-command-loop-gui-dispatch-current-context ()
  "Refresh GUI context from the backend and dispatch its current key sequence."
  (emacs-command-loop-gui-refresh-context-from-backend)
  (emacs-command-loop-gui-dispatch-key-sequence))

;;;###autoload
(defun emacs-command-loop-gui-after-key-dispatch ()
  "Run GUI bridge post-key-dispatch bookkeeping.
This is separated from raw key command selection so bridge adapters can
route every key-dispatch entrypoint through the same command-loop-owned
post-key policy."
  (emacs-command-loop-gui--backend-call :after-key-dispatch))

;;;###autoload
(defun emacs-command-loop-gui-dispatch-key-request ()
  "Dispatch the current GUI key request and run post-key bookkeeping."
  (let ((result (emacs-command-loop-gui-dispatch-key-sequence)))
    (emacs-command-loop-gui-after-key-dispatch)
    result))

;;;###autoload
(defun emacs-command-loop-gui-dispatch-key-request-context (&rest plist)
  "Set GUI bridge command context from PLIST and run a key request."
  (apply #'emacs-command-loop-gui-set-context plist)
  (emacs-command-loop-gui-dispatch-key-request))

;;;###autoload
(defun emacs-command-loop-gui-dispatch-key-request-current-context ()
  "Refresh GUI context from the backend and run a key request."
  (emacs-command-loop-gui-refresh-context-from-backend)
  (emacs-command-loop-gui-dispatch-key-request))

;;;###autoload
(defun emacs-command-loop-gui-run-request ()
  "Run the current GUI bridge request.
Direct command requests execute `emacs-command-loop-gui-command' through
`emacs-command-loop-gui-command-execute'.  Raw key requests dispatch
`emacs-command-loop-gui-keys' through `emacs-command-loop-gui-dispatch-key-sequence'.
The optional backend callback `:after-key-dispatch' is transport-local
bookkeeping such as macro recording."
  (if emacs-command-loop-gui-command
      (progn
        (emacs-command-loop-gui-command-execute)
        'direct)
    (if (equal emacs-command-loop-gui-keys "")
        (progn
          (emacs-command-loop-gui-command-execute)
          'direct)
      (progn
        (emacs-command-loop-gui-dispatch-key-request)
        'key))))

;;;###autoload
(defun emacs-command-loop-gui-run-request-context (&rest plist)
  "Set GUI bridge command context from PLIST and run that request."
  (apply #'emacs-command-loop-gui-set-context plist)
  (emacs-command-loop-gui-run-request))

;;;###autoload
(defun emacs-command-loop-gui-run-request-current-context ()
  "Refresh GUI context from the backend and run the current request."
  (emacs-command-loop-gui-refresh-context-from-backend)
  (emacs-command-loop-gui-run-request))

;;;###autoload
(defun emacs-command-loop-gui-replay-key-lines (source dispatch)
  "Replay newline-separated key SOURCE by calling DISPATCH for each key.
Empty lines are ignored.  DISPATCH is called with one key sequence
string at a time, preserving SOURCE order.  Bridge adapters own the
mutable transport state around each dispatch; the command-loop runtime
owns the key-record parsing policy."
  (let ((source (or source ""))
        (index 0)
        (start 0)
        (count 0))
    (while (<= index (length source))
      (if (or (= index (length source))
              (= (aref source index) 10))
          (let ((line (substring source start index)))
            (when (not (equal line ""))
              (funcall dispatch line)
              (setq count (+ count 1)))
            (setq start (+ index 1)))
        nil)
      (setq index (+ index 1)))
    count))

;;;; --- feed helpers ---------------------------------------------------

(defun emacs-command-loop-feed-events (&rest events)
  "Append EVENTS to the substrate queue (= FIFO order).
First arg consumed first by `emacs-command-loop-read-event'."
  (setq emacs-command-loop--unread-events
        (append emacs-command-loop--unread-events events))
  events)

(defun emacs-command-loop-pending-p ()
  "Return non-nil when there is at least one queued event.
Drains both the substrate queue and a bound `unread-command-events'
defvar (= the standalone-Emacs convention)."
  (or emacs-command-loop--unread-events
      (and (boundp 'unread-command-events)
           (symbol-value 'unread-command-events))))

;;;; --- readers --------------------------------------------------------

(defun emacs-command-loop--pop-event ()
  "Pop one event from the active queue.  Substrate first, then the
bound `unread-command-events' if any.  Returns the event, or signals
`emacs-command-loop-no-input' on empty."
  (cond
   (emacs-command-loop--unread-events
    (let ((ev (car emacs-command-loop--unread-events)))
      (setq emacs-command-loop--unread-events
            (cdr emacs-command-loop--unread-events))
      ev))
   ((and (boundp 'unread-command-events)
         (symbol-value 'unread-command-events))
    (let* ((q (symbol-value 'unread-command-events))
           (ev (car q)))
      (set 'unread-command-events (cdr q))
      ev))
   (t (signal 'emacs-command-loop-no-input nil))))

(defun emacs-command-loop-read-event (&optional prompt _suppress seconds)
  "Read one event from the queue.
PROMPT and SECONDS are accepted for API parity but ignored — the
substrate has no terminal access; the future TUI bridge will wire
`emacs-tui-event-poll' here.

Side effect: updates `emacs-command-loop--last-input-event' (and the
non-menu mirror)."
  (ignore prompt seconds)
  (when (and emacs-command-loop--quit-flag
             (not emacs-command-loop--inhibit-quit))
    (setq emacs-command-loop--quit-flag nil)
    (signal 'emacs-command-loop-quit nil))
  (let ((ev (emacs-command-loop--pop-event)))
    (setq emacs-command-loop--last-input-event   ev
          emacs-command-loop--last-nonmenu-event ev)
    ;; Track X follow-up (2026-05-05): also publish the event to the
    ;; canonical unprefixed defvars (= what `self-insert-command' /
    ;; `digit-argument' / etc. read).  Without this mirror the public
    ;; `last-input-event' / `last-nonmenu-event' stay nil, and any
    ;; command dispatched via `emacs-command-loop-step' that consults
    ;; them sees a stale nil even though the prefixed slot was set.
    (when (boundp 'last-input-event)
      (set 'last-input-event ev))
    (when (boundp 'last-nonmenu-event)
      (set 'last-nonmenu-event ev))
    ev))

(defun emacs-command-loop-read-char (&optional prompt _ihib seconds)
  "Like `read-event' but require the result to be a character (integer).
Symbols and lists signal `wrong-type-argument'."
  (let ((ev (emacs-command-loop-read-event prompt nil seconds)))
    (unless (integerp ev)
      (signal 'wrong-type-argument (list 'integerp ev)))
    ev))

(defun emacs-command-loop-read-command (prompt &optional default-value)
  "Read a command name with completion, returning the symbol.
MVP: routes through the minibuffer reader (when available) and
falls back to a queue-fed string.  DEFAULT-VALUE is used when the
read input is empty."
  (let* ((reader (cond
                  ((fboundp 'emacs-minibuffer-completing-read)
                   (lambda (p)
                     (emacs-minibuffer-completing-read
                      p obarray 'commandp t nil
                      'extended-command-history default-value)))
                  ((fboundp 'completing-read)
                   (lambda (p)
                     (completing-read p obarray 'commandp t nil
                                      'extended-command-history
                                      default-value)))
                  (t (lambda (_p)
                       (let ((ev (emacs-command-loop--pop-event)))
                         (cond ((stringp ev) ev)
                               ((symbolp ev) (symbol-name ev))
                               (t (format "%s" ev))))))))
         (input (funcall reader prompt))
         (name (cond
                ((or (null input) (and (stringp input)
                                       (= (length input) 0)))
                 (cond ((symbolp default-value) default-value)
                       ((stringp default-value) (intern default-value))
                       (t nil)))
                ((symbolp input) input)
                ((stringp input) (intern input))
                (t (signal 'wrong-type-argument
                           (list 'string-or-symbol input))))))
    name))

;;;; --- this-command-keys family --------------------------------------

(defun emacs-command-loop-clear-this-command-keys (&optional keep-record)
  "Reset the per-command key accumulator.  KEEP-RECORD reserved for
parity; the substrate has no recent-keys ring yet."
  (ignore keep-record)
  (setq emacs-command-loop--this-command-keys ""))

(defun emacs-command-loop-record-key (event)
  "Append EVENT to the current command-keys accumulator.
Integer events are appended as their character; other events are
appended as their `format'-printed representation (= MVP)."
  (let ((s (cond
            ((integerp event) (string event))
            ((stringp event)  event)
            ((symbolp event)  (symbol-name event))
            (t (format "%s" event)))))
    (setq emacs-command-loop--this-command-keys
          (concat emacs-command-loop--this-command-keys s))
    (setq emacs-command-loop--last-command-event event)
    ;; Track X follow-up (2026-05-05): mirror to the public unprefixed
    ;; `last-command-event' so `self-insert-command' (= reads it for the
    ;; char to insert) and other interactive commands see the value the
    ;; command-loop just consumed.  Same rationale as `read-event'.
    (when (boundp 'last-command-event)
      (set 'last-command-event event))
    s))

(defun emacs-command-loop-this-command-keys ()
  "Return the accumulated key string for the current command."
  emacs-command-loop--this-command-keys)

(defun emacs-command-loop-this-command-keys-vector ()
  "Return the accumulated keys as a vector of events.
MVP: each char of the string accumulator becomes one element."
  (let* ((s emacs-command-loop--this-command-keys)
         (n (length s))
         (v (make-vector n 0))
         (i 0))
    (while (< i n)
      (aset v i (aref s i))
      (setq i (+ i 1)))
    v))

;;;; --- command bookkeeping -------------------------------------------

(defun emacs-command-loop-set-this-command (cmd)
  "Set the command currently being dispatched to CMD."
  (setq emacs-command-loop--this-command      cmd
        emacs-command-loop--real-this-command cmd))

(defun emacs-command-loop-mark-command-finished ()
  "Promote `this-command' → `last-command' and clear the key buffer.
Called by `command-loop-1' (B.4) at the end of each iteration."
  (setq emacs-command-loop--last-command emacs-command-loop--this-command
        emacs-command-loop--this-command nil
        emacs-command-loop--real-this-command nil)
  (emacs-command-loop-clear-this-command-keys))

;;;; --- read-key-sequence (Phase B.2) ---------------------------------

(defun emacs-command-loop--keys-stringable-p (vec)
  "Return non-nil when every element of VEC is a plain ASCII char.
Used to decide whether `read-key-sequence' returns a string or a
vector — matches Emacs' contract that a sequence of unmodified
chars folds to a string."
  (let ((i 0) (n (length vec)) (ok t))
    (while (and ok (< i n))
      (let ((e (aref vec i)))
        (unless (and (integerp e)
                     (>= e 0)
                     (< e #x80))
          (setq ok nil)))
      (setq i (1+ i)))
    ok))

(defun emacs-command-loop--vec->string (vec)
  "Concatenate VEC of chars into a string."
  (let* ((n (length vec))
         (s (make-string n 0))
         (i 0))
    (while (< i n)
      (aset s i (aref vec i))
      (setq i (1+ i)))
    s))

(defun emacs-command-loop--read-keys-vec (prompt)
  "Read one complete key sequence as a vector of events.
Walks the active keymap chain (= via `emacs-keymap-key-binding')
after each event; if the lookup returns a keymap the sequence is a
prefix and we keep reading; on a non-keymap binding (function /
symbol / lambda / cons / nil) we stop and return the accumulated
vector."
  (let ((vec [])
        (done nil))
    (while (not done)
      (let* ((ev (emacs-command-loop-read-event prompt))
             (next (vconcat vec (vector ev)))
             (binding (cond
                       ((fboundp 'emacs-keymap-key-binding)
                        (emacs-keymap-key-binding next))
                       ((fboundp 'key-binding) (key-binding next))
                       (t nil))))
        (setq vec next)
        (emacs-command-loop-record-key ev)
        (cond
         ;; Prefix: a keymap binding means more events expected.
         ((and binding
               (or (and (fboundp 'emacs-keymap-keymapp)
                        (emacs-keymap-keymapp binding))
                   (and (fboundp 'keymapp) (keymapp binding))))
          nil)
         (t
          (setq done t)))))
    vec))

(defun emacs-command-loop-read-key-sequence (&optional prompt _continue
                                                       _dont-downcase
                                                       _can-return-switch
                                                       _cmd-loop)
  "Read one complete key sequence; return a string or a vector.
Returns a string when every event in the sequence is an unmodified
ASCII char, else a vector.  PROMPT and the four optional arguments
are accepted for API parity but ignored in the MVP."
  (let ((vec (emacs-command-loop--read-keys-vec prompt)))
    (if (emacs-command-loop--keys-stringable-p vec)
        (emacs-command-loop--vec->string vec)
      vec)))

(defun emacs-command-loop-read-key-sequence-vector (&optional prompt
                                                              _continue
                                                              _dont-downcase
                                                              _can-return-switch
                                                              _cmd-loop)
  "Like `emacs-command-loop-read-key-sequence' but always vector."
  (emacs-command-loop--read-keys-vec prompt))

;;;; --- prefix-arg state (Phase B.3 placeholder, B.5 driver) ----------

(defvar emacs-command-loop--prefix-arg nil
  "Pending prefix arg to be consumed by the next `call-interactively'.
Set by `universal-argument' / `digit-argument' (= B.5).")

(defvar emacs-command-loop--current-prefix-arg nil
  "The prefix arg in effect for the command currently being executed.
Bound by `call-interactively' from `prefix-arg' before it dispatches.")

;;;; --- call-interactively (Phase B.3) --------------------------------

(defun emacs-command-loop--prefix-numeric-value (arg)
  "Replicate `prefix-numeric-value' for the supported prefix-arg shapes.
- nil → 1
- `-' → -1
- (N) (= a list with one int from `\\[universal-argument]') → N
- integer N → N
- everything else → 1"
  (cond
   ((null arg) 1)
   ((eq arg '-) -1)
   ((integerp arg) arg)
   ((consp arg)
    (let ((head (car arg)))
      (cond
       ((eq head '-) -1)
       ((integerp head) head)
       (t 1))))
   (t 1)))

(defun emacs-command-loop--build-args (spec)
  "Build an args list from an interactive SPEC.

SPEC is the body of `(interactive ...)':
- nil           → no args
- string        → parse a small subset of interactive codes:
                  P (raw prefix), p (numeric prefix), N (number),
                  s (string via read-string), n (number via
                  read-number).  Other codes signal.
- list / form   → eval and return its result as the args list."
  (cond
   ((null spec) nil)
   ((stringp spec)
    (let ((args nil)
          (lines (split-string spec "\n")))
      (dolist (line lines)
        (when (> (length line) 0)
          (let ((code (aref line 0))
                (prompt (substring line 1)))
            (cond
             ((eq code ?P)
              (push emacs-command-loop--current-prefix-arg args))
             ((eq code ?p)
              (push (emacs-command-loop--prefix-numeric-value
                     emacs-command-loop--current-prefix-arg)
                    args))
             ((eq code ?N)
              (push (or emacs-command-loop--current-prefix-arg
                        (if (fboundp 'read-number)
                            (read-number prompt 0)
                          0))
                    args))
             ((eq code ?n)
              (push (if (fboundp 'read-number)
                        (read-number prompt 0)
                      0)
                    args))
             ((eq code ?s)
              (push (if (fboundp 'read-string)
                        (read-string prompt)
                      "")
                    args))
             (t (signal 'emacs-command-loop-error
                        (list 'unsupported-interactive-code code)))))))
      (nreverse args)))
   ((or (consp spec) (functionp spec))
    (eval spec t))
   (t nil)))

(defun emacs-command-loop-call-interactively (function &optional record-flag keys)
  "Phase B.3 MVP: invoke FUNCTION as if interactively from the keyboard.
RECORD-FLAG / KEYS are accepted for API parity.

Reads FUNCTION's interactive form, builds an arg list from the spec,
binds `current-prefix-arg' from the pending `prefix-arg', dispatches
the call, then promotes `this-command' to `last-command'."
  (ignore record-flag keys)
  (unless (or (and (symbolp function) (fboundp function))
              (functionp function))
    (signal 'wrong-type-argument (list 'commandp function)))
  (let* ((form (cond
                ((symbolp function)
                 (or (and (fboundp 'interactive-form)
                          (interactive-form function))
                     ;; Fallback: peek at the lambda's body.
                     (let ((def (and (fboundp function)
                                     (symbol-function function))))
                       (and (consp def)
                            (let ((b (cond
                                      ((eq (car def) 'lambda) (cddr def))
                                      ((eq (car def) 'closure) (cdddr def))
                                      (t nil))))
                              (and b (consp (car b))
                                   (eq (caar b) 'interactive)
                                   (car b)))))))
                ((and (fboundp 'interactive-form)
                      (interactive-form function)))
                (t nil)))
         (spec (and (consp form) (cadr form)))
         (emacs-command-loop--current-prefix-arg
          emacs-command-loop--prefix-arg)
         (args (emacs-command-loop--build-args spec)))
    (setq emacs-command-loop--prefix-arg nil)
    (emacs-command-loop-set-this-command function)
    (let ((result (apply function args)))
      (emacs-command-loop-mark-command-finished)
      result)))

(defun emacs-command-loop-funcall-interactively (function &rest args)
  "Like `funcall' but signal that the call is interactive.
MVP: this is plain `apply' — `called-interactively-p' tracking is
deferred until we have a richer call-stack inspection layer."
  (apply function args))

(defun emacs-command-loop-command-execute (cmd &optional record-flag keys special)
  "Phase B.3 MVP: dispatch CMD as a command.

CMD may be:
- a symbol whose function-slot is a command → call-interactively'd
- a lambda / closure with an interactive form → call-interactively'd
- a string or vector keyboard macro → events re-fed into the queue
- anything else → wrong-type-argument

RECORD-FLAG / KEYS / SPECIAL are accepted for API parity."
  (ignore special)
  (cond
   ((or (stringp cmd) (vectorp cmd))
    (let ((i 0) (n (length cmd)))
      (while (< i n)
        (setq emacs-command-loop--unread-events
              (append emacs-command-loop--unread-events
                      (list (aref cmd i))))
        (setq i (1+ i)))
      nil))
   ((or (and (symbolp cmd) (fboundp cmd))
        (functionp cmd))
    (emacs-command-loop-call-interactively cmd record-flag keys))
   (t (signal 'wrong-type-argument (list 'commandp cmd)))))

;;;; --- command-loop-1 driver (Phase B.4) -----------------------------

(defvar emacs-command-loop--pre-command-hook nil
  "Substrate-internal mirror of `pre-command-hook'.  When the bridge
is loaded, the unprefixed defvar is the canonical place — this
slot is currently unused (= reserved for the standalone path).")

(defvar emacs-command-loop--post-command-hook nil
  "Substrate-internal mirror of `post-command-hook'.  Same notes as
the pre slot.")

(defvar emacs-command-loop--undefined-key-handler nil
  "If non-nil, called with one arg (= the unbound key vector) when
`emacs-command-loop-step' encounters a sequence with no binding.
nil = silently skip and continue.")

(defun emacs-command-loop--lookup-command (key-seq)
  "Look up KEY-SEQ in the active keymap chain, return the binding."
  (cond
   ((fboundp 'emacs-keymap-key-binding)
    (emacs-keymap-key-binding key-seq))
   ((fboundp 'key-binding) (key-binding key-seq))
   (t nil)))

(defun emacs-command-loop-step ()
  "Run one command-loop iteration:
1. read-key-sequence to consume events from the queue,
2. look up the resulting binding,
3. run `pre-command-hook',
4. dispatch via `command-execute' (= which calls `call-interactively'),
5. run `post-command-hook'.
Returns the binding (= function or nil)."
  (let* ((vec (emacs-command-loop--read-keys-vec nil))
         (binding (emacs-command-loop--lookup-command vec)))
    (cond
     ((or (and (fboundp 'emacs-keymap-keymapp)
               (emacs-keymap-keymapp binding))
          (and (fboundp 'keymapp) (keymapp binding)))
      ;; Should not happen — read-keys-vec stops on non-keymap.
      nil)
     ((null binding)
      (when emacs-command-loop--undefined-key-handler
        (funcall emacs-command-loop--undefined-key-handler vec))
      nil)
     (t
      (when (boundp 'pre-command-hook)
        (run-hooks 'pre-command-hook))
      (prog1 (emacs-command-loop-command-execute binding)
        (when (boundp 'post-command-hook)
          (run-hooks 'post-command-hook)))))))

(defun emacs-command-loop-drain ()
  "Run `emacs-command-loop-step' until the unread queue is empty.
Pure drain — does NOT catch quit signals.  Use
`emacs-command-loop-1' for the quit-aware variant.
Returns the number of commands executed."
  (let ((n 0))
    (while (emacs-command-loop-pending-p)
      (emacs-command-loop-step)
      (setq n (1+ n)))
    n))

(defun emacs-command-loop-1 ()
  "Phase B.4 + B.6: drain the unread queue, swallowing `quit' so the
loop continues across user `keyboard-quit' / `C-g' presses.
Each `quit' clears the substrate `quit-flag' and counts as one
iteration.  Returns the iteration count.

Track X (2026-05-04): also swallows `end-of-buffer' /
`beginning-of-buffer' (= signaled by `forward-char' / `backward-char'
when point is at the edge), surfacing them as a soft echo-area message
instead of letting them propagate to the Layer-1 eval-error printer
(= what produced the user-visible \"nelisp: eval error:
args-out-of-range\" before)."
  (let ((n 0))
    (while (emacs-command-loop-pending-p)
      (condition-case _err
          (progn (emacs-command-loop-step)
                 (setq n (1+ n)))
        (quit
         (setq emacs-command-loop--quit-flag nil
               n (1+ n)))
        (emacs-command-loop-quit
         (setq emacs-command-loop--quit-flag nil
               n (1+ n)))
        (end-of-buffer
         (when (fboundp 'message)
           (message "End of buffer"))
         (setq n (1+ n)))
        (beginning-of-buffer
         (when (fboundp 'message)
           (message "Beginning of buffer"))
         (setq n (1+ n)))))
    n))

;;;; --- recursive-edit / quit (Phase B.6) -----------------------------

(defvar emacs-command-loop--recursion-depth 0
  "Number of `recursive-edit' frames currently on the stack.")

(defun emacs-command-loop-keyboard-quit ()
  "Phase B.6: signal `quit'.

When dispatched from inside `command-loop-1' the signal unwinds
back to the loop's `condition-case', which sets `quit-flag = nil'
and continues.  When dispatched from inside `recursive-edit' the
signal unwinds the same way without exiting the recursive-edit
frame (= matches Emacs behaviour: C-g aborts the command, not the
edit session)."
  (interactive)
  (signal 'quit nil))

(defun emacs-command-loop-recursive-edit ()
  "Phase B.6: enter a nested command-loop.

Increments `recursion-depth' for the duration, runs the loop
until the queue empties or `exit-recursive-edit' / `abort-
recursive-edit' throws out.  Returns nil on normal exit, the
abort sentinel (= 'aborted) on abort."
  (interactive)
  (let ((emacs-command-loop--recursion-depth
         (1+ emacs-command-loop--recursion-depth)))
    (catch 'emacs-command-loop-exit
      (emacs-command-loop-1)
      nil)))

(defun emacs-command-loop-exit-recursive-edit ()
  "Phase B.6: throw out of the most recent `recursive-edit' frame."
  (interactive)
  (if (zerop emacs-command-loop--recursion-depth)
      (signal 'emacs-command-loop-error
              '(no-recursive-edit-active))
    (throw 'emacs-command-loop-exit nil)))

(defun emacs-command-loop-abort-recursive-edit ()
  "Phase B.6: throw out with the `aborted' sentinel.
If no recursive-edit is active, signal `quit' instead."
  (interactive)
  (if (zerop emacs-command-loop--recursion-depth)
      (signal 'quit nil)
    (throw 'emacs-command-loop-exit 'aborted)))

(defun emacs-command-loop-top-level ()
  "Phase B.6: drain at the top-level.
Resets state first, then runs `command-loop-1'.  Real Emacs'
top-level throws to a tag set by the C `Frecursive_edit', but
under our drain semantics a reset+drain captures the intent."
  (interactive)
  (emacs-command-loop-reset)
  (emacs-command-loop-1))

(defun emacs-command-loop-recursion-depth ()
  "Return the current `recursive-edit' nesting depth."
  emacs-command-loop--recursion-depth)

;;;; --- prefix-arg setters (Phase B.5) ---------------------------------

(defun emacs-command-loop-universal-argument ()
  "Phase B.5: bind a transient prefix-arg.

- prefix-arg nil  → '(4)
- prefix-arg '-   → '(-4)
- prefix-arg (N)  → list of (* 4 N)   (= C-u C-u multiplies)
- otherwise       → '(4)

Reads `emacs-command-loop--current-prefix-arg' (= the incoming
prefix arg captured by `call-interactively') and writes back to
`emacs-command-loop--prefix-arg' so the next command sees it."
  (interactive)
  (let ((prev emacs-command-loop--current-prefix-arg))
    (setq emacs-command-loop--prefix-arg
          (cond
           ((null prev) '(4))
           ((eq prev '-) '(-4))
           ((and (consp prev) (integerp (car prev)))
            (list (* 4 (car prev))))
           (t '(4))))))

(defun emacs-command-loop-digit-argument (arg)
  "Phase B.5: append a decimal digit to the prefix-arg.

ARG comes from the interactive `P' spec (= the incoming prefix);
the digit value is derived from `last-command-event' (= masked to
the low 7 bits to drop modifiers, then the digit char value)."
  (interactive "P")
  (let* ((event (or emacs-command-loop--last-command-event 0))
         (digit (when (integerp event)
                  (- (logand event #x7f) ?0))))
    (when (and digit (<= 0 digit) (<= digit 9))
      (setq emacs-command-loop--prefix-arg
            (cond
             ((integerp arg)
              (if (>= arg 0)
                  (+ (* 10 arg) digit)
                (- (* 10 arg) digit)))
             ((eq arg '-)
              (if (zerop digit) '- (- digit)))
             ((consp arg)
              ;; (4) etc. — first digit replaces the universal-arg list.
              digit)
             (t digit))))))

(defun emacs-command-loop-negative-argument (arg)
  "Phase B.5: flip the sign of the pending prefix-arg."
  (interactive "P")
  (setq emacs-command-loop--prefix-arg
        (cond
         ((integerp arg) (- arg))
         ((eq arg '-) nil)
         (t '-))))

(defconst emacs-command-loop-gui-extended-command-candidate-names
  '(
    "find-file"
    "same-window-prefix"
    "other-window-prefix"
    "other-frame-prefix"
    "project-other-window-command"
    "project-other-frame-command"
    "project-other-tab-command"
    "find-file-other-window"
    "find-file-other-frame"
    "find-file-other-tab"
    "project-shell"
    "project-eshell"
    "project-or-external-find-file"
    "project-find-file"
    "project-find-dir"
    "project-dired"
    "project-any-command"
    "project-execute-extended-command"
    "project-switch-project"
    "find-file-read-only"
    "find-file-read-only-other-window"
    "find-file-read-only-other-frame"
    "find-file-read-only-other-tab"
    "find-alternate-file"
    "list-directory"
    "dired"
    "dired-jump"
    "dired-jump-other-window"
    "dired-other-window"
    "dired-other-frame"
    "dired-other-tab"
    "dired-mark"
    "dired-unmark"
    "dired-flag-file-deletion"
    "dired-do-flagged-delete"
    "dired-do-rename"
    "dired-do-copy"
    "org-todo"
    "org-narrow-to-subtree"
    "org-table-next-field"
    "org-capture"
    "org-agenda"
    "magit-status"
    "magit-stage-file"
    "magit-unstage-file"
    "magit-commit"
    "magit-diff"
    "magit-log"
    "Info-next"
    "Info-prev"
    "Info-up"
    "customize-variable"
    "customize-save-variable"
    "org-cycle"
    "org-shifttab"
    "org-table-align"
    "vc-root-diff"
    "vc-edit-next-command"
    "vc-next-action"
    "ispell-word"
    "eww-search-words"
    "compose-mail"
    "compose-mail-other-window"
    "compose-mail-other-frame"
    "calc-dispatch"
    "2C-command"
    "2C-two-columns"
    "2C-associate-buffer"
    "2C-split"
    "emoji-zoom-increase"
    "emoji-zoom-decrease"
    "emoji-zoom-reset"
    "emoji-describe"
    "emoji-insert"
    "emoji-list"
    "emoji-recent"
    "emoji-search"
    "add-change-log-entry-other-window"
    "insert-file"
    "insert-buffer"
    "save-buffer"
    "basic-save-buffer"
    "save-some-buffers"
    "revert-buffer"
    "revert-buffer-quick"
    "point-to-register"
    "jump-to-register"
    "frameset-to-register"
    "window-configuration-to-register"
    "copy-to-register"
    "insert-register"
    "number-to-register"
    "increment-register"
    "bookmark-set"
    "bookmark-set-no-overwrite"
    "bookmark-jump"
    "bookmark-bmenu-list"
    "copy-rectangle-to-register"
    "copy-rectangle-as-kill"
    "rectangle-number-lines"
    "kill-rectangle"
    "delete-rectangle"
    "clear-rectangle"
    "open-rectangle"
    "string-rectangle"
    "yank-rectangle"
    "write-file"
    "expand-abbrev"
    "add-global-abbrev"
    "add-mode-abbrev"
    "inverse-add-global-abbrev"
    "inverse-add-mode-abbrev"
    "abbrev-prefix-mark"
    "expand-jump-to-next-slot"
    "expand-jump-to-previous-slot"
    "switch-to-buffer"
    "switch-to-buffer-other-window"
    "switch-to-buffer-other-frame"
    "switch-to-buffer-other-tab"
    "project-switch-to-buffer"
    "display-buffer"
    "display-buffer-other-frame"
    "rename-buffer"
    "rename-uniquely"
    "clone-buffer"
    "clone-indirect-buffer-other-window"
    "kill-buffer"
    "kill-buffer-and-window"
    "list-buffers"
    "project-list-buffers"
    "project-kill-buffers"
    "project-any-command"
    "project-execute-extended-command"
    "project-other-window-command"
    "project-other-frame-command"
    "project-other-tab-command"
    "project-switch-project"
    "occur"
    "imenu"
    "keyboard-escape-quit"
    "exit-recursive-edit"
    "abort-recursive-edit"
    "goto-line"
    "goto-line-relative"
    "narrow-to-defun"
    "narrow-to-region"
    "narrow-to-page"
    "widen"
    "move-to-column"
    "eval-last-sexp"
    "shell-command"
    "project-shell-command"
    "project-async-shell-command"
    "project-compile"
    "project-or-external-find-regexp"
    "project-find-regexp"
    "project-query-replace-regexp"
    "project-vc-dir"
    "vc-diff"
    "vc-print-log"
    "eval-expression"
    "font-lock-update"
    "insert-char"
    "text-scale-adjust"
    "global-text-scale-adjust"
    "suspend-frame"
    "tmm-menubar"
    "set-selective-display"
    "toggle-input-method"
    "activate-transient-input-method"
    "set-input-method"
    "set-file-name-coding-system"
    "set-next-selection-coding-system"
    "universal-coding-system-argument"
    "set-buffer-file-coding-system"
    "set-keyboard-coding-system"
    "set-language-environment"
    "set-buffer-process-coding-system"
    "revert-buffer-with-coding-system"
    "set-terminal-coding-system"
    "set-selection-coding-system"
    "highlight-symbol-at-point"
    "highlight-regexp"
    "highlight-phrase"
    "highlight-lines-matching-regexp"
    "unhighlight-regexp"
    "hi-lock-find-patterns"
    "hi-lock-write-interactive-patterns"
    "kmacro-start-macro"
    "kmacro-end-macro"
    "kmacro-end-and-call-macro"
    "kbd-macro-query"
    "kmacro-keymap"
    "kmacro-delete-ring-head"
    "kmacro-edit-macro-repeat"
    "kmacro-set-format"
    "kmacro-end-or-call-macro-repeat"
    "kmacro-call-ring-2nd-repeat"
    "kmacro-cycle-ring-next"
    "kmacro-cycle-ring-previous"
    "kmacro-swap-ring"
    "kmacro-view-macro-repeat"
    "kmacro-edit-macro"
    "kmacro-step-edit-macro"
    "kmacro-bind-to-key"
    "kmacro-redisplay"
    "edit-kbd-macro"
    "kmacro-edit-lossage"
    "kmacro-name-last-macro"
    "apply-macro-to-region-lines"
    "kmacro-to-register"
    "replace-string"
    "replace-regexp"
    "query-replace"
    "query-replace-regexp"
    "goto-char"
    "describe-function"
    "describe-variable"
    "describe-key"
    "describe-key-briefly"
    "describe-bindings"
    "help-for-help"
    "describe-coding-system"
    "describe-input-method"
    "describe-language-environment"
    "apropos-command"
    "apropos-documentation"
    "view-echo-area-messages"
    "about-emacs"
    "describe-copying"
    "view-emacs-debugging"
    "view-external-packages"
    "view-emacs-FAQ"
    "view-emacs-news"
    "describe-distribution"
    "view-emacs-problems"
    "view-emacs-todo"
    "describe-no-warranty"
    "describe-gnu-project"
    "view-hello-file"
    "view-lossage"
    "describe-mode"
    "describe-symbol"
    "help-quit"
    "describe-syntax"
    "help-with-tutorial"
    "display-local-help"
    "help-find-source"
    "help-quick-toggle"
    "search-forward-help-for-help"
    "xref-go-back"
    "xref-go-forward"
    "xref-find-definitions"
    "xref-find-references"
    "xref-find-apropos"
    "xref-find-definitions-other-window"
    "xref-find-definitions-other-frame"
    "next-error"
    "previous-error"
    "repeat-complex-command"
    "info"
    "info-other-window"
    "info-emacs-manual"
    "info-display-manual"
    "view-order-manuals"
    "Info-goto-emacs-command-node"
    "Info-goto-emacs-key-command-node"
    "info-lookup-symbol"
    "describe-package"
    "finder-by-keyword"
    "where-is"
    "describe-command"
    "what-cursor-position"
    "universal-argument"
    "digit-argument"
    "negative-argument"
    "forward-char"
    "backward-char"
    "beginning-of-buffer"
    "end-of-buffer"
    "beginning-of-line"
    "back-to-indentation"
    "end-of-line"
    "next-line"
    "previous-line"
    "set-goal-column"
    "scroll-up-command"
    "scroll-down-command"
    "scroll-left"
    "scroll-right"
    "scroll-other-window"
    "scroll-other-window-down"
    "recenter-top-bottom"
    "move-to-window-line-top-bottom"
    "reposition-window"
    "recenter-other-window"
    "isearch-forward"
    "isearch-backward"
    "isearch-forward-regexp"
    "isearch-backward-regexp"
    "isearch-forward-symbol-at-point"
    "isearch-forward-thing-at-point"
    "isearch-forward-symbol"
    "isearch-forward-word"
    "indent-region"
    "delete-other-windows"
    "delete-window"
    "split-window-right"
    "split-window-below"
    "balance-windows"
    "shrink-window-if-larger-than-buffer"
    "fit-window-to-buffer"
    "delete-windows-on"
    "split-root-window-below"
    "split-root-window-right"
    "tear-off-window"
    "toggle-window-dedicated"
    "quit-window"
    "window-toggle-side-windows"
    "enlarge-window"
    "shrink-window-horizontally"
    "enlarge-window-horizontally"
    "other-window"
    "forward-word"
    "backward-word"
    "beginning-of-defun"
    "forward-sexp"
    "backward-sexp"
    "end-of-defun"
    "mark-defun"
    "mark-sexp"
    "kill-sexp"
    "down-list"
    "forward-list"
    "backward-list"
    "transpose-sexps"
    "backward-up-list"
    "kill-word"
    "backward-kill-word"
    "zap-to-char"
    "dabbrev-expand"
    "dabbrev-completion"
    "complete-symbol"
    "transpose-words"
    "insert-parentheses"
    "move-past-close-and-reindent"
    "transpose-lines"
    "mark-word"
    "count-words-region"
    "count-lines-page"
    "forward-paragraph"
    "backward-paragraph"
    "mark-paragraph"
    "fill-paragraph"
    "set-fill-column"
    "set-fill-prefix"
    "comment-set-column"
    "forward-sentence"
    "backward-sentence"
    "kill-sentence"
    "backward-kill-sentence"
    "transpose-chars"
    "delete-horizontal-space"
    "cycle-spacing"
    "not-modified"
    "just-one-space"
    "delete-indentation"
    "comment-line"
    "comment-dwim"
    "upcase-word"
    "downcase-word"
    "capitalize-word"
    "upcase-region"
    "downcase-region"
    "capitalize-region"
    "sort-lines"
    "delete-char"
    "delete-backward-char"
    "self-insert-command"
    "quoted-insert"
    "indent-for-tab-command"
    "tab-to-tab-stop"
    "newline"
    "electric-newline-and-maybe-indent"
    "default-indent-new-line"
    "open-line"
    "split-line"
    "delete-blank-lines"
    "kill-line"
    "kill-whole-line"
    "yank"
    "yank-pop"
    "set-mark-command"
    "exchange-point-and-mark"
    "pop-global-mark"
    "rectangle-mark-mode"
    "toggle-truncate-lines"
    "mark-whole-buffer"
    "mark-page"
    "backward-page"
    "forward-page"
    "indent-rigidly"
    "delete-region"
    "kill-region"
    "copy-region-as-kill"
    "kill-ring-save"
    "append-next-kill"
    "undo"
    "undo-redo"
    "delete-trailing-whitespace"
    "untabify"
    )
  "Curated GUI M-x command candidate names.
Standalone NeLisp cannot reliably enumerate `obarray' with
`mapatoms' / `commandp', so this seed preserves GUI bridge
completion coverage while command semantics move into runtime.")

;;;###autoload
(defun emacs-command-loop-gui-extended-command-candidates ()
  "Return newline-separated GUI M-x command candidates."
  (let ((out "")
        (names emacs-command-loop-gui-extended-command-candidate-names))
    (while names
      (setq out (concat out (car names) "\n"))
      (setq names (cdr names)))
    out))

(defconst emacs-command-loop-gui-command-registry-names
  '(
    "execute-extended-command"
    "execute-extended-command-for-buffer"
    "describe-function"
    "describe-variable"
    "describe-key"
    "describe-key-briefly"
    "describe-bindings"
    "help-for-help"
    "describe-coding-system"
    "describe-input-method"
    "describe-language-environment"
    "apropos-command"
    "apropos-documentation"
    "view-echo-area-messages"
    "scratch-buffer"
    "messages-buffer"
    "warnings-buffer"
    "about-emacs"
    "describe-copying"
    "view-emacs-debugging"
    "view-external-packages"
    "view-emacs-FAQ"
    "view-emacs-news"
    "describe-distribution"
    "view-emacs-problems"
    "view-emacs-todo"
    "describe-no-warranty"
    "describe-gnu-project"
    "view-hello-file"
    "view-lossage"
    "describe-mode"
    "describe-symbol"
    "help-quit"
    "describe-syntax"
    "help-with-tutorial"
    "display-local-help"
    "help-find-source"
    "help-quick-toggle"
    "search-forward-help-for-help"
    "eval-last-sexp"
    "eval-expression"
    "font-lock-update"
    "insert-char"
    "text-scale-adjust"
    "global-text-scale-adjust"
    "suspend-frame"
    "tmm-menubar"
    "set-selective-display"
    "toggle-input-method"
    "activate-transient-input-method"
    "set-input-method"
    "set-file-name-coding-system"
    "set-next-selection-coding-system"
    "universal-coding-system-argument"
    "set-buffer-file-coding-system"
    "set-keyboard-coding-system"
    "set-language-environment"
    "set-buffer-process-coding-system"
    "revert-buffer-with-coding-system"
    "set-terminal-coding-system"
    "set-selection-coding-system"
    "highlight-symbol-at-point"
    "highlight-regexp"
    "highlight-phrase"
    "highlight-lines-matching-regexp"
    "unhighlight-regexp"
    "hi-lock-find-patterns"
    "hi-lock-write-interactive-patterns"
    "emoji-zoom-increase"
    "emoji-zoom-decrease"
    "emoji-zoom-reset"
    "emoji-describe"
    "emoji-insert"
    "emoji-list"
    "emoji-recent"
    "emoji-search"
    "kmacro-start-macro"
    "kmacro-end-macro"
    "kmacro-end-and-call-macro"
    "kbd-macro-query"
    "kmacro-set-counter"
    "kmacro-add-counter"
    "kmacro-insert-counter"
    "kmacro-keymap"
    "kmacro-delete-ring-head"
    "kmacro-edit-macro-repeat"
    "kmacro-set-format"
    "kmacro-end-or-call-macro-repeat"
    "kmacro-call-ring-2nd-repeat"
    "kmacro-cycle-ring-next"
    "kmacro-cycle-ring-previous"
    "kmacro-swap-ring"
    "kmacro-view-macro-repeat"
    "kmacro-edit-macro"
    "kmacro-step-edit-macro"
    "kmacro-bind-to-key"
    "kmacro-redisplay"
    "edit-kbd-macro"
    "kmacro-edit-lossage"
    "kmacro-name-last-macro"
    "apply-macro-to-region-lines"
    "kmacro-to-register"
    "xref-go-back"
    "xref-go-forward"
    "xref-find-definitions"
    "xref-find-references"
    "xref-find-apropos"
    "xref-find-definitions-other-window"
    "xref-find-definitions-other-frame"
    "next-error"
    "previous-error"
    "repeat-complex-command"
    "info"
    "info-other-window"
    "info-emacs-manual"
    "info-display-manual"
    "view-order-manuals"
    "Info-goto-emacs-command-node"
    "Info-goto-emacs-key-command-node"
    "info-lookup-symbol"
    "describe-package"
    "finder-by-keyword"
    "where-is"
    "describe-command"
    "what-cursor-position"
    "shell-command"
    "shell-command-on-region"
    "async-shell-command"
    "project-shell-command"
    "project-async-shell-command"
    "project-shell"
    "project-eshell"
    "project-compile"
    "project-find-regexp"
    "project-or-external-find-regexp"
    "project-vc-dir"
    "vc-diff"
    "vc-print-log"
    "repeat"
    "universal-argument"
    "digit-argument"
    "negative-argument"
    "find-file"
    "same-window-prefix"
    "other-window-prefix"
    "other-tab-prefix"
    "other-frame-prefix"
    "find-file-other-window"
    "find-file-other-frame"
    "find-file-other-tab"
    "project-or-external-find-file"
    "project-find-file"
    "project-find-dir"
    "project-dired"
    "project-any-command"
    "project-execute-extended-command"
    "project-other-window-command"
    "project-other-tab-command"
    "project-other-frame-command"
    "project-switch-project"
    "add-change-log-entry-other-window"
    "find-file-read-only"
    "find-file-read-only-other-window"
    "find-file-read-only-other-frame"
    "find-file-read-only-other-tab"
    "toggle-read-only"
    "read-only-mode"
    "find-alternate-file"
    "list-directory"
    "dired"
    "dired-jump"
    "dired-jump-other-window"
    "dired-other-window"
    "dired-other-frame"
    "dired-other-tab"
    "compose-mail"
    "compose-mail-other-window"
    "compose-mail-other-frame"
    "calc-dispatch"
    "2C-command"
    "2C-two-columns"
    "2C-associate-buffer"
    "2C-split"
    "insert-file"
    "insert-buffer"
    "point-to-register"
    "jump-to-register"
    "frameset-to-register"
    "window-configuration-to-register"
    "copy-to-register"
    "insert-register"
    "number-to-register"
    "increment-register"
    "bookmark-set"
    "bookmark-set-no-overwrite"
    "bookmark-jump"
    "bookmark-bmenu-list"
    "write-file"
    "save-buffer"
    "basic-save-buffer"
    "save-some-buffers"
    "revert-buffer"
    "revert-buffer-quick"
    "forward-char"
    "backward-char"
    "beginning-of-buffer"
    "end-of-buffer"
    "beginning-of-line"
    "back-to-indentation"
    "end-of-line"
    "move-beginning-of-line"
    "move-end-of-line"
    "goto-line"
    "goto-line-relative"
    "goto-char"
    "move-to-column"
    "narrow-to-defun"
    "narrow-to-region"
    "narrow-to-page"
    "widen"
    "next-line"
    "previous-line"
    "set-goal-column"
    "scroll-up-command"
    "scroll-down-command"
    "scroll-left"
    "scroll-right"
    "tab-new"
    "tab-new-to"
    "tab-group"
    "tab-undo"
    "tab-move"
    "tab-move-to"
    "tab-close"
    "tab-close-other"
    "tab-detach"
    "tab-window-detach"
    "delete-frame"
    "delete-other-frames"
    "make-frame-command"
    "other-frame"
    "clone-frame"
    "undelete-frame"
    "tab-next"
    "tab-previous"
    "tab-duplicate"
    "tab-switch"
    "tab-rename"
    "scroll-other-window"
    "scroll-other-window-down"
    "recenter-top-bottom"
    "move-to-window-line-top-bottom"
    "reposition-window"
    "recenter-other-window"
    "isearch-forward"
    "isearch-backward"
    "isearch-forward-regexp"
    "isearch-backward-regexp"
    "isearch-forward-symbol-at-point"
    "isearch-forward-thing-at-point"
    "isearch-forward-symbol"
    "isearch-forward-word"
    "replace-string"
    "replace-regexp"
    "query-replace"
    "query-replace-regexp"
    "project-query-replace-regexp"
    "switch-to-buffer"
    "switch-to-buffer-other-window"
    "switch-to-buffer-other-frame"
    "switch-to-buffer-other-tab"
    "project-switch-to-buffer"
    "display-buffer"
    "display-buffer-other-frame"
    "rename-buffer"
    "rename-uniquely"
    "clone-buffer"
    "clone-indirect-buffer-other-window"
    "kill-buffer"
    "kill-buffer-and-window"
    "project-kill-buffers"
    "list-buffers"
    "project-list-buffers"
    "occur"
    "imenu"
    "save-buffers-kill-terminal"
    "save-buffers-kill-emacs"
    "kill-emacs"
    "keyboard-quit"
    "keyboard-escape-quit"
    "exit-recursive-edit"
    "abort-recursive-edit"
    "delete-other-windows"
    "delete-window"
    "split-window-right"
    "split-window-below"
    "balance-windows"
    "shrink-window-if-larger-than-buffer"
    "fit-window-to-buffer"
    "delete-windows-on"
    "split-root-window-below"
    "split-root-window-right"
    "tear-off-window"
    "toggle-window-dedicated"
    "quit-window"
    "dired-mark"
    "dired-unmark"
    "dired-flag-file-deletion"
    "dired-do-flagged-delete"
    "dired-do-rename"
    "dired-do-copy"
    "org-todo"
    "org-narrow-to-subtree"
    "org-table-next-field"
    "org-capture"
    "org-agenda"
    "magit-status"
    "magit-stage-file"
    "magit-unstage-file"
    "magit-commit"
    "magit-diff"
    "magit-log"
    "Info-next"
    "Info-prev"
    "Info-up"
    "customize-variable"
    "customize-save-variable"
    "org-cycle"
    "org-shifttab"
    "org-table-align"
    "org-metaright"
    "org-metaleft"
    "vc-root-diff"
    "vc-edit-next-command"
    "vc-next-action"
    "ispell-word"
    "eww-search-words"
    "window-toggle-side-windows"
    "enlarge-window"
    "shrink-window-horizontally"
    "enlarge-window-horizontally"
    "other-window"
    "forward-word"
    "backward-word"
    "beginning-of-defun"
    "forward-sexp"
    "backward-sexp"
    "end-of-defun"
    "mark-defun"
    "mark-sexp"
    "kill-sexp"
    "down-list"
    "forward-list"
    "backward-list"
    "transpose-sexps"
    "backward-up-list"
    "kill-word"
    "backward-kill-word"
    "zap-to-char"
    "expand-abbrev"
    "add-global-abbrev"
    "add-mode-abbrev"
    "inverse-add-global-abbrev"
    "inverse-add-mode-abbrev"
    "abbrev-prefix-mark"
    "expand-jump-to-next-slot"
    "expand-jump-to-previous-slot"
    "dabbrev-expand"
    "dabbrev-completion"
    "complete-symbol"
    "transpose-words"
    "insert-parentheses"
    "move-past-close-and-reindent"
    "transpose-lines"
    "mark-word"
    "count-words-region"
    "count-lines-page"
    "forward-paragraph"
    "backward-paragraph"
    "mark-paragraph"
    "fill-paragraph"
    "set-fill-column"
    "set-fill-prefix"
    "comment-set-column"
    "forward-sentence"
    "backward-sentence"
    "kill-sentence"
    "backward-kill-sentence"
    "transpose-chars"
    "delete-horizontal-space"
    "cycle-spacing"
    "not-modified"
    "just-one-space"
    "delete-indentation"
    "comment-line"
    "comment-dwim"
    "upcase-word"
    "downcase-word"
    "capitalize-word"
    "upcase-region"
    "downcase-region"
    "capitalize-region"
    "sort-lines"
    "delete-char"
    "backward-delete-char"
    "delete-backward-char"
    "self-insert-command"
    "quoted-insert"
    "indent-for-tab-command"
    "tab-to-tab-stop"
    "indent-region"
    "indent-rigidly"
    "newline"
    "electric-newline-and-maybe-indent"
    "default-indent-new-line"
    "open-line"
    "split-line"
    "delete-blank-lines"
    "kill-line"
    "kill-whole-line"
    "yank"
    "yank-pop"
    "set-mark-command"
    "exchange-point-and-mark"
    "pop-global-mark"
    "rectangle-mark-mode"
    "toggle-truncate-lines"
    "mark-whole-buffer"
    "mark-page"
    "backward-page"
    "forward-page"
    "delete-region"
    "kill-region"
    "copy-region-as-kill"
    "kill-ring-save"
    "copy-rectangle-to-register"
    "copy-rectangle-as-kill"
    "rectangle-number-lines"
    "kill-rectangle"
    "delete-rectangle"
    "clear-rectangle"
    "open-rectangle"
    "string-rectangle"
    "yank-rectangle"
    "append-next-kill"
    "undo"
    "undo-redo"
    "delete-trailing-whitespace"
    "untabify"
    ;; Toolbar menu-open no-op: the GUI bridge assigns `ignore' to
    ;; `files--bridge-command' when a click only expands a submenu (no
    ;; command should run).  Registered here so the runtime registry is
    ;; congruent with the bridge's command set and the bridge fallback
    ;; can later be dropped in favour of the runtime recognition path.
    "ignore"
    )
  "Curated GUI bridge command names accepted by commandp.
This preserves bridge command coverage while the command-loop
runtime becomes the owner of command recognition policy.")

;;;###autoload
(defun emacs-command-loop-gui-command-registered-p (&optional command)
  "Return non-nil when COMMAND is a known GUI bridge command.
When COMMAND is nil, use `emacs-command-loop-gui-command'."
  (let* ((command (or command emacs-command-loop-gui-command))
         (name (cond
                ((symbolp command) (symbol-name command))
                ((stringp command) command)
                (t nil))))
    (and name
         (member name emacs-command-loop-gui-command-registry-names)
         t)))

;;;###autoload
(defun emacs-command-loop-gui-command-accepted-p (&optional command)
  "Return non-nil when COMMAND can be dispatched by GUI command execution.
This is the command recognition policy shared by the GUI bridge adapter
and the runtime command loop.  It accepts the curated bridge registry and
runtime-loaded function symbols, without consulting the GUI backend to
avoid recursive `commandp' callbacks."
  (let ((command (or command emacs-command-loop-gui-command)))
    (or (emacs-command-loop-gui-command-registered-p command)
        (and (symbolp command) (fboundp command))
        (functionp command))))


(defconst emacs-command-loop-gui-read-only-command-names
  '(
    "insert-file"
    "insert-buffer"
    "insert-register"
    "number-to-register"
    "increment-register"
    "bookmark-set"
    "bookmark-set-no-overwrite"
    "bookmark-jump"
    "bookmark-bmenu-list"
    "emoji-list"
    "emoji-recent"
    "emoji-search"
    "emoji-describe"
    "emoji-zoom-increase"
    "emoji-zoom-decrease"
    "emoji-zoom-reset"
    "text-scale-adjust"
    "global-text-scale-adjust"
    "suspend-frame"
    "tmm-menubar"
    "repeat"
    "write-file"
    "save-buffer"
    "basic-save-buffer"
    "kill-word"
    "kill-sexp"
    "backward-kill-word"
    "zap-to-char"
    "dabbrev-expand"
    "dabbrev-completion"
    "complete-symbol"
    "transpose-words"
    "transpose-sexps"
    "insert-parentheses"
    "move-past-close-and-reindent"
    "transpose-lines"
    "fill-paragraph"
    "kill-sentence"
    "backward-kill-sentence"
    "transpose-chars"
    "delete-horizontal-space"
    "cycle-spacing"
    "just-one-space"
    "delete-indentation"
    "comment-line"
    "comment-dwim"
    "upcase-word"
    "downcase-word"
    "capitalize-word"
    "upcase-region"
    "downcase-region"
    "capitalize-region"
    "sort-lines"
    "delete-char"
    "backward-delete-char"
    "delete-backward-char"
    "self-insert-command"
    "quoted-insert"
    "indent-for-tab-command"
    "tab-to-tab-stop"
    "indent-region"
    "indent-rigidly"
    "newline"
    "electric-newline-and-maybe-indent"
    "default-indent-new-line"
    "open-line"
    "split-line"
    "delete-blank-lines"
    "kill-line"
    "kill-whole-line"
    "yank"
    "yank-pop"
    "delete-region"
    "kill-region"
    "kill-rectangle"
    "rectangle-number-lines"
    "delete-rectangle"
    "clear-rectangle"
    "open-rectangle"
    "string-rectangle"
    "yank-rectangle"
    "replace-string"
    "replace-regexp"
    "query-replace"
    "query-replace-regexp"
    "project-query-replace-regexp"
    "undo"
    "undo-redo"
    "delete-trailing-whitespace"
    "untabify"
    )
  "GUI bridge commands rejected in read-only buffers.")

;;;###autoload
(defun emacs-command-loop-gui-read-only-command-p (&optional command)
  "Return non-nil when COMMAND is in the GUI bridge registry.
When COMMAND is nil, use `emacs-command-loop-gui-command'."
  (let* ((command (or command emacs-command-loop-gui-command))
         (name (cond
                ((symbolp command) (symbol-name command))
                ((stringp command) command)
                (t nil))))
    (and name
         (member name emacs-command-loop-gui-read-only-command-names)
         t)))

(defconst emacs-command-loop-gui-prefix-command-names
  '(
    "universal-argument"
    "digit-argument"
    "negative-argument"
    )
  "GUI bridge prefix-argument commands.")

;;;###autoload
(defun emacs-command-loop-gui-prefix-command-p (&optional command)
  "Return non-nil when COMMAND is in the GUI bridge registry.
When COMMAND is nil, use `emacs-command-loop-gui-command'."
  (let* ((command (or command emacs-command-loop-gui-command))
         (name (cond
                ((symbolp command) (symbol-name command))
                ((stringp command) command)
                (t nil))))
    (and name
         (member name emacs-command-loop-gui-prefix-command-names)
         t)))

(defconst emacs-command-loop-gui-prefix-repeat-command-names
  '("forward-char"
    "backward-char"
    "next-line"
    "previous-line"
    "delete-char"
    "backward-delete-char"
    "delete-backward-char"
    "self-insert-command")
  "GUI bridge commands repeated COUNT times for numeric prefix args.")

;;;###autoload
(defun emacs-command-loop-gui-prefix-repeat-command-p (&optional command)
  "Return non-nil when COMMAND should repeat under a numeric prefix arg.
When COMMAND is nil, use `emacs-command-loop-gui-command'."
  (let* ((command (or command emacs-command-loop-gui-command))
         (name (cond
                ((symbolp command) (symbol-name command))
                ((stringp command) command)
                (t nil))))
    (and name
         (member name emacs-command-loop-gui-prefix-repeat-command-names)
         t)))

(defconst emacs-command-loop-gui-prefix-invert-command-alist
  '((forward-char . backward-char)
    (backward-char . forward-char)
    (next-line . previous-line)
    (previous-line . next-line)
    (delete-char . delete-backward-char)
    (delete-backward-char . delete-char)
    (backward-delete-char . delete-char))
  "GUI bridge command mapping used for negative numeric prefix args.")

;;;###autoload
(defun emacs-command-loop-gui-prefix-inverted-command (&optional command)
  "Return COMMAND's negative-prefix inverse, or nil.
When COMMAND is nil, use `emacs-command-loop-gui-command'.  String
commands are accepted and interned for lookup."
  (let ((command (or command emacs-command-loop-gui-command)))
    (cdr (assq (cond
                ((symbolp command) command)
                ((stringp command) (intern command))
                (t nil))
               emacs-command-loop-gui-prefix-invert-command-alist))))

;;;###autoload
(defun emacs-command-loop-gui-prefix-arg-number (&optional text)
  "Return numeric value of GUI bridge textual prefix arg TEXT.
TEXT defaults to `emacs-command-loop-gui-prefix-arg'.  Empty text is 1;
a bare minus sign is -1."
  (let* ((text (or text emacs-command-loop-gui-prefix-arg ""))
         (index 0)
         (value 0)
         (negative nil)
         (seen-digit nil))
    (when (and (> (length text) 0)
               (= (aref text 0) ?-))
      (setq negative t)
      (setq index 1))
    (while (< index (length text))
      (let ((ch (aref text index)))
        (when (and (>= ch ?0) (<= ch ?9))
          (setq seen-digit t)
          (setq value (+ (* value 10) (- ch ?0)))))
      (setq index (1+ index)))
    (cond
     (seen-digit (if negative (- value) value))
     (negative -1)
     (t 1))))

;;;###autoload
(defun emacs-command-loop-gui-prefix-arg-absolute-number (&optional text)
  "Return absolute numeric value of GUI bridge textual prefix arg TEXT."
  (let ((value (emacs-command-loop-gui-prefix-arg-number text)))
    (if (< value 0) (- value) value)))

;;;###autoload
(defun emacs-command-loop-gui-prefix-number-string (value)
  "Return decimal string representation of prefix VALUE."
  (number-to-string value))

;;;###autoload
(defun emacs-command-loop-gui-prefix-digit-key (&optional arg keys)
  "Return prefix digit from ARG or KEYS, or the empty string.
ARG defaults to `emacs-command-loop-gui-arg'.  KEYS defaults to
`emacs-command-loop-gui-keys'.  The bridge command path stores the
typed digit either as an argument char or as the final key name char."
  (let ((arg (or arg emacs-command-loop-gui-arg ""))
        (keys (or keys emacs-command-loop-gui-keys ""))
        (digit ""))
    (when (> (length arg) 0)
      (let ((ch (aref arg 0)))
        (when (and (>= ch ?0) (<= ch ?9))
          (setq digit (substring arg 0 1)))))
    (when (and (equal digit "")
               (> (length keys) 0))
      (let ((ch (aref keys (1- (length keys)))))
        (when (and (>= ch ?0) (<= ch ?9))
          (setq digit (substring keys (1- (length keys)))))))
    digit))

;;;###autoload
(defun emacs-command-loop-gui-digit-argument ()
  "Append the active GUI digit-argument key to textual prefix state."
  (let ((digit (emacs-command-loop-gui-prefix-digit-key)))
    (unless (equal digit "")
      (emacs-command-loop-gui--set-prefix-arg
       (concat emacs-command-loop-gui-prefix-arg digit)))
    (emacs-command-loop-gui--set-status "prefix-arg")
    emacs-command-loop-gui-prefix-arg))

;;;###autoload
(defun emacs-command-loop-gui-negative-argument ()
  "Toggle a leading minus sign in GUI textual prefix state."
  (if (and (> (length emacs-command-loop-gui-prefix-arg) 0)
           (= (aref emacs-command-loop-gui-prefix-arg 0) ?-))
      (emacs-command-loop-gui--set-prefix-arg
       (substring emacs-command-loop-gui-prefix-arg 1))
    (emacs-command-loop-gui--set-prefix-arg
     (concat "-" emacs-command-loop-gui-prefix-arg)))
  (emacs-command-loop-gui--set-status "prefix-arg")
  emacs-command-loop-gui-prefix-arg)

;;;###autoload
(defun emacs-command-loop-gui-universal-argument ()
  "Update GUI textual prefix state for `universal-argument'."
  (if (equal emacs-command-loop-gui-prefix-arg "")
      (emacs-command-loop-gui--set-prefix-arg "4")
    (emacs-command-loop-gui--set-prefix-arg
     (emacs-command-loop-gui-prefix-number-string
      (* (emacs-command-loop-gui-prefix-arg-number) 4))))
  (emacs-command-loop-gui--set-status "prefix-arg")
  emacs-command-loop-gui-prefix-arg)

;;;###autoload
(defun emacs-command-loop-gui-invert-prefix-command-if-needed ()
  "Invert the active GUI command when the textual prefix arg is negative.
Return non-nil when the command was changed."
  (when (< (emacs-command-loop-gui-prefix-arg-number) 0)
    (let ((inverted
           (emacs-command-loop-gui-prefix-inverted-command
            emacs-command-loop-gui-command)))
      (when inverted
        (emacs-command-loop-gui--set-command inverted)
        (emacs-command-loop-gui--set-effective-command
         (symbol-name inverted))
        t))))

;;;###autoload
(defun emacs-command-loop-gui-execute-with-prefix-arg ()
  "Execute the active GUI command using textual prefix-arg semantics.
Numeric prefix args repeat simple motion/editing commands; negative
numeric prefix args first invert commands listed in
`emacs-command-loop-gui-prefix-invert-command-alist'."
  (let ((count (emacs-command-loop-gui-prefix-arg-absolute-number)))
    (emacs-command-loop-gui-invert-prefix-command-if-needed)
    (if (emacs-command-loop-gui-prefix-repeat-command-p
         emacs-command-loop-gui-command)
        (while (> count 0)
          (emacs-command-loop-gui-call-interactively
           emacs-command-loop-gui-command)
          (setq count (1- count)))
      (emacs-command-loop-gui-call-interactively
       emacs-command-loop-gui-command))
    (emacs-command-loop-gui--set-prefix-arg "")
    nil))

(defconst emacs-command-loop-gui-adapted-command-alist
  '((goto-char . goto-char)
    (zap-to-char . zap-to-char))
  "GUI bridge commands that require a transport-aware call adapter.
The car is the Emacs command, the cdr is the adapter kind supplied by
the GUI bridge backend.")

;;;###autoload
(defun emacs-command-loop-gui-command-adapter-kind (&optional command)
  "Return the GUI call adapter kind for COMMAND, or nil.
When COMMAND is nil, use `emacs-command-loop-gui-command'."
  (let ((command (or command emacs-command-loop-gui-command)))
    (cdr (assq (cond
                ((symbolp command) command)
                ((stringp command) (intern command))
                (t nil))
               emacs-command-loop-gui-adapted-command-alist))))

(defun emacs-command-loop-gui--call-adapted-command (command)
  "Call COMMAND through a registered GUI adapter, if one is needed."
  (let ((kind (emacs-command-loop-gui-command-adapter-kind command)))
    (when kind
      (emacs-command-loop-gui--backend-call :call-adapted-command
                                            command kind))))

;;;; --- M-x (execute-extended-command, Phase B.5) ----------------------

(defun emacs-command-loop-execute-extended-command
    (prefix-arg-incoming &optional command-name _typed)
  "Phase B.5 MVP: read a command name and dispatch it.

PREFIX-ARG-INCOMING (= the M-x command's own prefix arg) is
restored into `emacs-command-loop--prefix-arg' before the inner
`call-interactively' so the dispatched command sees it.
COMMAND-NAME may be supplied directly (= for tests / scripted
calls) to skip the minibuffer reader."
  (interactive "P")
  (let* ((cmd-name
          (or command-name
              (cond
               ((fboundp 'emacs-minibuffer-completing-read)
                (emacs-minibuffer-completing-read
                 "M-x " obarray 'commandp t nil
                 'extended-command-history))
               ((fboundp 'completing-read)
                (completing-read "M-x " obarray 'commandp t nil
                                 'extended-command-history))
               (t (signal 'emacs-command-loop-error
                          (list 'no-completing-read))))))
         (cmd (cond
               ((symbolp cmd-name) cmd-name)
               ((stringp cmd-name) (intern cmd-name))
               (t (signal 'wrong-type-argument
                          (list 'string-or-symbol cmd-name))))))
    (let ((emacs-command-loop--prefix-arg prefix-arg-incoming))
      (emacs-command-loop-call-interactively cmd))))

(provide 'emacs-command-loop)

;;; emacs-command-loop.el ends here
