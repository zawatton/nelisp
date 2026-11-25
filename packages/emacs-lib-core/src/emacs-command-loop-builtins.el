;;; emacs-command-loop-builtins.el --- Unprefixed command-loop bridges  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Doc 51 Track B Phase B.1 (2026-05-03) — Layer 2.
;;
;; Bridges the Emacs C-core *unprefixed* command-loop names to the
;; substrate in `emacs-command-loop.el', mirroring the bridge pattern
;; used by Track C / D / E / J / L1 / 11.C''.
;;
;; Function definitions use a host-aware install gate: host Emacs keeps
;; its C builtins, while standalone NeLisp overwrites any bootstrap
;; stubs with the real substrate functions.  Variables are still gated
;; on `unless (boundp ...)' so host-owned special variables win.
;;
;; Bridged today (B.1 = foundation only):
;;
;;   - Functions: read-event / read-char / read-command /
;;     this-command-keys / this-command-keys-vector /
;;     this-single-command-keys / this-single-command-raw-keys /
;;     clear-this-command-keys / recent-keys
;;
;;   - Variables: this-command / last-command / real-this-command /
;;     last-command-event / last-input-event / last-nonmenu-event /
;;     unread-command-events / quit-flag / inhibit-quit /
;;     throw-on-input
;;
;; B.2 (2026-05-03) added: read-key-sequence / read-key-sequence-vector.
;; B.3 (2026-05-03) added: call-interactively / funcall-interactively /
;;                          command-execute / prefix-arg / current-prefix-arg.
;; B.4 (2026-05-03) added: command-loop-1 / top-level / recursive-edit /
;;                          recursion-depth / pre-command-hook /
;;                          post-command-hook.
;; B.5 (2026-05-03) added: execute-extended-command / universal-argument /
;;                          digit-argument / negative-argument.
;; B.6 (2026-05-03) added: keyboard-quit / exit-recursive-edit + real
;;                          recursive-edit / top-level / recursion-depth.
;;                          `command-loop-1' wraps each step in a
;;                          quit-catch so C-g aborts the command,
;;                          not the loop.
;;
;; Deferred to subsequent phases:
;;   B.4: command-loop-1 / top-level
;;   B.5: execute-extended-command / prefix-arg / current-prefix-arg
;;   B.6: keyboard-quit / recursive-edit / abort-recursive-edit

;; Shim audit 2026-09-29: intentionally shadows native NeLisp definitions -- quit/sigint state belongs to the nemacs command loop.
;;; Code:

(require 'emacs-command-loop)

;;;; --- function bridges ----------------------------------------------

(defun emacs-command-loop-builtins--install-function-p (symbol)
  "Return non-nil when SYMBOL should be installed as an unprefixed bridge."
  (or (and (memq symbol '(sit-for sleep-for input-pending-p kill-emacs))
           (fboundp 'nelisp--write-stdout-bytes))
      (not (boundp 'emacs-version))
      (get symbol 'emacs-stub-bulk)
      (not (fboundp symbol))))

(defun emacs-command-loop-kill-emacs (&optional exit-code)
  "Restore terminal input and terminate standalone NeLisp.

Host Emacs keeps its native `kill-emacs'.  This helper is installed as
`kill-emacs' in standalone, or when the name is absent or a bulk stub."
  (let ((code (if (integerp exit-code) exit-code 0)))
    (unless (and (integerp code) (<= 0 code) (<= code 255))
      (setq code 1))
    (unwind-protect
        (when (bound-and-true-p kill-emacs-hook)
          (run-hook-wrapped
           'kill-emacs-hook
           (lambda (function)
             ;; Like GNU, report a failing hook and finish the remaining hooks.
             (condition-case err
                 (funcall function)
               ((error quit)
                (message "Error in kill-emacs-hook (%S): %S" function err)))
             nil)))
      (when (fboundp 'terminal-raw-mode-leave) (terminal-raw-mode-leave)))
    (if (fboundp 'nelisp--exit-process)
        (nelisp--exit-process code)
      (exit code))))

;; The headless prelude provides these names but cannot see frontend input
;; or restore the library's raw-mode state before its immediate process exit.
;; These are deliberate standalone compatibility shims; host Emacs wins.
(when (emacs-command-loop-builtins--install-function-p 'sit-for)
  (defalias 'sit-for #'emacs-command-loop-sit-for))

(when (emacs-command-loop-builtins--install-function-p 'sleep-for)
  (defalias 'sleep-for #'emacs-command-loop-sleep-for))

(when (emacs-command-loop-builtins--install-function-p 'input-pending-p)
  (defalias 'input-pending-p #'emacs-command-loop-input-pending-p))

(defun emacs-command-loop-builtins--read-input (reader prompt inherit seconds)
  "Call READER, returning nil on timeout while preserving its no-input signal."
  (condition-case err
      (funcall reader prompt inherit seconds)
    (emacs-command-loop-no-input
     (unless seconds (signal (car err) (cdr err))))))

(when (emacs-command-loop-builtins--install-function-p 'read-event)
  (defun read-event (&optional prompt inherit-input-method seconds)
    "Read an event, returning nil when SECONDS expires."
    (emacs-command-loop-builtins--read-input
     #'emacs-command-loop-read-event prompt inherit-input-method seconds)))

(when (emacs-command-loop-builtins--install-function-p 'read-char)
  (defun read-char (&optional prompt inherit-input-method seconds)
    "Read a character, returning nil when SECONDS expires."
    (emacs-command-loop-builtins--read-input
     #'emacs-command-loop-read-char prompt inherit-input-method seconds)))

(when (emacs-command-loop-builtins--install-function-p 'read-command)
  (defalias 'read-command #'emacs-command-loop-read-command))

(when (emacs-command-loop-builtins--install-function-p 'this-command-keys)
  (defalias 'this-command-keys #'emacs-command-loop-this-command-keys))

(when (emacs-command-loop-builtins--install-function-p 'this-command-keys-vector)
  (defalias 'this-command-keys-vector
    #'emacs-command-loop-this-command-keys-vector))

(when (emacs-command-loop-builtins--install-function-p 'this-single-command-keys)
  (defun this-single-command-keys ()
    "Return the last command's key sequence as a vector of events."
    (emacs-command-loop-this-command-keys-vector)))

(when (emacs-command-loop-builtins--install-function-p 'this-single-command-raw-keys)
  (defalias 'this-single-command-raw-keys
    #'emacs-command-loop-this-command-keys-vector))

(when (emacs-command-loop-builtins--install-function-p 'clear-this-command-keys)
  (defalias 'clear-this-command-keys
    #'emacs-command-loop-clear-this-command-keys))

(when (emacs-command-loop-builtins--install-function-p 'recent-keys)
  (defalias 'recent-keys #'emacs-command-loop-recent-keys))

(when (emacs-command-loop-builtins--install-function-p 'read-key-sequence)
  (defun read-key-sequence (prompt &optional continue-echo dont-downcase-last
                                  can-return-switch-frame cmd-loop
                                  disable-text-conversion)
    "Read a complete key sequence, displaying PROMPT when non-nil."
    (unless (or (null prompt) (stringp prompt))
      (signal 'wrong-type-argument (list 'stringp prompt)))
    (emacs-command-loop-read-key-sequence
     prompt continue-echo dont-downcase-last can-return-switch-frame cmd-loop)))

(when (emacs-command-loop-builtins--install-function-p 'read-key-sequence-vector)
  (defun read-key-sequence-vector (prompt &optional continue-echo
                                         dont-downcase-last
                                         can-return-switch-frame cmd-loop
                                         disable-text-conversion)
    "Read a complete key sequence and return it as a vector of events."
    (unless (or (null prompt) (stringp prompt))
      (signal 'wrong-type-argument (list 'stringp prompt)))
    (emacs-command-loop-read-key-sequence-vector
     prompt continue-echo dont-downcase-last can-return-switch-frame cmd-loop)))

;; The fixed reader does not expose GNU 31 nadvice oclosure metadata.
;; Keep its ordinary recognition, adding the shared advice composition path.
(defvar emacs-command-loop-builtins--native-commandp nil)
(when (fboundp 'nelisp--write-stdout-bytes)
  (unless emacs-command-loop--native-interactive-form
    (setq emacs-command-loop--native-interactive-form
          (symbol-function 'interactive-form)))
  (unless emacs-command-loop-builtins--native-commandp
    (setq emacs-command-loop-builtins--native-commandp (symbol-function 'commandp)))
  (defalias 'interactive-form #'emacs-command-loop--interactive-form)
  (defun commandp (object &optional for-call-interactively)
    "Recognize commands, including GNU nadvice's composed interactive spec."
    (or (funcall emacs-command-loop-builtins--native-commandp
                 object for-call-interactively)
        (emacs-command-loop--commandp object))))

;; GNU defines these commands in C with an `interactive' spec.  The native
;; and polyfilled definitions here carry no spec, so `commandp' and
;; `call-interactively' rejected them.  A symbol's `interactive-form'
;; property is what `interactive-form' consults for a function without an
;; inline spec.  Table generated from GNU Emacs 31.1:
;;   (dolist (n NAMES) (cadr (interactive-form n)))  ; C primitives only
;; A name that already has an `interactive-form' property is left alone.
(defconst emacs-command-loop-builtins--c-command-specs
  '(
    (abort-minibuffers "")
    (abort-recursive-edit "")
    (add-name-to-file "fAdd name to file: \nGName to add to %s: \np")
    (backward-char "^p")
    (base64-decode-region "r")
    (base64-encode-region "r")
    (base64url-encode-region "r")
    (buffer-enable-undo "")
    (call-last-kbd-macro "p")
    (capitalize-region (list (region-beginning) (region-end) (region-noncontiguous-p)))
    (chmod (let ((file (read-file-name "File: "))) (list file (read-file-modes nil file))))
    (copy-file "fCopy file: \nGCopy %s to file: \np\nP")
    (decode-coding-region "r\nzCoding system: ")
    (defining-kbd-macro "P")
    (delete-char "p\nP")
    (delete-region "r")
    (delete-frame "")
    (delete-other-windows-internal "")
    (delete-process (list 'message))
    (do-auto-save "")
    (downcase-region (list (region-beginning) (region-end) (region-noncontiguous-p)))
    (encode-coding-region "r\nzCoding system: ")
    (end-kbd-macro "p")
    (erase-buffer "*")
    (eval-buffer "")
    (eval-region "r")
    (forward-line "^p")
    (forward-word "^p")
    (garbage-collect "")
    (goto-char (goto-char--read-natnum-interactive "Go to char: "))
    (handle-switch-frame "^e")
    (iconify-frame "")
    (indent-to "NIndent to column: ")
    (indent-to-column "NIndent to column: ")
    (insert-char (list (read-char-by-name "Insert character (Unicode name or hex): ") (prefix-numeric-value current-prefix-arg) t))
    (kill-emacs "P")
    (kill-local-variable "vKill Local Variable: ")
    (kill-process (list (read-process-name "Kill process")))
    (lossage-size (list (read-number "Set maximum keystrokes to: " (lossage-size))))
    (lower-frame "")
    (make-frame-invisible "")
    (make-frame-visible "")
    (make-indirect-buffer "bMake indirect buffer (to buffer): \nBName of indirect buffer: ")
    (make-local-variable "vMake Local Variable: ")
    (make-symbolic-link "FMake symbolic link to file: \nGMake symbolic link to file %s: \np")
    (make-variable-buffer-local "vMake Variable Buffer Local: ")
    (malloc-info "")
    (malloc-trim "")
    (modify-syntax-entry "cSet syntax for character: \nsSet syntax for %s to: ")
    (move-to-column "NMove to column: ")
    (move-to-window-line "P")
    (narrow-to-region "r")
    (open-dribble-file "FOpen dribble file: ")
    (open-termscript "FOpen termscript file: ")
    (posix-search-backward "sPosix search backward: ")
    (posix-search-forward "sPosix search: ")
    (raise-frame "")
    (re-search-backward "sRE search backward: ")
    (re-search-forward "sRE search: ")
    (redirect-debugging-output "FDebug output file: \nP")
    (redraw-display "")
    (rename-file "fRename file: \nGRename %s to file: \np")
    (replace-region-contents (list (if (use-region-p) (region-beginning) (point-min)) (if (use-region-p) (region-end) (point-max)) (get-buffer (read-buffer-to-switch "Source buffer: "))))
    (scroll-left "^P\np")
    (scroll-right "^P\np")
    (search-backward "MSearch backward: ")
    (search-backward-regexp "sRE search backward: ")
    (search-forward "MSearch: ")
    (search-forward-regexp "sRE search: ")
    (select-frame "e")
    (set-file-modes (let ((file (read-file-name "File: "))) (list file (read-file-modes nil file))))
    (signal-process (list (read-string "Process (name or number): ") (read-signal-name)))
    (start-kbd-macro "P")
    (suspend-emacs "")
    (transpose-regions (if (< (length mark-ring) 2) (error "Other region must be marked before transposing two regions") (let* ((num (if current-prefix-arg (prefix-numeric-value current-prefix-arg) 0)) (ring-length (length mark-ring)) (eltnum (mod num ring-length)) (eltnum2 (mod (1+ num) ring-length))) (list (point) (mark) (elt mark-ring eltnum) (elt mark-ring eltnum2)))))
    (unix-sync "")
    (upcase-initials-region (list (region-beginning) (region-end) (region-noncontiguous-p)))
    (upcase-region (list (region-beginning) (region-end) (region-noncontiguous-p)))
    (upcase-word "p")
    (widen "")
    (write-region "r\nFWrite region to file: \ni\ni\ni\np")
    )
  "GNU 31.1 interactive specs of commands implemented in C.")

(dolist (entry emacs-command-loop-builtins--c-command-specs)
  ;; The property does not depend on when the definition is installed, so
  ;; this is independent of file load order.
  (unless (get (car entry) 'interactive-form)
    (put (car entry) 'interactive-form (list 'interactive (cadr entry)))))

(when (emacs-command-loop-builtins--install-function-p 'call-interactively)
  (defun call-interactively (function &optional record-flag keys)
    "Call FUNCTION using its interactive specification and current prefix.
When RECORD-FLAG is non-nil, record the invocation in `command-history'.
KEYS, when non-nil, must be a vector of invoking events."
    (unless (or (null keys) (vectorp keys))
      (signal 'wrong-type-argument (list 'vectorp keys)))
    (unless (emacs-command-loop--commandp function)
      (signal 'wrong-type-argument (list 'commandp function)))
    (let* ((form (emacs-command-loop--interactive-form function))
           (emacs-command-loop--current-prefix-arg current-prefix-arg)
           (args (emacs-command-loop--build-args (cadr form))))
      (unless (listp args)
        (signal 'wrong-type-argument (list 'listp args)))
      (when record-flag
        (let ((entry (list function))
              (rest args))
          (while rest
            (let ((arg (car rest)))
              (setq entry
                    (append entry
                            (list (if (or (consp arg)
                                          (and (symbolp arg) arg
                                               (not (eq arg t))))
                                      (list 'quote arg)
                                    arg)))))
            (setq rest (cdr rest)))
          (setq command-history (cons entry command-history))))
      (apply #'funcall-interactively function args))))

(when (emacs-command-loop-builtins--install-function-p 'funcall-interactively)
  (defalias 'funcall-interactively #'emacs-command-loop-funcall-interactively))

(when (emacs-command-loop-builtins--install-function-p 'command-execute)
  (defalias 'command-execute #'emacs-command-loop-command-execute))

(when (emacs-command-loop-builtins--install-function-p 'command-loop-1)
  (defalias 'command-loop-1 #'emacs-command-loop-1))

(when (emacs-command-loop-builtins--install-function-p 'top-level)
  (defalias 'top-level #'emacs-command-loop-top-level))

(when (emacs-command-loop-builtins--install-function-p 'recursive-edit)
  (defalias 'recursive-edit #'emacs-command-loop-recursive-edit))

(when (emacs-command-loop-builtins--install-function-p 'recursion-depth)
  (defalias 'recursion-depth #'emacs-command-loop-recursion-depth))

(when (emacs-command-loop-builtins--install-function-p 'execute-extended-command)
  (defalias 'execute-extended-command
    #'emacs-command-loop-execute-extended-command))

(when (emacs-command-loop-builtins--install-function-p 'universal-argument)
  (defalias 'universal-argument #'emacs-command-loop-universal-argument))

(when (emacs-command-loop-builtins--install-function-p 'digit-argument)
  (defalias 'digit-argument #'emacs-command-loop-digit-argument))

(when (emacs-command-loop-builtins--install-function-p 'negative-argument)
  (defalias 'negative-argument #'emacs-command-loop-negative-argument))

(when (emacs-command-loop-builtins--install-function-p 'prefix-numeric-value)
  (defalias 'prefix-numeric-value
    #'emacs-command-loop--prefix-numeric-value))

(when (emacs-command-loop-builtins--install-function-p 'keyboard-quit)
  (defalias 'keyboard-quit #'emacs-command-loop-keyboard-quit))

(when (emacs-command-loop-builtins--install-function-p 'exit-recursive-edit)
  (defun exit-recursive-edit ()
    "Exit the innermost recursive edit, or signal a user error."
    (interactive)
    (if (zerop emacs-command-loop--recursion-depth)
        (signal 'user-error '("No recursive edit is in progress"))
      (emacs-command-loop-exit-recursive-edit))))

(when (emacs-command-loop-builtins--install-function-p 'kill-emacs)
  (defalias 'kill-emacs #'emacs-command-loop-kill-emacs))

(defvar emacs-command-loop--sigint-handler-installed-p nil
  "Non-nil after the pure-Elisp SIGINT compatibility handler is installed.")

(when (emacs-command-loop-builtins--install-function-p 'install-sigint-handler)
  (defun install-sigint-handler ()
    "Install the pure-Elisp SIGINT compatibility handler.

Standalone NeLisp may provide this as a runtime builtin.  When it does
not, the Elisp fallback records installation and exposes the same
idempotent success value expected by `nemacs-main'."
    (setq emacs-command-loop--sigint-handler-installed-p t)
    t))

(when (emacs-command-loop-builtins--install-function-p '_sigint-handler-installed-p)
  (defun _sigint-handler-installed-p ()
    "Return non-nil when `install-sigint-handler' has been called."
    emacs-command-loop--sigint-handler-installed-p))

(when (emacs-command-loop-builtins--install-function-p 'set-quit-flag)
  (defun set-quit-flag ()
    "Set the command-loop quit flag and return t."
    (when (not (boundp 'emacs-version))
      (setq quit-flag t))
    (setq emacs-command-loop--quit-flag t)
    t))

(when (emacs-command-loop-builtins--install-function-p 'clear-quit-flag)
  (defun clear-quit-flag ()
    "Clear the command-loop quit flag and return nil."
    (when (not (boundp 'emacs-version))
      (setq quit-flag nil))
    (setq emacs-command-loop--quit-flag nil)
    nil))

(when (emacs-command-loop-builtins--install-function-p 'quit-flag-pending-p)
  (defun quit-flag-pending-p ()
    "Return non-nil when a quit is pending."
    (or emacs-command-loop--quit-flag
        (and (not (boundp 'emacs-version)) quit-flag))))

;; Real Emacs binds `recursive-edit' / `abort-recursive-edit' as C
;; primitives — same gating pattern.  Track C already aliased
;; `abort-recursive-edit' to the minibuffer cancel routine; the
;; minibuffer's quit-signal will still propagate through the
;; command-loop's condition-case so this is consistent.

;;;; --- variable bridges ----------------------------------------------

(defvar this-command nil
  "Command being dispatched; hooks may replace it for command history.")
(defvar last-command nil
  "Command completed by the preceding command-loop iteration.")
(defvar real-this-command nil
  "Selected command, preserved when hooks replace `this-command'.")

(unless (boundp 'last-command-event)
  (defvar last-command-event nil
    "Phase B.1 bridge: last event of the key sequence triggering the
current command."))

(unless (boundp 'last-input-event)
  (defvar last-input-event nil
    "Phase B.1 bridge: most recent event read by `read-event'."))

(unless (boundp 'last-nonmenu-event)
  (defvar last-nonmenu-event nil
    "Phase B.1 bridge: most recent input event NOT from a menu bar."))

(unless (boundp 'unread-command-events)
  (defvar unread-command-events nil
    "Phase B.1 bridge: standard alternative event-injection queue.
The substrate `emacs-command-loop-read-event' drains this as a
secondary source after `emacs-command-loop--unread-events' is empty."))

(unless (boundp 'quit-flag)
  (defvar quit-flag nil
    "Phase B.1 bridge: non-nil when quit was requested."))

(unless (boundp 'inhibit-quit)
  (defvar inhibit-quit nil
    "Phase B.1 bridge: non-nil suppresses `quit-flag' processing."))

(unless (boundp 'throw-on-input)
  (defvar throw-on-input nil
    "Phase B.1 bridge: tag to `throw' to on next input read."))

(unless (boundp 'prefix-arg)
  (defvar prefix-arg nil
    "Phase B.3 bridge: prefix arg pending for the next `call-interactively'."))

(unless (boundp 'current-prefix-arg)
  (defvar current-prefix-arg nil
    "Phase B.3 bridge: prefix arg of the command currently executing."))

(unless (boundp 'pre-command-hook)
  (defvar pre-command-hook nil
    "Phase B.4 bridge: hook run before each command-loop dispatch."))

(unless (fboundp 'tooltip-hide)
  (defvar tooltip-timeout-id nil)
  (defvar tooltip-hide-time nil)
  (defun tooltip-hide (&optional _ignored-arg)
    "Hide a tooltip, clearing its echo-area fallback on non-graphic frames."
    (when (timerp tooltip-timeout-id)
      (cancel-timer tooltip-timeout-id)
      (setq tooltip-timeout-id nil))
    (if (display-graphic-p)
        (when (and (fboundp 'x-hide-tip) (x-hide-tip))
          (setq tooltip-hide-time (float-time)))
      (let ((text (current-message)))
        (message "")
        (when (and text (not (equal text "")))
          (setq tooltip-hide-time (float-time))))))
  ;; GNU's default pre-command hook hides tooltips before every command,
  ;; including macro commands.  Keep this in the shared loop so binding or
  ;; removing the hook also suppresses the echo-area clear normally.
  (add-hook 'pre-command-hook #'tooltip-hide))

(unless (boundp 'post-command-hook)
  (defvar post-command-hook nil
    "Phase B.4 bridge: hook run after each command-loop dispatch."))

(when (emacs-command-loop-builtins--install-function-p 'prefix-numeric-value)
  (defun prefix-numeric-value (arg)
    "Return the numeric value of raw prefix argument ARG.
nil -> 1; the symbol `-' -> -1; an integer -> itself; a list (as produced
by `C-u') -> its car; anything else -> 1."
    (cond
     ((null arg) 1)
     ((eq arg '-) -1)
     ((integerp arg) arg)
     ((consp arg) (car arg))
     (t 1))))

(provide 'emacs-command-loop-builtins)

;;; emacs-command-loop-builtins.el ends here
