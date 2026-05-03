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
        emacs-command-loop--throw-on-input      nil))

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
   ((consp arg) (or (car arg) 1))
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

(provide 'emacs-command-loop)

;;; emacs-command-loop.el ends here
