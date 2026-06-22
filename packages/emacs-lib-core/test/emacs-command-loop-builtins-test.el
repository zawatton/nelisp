;;; emacs-command-loop-builtins-test.el --- ERT for emacs-command-loop  -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for the Layer 2 command-loop foundation (Phase B.1).  Under
;; host Emacs the unprefixed bridges are gated off (= host's C
;; builtins win), so behavioural assertions exercise the prefixed
;; `emacs-command-loop-*' API directly.  Featurep / fboundp / boundp
;; parity is checked separately.

;;; Code:

(require 'ert)
(require 'emacs-command-loop-builtins)
(require 'cl-lib)

(defmacro emacs-command-loop-builtins-test--with-fresh-state (&rest body)
  "Run BODY with a clean substrate state."
  (declare (indent 0) (debug (body)))
  `(progn
     (emacs-command-loop-reset)
     (unwind-protect
         (progn ,@body)
       (emacs-command-loop-reset))))

(defmacro emacs-command-loop-builtins-test--should-quit (&rest body)
  "Assert that BODY signals `quit'."
  (declare (indent 0) (debug (body)))
  `(let ((caught nil))
     (condition-case nil
         (progn ,@body)
       (quit (setq caught t)))
     (should caught)))

;;;; A. require-loads-cleanly + fboundp / boundp parity

(ert-deftest emacs-command-loop-builtins-test/require-loads-cleanly ()
  (should (featurep 'emacs-command-loop-builtins))
  (should (featurep 'emacs-command-loop))
  (dolist (sym '(read-event read-char read-command
                 this-command-keys this-command-keys-vector
                 this-single-command-keys this-single-command-raw-keys
                 clear-this-command-keys read-key-sequence
                 read-key-sequence-vector call-interactively
                 funcall-interactively command-execute command-loop-1
                 top-level recursive-edit recursion-depth
                 execute-extended-command universal-argument
                 digit-argument negative-argument keyboard-quit
                 exit-recursive-edit kill-emacs install-sigint-handler
                 _sigint-handler-installed-p set-quit-flag
                 clear-quit-flag quit-flag-pending-p))
    (should (fboundp sym)))
  (dolist (sym '(this-command last-command real-this-command
                 last-command-event last-input-event last-nonmenu-event
                 unread-command-events quit-flag inhibit-quit
                 throw-on-input))
    (should (boundp sym))))

;;;; B. feed-events + read-event roundtrip (FIFO)

(ert-deftest emacs-command-loop-builtins-test/feed-and-read-fifo ()
  (emacs-command-loop-builtins-test--with-fresh-state
    (emacs-command-loop-feed-events ?a ?b ?c)
    (should (= ?a (emacs-command-loop-read-event)))
    (should (= ?b (emacs-command-loop-read-event)))
    (should (= ?c (emacs-command-loop-read-event)))))

;;;; C. read-event signals on empty queue

(ert-deftest emacs-command-loop-builtins-test/read-event-empty-signals ()
  (emacs-command-loop-builtins-test--with-fresh-state
    (should-error (emacs-command-loop-read-event)
                  :type 'emacs-command-loop-no-input)))

;;;; D. read-event tracks last-input-event + last-nonmenu-event

(ert-deftest emacs-command-loop-builtins-test/read-event-tracks-last-input ()
  (emacs-command-loop-builtins-test--with-fresh-state
    (emacs-command-loop-feed-events ?x ?y)
    (emacs-command-loop-read-event)
    (should (= ?x emacs-command-loop--last-input-event))
    (should (= ?x emacs-command-loop--last-nonmenu-event))
    (emacs-command-loop-read-event)
    (should (= ?y emacs-command-loop--last-input-event))))

;; NB: pinning the public-defvar mirror (Track X follow-up 2026-05-05 —
;; `read-event' / `record-key' also publish their event into the
;; canonical unprefixed `last-input-event' / `last-command-event' so
;; `self-insert-command' etc. dispatched via `emacs-command-loop-step'
;; can read it) is done at the GUI driver level — under host Emacs
;; those defvars are C-owned and `(set ...)' does not round-trip
;; through the reader's accessor, so a Layer 2 ERT cannot observe the
;; mirror.  See nelisp-emacs-gtk `command_loop_dispatch_self_insert_*'
;; tests, which exercise the same path against a standalone Session.

;;;; E. read-char rejects non-integer events

(ert-deftest emacs-command-loop-builtins-test/read-char-rejects-non-integer ()
  (emacs-command-loop-builtins-test--with-fresh-state
    (emacs-command-loop-feed-events 'return)
    (should-error (emacs-command-loop-read-char)
                  :type 'wrong-type-argument)))

(ert-deftest emacs-command-loop-builtins-test/read-char-accepts-integer ()
  (emacs-command-loop-builtins-test--with-fresh-state
    (emacs-command-loop-feed-events ?Q)
    (should (= ?Q (emacs-command-loop-read-char)))))

;;;; F. unread-command-events fallback path

(ert-deftest emacs-command-loop-builtins-test/unread-command-events-fallback ()
  (emacs-command-loop-builtins-test--with-fresh-state
    ;; Substrate queue empty; populate the bridge defvar instead.
    (let ((unread-command-events (list ?p ?q)))
      ;; Drain via substrate read.  Since the substrate writes via
      ;; `(set 'unread-command-events ...)' which targets the dynamic
      ;; binding, the let-binding here MUST be of a defvar-declared
      ;; symbol — `unread-command-events' qualifies (= our bridge
      ;; defvars it).
      (should (= ?p (emacs-command-loop-read-event)))
      (should (= ?q (emacs-command-loop-read-event)))
      (should (null unread-command-events)))))

;;;; G. set-this-command + mark-command-finished bookkeeping

(ert-deftest emacs-command-loop-builtins-test/set-and-finish-this-command ()
  (emacs-command-loop-builtins-test--with-fresh-state
    (emacs-command-loop-set-this-command 'forward-char)
    (should (eq 'forward-char emacs-command-loop--this-command))
    (should (eq 'forward-char emacs-command-loop--real-this-command))
    (should (null emacs-command-loop--last-command))
    (emacs-command-loop-mark-command-finished)
    (should (eq 'forward-char emacs-command-loop--last-command))
    (should (null emacs-command-loop--this-command))
    (should (null emacs-command-loop--real-this-command))))

;;;; H. record-key + this-command-keys accumulator

(ert-deftest emacs-command-loop-builtins-test/record-key-accumulates ()
  (emacs-command-loop-builtins-test--with-fresh-state
    (emacs-command-loop-record-key ?h)
    (emacs-command-loop-record-key ?i)
    (should (equal "hi" (emacs-command-loop-this-command-keys)))
    (should (= ?i emacs-command-loop--last-command-event))
    (let ((v (emacs-command-loop-this-command-keys-vector)))
      (should (vectorp v))
      (should (= 2 (length v)))
      (should (= ?h (aref v 0)))
      (should (= ?i (aref v 1))))
    (emacs-command-loop-clear-this-command-keys)
    (should (equal "" (emacs-command-loop-this-command-keys)))))

;;;; I. quit-flag honoured + inhibit-quit shields

(ert-deftest emacs-command-loop-builtins-test/quit-flag-fires-signal ()
  (emacs-command-loop-builtins-test--with-fresh-state
    (emacs-command-loop-feed-events ?a)
    (setq emacs-command-loop--quit-flag t)
    (should-error (emacs-command-loop-read-event)
                  :type 'emacs-command-loop-quit)
    ;; Flag is consumed.
    (should (null emacs-command-loop--quit-flag))
    ;; Event was NOT popped (= signal raised before pop).
    (should (= ?a (emacs-command-loop-read-event)))))

(ert-deftest emacs-command-loop-builtins-test/inhibit-quit-shields ()
  (emacs-command-loop-builtins-test--with-fresh-state
    (emacs-command-loop-feed-events ?a)
    (setq emacs-command-loop--quit-flag t
          emacs-command-loop--inhibit-quit t)
    ;; With inhibit-quit set, read still fires.
    (should (= ?a (emacs-command-loop-read-event)))
    ;; Flag still latched; B.6 will clear it explicitly.
    (should (eq t emacs-command-loop--quit-flag))))

(ert-deftest emacs-command-loop-builtins-test/quit-flag-compat-roundtrip ()
  (emacs-command-loop-builtins-test--with-fresh-state
    (let ((quit-flag nil)
          (emacs-command-loop--quit-flag nil))
      (should (eq t (install-sigint-handler)))
      (should (_sigint-handler-installed-p))
      (clear-quit-flag)
      (should-not (quit-flag-pending-p))
      (should (eq t (set-quit-flag)))
      (should (quit-flag-pending-p))
      (should-not (clear-quit-flag))
      (should-not (quit-flag-pending-p)))))

;;;; J. reset wipes everything

(ert-deftest emacs-command-loop-builtins-test/reset-wipes-state ()
  (emacs-command-loop-feed-events ?z)
  (emacs-command-loop-set-this-command 'foo)
  (emacs-command-loop-record-key ?a)
  (setq emacs-command-loop--quit-flag t
        emacs-command-loop--inhibit-quit t)
  (emacs-command-loop-reset)
  (should (null emacs-command-loop--unread-events))
  (should (null emacs-command-loop--this-command))
  (should (null emacs-command-loop--last-command))
  (should (equal "" emacs-command-loop--this-command-keys))
  (should (null emacs-command-loop--quit-flag))
  (should (null emacs-command-loop--inhibit-quit)))

;;;; K. pending-p mirrors both queues

(ert-deftest emacs-command-loop-builtins-test/pending-p-checks-both-queues ()
  (emacs-command-loop-builtins-test--with-fresh-state
    (should-not (emacs-command-loop-pending-p))
    (emacs-command-loop-feed-events ?a)
    (should (emacs-command-loop-pending-p))
    (emacs-command-loop-read-event)
    (should-not (emacs-command-loop-pending-p))
    (let ((unread-command-events (list ?b)))
      (should (emacs-command-loop-pending-p)))))

;;;; L. Idempotence — second require leaves bindings alone

(ert-deftest emacs-command-loop-builtins-test/require-is-idempotent ()
  (let ((before-read    (symbol-function 'read-event))
        (before-keys-fn (symbol-function 'this-command-keys))
        (before-call    (symbol-function 'call-interactively))
        (before-execute (symbol-function 'command-execute)))
    (require 'emacs-command-loop-builtins)
    (should (eq before-read    (symbol-function 'read-event)))
    (should (eq before-keys-fn (symbol-function 'this-command-keys)))
    (should (eq before-call    (symbol-function 'call-interactively)))
    (should (eq before-execute (symbol-function 'command-execute)))))

(ert-deftest emacs-command-loop-builtins-test/bridge-overwrites-standalone-stubs-in-source ()
  (should (fboundp 'emacs-command-loop-builtins--install-function-p))
  (should-not (emacs-command-loop-builtins--install-function-p 'read-event))
  (let* ((file (locate-library "emacs-command-loop-builtins"))
         (file (if (and file (string-match-p "\\.elc\\'" file))
                   (concat (substring file 0 (- (length file) 1)))
                 file)))
    (should (and file (file-exists-p file)))
    (with-temp-buffer
      (insert-file-contents file)
      (dolist (sym '(read-event read-char read-command this-command-keys
                     this-command-keys-vector this-single-command-keys
                     this-single-command-raw-keys clear-this-command-keys
                     read-key-sequence read-key-sequence-vector
                     call-interactively funcall-interactively command-execute
                     command-loop-1 top-level recursive-edit recursion-depth
                     execute-extended-command universal-argument digit-argument
                     negative-argument keyboard-quit exit-recursive-edit
                     kill-emacs
                     install-sigint-handler _sigint-handler-installed-p
                     set-quit-flag clear-quit-flag quit-flag-pending-p))
        (goto-char (point-min))
        (should (search-forward
                 (format "(when (emacs-command-loop-builtins--install-function-p '%s)"
                         sym)
                 nil t))))))

(ert-deftest emacs-command-loop-builtins-test/install-gate-overwrites-bulk-stubs ()
  (let ((sym 'emacs-command-loop-builtins-test--stubbed))
    (unwind-protect
        (progn
          (fset sym #'ignore)
          (put sym 'emacs-stub-bulk t)
          (should (emacs-command-loop-builtins--install-function-p sym)))
      (put sym 'emacs-stub-bulk nil)
      (fmakunbound sym))))

(ert-deftest emacs-command-loop-builtins-test/kill-emacs-helper-calls-exit ()
  (let (calls)
    (cl-letf (((symbol-function 'exit)
               (lambda (code) (push code calls) :exited)))
      (should (eq :exited (emacs-command-loop-kill-emacs)))
      (should (eq :exited (emacs-command-loop-kill-emacs 42)))
      (should (eq :exited (emacs-command-loop-kill-emacs 'bad))))
    (should (equal calls '(1 42 0)))))

;;;; M. Phase B.2 — read-key-sequence

(require 'emacs-keymap)

(defmacro emacs-command-loop-builtins-test--with-fresh-keymaps (&rest body)
  "Run BODY with a fresh global map / chain to exercise read-key-sequence."
  (declare (indent 0) (debug (body)))
  `(let ((emacs-keymap-global-map (emacs-keymap-make-sparse-keymap))
         (emacs-keymap-local-map nil)
         (emacs-keymap-overriding-local-map nil)
         (emacs-keymap-overriding-terminal-local-map nil)
         (emacs-keymap-minor-mode-map-alist nil)
         (emacs-keymap-emulation-mode-map-alists nil)
         (emacs-keymap-chain-with-textprop nil))
     (emacs-command-loop-reset)
     (unwind-protect
         (progn ,@body)
       (emacs-command-loop-reset))))

(ert-deftest emacs-command-loop-builtins-test/read-key-sequence-single-bound ()
  (emacs-command-loop-builtins-test--with-fresh-keymaps
    (emacs-keymap-define-key emacs-keymap-global-map "a" 'self-insert-command)
    (emacs-command-loop-feed-events ?a)
    (let ((result (emacs-command-loop-read-key-sequence "go: ")))
      (should (equal "a" result)))))

(ert-deftest emacs-command-loop-builtins-test/read-key-sequence-undefined-stops ()
  (emacs-command-loop-builtins-test--with-fresh-keymaps
    ;; No bindings at all.
    (emacs-command-loop-feed-events ?z)
    (let ((result (emacs-command-loop-read-key-sequence)))
      (should (equal "z" result)))))

(ert-deftest emacs-command-loop-builtins-test/read-key-sequence-prefix-then-leaf ()
  (emacs-command-loop-builtins-test--with-fresh-keymaps
    ;; Bind C-x f (= ?\C-x ?f) to a function; C-x prefix should
    ;; auto-create a sub-keymap inside the global map.
    (let ((cx (emacs-keymap-make-sparse-keymap)))
      (emacs-keymap-define-key emacs-keymap-global-map (vector ?\C-x) cx)
      (emacs-keymap-define-key cx "f" 'find-file))
    (emacs-command-loop-feed-events ?\C-x ?f)
    (let ((result (emacs-command-loop-read-key-sequence-vector)))
      (should (vectorp result))
      (should (= 2 (length result)))
      (should (= ?\C-x (aref result 0)))
      (should (= ?f    (aref result 1))))))

(ert-deftest emacs-command-loop-builtins-test/read-key-sequence-symbol-event-returns-vector ()
  (emacs-command-loop-builtins-test--with-fresh-keymaps
    ;; A symbol event (= e.g. function key) forces vector return,
    ;; since stringable-p rejects non-integer events.
    (emacs-keymap-define-key emacs-keymap-global-map (vector 'return)
                             'newline)
    (emacs-command-loop-feed-events 'return)
    (let ((result (emacs-command-loop-read-key-sequence)))
      (should (vectorp result))
      (should (eq 'return (aref result 0))))))

(ert-deftest emacs-command-loop-builtins-test/read-key-sequence-records-keys ()
  (emacs-command-loop-builtins-test--with-fresh-keymaps
    (emacs-command-loop-feed-events ?a ?b)
    (emacs-keymap-define-key emacs-keymap-global-map "a" 'self-insert-command)
    (emacs-command-loop-read-key-sequence)
    (should (equal "a" (emacs-command-loop-this-command-keys)))))

;;;; N. Phase B.3 — call-interactively / command-execute

(defun emacs-command-loop-builtins-test--noop ()
  "Test fixture: takes no args, no interactive form."
  'noop-result)

(defun emacs-command-loop-builtins-test--noop-i ()
  "Test fixture: empty interactive form."
  (interactive)
  'noop-i-result)

(defun emacs-command-loop-builtins-test--accept-P (arg)
  "Test fixture: receives raw prefix arg."
  (interactive "P")
  arg)

(defun emacs-command-loop-builtins-test--accept-p (arg)
  "Test fixture: receives numeric prefix arg."
  (interactive "p")
  arg)

(defun emacs-command-loop-builtins-test--accept-list-spec (a b)
  "Test fixture: interactive form is a lisp form returning a list."
  (interactive (list 1 2))
  (cons a b))

(ert-deftest emacs-command-loop-builtins-test/call-interactively-no-spec ()
  (emacs-command-loop-builtins-test--with-fresh-state
    (let ((r (emacs-command-loop-call-interactively
              'emacs-command-loop-builtins-test--noop-i)))
      (should (eq 'noop-i-result r))
      ;; this-command was promoted to last-command
      (should (eq 'emacs-command-loop-builtins-test--noop-i
                  emacs-command-loop--last-command))
      (should (null emacs-command-loop--this-command)))))

(ert-deftest emacs-command-loop-builtins-test/call-interactively-P-passes-raw ()
  (emacs-command-loop-builtins-test--with-fresh-state
    (setq emacs-command-loop--prefix-arg '(4))
    (let ((r (emacs-command-loop-call-interactively
              'emacs-command-loop-builtins-test--accept-P)))
      (should (equal '(4) r))
      (should (null emacs-command-loop--prefix-arg)))))

(ert-deftest emacs-command-loop-builtins-test/call-interactively-p-numeric ()
  (emacs-command-loop-builtins-test--with-fresh-state
    (setq emacs-command-loop--prefix-arg '(4))
    (let ((r (emacs-command-loop-call-interactively
              'emacs-command-loop-builtins-test--accept-p)))
      (should (= 4 r))))
  (emacs-command-loop-builtins-test--with-fresh-state
    (setq emacs-command-loop--prefix-arg nil)
    (let ((r (emacs-command-loop-call-interactively
              'emacs-command-loop-builtins-test--accept-p)))
      (should (= 1 r)))))

(ert-deftest emacs-command-loop-builtins-test/call-interactively-list-spec ()
  (emacs-command-loop-builtins-test--with-fresh-state
    (let ((r (emacs-command-loop-call-interactively
              'emacs-command-loop-builtins-test--accept-list-spec)))
      (should (equal (cons 1 2) r)))))

(ert-deftest emacs-command-loop-builtins-test/call-interactively-rejects-non-function ()
  (emacs-command-loop-builtins-test--with-fresh-state
    (should-error (emacs-command-loop-call-interactively 42)
                  :type 'wrong-type-argument)))

(ert-deftest emacs-command-loop-builtins-test/funcall-interactively-passes-args ()
  (let ((r (emacs-command-loop-funcall-interactively #'+ 2 3)))
    (should (= 5 r))))

(ert-deftest emacs-command-loop-builtins-test/command-execute-on-symbol ()
  (emacs-command-loop-builtins-test--with-fresh-state
    (let ((r (emacs-command-loop-command-execute
              'emacs-command-loop-builtins-test--noop-i)))
      (should (eq 'noop-i-result r)))))

(ert-deftest emacs-command-loop-builtins-test/command-execute-on-keyboard-macro-string ()
  (emacs-command-loop-builtins-test--with-fresh-state
    (emacs-command-loop-command-execute "abc")
    (should (= ?a (emacs-command-loop-read-event)))
    (should (= ?b (emacs-command-loop-read-event)))
    (should (= ?c (emacs-command-loop-read-event)))))

(ert-deftest emacs-command-loop-builtins-test/command-execute-rejects-other ()
  (emacs-command-loop-builtins-test--with-fresh-state
    (should-error (emacs-command-loop-command-execute 42)
                  :type 'wrong-type-argument)))

(defvar emacs-command-loop-builtins-test--gui-calls nil)

(defun emacs-command-loop-builtins-test--gui-command ()
  "Test fixture for GUI bridge direct command dispatch."
  (push :gui-command emacs-command-loop-builtins-test--gui-calls)
  nil)

(ert-deftest emacs-command-loop-builtins-test/gui-command-execute-direct ()
  (emacs-command-loop-builtins-test--with-fresh-state
    (let (statuses)
      (setq emacs-command-loop-builtins-test--gui-calls nil)
      (emacs-command-loop-gui-register-backend
       :set-status (lambda (status) (push status statuses))
       :prefix-arg-empty-p (lambda () t))
      (emacs-command-loop-gui-set-context
       :command 'emacs-command-loop-builtins-test--gui-command
       :effective-command "emacs-command-loop-builtins-test--gui-command")
      (should-not (emacs-command-loop-gui-command-execute))
      (should (equal '(:gui-command)
                     emacs-command-loop-builtins-test--gui-calls))
      (should (null statuses))
      (should (eq 'emacs-command-loop-builtins-test--gui-command
                  emacs-command-loop--last-command)))))

(ert-deftest emacs-command-loop-builtins-test/gui-call-interactively-backend ()
  (emacs-command-loop-builtins-test--with-fresh-state
    (let (called)
      (emacs-command-loop-gui-register-backend
       :call-command
       (lambda (command)
         (push command called)
         :called))
      (emacs-command-loop-gui-set-context :command 'bridge-command)
      (should (eq :called (emacs-command-loop-gui-call-interactively)))
      (should (equal '(bridge-command) called))
      (should (eq 'bridge-command emacs-command-loop--last-command))
      (should (null emacs-command-loop--this-command)))))

(ert-deftest emacs-command-loop-builtins-test/gui-finish-command-bookkeeping ()
  (emacs-command-loop-builtins-test--with-fresh-state
    (emacs-command-loop-set-this-command 'bridge-command)
    (setq emacs-command-loop--this-command-keys "C-x C-f")
    (should (eq 'bridge-command
                (emacs-command-loop-gui-finish-command)))
    (should (eq 'bridge-command emacs-command-loop--last-command))
    (should (null emacs-command-loop--this-command))
    (should (null emacs-command-loop--real-this-command))
    (should (equal "C-x C-f" emacs-command-loop--this-command-keys))))

(ert-deftest emacs-command-loop-builtins-test/gui-call-interactively-context ()
  (emacs-command-loop-builtins-test--with-fresh-state
    (let (called)
      (emacs-command-loop-gui-register-backend
       :set-command (lambda (command) (push (list :set command) called))
       :set-keys (lambda (keys) (push (list :keys keys) called))
       :call-command (lambda (command) (push (list :call command) called)
                       :called))
      (should (eq :called
                  (emacs-command-loop-gui-call-interactively-context
                   :command 'bridge-command
                   :effective-command "bridge-command"
                   :keys "M-x"
                   :arg ""
                   :status "ok"
                   :prefix-arg "")))
      (should (equal '((:call bridge-command)
                       (:set bridge-command)
                       (:keys "M-x")
                       (:set bridge-command))
                     called))
      (should (eq 'bridge-command emacs-command-loop-gui-command))
      (should (string-equal "bridge-command"
                            emacs-command-loop-gui-effective-command))
      (should (string-equal "M-x" emacs-command-loop-gui-keys))
      (should (eq 'bridge-command emacs-command-loop--last-command)))))

(ert-deftest emacs-command-loop-builtins-test/gui-command-execution-state ()
  (emacs-command-loop-builtins-test--with-fresh-state
    (should (equal '(:command forward-char
                     :effective-command "forward-char"
                     :arg "3"
                     :status "ok")
                   (emacs-command-loop-gui-command-execution-state
                    "forward-char" "forward-char" "3")))
    (should (equal '(:command backward-char
                     :effective-command "backward-char"
                     :arg ""
                     :status "pending")
                   (emacs-command-loop-gui-command-execution-state
                    'backward-char nil nil "pending")))
    (should (equal '(:command replace-string
                     :effective-command "replace-string"
                     :arg "from"
                     :status "ok"
                     :minibuffer-arg "to"
                     :save-undo t)
                   (emacs-command-loop-gui-replace-execution-state
                    "replace-string" "from" "to")))))

(ert-deftest emacs-command-loop-builtins-test/gui-current-context-refresh ()
  (emacs-command-loop-builtins-test--with-fresh-state
    (let (set-values)
      (emacs-command-loop-gui-register-backend
       :current-command (lambda () 'current-command)
       :current-effective-command (lambda () "current-command")
       :current-keys (lambda () "C-c c")
       :current-arg (lambda () "arg")
       :current-status (lambda () "pending")
       :current-prefix-arg (lambda () "4")
       :set-command (lambda (command)
                      (push (list :command command) set-values))
       :set-effective-command
       (lambda (name) (push (list :effective name) set-values))
       :set-arg (lambda (arg) (push (list :arg arg) set-values))
       :set-status (lambda (status)
                     (push (list :status status) set-values))
       :set-prefix-arg (lambda (arg)
                         (push (list :prefix arg) set-values)))
      (should (equal
               '(:command current-command
                 :effective-command "current-command"
                 :keys "C-c c"
                 :arg "arg"
                 :status "pending"
                 :prefix-arg "4")
               (emacs-command-loop-gui-refresh-context-from-backend)))
      (should (eq 'current-command emacs-command-loop-gui-command))
      (should (string-equal "current-command"
                            emacs-command-loop-gui-effective-command))
      (should (string-equal "C-c c" emacs-command-loop-gui-keys))
      (should (string-equal "arg" emacs-command-loop-gui-arg))
      (should (string-equal "pending" emacs-command-loop-gui-status))
      (should (string-equal "4" emacs-command-loop-gui-prefix-arg))
      (should (equal '((:prefix "4")
                       (:status "pending")
                       (:arg "arg")
                       (:effective "current-command")
                       (:command current-command))
                     set-values)))))

(ert-deftest emacs-command-loop-builtins-test/gui-ingest-request-context ()
  (emacs-command-loop-builtins-test--with-fresh-state
    (let (set-values cleared)
      (emacs-command-loop-gui-register-backend
       :set-command (lambda (command)
                      (push (list :command command) set-values))
       :set-effective-command
       (lambda (name) (push (list :effective name) set-values))
       :set-keys (lambda (keys) (push (list :keys keys) set-values))
       :set-arg (lambda (arg) (push (list :arg arg) set-values))
       :set-status (lambda (status)
                     (push (list :status status) set-values))
       :set-prefix-arg (lambda (arg)
                         (push (list :prefix arg) set-values))
       :clear-command-request (lambda () (setq cleared t)))
      (should (equal
               '(:command find-file
                 :effective-command "find-file"
                 :keys ""
                 :arg "/tmp/a.txt"
                 :status "ok"
                 :prefix-arg "4")
               (emacs-command-loop-gui-ingest-request-context
                :command-name "find-file"
                :keys ""
                :arg "/tmp/a.txt"
                :prefix-arg "4"
                :status "ok")))
      (should (eq 'find-file emacs-command-loop-gui-command))
      (should (string-equal "find-file"
                            emacs-command-loop-gui-effective-command))
      (should (string-equal "/tmp/a.txt" emacs-command-loop-gui-arg))
      (should-not cleared)
      (setq set-values nil)
      (should (equal
               '(:command nil
                 :effective-command ""
                 :keys "C-x C-f"
                 :arg "/tmp/from-minibuffer"
                 :status "ok"
                 :prefix-arg "")
               (emacs-command-loop-gui-ingest-request-context
                :command-name ""
                :keys "C-x C-f"
                :arg "/tmp/original"
                :minibuffer-text "/tmp/from-minibuffer"
                :prefix-arg ""
                :status "ok")))
      (should cleared)
      (should-not emacs-command-loop-gui-command)
      (should (string-equal "" emacs-command-loop-gui-effective-command))
      (should (string-equal "C-x C-f" emacs-command-loop-gui-keys))
      (should (string-equal "/tmp/from-minibuffer"
                            emacs-command-loop-gui-arg))
      (should (equal '((:arg "/tmp/from-minibuffer")
                       (:effective "")
                       (:command nil)
                       (:status "ok")
                       (:prefix "")
                       (:arg "/tmp/original")
                       (:keys "C-x C-f")
                       (:effective "")
                       (:command nil))
                     set-values))
      (setq set-values nil
            cleared nil)
      (should (equal
               '(:command nil
                 :effective-command ""
                 :keys "C-f"
                 :arg "/tmp/ignored"
                 :status "ok"
                 :prefix-arg "")
               (emacs-command-loop-gui-ingest-request-context
                :command-name "end-of-buffer"
                :keys "C-f"
                :arg "/tmp/current"
                :minibuffer-text "/tmp/ignored"
                :prefix-arg ""
                :status "ok")))
      (should cleared)
      (should-not emacs-command-loop-gui-command)
      (should (string-equal "" emacs-command-loop-gui-effective-command))
      (should (string-equal "/tmp/ignored" emacs-command-loop-gui-arg))
      (should (equal '((:arg "/tmp/ignored")
                       (:effective "")
                       (:command nil)
                       (:status "ok")
                       (:prefix "")
                       (:arg "/tmp/current")
                       (:keys "C-f")
                       (:effective "")
                       (:command nil))
                     set-values)))))

(ert-deftest emacs-command-loop-builtins-test/gui-finalize-status ()
  (emacs-command-loop-builtins-test--with-fresh-state
    (let (statuses)
      (emacs-command-loop-gui-register-backend
       :set-status (lambda (status) (push status statuses))
       :error-status-p (lambda (status)
                         (string-equal status "backend-error")))
      (should (eq 'read-only
                  (emacs-command-loop-gui-finalize-status
                   :status "read-only")))
      (should (eq 'unsupported
                  (emacs-command-loop-gui-finalize-status
                   :status "unsupported")))
      (should (eq 'error
                  (emacs-command-loop-gui-finalize-status
                   :status "file-not-found")))
      (should (eq 'error
                  (emacs-command-loop-gui-finalize-status
                   :status "backend-error")))
      (should (eq 'minibuffer
                  (emacs-command-loop-gui-finalize-status
                   :status "minibuffer")))
      (should (eq 'prefix-arg
                  (emacs-command-loop-gui-finalize-status
                   :status "prefix-arg")))
      (should (eq 'normal
                  (emacs-command-loop-gui-finalize-status
                   :command 'org-metaright
                   :effective-command "org-metaright"
                   :status "unsupported")))
      (should (equal '("ok") statuses))
      (should (string-equal "ok" emacs-command-loop-gui-status)))))

(ert-deftest emacs-command-loop-builtins-test/gui-call-interactively-current-context ()
  (emacs-command-loop-builtins-test--with-fresh-state
    (let (called)
      (emacs-command-loop-gui-register-backend
       :current-command (lambda () 'backend-command)
       :call-command (lambda (command) (setq called command) :called))
      (should (eq :called
                  (emacs-command-loop-gui-call-interactively-current-context)))
      (should (eq 'backend-command called))
      (should (eq 'backend-command emacs-command-loop--last-command)))))

(ert-deftest emacs-command-loop-builtins-test/gui-call-interactively-adapted-command ()
  (emacs-command-loop-builtins-test--with-fresh-state
    (let (adapted direct)
      (should (eq 'goto-char
                  (emacs-command-loop-gui-command-adapter-kind
                   'goto-char)))
      (should (eq 'zap-to-char
                  (emacs-command-loop-gui-command-adapter-kind
                   'zap-to-char)))
      (should-not (emacs-command-loop-gui-command-adapter-kind
                   'forward-char))
      (emacs-command-loop-gui-register-backend
       :call-adapted-command
       (lambda (command kind)
         (push (list command kind) adapted)
         nil)
       :call-command
       (lambda (command)
         (push command direct)
         :direct))
      (emacs-command-loop-gui-set-context :command 'goto-char)
      (should-not (emacs-command-loop-gui-call-interactively))
      (should (equal '((goto-char goto-char)) adapted))
      (should-not direct)
      (setq adapted nil)
      (emacs-command-loop-gui-set-context :command 'zap-to-char)
      (should-not (emacs-command-loop-gui-call-interactively))
      (should (equal '((zap-to-char zap-to-char)) adapted))
      (should-not direct)
      (emacs-command-loop-gui-set-context :command 'forward-char)
      (should (eq :direct (emacs-command-loop-gui-call-interactively)))
      (should (equal '(forward-char) direct)))))

(ert-deftest emacs-command-loop-builtins-test/gui-command-execute-read-only ()
  (emacs-command-loop-builtins-test--with-fresh-state
    (let (statuses called)
      (emacs-command-loop-gui-register-backend
       :commandp (lambda (_command) t)
       :read-only-p (lambda () t)
       :read-only-command-p (lambda (_command) t)
       :call-command (lambda (_command) (setq called t))
       :set-status (lambda (status) (push status statuses)))
      (emacs-command-loop-gui-set-context :command 'ignored-command)
      (should-not (emacs-command-loop-gui-command-execute))
      (should-not called)
      (should (equal '("read-only") statuses))
      (should (string-equal "read-only" emacs-command-loop-gui-status)))))

(ert-deftest emacs-command-loop-builtins-test/gui-command-execute-context ()
  (emacs-command-loop-builtins-test--with-fresh-state
    (let (called)
      (emacs-command-loop-gui-register-backend
       :commandp (lambda (_command) t)
       :prefix-arg-empty-p (lambda () t)
       :call-command (lambda (command) (setq called command) :called))
      (should (eq :called
                  (emacs-command-loop-gui-command-execute-context
                   :command 'context-command
                   :effective-command "context-command"
                   :keys "C-c c"
                   :arg ""
                   :status "ok"
                   :prefix-arg "")))
      (should (eq 'context-command called))
      (should (eq 'context-command emacs-command-loop-gui-command))
      (should (string-equal "context-command"
                            emacs-command-loop-gui-effective-command))
      (should (string-equal "C-c c" emacs-command-loop-gui-keys)))))

(ert-deftest emacs-command-loop-builtins-test/gui-command-execute-current-context ()
  (emacs-command-loop-builtins-test--with-fresh-state
    (let (called)
      (emacs-command-loop-gui-register-backend
       :current-command (lambda () 'backend-command)
       :current-effective-command (lambda () "backend-command")
       :current-keys (lambda () "C-c c")
       :current-arg (lambda () "")
       :current-status (lambda () "ok")
       :current-prefix-arg (lambda () "")
       :commandp (lambda (_command) t)
       :prefix-arg-empty-p (lambda () t)
       :call-command (lambda (command) (setq called command) :called))
      (should (eq :called
                  (emacs-command-loop-gui-command-execute-current-context)))
      (should (eq 'backend-command called))
      (should (eq 'backend-command emacs-command-loop-gui-command))
      (should (string-equal "C-c c" emacs-command-loop-gui-keys)))))

(ert-deftest emacs-command-loop-builtins-test/gui-before-command-clears-cycle-state ()
  (emacs-command-loop-builtins-test--with-fresh-state
    (let (cleared fallback)
      (emacs-command-loop-gui-register-backend
       :clear-cycle-spacing-state (lambda () (setq cleared t))
       :before-command (lambda (_command) (setq fallback t)))
      (should-not (emacs-command-loop-gui-before-command 'cycle-spacing))
      (should-not cleared)
      (should-not fallback)
      (emacs-command-loop-gui-before-command 'forward-char)
      (should cleared)
      (should-not fallback))))

(ert-deftest emacs-command-loop-builtins-test/gui-self-insert-key-text-policy ()
  (emacs-command-loop-builtins-test--with-fresh-state
    (should (equal "a" (emacs-command-loop-gui-self-insert-key-text "a")))
    (should (equal " " (emacs-command-loop-gui-self-insert-key-text "SPC")))
    (should-not (emacs-command-loop-gui-self-insert-key-text "C-x"))
    (emacs-command-loop-gui-set-context :keys "z")
    (should (equal "z" (emacs-command-loop-gui-self-insert-key-text)))))

(ert-deftest emacs-command-loop-builtins-test/gui-command-execute-prefix ()
  (emacs-command-loop-builtins-test--with-fresh-state
    (let (prefix-executed direct-called)
      (emacs-command-loop-gui-register-backend
       :commandp (lambda (_command) t)
       :prefix-command-p (lambda (_command) nil)
       :prefix-arg-empty-p (lambda () nil)
       :execute-with-prefix-arg (lambda () (setq prefix-executed t))
       :call-command (lambda (_command) (setq direct-called t)))
      (emacs-command-loop-gui-set-context :command 'prefixed-command)
      (emacs-command-loop-gui-command-execute)
      (should prefix-executed)
      (should-not direct-called))))

(ert-deftest emacs-command-loop-builtins-test/gui-command-execute-call-helper ()
  (emacs-command-loop-builtins-test--with-fresh-state
    (let (called before prefix-called prefix-seen)
      (emacs-command-loop-gui-register-backend
       :before-command (lambda (command) (setq before command))
       :prefix-arg-empty-p (lambda () t)
       :call-command (lambda (command) (setq called command)))
      (emacs-command-loop-gui-set-context :command 'normal-command)
      (emacs-command-loop-gui-command-execute-call)
      (should (eq 'normal-command before))
      (should (eq 'normal-command called))
      (setq called nil
            before nil)
      (emacs-command-loop-gui-register-backend
       :before-command (lambda (command) (setq before command))
       :current-prefix-arg (lambda () "3")
       :prefix-arg-empty-p (lambda () nil)
       :execute-with-prefix-arg
       (lambda ()
         (setq prefix-called t
               prefix-seen emacs-command-loop-gui-prefix-arg)
         nil)
       :call-command (lambda (command) (setq called command)))
      (setq emacs-command-loop-gui-prefix-arg "4")
      (emacs-command-loop-gui-set-context :command 'prefixed-command)
      (emacs-command-loop-gui-command-execute-call)
      (should (eq 'prefixed-command before))
      (should prefix-called)
      (should (equal "3" prefix-seen))
      (should-not called))))

(ert-deftest emacs-command-loop-builtins-test/gui-keymap-command ()
  (emacs-command-loop-builtins-test--with-fresh-state
    (let ((source (concat "C-x C-f\tfind-file\n"
                          "C-x C-s\tsave-buffer\n"
                          "C-x b\tswitch-to-buffer\n")))
      (should (string-equal
               "find-file"
               (emacs-command-loop-gui-keymap-command source "C-x C-f")))
      (should (string-equal
               "switch-to-buffer"
               (emacs-command-loop-gui-keymap-command source "C-x b")))
      (should (string-equal
               ""
               (emacs-command-loop-gui-keymap-command source "C-x k")))
      (should (string-equal
               ""
               (emacs-command-loop-gui-keymap-command source ""))))))

(ert-deftest emacs-command-loop-builtins-test/gui-lookup-key-sequence-sources ()
  (emacs-command-loop-builtins-test--with-fresh-state
    (should (string-equal
             "user-command"
             (emacs-command-loop-gui-lookup-key-sequence-from-sources
              "C-c x"
              "C-c x\tuser-command\n"
              "C-c x\tmode-command\n"
              "C-c x\tglobal-command\n")))
    (should (string-equal
             "mode-command"
             (emacs-command-loop-gui-lookup-key-sequence-from-sources
              "C-c x"
              ""
              "C-c x\tmode-command\n"
              "C-c x\tglobal-command\n")))
    (should (string-equal
             "global-command"
             (emacs-command-loop-gui-lookup-key-sequence-from-sources
              "C-c x"
              nil
              ""
              "C-c x\tglobal-command\n")))
    (should (string-equal
             ""
             (emacs-command-loop-gui-lookup-key-sequence-from-sources
              "C-c y"
              "C-c x\tuser-command\n")))))

(ert-deftest emacs-command-loop-builtins-test/gui-key-dispatch-spec ()
  (emacs-command-loop-builtins-test--with-fresh-state
    (emacs-command-loop-gui-register-backend
     :lookup-key-sequence (lambda () "forward-char"))
    (emacs-command-loop-gui-set-context :keys "C-f" :arg "")
    (should (equal '(:command forward-char
                     :effective-command "forward-char"
                     :arg ""
                     :status "ok")
                   (emacs-command-loop-gui-key-dispatch-spec)))
    (emacs-command-loop-gui-register-backend
     :lookup-key-sequence (lambda () ""))
    (emacs-command-loop-gui-set-context :keys "a" :arg "")
    (should (equal '(:command self-insert-command
                     :effective-command "self-insert-command"
                     :arg "a"
                     :status "ok"
                     :self-insert-text "a")
                   (emacs-command-loop-gui-key-dispatch-spec)))
    (emacs-command-loop-gui-set-context :keys "C-x C-z" :arg "")
    (should (equal '(:command nil
                     :effective-command "C-x C-z"
                     :arg ""
	                     :status "unsupported")
	                   (emacs-command-loop-gui-key-dispatch-spec)))))

(ert-deftest emacs-command-loop-builtins-test/normalize-key-event ()
  (should (eq 'backspace
              (emacs-command-loop-normalize-key-event
               #xff08 0 0 :named-events '((#xff08 . backspace)))))
  (should (= ?\C-x
             (emacs-command-loop-normalize-key-event
              ?x 4 0 :control-mask 4)))
  (should (= 0
             (emacs-command-loop-normalize-key-event
              ?\s 4 ?\s :control-mask 4)))
  (should (= ?a
             (emacs-command-loop-normalize-key-event
              ?a 0 ?a)))
  (should-not
   (emacs-command-loop-normalize-key-event #xffff 0 0)))

(ert-deftest emacs-command-loop-builtins-test/key-dispatch-lane-priority ()
  (should (eq 'minibuffer
              (emacs-command-loop-key-dispatch-lane
               :minibuffer-active t
               :query-replace-pending t
               :event ?a)))
  (should (eq 'query-replace
              (emacs-command-loop-key-dispatch-lane
               :query-replace-pending t
               :describe-key-pending t
               :event ?a)))
  (should (eq 'describe-key
              (emacs-command-loop-key-dispatch-lane
               :describe-key-pending t
               :quoted-insert-pending t
               :event ?a)))
  (should (eq 'keymap
              (emacs-command-loop-key-dispatch-lane :event ?a))))

(ert-deftest emacs-command-loop-builtins-test/keyboard-quit-state-plan ()
  (let ((plan
         (emacs-command-loop-keyboard-quit-state
          :minibuffer-active t
          :query-replace-pending t
          :describe-key-pending nil
          :register-pending-op 'copy
          :quoted-insert-pending t
          :mark-active t
          :pending-prefix [24])))
    (should (equal '(minibuffer query-replace register quoted-insert
                                mark prefix)
                   (plist-get plan :clear)))
    (should (equal "Quit" (plist-get plan :message)))))

(ert-deftest emacs-command-loop-builtins-test/key-dispatch-lane-electric-pair ()
  (should (eq 'electric-pair
              (emacs-command-loop-key-dispatch-lane
               :event ?\(
               :electric-pair-p t
               :electric-open-pairs '((?\( . ?\)))
               :electric-close-set '(?\)))))
  (should (eq 'keymap
              (emacs-command-loop-key-dispatch-lane
               :event ?\(
               :pending-prefix [?x]
               :electric-pair-p t
               :electric-open-pairs '((?\( . ?\)))
               :electric-close-set '(?\)))))
  (should (eq 'keymap
              (emacs-command-loop-key-dispatch-lane
               :event ?\(
               :electric-pair-p t
               :electric-open-pairs '((?\( . ?\)))
               :electric-close-set '(?\))
               :read-only-p t))))

(ert-deftest emacs-command-loop-builtins-test/install-basic-edit-key-bindings ()
  (let ((m (make-sparse-keymap)))
    (emacs-command-loop-install-basic-edit-key-bindings m)
    (should (eq 'self-insert-command (lookup-key m (vector ?a))))
    (should (eq 'newline (lookup-key m (vector 13))))
    (should (eq 'delete-backward-char (lookup-key m (vector 127))))
    (should (eq 'delete-backward-char (lookup-key m (vector 'backspace))))
    (should (eq 'forward-char (lookup-key m (vector ?\C-f))))
    (should (eq 'backward-char (lookup-key m (vector 'left))))))

(ert-deftest emacs-command-loop-builtins-test/install-c-x-prefix-key-bindings ()
  (let ((m (make-sparse-keymap)))
    (emacs-command-loop-install-c-x-prefix-key-bindings
     m
     '((find-file . find-file)
       (save-buffer . save-buffer)
       (switch-to-buffer . switch-to-buffer)
       (list-buffers . list-buffers)
       (kill-buffer . kill-buffer)
       (quit . save-buffers-kill-emacs)
       (split-window-below . split-window-below)
       (split-window-right . split-window-right)
       (delete-window . delete-window)
       (delete-other-windows . delete-other-windows)
       (other-window . other-window))
     :command-bound-p (lambda (_command) t))
    (should (eq 'find-file (lookup-key m (vector ?\C-f))))
    (should (eq 'save-buffer (lookup-key m (vector ?\C-s))))
    (should (eq 'switch-to-buffer (lookup-key m (vector ?b))))
    (should (eq 'list-buffers (lookup-key m (vector ?\C-b))))
    (should (eq 'kill-buffer (lookup-key m (vector ?k))))
    (should (eq 'save-buffers-kill-emacs (lookup-key m (vector ?\C-c))))
    (should (eq 'split-window-below (lookup-key m (vector ?2))))
    (should (eq 'other-window (lookup-key m (vector ?o))))))

(ert-deftest emacs-command-loop-builtins-test/install-help-prefix-key-bindings ()
  (let ((m (make-sparse-keymap)))
    (emacs-command-loop-install-help-prefix-key-bindings
     m
     '((describe-key . describe-key)
       (describe-bindings . describe-bindings)
       (describe-function . describe-function)
       (describe-variable . describe-variable)
       (apropos . apropos))
     :command-bound-p (lambda (_command) t))
    (should (eq 'describe-key (lookup-key m (vector ?k))))
    (should (eq 'describe-bindings (lookup-key m (vector ?b))))
    (should (eq 'describe-function (lookup-key m (vector ?f))))
    (should (eq 'describe-variable (lookup-key m (vector ?v))))
    (should (eq 'apropos (lookup-key m (vector ?a))))))

(ert-deftest emacs-command-loop-builtins-test/build-standard-keymap ()
  (let ((m
         (emacs-command-loop-build-standard-keymap
          :quit-command 'test-quit
          :c-x-command-alist '((find-file . test-find-file)
                               (save-buffer . test-save-buffer))
          :c-x-extra-bindings (list (cons (vector ?u) 'test-undo))
          :extra-bindings (list (cons (vector ?x) 'test-extra))
          :help-command-alist '((describe-key . test-describe-key))
          :help-command-bound-p (lambda (_command) t))))
    (should (keymapp m))
    (should (eq 'test-quit (lookup-key m (kbd "C-x C-c"))))
    (should (eq 'test-quit (lookup-key m (kbd "C-c C-q"))))
    (should (eq 'test-undo (lookup-key m (kbd "C-x u"))))
    (should (eq 'test-extra (lookup-key m (vector ?x))))
    (should (eq 'test-describe-key (lookup-key m (kbd "C-h k"))))))

(ert-deftest emacs-command-loop-builtins-test/ensure-keymap-bindings ()
  (let ((rebuilt nil)
        (cleared nil)
        (m (make-sparse-keymap)))
    (define-key m (vector ?a) 'old-command)
    (should
     (eq 'new-map
         (emacs-command-loop-ensure-keymap-bindings
          :keymap m
          :required-bindings `((,(vector ?a) . self-insert-command))
          :lookup-key #'lookup-key
          :clear-keymap (lambda ()
                          (setq cleared t))
          :init-keymap (lambda ()
                         (setq rebuilt t)
                         'new-map))))
    (should cleared)
    (should rebuilt))
  (let ((m (make-sparse-keymap)))
    (define-key m (vector ?a) 'self-insert-command)
    (should
     (eq m
         (emacs-command-loop-ensure-keymap-bindings
          :keymap m
          :required-bindings `((,(vector ?a) . self-insert-command))
          :lookup-key #'lookup-key
          :clear-keymap (lambda ()
                          (error "should not clear"))
          :init-keymap (lambda ()
                         (error "should not rebuild")))))))

(ert-deftest emacs-command-loop-builtins-test/key-dispatch-read-only-policy ()
  (should (emacs-command-loop-key-dispatch-read-only-blocked-p
           'self-insert-command t '(self-insert-command yank)))
  (should-not
   (emacs-command-loop-key-dispatch-read-only-blocked-p
    'forward-char t '(self-insert-command yank)))
  (should-not
   (emacs-command-loop-key-dispatch-read-only-blocked-p
    'self-insert-command nil '(self-insert-command yank)))
  (should (emacs-command-loop-key-dispatch-read-only-blocked-p
           'custom-write t nil (lambda (binding)
                                 (eq binding 'custom-write)))))

(ert-deftest emacs-command-loop-builtins-test/key-dispatch-recording-policy ()
  (should (emacs-command-loop-key-dispatch-recording-p
           t 'self-insert-command '(start-kbd-macro end-kbd-macro)))
  (should-not
   (emacs-command-loop-key-dispatch-recording-p
    nil 'self-insert-command '(start-kbd-macro end-kbd-macro)))
  (should-not
   (emacs-command-loop-key-dispatch-recording-p
    t 'start-kbd-macro '(start-kbd-macro end-kbd-macro))))

(ert-deftest emacs-command-loop-builtins-test/key-dispatch-execution-kind ()
  (let ((inline '((self-insert-command . self-insert)
                  (delete-backward-char . delete-backward-char)
                  (kill-line . kill-line)
                  (yank . yank)))
        (direct (lambda (binding) (memq binding '(forward-char custom-command)))))
    (should (eq 'self-insert
                (emacs-command-loop-key-dispatch-execution-kind
                 'self-insert-command ?a
                 :inline-edit-commands inline
                 :direct-command-p direct)))
    (should (eq 'fallback
                (emacs-command-loop-key-dispatch-execution-kind
                 'self-insert-command 'left
                 :inline-edit-commands inline
                 :direct-command-p direct)))
    (should (eq 'delete-backward-char
                (emacs-command-loop-key-dispatch-execution-kind
                 'delete-backward-char 127
                 :inline-edit-commands inline
                 :direct-command-p direct)))
    (should (eq 'direct-funcall
                (emacs-command-loop-key-dispatch-execution-kind
                 'forward-char ?f
                 :inline-edit-commands inline
                 :direct-command-p direct)))
    (should (eq 'fallback
                (emacs-command-loop-key-dispatch-execution-kind
                 'missing-command ?x
                 :inline-edit-commands inline
                 :direct-command-p direct)))))

(ert-deftest emacs-command-loop-builtins-test/key-dispatch-direct-command-p ()
  (should (emacs-command-loop-key-dispatch-direct-command-p
           'find-file '(find-file save-buffer)))
  (should-not (emacs-command-loop-key-dispatch-direct-command-p
               'other-command '(find-file save-buffer))))

(ert-deftest emacs-command-loop-builtins-test/key-dispatch-direct-funcall ()
  (emacs-command-loop-builtins-test--with-fresh-state
    (let ((ran nil))
      (defun emacs-command-loop-builtins-test--direct-command ()
        (setq ran t)
        'direct-value)
      (let ((result
             (emacs-command-loop-key-dispatch-direct-funcall
              'emacs-command-loop-builtins-test--direct-command)))
        (should (plist-get result :ok))
        (should (eq 'direct-value (plist-get result :value)))
        (should ran)
        (should (eq 'emacs-command-loop-builtins-test--direct-command
                    emacs-command-loop--last-command))))))

(ert-deftest emacs-command-loop-builtins-test/key-dispatch-direct-funcall-error ()
  (emacs-command-loop-builtins-test--with-fresh-state
    (defun emacs-command-loop-builtins-test--direct-error ()
      (error "direct boom"))
    (let ((result
           (emacs-command-loop-key-dispatch-direct-funcall
            'emacs-command-loop-builtins-test--direct-error)))
      (should-not (plist-get result :ok))
      (should (string-match-p "direct boom" (plist-get result :message)))
      (should (plist-get result :error))
      (should (eq 'emacs-command-loop-builtins-test--direct-error
                  emacs-command-loop--last-command)))))

(ert-deftest emacs-command-loop-builtins-test/key-dispatch-inline-command-recording ()
  (emacs-command-loop-builtins-test--with-fresh-state
    (should (eq 'self-insert-command
                (emacs-command-loop-key-dispatch-inline-command
                 'self-insert)))
    (should (eq 'delete-backward-char
                (emacs-command-loop-key-dispatch-record-inline-command
                 'delete-backward-char)))
    (should (eq 'delete-backward-char emacs-command-loop--last-command))
    (setq emacs-command-loop--last-command 'kept)
    (should-not
     (emacs-command-loop-key-dispatch-record-inline-command 'unknown-kind))
    (should (eq 'kept emacs-command-loop--last-command))))

(ert-deftest emacs-command-loop-builtins-test/key-dispatch-run-inline-edit ()
  (emacs-command-loop-builtins-test--with-fresh-state
    (let (applied)
      (let ((result
             (emacs-command-loop-key-dispatch-run-inline-edit
              'yank
              (lambda () '(:kind insert :text "abc"))
              (lambda (edit)
                (setq applied edit)
                'applied))))
        (should (eq 'yank (plist-get result :command)))
        (should (equal '(:kind insert :text "abc")
                       (plist-get result :edit)))
        (should (equal applied (plist-get result :edit)))
        (should (eq 'applied (plist-get result :apply-result)))
        (should (eq 'yank emacs-command-loop--last-command))))))

(ert-deftest emacs-command-loop-builtins-test/key-dispatch-run-inline-kind ()
  (emacs-command-loop-builtins-test--with-fresh-state
    (let (applied)
      (let ((result
             (emacs-command-loop-key-dispatch-run-inline-kind
              'kill-line
              '((delete-backward-char . ignore)
                (kill-line . emacs-command-loop-builtins-test--inline-edit))
              (lambda (edit)
                (setq applied edit)
                :applied))))
        (should (eq 'kill-line (plist-get result :kind)))
        (should (eq 'kill-line (plist-get result :command)))
        (should (equal '(:kind inline-test)
                       (plist-get result :edit)))
        (should (equal applied (plist-get result :edit)))
        (should (eq :applied (plist-get result :apply-result)))
        (should (eq 'kill-line emacs-command-loop--last-command)))
      (setq emacs-command-loop--last-command 'kept)
      (should-not
       (emacs-command-loop-key-dispatch-run-inline-kind
        'unknown
        '((kill-line . emacs-command-loop-builtins-test--inline-edit))
        #'ignore))
      (should (eq 'kept emacs-command-loop--last-command)))))

(defun emacs-command-loop-builtins-test--inline-edit ()
  "Return a minimal edit plist for inline dispatch tests."
  '(:kind inline-test))

(ert-deftest emacs-command-loop-builtins-test/key-dispatch-run-self-insert ()
  (emacs-command-loop-builtins-test--with-fresh-state
    (let ((key ?a)
          (edit '(:beg 1 :end 2))
          applied)
      (should (eq :point-after
                  (emacs-command-loop-key-dispatch-run-self-insert
                   key
                   (lambda () edit)
                   (lambda (result)
                     (setq applied (list key result))
                     :point-after))))
      (should (equal (list ?a edit) applied))
      (should (eq 'self-insert-command emacs-command-loop--last-command)))))

(ert-deftest emacs-command-loop-builtins-test/menu-action-command ()
  (let ((commands '(("find-file" . find-file)
                    ("save-buffer" . save-buffer)
                    ("separator" . nil))))
    (should (eq 'find-file
                (emacs-command-loop-menu-action-command
                 "find-file" commands)))
    (should-not
     (emacs-command-loop-menu-action-command "missing" commands))
    (should-not
     (emacs-command-loop-menu-action-command "separator" commands))
    (should-not
     (emacs-command-loop-menu-action-command 'find-file commands))))

(ert-deftest emacs-command-loop-builtins-test/run-menu-action-command ()
  (let ((commands '(("find-file" . find-file)
                    ("save-buffer" . save-buffer)))
        called)
    (let ((result
           (emacs-command-loop-run-menu-action-command
            "save-buffer" commands
            :call-interactively
            (lambda (command)
              (setq called command)
              :called))))
      (should (eq 'save-buffer called))
      (should (equal "save-buffer" (plist-get result :action)))
      (should (eq 'save-buffer (plist-get result :command)))
      (should (eq :called (plist-get result :value))))
    (should-not
     (emacs-command-loop-run-menu-action-command
      "missing" commands
      :call-interactively (lambda (_command) :unexpected)))))

(ert-deftest emacs-command-loop-builtins-test/command-name-symbol ()
  (let ((callable (lambda (symbol)
                    (memq symbol
                          '(find-file nemacs-gtk-find-file
                            save-buffer)))))
    (should (eq 'find-file
                (emacs-command-loop-command-name-symbol
                 "find-file" :callable-p callable)))
    (should (eq 'nemacs-gtk-find-file
                (emacs-command-loop-command-name-symbol
                 "find-file"
                 :prefer-prefix "nemacs-gtk-"
                 :callable-p callable)))
    (should (eq 'missing-command
                (emacs-command-loop-command-name-symbol
                 "missing-command"
                 :callable-p callable
                 :allow-unbound t)))
    (should-not
     (emacs-command-loop-command-name-symbol
      "missing-command" :callable-p callable))
    (should-not
     (emacs-command-loop-command-name-symbol "" :callable-p callable))
    (should-not
     (emacs-command-loop-command-name-symbol 42 :callable-p callable))))

(defun emacs-command-loop-builtins-test--mx-dispatch-command ()
  "Test command for generic M-x dispatch helpers."
  (interactive)
  :direct)

(ert-deftest emacs-command-loop-builtins-test/ensure-command ()
  (let (messages)
    (should
     (emacs-command-loop-ensure-command
      'emacs-command-loop-builtins-test--mx-dispatch-command))
    (should-not
     (emacs-command-loop-ensure-command
      'emacs-command-loop-builtins-test--missing-command
      :feature-alist '((emacs-command-loop-builtins-test--missing-command
                        . missing-feature))
      :message-function
      (lambda (format-string &rest args)
        (push (apply #'format format-string args) messages))))
    (should (equal 1 (length messages)))
    (should (string-match-p "load failed" (car messages)))))

(ert-deftest emacs-command-loop-builtins-test/dispatch-command-with-handlers ()
  (let (called after messages)
    (should (eq :handler
                (emacs-command-loop-dispatch-command-with-handlers
                 'find-file
                 '((find-file . (lambda () :handler)))
                 :call-command (lambda (_command) :unexpected))))
    (should (equal '(:generic save-buffer)
                   (emacs-command-loop-dispatch-command-with-handlers
                    'save-buffer nil
                    :ensure-command (lambda (command)
                                      (setq called command)
                                      t)
                    :call-command (lambda (command)
                                    (list :generic command))
                    :after-command (lambda (command result)
                                     (setq after (list command result))))))
    (should (eq called 'save-buffer))
    (should (equal after '(save-buffer (:generic save-buffer))))
    (should-not
     (emacs-command-loop-dispatch-command-with-handlers
      'missing nil
      :ensure-command (lambda (_command) nil)
      :message-function
      (lambda (format-string &rest args)
        (push (apply #'format format-string args) messages))))
    (should (equal '("M-x missing is not a command") messages))))

(ert-deftest emacs-command-loop-builtins-test/run-extended-command-direct ()
  (let (prompts commands)
    (should
     (eq :ran
         (emacs-command-loop-run-extended-command
          :read-string (lambda (prompt)
                         (push prompt prompts)
                         "save-buffer")
          :dispatch-command (lambda (command)
                              (push command commands)
                              :ran))))
    (should (equal '("M-x ") prompts))
    (should (equal '(save-buffer) commands))))

(ert-deftest emacs-command-loop-builtins-test/run-extended-command-handlers ()
  (should
   (eq :handled
       (emacs-command-loop-run-extended-command
        :command-name "find-file"
        :handlers '((find-file . (lambda () :handled)))))))

(ert-deftest emacs-command-loop-builtins-test/run-extended-command-reports-error ()
  (let (messages)
    (should-not
     (emacs-command-loop-run-extended-command
      :command-name "broken-command"
      :dispatch-command (lambda (_command)
                          (error "boom"))
      :message-function
      (lambda (format-string &rest args)
        (push (apply #'format format-string args) messages))))
    (should (equal '("M-x broken-command failed: (error \"boom\")")
                   messages))))

(ert-deftest emacs-command-loop-builtins-test/run-extended-command-unbound-hook ()
  (let (missing)
    (should-not
     (emacs-command-loop-run-extended-command
      :command-name "missing-command"
      :callable-p (lambda (_symbol) nil)
      :allow-unbound nil
      :unbound-function (lambda (name)
                          (setq missing name))))
    (should (equal "missing-command" missing))))

(ert-deftest emacs-command-loop-builtins-test/repeat-last-command ()
  (let (called)
    (should (eq 'empty
                (plist-get
                 (emacs-command-loop-repeat-last-command
                  :last-command nil
                  :repeat-command 'repeat
                  :callable-p (lambda (_command) t))
                 :status)))
    (should (eq 'empty
                (plist-get
                 (emacs-command-loop-repeat-last-command
                  :last-command 'repeat
                  :repeat-command 'repeat
                  :callable-p (lambda (_command) t))
                 :status)))
    (let ((unbound
           (emacs-command-loop-repeat-last-command
            :last-command 'missing-command
            :callable-p (lambda (_command) nil))))
      (should (eq 'unbound (plist-get unbound :status)))
      (should (eq 'missing-command (plist-get unbound :command))))
    (let ((ok
           (emacs-command-loop-repeat-last-command
            :last-command 'forward-char
            :repeat-command 'repeat
            :callable-p (lambda (_command) t)
            :call-interactively
            (lambda (command)
              (setq called command)
              :called))))
      (should (eq 'ok (plist-get ok :status)))
      (should (eq 'forward-char (plist-get ok :command)))
      (should (eq :called (plist-get ok :value)))
      (should (eq 'forward-char called)))))

(ert-deftest emacs-command-loop-builtins-test/key-source-command-event ()
  (should (= ?a (emacs-command-loop-key-source-command-event ?a)))
  (should (= ?a (emacs-command-loop-key-source-command-event
                 (list :char ?a :name ?b))))
  (should (= ?x (emacs-command-loop-key-source-command-event
                 (list :name ?x))))
  (should-not
   (emacs-command-loop-key-source-command-event (list :name 'backspace)))
  (should-not
   (emacs-command-loop-key-source-command-event (list :char 'left
                                                      :name 'right))))

(ert-deftest emacs-command-loop-builtins-test/key-dispatch-post-command-policy ()
  (should-not
   (emacs-command-loop-key-dispatch-undo-boundary-p
    'self-insert-command))
  (should
   (emacs-command-loop-key-dispatch-undo-boundary-p 'kill-line))
  (should-not
   (emacs-command-loop-key-dispatch-buffer-cache-invalidating-p
    'forward-char))
  (should-not
   (emacs-command-loop-key-dispatch-buffer-cache-invalidating-p
    'custom-motion
    :extra-non-mutating-commands '(custom-motion)))
  (should
   (emacs-command-loop-key-dispatch-buffer-cache-invalidating-p 'yank))
  (should-not
   (emacs-command-loop-key-dispatch-cycle-reset-p
    'move-to-window-line-top-bottom 'move-to-window-line-top-bottom))
  (should
   (emacs-command-loop-key-dispatch-cycle-reset-p
    'forward-char 'move-to-window-line-top-bottom))
  (let ((policy
         (emacs-command-loop-key-dispatch-post-command-policy
          'self-insert-command
          :cycle-command 'move-to-window-line-top-bottom)))
    (should-not (plist-get policy :undo-boundary-p))
    (should (plist-get policy :cycle-reset-p))
    (should (plist-get policy :buffer-cache-invalidating-p)))
  (let ((policy
         (emacs-command-loop-key-dispatch-post-command-policy
          'forward-char
          :cycle-command 'move-to-window-line-top-bottom)))
    (should (plist-get policy :undo-boundary-p))
    (should (plist-get policy :cycle-reset-p))
    (should-not (plist-get policy :buffer-cache-invalidating-p))))

(ert-deftest emacs-command-loop-builtins-test/key-dispatch-plan-prefix-command ()
  (let ((prefix-map (list 'keymap)))
    (let ((plan
           (emacs-command-loop-key-dispatch-plan
            :events [?a]
            :prefix []
            :lookup-single (lambda (_key) prefix-map)
            :lookup-sequence (lambda (_seq) nil)
            :keymap-p (lambda (binding) (eq binding prefix-map)))))
      (should (eq 'prefix (plist-get plan :kind)))
      (should (eq prefix-map (plist-get plan :binding)))
      (should (equal [?a] (plist-get plan :next-prefix))))
    (let ((plan
           (emacs-command-loop-key-dispatch-plan
            :events [?b]
            :prefix [?a]
            :lookup-sequence
            (lambda (seq)
              (and (equal seq [?a ?b]) 'probe-command)))))
      (should (eq 'command (plist-get plan :kind)))
      (should (eq 'probe-command (plist-get plan :binding)))
      (should (equal [] (plist-get plan :next-prefix))))))

(ert-deftest emacs-command-loop-builtins-test/key-dispatch-plan-self-insert-fast ()
  (let ((lookup-called nil))
    (cl-letf (((symbol-function 'self-insert-command) (lambda (&rest _) nil)))
      (let ((plan
             (emacs-command-loop-key-dispatch-plan
              :events [?a]
              :prefix []
              :lookup-sequence (lambda (_seq) (setq lookup-called t))
              :fast-self-insert-p t)))
        (should (eq 'self-insert (plist-get plan :kind)))
        (should (eq 'self-insert-command (plist-get plan :binding)))
        (should-not lookup-called)))))

(ert-deftest emacs-command-loop-builtins-test/key-dispatch-run-plan-prefix ()
  (let (prefix)
    (let ((result
           (emacs-command-loop-key-dispatch-run-plan
            (list :kind 'prefix
                  :binding '(keymap)
                  :next-prefix [?x])
            :set-prefix (lambda (value)
                          (setq prefix value)))))
      (should (eq 'prefix (plist-get result :status)))
      (should (equal [?x] prefix)))))

(ert-deftest emacs-command-loop-builtins-test/key-dispatch-run-plan-self-insert ()
  (emacs-command-loop-builtins-test--with-fresh-state
    (let (prefix last-event after-point command-point)
      (let ((result
             (emacs-command-loop-key-dispatch-run-plan
              (list :kind 'self-insert
                    :binding 'self-insert-command
                    :event ?a
                    :next-prefix [])
              :set-prefix (lambda (value)
                            (setq prefix value))
              :set-last-command-event (lambda (event)
                                        (setq last-event event))
              :run-self-insert (lambda (event _plan)
                                  (should (= ?a event))
                                  :point-after)
              :after-self-insert (lambda (point _plan)
                                   (setq after-point point))
              :after-command (lambda (point _plan)
                               (setq command-point point)))))
        (should (eq 'self-insert (plist-get result :status)))
        (should (equal [] prefix))
        (should (= ?a last-event))
        (should (eq :point-after after-point))
        (should (eq :point-after command-point))))))

(ert-deftest emacs-command-loop-builtins-test/key-dispatch-run-plan-command ()
  (let (prefix last-event inserted after-point fallback-command)
    (let ((result
           (emacs-command-loop-key-dispatch-run-plan
            (list :kind 'command
                  :binding 'self-insert-command
                  :event ?a
                  :next-prefix [])
            :source-event (list :name ?x)
            :set-prefix (lambda (value)
                          (setq prefix value))
            :set-last-command-event (lambda (event)
                                      (setq last-event event))
            :inline-edit-commands '((self-insert-command . self-insert))
            :direct-command-p (lambda (_command) nil)
            :run-self-insert (lambda (event _plan)
                                (setq inserted event)
                                :point-after)
            :after-self-insert (lambda (point _plan)
                                 (setq after-point point))
            :command-execute (lambda (command)
                               (setq fallback-command command)))))
      (should (eq 'command (plist-get result :status)))
      (should (eq 'self-insert (plist-get result :execution-kind)))
      (should (equal [] prefix))
      (should (= ?x last-event))
      (should (= ?a inserted))
      (should (eq :point-after after-point))
      (should-not fallback-command))))

(ert-deftest emacs-command-loop-builtins-test/key-dispatch-run-plan-inline-kind ()
  (let (prefix inline after-inline after-command)
    (let ((result
           (emacs-command-loop-key-dispatch-run-plan
            (list :kind 'command
                  :binding 'kill-line
                  :event ?k
                  :next-prefix [])
            :set-prefix (lambda (value)
                          (setq prefix value))
            :inline-edit-commands '((kill-line . kill-line))
            :direct-command-p (lambda (_command) nil)
            :run-inline-kind
            (lambda (kind event plan)
              (setq inline (list kind event (plist-get plan :binding)))
              :point-after-inline)
            :after-inline-kind
            (lambda (kind point plan)
              (setq after-inline
                    (list kind point (plist-get plan :binding))))
            :after-command
            (lambda (point plan)
              (setq after-command
                    (list point (plist-get plan :binding)))))))
      (should (eq 'command (plist-get result :status)))
      (should (eq 'kill-line (plist-get result :execution-kind)))
      (should (equal [] prefix))
      (should (equal '(kill-line ?k kill-line) inline))
      (should (equal '(kill-line :point-after-inline kill-line)
                     after-inline))
      (should (equal '(:point-after-inline kill-line) after-command)))))

(ert-deftest emacs-command-loop-builtins-test/key-dispatch-run-plan-direct-callback ()
  (let (direct after-direct error)
    (let ((result
           (emacs-command-loop-key-dispatch-run-plan
            (list :kind 'command
                  :binding 'custom-command
                  :event ?c
                  :next-prefix [])
            :direct-command-p (lambda (_command) t)
            :run-direct-command
            (lambda (command plan)
              (setq direct (list command (plist-get plan :event)))
              (list :command command :ok t :value :direct-value))
            :after-direct-command
            (lambda (command dispatch plan)
              (setq after-direct
                    (list command
                          (plist-get dispatch :value)
                          (plist-get plan :binding)))))))
      (should (eq 'command (plist-get result :status)))
      (should (eq 'direct-funcall (plist-get result :execution-kind)))
      (should (equal '(custom-command ?c) direct))
      (should (equal '(custom-command :direct-value custom-command)
                     after-direct)))
    (let ((result
           (emacs-command-loop-key-dispatch-run-plan
            (list :kind 'command
                  :binding 'broken-command
                  :event ?b
                  :next-prefix [])
            :direct-command-p (lambda (_command) t)
            :run-direct-command
            (lambda (command _plan)
              (list :command command :ok nil :message "broken"))
            :on-direct-error
            (lambda (command dispatch)
              (setq error (list command (plist-get dispatch :message)))))))
      (should (eq 'direct-error (plist-get result :status)))
      (should (equal '(broken-command "broken") error)))))

(ert-deftest emacs-command-loop-builtins-test/gui-dispatch-key-self-insert ()
  (emacs-command-loop-builtins-test--with-fresh-state
    (let (commands args)
      (emacs-command-loop-gui-register-backend
       :set-command (lambda (command) (push command commands))
       :set-arg (lambda (arg) (push arg args))
       :lookup-key-sequence (lambda () "")
       :commandp (lambda (_command) t)
       :prefix-arg-empty-p (lambda () t)
       :call-command (lambda (_command) :called))
      (emacs-command-loop-gui-set-context :keys "a" :arg "")
      (should (eq :called (emacs-command-loop-gui-dispatch-key-sequence)))
      (should (eq 'self-insert-command emacs-command-loop-gui-command))
      (should (equal "a" (car args)))
      (should (memq 'self-insert-command commands)))))

(ert-deftest emacs-command-loop-builtins-test/gui-dispatch-key-keeps-normal-arg ()
  (emacs-command-loop-builtins-test--with-fresh-state
    (let (args called)
      (emacs-command-loop-gui-register-backend
       :set-arg (lambda (arg) (push arg args))
       :lookup-key-sequence (lambda () "forward-char")
       :commandp (lambda (_command) t)
       :prefix-arg-empty-p (lambda () t)
       :call-command (lambda (command)
                       (setq called command)
                       :called))
      (emacs-command-loop-gui-set-context :keys "C-f" :arg "stale")
      (setq args nil)
      (should (eq :called (emacs-command-loop-gui-dispatch-key-sequence)))
      (should (eq 'forward-char called))
      (should-not args))))

(ert-deftest emacs-command-loop-builtins-test/gui-dispatch-key-unsupported ()
  (emacs-command-loop-builtins-test--with-fresh-state
    (let (statuses)
      (emacs-command-loop-gui-register-backend
       :lookup-key-sequence (lambda () "")
       :set-status (lambda (status) (push status statuses)))
      (emacs-command-loop-gui-set-context :keys "C-x C-z" :arg "")
      (should-not (emacs-command-loop-gui-dispatch-key-sequence))
      (should (string-equal "C-x C-z"
                            emacs-command-loop-gui-effective-command))
      (should (equal '("unsupported") statuses)))))

(ert-deftest emacs-command-loop-builtins-test/gui-run-request-direct-and-key ()
  (emacs-command-loop-builtins-test--with-fresh-state
    (let (called after-key)
      (emacs-command-loop-gui-register-backend
       :commandp (lambda (_command) t)
       :prefix-arg-empty-p (lambda () t)
       :lookup-key-sequence (lambda () "probe-command")
       :call-command (lambda (command)
                       (setq called command)
                       :called)
       :after-key-dispatch (lambda ()
                             (setq after-key t)))
      (emacs-command-loop-gui-set-context
       :command 'direct-command
       :effective-command "direct-command"
       :keys ""
       :arg ""
       :status "ok"
       :prefix-arg "")
      (should (eq 'direct (emacs-command-loop-gui-run-request)))
      (should (eq 'direct-command called))
      (should-not after-key)
      (setq called nil)
      (emacs-command-loop-gui-set-context
       :command 'direct-command
       :effective-command "direct-command"
       :keys "stale-key"
       :arg ""
       :status "ok"
       :prefix-arg "")
      (should (eq 'direct (emacs-command-loop-gui-run-request)))
      (should (eq 'direct-command called))
      (should-not after-key)
      (setq called nil)
      (emacs-command-loop-gui-set-context
       :command nil
       :effective-command ""
       :keys "C-c p"
       :arg ""
       :status "ok"
       :prefix-arg "")
      (should (eq 'key (emacs-command-loop-gui-run-request)))
      (should (eq 'probe-command called))
      (should after-key))))

(ert-deftest emacs-command-loop-builtins-test/gui-dispatch-key-request-runs-after-key ()
  (emacs-command-loop-builtins-test--with-fresh-state
    (let (called after-key)
      (emacs-command-loop-gui-register-backend
       :lookup-key-sequence (lambda () "probe-command")
       :commandp (lambda (_command) t)
       :prefix-arg-empty-p (lambda () t)
       :call-command (lambda (command)
                       (setq called command)
                       :called)
       :after-key-dispatch (lambda ()
                             (setq after-key t)))
      (emacs-command-loop-gui-set-context
       :command nil
       :effective-command ""
       :keys "C-c p"
       :arg ""
       :status "ok"
       :prefix-arg "")
      (should (eq :called
                  (emacs-command-loop-gui-dispatch-key-request)))
      (should (eq 'probe-command called))
      (should after-key))))

(ert-deftest emacs-command-loop-builtins-test/gui-replay-key-lines ()
  (emacs-command-loop-builtins-test--with-fresh-state
    (let (keys)
      (should (= 3
                 (emacs-command-loop-gui-replay-key-lines
                  "C-f\n\nC-b\na"
                  (lambda (key)
                    (push key keys)))))
      (should (equal '("a" "C-b" "C-f") keys)))))

(ert-deftest emacs-command-loop-builtins-test/gui-writeback-command-name ()
  (emacs-command-loop-builtins-test--with-fresh-state
    (should (equal "forward-char"
                   (emacs-command-loop-gui-writeback-command-name
                    'forward-char "forward-char")))
    (should (equal "project-query-replace-regexp"
                   (emacs-command-loop-gui-writeback-command-name
                    'project-query-replace-regexp "minibuffer")))
    (should (equal "self-insert-command"
                   (emacs-command-loop-gui-writeback-command-name
                    nil 'self-insert-command)))
    (should (equal "save-buffer"
                   (emacs-command-loop-gui-writeback-command-name
                    'save-buffer nil)))
    (should (equal ""
                   (emacs-command-loop-gui-writeback-command-name
                    nil nil)))))

(ert-deftest emacs-command-loop-builtins-test/gui-write-post-command-state ()
  (emacs-command-loop-builtins-test--with-fresh-state
    (let (calls statuses)
      (emacs-command-loop-gui-register-backend
       :clear-display-prefix-after-command
       (lambda () (push 'clear-display-prefix calls))
       :write-minibuffer-state (lambda () (push 'minibuffer calls))
       :write-redisplay-state (lambda () (push 'redisplay calls))
       :write-prefix-arg-state (lambda () (push 'prefix calls))
       :write-kmacro-state (lambda () (push 'kmacro calls))
       :write-last-command-state (lambda () (push 'last-command calls))
       :write-kill-ring-state (lambda () (push 'kill-ring calls))
       :write-window-split-delta (lambda () (push 'window-split calls))
       :write-window-dedicated-state (lambda () (push 'window-dedicated calls))
       :write-side-windows-state (lambda () (push 'side-windows calls))
       :write-frame-state (lambda () (push 'frame calls))
       :set-status (lambda (status) (push status statuses)))
      (let ((state
             (emacs-command-loop-gui-write-post-command-state
              'project-query-replace-regexp "minibuffer" "prefix-arg")))
        (should (equal "project-query-replace-regexp"
                       (plist-get state :command-name)))
        (should (eq 'prefix-arg (plist-get state :lane)))
        (should (equal '(clear-display-prefix minibuffer redisplay prefix
                         kmacro last-command kill-ring window-split
                         window-dedicated side-windows frame)
                       (nreverse calls)))
        (should-not statuses)))))

(ert-deftest emacs-command-loop-builtins-test/gui-lane-writeback-spec ()
  (should (equal '(:status t :buffer t :read-only-one t
                   :point t :mark t :window-start t :written t)
                 (emacs-command-loop-gui-lane-writeback-spec
                  'read-only)))
  (should (equal '(:status t :minibuffer t :buffer t
                   :point t :mark t :window-start t :written t)
                 (emacs-command-loop-gui-lane-writeback-spec
                  "minibuffer")))
  (should (equal '(:status t :point t :mark t :window-start t
                   :prefix-arg t :written t)
                 (emacs-command-loop-gui-lane-writeback-spec
                  'prefix-arg)))
  (should-not (emacs-command-loop-gui-lane-writeback-spec
               'normal)))

(ert-deftest emacs-command-loop-builtins-test/gui-writeback-spec-flag ()
  (let ((spec (emacs-command-loop-gui-lane-writeback-spec 'prefix-arg)))
    (should (emacs-command-loop-gui-writeback-spec-flag spec :status))
    (should (emacs-command-loop-gui-writeback-spec-flag spec :prefix-arg))
    (should-not (emacs-command-loop-gui-writeback-spec-flag spec :buffer))
    (should-not (emacs-command-loop-gui-writeback-spec-flag nil :status))))

(ert-deftest emacs-command-loop-builtins-test/gui-write-lane-state ()
  (emacs-command-loop-builtins-test--with-fresh-state
    (let (calls)
      (emacs-command-loop-gui-register-backend
       :write-status-state (lambda () (push 'status calls))
       :write-minibuffer-state (lambda () (push 'minibuffer calls))
       :write-buffer-state (lambda () (push 'buffer calls))
       :write-file-state (lambda () (push 'file calls))
       :write-read-only-one-state (lambda () (push 'read-only-one calls))
       :write-read-only-state (lambda () (push 'read-only calls))
       :write-point-state (lambda () (push 'point calls))
       :write-mark-state (lambda () (push 'mark calls))
       :write-window-start-state (lambda () (push 'window-start calls))
       :write-prefix-arg-state (lambda () (push 'prefix calls))
       :mark-written-state (lambda () (push 'written calls)))
      (should (emacs-command-loop-gui-write-lane-state 'read-only))
      (should (equal '(status buffer read-only-one point mark
                       window-start written)
                     (nreverse calls)))
      (setq calls nil)
      (should (emacs-command-loop-gui-write-lane-state "prefix-arg"))
      (should (equal '(status point mark window-start prefix written)
                     (nreverse calls)))
      (should-not (emacs-command-loop-gui-write-lane-state 'normal)))))

(ert-deftest emacs-command-loop-builtins-test/gui-apply-post-command-writeback ()
  (emacs-command-loop-builtins-test--with-fresh-state
    (let (calls)
      (emacs-command-loop-gui-register-backend
       :write-minibuffer-state (lambda () (push 'post-minibuffer calls))
       :write-redisplay-state (lambda () (push 'redisplay calls))
       :write-prefix-arg-state (lambda () (push 'prefix calls))
       :write-status-state (lambda () (push 'status calls))
       :write-point-state (lambda () (push 'point calls))
       :write-mark-state (lambda () (push 'mark calls))
       :write-window-start-state (lambda () (push 'window-start calls))
       :mark-written-state (lambda () (push 'written calls)))
      (let ((state
             (emacs-command-loop-gui-apply-post-command-writeback
              'project-query-replace-regexp "minibuffer" "prefix-arg")))
        (should (equal "" (plist-get state :command-name)))
        (should (eq 'normal (plist-get state :lane)))
        (should (equal "normal" (plist-get state :lane-name)))
        (should (plist-get state :lane-written-p))
        (should (equal '(post-minibuffer redisplay prefix status point
                         mark window-start prefix written)
                       (nreverse calls)))))
    (let ((state
           (emacs-command-loop-gui-apply-post-command-writeback
            'forward-char "forward-char" "ok")))
      (should (equal "forward-char" (plist-get state :command-name)))
      (should (eq 'normal (plist-get state :lane)))
      (should (equal "normal" (plist-get state :lane-name)))
      (should-not (plist-get state :lane-written-p)))))

(ert-deftest emacs-command-loop-builtins-test/gui-minibuffer-active-runtime-handle ()
  (emacs-command-loop-builtins-test--with-fresh-state
    (let (handled legacy-called)
      (emacs-command-loop-gui-register-backend
       :minibuffer-active-p (lambda () t)
       :minibuffer-key (lambda () "x")
       :minibuffer-purpose (lambda () "switch-to-buffer")
       :minibuffer-handle-key (lambda () (setq legacy-called t)))
      (cl-letf (((symbol-function 'emacs-minibuffer-gui-handle-key)
                 (lambda (key purpose)
                   (setq handled (list key purpose))
                   :runtime-handled)))
        (should (eq :runtime-handled
                    (emacs-command-loop-gui-dispatch-key-sequence)))
        (should (equal '("x" "switch-to-buffer") handled))
        (should-not legacy-called)))))

(ert-deftest emacs-command-loop-builtins-test/gui-minibuffer-active-current-context ()
  (emacs-command-loop-builtins-test--with-fresh-state
    (let (handled legacy-called)
      (emacs-command-loop-gui-register-backend
       :minibuffer-active-p (lambda () t)
       :minibuffer-handle-key (lambda () (setq legacy-called t)))
      (cl-letf (((symbol-function 'emacs-minibuffer-gui-handle-key-current-context)
                 (lambda ()
                   (setq handled t)
                   :current-context)))
        (should (eq :current-context
                    (emacs-command-loop-gui-dispatch-key-sequence)))
        (should handled)
        (should-not legacy-called)))))

(ert-deftest emacs-command-loop-builtins-test/gui-minibuffer-start-runtime-keymaps ()
  (emacs-command-loop-builtins-test--with-fresh-state
    (let (start-args legacy-called)
      (emacs-command-loop-gui-register-backend
       :minibuffer-mode-keymap-source
       (lambda () "C-c x\tmode-command\tMode: \n")
       :minibuffer-keymap-source
       (lambda () "C-c x\tglobal-command\tGlobal: \n")
       :minibuffer-key (lambda () "C-c x")
       :minibuffer-initial-input (lambda () "seed")
       :maybe-start-minibuffer (lambda () (setq legacy-called t)))
      (cl-letf (((symbol-function 'emacs-minibuffer-gui-maybe-start-from-keymaps)
                 (lambda (mode-source global-source key initial-input)
                   (setq start-args
                         (list mode-source global-source key initial-input))
                   t)))
        (should (emacs-command-loop-gui-maybe-start-minibuffer))
        (should (equal (list "C-c x\tmode-command\tMode: \n"
                             "C-c x\tglobal-command\tGlobal: \n"
                             "C-c x"
                             "seed")
                       start-args))
        (should-not legacy-called)))))

(ert-deftest emacs-command-loop-builtins-test/gui-minibuffer-start-current-context ()
  (emacs-command-loop-builtins-test--with-fresh-state
    (let (started legacy-called)
      (emacs-command-loop-gui-register-backend
       :maybe-start-minibuffer (lambda () (setq legacy-called t)))
      (cl-letf (((symbol-function 'emacs-minibuffer-gui-maybe-start-current-context)
                 (lambda ()
                   (setq started t)
                   t)))
        (should (emacs-command-loop-gui-maybe-start-minibuffer))
        (should started)
        (should-not legacy-called)))))

(ert-deftest emacs-command-loop-builtins-test/gui-dispatch-context ()
  (emacs-command-loop-builtins-test--with-fresh-state
    (let (called)
      (emacs-command-loop-gui-register-backend
       :lookup-key-sequence (lambda () "forward-char")
       :commandp (lambda (_command) t)
       :prefix-arg-empty-p (lambda () t)
       :call-command (lambda (command) (setq called command) :called))
      (should (eq :called
                  (emacs-command-loop-gui-dispatch-context
                   :keys "C-f"
                   :arg ""
                   :status "ok")))
      (should (eq 'forward-char called))
      (should (eq 'forward-char emacs-command-loop-gui-command))
      (should (string-equal "forward-char"
                            emacs-command-loop-gui-effective-command)))))

(ert-deftest emacs-command-loop-builtins-test/gui-execute-extended-command ()
  (emacs-command-loop-builtins-test--with-fresh-state
    (let (called args statuses commands)
      (emacs-command-loop-gui-set-context
       :command 'execute-extended-command
       :effective-command "execute-extended-command"
       :arg "goto-line"
       :status "ok")
      (emacs-command-loop-gui-register-backend
       :current-minibuffer-arg (lambda () "42")
       :commandp (lambda (_command) t)
       :prefix-arg-empty-p (lambda () t)
       :set-command (lambda (command) (push command commands))
       :set-status (lambda (status) (push status statuses))
       :call-command
       (lambda (command)
         (setq called command
               args (list emacs-command-loop-gui-arg))
         :called))
      (should (eq :called
                  (emacs-command-loop-gui-execute-extended-command)))
      (should (eq 'goto-line called))
      (should (equal '("42") args))
      (should (string-equal "goto-line"
                            emacs-command-loop-gui-effective-command))
      (should (eq 'execute-extended-command
                  emacs-command-loop-gui-command))
      (should (memq 'goto-line commands))
      (should (memq 'execute-extended-command commands))
      (should-not statuses))))

(ert-deftest emacs-command-loop-builtins-test/gui-execute-extended-command-empty ()
  (emacs-command-loop-builtins-test--with-fresh-state
    (let (statuses called)
      (emacs-command-loop-gui-set-context
       :command 'execute-extended-command
       :arg ""
       :status "ok")
      (emacs-command-loop-gui-register-backend
       :set-status (lambda (status) (push status statuses))
       :call-command (lambda (_command) (setq called t)))
      (should-not (emacs-command-loop-gui-execute-extended-command))
      (should-not called)
      (should (equal '("unsupported") statuses))
      (should (string-equal "unsupported"
                            emacs-command-loop-gui-status)))))

(ert-deftest emacs-command-loop-builtins-test/gui-project-command ()
  (emacs-command-loop-builtins-test--with-fresh-state
    (let (called args commands)
      (emacs-command-loop-gui-set-context
       :command 'project-execute-extended-command
       :effective-command "project-execute-extended-command"
       :arg "project-dired"
       :status "ok")
      (emacs-command-loop-gui-register-backend
       :current-minibuffer-arg (lambda () "nested")
       :commandp (lambda (_command) t)
       :prefix-arg-empty-p (lambda () t)
       :set-command (lambda (command) (push command commands))
       :call-command
       (lambda (command)
         (setq called command
               args (list emacs-command-loop-gui-arg))
         :called))
      (should (eq :called
                  (emacs-command-loop-gui-project-command
                   nil nil 'project-execute-extended-command)))
      (should (eq 'project-dired called))
      (should (equal '("nested") args))
      (should (string-equal "project-dired"
                            emacs-command-loop-gui-effective-command))
      (should (eq 'project-execute-extended-command
                  emacs-command-loop-gui-command))
      (should (memq 'project-dired commands))
      (should (memq 'project-execute-extended-command commands)))))

(ert-deftest emacs-command-loop-builtins-test/gui-project-command-defaults ()
  (emacs-command-loop-builtins-test--with-fresh-state
    (let (called)
      (emacs-command-loop-gui-set-context
       :command 'project-any-command
       :arg ""
       :status "ok")
      (emacs-command-loop-gui-register-backend
       :current-minibuffer-arg (lambda () "")
       :commandp (lambda (_command) t)
       :prefix-arg-empty-p (lambda () t)
       :call-command (lambda (command) (setq called command) :called))
      (should (eq :called
                  (emacs-command-loop-gui-project-command)))
      (should (eq 'project-dired called))
      (should (string-equal "project-dired"
                            emacs-command-loop-gui-effective-command))
      (should (eq 'project-any-command
                  emacs-command-loop-gui-command)))))

;;;; O. Phase B.4 — command-loop-1 driver

(defvar emacs-command-loop-builtins-test--counter 0)

(defun emacs-command-loop-builtins-test--bump ()
  (interactive)
  (setq emacs-command-loop-builtins-test--counter
        (1+ emacs-command-loop-builtins-test--counter)))

(ert-deftest emacs-command-loop-builtins-test/command-loop-1-drains ()
  (emacs-command-loop-builtins-test--with-fresh-keymaps
    (emacs-keymap-define-key emacs-keymap-global-map "a"
                             'emacs-command-loop-builtins-test--bump)
    (setq emacs-command-loop-builtins-test--counter 0)
    (emacs-command-loop-feed-events ?a ?a ?a)
    (let ((n (emacs-command-loop-1)))
      (should (= 3 n))
      (should (= 3 emacs-command-loop-builtins-test--counter)))))

(ert-deftest emacs-command-loop-builtins-test/command-loop-1-empty-is-noop ()
  (emacs-command-loop-builtins-test--with-fresh-state
    (should (= 0 (emacs-command-loop-1)))))

(ert-deftest emacs-command-loop-builtins-test/pre-and-post-command-hook-fire ()
  (emacs-command-loop-builtins-test--with-fresh-keymaps
    (emacs-keymap-define-key emacs-keymap-global-map "a"
                             'emacs-command-loop-builtins-test--bump)
    (let* ((pre-fired 0)
           (post-fired 0)
           (pre-command-hook
            (list (lambda () (setq pre-fired (1+ pre-fired)))))
           (post-command-hook
            (list (lambda () (setq post-fired (1+ post-fired))))))
      (setq emacs-command-loop-builtins-test--counter 0)
      (emacs-command-loop-feed-events ?a ?a)
      (emacs-command-loop-1)
      (should (= 2 pre-fired))
      (should (= 2 post-fired))
      (should (= 2 emacs-command-loop-builtins-test--counter)))))

(ert-deftest emacs-command-loop-builtins-test/undefined-key-handler-fires ()
  (emacs-command-loop-builtins-test--with-fresh-keymaps
    (let* ((seen nil)
           (emacs-command-loop--undefined-key-handler
            (lambda (vec) (push (aref vec 0) seen))))
      (emacs-command-loop-feed-events ?z ?y)
      (emacs-command-loop-1)
      ;; seen is accumulated via push (= LIFO); reverse to get feed order.
      (should (equal (list ?z ?y) (reverse seen))))))

(ert-deftest emacs-command-loop-builtins-test/recursion-depth-stub-zero ()
  (should (= 0 (emacs-command-loop-recursion-depth))))

;;;; P. Phase B.5 — universal-argument / digit-argument / M-x

(ert-deftest emacs-command-loop-builtins-test/b5-fbound-parity ()
  (dolist (sym '(execute-extended-command universal-argument
                 digit-argument negative-argument prefix-numeric-value))
    (should (fboundp sym))))

(ert-deftest emacs-command-loop-builtins-test/universal-argument-basic ()
  (emacs-command-loop-builtins-test--with-fresh-state
    (emacs-command-loop-call-interactively
     'emacs-command-loop-universal-argument)
    (should (equal '(4) emacs-command-loop--prefix-arg))))

(ert-deftest emacs-command-loop-builtins-test/universal-argument-stacks ()
  (emacs-command-loop-builtins-test--with-fresh-state
    ;; First C-u: nil → '(4)
    (emacs-command-loop-call-interactively
     'emacs-command-loop-universal-argument)
    ;; Second C-u: '(4) → '(16)
    (emacs-command-loop-call-interactively
     'emacs-command-loop-universal-argument)
    (should (equal '(16) emacs-command-loop--prefix-arg))))

(ert-deftest emacs-command-loop-builtins-test/prefix-numeric-value-helper ()
  (should (= 1 (emacs-command-loop--prefix-numeric-value nil)))
  (should (= -1 (emacs-command-loop--prefix-numeric-value '-)))
  (should (= -1 (emacs-command-loop--prefix-numeric-value '(-))))
  (should (= 4 (emacs-command-loop--prefix-numeric-value '(4))))
  (should (= 7 (emacs-command-loop--prefix-numeric-value 7))))

(ert-deftest emacs-command-loop-builtins-test/digit-argument-first-digit ()
  (emacs-command-loop-builtins-test--with-fresh-state
    ;; C-u sets prefix-arg=(4); now press 5.
    (setq emacs-command-loop--prefix-arg '(4)
          emacs-command-loop--last-command-event ?5)
    (emacs-command-loop-call-interactively
     'emacs-command-loop-digit-argument)
    (should (= 5 emacs-command-loop--prefix-arg))))

(ert-deftest emacs-command-loop-builtins-test/digit-argument-continuation ()
  (emacs-command-loop-builtins-test--with-fresh-state
    ;; prefix-arg=5; press 3 → expect 53
    (setq emacs-command-loop--prefix-arg 5
          emacs-command-loop--last-command-event ?3)
    (emacs-command-loop-call-interactively
     'emacs-command-loop-digit-argument)
    (should (= 53 emacs-command-loop--prefix-arg))))

(ert-deftest emacs-command-loop-builtins-test/negative-argument-toggles ()
  (emacs-command-loop-builtins-test--with-fresh-state
    (emacs-command-loop-call-interactively
     'emacs-command-loop-negative-argument)
    (should (eq '- emacs-command-loop--prefix-arg))
    ;; Apply on integer 5 → -5.
    (setq emacs-command-loop--prefix-arg 5)
    (emacs-command-loop-call-interactively
     'emacs-command-loop-negative-argument)
    (should (= -5 emacs-command-loop--prefix-arg))))

(defun emacs-command-loop-builtins-test--echo-prefix (arg)
  "Test fixture: returns the raw incoming prefix arg."
  (interactive "P")
  arg)

(ert-deftest emacs-command-loop-builtins-test/execute-extended-command-direct ()
  (emacs-command-loop-builtins-test--with-fresh-state
    ;; Pass the command name in directly (= bypass minibuffer).
    (let ((r (emacs-command-loop-execute-extended-command
              nil 'emacs-command-loop-builtins-test--echo-prefix)))
      (should (null r)))))

(ert-deftest emacs-command-loop-builtins-test/execute-extended-command-passes-prefix ()
  (emacs-command-loop-builtins-test--with-fresh-state
    (let ((r (emacs-command-loop-execute-extended-command
              '(4) 'emacs-command-loop-builtins-test--echo-prefix)))
      (should (equal '(4) r)))))

(ert-deftest emacs-command-loop-builtins-test/gui-extended-command-candidates ()
  (emacs-command-loop-builtins-test--with-fresh-state
    (let ((candidates (emacs-command-loop-gui-extended-command-candidates)))
      (should (> (length emacs-command-loop-gui-extended-command-candidate-names)
                 300))
      (dolist (name '("find-file" "forward-char" "kill-line"
                      "replace-string" "describe-function" "untabify"))
        (should (string-match-p
                 (concat "\\(?:\\`\\|\n\\)" (regexp-quote name) "\n")
                 candidates))))))

(ert-deftest emacs-command-loop-builtins-test/gui-command-registry ()
  (emacs-command-loop-builtins-test--with-fresh-state
    (should (> (length emacs-command-loop-gui-command-registry-names) 400))
    (dolist (command '(find-file read-only-mode describe-function
                       project-query-replace-regexp forward-char untabify
                       ignore))
      (should (emacs-command-loop-gui-command-registered-p command))
      (should (emacs-command-loop-gui-command-registered-p
               (symbol-name command))))
    ;; `ignore' is the GUI bridge's toolbar menu-open no-op.  It must be
    ;; in the curated registry so the runtime recognition policy accepts
    ;; it directly — that congruence is what lets the bridge drop its
    ;; hand-rolled `commandp' fallback in favour of this runtime path.
    (should (emacs-command-loop-gui-command-registered-p 'ignore))
    (should (emacs-command-loop-gui-command-accepted-p 'ignore))
    ;; The contract the bridge `commandp' relies on: every curated
    ;; registry name is accepted by the shared recognition policy.
    (dolist (name emacs-command-loop-gui-command-registry-names)
      (should (emacs-command-loop-gui-command-accepted-p (intern name))))
    (emacs-command-loop-gui-set-context :command 'find-file)
    (should (emacs-command-loop-gui-command-registered-p))
    (should-not (emacs-command-loop-gui-command-registered-p
                 'nemacs-command-that-does-not-exist))
    (should (emacs-command-loop-gui-command-accepted-p 'find-file))
    (should (emacs-command-loop-gui-command-accepted-p
             'emacs-command-loop-builtins-test--gui-command))
    (should-not (emacs-command-loop-gui-command-registered-p
                 'emacs-command-loop-builtins-test--gui-command))
    (should-not (emacs-command-loop-gui-command-accepted-p
                 'nemacs-command-that-does-not-exist))))

(ert-deftest emacs-command-loop-builtins-test/gui-command-policy-registries ()
  (emacs-command-loop-builtins-test--with-fresh-state
    (should (> (length emacs-command-loop-gui-read-only-command-names) 80))
    (dolist (command '(insert-file save-buffer kill-line replace-string
                       delete-region untabify))
      (should (emacs-command-loop-gui-read-only-command-p command))
      (should (emacs-command-loop-gui-read-only-command-p
               (symbol-name command))))
    (should-not (emacs-command-loop-gui-read-only-command-p 'forward-char))
    (dolist (command '(universal-argument digit-argument negative-argument))
      (should (emacs-command-loop-gui-prefix-command-p command))
      (should (emacs-command-loop-gui-prefix-command-p
               (symbol-name command))))
    (emacs-command-loop-gui-set-context :command 'digit-argument)
    (should (emacs-command-loop-gui-prefix-command-p))
    (should-not (emacs-command-loop-gui-prefix-command-p 'find-file))))

(ert-deftest emacs-command-loop-builtins-test/gui-undo-save-policy ()
  (emacs-command-loop-builtins-test--with-fresh-state
    (should (> (length emacs-command-loop-gui-undo-save-command-names) 60))
    (dolist (command '(self-insert-command kill-line replace-string
                       insert-file untabify))
      (should (emacs-command-loop-gui-undo-save-command-p command))
      (should (emacs-command-loop-gui-undo-save-command-p
               (symbol-name command))))
    (should-not (emacs-command-loop-gui-undo-save-command-p 'forward-char))
    (should-not (emacs-command-loop-gui-undo-save-command-p 'save-buffer))))

(ert-deftest emacs-command-loop-builtins-test/gui-save-undo-if-needed ()
  (emacs-command-loop-builtins-test--with-fresh-state
    (let (saved legacy)
      (emacs-command-loop-gui-register-backend
       :save-undo-state (lambda () (setq saved t) :saved)
       :save-undo-if-needed (lambda () (setq legacy t) :legacy))
      (emacs-command-loop-gui-set-context :command 'kill-line)
      (should (eq :saved (emacs-command-loop-gui-save-undo-if-needed)))
      (should saved)
      (should-not legacy)
      (setq saved nil)
      (emacs-command-loop-gui-set-context :command 'forward-char)
      (should-not (emacs-command-loop-gui-save-undo-if-needed))
      (should-not saved))))

(ert-deftest emacs-command-loop-builtins-test/gui-prefix-repeat-and-invert-policy ()
  (emacs-command-loop-builtins-test--with-fresh-state
    (dolist (command '(forward-char backward-char next-line previous-line
                       delete-char backward-delete-char delete-backward-char
                       self-insert-command))
      (should (emacs-command-loop-gui-prefix-repeat-command-p command))
      (should (emacs-command-loop-gui-prefix-repeat-command-p
               (symbol-name command))))
    (should-not (emacs-command-loop-gui-prefix-repeat-command-p 'find-file))
    (should (eq 'backward-char
                (emacs-command-loop-gui-prefix-inverted-command
                 'forward-char)))
    (should (eq 'forward-char
                (emacs-command-loop-gui-prefix-inverted-command
                 "backward-char")))
    (should (eq 'previous-line
                (emacs-command-loop-gui-prefix-inverted-command
                 'next-line)))
    (should (eq 'delete-backward-char
                (emacs-command-loop-gui-prefix-inverted-command
                 'delete-char)))
    (should (eq 'delete-char
                (emacs-command-loop-gui-prefix-inverted-command
                 'backward-delete-char)))
    (emacs-command-loop-gui-set-context :command 'previous-line)
    (should (eq 'next-line
                (emacs-command-loop-gui-prefix-inverted-command)))
    (should-not (emacs-command-loop-gui-prefix-inverted-command
                 'find-file))))

(ert-deftest emacs-command-loop-builtins-test/gui-prefix-arg-number-policy ()
  (emacs-command-loop-builtins-test--with-fresh-state
    (should (= 1 (emacs-command-loop-gui-prefix-arg-number "")))
    (should (= 4 (emacs-command-loop-gui-prefix-arg-number "4")))
    (should (= 42 (emacs-command-loop-gui-prefix-arg-number "42")))
    (should (= -1 (emacs-command-loop-gui-prefix-arg-number "-")))
    (should (= -9 (emacs-command-loop-gui-prefix-arg-number "-9")))
    (emacs-command-loop-gui-set-context :prefix-arg "-3")
    (should (= -3 (emacs-command-loop-gui-prefix-arg-number)))
    (should (= 3 (emacs-command-loop-gui-prefix-arg-absolute-number)))))

(ert-deftest emacs-command-loop-builtins-test/gui-prefix-digit-key-policy ()
  (emacs-command-loop-builtins-test--with-fresh-state
    (should (string-equal "7"
                          (emacs-command-loop-gui-prefix-digit-key
                           "7" "C-3")))
    (should (string-equal "3"
                          (emacs-command-loop-gui-prefix-digit-key
                           "" "C-3")))
    (should (string-equal ""
                          (emacs-command-loop-gui-prefix-digit-key
                           "" "C--")))))

(ert-deftest emacs-command-loop-builtins-test/gui-prefix-commands-update-state ()
  (emacs-command-loop-builtins-test--with-fresh-state
    (let (prefix-updates statuses)
      (emacs-command-loop-gui-register-backend
       :set-prefix-arg (lambda (arg) (push arg prefix-updates))
       :set-status (lambda (status) (push status statuses)))
      (should (string-equal "4"
                            (emacs-command-loop-gui-universal-argument)))
      (should (string-equal "16"
                            (emacs-command-loop-gui-universal-argument)))
      (emacs-command-loop-gui-set-context :keys "C-3" :arg "")
      (should (string-equal "163"
                            (emacs-command-loop-gui-digit-argument)))
      (should (string-equal "-163"
                            (emacs-command-loop-gui-negative-argument)))
      (should (string-equal "163"
                            (emacs-command-loop-gui-negative-argument)))
      (should (equal '("163" "-163" "163" "16" "4")
                     prefix-updates))
      (should (equal '("prefix-arg" "prefix-arg" "prefix-arg"
                       "prefix-arg" "prefix-arg")
                     statuses)))))

(ert-deftest emacs-command-loop-builtins-test/gui-execute-with-prefix-arg-repeat ()
  (emacs-command-loop-builtins-test--with-fresh-state
    (let (calls prefix-updates)
      (emacs-command-loop-gui-register-backend
       :set-command (lambda (command) (push (list :set command) calls))
       :set-effective-command (lambda (name) (push (list :effective name)
                                                   calls))
       :set-prefix-arg (lambda (arg) (push arg prefix-updates))
       :call-command (lambda (command) (push (list :call command) calls)
                       nil))
      (emacs-command-loop-gui-set-context
       :command 'forward-char
       :effective-command "forward-char"
       :prefix-arg "3")
      (should-not (emacs-command-loop-gui-execute-with-prefix-arg))
      (should (= 3 (length (cl-remove-if-not
                            (lambda (item)
                              (equal item '(:call forward-char)))
                            calls))))
      (should (equal "" emacs-command-loop-gui-prefix-arg))
      (should (equal '("" "3") prefix-updates)))))

(ert-deftest emacs-command-loop-builtins-test/gui-execute-with-prefix-arg-inverts-negative ()
  (emacs-command-loop-builtins-test--with-fresh-state
    (let (called effective prefix-updates)
      (emacs-command-loop-gui-register-backend
       :set-effective-command (lambda (name) (setq effective name))
       :set-prefix-arg (lambda (arg) (push arg prefix-updates))
       :call-command (lambda (command) (push command called) nil))
      (emacs-command-loop-gui-set-context
       :command 'forward-char
       :effective-command "forward-char"
       :prefix-arg "-2")
      (should-not (emacs-command-loop-gui-execute-with-prefix-arg))
      (should (equal '(backward-char backward-char) called))
      (should (eq 'backward-char emacs-command-loop-gui-command))
      (should (string-equal "backward-char" effective))
      (should (equal '("" "-2") prefix-updates)))))

(ert-deftest emacs-command-loop-builtins-test/gui-command-execute-runtime-policies ()
  (emacs-command-loop-builtins-test--with-fresh-state
    (let (statuses called)
      (emacs-command-loop-gui-register-backend
       :commandp (lambda (_command) t)
       :read-only-p (lambda () t)
       :call-command (lambda (_command) (setq called t))
       :set-status (lambda (status) (push status statuses)))
      (emacs-command-loop-gui-set-context :command 'kill-line)
      (should-not (emacs-command-loop-gui-command-execute))
      (should-not called)
      (should (equal '("read-only") statuses)))
    (let (called)
      (emacs-command-loop-gui-register-backend
       :commandp (lambda (_command) t)
       :read-only-p (lambda () nil)
       :call-command (lambda (command) (setq called command))
       :prefix-arg-empty-p (lambda () t))
      (emacs-command-loop-gui-set-context :command 'digit-argument)
      (emacs-command-loop-gui-command-execute)
      (should (eq 'digit-argument called)))))

(ert-deftest emacs-command-loop-builtins-test/gui-command-execute-no-prefix-backend ()
  (emacs-command-loop-builtins-test--with-fresh-state
    (let (called prefix-called)
      (emacs-command-loop-gui-register-backend
       :commandp (lambda (_command) t)
       :read-only-p (lambda () nil)
       :call-command (lambda (command) (setq called command))
       :execute-with-prefix-arg (lambda () (setq prefix-called t)))
      (emacs-command-loop-gui-set-context :command 'forward-char)
      (emacs-command-loop-gui-command-execute)
      (should (eq 'forward-char called))
      (should-not prefix-called))))

(ert-deftest emacs-command-loop-builtins-test/gui-dispatch-current-context ()
  (emacs-command-loop-builtins-test--with-fresh-state
    (let (called effective commands)
      (emacs-command-loop-gui-register-backend
       :current-command (lambda () nil)
       :current-effective-command (lambda () "")
       :current-keys (lambda () "C-c p")
       :current-arg (lambda () "")
       :current-status (lambda () "ok")
       :current-prefix-arg (lambda () "")
       :lookup-key-sequence (lambda () "probe-command")
       :commandp (lambda (_command) t)
       :read-only-p (lambda () nil)
       :prefix-arg-empty-p (lambda () t)
       :set-effective-command (lambda (name) (setq effective name))
       :set-command (lambda (command) (push command commands))
       :call-command (lambda (command) (setq called command)))
      (emacs-command-loop-gui-dispatch-current-context)
      (should (eq 'probe-command called))
      (should (equal "probe-command" effective))
      (should (member 'probe-command commands))
      (should (equal "C-c p" emacs-command-loop-gui-keys)))))

(ert-deftest emacs-command-loop-builtins-test/c-u-then-digit-then-cmd-end-to-end ()
  (emacs-command-loop-builtins-test--with-fresh-state
    ;; Simulate the C-u 5 cmd sequence by direct calls.
    (emacs-command-loop-call-interactively
     'emacs-command-loop-universal-argument)
    (should (equal '(4) emacs-command-loop--prefix-arg))
    (setq emacs-command-loop--last-command-event ?5)
    (emacs-command-loop-call-interactively
     'emacs-command-loop-digit-argument)
    (should (= 5 emacs-command-loop--prefix-arg))
    (let ((r (emacs-command-loop-call-interactively
              'emacs-command-loop-builtins-test--echo-prefix)))
      (should (= 5 r)))))

;;;; Q. Phase B.6 — keyboard-quit / recursive-edit

(ert-deftest emacs-command-loop-builtins-test/b6-fbound-parity ()
  (dolist (sym '(keyboard-quit exit-recursive-edit))
    (should (fboundp sym))))

(ert-deftest emacs-command-loop-builtins-test/keyboard-quit-signals-quit ()
  (emacs-command-loop-builtins-test--with-fresh-state
    (emacs-command-loop-builtins-test--should-quit
      (emacs-command-loop-keyboard-quit))))

(ert-deftest emacs-command-loop-builtins-test/command-loop-1-swallows-quit ()
  (emacs-command-loop-builtins-test--with-fresh-keymaps
    (emacs-keymap-define-key emacs-keymap-global-map "q"
                             'emacs-command-loop-keyboard-quit)
    (emacs-keymap-define-key emacs-keymap-global-map "a"
                             'emacs-command-loop-builtins-test--bump)
    (setq emacs-command-loop-builtins-test--counter 0)
    ;; Sequence: q (= signal quit, swallowed) then a (= bump).
    (emacs-command-loop-feed-events ?q ?a)
    (let ((n (emacs-command-loop-1)))
      (should (= 2 n))
      (should (= 1 emacs-command-loop-builtins-test--counter)))))

(ert-deftest emacs-command-loop-builtins-test/recursive-edit-tracks-depth ()
  (emacs-command-loop-builtins-test--with-fresh-state
    (should (= 0 (emacs-command-loop-recursion-depth)))
    ;; Empty queue → recursive-edit returns immediately.
    (let ((r (emacs-command-loop-recursive-edit)))
      (should (null r)))
    (should (= 0 (emacs-command-loop-recursion-depth)))))

(ert-deftest emacs-command-loop-builtins-test/recursive-edit-depth-during ()
  (emacs-command-loop-builtins-test--with-fresh-keymaps
    ;; A command that records the depth IT sees.
    (let* ((seen-depth nil)
           (probe (lambda ()
                    (interactive)
                    (setq seen-depth
                          (emacs-command-loop-recursion-depth)))))
      (emacs-keymap-define-key emacs-keymap-global-map "a" probe)
      (emacs-command-loop-feed-events ?a)
      (emacs-command-loop-recursive-edit)
      (should (= 1 seen-depth))
      (should (= 0 (emacs-command-loop-recursion-depth))))))

(ert-deftest emacs-command-loop-builtins-test/exit-recursive-edit-throws ()
  (emacs-command-loop-builtins-test--with-fresh-keymaps
    ;; Inner command that exits the recursive-edit.
    (emacs-keymap-define-key emacs-keymap-global-map "x"
                             'emacs-command-loop-exit-recursive-edit)
    (emacs-keymap-define-key emacs-keymap-global-map "a"
                             'emacs-command-loop-builtins-test--bump)
    (setq emacs-command-loop-builtins-test--counter 0)
    ;; Feed: a x a — the third 'a' should NOT execute because 'x' threw out.
    (emacs-command-loop-feed-events ?a ?x ?a)
    (emacs-command-loop-recursive-edit)
    (should (= 1 emacs-command-loop-builtins-test--counter))))

(ert-deftest emacs-command-loop-builtins-test/exit-recursive-edit-without-frame-errors ()
  (emacs-command-loop-builtins-test--with-fresh-state
    (should-error (emacs-command-loop-exit-recursive-edit)
                  :type 'emacs-command-loop-error)))

(ert-deftest emacs-command-loop-builtins-test/abort-recursive-edit-without-frame-quits ()
  (emacs-command-loop-builtins-test--with-fresh-state
    (emacs-command-loop-builtins-test--should-quit
      (emacs-command-loop-abort-recursive-edit))))

(ert-deftest emacs-command-loop-builtins-test/abort-recursive-edit-throws-aborted ()
  (emacs-command-loop-builtins-test--with-fresh-keymaps
    (emacs-keymap-define-key emacs-keymap-global-map "x"
                             'emacs-command-loop-abort-recursive-edit)
    (emacs-command-loop-feed-events ?x)
    (let ((r (emacs-command-loop-recursive-edit)))
      (should (eq 'aborted r)))))

(ert-deftest emacs-command-loop-builtins-test/needs-review-data-surfaces ()
  (emacs-command-loop-builtins-test--with-fresh-state
    (should-not emacs-command-loop-gui-backend)
    (should (assq 13 emacs-command-loop-basic-edit-key-bindings))
    (should (assq ?\C-f emacs-command-loop-c-x-prefix-key-bindings))
    (should (assq ?k emacs-command-loop-help-prefix-key-bindings))
    (should (assq 'self-insert
                  emacs-command-loop-key-dispatch-inline-command-alist))
    (let ((emacs-command-loop-command-feature-hints
           '((sample-command . sample-feature))))
      (should (assq 'sample-command
                    emacs-command-loop-command-feature-hints)))
    (should (memq 'forward-char
                  emacs-command-loop-key-dispatch-non-mutating-commands))
    (should (member "ignore"
                    emacs-command-loop-gui-benign-status-command-names))
    (should (member "digit-argument"
                    emacs-command-loop-gui-prefix-command-names))
    (should (member "forward-char"
                    emacs-command-loop-gui-prefix-repeat-command-names))
    (should (assq 'forward-char
                  emacs-command-loop-gui-prefix-invert-command-alist))
    (should (assq 'goto-char
                  emacs-command-loop-gui-adapted-command-alist))))

(ert-deftest emacs-command-loop-builtins-test/needs-review-small-helpers ()
  (emacs-command-loop-builtins-test--with-fresh-state
    (should (emacs-command-loop-keymap-binding-p (list 'keymap)))
    (should-not (emacs-command-loop-keymap-binding-p 'forward-char))
    (should (string-match-p
             "boom"
             (emacs-command-loop-key-dispatch-error-message
              '(error "boom"))))
    (cl-letf (((symbol-function 'self-insert-command)
               (lambda (&rest _args) nil)))
      (should (emacs-command-loop-printable-self-insert-p
               'self-insert-command ?a))
      (should-not (emacs-command-loop-printable-self-insert-p
                   'self-insert-command 9))
      (should-not (emacs-command-loop-printable-self-insert-p
                   'forward-char ?a)))
    (should (string-equal "42"
                          (emacs-command-loop-gui-prefix-number-string
                           42)))
    (emacs-command-loop-gui-set-context
     :command 'forward-char
     :effective-command "forward-char"
     :prefix-arg "-2")
    (should (emacs-command-loop-gui-invert-prefix-command-if-needed))
    (should (eq 'backward-char emacs-command-loop-gui-command))
    (should (string-equal "backward-char"
                          emacs-command-loop-gui-effective-command))
    (emacs-command-loop-gui-set-context
     :command 'find-file
     :effective-command "find-file"
     :prefix-arg "-2")
    (should-not (emacs-command-loop-gui-invert-prefix-command-if-needed))
    (should (eq 'find-file emacs-command-loop-gui-command))))

(ert-deftest emacs-command-loop-builtins-test/needs-review-current-context-wrappers ()
  (emacs-command-loop-builtins-test--with-fresh-state
    (let (called after-key)
      (emacs-command-loop-gui-register-backend
       :lookup-key-sequence (lambda () "probe-command")
       :commandp (lambda (_command) t)
       :read-only-p (lambda () nil)
       :prefix-arg-empty-p (lambda () t)
       :call-command (lambda (command) (setq called command)
                       :called)
       :after-key-dispatch (lambda () (setq after-key t)))
      (should (eq :called
                  (emacs-command-loop-gui-dispatch-key-request-context
                   :keys "C-c p")))
      (should (eq 'probe-command called))
      (should after-key)))
  (emacs-command-loop-builtins-test--with-fresh-state
    (let (called)
      (emacs-command-loop-gui-register-backend
       :commandp (lambda (_command) t)
       :read-only-p (lambda () nil)
       :prefix-arg-empty-p (lambda () t)
       :call-command (lambda (command) (setq called command)
                       :direct-called))
      (should (eq 'direct
                  (emacs-command-loop-gui-run-request-context
                   :command 'direct-command)))
      (should (eq 'direct-command called))))
  (emacs-command-loop-builtins-test--with-fresh-state
    (emacs-command-loop-gui-register-backend
     :current-command (lambda () 'project-query-replace-regexp)
     :current-effective-command (lambda () "minibuffer"))
    (should (string-equal
             "project-query-replace-regexp"
             (emacs-command-loop-gui-writeback-command-name-current-context))))
  (emacs-command-loop-builtins-test--with-fresh-state
    (let (callbacks)
      (emacs-command-loop-gui-register-backend
       :current-command (lambda () 'project-query-replace-regexp)
       :current-effective-command (lambda () "minibuffer")
       :current-status (lambda () "prefix-arg")
       :clear-display-prefix-after-command
       (lambda () (push :clear callbacks))
       :write-minibuffer-state
       (lambda () (push :minibuffer callbacks)))
      (let ((result
             (emacs-command-loop-gui-write-post-command-state-current-context)))
        (should (equal "project-query-replace-regexp"
                       (plist-get result :command-name)))
        (should (eq 'prefix-arg (plist-get result :lane)))
        (should (memq :clear callbacks))
        (should (memq :minibuffer callbacks)))))
  (emacs-command-loop-builtins-test--with-fresh-state
    (emacs-command-loop-gui-set-context :keys "C-x" :arg "seed")
    (should (string-equal "C-x"
                          (emacs-command-loop-gui-minibuffer-key)))
    (should (string-equal "seed"
                          (emacs-command-loop-gui-minibuffer-initial-input)))
    (emacs-command-loop-gui-register-backend
     :minibuffer-key (lambda () "RET")
     :minibuffer-initial-input (lambda () "typed"))
    (should (string-equal "RET"
                          (emacs-command-loop-gui-minibuffer-key)))
    (should (string-equal "typed"
                          (emacs-command-loop-gui-minibuffer-initial-input))))
  (emacs-command-loop-builtins-test--with-fresh-state
    (let (called args commands)
      (emacs-command-loop-gui-register-backend
       :current-command (lambda () 'project-any-command)
       :current-effective-command (lambda () "project-any-command")
       :current-arg (lambda () "")
       :current-minibuffer-arg (lambda () "nested")
       :commandp (lambda (_command) t)
       :read-only-p (lambda () nil)
       :prefix-arg-empty-p (lambda () t)
       :set-command (lambda (command) (push command commands))
       :set-arg (lambda (arg) (push arg args))
       :call-command (lambda (command) (setq called command)
                       :project-called))
      (should (eq :project-called
                  (emacs-command-loop-gui-project-command-current-context
                   'project-any-command)))
      (should (eq 'project-dired called))
      (should (member "nested" args))
      (should (eq 'project-any-command (car commands))))))

(ert-deftest emacs-command-loop-builtins-test/needs-review-read-command-and-drain ()
  (emacs-command-loop-builtins-test--with-fresh-keymaps
    (let ((had-reader (fboundp 'emacs-minibuffer-completing-read))
          (old-reader (and (fboundp 'emacs-minibuffer-completing-read)
                           (symbol-function
                            'emacs-minibuffer-completing-read))))
      (unwind-protect
          (progn
            (fset 'emacs-minibuffer-completing-read
                  (lambda (&rest _args) "forward-char"))
            (should (eq 'forward-char
                        (emacs-command-loop-read-command "M-x ")))
            (fset 'emacs-minibuffer-completing-read
                  (lambda (&rest _args) ""))
            (should (eq 'save-buffer
                        (emacs-command-loop-read-command
                         "M-x " 'save-buffer))))
        (if had-reader
            (fset 'emacs-minibuffer-completing-read old-reader)
          (fmakunbound 'emacs-minibuffer-completing-read))))
    (setq emacs-command-loop-builtins-test--counter 0)
    (emacs-keymap-define-key emacs-keymap-global-map "a"
                             'emacs-command-loop-builtins-test--bump)
    (emacs-command-loop-feed-events ?a ?a)
    (should (= 2 (emacs-command-loop-drain)))
    (should (= 2 emacs-command-loop-builtins-test--counter))
    (emacs-command-loop-feed-events ?a)
    (setq emacs-command-loop--this-command 'dirty)
    (should (= 0 (emacs-command-loop-top-level)))
    (should-not emacs-command-loop--this-command)
    (should-not (emacs-command-loop-pending-p))))

(provide 'emacs-command-loop-builtins-test)

;;; emacs-command-loop-builtins-test.el ends here
