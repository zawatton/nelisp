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
    (should-error (emacs-command-loop-keyboard-quit) :type 'quit)))

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
    (should-error (emacs-command-loop-abort-recursive-edit) :type 'quit)))

(ert-deftest emacs-command-loop-builtins-test/abort-recursive-edit-throws-aborted ()
  (emacs-command-loop-builtins-test--with-fresh-keymaps
    (emacs-keymap-define-key emacs-keymap-global-map "x"
                             'emacs-command-loop-abort-recursive-edit)
    (emacs-command-loop-feed-events ?x)
    (let ((r (emacs-command-loop-recursive-edit)))
      (should (eq 'aborted r)))))

(provide 'emacs-command-loop-builtins-test)

;;; emacs-command-loop-builtins-test.el ends here
