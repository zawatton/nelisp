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
                 clear-this-command-keys))
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
        (before-keys-fn (symbol-function 'this-command-keys)))
    (require 'emacs-command-loop-builtins)
    (should (eq before-read    (symbol-function 'read-event)))
    (should (eq before-keys-fn (symbol-function 'this-command-keys)))))

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

(provide 'emacs-command-loop-builtins-test)

;;; emacs-command-loop-builtins-test.el ends here
