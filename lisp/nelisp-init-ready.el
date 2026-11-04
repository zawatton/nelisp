;;; nelisp-init-ready.el --- Ready-state contract for application initialization  -*- lexical-binding: t; -*-

;;; Commentary:

;; Implements the "Proposed ready-state contract" of Doc 235 as a general
;; library: an identity fingerprint plus an initialization transaction with
;; the states UNSEEN -> INITIALIZING -> READY | FAILED.
;;
;; A transaction is a plain vector (no cl-defstruct, so it loads in minimal
;; runtimes):
;;   [nelisp-init-ready-txn STATE IDENTITY STATUS ROOT-SET REGISTRATIONS]
;; STATE is one of `unseen', `initializing', `ready', `failed'.  Use the
;; accessor functions; the layout is private.

;;; Code:

(define-error 'nelisp-init-ready-error "Init-ready error")
(define-error 'nelisp-init-ready-reentrant
  "Reentrant initialization" 'nelisp-init-ready-error)
(define-error 'nelisp-init-ready-conflict
  "Conflicting registration" 'nelisp-init-ready-error)
(define-error 'nelisp-init-ready-stale
  "Stale or mismatching identity" 'nelisp-init-ready-error)
(define-error 'nelisp-init-ready-failed
  "Initialization previously failed" 'nelisp-init-ready-error)

(defun nelisp-init-ready-fingerprint (components)
  "Return a deterministic SHA-256 hex fingerprint of COMPONENTS.
COMPONENTS is a non-empty alist of (KEY . STRING-VALUE) covering runtime or
image version, prelude and module hashes, compiler and native ABI versions,
feature set, producer/admission version and required registration
identities.  Keys are sorted with `string<' on `symbol-name' (or the string
itself), so input order is irrelevant.  Each key and value is encoded with a
decimal length prefix, so (\"ab\" . \"c\") and (\"a\" . \"bc\") differ.
Signal `nelisp-init-ready-error' on an empty alist, duplicate keys or
non-string values.

The fingerprint deliberately does NOT freeze live Lisp state.  After READY,
`fset', advice and module redefinition remain legal; invalidating or
revalidating dependent native identities is the job of native guards
(Doc 231), not of this module."
  (when (or (null components) (not (listp components)))
    (signal 'nelisp-init-ready-error (list "empty or invalid components")))
  (let ((entries nil) (seen nil))
    (dolist (c components)
      (unless (and (consp c) (or (symbolp (car c)) (stringp (car c))))
        (signal 'nelisp-init-ready-error (list "invalid component" c)))
      (unless (stringp (cdr c))
        (signal 'nelisp-init-ready-error (list "non-string value" (car c))))
      (let ((k (nelisp-init-ready--key-name (car c))))
        (when (member k seen)
          (signal 'nelisp-init-ready-error (list "duplicate key" k)))
        (push k seen)
        (push (cons k (cdr c)) entries)))
    (setq entries (sort entries (lambda (a b) (string< (car a) (car b)))))
    (let ((parts nil))
      (dolist (e entries)
        (push (format "%d:%s%d:%s" (length (car e)) (car e)
                      (length (cdr e)) (cdr e))
              parts))
      (secure-hash 'sha256 (apply #'concat (nreverse parts))))))

(defun nelisp-init-ready--key-name (key)
  "Return the string name of KEY (symbol or string)."
  (if (symbolp key) (symbol-name key) key))

;;; Transaction

(defun nelisp-init-ready-make ()
  "Return a fresh transaction in state `unseen'."
  (vector 'nelisp-init-ready-txn 'unseen nil nil nil nil))

(defun nelisp-init-ready-state (txn)
  "Return the state of TXN: `unseen', `initializing', `ready' or `failed'."
  (aref txn 1))

(defun nelisp-init-ready-identity (txn)
  "Return the identity recorded in TXN, or nil."
  (aref txn 2))

(defun nelisp-init-ready-status (txn)
  "Return the failure status recorded in TXN, or nil."
  (aref txn 3))

(defun nelisp-init-ready-root-set (txn)
  "Return the root set published by TXN, or nil."
  (aref txn 4))

(defun nelisp-init-ready-registrations (txn)
  "Return the registrations of TXN as an alist (DEFINITION-ID . PROVIDER)."
  (reverse (aref txn 5)))

(defun nelisp-init-ready--check-active (txn identity op)
  "Signal unless TXN is INITIALIZING with IDENTITY; OP names the caller."
  (unless (eq (aref txn 1) 'initializing)
    (signal 'nelisp-init-ready-error
            (list op "transaction not initializing" (aref txn 1))))
  (unless (equal (aref txn 2) identity)
    (signal 'nelisp-init-ready-stale (list op identity (aref txn 2)))))

(defun nelisp-init-ready-begin (txn identity)
  "Begin initialization of TXN under IDENTITY.
Return t when initialization started, or `:reuse' when TXN is already READY
with the same IDENTITY.  A different identity in any non-UNSEEN state signals
`nelisp-init-ready-stale' before anything runs.  Same-identity INITIALIZING
signals `nelisp-init-ready-reentrant'; same-identity FAILED signals
`nelisp-init-ready-failed' with the recorded status.  Never retries."
  (let ((state (aref txn 1)))
    (cond
     ((eq state 'unseen)
      (aset txn 1 'initializing)
      (aset txn 2 identity)
      t)
     ((not (equal (aref txn 2) identity))
      (signal 'nelisp-init-ready-stale (list 'begin identity (aref txn 2))))
     ((eq state 'ready) :reuse)
     ((eq state 'initializing)
      (signal 'nelisp-init-ready-reentrant (list identity)))
     ((eq state 'failed)
      (signal 'nelisp-init-ready-failed (list identity (aref txn 3))))
     (t (signal 'nelisp-init-ready-error (list "corrupt state" state))))))

(defun nelisp-init-ready-register (txn identity definition-id provider)
  "Register DEFINITION-ID with PROVIDER in TXN under IDENTITY.
Only valid while INITIALIZING.  Re-registering the same pair is idempotent;
the same DEFINITION-ID with a different PROVIDER signals
`nelisp-init-ready-conflict'."
  (nelisp-init-ready--check-active txn identity 'register)
  (let ((old (assoc definition-id (aref txn 5))))
    (cond
     ((null old) (aset txn 5 (cons (cons definition-id provider) (aref txn 5))))
     ((equal (cdr old) provider) nil)
     (t (signal 'nelisp-init-ready-conflict
                (list definition-id (cdr old) provider)))))
  nil)

(defun nelisp-init-ready-publish (txn identity root-set)
  "Move TXN from INITIALIZING to READY, recording ROOT-SET (a list).
Return TXN.  Signal on wrong state or identity."
  (nelisp-init-ready--check-active txn identity 'publish)
  (unless (listp root-set)
    (signal 'nelisp-init-ready-error (list "root-set must be a list")))
  (aset txn 4 root-set)
  (aset txn 1 'ready)
  txn)

(defun nelisp-init-ready-fail (txn identity status)
  "Move TXN from INITIALIZING to FAILED, recording STATUS."
  (nelisp-init-ready--check-active txn identity 'fail)
  (aset txn 3 status)
  (aset txn 1 'failed)
  txn)

(defun nelisp-init-ready-run (txn identity thunk)
  "Run THUNK as the initialization of TXN under IDENTITY, exactly once.
Return `:reuse' without calling THUNK when TXN is READY with IDENTITY.
Otherwise call THUNK with TXN; its return value is the root set to publish.
If THUNK signals, record FAILED with the condition and re-signal the
original condition unchanged.  Any other nonlocal exit (a `throw', or a
quit not handled as an error) also records FAILED, with status
\(:nonlocal-exit), so the transaction is never left INITIALIZING and a
later `nelisp-init-ready-begin' reports the failure instead of re-running
user initialization.  Returns the READY transaction on success."
  (if (eq (nelisp-init-ready-begin txn identity) :reuse)
      :reuse
    (let ((done nil) roots)
      (unwind-protect
          (progn
            (setq roots (condition-case err
                            (funcall thunk txn)
                          (error
                           (setq done t)
                           (nelisp-init-ready-fail txn identity err)
                           (signal (car err) (cdr err)))))
            (setq done t))
        (unless done
          (nelisp-init-ready-fail txn identity '(:nonlocal-exit))))
      (nelisp-init-ready-publish txn identity roots))))

(provide 'nelisp-init-ready)
;;; nelisp-init-ready.el ends here
