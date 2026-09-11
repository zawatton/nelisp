;;; nelisp-dev-failure.el --- bounded live REPL failure adapter -*- lexical-binding: t; -*-

;;; Code:

(require 'cl-lib)
(require 'nelisp-dev-protocol)
(require 'nelisp-repl-session)

(defconst nelisp-dev-failure--ttl 900)
(defconst nelisp-dev-failure--max-handles 64)
(defvar nelisp-dev-failure--handles (make-hash-table :test #'equal))
(defvar nelisp-dev-failure--nonce 0)

(defun nelisp-dev-failure--context-get (context key)
  (plist-get context (intern (concat ":" key))))

(defun nelisp-dev-failure--arg (request key)
  (cdr (assoc key (cdr (assoc "arguments" request)))))

(defun nelisp-dev-failure--session-id (context)
  (nelisp-dev-failure--context-get context "session-id"))

(defun nelisp-dev-failure--wire-identity (context)
  (list (cons "target" (or (nelisp-dev-failure--context-get context "target")
                            "host-emacs"))
        (cons "session_id" (or (nelisp-dev-failure--session-id context) :null))
        (cons "source_revision" :null)
        (cons "source_content_hash" :null)
        (cons "runtime_artifact_hash" :null)
        (cons "generation" :null)))

(defun nelisp-dev-failure--purge ()
  (let ((now (float-time)) dead)
    (maphash (lambda (key entry)
               (when (or (<= (nth 0 entry) now)
                         (not (memq (nth 2 entry) nelisp-repl-session--failures)))
                 (push key dead)))
             nelisp-dev-failure--handles)
    (dolist (key dead) (remhash key nelisp-dev-failure--handles))))

(defun nelisp-dev-failure-clear ()
  "Release all diagnostic handles held by this adapter.
The live REPL failure records themselves are cleared by the session owner."
  (let ((count (hash-table-count nelisp-dev-failure--handles)))
    (clrhash nelisp-dev-failure--handles)
    count))

(defun nelisp-dev-failure--function-name (function)
  (if (symbolp function) (symbol-name function) "unknown"))

(defun nelisp-dev-failure--condition-name (condition)
  (if (symbolp condition) (symbol-name condition) "unknown"))

(defun nelisp-dev-failure--record-identity (record session-id)
  "Hash only bounded scalar metadata; never print an application value."
  (secure-hash
   'sha256
   (format "%s\0%d\0%s\0%s\0%s\0%d\0%d"
           (or session-id "") (or (plist-get record :id) -1)
           (nelisp-dev-failure--function-name (plist-get record :function))
           (nelisp-dev-failure--condition-name (plist-get record :condition))
           (or (plist-get record :message) "")
           (length (plist-get record :args))
           (if (plist-get record :truncated) 1 0))))

(defun nelisp-dev-failure--live-record (handle session-id)
  (nelisp-dev-failure--purge)
  (let ((entry (and (stringp handle) (gethash handle nelisp-dev-failure--handles)))
        (live nelisp-repl-session--failures))
    (when (and entry (equal session-id (nth 1 entry)))
      (let ((record (nth 2 entry)))
        (when (and (memq record live)
                   (equal (nth 3 entry)
                          (nelisp-dev-failure--record-identity record session-id)))
          record)))))

(defun nelisp-dev-failure--handle (record session-id)
  (nelisp-dev-failure--purge)
  (let ((identity (nelisp-dev-failure--record-identity record session-id)) existing)
    (maphash (lambda (handle entry)
               (when (and (eq record (nth 2 entry))
                          (equal session-id (nth 1 entry))
                          (equal identity (nth 3 entry)))
                 (setq existing handle))) nelisp-dev-failure--handles)
    (or existing
        (when (< (hash-table-count nelisp-dev-failure--handles)
                 nelisp-dev-failure--max-handles)
          (let ((handle (format "failure-%d-%s"
                                (cl-incf nelisp-dev-failure--nonce) identity)))
            (puthash handle (list (+ (float-time) nelisp-dev-failure--ttl)
                                  session-id record identity)
                     nelisp-dev-failure--handles)
            handle)))))

(defun nelisp-dev-failure--find (request context &optional allow-id)
  (let* ((session-id (nelisp-dev-failure--session-id context))
         (handle (or (nelisp-dev-failure--arg request "failure_handle")
                     (nelisp-dev-failure--arg request "failure_identity")))
         (id (nelisp-dev-failure--arg request "failure_id"))
         (record (and handle (nelisp-dev-failure--live-record handle session-id))))
    (when (and allow-id (not handle) (integerp id))
      ;; ID lookup is accepted only to issue a handle for a currently live
      ;; record.  Retry still requires the handle, avoiding clear/reuse races.
      (setq record (cl-find id nelisp-repl-session--failures
                            :key (lambda (entry) (plist-get entry :id)))))
    (list record handle session-id)))

(defun nelisp-dev-failure--diagnose (request context)
  (let* ((found (nelisp-dev-failure--find request context t))
         (record (nth 0 found))
         (session-id (nth 2 found)))
    (cond
     ((not (stringp session-id))
      (list :status "unsupported" :summary '(("errors" . 1))
            :diagnostics (vector (list (cons "code" "NELISP-DEV-LIVE-SESSION-REQUIRED")))
            :data nil :limitations ["Diagnosis requires the live REPL session context."]))
     ((not record)
      (list :status "failed" :summary '(("errors" . 1))
            :diagnostics (vector (list (cons "code" "NELISP-DEV-STALE-OR-UNKNOWN-FAILURE")))
            :data nil :limitations ["The failure handle is absent, expired, cleared, or belongs to another session."]))
     (t
      (let ((handle (or (nth 1 found) (nelisp-dev-failure--handle record session-id))))
        (list :status (if handle "ok" "inconclusive")
              :summary '(("errors" . 0) ("warnings" . 0))
              :diagnostics (vector (list (cons "code" "NELISP-REPL-FAILURE")
                                  (cons "id" (or handle "failure-handle-unavailable"))
                                  (cons "phase" "call")
                                  (cons "source_precision" "unknown")
                                  (cons "call_chain" :null)
                                  (cons "failure_handle" (or handle :null))
                                  (cons "failure_id" (plist-get record :id))
                                  (cons "function" (nelisp-dev-failure--function-name
                                                     (plist-get record :function)))
                                  (cons "condition" (nelisp-dev-failure--condition-name
                                                       (plist-get record :condition)))
                                  (cons "message" (plist-get record :message))
                                  (cons "argument_count" (length (plist-get record :args)))
                                  (cons "retryable" (if (plist-get record :retryable) t :false))))
              :data (list (cons "failure_handle" (or handle :null))
                          (cons "identity" (or (nth 3 (gethash handle nelisp-dev-failure--handles)) :null)))
              :limitations ["Function and argument identities are unknown for opaque values; arguments are never serialized or printed."
                            "Diagnosis performs no saved call and does not inspect application state."
                            "Call chain, exact source and loaded generation were not captured."
                            "Handles expire after 15 minutes; a full handle store makes retry unavailable."]))))))

(defun nelisp-dev-failure--retry (request context)
  (let* ((found (nelisp-dev-failure--find request context nil))
         (record (nth 0 found))
         (session-id (nth 2 found))
         (policy (or (nelisp-dev-failure--arg request "effects_policy")
                     (nelisp-dev-failure--arg request "effects-policy")
                     (nelisp-dev-failure--context-get context "effects-policy"))))
    (cond
     ((not (stringp session-id))
      (list :status "unsupported" :summary '(("errors" . 1)) :diagnostics
            (vector (list (cons "code" "NELISP-DEV-LIVE-SESSION-REQUIRED")))
            :limitations ["Retry requires the live REPL session context."]))
     ((not (member policy '("explicit-only" :explicit-only explicit-only)))
      (list :status "failed" :summary '(("errors" . 1)) :diagnostics
            (vector (list (cons "code" "NELISP-DEV-EFFECTS-POLICY-REQUIRED")))
            :limitations ["Retry requires an explicit effects policy in the request or trusted context."]))
     ((not record)
      (list :status "failed" :summary '(("errors" . 1)) :diagnostics
            (vector (list (cons "code" "NELISP-DEV-STALE-OR-UNKNOWN-FAILURE")))
            :limitations ["No saved call was invoked."]))
     (t
      (condition-case err
          (let ((result (nelisp-repl-session-retry (plist-get record :id))))
            (list :status "ok" :summary '(("errors" . 0)) :diagnostics []
                  :data (list (cons "failure_handle" (or (nth 1 found) :null))
                              (cons "result_type"
                                    (cond ((null result) "null")
                                          ((stringp result) "string")
                                          ((numberp result) "number")
                                          ((symbolp result) "symbol")
                                          ((listp result) "list")
                                          ((vectorp result) "vector")
                                          (t "unknown"))))
                  :limitations ["Retry was explicitly requested; application effects are caller responsibility."]))
        (error
         (list :status "failed" :summary '(("errors" . 1))
               :diagnostics (vector (list (cons "code" "NELISP-REPL-RETRY-FAILED")
                                   (cons "condition" (nelisp-dev-failure--condition-name (car err)))
                                   (cons "message" "The explicit retry signalled an error; condition data is not serialized.")))
               :limitations ["The saved call was invoked only after handle and policy validation."])))))))

(defun nelisp-dev-failure--wire (operation request context result)
  (nelisp-dev-protocol-envelope
   operation (cdr (assoc "request_id" request)) (plist-get result :status)
   (nelisp-dev-failure--wire-identity context) (plist-get result :summary)
   (plist-get result :diagnostics) (plist-get result :data)
   (plist-get result :limitations)))

(defun nelisp-dev-failure-dispatch (request context)
  "Dispatch bounded live-session `diagnose' and explicit `retry' requests."
  (let* ((operation (cdr (assoc "operation" request)))
         (requested-session (cdr (assoc "session_id" request)))
         (session-id (nelisp-dev-failure--session-id context)))
    (nelisp-dev-failure--wire
     operation request context
     (cond ((not (equal (or (nelisp-dev-failure--context-get context "target")
                            "host-emacs") "host-emacs"))
            (list :status "unsupported" :summary '(("errors" . 1))
                  :diagnostics (vector (list (cons "code" "NELISP-DEV-TARGET-UNSUPPORTED")))
                  :limitations ["This adapter only serves a host-emacs live session."]))
           ((and (stringp requested-session)
                 (not (equal requested-session session-id)))
            (list :status "unsupported" :summary '(("errors" . 1))
                  :diagnostics (vector (list (cons "code" "NELISP-DEV-LIVE-SESSION-REQUIRED")))
                  :limitations ["The request session identity does not match the live context."]))
           ((equal operation "diagnose")
            (nelisp-dev-failure--diagnose request context))
           ((equal operation "retry")
            (nelisp-dev-failure--retry request context))
           (t (list :status "unsupported" :summary '(("errors" . 1))
                    :diagnostics (vector (list (cons "code" "NELISP-DEV-UNKNOWN-FAILURE-OPERATION")))
                    :limitations ["Only diagnose and retry are provided by this adapter."]))))))

(provide 'nelisp-dev-failure)
;;; nelisp-dev-failure.el ends here
