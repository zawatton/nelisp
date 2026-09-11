;;; nelisp-dev.el --- shared P1 development dispatcher -*- lexical-binding: t; -*-
(require 'json)
(require 'nelisp-dev-protocol)
(declare-function nelisp-dev-source-dispatch "nelisp-dev-source" (request context))
(declare-function nelisp-dev-session-dispatch "nelisp-dev-session" (request context))
(defvar nelisp-dev--session-id nil)

(defun nelisp-dev-session-context (&optional root)
  "Create a context for this host Emacs process's explicit replay registry.
The process-lifetime ID is local metadata, not a remote attachment credential."
  (let ((context (nelisp-dev-context root)))
    (unless nelisp-dev--session-id
      (setq nelisp-dev--session-id
            (secure-hash 'sha256 (format "%S:%S" (current-time) (random)))))
    (plist-put context :session-id nelisp-dev--session-id)
    (plist-put context :adapters
               (append (plist-get context :adapters)
                       (list (cons "session.export" #'nelisp-dev-session-dispatch)
                             (cons "session.replay" #'nelisp-dev-session-dispatch))))
    context))

(defun nelisp-dev-context (&optional root)
  "Create a source-query context for this host Emacs process.
This does not connect to or impersonate a standalone REPL session."
  (require 'nelisp-dev-source)
  (require 'nelisp-dev-session)
  (list :root (or root default-directory) :target "host-emacs" :session-id nil
        :adapters (append
                   (mapcar (lambda (op) (cons op #'nelisp-dev-source-dispatch))
                           '("describe" "check" "impact"))
                   (list (cons "session.validate" #'nelisp-dev-session-dispatch)))))

(defun nelisp-dev--object-p (value)
  (and (proper-list-p value)
       (cl-every (lambda (pair) (and (consp pair) (stringp (car pair)))) value)
       (= (length value) (length (delete-dups (mapcar #'car value))))))

(defun nelisp-dev--valid-request-p (request)
  (and (nelisp-dev--object-p request)
       (cl-every (lambda (key) (assoc key request))
                 '("schema_version" "operation" "request_id" "arguments"
                   "target" "session_id" "limits"))
       (cl-every (lambda (pair)
                   (member (car pair) '("schema_version" "operation" "request_id"
                                       "arguments" "target" "session_id" "limits"))) request)
       (equal (cdr (assoc "schema_version" request)) "1")
       (cl-every (lambda (key)
                   (let ((v (cdr (assoc key request))))
                     (and (stringp v) (< 0 (length v) 129))))
                 '("operation" "request_id"))
       (cl-every (lambda (key)
                   (let ((v (cdr (assoc key request))))
                     (or (eq v :null) (and (stringp v) (<= (length v) 256)))))
                 '("target" "session_id"))
       (nelisp-dev--object-p (cdr (assoc "arguments" request)))
       (nelisp-dev--object-p (cdr (assoc "limits" request)))
       (let* ((limits (cdr (assoc "limits" request)))
              (bytes (cdr (assoc "bytes" limits)))
              (size (cdr (assoc "page_size" limits)))
              (cursor (cdr (assoc "cursor" limits))))
         (and (cl-every (lambda (pair) (member (car pair) '("bytes" "page_size" "cursor"))) limits)
              (or (not (assoc "bytes" limits))
                  (and (integerp bytes) (<= 1024 bytes 16384)))
              (or (not (assoc "page_size" limits))
                  (and (integerp size) (<= 1 size 200)))
              (or (not (assoc "cursor" limits)) (eq cursor :null)
                  (and (stringp cursor) (<= (length cursor) 256)))))))

(defun nelisp-dev--arg (request key)
  (cdr (assoc key (cdr (assoc "arguments" request)))))

(defun nelisp-dev--identity (context)
  (list (cons "target" (or (plist-get context :target) "host-emacs"))
        (cons "source_revision" :null) (cons "source_content_hash" :null)
        (cons "runtime_artifact_hash" :null)
        (cons "session_id" (or (plist-get context :session-id) :null))
        (cons "generation" :null)))

(defun nelisp-dev--request-id (request)
  (or (cdr (assoc "request_id" request)) "request-unknown"))

(defun nelisp-dev--capabilities (request context)
  (nelisp-dev-protocol-envelope
   "capabilities" (nelisp-dev--request-id request) "ok"
   (nelisp-dev--identity context) (list (cons "errors" 0) (cons "warnings" 0)) []
   (list (cons "protocol_versions" ["1"])
         (cons "operations" (vconcat '("capabilities" "test")
                                     (mapcar #'car (plist-get context :adapters))))
         (cons "targets" (list (cons "host-emacs" "partial")
                                (cons "normal-standalone" "unverified")
                                (cons "native-linux-x86_64" "unsupported")))
         (cons "adapters"
               (list (cons "DEV001" (list (cons "status" "partial")))
                     (cons "DEV010" (list (cons "status" "partial")))))
         (cons "fboundp_is_discovery_only" t))
   ["Only registered adapters are available; native target verification is separate."
     "Source and loaded artifact identities are unknown until supplied by an adapter."]))

(defun nelisp-dev--test (request context)
  (let* ((name (or (nelisp-dev--arg request "gate") "ert-full"))
         (root (or (plist-get context :root) default-directory))
         (_ (unless (and (stringp name)
                         (string-match-p "\\`[a-zA-Z0-9][a-zA-Z0-9_-]*\\'" name))
              (error "NELISP-DEV-INVALID-REQUEST: invalid gate name")))
         (path (expand-file-name (concat "target/gates/" name ".json") root)))
    (if (not (file-readable-p path))
        (nelisp-dev-protocol-envelope
         "test" (nelisp-dev--request-id request) "inconclusive"
         (nelisp-dev--identity context) (list (cons "passed" 0) (cons "failed" 0)
                                               (cons "skipped" 0) (cons "not_run" 1)) [] nil
         ["gate report is missing"])
      (let* ((_ (when (> (file-attribute-size (file-attributes path)) 1048576)
                  (error "Gate report exceeds input budget")))
             (report (nelisp-dev-protocol-string-keys
                      (json-read-file path)))
             (status (cdr (assoc "status" report)))
             (passed (or (cdr (assoc "passed" report)) 0))
             (failed (or (cdr (assoc "failed" report)) 0))
             (skipped (or (cdr (assoc "skipped" report)) 0))
             (ran (or (cdr (assoc "ran" report)) 0))
             (_ (unless (and (equal (cdr (assoc "schema" report)) "nelisp-gate/1")
                              (equal (cdr (assoc "name" report)) name)
                              (cl-every (lambda (n) (and (integerp n) (>= n 0)))
                                        (list ran passed failed skipped)))
                  (error "Invalid gate report"))))
        (nelisp-dev-protocol-envelope
         "test" (nelisp-dev--request-id request)
         (if (or (equal status "fail") (> failed 0)) "failed" "inconclusive")
         (nelisp-dev--identity context)
         (list (cons "passed" passed) (cons "failed" failed)
               (cons "skipped" skipped) (cons "not_run" 1)) []
         (list (cons "gate" name) (cons "executed" :false)
               (cons "report" report))
         ["No test was executed by this request; the report is historical evidence."
           "The legacy report has no verified source/artifact binding; freshness is unknown."
           "Zero cases, required skips and timeouts cannot establish a clean verdict."])))))

(defun nelisp-dev--dispatch (request context)
  "Dispatch a string-key alist REQUEST under CONTEXT." 
  (let ((op (and (nelisp-dev--object-p request) (cdr (assoc "operation" request))))
        (requested-session (and (nelisp-dev--object-p request) (cdr (assoc "session_id" request)))))
    (if (not (nelisp-dev--valid-request-p request))
        (nelisp-dev-protocol-envelope
         (if (stringp op) op "invalid") "request-unknown" "failed"
         (nelisp-dev--identity context) (list (cons "errors" 1))
         (vector (list (cons "code" "NELISP-DEV-INVALID-REQUEST"))) nil
         ["schema_version=1, string operation and request_id are required"])
    (if (and requested-session (not (eq requested-session :null))
             (not (equal requested-session (plist-get context :session-id))))
        (nelisp-dev-protocol-envelope
         op (nelisp-dev--request-id request) "unsupported"
         (nelisp-dev--identity context) nil
         [( ("code" . "NELISP-DEV-LIVE-SESSION-REQUIRED"))]
         (list (cons "required_interface" "repl"))
         ["NELISP-DEV-LIVE-SESSION-REQUIRED"])
    (let ((hook (cdr (assoc op (plist-get context :adapters)))))
      (cond
       ((let ((target (cdr (assoc "target" request))))
          (and (not (eq target :null))
               (not (equal target (or (plist-get context :target) "host-emacs")))))
        (nelisp-dev-protocol-envelope op (nelisp-dev--request-id request) "unsupported"
                                     (nelisp-dev--identity context) nil [] nil
                                     ["This context cannot execute for the requested target"]))
       ((functionp hook) (funcall hook request context))
       ((equal op "capabilities") (nelisp-dev--capabilities request context))
     ((equal op "test") (nelisp-dev--test request context))
     ((member op '("describe" "check" "diagnose" "impact" "retry" "reload.plan"
                   "reload.apply" "session.export" "session.validate" "session.replay"
                   "session.clear" "gc.snapshot" "gc.compare" "gc.collect"))
      (nelisp-dev-protocol-envelope op (nelisp-dev--request-id request) "unsupported"
                                    (nelisp-dev--identity context) nil [] nil
                                    ["adapter is not implemented in P1"]))
     (t (nelisp-dev-protocol-envelope op (nelisp-dev--request-id request) "failed"
                                      (nelisp-dev--identity context)
                                      (list (cons "errors" 1))
                                      (vector (list (cons "code" "NELISP-DEV-UNKNOWN-OPERATION")))
                                      nil []))))))))

(defun nelisp-dev-dispatch (request context)
  "Validate and dispatch REQUEST, preserving bounded structured failures."
  (condition-case err
      (let ((result (nelisp-dev--dispatch request context)))
        (if (nelisp-dev--valid-request-p request)
            (nelisp-dev-protocol-page result request context)
          result))
    (error
     (let* ((message (error-message-string err))
            (code (cond ((string-prefix-p "NELISP-DEV-STALE-CURSOR" message)
                         "NELISP-DEV-STALE-CURSOR")
                        ((string-prefix-p "NELISP-DEV-INVALID-REQUEST" message)
                         "NELISP-DEV-INVALID-REQUEST")
                        (t "NELISP-DEV-INTERNAL-ERROR"))))
       (nelisp-dev-protocol-envelope
        "invalid" "request-unknown" "failed" (nelisp-dev--identity context)
        '(("errors" . 1))
        (vector (list (cons "code" code) (cons "message" (substring message 0 (min 512 (length message))))))
        nil ["The operation did not complete; no successful result is implied."])))))

(provide 'nelisp-dev)
