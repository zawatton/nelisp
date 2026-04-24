;;; nelisp-server-test.el --- Tests for MCP server spine -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Phase 5-E §3.1 ERT coverage for `src/nelisp-server.el'.  The
;; `nelisp-deftool' macro and read-only tool implementations are
;; covered separately under §3.2 / §3.3; these tests exercise the
;; server spine only: lifecycle, method registry, dispatcher,
;; JSON-RPC encode/decode, error envelopes, stdio runner.

;;; Code:

(require 'ert)
(require 'json)
(require 'nelisp-server)

;;; Helpers ----------------------------------------------------------

(defmacro nelisp-server-test--with-fresh (&rest body)
  "Reinitialise server state (and clear the tool registry) around
BODY.  Ensures tests do not leak registry entries into each other."
  (declare (indent 0))
  `(unwind-protect
       (progn
         (clrhash nelisp-server--tool-registry)
         (nelisp-server-start)
         ,@body)
     (nelisp-server-stop)
     (clrhash nelisp-server--tool-registry)))

(defun nelisp-server-test--register-dummy-tool (name result)
  "Register a dummy tool NAME that returns RESULT verbatim for any
arguments.  Used by tools/list + tools/call tests."
  (puthash name
           (list :description (format "dummy tool %s" name)
                 :input-schema (list :type "object"
                                     :properties nil)
                 :handler (lambda (_args) result))
           nelisp-server--tool-registry))

;;; Lifecycle --------------------------------------------------------

(ert-deftest nelisp-server-test-start-sets-state ()
  (nelisp-server-test--with-fresh
    (should (plist-get nelisp-server--state :running))
    (should (string= (plist-get nelisp-server--state :protocol-version)
                     nelisp-server-protocol-version))))

(ert-deftest nelisp-server-test-stop-clears-state ()
  (nelisp-server-start)
  (nelisp-server-stop)
  (should (null nelisp-server--state))
  (should (zerop (hash-table-count nelisp-server--method-registry))))

(ert-deftest nelisp-server-test-start-installs-builtin-handlers ()
  (nelisp-server-test--with-fresh
    (should (gethash "initialize" nelisp-server--method-registry))
    (should (gethash "notifications/initialized"
                     nelisp-server--method-registry))
    (should (gethash "tools/list" nelisp-server--method-registry))
    (should (gethash "tools/call" nelisp-server--method-registry))))

;;; Pure dispatcher --------------------------------------------------

(ert-deftest nelisp-server-test-dispatch-initialize ()
  (nelisp-server-test--with-fresh
    (let ((result (nelisp-server-dispatch "initialize" nil)))
      (should (equal (plist-get result :protocolVersion)
                     "2024-11-05"))
      (should (equal (plist-get result :serverInfo)
                     (list :name "nelisp-anvil" :version "0.1")))
      (should (plist-get nelisp-server--state :init-sent)))))

(ert-deftest nelisp-server-test-dispatch-unknown-method ()
  (nelisp-server-test--with-fresh
    (let* ((result (nelisp-server-dispatch "does/not-exist" nil))
           (err (plist-get result :error)))
      (should err)
      (should (= -32601 (plist-get err :code)))
      (should (string-match-p "method not found"
                              (plist-get err :message))))))

(ert-deftest nelisp-server-test-dispatch-notifications-initialized-drops ()
  (nelisp-server-test--with-fresh
    (should (eq :notification
                (nelisp-server-dispatch "notifications/initialized"
                                        nil)))))

(ert-deftest nelisp-server-test-dispatch-handler-error-wraps-32603 ()
  (nelisp-server-test--with-fresh
    (nelisp-server-register-handler
     "boom"
     (lambda (_params) (error "boom!")))
    (let* ((result (nelisp-server-dispatch "boom" nil))
           (err (plist-get result :error)))
      (should err)
      (should (= -32603 (plist-get err :code))))))

(ert-deftest nelisp-server-test-dispatch-tools-call-invalid-params ()
  "Missing `name' yields -32602 invalid-params."
  (nelisp-server-test--with-fresh
    (let* ((result (nelisp-server-dispatch "tools/call" nil))
           (err (plist-get result :error)))
      (should err)
      (should (= -32602 (plist-get err :code))))))

(ert-deftest nelisp-server-test-dispatch-tools-call-unknown-tool ()
  (nelisp-server-test--with-fresh
    (let* ((result (nelisp-server-dispatch
                    "tools/call"
                    '((name . "missing-tool") (arguments . nil))))
           (err (plist-get result :error)))
      (should err)
      (should (= -32602 (plist-get err :code))))))

;;; tools/list + tools/call via registry ----------------------------

(ert-deftest nelisp-server-test-tools-list-from-registry ()
  (nelisp-server-test--with-fresh
    (nelisp-server-test--register-dummy-tool
     "alpha" (list :payload "A"))
    (nelisp-server-test--register-dummy-tool
     "bravo" (list :payload "B"))
    (let* ((result (nelisp-server-dispatch "tools/list" nil))
           (tools (plist-get result :tools))
           (names (sort (mapcar (lambda (spec) (plist-get spec :name))
                                tools)
                        #'string<)))
      (should (equal '("alpha" "bravo") names)))))

(ert-deftest nelisp-server-test-tools-list-empty-properties-is-object ()
  "No-arg tools' inputSchema.properties must JSON-encode as `{}' not
`null'.  Claude Code's zod schema rejects the `null' form and
surfaces it as \"Failed to fetch tools: invalid_type at
tools[*].inputSchema.properties\"."
  (nelisp-server-test--with-fresh
    (puthash "noargs"
             (list :description "dummy no-arg tool"
                   :input-schema
                   (list :type "object"
                         :properties (make-hash-table :test 'equal))
                   :handler (lambda (_args) (list :ok t)))
             nelisp-server--tool-registry)
    (let* ((req "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"tools/list\"}")
           (resp (nelisp-server-call-json req)))
      (should (string-match-p "\"properties\":{}" resp))
      (should-not (string-match-p "\"properties\":null" resp)))))

(ert-deftest nelisp-server-test-tools-list-default-schema-is-object ()
  "When a tool registers without an explicit :input-schema, the
fallback schema's `properties' field must still serialize as `{}'."
  (nelisp-server-test--with-fresh
    (puthash "implicit"
             (list :description "no schema declared"
                   :handler (lambda (_args) (list :ok t)))
             nelisp-server--tool-registry)
    (let* ((req "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"tools/list\"}")
           (resp (nelisp-server-call-json req)))
      (should (string-match-p "\"properties\":{}" resp))
      (should-not (string-match-p "\"properties\":null" resp)))))

(ert-deftest nelisp-server-test-tools-call-registry-hit ()
  (nelisp-server-test--with-fresh
    (nelisp-server-test--register-dummy-tool
     "echo" (list :got 42))
    (let* ((result (nelisp-server-dispatch
                    "tools/call"
                    '((name . "echo") (arguments . nil))))
           (content (plist-get result :content)))
      (should (listp content))
      (should (= 1 (length content)))
      (should (string-match-p ":got 42"
                              (plist-get (car content) :text))))))

;;; JSON-RPC wire ----------------------------------------------------

(ert-deftest nelisp-server-test-call-json-initialize-round-trip ()
  (nelisp-server-test--with-fresh
    (let* ((req "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"initialize\"}")
           (resp (nelisp-server-call-json req))
           (parsed (json-read-from-string resp)))
      (should (string= "2.0" (alist-get 'jsonrpc parsed)))
      (should (= 1 (alist-get 'id parsed)))
      (let* ((result (alist-get 'result parsed))
             (info (alist-get 'serverInfo result)))
        (should (string= "2024-11-05"
                         (alist-get 'protocolVersion result)))
        (should (string= "nelisp-anvil" (alist-get 'name info)))))))

(ert-deftest nelisp-server-test-call-json-notification-returns-nil ()
  (nelisp-server-test--with-fresh
    (let ((req "{\"jsonrpc\":\"2.0\",\"method\":\"notifications/initialized\"}"))
      (should (null (nelisp-server-call-json req))))))

(ert-deftest nelisp-server-test-call-json-error-envelope-encoded ()
  (nelisp-server-test--with-fresh
    (let* ((req "{\"jsonrpc\":\"2.0\",\"id\":7,\"method\":\"nope\"}")
           (resp (nelisp-server-call-json req))
           (parsed (json-read-from-string resp))
           (err (alist-get 'error parsed)))
      (should (= 7 (alist-get 'id parsed)))
      (should err)
      (should (= -32601 (alist-get 'code err))))))

(ert-deftest nelisp-server-test-safe-call-json-parse-error ()
  "Malformed JSON yields -32700 with id null."
  (nelisp-server-test--with-fresh
    (let* ((resp (nelisp-server--safe-call-json "{not json"))
           (parsed (json-read-from-string resp))
           (err (alist-get 'error parsed)))
      (should (null (alist-get 'id parsed)))
      (should (= -32700 (alist-get 'code err))))))

;;; Registry extension ----------------------------------------------

(ert-deftest nelisp-server-test-register-handler-override ()
  (nelisp-server-test--with-fresh
    (nelisp-server-register-handler
     "custom/ping"
     (lambda (_params) (list :pong t)))
    (let ((result (nelisp-server-dispatch "custom/ping" nil)))
      (should (eq t (plist-get result :pong))))))

;;; stdio runner (scripted) -----------------------------------------

(ert-deftest nelisp-server-test-run-stdio-scripted ()
  "Feed a scripted `initialize' + EOF through the runner via a
`read-from-minibuffer' stub and capture the response from
`standard-output'."
  (let* ((requests (list (concat "{\"jsonrpc\":\"2.0\","
                                 "\"id\":11,\"method\":\"initialize\"}")
                         nil))
         (captured (generate-new-buffer " *nelisp-server-test-out*")))
    (unwind-protect
        (cl-letf (((symbol-function 'read-from-minibuffer)
                   (lambda (&rest _args)
                     (or (pop requests)
                         (signal 'end-of-file nil)))))
          (let ((standard-output captured))
            (nelisp-server-run-stdio))
          (with-current-buffer captured
            (let* ((out (string-trim (buffer-string)))
                   (parsed (json-read-from-string out)))
              (should (= 11 (alist-get 'id parsed)))
              (should (alist-get 'result parsed)))))
      (kill-buffer captured))))

(ert-deftest nelisp-server-test-run-stdio-stops-on-shutdown ()
  "If a custom shutdown handler flips :running to nil, the runner
exits after the current line."
  (let* ((requests (list "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"custom/shutdown\"}"
                         "{\"jsonrpc\":\"2.0\",\"id\":2,\"method\":\"initialize\"}"
                         nil))
         (captured (generate-new-buffer " *nelisp-server-test-out*")))
    (unwind-protect
        (cl-letf (((symbol-function 'read-from-minibuffer)
                   (lambda (&rest _args)
                     (or (pop requests)
                         (signal 'end-of-file nil)))))
          (let ((standard-output captured))
            ;; Preinstall a shutdown handler BEFORE the runner's
            ;; start-reinstall blows it away: we override the
            ;; builtins by providing our own after start.
            (cl-letf* ((orig (symbol-function
                              'nelisp-server--install-builtin-handlers))
                       ((symbol-function
                         'nelisp-server--install-builtin-handlers)
                        (lambda ()
                          (funcall orig)
                          (nelisp-server-register-handler
                           "custom/shutdown"
                           (lambda (_params)
                             (setq nelisp-server--state
                                   (plist-put nelisp-server--state
                                              :running nil))
                             (list :ok t))))))
              (nelisp-server-run-stdio)))
          (with-current-buffer captured
            (let* ((lines (split-string (buffer-string) "\n" t))
                   (first (json-read-from-string (car lines))))
              ;; Only the shutdown reply made it out — the second
              ;; request never ran because :running flipped nil.
              (should (= 1 (length lines)))
              (should (= 1 (alist-get 'id first))))))
      (kill-buffer captured))))

(provide 'nelisp-server-test)

;;; nelisp-server-test.el ends here
