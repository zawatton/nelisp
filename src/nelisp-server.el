;;; nelisp-server.el --- MCP server spine for anvil-on-nelisp -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Phase 5-E §3.1 per Doc 16.  JSON-RPC 2.0 stdio MCP server skeleton
;; running on NeLisp; supersedes `examples/nelisp-daemon/main.el'
;; (Phase 5-C.4 scratch demo) as the permanent spine that later
;; phases layer tools onto.
;;
;; Responsibilities:
;;   - Line-delimited JSON-RPC 2.0 framing over stdio (no
;;     `Content-Length' header; one request per line).
;;   - MCP 2024-11-05 lifecycle: `initialize' / `notifications/
;;     initialized'; server advertises `tools.listChanged = false'.
;;   - Pluggable method registry.  Built-ins cover MCP
;;     `initialize', `notifications/initialized', `tools/list',
;;     `tools/call'.  Extensions register with
;;     `nelisp-server-register-handler'.
;;   - Tool registry (`nelisp-server--tool-registry') populated by
;;     `nelisp-deftool' (Phase 5-E §3.2; macro not yet defined).
;;     Built-in `tools/list' iterates this registry.
;;   - JSON-RPC error envelopes with the four standard codes:
;;     -32700 parse / -32601 method-not-found / -32602 invalid-
;;     params / -32603 internal.
;;
;; Non-goals (Phase 5-E Non-goals from Doc 16):
;;   - No tool implementations here; §3.3 adds file / git / http
;;     read-only tools via `nelisp-deftool'.
;;   - No `prompts' / `resources' / `sampling' MCP extensions.
;;   - No batch requests or cancellation.
;;   - No `Content-Length' header framing; line-delimited only.

;;; Code:

(require 'cl-lib)
(require 'json)

;;; State --------------------------------------------------------------

(defvar nelisp-server--state nil
  "Server lifecycle plist.
Keys: :running (bool) :init-sent (bool) :protocol-version (string).
`nelisp-server-start' resets, `nelisp-server-stop' clears to nil.")

(defvar nelisp-server--method-registry (make-hash-table :test 'equal)
  "Map of JSON-RPC method string to handler function of one arg
PARAMS (alist).  Handler returns either:
- a result plist (converted to alist + JSON-encoded by the wire)
- the keyword `:notification' (no response sent; for client-
  originated notifications such as `notifications/initialized')
- a result plist with an `:error' key mapping to (:code :message)
  plist (emitted as JSON-RPC `error' envelope).")

(defvar nelisp-server--tool-registry (make-hash-table :test 'equal)
  "Map of tool name string to spec plist
 (:description STR :input-schema SCHEMA :handler FN).  Populated
by `nelisp-deftool' (Phase 5-E §3.2).")

(defconst nelisp-server-protocol-version "2024-11-05"
  "MCP protocol version advertised by this server.")

(defconst nelisp-server-info
  (list :name "nelisp-anvil" :version "0.1")
  "serverInfo plist returned by `initialize'.")

;;; Error envelope helpers -------------------------------------------

(defun nelisp-server--error-envelope (code message)
  "Return a result plist that signals a JSON-RPC error.
CODE is an integer error code, MESSAGE a human-readable string."
  (list :error (list :code code :message message)))

(defun nelisp-server--error-p (result)
  "Non-nil if RESULT (from a method handler) is an error envelope."
  (and (listp result)
       (keywordp (car-safe result))
       (plist-get result :error)))

;;; plist <-> alist conversion for JSON encoding ---------------------

(defun nelisp-server--plist-to-alist (plist)
  "Convert plist (recursively) to alist for `json-encode'.
Vectors pass through as JSON arrays; plain lists (non-keyword
head) are mapped as arrays; keyword-headed plists become objects.
Strip the leading colon of keyword keys."
  (cond
   ((vectorp plist) plist)
   ((and (listp plist) (keywordp (car-safe plist)))
    (let ((acc nil))
      (while plist
        (let ((k (car plist)) (v (cadr plist)))
          (push (cons (intern (substring (symbol-name k) 1))
                      (nelisp-server--plist-to-alist v))
                acc))
        (setq plist (cddr plist)))
      (nreverse acc)))
   ((and (listp plist) plist)
    (mapcar #'nelisp-server--plist-to-alist plist))
   (t plist)))

;;; Built-in method handlers ----------------------------------------

(defun nelisp-server--handle-initialize (_params)
  "Handle MCP `initialize'.  Returns serverInfo + capabilities +
protocolVersion.  Advertises tools with listChanged = false."
  (setq nelisp-server--state
        (plist-put nelisp-server--state :init-sent t))
  (list :serverInfo nelisp-server-info
        :capabilities (list :tools (list :listChanged :false))
        :protocolVersion nelisp-server-protocol-version))

(defun nelisp-server--handle-initialized (_params)
  "Handle MCP `notifications/initialized'.  No response per spec."
  :notification)

(defun nelisp-server--handle-tools-list (_params)
  "Return every registered tool as MCP {name, description,
inputSchema}."
  (let ((tools nil))
    (maphash
     (lambda (name spec)
       (push (list :name name
                   :description (or (plist-get spec :description)
                                    "")
                   :inputSchema (or (plist-get spec :input-schema)
                                    (list :type "object"
                                          :properties nil)))
             tools))
     nelisp-server--tool-registry)
    (list :tools (nreverse tools))))

(defun nelisp-server--handle-tools-call (params)
  "Dispatch MCP `tools/call'.  PARAMS is an alist with `name' and
`arguments'.  Looks up `name' in `nelisp-server--tool-registry'
and invokes the spec's `:handler' with `arguments'."
  (let* ((name (alist-get 'name params))
         (args (alist-get 'arguments params))
         (spec (and (stringp name)
                    (gethash name nelisp-server--tool-registry))))
    (cond
     ((not (stringp name))
      (nelisp-server--error-envelope
       -32602 "tools/call: missing string `name' field"))
     ((null spec)
      (nelisp-server--error-envelope
       -32602 (format "tools/call: unknown tool %s" name)))
     (t
      (condition-case err
          (let* ((handler (plist-get spec :handler))
                 (result (funcall handler args)))
            ;; MCP tools/call result wraps content items; we emit
            ;; a single text block carrying the prin1 form of the
            ;; handler's return value.  Tools can already shape
            ;; the result plist themselves -- this is the minimal
            ;; envelope MCP expects.
            (list :content
                  (list (list :type "text"
                              :text (format "%S" result)))))
        (error
         (nelisp-server--error-envelope
          -32603
          (format "tools/call: handler %s: %S" name err))))))))

;;; Method registry helpers -----------------------------------------

(defun nelisp-server-register-handler (method handler)
  "Register HANDLER (function of one PARAMS arg) under METHOD
 (string).  Overwrites any previous registration for METHOD."
  (puthash method handler nelisp-server--method-registry))

(defun nelisp-server--install-builtin-handlers ()
  (nelisp-server-register-handler
   "initialize" #'nelisp-server--handle-initialize)
  (nelisp-server-register-handler
   "notifications/initialized" #'nelisp-server--handle-initialized)
  (nelisp-server-register-handler
   "tools/list" #'nelisp-server--handle-tools-list)
  (nelisp-server-register-handler
   "tools/call" #'nelisp-server--handle-tools-call))

;;; Lifecycle --------------------------------------------------------

(defun nelisp-server-start ()
  "Initialise state + reinstall built-in method handlers.
The tool registry is NOT cleared so `nelisp-deftool' forms that
executed at load time survive across restarts."
  (nelisp-server-stop)
  (clrhash nelisp-server--method-registry)
  (setq nelisp-server--state
        (list :running t
              :init-sent nil
              :protocol-version nelisp-server-protocol-version))
  (nelisp-server--install-builtin-handlers)
  nelisp-server--state)

(defun nelisp-server-stop ()
  "Tear down the method registry and mark state nil."
  (when nelisp-server--state
    (clrhash nelisp-server--method-registry)
    (setq nelisp-server--state nil)))

;;; Pure dispatcher --------------------------------------------------

(defun nelisp-server-dispatch (method params)
  "Dispatch METHOD (string) with PARAMS (alist).  Returns:
- `:notification' — caller should NOT emit a response
- an error envelope — (:error (:code N :message S))
- a result plist — JSON-encoded by the wire layer

Unknown methods yield a `-32601' error envelope.  Handler errors
are caught and wrapped as `-32603'."
  (let ((handler (gethash method nelisp-server--method-registry)))
    (cond
     ((null handler)
      (nelisp-server--error-envelope
       -32601 (format "method not found: %s" method)))
     (t
      (condition-case err
          (funcall handler params)
        (error
         (nelisp-server--error-envelope
          -32603 (format "handler %s: %S" method err))))))))

;;; JSON-RPC wire wrapper --------------------------------------------

(defun nelisp-server-call-json (request-json)
  "Accept a JSON-RPC 2.0 request string, dispatch, and return the
JSON response string.  Returns nil for notifications (dispatcher
replied `:notification').

Parse errors are surfaced via a JSON-RPC error envelope with
`id: null' and code `-32700'."
  (let* ((req (condition-case err
                  (json-read-from-string request-json)
                (error
                 (throw 'nelisp-server--parse-error
                        (nelisp-server--parse-error-json err))))))
    (let* ((id (alist-get 'id req))
           (method (alist-get 'method req))
           (params (alist-get 'params req))
           (result (nelisp-server-dispatch method params)))
      (cond
       ((eq result :notification) nil)
       ((nelisp-server--error-p result)
        (json-encode
         (list (cons 'jsonrpc "2.0")
               (cons 'id id)
               (cons 'error
                     (nelisp-server--plist-to-alist
                      (plist-get result :error))))))
       (t
        (json-encode
         (list (cons 'jsonrpc "2.0")
               (cons 'id id)
               (cons 'result
                     (nelisp-server--plist-to-alist result)))))))))

(defun nelisp-server--parse-error-json (err)
  "Build a JSON-RPC `-32700' parse-error response with id null."
  (json-encode
   (list (cons 'jsonrpc "2.0")
         (cons 'id nil)
         (cons 'error
               (list (cons 'code -32700)
                     (cons 'message (format "parse error: %S" err)))))))

(defun nelisp-server--safe-call-json (request-json)
  "Wrap `nelisp-server-call-json' and never raise; surface parse
errors as JSON error envelopes instead."
  (or (catch 'nelisp-server--parse-error
        (nelisp-server-call-json request-json))
      nil))

;;; stdio runner ------------------------------------------------------

(defun nelisp-server-run-stdio ()
  "Read line-delimited JSON-RPC requests from STDIN, emit responses
 (or nothing for notifications) to STDOUT, until state is cleared
or EOF is seen.  Intended invocation:

  emacs --batch -Q -L src -l nelisp-server -f nelisp-server-run-stdio"
  (nelisp-server-start)
  (let ((line nil))
    (while (and (plist-get nelisp-server--state :running)
                (setq line (ignore-errors (read-from-minibuffer ""))))
      (when (and (stringp line) (not (string-empty-p line)))
        (let ((resp (nelisp-server--safe-call-json line)))
          (when resp
            (princ resp)
            (terpri))))))
  (nelisp-server-stop))

(provide 'nelisp-server)

;;; nelisp-server.el ends here
