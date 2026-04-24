;;; main.el --- NeLisp daemon minimum (Phase 5-C.4 demo) -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Phase 5-C.4 per Doc 14 §3.4.  Three-layer integration demo for
;; the AI-critical path: `nelisp-process' (actor event plumbing
;; reused) + `nelisp-network' (HTTP client) + `nelisp-file-notify'
;; (watcher) driving a minimal stdio JSON-RPC "daemon" that
;; answers MCP-flavoured requests.
;;
;; This example is NOT a real MCP server.  It is a purposely thin
;; scaffold that proves the Phase 5-C primitives compose; the
;; framing (line-delimited JSON, no `Content-Length' header, no
;; notifications) is the smallest shape that still lets us send
;; an `initialize' / `tools/list' / `tools/call' round trip.
;;
;; Public entry points:
;;
;;   `nelisp-daemon-start'       — fresh state + event collector
;;   `nelisp-daemon-stop'        — tear down watches + actor
;;   `nelisp-daemon-dispatch'    — pure (method params) -> result
;;                                 plist.  Testable without JSON.
;;   `nelisp-daemon-call-json'   — JSON-RPC string in/out wrapper
;;   `nelisp-daemon-run-stdio'   — read stdin line-by-line, write
;;                                 responses to stdout; exits on
;;                                 `shutdown'
;;
;; Exposed tools (all under `tools/call'):
;;
;;   http-get       GET a URL via `nelisp-http-get', return status+body
;;   watch          Register a `nelisp-file-notify' watch on a DIR
;;   watch-events   Drain + return the event list accumulated by
;;                  the collector actor
;;   shutdown       Stop the daemon, causing `run-stdio' to exit
;;
;; `file-change' events posted by the file-notify trampoline reach
;; the collector actor as `nelisp-event's; the actor pushes a
;; compact summary onto a global list so the demo can expose them
;; through a tool call without wiring a bespoke request/response
;; protocol here.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'nelisp-actor)
(require 'nelisp-eventloop)
(require 'nelisp-network)
(require 'nelisp-file-notify)

;;; State --------------------------------------------------------------

(defvar nelisp-daemon--state nil
  "Plist with :collector (actor) :watches (alist dir -> wrap)
:running (bool).  Reset by `nelisp-daemon-start'/`-stop'.")

(defvar nelisp-daemon--events nil
  "Newest-first list of event summaries, each a plist
 (:kind :action :file :file1).  Pushed by the collector actor.")

(defun nelisp-daemon--collector-lambda ()
  "Return an actor body that collects `file-change' events into
`nelisp-daemon--events' until a `daemon-stop' event arrives."
  (nelisp-actor-lambda
    (let ((running t))
      (while running
        (let ((msg (nelisp-receive)))
          (cond
           ((and (nelisp-event-p msg)
                 (eq (nelisp-event-kind msg) 'daemon-stop))
            (setq running nil))
           ((and (nelisp-event-p msg)
                 (eq (nelisp-event-kind msg) 'file-change))
            (let ((data (nelisp-event-data msg)))
              (push (list :kind 'file-change
                          :action (plist-get data :action)
                          :file (plist-get data :file)
                          :file1 (plist-get data :file1))
                    nelisp-daemon--events))
            (nelisp-yield))
           (t (nelisp-yield))))))))

(defun nelisp-daemon-start ()
  "Initialise daemon state and spawn the collector actor."
  (nelisp-daemon-stop)
  (setq nelisp-daemon--events nil)
  (let ((collector (nelisp-spawn (nelisp-daemon--collector-lambda))))
    (setq nelisp-daemon--state
          (list :collector collector
                :watches nil
                :running t))
    collector))

(defun nelisp-daemon-stop ()
  "Remove every watch and signal the collector to terminate."
  (when nelisp-daemon--state
    (dolist (entry (plist-get nelisp-daemon--state :watches))
      (ignore-errors (nelisp-file-notify-rm-watch (cdr entry))))
    (let ((collector (plist-get nelisp-daemon--state :collector)))
      (when (and collector (nelisp-actor-p collector)
                 (not (memq (nelisp-actor-status collector)
                            '(:dead :crashed))))
        (ignore-errors
          (nelisp-send collector (nelisp-make-event 'daemon-stop nil)))
        (nelisp-actor-run-until-idle)))
    (setq nelisp-daemon--state nil)))

;;; Tool implementations ---------------------------------------------

(defun nelisp-daemon--tool-http-get (args)
  (let* ((url (or (alist-get 'url args) (alist-get "url" args nil nil #'equal)))
         (result (nelisp-http-get url)))
    (list :status (plist-get result :status)
          :body (or (plist-get result :body) "")
          :cached (plist-get result :cached))))

(defun nelisp-daemon--tool-watch (args)
  (let* ((dir (or (alist-get 'dir args) (alist-get "dir" args nil nil #'equal)))
         (collector (plist-get nelisp-daemon--state :collector))
         (wrap (nelisp-file-notify-add-watch
                dir '(change) #'ignore :actor collector)))
    (setq nelisp-daemon--state
          (plist-put nelisp-daemon--state :watches
                     (cons (cons dir wrap)
                           (plist-get nelisp-daemon--state :watches))))
    (list :watching dir :ok t)))

(defun nelisp-daemon--tool-watch-events (_args)
  (nelisp-actor-run-until-idle)
  (let ((events (reverse nelisp-daemon--events)))
    (setq nelisp-daemon--events nil)
    (list :events events :count (length events))))

(defun nelisp-daemon--tool-shutdown (_args)
  (setq nelisp-daemon--state
        (plist-put nelisp-daemon--state :running nil))
  (list :ok t))

;;; Pure dispatcher ---------------------------------------------------

(defun nelisp-daemon-dispatch (method params)
  "Dispatch METHOD (string) with PARAMS (alist) and return a
result plist.  Used directly by ERT; `call-json' wraps this."
  (pcase method
    ("initialize"
     (list :serverInfo (list :name "nelisp-daemon-demo"
                             :version "0.1")
           :protocolVersion "2025-06-18"))
    ("tools/list"
     (list :tools
           (list
            (list :name "http-get"
                  :description "Fetch a URL via nelisp-http-get")
            (list :name "watch"
                  :description "Register a file-notify watch on a DIR")
            (list :name "watch-events"
                  :description "Drain and return accumulated file-change events")
            (list :name "shutdown"
                  :description "Stop the daemon stdio loop"))))
    ("tools/call"
     (let ((name (or (alist-get 'name params)
                     (alist-get "name" params nil nil #'equal)))
           (args (or (alist-get 'arguments params)
                     (alist-get "arguments" params nil nil #'equal))))
       (pcase name
         ("http-get"     (nelisp-daemon--tool-http-get args))
         ("watch"        (nelisp-daemon--tool-watch args))
         ("watch-events" (nelisp-daemon--tool-watch-events args))
         ("shutdown"     (nelisp-daemon--tool-shutdown args))
         (_ (list :error (format "unknown tool %S" name))))))
    (_ (list :error (format "unknown method %s" method)))))

;;; JSON-RPC wire wrapper --------------------------------------------

(defun nelisp-daemon--plist-to-alist (plist)
  "Convert a plist (recursively) to an alist suitable for
`json-encode'.  Leaves non-plist lists alone as arrays."
  (cond
   ((and (listp plist)
         (keywordp (car-safe plist)))
    (let ((acc nil))
      (while plist
        (let ((k (car plist)) (v (cadr plist)))
          (push (cons (intern (substring (symbol-name k) 1))
                      (nelisp-daemon--plist-to-alist v))
                acc)
          (setq plist (cddr plist))))
      (nreverse acc)))
   ((and (listp plist) plist)
    (mapcar #'nelisp-daemon--plist-to-alist plist))
   (t plist)))

(defun nelisp-daemon-call-json (request-json)
  "Accept a JSON-RPC 2.0 request string, dispatch, return a JSON
response string.  Errors are reported as `result: {error: ...}'
rather than as JSON-RPC error objects (the demo doesn't need the
full error envelope)."
  (let* ((req (json-read-from-string request-json))
         (id (alist-get 'id req))
         (method (alist-get 'method req))
         (params (alist-get 'params req))
         (result-plist (nelisp-daemon-dispatch method params))
         (result-alist (nelisp-daemon--plist-to-alist result-plist)))
    (json-encode
     (list (cons 'jsonrpc "2.0")
           (cons 'id id)
           (cons 'result result-alist)))))

;;; stdio runner (production-ish) ------------------------------------

(defun nelisp-daemon-run-stdio ()
  "Read line-delimited JSON-RPC requests from STDIN, write
responses to STDOUT, until `shutdown' is called or EOF is seen.
Intended to be invoked by `emacs --batch -l main.el -f
nelisp-daemon-run-stdio'."
  (nelisp-daemon-start)
  (let ((line nil))
    (while (and (plist-get nelisp-daemon--state :running)
                (setq line (ignore-errors (read-from-minibuffer ""))))
      (when (and (stringp line) (not (string-empty-p line)))
        (let ((resp (condition-case err
                        (nelisp-daemon-call-json line)
                      (error (format
                              "{\"jsonrpc\":\"2.0\",\"id\":null,\"result\":{\"error\":%S}}"
                              (format "%S" err))))))
          (princ resp)
          (princ "\n")))))
  (nelisp-daemon-stop))

(provide 'nelisp-daemon)
;;; main.el ends here
