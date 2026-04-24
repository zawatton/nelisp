;;; nelisp-server-sketch.el --- Phase 5-E primitive forecast sketch -*- lexical-binding: t; -*-

;; Forecast sketch used by `docs/design/16-audit-script.el' to detect
;; host-subr gaps before Phase 5-E §3.1-§3.3 implementation.  Not
;; meant to execute verbatim; only the head symbols of every (foo ...)
;; form are walked by the audit.  Covers:
;;
;;   §3.1 `src/nelisp-server.el' — MCP server (JSON-RPC 2.0 stdio,
;;        initialize / notifications/initialized / tools/list /
;;        tools/call, error envelope)
;;   §3.2 `nelisp-deftool' macro + dispatcher (registry hash-table,
;;        duplicate detection, schema normalize)
;;   §3.3 initial tools — file-read / file-outline / git-log /
;;        git-status / http-get / data-get-path / data-set-path

;;; Code:

(require 'cl-lib)
(require 'json)

;; -- §3.1 server -----------------------------------------------------

(defvar nelisp-server--state nil)
(defvar nelisp-server--tool-registry (make-hash-table :test 'equal))
(defvar nelisp-server--method-registry (make-hash-table :test 'equal))

(defun nelisp-server-start ()
  (setq nelisp-server--state (list :running t :init-sent nil))
  (clrhash nelisp-server--method-registry)
  (nelisp-server-register-handler "initialize" #'nelisp-server--handle-initialize)
  (nelisp-server-register-handler "notifications/initialized" #'nelisp-server--handle-initialized)
  (nelisp-server-register-handler "tools/list" #'nelisp-server--handle-tools-list)
  (nelisp-server-register-handler "tools/call" #'nelisp-server--handle-tools-call))

(defun nelisp-server-stop ()
  (setq nelisp-server--state nil)
  (clrhash nelisp-server--method-registry))

(defun nelisp-server-register-handler (method handler)
  (puthash method handler nelisp-server--method-registry))

(defun nelisp-server--handle-initialize (_params)
  (list :serverInfo (list :name "nelisp-anvil" :version "0.1")
        :capabilities (list :tools (list :listChanged :false))
        :protocolVersion "2024-11-05"))

(defun nelisp-server--handle-initialized (_params)
  :notification)

(defun nelisp-server--handle-tools-list (_params)
  (let ((tools nil))
    (maphash
     (lambda (name spec)
       (push (list :name name
                   :description (plist-get spec :description)
                   :inputSchema (plist-get spec :input-schema))
             tools))
     nelisp-server--tool-registry)
    (list :tools (nreverse tools))))

(defun nelisp-server--handle-tools-call (params)
  (let* ((name (alist-get 'name params))
         (args (alist-get 'arguments params))
         (spec (gethash name nelisp-server--tool-registry)))
    (cond
     ((null spec)
      (nelisp-server--error-envelope -32602
                                     (format "unknown tool %S" name)))
     (t
      (condition-case err
          (let ((result (funcall (plist-get spec :handler) args)))
            (list :content (list (list :type "text"
                                       :text (format "%S" result)))))
        (error
         (nelisp-server--error-envelope
          -32603 (format "handler error: %S" err))))))))

(defun nelisp-server--error-envelope (code message)
  (list :error (list :code code :message message)))

(defun nelisp-server-dispatch (method params)
  (let ((handler (gethash method nelisp-server--method-registry)))
    (cond
     ((null handler)
      (nelisp-server--error-envelope -32601
                                     (format "method not found: %s" method)))
     (t
      (funcall handler params)))))

(defun nelisp-server--plist-to-alist (plist)
  (cond
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

(defun nelisp-server-call-json (request-json)
  (let* ((req (json-read-from-string request-json))
         (id (alist-get 'id req))
         (method (alist-get 'method req))
         (params (alist-get 'params req))
         (result (nelisp-server-dispatch method params)))
    (cond
     ((eq result :notification) nil)
     ((and (listp result) (plist-get result :error))
      (json-encode
       (list (cons 'jsonrpc "2.0")
             (cons 'id id)
             (cons 'error (nelisp-server--plist-to-alist
                           (plist-get result :error))))))
     (t
      (json-encode
       (list (cons 'jsonrpc "2.0")
             (cons 'id id)
             (cons 'result (nelisp-server--plist-to-alist result))))))))

(defun nelisp-server-run-stdio ()
  (nelisp-server-start)
  (let ((line nil))
    (while (and (plist-get nelisp-server--state :running)
                (setq line (ignore-errors (read-from-minibuffer ""))))
      (when (and (stringp line) (not (string-empty-p line)))
        (let ((resp (condition-case err
                        (nelisp-server-call-json line)
                      (error
                       (json-encode
                        (list (cons 'jsonrpc "2.0")
                              (cons 'id nil)
                              (cons 'error
                                    (list (cons 'code -32700)
                                          (cons 'message
                                                (format "parse: %S" err))))))))))
          (when resp
            (princ resp)
            (terpri))))))
  (nelisp-server-stop))

;; -- §3.2 deftool macro expansion shape ------------------------------

(defmacro nelisp-deftool (name &rest spec-and-handler)
  (let* ((description (plist-get spec-and-handler :description))
         (input-schema (plist-get spec-and-handler :input-schema))
         (handler (plist-get spec-and-handler :handler))
         (name-str (symbol-name name)))
    `(progn
       (when (gethash ,name-str nelisp-server--tool-registry)
         (error "duplicate nelisp-deftool: %s" ,name-str))
       (puthash ,name-str
                (list :description ,description
                      :input-schema ,input-schema
                      :handler ,handler)
                nelisp-server--tool-registry)
       ',name)))

;; -- §3.3 initial tools ---------------------------------------------

(nelisp-deftool file-read
  :description "Read a file; return its content as a string."
  :input-schema '((path . ((type . "string")))
                  (required . ["path"]))
  :handler (lambda (args)
             (let ((path (alist-get 'path args)))
               (with-temp-buffer
                 (insert-file-contents path)
                 (buffer-string)))))

(nelisp-deftool file-outline
  :description "Extract structural outline from an elisp/org/markdown file."
  :input-schema '((path . ((type . "string")))
                  (required . ["path"]))
  :handler (lambda (args)
             (let* ((path (alist-get 'path args))
                    (ext (file-name-extension path))
                    (result nil))
               (with-temp-buffer
                 (insert-file-contents path)
                 (goto-char (point-min))
                 (cond
                  ((member ext '("el"))
                   (while (re-search-forward
                           "^(\\(defun\\|defmacro\\|defvar\\|defconst\\|defcustom\\) +\\([^ ]+\\)"
                           nil t)
                     (push (list :kind (match-string 1)
                                 :name (match-string 2)
                                 :line (line-number-at-pos))
                           result)))
                  ((member ext '("org"))
                   (while (re-search-forward "^\\(\\*+\\) +\\(.+\\)$" nil t)
                     (push (list :level (length (match-string 1))
                                 :title (match-string 2)
                                 :line (line-number-at-pos))
                           result)))
                  ((member ext '("md" "markdown"))
                   (while (re-search-forward "^\\(#+\\) +\\(.+\\)$" nil t)
                     (push (list :level (length (match-string 1))
                                 :title (match-string 2)
                                 :line (line-number-at-pos))
                           result)))))
               (list :outline (nreverse result) :count (length result)))))

(nelisp-deftool git-log
  :description "Return recent git commits under cwd."
  :input-schema '((limit . ((type . "integer"))))
  :handler (lambda (args)
             (let* ((limit (or (alist-get 'limit args) 10))
                    (proc (make-process
                           :name "nelisp-git-log"
                           :command (list "git" "log"
                                          "--oneline"
                                          (format "-%d" limit))
                           :buffer (generate-new-buffer " *nelisp-git*"))))
               (while (process-live-p proc)
                 (accept-process-output proc 0.1))
               (let ((buf (process-buffer proc)))
                 (unwind-protect
                     (with-current-buffer buf
                       (list :log (buffer-string)
                             :exit (process-exit-status proc)))
                   (kill-buffer buf))))))

(nelisp-deftool git-status
  :description "Return git status --short under cwd."
  :input-schema '()
  :handler (lambda (_args)
             (let* ((proc (make-process
                           :name "nelisp-git-st"
                           :command (list "git" "status" "--short")
                           :buffer (generate-new-buffer " *nelisp-git-st*"))))
               (while (process-live-p proc)
                 (accept-process-output proc 0.1))
               (let ((buf (process-buffer proc)))
                 (unwind-protect
                     (with-current-buffer buf
                       (list :status (buffer-string)
                             :exit (process-exit-status proc)))
                   (kill-buffer buf))))))

(nelisp-deftool http-get
  :description "GET an HTTP URL; returns status + body."
  :input-schema '((url . ((type . "string")))
                  (required . ["url"]))
  :handler (lambda (args)
             (let ((url (alist-get 'url args)))
               (nelisp-http-get url))))

(nelisp-deftool data-get-path
  :description "Get a nested value from a global plist-tree by dotted key path."
  :input-schema '((path . ((type . "string"))))
  :handler (lambda (args)
             (let* ((raw (alist-get 'path args))
                    (parts (split-string raw "\\.")))
               (cl-reduce
                (lambda (acc k) (plist-get acc (intern (concat ":" k))))
                parts
                :initial-value nelisp-server--data-store))))

(nelisp-deftool data-set-path
  :description "Set a nested value in a global plist-tree."
  :input-schema '((path . ((type . "string")))
                  (value . ((type . "string"))))
  :handler (lambda (args)
             (let* ((raw (alist-get 'path args))
                    (value (alist-get 'value args))
                    (parts (mapcar (lambda (k) (intern (concat ":" k)))
                                   (split-string raw "\\."))))
               (setq nelisp-server--data-store
                     (nelisp-server--plist-tree-set
                      nelisp-server--data-store parts value))
               (list :ok t))))

(defvar nelisp-server--data-store nil)

(defun nelisp-server--plist-tree-set (tree path value)
  (cond
   ((null path) value)
   (t
    (let ((k (car path))
          (rest (cdr path)))
      (plist-put tree k
                 (nelisp-server--plist-tree-set
                  (plist-get tree k) rest value))))))

(provide 'nelisp-server-sketch)
;;; nelisp-server-sketch.el ends here
