;;; nelisp-tools.el --- Initial MCP tool port for anvil-on-nelisp -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Phase 5-E §3.3 per Doc 16.  The initial read-only MCP tool set
;; registered on top of `nelisp-server.el' via `nelisp-deftool'
;; (§3.2).  Covers the smallest useful anvil-on-nelisp surface:
;; reading files, listing recent git commits, pulling a URL, and
;; poking at a NeLisp-local key/value plist tree.
;;
;; Tools registered (§3.3 table from Doc 16):
;;
;;   file-read        Read a file, return its content
;;   file-outline     Structural outline for .el / .org / .md
;;   git-log          Recent commits (--oneline -N)
;;   git-status       git status --short
;;   http-get         GET a URL (wraps `nelisp-http-get')
;;   data-get-path    Nested lookup in `nelisp-tools--data-store'
;;   data-set-path    Nested write into `nelisp-tools--data-store'
;;
;; Phase 5-F.1.2 per Doc 17 §2.7 A (state-* via anvil-state port):
;;
;;   state-set        Persist VAL under KEY (:ns / :ttl)
;;   state-get        Fetch value (:ns / :default)
;;   state-delete     Remove KEY (:ns); returns t/nil
;;   state-list-ns    List namespaces currently holding rows
;;   state-list-keys  Keys under a namespace (:limit)
;;   state-count      Row count, optionally filtered by NS
;;
;; Non-goals (Phase 5-E §2.4 A: read-only):
;;   - No destructive file operations (replace / write / Edit).
;;   - No worker-backed heavy tools (ert-run / offload-eval etc.).
;;   - No subprocess pool; git tools spawn one-off host processes.
;;
;; `nelisp-tools-register-all' auto-runs at load time.  Tests that
;; clear the registry in `nelisp-server--tool-registry' must
;; re-invoke `nelisp-tools-register-all' before asserting on the
;; presence or behaviour of any tool here.

;;; Code:

(require 'cl-lib)
(require 'nelisp-server)
(require 'nelisp-network)
(require 'nelisp-state)
(require 'nelisp-http)

;;; Data-store (scratch KV used by data-get/set-path) ---------------

(defvar nelisp-tools--data-store nil
  "In-memory plist tree backing the data-get-path / data-set-path
tools.  Keys inside the tree are keyword symbols; callers address
them via dotted string paths (\"a.b.c\").")

(defun nelisp-tools--split-dotted-path (raw)
  "Split RAW dotted string path into a list of keyword symbols.
Empty components error, matching the strict JSON-RPC input shape."
  (unless (stringp raw)
    (error "data-*: path must be a string, got %S" raw))
  (let ((parts (split-string raw "\\.")))
    (when (or (null parts)
              (cl-some #'string-empty-p parts))
      (error "data-*: invalid path %S" raw))
    (mapcar (lambda (k) (intern (concat ":" k))) parts)))

(defun nelisp-tools--plist-tree-get (tree keys)
  "Recursively descend KEYS (a list of keyword symbols) into
TREE (a plist)."
  (cond
   ((null keys) tree)
   (t (nelisp-tools--plist-tree-get
       (plist-get tree (car keys)) (cdr keys)))))

(defun nelisp-tools--plist-tree-set (tree keys value)
  "Return a copy of TREE with VALUE installed at path KEYS."
  (cond
   ((null keys) value)
   (t
    (let* ((k (car keys))
           (rest (cdr keys))
           (child (plist-get tree k))
           (new-child (nelisp-tools--plist-tree-set child rest value)))
      (plist-put (or tree (list k nil)) k new-child)))))

(defun nelisp-tools--data-store-reset (&optional value)
  "Reset the data-store to VALUE (default nil) — primarily a test
affordance; not exposed as an MCP tool."
  (setq nelisp-tools--data-store value))

(defun nelisp-tools--plist-tree-keys (tree)
  "Return TREE's keyword symbols (or nil).
Phase 5-F.2.3 helper for `data-list-keys'."
  (when (and tree (listp tree))
    (let (keys (cur tree))
      (while cur
        (push (car cur) keys)
        (setq cur (cddr cur)))
      (nreverse keys))))

(defun nelisp-tools--org-flat-headlines (path)
  "Scan PATH and return flat list of (:level :title :line) entries.
Phase 5-F.3.1 helper for `org-read-outline-tree' — pure regex,
no host org-mode dependency."
  (with-temp-buffer
    (insert-file-contents path)
    (goto-char (point-min))
    (let (entries)
      (while (re-search-forward
              "^\\(\\*+\\)[[:space:]]+\\(.+\\)$"
              nil t)
        (push (list :level (length (match-string 1))
                    :title (match-string 2)
                    :line (line-number-at-pos))
              entries))
      (nreverse entries))))

(defun nelisp-tools--org-build-tree (entries)
  "Convert flat ENTRIES into nested tree by stack-walking levels.
Returns a list of root plists, each with a :children list (recursive)."
  (let* ((root (list :level 0 :title nil :line 0 :children nil))
         (stack (list root)))
    (dolist (entry entries)
      (let ((lvl (plist-get entry :level)))
        (while (>= (plist-get (car stack) :level) lvl)
          (pop stack))
        (let* ((node (list :level lvl
                           :title (plist-get entry :title)
                           :line (plist-get entry :line)
                           :children nil))
               (parent (car stack)))
          (setf (plist-get parent :children)
                (append (plist-get parent :children) (list node)))
          (push node stack))))
    (plist-get root :children)))

;;; Subprocess helper (git tools) -----------------------------------

(defun nelisp-tools--run-command (name command)
  "Run COMMAND (list of strings) synchronously via `make-process',
capture stdout+stderr into a temp buffer, and return plist
 (:output STR :exit INT).

Installs `#\\='ignore' as the sentinel to suppress the default
\"Process ... finished\" message that would otherwise be appended
to the capture buffer."
  (let* ((buf (generate-new-buffer (format " *nelisp-tool-%s*" name)))
         (proc (make-process
                :name (format "nelisp-tool-%s" name)
                :command command
                :buffer buf
                :noquery t
                :sentinel #'ignore)))
    (unwind-protect
        (progn
          (while (process-live-p proc)
            (accept-process-output proc 0.2))
          (with-current-buffer buf
            (list :output (buffer-string)
                  :exit (process-exit-status proc))))
      (when (buffer-live-p buf) (kill-buffer buf)))))

;;; Tool definitions ------------------------------------------------

(defun nelisp-tools-register-all ()
  "Register every Phase 5-E §3.3 initial MCP tool on
`nelisp-server--tool-registry'.  Re-callable after a registry
clear; will error if any tool is still registered."

  (nelisp-deftool file-read
    :description "Read a file from disk and return its contents."
    :input-schema (list :type "object"
                        :properties
                        (list :path (list :type "string"
                                          :description "Absolute file path"))
                        :required ["path"])
    :handler (lambda (args)
               (let ((path (alist-get 'path args)))
                 (unless (stringp path)
                   (error "file-read: missing string `path'"))
                 (with-temp-buffer
                   (insert-file-contents path)
                   (list :path path
                         :content (buffer-string))))))

  (nelisp-deftool file-outline
    :description "Extract a structural outline from an .el / .org / .md file."
    :input-schema (list :type "object"
                        :properties
                        (list :path (list :type "string"
                                          :description "Absolute file path"))
                        :required ["path"])
    :handler (lambda (args)
               (let* ((path (alist-get 'path args))
                      (ext (file-name-extension (or path "")))
                      (entries nil))
                 (unless (stringp path)
                   (error "file-outline: missing string `path'"))
                 (with-temp-buffer
                   (insert-file-contents path)
                   (goto-char (point-min))
                   (cond
                    ((member ext '("el"))
                     (while (re-search-forward
                             "^(\\(def[a-z*-]+\\)[[:space:]]+\\([^[:space:]()]+\\)"
                             nil t)
                       (push (list :kind (match-string 1)
                                   :name (match-string 2)
                                   :line (line-number-at-pos))
                             entries)))
                    ((member ext '("org"))
                     (while (re-search-forward
                             "^\\(\\*+\\)[[:space:]]+\\(.+\\)$"
                             nil t)
                       (push (list :level (length (match-string 1))
                                   :title (match-string 2)
                                   :line (line-number-at-pos))
                             entries)))
                    ((member ext '("md" "markdown"))
                     (while (re-search-forward
                             "^\\(#+\\)[[:space:]]+\\(.+\\)$"
                             nil t)
                       (push (list :level (length (match-string 1))
                                   :title (match-string 2)
                                   :line (line-number-at-pos))
                             entries)))
                    (t
                     (error "file-outline: unsupported extension %S" ext))))
                 (list :path path
                       :format ext
                       :count (length entries)
                       :outline (nreverse entries)))))

  (nelisp-deftool git-log
    :description "Return recent git commits (oneline) under cwd."
    :input-schema (list :type "object"
                        :properties
                        (list :limit (list :type "integer"
                                           :description "Max commits (default 10)")))
    :handler (lambda (args)
               (let* ((limit (or (alist-get 'limit args) 10))
                      (res (nelisp-tools--run-command
                            "git-log"
                            (list "git" "-c" "color.ui=never"
                                  "--no-pager" "log" "--oneline"
                                  (format "-%d" limit)))))
                 (list :limit limit
                       :log (plist-get res :output)
                       :exit (plist-get res :exit)))))

  (nelisp-deftool git-status
    :description "Return `git status --short' under cwd."
    :input-schema (list :type "object"
                        :properties (make-hash-table :test 'equal))
    :handler (lambda (_args)
               (let ((res (nelisp-tools--run-command
                           "git-status"
                           (list "git" "-c" "color.ui=never"
                                 "--no-pager" "status" "--short"))))
                 (list :status (plist-get res :output)
                       :exit (plist-get res :exit)))))

  (nelisp-deftool http-get
    :description "HTTP GET — wraps `nelisp-http-get' with default cache TTL."
    :input-schema (list :type "object"
                        :properties
                        (list :url (list :type "string"
                                         :description "HTTP or HTTPS URL"))
                        :required ["url"])
    :handler (lambda (args)
               (let ((url (alist-get 'url args)))
                 (unless (stringp url)
                   (error "http-get: missing string `url'"))
                 (nelisp-http-get url))))

  (nelisp-deftool data-get-path
    :description "Return nested value at dotted key PATH in data-store."
    :input-schema (list :type "object"
                        :properties
                        (list :path (list :type "string"
                                          :description "Dotted key path e.g. foo.bar"))
                        :required ["path"])
    :handler (lambda (args)
               (let* ((raw (alist-get 'path args))
                      (keys (nelisp-tools--split-dotted-path raw))
                      (value (nelisp-tools--plist-tree-get
                              nelisp-tools--data-store keys)))
                 (list :path raw :value value))))

  (nelisp-deftool data-set-path
    :description "Store VALUE at nested dotted key PATH in data-store."
    :input-schema (list :type "object"
                        :properties
                        (list :path (list :type "string")
                              :value (list :description "any JSON value"))
                        :required ["path" "value"])
    :handler (lambda (args)
               (let* ((raw (alist-get 'path args))
                      (value (alist-get 'value args))
                      (keys (nelisp-tools--split-dotted-path raw)))
                 (setq nelisp-tools--data-store
                       (nelisp-tools--plist-tree-set
                        nelisp-tools--data-store keys value))
                 (list :path raw :stored t))))

  ;; Phase 5-F.1.2 — state-* tools (SQLite-backed KV via nelisp-state)

  (nelisp-deftool state-set
    :description "Persist VAL under KEY in namespace NS (default \"default\"), optional TTL seconds."
    :input-schema (list :type "object"
                        :properties
                        (list :key (list :type "string"
                                         :description "Row key, non-empty")
                              :value (list :description "Any readable Lisp value")
                              :ns (list :type "string"
                                        :description "Namespace (default \"default\")")
                              :ttl (list :type "integer"
                                         :description "Seconds until expiry (nil = never)"))
                        :required ["key" "value"])
    :handler (lambda (args)
               (let* ((key (alist-get 'key args))
                      (value (alist-get 'value args))
                      (ns (alist-get 'ns args))
                      (ttl (alist-get 'ttl args))
                      (opts (append (and ns (list :ns ns))
                                    (and ttl (list :ttl ttl)))))
                 (nelisp-state-set key value opts)
                 (list :key key :ns (or ns "default") :stored t))))

  (nelisp-deftool state-get
    :description "Fetch value under KEY in namespace NS, or DEFAULT when absent/expired."
    :input-schema (list :type "object"
                        :properties
                        (list :key (list :type "string")
                              :ns (list :type "string"
                                        :description "Namespace (default \"default\")")
                              :default (list :description "Returned when KEY missing"))
                        :required ["key"])
    :handler (lambda (args)
               (let* ((key (alist-get 'key args))
                      (ns (alist-get 'ns args))
                      (has-default (assq 'default args))
                      (default (alist-get 'default args))
                      (opts (append (and ns (list :ns ns))
                                    (and has-default (list :default default))))
                      (value (nelisp-state-get key opts)))
                 (list :key key :ns (or ns "default") :value value))))

  (nelisp-deftool state-delete
    :description "Remove KEY from namespace NS; returns :deleted t/nil."
    :input-schema (list :type "object"
                        :properties
                        (list :key (list :type "string")
                              :ns (list :type "string"))
                        :required ["key"])
    :handler (lambda (args)
               (let* ((key (alist-get 'key args))
                      (ns (alist-get 'ns args))
                      (opts (and ns (list :ns ns)))
                      (ok (nelisp-state-delete key opts)))
                 (list :key key :ns (or ns "default") :deleted (and ok t)))))

  (nelisp-deftool state-list-ns
    :description "List namespaces currently holding rows, sorted."
    :input-schema (list :type "object"
                        :properties (make-hash-table :test 'equal))
    :handler (lambda (_args)
               (list :namespaces (nelisp-state-list-ns))))

  (nelisp-deftool state-list-keys
    :description "Sorted keys under namespace NS. Optional LIMIT."
    :input-schema (list :type "object"
                        :properties
                        (list :ns (list :type "string"
                                        :description "Namespace, required")
                              :limit (list :type "integer"
                                           :description "Max rows to return"))
                        :required ["ns"])
    :handler (lambda (args)
               (let* ((ns (alist-get 'ns args))
                      (limit (alist-get 'limit args))
                      (opts (append (list :ns ns)
                                    (and limit (list :limit limit)))))
                 (list :ns ns :keys (nelisp-state-list-keys opts)))))

  (nelisp-deftool state-count
    :description "Row count (total, or within NS)."
    :input-schema (list :type "object"
                        :properties
                        (list :ns (list :type "string"
                                        :description "Optional namespace filter")))
    :handler (lambda (args)
               (let* ((ns (alist-get 'ns args))
                      (count (nelisp-state-count ns)))
                 (list :ns (or ns :all) :count count))))

  ;; Phase 5-F.2.1 — file ops (Doc 19)

  (nelisp-deftool file-replace-string
    :description "Replace OLD substring with NEW in PATH; returns :replaced count."
    :input-schema (list :type "object"
                        :properties
                        (list :path (list :type "string")
                              :old (list :type "string")
                              :new (list :type "string"))
                        :required ["path" "old" "new"])
    :handler (lambda (args)
               (let* ((path (alist-get 'path args))
                      (old (alist-get 'old args))
                      (new (alist-get 'new args)))
                 (unless (and (stringp path) (stringp old) (stringp new))
                   (error "file-replace-string: missing string args"))
                 (when (string-empty-p old)
                   (error "file-replace-string: `old' must be non-empty"))
                 (with-temp-buffer
                   (insert-file-contents path)
                   (goto-char (point-min))
                   (let ((count 0))
                     (while (search-forward old nil t)
                       (replace-match new t t)
                       (setq count (1+ count)))
                     (write-region (point-min) (point-max) path)
                     (list :path path :replaced count))))))

  (nelisp-deftool file-insert-at-line
    :description "Insert CONTENT before LINE (1-indexed) in PATH."
    :input-schema (list :type "object"
                        :properties
                        (list :path (list :type "string")
                              :line (list :type "integer"
                                          :description "1-indexed line")
                              :content (list :type "string"))
                        :required ["path" "line" "content"])
    :handler (lambda (args)
               (let* ((path (alist-get 'path args))
                      (line (alist-get 'line args))
                      (content (alist-get 'content args)))
                 (unless (and (stringp path) (integerp line) (stringp content))
                   (error "file-insert-at-line: missing or wrong-type args"))
                 (when (< line 1)
                   (error "file-insert-at-line: `line' must be >= 1"))
                 (with-temp-buffer
                   (insert-file-contents path)
                   (goto-char (point-min))
                   (forward-line (1- line))
                   (insert content)
                   (unless (or (string-empty-p content)
                               (string-suffix-p "\n" content))
                     (insert "\n"))
                   (write-region (point-min) (point-max) path)
                   (list :path path :line line :inserted t)))))

  (nelisp-deftool file-delete-lines
    :description "Delete COUNT lines starting at LINE (1-indexed) in PATH."
    :input-schema (list :type "object"
                        :properties
                        (list :path (list :type "string")
                              :line (list :type "integer")
                              :count (list :type "integer"))
                        :required ["path" "line" "count"])
    :handler (lambda (args)
               (let* ((path (alist-get 'path args))
                      (line (alist-get 'line args))
                      (count (alist-get 'count args)))
                 (unless (and (stringp path) (integerp line) (integerp count))
                   (error "file-delete-lines: wrong-type args"))
                 (when (or (< line 1) (< count 1))
                   (error "file-delete-lines: `line' and `count' must be >= 1"))
                 (with-temp-buffer
                   (insert-file-contents path)
                   (goto-char (point-min))
                   (forward-line (1- line))
                   (let ((start (point))
                         (end (progn (forward-line count) (point))))
                     (delete-region start end)
                     (write-region (point-min) (point-max) path)
                     (list :path path :line line :deleted count))))))

  (nelisp-deftool file-append
    :description "Append CONTENT to end of PATH (no auto trailing newline)."
    :input-schema (list :type "object"
                        :properties
                        (list :path (list :type "string")
                              :content (list :type "string"))
                        :required ["path" "content"])
    :handler (lambda (args)
               (let* ((path (alist-get 'path args))
                      (content (alist-get 'content args)))
                 (unless (and (stringp path) (stringp content))
                   (error "file-append: missing string args"))
                 (write-region content nil path 'append 'silent)
                 (list :path path :appended (length content)))))

  (nelisp-deftool file-read-snippet
    :description "Read WINDOW lines centred on LINE in PATH (default WINDOW=20, max 100)."
    :input-schema (list :type "object"
                        :properties
                        (list :path (list :type "string")
                              :line (list :type "integer")
                              :window (list :type "integer"))
                        :required ["path" "line"])
    :handler (lambda (args)
               (let* ((path (alist-get 'path args))
                      (line (alist-get 'line args))
                      (raw-window (or (alist-get 'window args) 20))
                      (window (min raw-window 100)))
                 (unless (and (stringp path) (integerp line))
                   (error "file-read-snippet: wrong-type args"))
                 (when (< line 1)
                   (error "file-read-snippet: `line' must be >= 1"))
                 (with-temp-buffer
                   (insert-file-contents path)
                   (let* ((total (count-lines (point-min) (point-max)))
                          (half (/ window 2))
                          (start (max 1 (- line half)))
                          (end (min total (+ line half)))
                          (extracted
                           (progn
                             (goto-char (point-min))
                             (forward-line (1- start))
                             (let ((bol (point)))
                               (forward-line (1+ (- end start)))
                               (buffer-substring-no-properties bol (point))))))
                     (list :path path
                           :start start :end end
                           :total-lines total
                           :content extracted))))))

  ;; Phase 5-F.2.2 — git ops (Doc 19)

  (nelisp-deftool git-diff-names
    :description "List changed file paths. With REV, since REV; without, working tree changes."
    :input-schema (list :type "object"
                        :properties
                        (list :rev (list :type "string"
                                         :description "Optional revision (e.g. HEAD~1)")))
    :handler (lambda (args)
               (let* ((rev (alist-get 'rev args))
                      (cmd (if rev
                               (list "git" "-c" "color.ui=never"
                                     "--no-pager" "diff" "--name-only" rev)
                             (list "git" "-c" "color.ui=never"
                                   "--no-pager" "diff" "--name-only")))
                      (res (nelisp-tools--run-command "git-diff-names" cmd))
                      (out (or (plist-get res :output) ""))
                      (files (split-string out "\n" t "[ \t]+")))
                 (list :rev (or rev :working-tree)
                       :files files
                       :exit (plist-get res :exit)))))

  (nelisp-deftool git-head-sha
    :description "Return the full SHA of the current HEAD commit."
    :input-schema (list :type "object"
                        :properties (make-hash-table :test 'equal))
    :handler (lambda (_args)
               (let* ((res (nelisp-tools--run-command
                            "git-head-sha"
                            (list "git" "rev-parse" "HEAD")))
                      (sha (string-trim (or (plist-get res :output) ""))))
                 (list :sha sha :exit (plist-get res :exit)))))

  (nelisp-deftool git-branch-current
    :description "Return the current branch (symbolic short name)."
    :input-schema (list :type "object"
                        :properties (make-hash-table :test 'equal))
    :handler (lambda (_args)
               (let* ((res (nelisp-tools--run-command
                            "git-branch-current"
                            (list "git" "rev-parse" "--abbrev-ref" "HEAD")))
                      (branch (string-trim (or (plist-get res :output) ""))))
                 (list :branch branch :exit (plist-get res :exit)))))

  (nelisp-deftool git-repo-root
    :description "Return the absolute path of the git repo root (toplevel)."
    :input-schema (list :type "object"
                        :properties (make-hash-table :test 'equal))
    :handler (lambda (_args)
               (let* ((res (nelisp-tools--run-command
                            "git-repo-root"
                            (list "git" "rev-parse" "--show-toplevel")))
                      (root (string-trim (or (plist-get res :output) ""))))
                 (list :root root :exit (plist-get res :exit)))))

  ;; Phase 5-F.2.3 — data ops (Doc 19)

  (nelisp-deftool data-list-keys
    :description "Return keys at PATH in data-store (top-level if PATH absent)."
    :input-schema (list :type "object"
                        :properties
                        (list :path (list :type "string"
                                          :description "Optional dotted path")))
    :handler (lambda (args)
               (let* ((raw (alist-get 'path args))
                      (subtree (if raw
                                   (nelisp-tools--plist-tree-get
                                    nelisp-tools--data-store
                                    (nelisp-tools--split-dotted-path raw))
                                 nelisp-tools--data-store))
                      (keys (nelisp-tools--plist-tree-keys subtree)))
                 (list :path (or raw "")
                       :keys (mapcar #'symbol-name keys)))))

  (nelisp-deftool data-delete-path
    :description "Set the value at PATH to nil (effectively unset)."
    :input-schema (list :type "object"
                        :properties
                        (list :path (list :type "string"))
                        :required ["path"])
    :handler (lambda (args)
               (let* ((raw (alist-get 'path args))
                      (keys (nelisp-tools--split-dotted-path raw)))
                 (setq nelisp-tools--data-store
                       (nelisp-tools--plist-tree-set
                        nelisp-tools--data-store keys nil))
                 (list :path raw :deleted t))))

  ;; Phase 5-F.3.1 — org-read-outline-tree (Doc 20 §2.5)
  ;; Hierarchical org outline (vs flat `file-outline').  Pure regex,
  ;; no host `org-mode' dependency; stays valid under `emacs --batch -Q'.

  ;; Phase 6.2.3 — anvil-http MVP MCP surface (Doc 24).  Five tools
  ;; expose `nelisp-http-fetch' / `-fetch-head' / cache I/O so non-Emacs
  ;; users can drive the cache-aware HTTP path entirely over MCP.

  (nelisp-deftool http-fetch
    :description "GET URL with TTL cache via nelisp-state ns=\"http\"."
    :input-schema (list :type "object"
                        :properties
                        (list :url (list :type "string"
                                         :description "HTTP/HTTPS URL")
                              :timeout-sec (list :type "integer")
                              :ttl (list :type "integer"
                                         :description "Cache TTL seconds (default 86400)")
                              :no-cache (list :type "boolean"
                                              :description "Skip cache reads + writes"))
                        :required ["url"])
    :handler (lambda (args)
               (let* ((url (alist-get 'url args))
                      (timeout (alist-get 'timeout-sec args))
                      (ttl (alist-get 'ttl args))
                      (no-cache (alist-get 'no-cache args)))
                 (unless (stringp url)
                   (error "http-fetch: missing string `url'"))
                 (apply #'nelisp-http-fetch url
                        (append (and timeout (list :timeout-sec timeout))
                                (and ttl (list :ttl ttl))
                                (and no-cache (list :no-cache t)))))))

  (nelisp-deftool http-head
    :description "HEAD URL — returns :status :headers :final-url :elapsed-ms (no cache)."
    :input-schema (list :type "object"
                        :properties
                        (list :url (list :type "string")
                              :timeout-sec (list :type "integer"))
                        :required ["url"])
    :handler (lambda (args)
               (let ((url (alist-get 'url args))
                     (timeout (alist-get 'timeout-sec args)))
                 (unless (stringp url)
                   (error "http-head: missing string `url'"))
                 (apply #'nelisp-http-fetch-head url
                        (and timeout (list :timeout-sec timeout))))))

  (nelisp-deftool http-cache-get
    :description "Return cached entry plist for URL or nil on miss."
    :input-schema (list :type "object"
                        :properties
                        (list :url (list :type "string"))
                        :required ["url"])
    :handler (lambda (args)
               (let ((url (alist-get 'url args)))
                 (unless (stringp url)
                   (error "http-cache-get: missing string `url'"))
                 (or (nelisp-http-fetch-cache-get url)
                     (list :url url :cached nil)))))

  (nelisp-deftool http-cache-clear
    :description "Drop cached HTTP entries.  With URL, drop one; without, wipe all."
    :input-schema (list :type "object"
                        :properties
                        (list :url (list :type "string"
                                         :description "Optional — single-entry clear")))
    :handler (lambda (args)
               (let* ((url (alist-get 'url args))
                      (result (nelisp-http-fetch-cache-clear url)))
                 (list :url url :cleared (or result t)))))

  (nelisp-deftool http-post
    :description "POST URL with BODY (string / alist→form / plist→json) + optional auth."
    :input-schema (list :type "object"
                        :properties
                        (list :url (list :type "string")
                              :body (list :description
                                          "string / alist of pairs / plist (starts with keyword) / nil")
                              :content-type (list :type "string")
                              :timeout-sec (list :type "integer")
                              :bearer (list :type "string"
                                            :description "Convenience — sets Authorization: Bearer <value>"))
                        :required ["url"])
    :handler (lambda (args)
               (let* ((url (alist-get 'url args))
                      (body (alist-get 'body args))
                      (ct (alist-get 'content-type args))
                      (timeout (alist-get 'timeout-sec args))
                      (bearer (alist-get 'bearer args))
                      (auth (and bearer (list :bearer bearer))))
                 (unless (stringp url)
                   (error "http-post: missing string `url'"))
                 (apply #'nelisp-http-fetch-post url
                        (append (and body (list :body body))
                                (and ct (list :content-type ct))
                                (and timeout (list :timeout-sec timeout))
                                (and auth (list :auth auth)))))))

  (nelisp-deftool http-cache-status
    :description "Summarise the http cache namespace: count + URL list."
    :input-schema (list :type "object"
                        :properties (make-hash-table :test 'equal))
    :handler (lambda (_args)
               (let* ((urls (nelisp-http-fetch-cache-list))
                      (count (length urls)))
                 (list :count count
                       :urls (or urls [])))))

  (nelisp-deftool org-read-outline-tree
    :description "Hierarchical headline tree of an .org file (parent/child structure)."
    :input-schema (list :type "object"
                        :properties
                        (list :path (list :type "string"
                                          :description "Absolute .org file path"))
                        :required ["path"])
    :handler (lambda (args)
               (let* ((path (alist-get 'path args))
                      (ext (file-name-extension (or path ""))))
                 (unless (stringp path)
                   (error "org-read-outline-tree: missing string `path'"))
                 (unless (member ext '("org"))
                   (error "org-read-outline-tree: extension must be .org, got %S" ext))
                 (let* ((flat (nelisp-tools--org-flat-headlines path))
                        (tree (nelisp-tools--org-build-tree flat)))
                   (list :path path
                         :total-headlines (length flat)
                         :tree tree)))))

  t)

;; Auto-register at load time so `require 'nelisp-tools' is enough
;; to make all tools visible via tools/list.
(nelisp-tools-register-all)

(provide 'nelisp-tools)

;;; nelisp-tools.el ends here
