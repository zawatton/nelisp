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

  t)

;; Auto-register at load time so `require 'nelisp-tools' is enough
;; to make all tools visible via tools/list.
(nelisp-tools-register-all)

(provide 'nelisp-tools)

;;; nelisp-tools.el ends here
