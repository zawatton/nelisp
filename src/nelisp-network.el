;;; nelisp-network.el --- NeLisp network / HTTP substrate -*- lexical-binding: t; -*-
;;
;; Phase 5-C.2 per Doc 14.  Thin wrapper over host
;; `make-network-process' / `open-network-stream' plus a minimal
;; HTTP client (GET / HEAD + ETag / TTL cache) on top of the host
;; `url' library.
;;
;; Scope per Doc 14 §2.4 A + §2.5 A:
;;   - TLS stack is NOT reimplemented: we forward `:type 'tls' to
;;     host `open-network-stream' which goes through GnuTLS / NSS.
;;   - HTTP request building / redirect / coding conversion is
;;     delegated to host `url-retrieve-synchronously'; we layer
;;     per-URL cache + ETag revalidation on top.

;;; Code:

(require 'cl-lib)
(require 'nelisp-eval)
(require 'nelisp-actor)
(require 'url)
(autoload 'nelisp-make-event "nelisp-eventloop")

;;; Network layer ----------------------------------------------------

(cl-defstruct (nelisp-network
               (:constructor nelisp-network--make)
               (:copier nil))
  id
  host-conn         ; underlying host network process
  (actor nil)
  (status 'open)
  user-sentinel
  user-filter
  (props nil))

(defvar nelisp-network--registry nil
  "List of live `nelisp-network' wraps.")

(defun nelisp-network--reset-registry ()
  "Drop every wrap from the registry (test helper)."
  (setq nelisp-network--registry nil))

(defun nelisp-network--find-by-host (host-conn)
  (cl-find host-conn nelisp-network--registry
           :key #'nelisp-network-host-conn :test #'eq))

(defun nelisp-network--post-event (wrap kind data)
  (let ((actor (nelisp-network-actor wrap)))
    (when (and actor
               (nelisp-actor-p actor)
               (fboundp 'nelisp-make-event)
               (not (memq (nelisp-actor-status actor)
                          '(:dead :crashed))))
      (nelisp-send actor (nelisp-make-event kind data)))))

(defun nelisp-network--host-sentinel-trampoline (host-conn event)
  (let ((wrap (nelisp-network--find-by-host host-conn)))
    (when wrap
      (setf (nelisp-network-status wrap) (process-status host-conn))
      (nelisp-network--post-event wrap 'network-state
                                  (list :wrap wrap :event event))
      (let ((sen (nelisp-network-user-sentinel wrap)))
        (when sen
          (condition-case err
              (nelisp--apply sen (list wrap event))
            (error (message "nelisp-network sentinel error: %S"
                            err))))))))

(defun nelisp-network--host-filter-trampoline (host-conn chunk)
  (let ((wrap (nelisp-network--find-by-host host-conn)))
    (when wrap
      (nelisp-network--post-event wrap 'network-output
                                  (list :wrap wrap :chunk chunk))
      (let ((flt (nelisp-network-user-filter wrap)))
        (when flt
          (condition-case err
              (nelisp--apply flt (list wrap chunk))
            (error (message "nelisp-network filter error: %S"
                            err))))))))

(cl-defun nelisp-open-network (&key name host service (type 'tcp)
                                    coding sentinel filter
                                    actor props buffer)
  "Open a network connection and return a `nelisp-network' wrap.

Arguments:
  NAME     Connection name.  Default \"nelisp-net\".
  HOST     Remote host (string).  Required.
  SERVICE  Port (integer) or service name.  Required.
  TYPE     `tcp' (default) or `tls'.  Forwarded to host.
  CODING   Default `utf-8'.
  SENTINEL / FILTER  As in `nelisp-make-process': dispatched via
                     `nelisp--apply', may be host or NeLisp closures.
  ACTOR    Optional Phase 4 actor to receive `network-state' /
           `network-output' `nelisp-event' messages.
  PROPS    Initial plist for `nelisp-network-get'/`-put'.
  BUFFER   Optional host buffer for inbound bytes."
  (unless host (signal 'wrong-type-argument (list 'stringp host)))
  (unless service (signal 'wrong-type-argument (list 'integerp service)))
  (let* ((name (or name "nelisp-net"))
         (conn
          (pcase type
            ('tls
             (open-network-stream
              (format "%s" name)
              buffer host service :type 'tls :coding (or coding 'utf-8)))
            (_
             (make-network-process
              :name (format "%s" name)
              :host host :service service
              :coding (or coding 'utf-8)
              :buffer buffer
              :sentinel #'nelisp-network--host-sentinel-trampoline
              :filter #'nelisp-network--host-filter-trampoline))))
         (wrap (nelisp-network--make
                :id name
                :host-conn conn
                :actor actor
                :status (process-status conn)
                :user-sentinel sentinel
                :user-filter filter
                :props props)))
    ;; When `open-network-stream' built a TLS connection the trampoline
    ;; wasn't attached at creation time — attach it now so the same
    ;; dispatch semantics hold.
    (when (eq type 'tls)
      (set-process-sentinel conn
                            #'nelisp-network--host-sentinel-trampoline)
      (set-process-filter conn
                          #'nelisp-network--host-filter-trampoline))
    (push wrap nelisp-network--registry)
    wrap))

(defun nelisp-network-send-string (wrap s)
  (process-send-string (nelisp-network-host-conn wrap) s))

(defun nelisp-network-close (wrap)
  (let ((conn (nelisp-network-host-conn wrap)))
    (when (processp conn)
      (ignore-errors (delete-process conn))))
  (setq nelisp-network--registry (delq wrap nelisp-network--registry))
  (setf (nelisp-network-status wrap) 'closed)
  nil)

(defun nelisp-network-live-p (wrap)
  (process-live-p (nelisp-network-host-conn wrap)))

(defun nelisp-network-list ()
  (copy-sequence nelisp-network--registry))

(defun nelisp-network-get (wrap key)
  (plist-get (nelisp-network-props wrap) key))

(defun nelisp-network-put (wrap key value)
  (setf (nelisp-network-props wrap)
        (plist-put (nelisp-network-props wrap) key value))
  value)

;;; HTTP client ------------------------------------------------------

(defvar nelisp-http--cache (make-hash-table :test 'equal)
  "URL -> plist (:status :body :headers :etag :last-modified
:fetched-at :ttl).")

(defun nelisp-http-cache-clear ()
  "Discard every cached HTTP response."
  (clrhash nelisp-http--cache))

(defun nelisp-http-cache-size ()
  "Number of cached HTTP responses."
  (hash-table-count nelisp-http--cache))

(defun nelisp-http--cache-fresh-p (entry)
  "Return t if ENTRY's :ttl has not elapsed since :fetched-at."
  (let ((ttl (plist-get entry :ttl))
        (fetched (plist-get entry :fetched-at)))
    (and (numberp ttl) (numberp fetched)
         (< (- (float-time) fetched) ttl))))

(defun nelisp-http--parse-buffer (buf)
  "Extract (:status :headers :body) from a `url-retrieve-synchronously'
result buffer BUF."
  (with-current-buffer buf
    (goto-char (point-min))
    (let ((status nil)
          (headers nil)
          (body nil))
      (when (re-search-forward "^HTTP/[0-9.]+ \\([0-9]+\\)" nil t)
        (setq status (string-to-number (match-string 1))))
      (goto-char (point-min))
      (forward-line 1)
      (let ((hdr-start (point))
            (hdr-end (and (re-search-forward "^\r?$" nil t)
                          (match-beginning 0))))
        (when hdr-end
          (let ((hstr (buffer-substring-no-properties hdr-start hdr-end)))
            (dolist (line (split-string hstr "\r?\n" t))
              (when (string-match "\\`\\([A-Za-z0-9-]+\\):[ \t]*\\(.*\\)\\'"
                                  line)
                (push (cons (downcase (match-string 1 line))
                            (match-string 2 line))
                      headers))))
          (goto-char hdr-end)
          (forward-line 1)
          (setq body (buffer-substring-no-properties (point) (point-max)))))
      (list :status status
            :headers (nreverse headers)
            :body body))))

(cl-defun nelisp-http-get (url &key headers timeout cache-ttl)
  "Fetch URL via host `url-retrieve-synchronously' with ETag/TTL cache.

Returns a plist (:status :body :headers :etag :cached).

When CACHE-TTL is a number, responses are cached for CACHE-TTL
seconds; a subsequent call within the TTL window returns the
cached body with :cached = t.  After the window, if an ETag was
captured, the request issues `If-None-Match' for 304 short-circuit.

HEADERS is an alist of (NAME . VALUE) appended verbatim.  TIMEOUT
(seconds) defaults to 10.  The TLS path is host-handled; this
wrapper is content-agnostic."
  (let* ((cache (gethash url nelisp-http--cache))
         (ttl (or cache-ttl 0))
         (url-request-method "GET")
         (url-request-timeout (or timeout 10)))
    (cond
     ;; 1. Fresh TTL hit.
     ((and cache (nelisp-http--cache-fresh-p cache))
      (append cache (list :cached t)))
     (t
      (let* ((etag (and cache (plist-get cache :etag)))
             (url-request-extra-headers
              (append (and etag (list (cons "If-None-Match" etag)))
                      headers))
             (buf (ignore-errors
                    (url-retrieve-synchronously url nil t
                                                (or timeout 10)))))
        (cond
         ((null buf)
          (or cache
              (list :status nil :body nil :headers nil :cached nil)))
         (t
          (let* ((parsed (nelisp-http--parse-buffer buf))
                 (status (plist-get parsed :status))
                 (hdrs (plist-get parsed :headers))
                 (etag-header (cdr (assoc "etag" hdrs)))
                 (lm-header (cdr (assoc "last-modified" hdrs)))
                 (entry (list :status status
                              :body (plist-get parsed :body)
                              :headers hdrs
                              :etag etag-header
                              :last-modified lm-header
                              :fetched-at (float-time)
                              :ttl ttl)))
            (ignore-errors (kill-buffer buf))
            (cond
             ;; 304 — reuse cached body.
             ((and (eq status 304) cache)
              (let ((revived (copy-sequence cache)))
                (setq revived (plist-put revived :fetched-at
                                         (float-time)))
                (setq revived (plist-put revived :ttl ttl))
                (puthash url revived nelisp-http--cache)
                (append revived (list :cached t))))
             (t
              (puthash url entry nelisp-http--cache)
              (append entry (list :cached nil))))))))))))

(cl-defun nelisp-http-head (url &key headers timeout)
  "Fetch URL via HEAD; return a plist (:status :headers)."
  (let* ((url-request-method "HEAD")
         (url-request-extra-headers headers)
         (url-request-timeout (or timeout 10))
         (buf (ignore-errors
                (url-retrieve-synchronously url nil t
                                            (or timeout 10)))))
    (if (null buf)
        (list :status nil :headers nil)
      (let ((parsed (nelisp-http--parse-buffer buf)))
        (ignore-errors (kill-buffer buf))
        (list :status (plist-get parsed :status)
              :headers (plist-get parsed :headers))))))

(provide 'nelisp-network)
;;; nelisp-network.el ends here
