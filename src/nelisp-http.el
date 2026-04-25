;;; nelisp-http.el --- NeLisp HTTP fetch + cache  -*- lexical-binding: t; -*-

;; Phase 6.2.1 — MVP subset port of anvil-http.el (Doc 09 SHIPPED) per
;; Doc 24 §3 plan.  GET only, cache via nelisp-state ns="http", no
;; robots / offload / extract / body-mode / metrics / retry.  Those
;; live in anvil-http and may be ported piecemeal under Phase 6.2.5+.
;;
;; Why a new module instead of growing nelisp-network.el:
;;   nelisp-network's `nelisp-http-get' is a low-level GET-only
;;   primitive used by Phase 5-E's `http-get' MCP tool (smoke test
;;   dependency).  Keep it intact; nelisp-http builds the cache-aware
;;   high-level surface on top of host `url' package via the Phase
;;   6.2.0 primitives.
;;
;; Cache schema (matches anvil-http for cross-tool coexistence):
;;   ns       = "http"
;;   key      = normalized URL (lowercase scheme+host, fragment dropped)
;;   value    = (:status N :headers ALIST :body STR :fetched-at INT
;;               :final-url STR :etag STR :last-modified STR)
;;   ttl      = `nelisp-http-cache-ttl-sec' (default 86400)

;;; Code:

(require 'cl-lib)
(require 'url)
(require 'url-http)
(require 'url-parse)
(require 'nelisp-state)

(defgroup nelisp-http nil
  "NeLisp HTTP fetch + cache."
  :group 'nelisp
  :prefix "nelisp-http-")

(defvar nelisp-http-allowed-schemes '("http" "https")
  "Schemes accepted by `nelisp-http-fetch'.")

(defvar nelisp-http-timeout-sec 30
  "Default timeout (seconds) for synchronous GET.")

(defvar nelisp-http-cache-ttl-sec 86400
  "Default cache TTL (seconds).  Nil disables time-based expiry.")

(defvar nelisp-http-user-agent "nelisp-http/0.1"
  "User-Agent header sent on every request.")

(defvar nelisp-http-max-redirections 5
  "Maximum redirects honoured by `url-retrieve-synchronously'.")

(defconst nelisp-http--state-ns "http"
  "`nelisp-state' namespace for cached HTTP responses.")

;;; URL hygiene -------------------------------------------------------

(defun nelisp-http--check-url (url)
  "Validate URL string and scheme.  Signal `user-error' on failure."
  (unless (and (stringp url) (not (string-empty-p url)))
    (user-error "nelisp-http: URL must be a non-empty string, got %S" url))
  (let* ((parsed (url-generic-parse-url url))
         (scheme (and (url-type parsed) (downcase (url-type parsed)))))
    (unless (member scheme nelisp-http-allowed-schemes)
      (user-error "nelisp-http: scheme %S not in `nelisp-http-allowed-schemes'"
                  scheme))))

(defun nelisp-http--normalize-url (url)
  "Return canonical form of URL for cache lookup.
Lowercases scheme and host, strips fragment, keeps path+query."
  (let ((parsed (url-generic-parse-url url)))
    (when (url-type parsed)
      (setf (url-type parsed) (downcase (url-type parsed))))
    (when (url-host parsed)
      (setf (url-host parsed) (downcase (url-host parsed))))
    (setf (url-target parsed) nil)
    (url-recreate-url parsed)))

;;; Cache I/O ---------------------------------------------------------

(defun nelisp-http--cache-get (norm-url)
  "Return cached response plist for NORM-URL or nil on miss / error."
  (condition-case _err
      (nelisp-state-get norm-url (list :ns nelisp-http--state-ns))
    (error nil)))

(defun nelisp-http--cache-put (norm-url entry &optional ttl)
  "Store ENTRY under NORM-URL with optional TTL seconds."
  (condition-case _err
      (nelisp-state-set
       norm-url entry
       (if ttl (list :ns nelisp-http--state-ns :ttl ttl)
         (list :ns nelisp-http--state-ns)))
    (error nil)))

(defun nelisp-http--cache-delete (norm-url)
  "Drop NORM-URL from cache.  Returns t when a row was removed."
  (condition-case _err
      (nelisp-state-delete norm-url (list :ns nelisp-http--state-ns))
    (error nil)))

(defun nelisp-http--cache-list ()
  "Return list of cached normalized URLs."
  (condition-case _err
      (nelisp-state-list-keys (list :ns nelisp-http--state-ns))
    (error nil)))

(defun nelisp-http--cache-clear ()
  "Wipe every entry in the http namespace.  Returns deletion count."
  (condition-case _err
      (nelisp-state-delete-ns nelisp-http--state-ns)
    (error 0)))

(defun nelisp-http--cache-entry (status headers body fetched-at final-url)
  "Build a cache entry plist."
  (list :status status
        :headers headers
        :body body
        :fetched-at fetched-at
        :final-url final-url
        :etag (plist-get headers :etag)
        :last-modified (plist-get headers :last-modified)))

;;; Response parsing --------------------------------------------------

(defun nelisp-http--parse-response (original-url)
  "Parse the current `url-retrieve-synchronously' buffer into a plist.
Returns (:status :headers :body :final-url).  Caller must `kill-buffer'."
  (let* ((end-headers
          (and (boundp 'url-http-end-of-headers)
               (markerp url-http-end-of-headers)
               (marker-position url-http-end-of-headers)))
         (final
          (or (and (boundp 'url-http-target-url)
                   (let ((u (symbol-value 'url-http-target-url)))
                     (cond ((stringp u) u)
                           ((and u (fboundp 'url-recreate-url))
                            (url-recreate-url u))
                           (t nil))))
              original-url))
         status headers)
    (goto-char (point-min))
    (when (re-search-forward "\\`HTTP/[0-9.]+ +\\([0-9]+\\)"
                             (or end-headers (point-max)) t)
      (setq status (string-to-number (match-string 1))))
    (forward-line 1)
    (while (and end-headers (< (point) end-headers))
      (when (looking-at "^\\([^:\r\n]+\\):[ \t]*\\(.*?\\)[ \t]*\r?$")
        (let ((key (downcase (string-trim (match-string 1))))
              (val (match-string 2)))
          (setq headers (plist-put headers
                                   (intern (concat ":" key))
                                   val))))
      (forward-line 1))
    (let ((body (if end-headers
                    (buffer-substring-no-properties end-headers (point-max))
                  "")))
      (when (and (> (length body) 0)
                 (or (eq (aref body 0) ?\n)
                     (eq (aref body 0) ?\r)))
        (setq body (replace-regexp-in-string "\\`\r?\n" "" body)))
      (list :status status
            :headers headers
            :body body
            :final-url final))))

;;; Network primitive (one-shot, no retry — MVP) ----------------------

(defun nelisp-http--request (method url extra-headers timeout)
  "Issue one METHOD request to URL with EXTRA-HEADERS and TIMEOUT (seconds).
Returns (:status :headers :body :final-url) or signals an error."
  (let* ((url-request-method method)
         (url-request-extra-headers
          (append
           (unless (assoc "User-Agent" extra-headers)
             (list (cons "User-Agent" nelisp-http-user-agent)))
           (unless (assoc "Accept-Encoding" extra-headers)
             (list (cons "Accept-Encoding" "gzip")))
           extra-headers))
         (url-show-status nil)
         (url-automatic-caching nil)
         (url-max-redirections nelisp-http-max-redirections)
         (buf nil))
    (condition-case err
        (setq buf (url-retrieve-synchronously url t t timeout))
      (error
       (error "nelisp-http: network error on %s: %s"
              url (error-message-string err))))
    (unless (buffer-live-p buf)
      (error "nelisp-http: no response (timeout %ds?) for %s"
             timeout url))
    (unwind-protect
        (with-current-buffer buf
          (let ((resp (nelisp-http--parse-response url)))
            (unless (plist-get resp :status)
              (error "nelisp-http: malformed response from %s" url))
            resp))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(defun nelisp-http--build-request-headers (headers cached if-newer-than)
  "Compose request headers from caller HEADERS + cache validators."
  (let ((extra (mapcar (lambda (kv) (cons (car kv) (cdr kv))) headers)))
    (when cached
      (let ((etag (plist-get cached :etag))
            (lm (plist-get cached :last-modified)))
        (when etag (push (cons "If-None-Match" etag) extra))
        (when lm (push (cons "If-Modified-Since" lm) extra))))
    (when (and if-newer-than (integerp if-newer-than))
      (push (cons "If-Modified-Since"
                  (format-time-string "%a, %d %b %Y %H:%M:%S GMT"
                                      if-newer-than t))
            extra))
    extra))

;;; Public API --------------------------------------------------------

(defun nelisp-http--response-plist (entry from-cache elapsed-ms)
  "Public response plist shape; mirrors anvil-http for portability."
  (list :status (plist-get entry :status)
        :headers (plist-get entry :headers)
        :body (plist-get entry :body)
        :from-cache from-cache
        :cached-at (plist-get entry :fetched-at)
        :final-url (plist-get entry :final-url)
        :elapsed-ms elapsed-ms))

;;;###autoload
(cl-defun nelisp-http-fetch (url &key headers timeout-sec ttl
                                 no-cache if-newer-than)
  "GET URL with TTL cache via `nelisp-state' ns=\"http\".

Keyword args:
  :headers         alist of (STRING . STRING) extra request headers.
  :timeout-sec     override `nelisp-http-timeout-sec'.
  :ttl             override `nelisp-http-cache-ttl-sec'.  Negative or
                   zero disables freshness check (always re-validate).
  :no-cache        skip cache reads AND writes.
  :if-newer-than   unix epoch int; sends If-Modified-Since."
  (nelisp-http--check-url url)
  (nelisp-state-enable)
  (let* ((norm (nelisp-http--normalize-url url))
         (eff-ttl (or ttl nelisp-http-cache-ttl-sec))
         (now (truncate (float-time)))
         (cached (unless no-cache (nelisp-http--cache-get norm))))
    (if (and cached
             (numberp eff-ttl) (> eff-ttl 0)
             (numberp (plist-get cached :fetched-at))
             (< (- now (plist-get cached :fetched-at)) eff-ttl))
        ;; Fresh — zero network.
        (nelisp-http--response-plist cached t 0)
      ;; Network round-trip with conditional validators.
      (let* ((extra (nelisp-http--build-request-headers
                     headers cached if-newer-than))
             (timeout (or timeout-sec nelisp-http-timeout-sec))
             (start (float-time))
             (resp (nelisp-http--request "GET" url extra timeout))
             (elapsed-ms (round (* 1000 (- (float-time) start))))
             (status (plist-get resp :status))
             (resp-headers (plist-get resp :headers))
             (resp-body (plist-get resp :body))
             (resp-final (plist-get resp :final-url)))
        (cond
         ((= status 304)
          (unless cached
            (error "nelisp-http: 304 from %s but no cache entry" url))
          (let* ((merged (nelisp-http--merge-headers
                          (plist-get cached :headers) resp-headers))
                 (refreshed (nelisp-http--cache-entry
                             (plist-get cached :status)
                             merged
                             (plist-get cached :body)
                             now
                             (or resp-final (plist-get cached :final-url)))))
            (unless no-cache
              (nelisp-http--cache-put norm refreshed eff-ttl))
            (nelisp-http--response-plist refreshed t elapsed-ms)))
         ((and (>= status 200) (< status 300))
          (let ((entry (nelisp-http--cache-entry
                        status resp-headers resp-body now resp-final)))
            (unless no-cache
              (nelisp-http--cache-put norm entry eff-ttl))
            (nelisp-http--response-plist entry nil elapsed-ms)))
         (t
          (error "nelisp-http: HTTP %d for %s" status url)))))))

(defun nelisp-http--merge-headers (base override)
  "Shallow-merge two header plists, OVERRIDE winning on clashes."
  (let ((out (copy-sequence base))
        (tail override))
    (while tail
      (setq out (plist-put out (car tail) (cadr tail)))
      (setq tail (cddr tail)))
    out))

;;;###autoload
(cl-defun nelisp-http-fetch-head (url &key headers timeout-sec)
  "HEAD URL and return (:status :headers :final-url :elapsed-ms).
HEAD responses never touch the cache (no body to memoize, and the
upstream may freshen validators we'd otherwise miss)."
  (nelisp-http--check-url url)
  (let* ((extra (nelisp-http--build-request-headers headers nil nil))
         (timeout (or timeout-sec nelisp-http-timeout-sec))
         (start (float-time))
         (resp (nelisp-http--request "HEAD" url extra timeout))
         (elapsed-ms (round (* 1000 (- (float-time) start)))))
    (list :status (plist-get resp :status)
          :headers (plist-get resp :headers)
          :final-url (plist-get resp :final-url)
          :elapsed-ms elapsed-ms)))

;; The Phase 5-E `nelisp-http-get' / `nelisp-http-head' (nelisp-network.el)
;; ship their own in-memory hash cache + `nelisp-http-cache-clear' /
;; `-cache-size' helpers; we pick `nelisp-http-fetch-*' here so the two
;; backends can coexist until Phase 6.2.5+ consolidates them.

;;;###autoload
(defun nelisp-http-fetch-cache-clear (&optional url)
  "Clear nelisp-http-fetch cache entries.
With URL drop just that entry, else wipe the whole namespace."
  (if url
      (nelisp-http--cache-delete (nelisp-http--normalize-url url))
    (nelisp-http--cache-clear)))

;;;###autoload
(defun nelisp-http-fetch-cache-get (url)
  "Return cached entry plist for URL or nil."
  (nelisp-http--cache-get (nelisp-http--normalize-url url)))

;;;###autoload
(defun nelisp-http-fetch-cache-list ()
  "Return list of currently cached normalized URLs."
  (nelisp-http--cache-list))

(provide 'nelisp-http)
;;; nelisp-http.el ends here
