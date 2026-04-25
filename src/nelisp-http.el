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
(require 'dom)
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

(defvar nelisp-http-respect-robots-txt t
  "When non-nil, fetch the origin's robots.txt and signal user-error
on a Disallow match before any network round-trip (Phase 6.2.5).")

(defvar nelisp-http-robots-ttl-sec 86400
  "Cache TTL (seconds) for robots.txt entries.")

(defvar nelisp-http-batch-max 64
  "Maximum URL count accepted by `nelisp-http-fetch-batch' in one call.")

(defconst nelisp-http--state-ns "http"
  "`nelisp-state' namespace for cached HTTP responses.")

(defconst nelisp-http--robots-state-ns "http-robots"
  "`nelisp-state' namespace for cached robots.txt entries.")

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

;;; Body / auth helpers (Phase 6.2.8) --------------------------------

(defun nelisp-http--alist-of-string-pairs-p (x)
  "Non-nil when X looks like an alist of (KEY . VAL) pairs.
Used to disambiguate alist-form bodies from plist-form bodies."
  (and (consp x)
       (not (keywordp (car x)))
       (consp (car x))
       (not (consp (cdr (car x))))))

(defun nelisp-http--url-encode-form (alist)
  "Return ALIST encoded as application/x-www-form-urlencoded."
  (mapconcat
   (lambda (pair)
     (format "%s=%s"
             (url-hexify-string (format "%s" (car pair)))
             (url-hexify-string (format "%s" (cdr pair)))))
   alist
   "&"))

(defun nelisp-http--plist-to-hash (plist)
  "Return PLIST as a JSON-friendly hash table.
Keyword keys lose their leading colon."
  (let ((h (make-hash-table :test 'equal)))
    (while plist
      (let ((k (car plist))
            (v (cadr plist)))
        (puthash (cond ((keywordp k) (substring (symbol-name k) 1))
                       ((symbolp k) (symbol-name k))
                       (t (format "%s" k)))
                 v h))
      (setq plist (cddr plist)))
    h))

(defun nelisp-http--encode-body (body)
  "Encode BODY into (DATA . CONTENT-TYPE).
- nil → (nil . nil)
- string → (BODY . nil)        — caller sets Content-Type via :headers
- alist of (K . V) → (form-urlencoded . application/x-www-form-urlencoded)
- plist (starts with keyword) → (json . application/json)"
  (cond
   ((null body) (cons nil nil))
   ((stringp body) (cons body nil))
   ((nelisp-http--alist-of-string-pairs-p body)
    (cons (nelisp-http--url-encode-form body)
          "application/x-www-form-urlencoded"))
   ((and (listp body) (keywordp (car body)))
    (cons (json-serialize (nelisp-http--plist-to-hash body)
                          :null-object :null
                          :false-object :false)
          "application/json"))
   (t (error "nelisp-http: cannot encode body of type %S"
             (type-of body)))))

(defun nelisp-http--apply-auth (headers auth)
  "Augment HEADERS alist with credentials from AUTH plist.
AUTH:
  (:bearer TOKEN)             → Authorization: Bearer TOKEN
  (:basic (USER . PASS))      → Authorization: Basic base64(USER:PASS)
  (:basic USER PASS)          → same as above (positional)
  (:header (NAME . VALUE))    → custom header
A list of these forms is also accepted."
  (cond
   ((null auth) headers)
   ((and (consp auth) (consp (car auth)) (keywordp (caar auth)))
    (cl-reduce (lambda (h spec) (nelisp-http--apply-auth h spec))
               auth :initial-value headers))
   (t
    (pcase (car-safe auth)
      (:bearer
       (cons (cons "Authorization"
                   (format "Bearer %s" (cadr auth)))
             (assq-delete-all "Authorization" (copy-sequence headers))))
      (:basic
       (let* ((tail (cdr auth))
              (user (if (consp (car tail)) (caar tail) (car tail)))
              (pass (if (consp (car tail)) (cdar tail) (cadr tail)))
              (encoded (base64-encode-string
                        (encode-coding-string
                         (format "%s:%s" user pass) 'utf-8)
                        t)))
         (cons (cons "Authorization" (format "Basic %s" encoded))
               (assq-delete-all "Authorization"
                                (copy-sequence headers)))))
      (:header
       (let ((pair (cadr auth)))
         (cons (cons (car pair) (cdr pair)) headers)))
      (_ headers)))))

;;; Network primitive (one-shot, no retry — MVP) ----------------------

(defun nelisp-http--request (method url extra-headers timeout &optional body)
  "Issue one METHOD request to URL with EXTRA-HEADERS and TIMEOUT (seconds).
BODY is an optional request body string for POST / PUT / PATCH.
Returns (:status :headers :body :final-url) or signals an error."
  (let* ((url-request-method method)
         (url-request-data body)
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

;;; Extract pipeline (Phase 6.2.7) ------------------------------------

(defun nelisp-http--libxml-p ()
  "Return non-nil when this Emacs build bundles libxml2."
  (fboundp 'libxml-parse-html-region))

(defun nelisp-http--extract-target-from-content-type (ct)
  "Classify Content-Type CT as `html', `json', `xml', or nil."
  (cond
   ((not (stringp ct)) nil)
   ((string-match-p "\\`\\(?:[[:space:]]*\\)\\(?:text/html\\|application/xhtml\\+xml\\)\\b"
                    (downcase ct))
    'html)
   ((string-match-p "\\`\\(?:[[:space:]]*\\)application/json\\b" (downcase ct))
    'json)
   ((string-match-p "\\`\\(?:[[:space:]]*\\)\\(?:application/xml\\|text/xml\\)\\b"
                    (downcase ct))
    'xml)
   (t nil)))

(defun nelisp-http--parse-selector (s)
  "Parse CSS-subset selector S.
Returns (:tag SYM) / (:class STR) / (:id STR) / (:tag-class SYM STR) /
(:tag-id SYM STR), or nil for anything outside the subset."
  (let ((s (and (stringp s) (string-trim s))))
    (cond
     ((or (null s) (string-empty-p s)) nil)
     ((string-match "\\`#\\([A-Za-z][A-Za-z0-9_-]*\\)\\'" s)
      (list :id (match-string 1 s)))
     ((string-match "\\`\\.\\([A-Za-z][A-Za-z0-9_-]*\\)\\'" s)
      (list :class (match-string 1 s)))
     ((string-match "\\`\\([A-Za-z][A-Za-z0-9]*\\)#\\([A-Za-z][A-Za-z0-9_-]*\\)\\'" s)
      (list :tag-id (intern (downcase (match-string 1 s)))
            (match-string 2 s)))
     ((string-match "\\`\\([A-Za-z][A-Za-z0-9]*\\)\\.\\([A-Za-z][A-Za-z0-9_-]*\\)\\'" s)
      (list :tag-class (intern (downcase (match-string 1 s)))
            (match-string 2 s)))
     ((string-match "\\`[A-Za-z][A-Za-z0-9]*\\'" s)
      (list :tag (intern (downcase s))))
     (t nil))))

(defun nelisp-http--dom-select (dom parts)
  "Return DOM nodes matching PARTS produced by `--parse-selector'."
  (pcase parts
    (`(:tag ,tag) (dom-by-tag dom tag))
    (`(:class ,cls) (dom-by-class dom (format "\\b%s\\b" (regexp-quote cls))))
    (`(:id ,id) (dom-by-id dom (format "\\`%s\\'" (regexp-quote id))))
    (`(:tag-class ,tag ,cls)
     (seq-filter (lambda (el) (eq (dom-tag el) tag))
                 (dom-by-class dom (format "\\b%s\\b" (regexp-quote cls)))))
    (`(:tag-id ,tag ,id)
     (seq-filter (lambda (el) (eq (dom-tag el) tag))
                 (dom-by-id dom (format "\\`%s\\'" (regexp-quote id)))))
    (_ nil)))

(defun nelisp-http--select-html-libxml (html selector)
  "Return text from HTML matching SELECTOR via libxml + dom.el."
  (let ((parts (nelisp-http--parse-selector selector)))
    (when (and parts (fboundp 'libxml-parse-html-region))
      (let* ((dom (with-temp-buffer
                    (insert html)
                    (libxml-parse-html-region (point-min) (point-max))))
             (nodes (and dom (nelisp-http--dom-select dom parts))))
        (when nodes
          (let ((texts (delq nil
                             (mapcar (lambda (n)
                                       (let ((s (string-trim
                                                 (or (dom-text n) ""))))
                                         (and (not (string-empty-p s)) s)))
                                     nodes))))
            (when texts (mapconcat #'identity texts "\n\n"))))))))

(defun nelisp-http--strip-tags (html)
  "Return HTML with tags removed and whitespace collapsed."
  (let* ((s (replace-regexp-in-string "<[^>]+>" "" html))
         (s (replace-regexp-in-string "&amp;" "&" s))
         (s (replace-regexp-in-string "&lt;" "<" s))
         (s (replace-regexp-in-string "&gt;" ">" s))
         (s (replace-regexp-in-string "&quot;" "\"" s))
         (s (replace-regexp-in-string "[ \t]+" " " s))
         (s (replace-regexp-in-string "\n[ \t]*\n[ \t\n]*" "\n\n" s)))
    (string-trim s)))

(defun nelisp-http--select-html-fallback (html selector)
  "Regex-subset selector for builds without libxml."
  (let* ((parts (nelisp-http--parse-selector selector))
         (case-fold-search t)
         (rx nil)
         (group 1))
    (pcase parts
      (`(:tag ,tag)
       (setq rx (format "<%s\\b[^>]*>\\(\\(?:.\\|\n\\)*?\\)</%s>"
                        (symbol-name tag) (symbol-name tag))))
      (`(:id ,id)
       (setq rx (format "<\\([A-Za-z][A-Za-z0-9]*\\)\\b[^>]*\\bid=[\"']%s[\"'][^>]*>\\(\\(?:.\\|\n\\)*?\\)</\\1>"
                        (regexp-quote id)))
       (setq group 2))
      (`(:class ,cls)
       (setq rx (format "<\\([A-Za-z][A-Za-z0-9]*\\)\\b[^>]*\\bclass=[\"'][^\"']*\\b%s\\b[^\"']*[\"'][^>]*>\\(\\(?:.\\|\n\\)*?\\)</\\1>"
                        (regexp-quote cls)))
       (setq group 2))
      (`(:tag-class ,tag ,cls)
       (setq rx (format "<%s\\b[^>]*\\bclass=[\"'][^\"']*\\b%s\\b[^\"']*[\"'][^>]*>\\(\\(?:.\\|\n\\)*?\\)</%s>"
                        (symbol-name tag) (regexp-quote cls)
                        (symbol-name tag))))
      (`(:tag-id ,tag ,id)
       (setq rx (format "<%s\\b[^>]*\\bid=[\"']%s[\"'][^>]*>\\(\\(?:.\\|\n\\)*?\\)</%s>"
                        (symbol-name tag) (regexp-quote id)
                        (symbol-name tag)))))
    (when rx
      (let ((pos 0) (acc nil))
        (while (string-match rx html pos)
          (let* ((content (match-string group html))
                 (text (and content (nelisp-http--strip-tags content))))
            (when (and text (not (string-empty-p text)))
              (push text acc)))
          (setq pos (match-end 0)))
        (when acc (mapconcat #'identity (nreverse acc) "\n\n"))))))

(defun nelisp-http--split-json-path (path)
  "Tokenize dotted-path PATH into (:key STR) / (:index INT) / (:wildcard)."
  (let ((i 0) (n (length path)) (tokens nil))
    (while (< i n)
      (let ((ch (aref path i)))
        (cond
         ((eq ch ?.) (cl-incf i))
         ((eq ch ?\[)
          (let* ((end (string-match "\\]" path i))
                 (inner (and end (substring path (1+ i) end))))
            (unless end
              (error "nelisp-http: unterminated [] in json-path %S" path))
            (push (cond
                   ((equal inner "*") (list :wildcard))
                   ((string-match-p "\\`-?[0-9]+\\'" inner)
                    (list :index (string-to-number inner)))
                   (t (list :key inner)))
                  tokens)
            (setq i (1+ end))))
         (t
          (let* ((end (or (string-match "[.[]" path i) n))
                 (seg (substring path i end)))
            (push (if (string-match-p "\\`-?[0-9]+\\'" seg)
                      (list :index (string-to-number seg))
                    (list :key seg))
                  tokens)
            (setq i end))))))
    (nreverse tokens)))

(defun nelisp-http--json-walk (node segments)
  "Walk NODE by SEGMENTS produced by `--split-json-path'."
  (cond
   ((null segments) node)
   ((null node) nil)
   (t
    (pcase (car segments)
      (`(:key ,k)
       (when (hash-table-p node)
         (let ((val (gethash k node)))
           (nelisp-http--json-walk val (cdr segments)))))
      (`(:index ,idx)
       (when (and (vectorp node)
                  (>= idx 0) (< idx (length node)))
         (nelisp-http--json-walk (aref node idx) (cdr segments))))
      (`(:wildcard)
       (when (vectorp node)
         (let ((results
                (delq nil
                      (mapcar (lambda (el)
                                (nelisp-http--json-walk el (cdr segments)))
                              (append node nil)))))
           (vconcat results))))
      (_ nil)))))

(defun nelisp-http--select-json-dotted (json-string path)
  "Parse JSON-STRING and walk dotted PATH; return JSON of subtree or nil."
  (condition-case _
      (let* ((node (json-parse-string
                    json-string
                    :object-type 'hash-table
                    :array-type 'array
                    :null-object :null
                    :false-object :false))
             (segments (nelisp-http--split-json-path path))
             (result (and segments
                          (nelisp-http--json-walk node segments))))
        (when result
          (json-serialize result
                          :null-object :null
                          :false-object :false)))
    (error nil)))

(defun nelisp-http--plist-put! (plist &rest kvs)
  "Return PLIST with KVS (flat key/value list) applied via `plist-put'."
  (let ((p (copy-sequence plist)))
    (while kvs
      (setq p (plist-put p (car kvs) (cadr kvs)))
      (setq kvs (cddr kvs)))
    p))

(defun nelisp-http--apply-extract (response selector json-path)
  "Post-process RESPONSE with SELECTOR / JSON-PATH when supplied.
Mirrors anvil-http extract semantics — never erases :body silently;
mismatches set :extract-miss t and tag :extract-engine."
  (if (and (null selector) (null json-path))
      response
    (let* ((headers (plist-get response :headers))
           (ct (plist-get headers :content-type))
           (target (nelisp-http--extract-target-from-content-type ct))
           (body (plist-get response :body)))
      (cond
       ((and selector (memq target '(html xml)) (stringp body))
        (let* ((engine (if (nelisp-http--libxml-p) 'libxml 'regex-subset))
               (extracted
                (if (eq engine 'libxml)
                    (nelisp-http--select-html-libxml body selector)
                  (nelisp-http--select-html-fallback body selector))))
          (if (and extracted (not (string-empty-p extracted)))
              (nelisp-http--plist-put!
               response
               :body extracted
               :extract-mode 'selector
               :extract-engine engine)
            (nelisp-http--plist-put!
             response
             :extract-miss t
             :extract-mode 'selector
             :extract-engine engine))))
       ((and json-path (eq target 'json) (stringp body))
        (let ((extracted (nelisp-http--select-json-dotted body json-path)))
          (if extracted
              (nelisp-http--plist-put!
               response
               :body extracted
               :extract-mode 'json-path
               :extract-engine 'json)
            (nelisp-http--plist-put!
             response
             :extract-miss t
             :extract-mode 'json-path
             :extract-engine 'json))))
       (t
        (nelisp-http--plist-put!
         response
         :extract-miss t
         :extract-mode (cond (selector 'selector) (json-path 'json-path))
         :extract-engine 'content-type-mismatch))))))

;;; URL origin / path helpers (Phase 6.2.5) --------------------------

(defun nelisp-http--url-origin (url)
  "Return scheme://host[:port] for URL, omitting the default port."
  (let* ((u (url-generic-parse-url url))
         (scheme (url-type u))
         (host (url-host u))
         (port (url-port u))
         (default (pcase scheme ("http" 80) ("https" 443) (_ nil))))
    (if (and (numberp port) default (= port default))
        (format "%s://%s" scheme host)
      (format "%s://%s:%s" scheme host port))))

(defun nelisp-http--url-path (url)
  "Return path+query of URL, defaulting to `/' when absent."
  (let* ((u (url-generic-parse-url url))
         (filename (url-filename u)))
    (if (or (null filename) (string-empty-p filename))
        "/"
      filename)))

(defun nelisp-http--is-robots-url-p (url)
  "Return non-nil when URL itself points at /robots.txt."
  (string-equal "/robots.txt" (nelisp-http--url-path url)))

;;; robots.txt parser + matcher (Phase 6.2.5) ------------------------

(defun nelisp-http--robots-parse (text)
  "Parse robots.txt TEXT into a list of (UA-LIST . RULES).
Each RULES element is `(DIRECTIVE . PATTERN)' where DIRECTIVE is
`allow' or `disallow'.  Comments and unknown directives are ignored.
Consecutive User-agent lines accumulate into one group until a rule
follows, mirroring RFC 9309 group definition."
  (let ((groups nil)
        (current-uas nil)
        (current-rules nil)
        (in-rules nil))
    (dolist (raw (split-string (or text "") "\n"))
      (let* ((stripped (replace-regexp-in-string "#.*\\'" "" raw))
             (line (string-trim stripped)))
        (unless (string-empty-p line)
          (when (string-match "\\`\\([A-Za-z-]+\\)[ \t]*:[ \t]*\\(.*\\)\\'"
                              line)
            (let ((key (downcase (match-string 1 line)))
                  (val (string-trim (match-string 2 line))))
              (pcase key
                ("user-agent"
                 (when in-rules
                   (push (cons (nreverse current-uas)
                               (nreverse current-rules))
                         groups)
                   (setq current-uas nil
                         current-rules nil
                         in-rules nil))
                 (push val current-uas))
                ("allow"
                 (when current-uas
                   (push (cons 'allow val) current-rules)
                   (setq in-rules t)))
                ("disallow"
                 (when current-uas
                   (push (cons 'disallow val) current-rules)
                   (setq in-rules t)))
                (_ nil)))))))
    (when current-uas
      (push (cons (nreverse current-uas)
                  (nreverse current-rules))
            groups))
    (nreverse groups)))

(defun nelisp-http--robots-pick-group (groups ua)
  "Return rules for the group best matching UA string.
Longest substring match wins; falls back to `*' group when no
specific token matches."
  (let ((ua-lc (downcase (or ua "")))
        (best-rules nil)
        (best-len -1)
        (star-rules nil))
    (dolist (group groups)
      (let ((uas (car group))
            (rules (cdr group)))
        (dolist (u uas)
          (let ((u-lc (downcase u)))
            (cond
             ((equal u-lc "*")
              (unless star-rules (setq star-rules rules)))
             ((and (not (string-empty-p u-lc))
                   (string-match-p (regexp-quote u-lc) ua-lc)
                   (> (length u-lc) best-len))
              (setq best-rules rules
                    best-len (length u-lc))))))))
    (or best-rules star-rules)))

(defun nelisp-http--robots-pattern-to-regex (pattern)
  "Convert a robots.txt PATTERN to an Emacs regex.
`*' expands to `.*', trailing `$' anchors to end-of-URL, every other
character is regex-quoted.  Empty / nil PATTERN returns nil."
  (when (and pattern (not (string-empty-p pattern)))
    (let ((end-anchor nil)
          (p pattern))
      (when (string-suffix-p "$" p)
        (setq end-anchor t)
        (setq p (substring p 0 (1- (length p)))))
      (let ((chunks (mapcar
                     (lambda (ch)
                       (cond
                        ((eq ch ?*) ".*")
                        (t (regexp-quote (char-to-string ch)))))
                     (string-to-list p))))
        (concat "\\`"
                (mapconcat #'identity chunks "")
                (if end-anchor "\\'" ""))))))

(defun nelisp-http--robots-match (rules path)
  "Return the winning rule for PATH against RULES.
Longest pattern wins, Allow beats Disallow on ties (RFC 9309).
Return value is (ALLOW-P . PATTERN-LENGTH) or nil."
  (let ((best nil))
    (dolist (rule rules)
      (let* ((dir (car rule))
             (pat (cdr rule))
             (rx (nelisp-http--robots-pattern-to-regex pat)))
        (when (and rx (string-match-p rx path))
          (let* ((len (length pat))
                 (allow-p (eq dir 'allow))
                 (current (cons allow-p len)))
            (cond
             ((null best) (setq best current))
             ((> len (cdr best)) (setq best current))
             ((and (= len (cdr best)) allow-p)
              (setq best current)))))))
    best))

(defun nelisp-http--robots-fetch (origin)
  "Fetch ORIGIN/robots.txt, cache for `nelisp-http-robots-ttl-sec'.
Returns (:body STR-OR-NIL :fetched-at FLOAT).  Nil body = fail-open
(404, network error, non-200) per RFC 9309 guidance."
  (let* ((url (concat origin "/robots.txt"))
         (ns nelisp-http--robots-state-ns)
         (cached (condition-case _e
                     (nelisp-state-get url (list :ns ns))
                   (error nil)))
         (now (float-time))
         (ttl nelisp-http-robots-ttl-sec))
    (if (and cached
             (numberp (plist-get cached :fetched-at))
             (< (- now (plist-get cached :fetched-at)) ttl))
        cached
      (let ((entry
             (condition-case _err
                 (let ((resp (nelisp-http-fetch
                              url
                              :skip-robots-check t
                              :no-cache t)))
                   (if (= 200 (plist-get resp :status))
                       (list :body (plist-get resp :body)
                             :fetched-at now)
                     (list :body nil :fetched-at now)))
               (error (list :body nil :fetched-at now)))))
        (condition-case _e
            (nelisp-state-set url entry (list :ns ns))
          (error nil))
        entry))))

(defun nelisp-http--robots-evaluate (url ua)
  "Evaluate URL against its origin's robots.txt for UA.
Returns (:origin :allowed :rule-length :robots-present).  fail-open
when robots.txt is absent."
  (let* ((origin (nelisp-http--url-origin url))
         (entry (nelisp-http--robots-fetch origin))
         (body (plist-get entry :body)))
    (if (not body)
        (list :origin origin
              :allowed t
              :rule-length nil
              :robots-present nil)
      (let* ((groups (nelisp-http--robots-parse body))
             (rules (nelisp-http--robots-pick-group groups ua))
             (path (nelisp-http--url-path url))
             (match (and rules (nelisp-http--robots-match rules path))))
        (list :origin origin
              :allowed (if match (car match) t)
              :rule-length (and match (cdr match))
              :robots-present t)))))

(defun nelisp-http--robots-check-signal (url)
  "Raise `user-error' if URL is Disallowed by its origin's robots.txt."
  (let ((r (nelisp-http--robots-evaluate url nelisp-http-user-agent)))
    (unless (plist-get r :allowed)
      (user-error
       "nelisp-http: %s is Disallowed for %s by robots.txt at %s"
       url nelisp-http-user-agent (plist-get r :origin)))))

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
                                 no-cache if-newer-than
                                 selector json-path
                                 skip-robots-check)
  "GET URL with TTL cache via `nelisp-state' ns=\"http\".

Keyword args:
  :headers         alist of (STRING . STRING) extra request headers.
  :timeout-sec     override `nelisp-http-timeout-sec'.
  :ttl             override `nelisp-http-cache-ttl-sec'.  Negative or
                   zero disables freshness check (always re-validate).
  :no-cache        skip cache reads AND writes.
  :if-newer-than   unix epoch int; sends If-Modified-Since.
  :selector        CSS-subset selector evaluated against text/html
                   bodies (libxml when available, regex fallback
                   otherwise).  Result body is the matched text;
                   :extract-mode and :extract-engine annotate the run.
  :json-path       Dotted-path string (e.g. `data.results[0].id',
                   `items[*].name') applied to application/json
                   bodies.  Body is replaced with the located
                   sub-tree serialized back to JSON."
  (nelisp-http--check-url url)
  (nelisp-state-enable)
  (when (and nelisp-http-respect-robots-txt
             (not skip-robots-check)
             (not (nelisp-http--is-robots-url-p url)))
    (nelisp-http--robots-check-signal url))
  (let* ((norm (nelisp-http--normalize-url url))
         (eff-ttl (or ttl nelisp-http-cache-ttl-sec))
         (now (truncate (float-time)))
         (cached (unless no-cache (nelisp-http--cache-get norm))))
    (if (and cached
             (numberp eff-ttl) (> eff-ttl 0)
             (numberp (plist-get cached :fetched-at))
             (< (- now (plist-get cached :fetched-at)) eff-ttl))
        ;; Fresh — zero network; extract still applies.
        (nelisp-http--apply-extract
         (nelisp-http--response-plist cached t 0)
         selector json-path)
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
            (nelisp-http--apply-extract
             (nelisp-http--response-plist refreshed t elapsed-ms)
             selector json-path)))
         ((and (>= status 200) (< status 300))
          (let ((entry (nelisp-http--cache-entry
                        status resp-headers resp-body now resp-final)))
            (unless no-cache
              (nelisp-http--cache-put norm entry eff-ttl))
            (nelisp-http--apply-extract
             (nelisp-http--response-plist entry nil elapsed-ms)
             selector json-path)))
         (t
          (error "nelisp-http: HTTP %d for %s" status url)))))))

;;;###autoload
(cl-defun nelisp-http-fetch-batch (urls &key headers timeout-sec ttl
                                        no-cache selector json-path
                                        skip-robots-check)
  "Fetch URLS sequentially (MVP no async); return list in input order.

Per-URL failures surface as (:url URL :error STR) plists so one bad
entry does not sink the whole batch.  Cached entries within TTL are
served zero-network like `nelisp-http-fetch'.

Async parallel fetch (`url-queue-parallel-processes' fan-out) is
DEFER scope — Phase 6.2.6 MVP keeps the surface simple and the
shape compatible so a future async backend can drop in without
caller changes."
  (when (> (length urls) nelisp-http-batch-max)
    (user-error
     "nelisp-http: batch URL count %d exceeds `nelisp-http-batch-max' %d"
     (length urls) nelisp-http-batch-max))
  (mapcar
   (lambda (url)
     (condition-case err
         (nelisp-http-fetch
          url
          :headers headers
          :timeout-sec timeout-sec
          :ttl ttl
          :no-cache no-cache
          :selector selector
          :json-path json-path
          :skip-robots-check skip-robots-check)
       (error (list :url url :error (error-message-string err)))))
   urls))

(defun nelisp-http--merge-headers (base override)
  "Shallow-merge two header plists, OVERRIDE winning on clashes."
  (let ((out (copy-sequence base))
        (tail override))
    (while tail
      (setq out (plist-put out (car tail) (cadr tail)))
      (setq tail (cddr tail)))
    out))

;;;###autoload
(cl-defun nelisp-http-fetch-post (url &key body content-type headers
                                      accept timeout-sec auth)
  "POST URL with BODY and return a response plist.

Keyword args:
  :body          String → sent verbatim (caller sets :content-type or
                 :headers).  Alist of (K . V) → form-urlencoded.  Plist
                 starting with a keyword → JSON.  nil → empty body.
  :content-type  Override Content-Type.
  :headers       Alist of extra request headers.
  :accept        MIME string added as Accept header (short-hand).
  :timeout-sec   Override `nelisp-http-timeout-sec'.
  :auth          (:bearer TOKEN) / (:basic (USER . PASS)) /
                 (:header (NAME . VAL)), or a list of such specs.

Returns (:status :headers :body :from-cache nil :cached-at nil
:final-url :elapsed-ms).  POSTs are never cached."
  (nelisp-http--check-url url)
  (let* ((encoded (nelisp-http--encode-body body))
         (data (car encoded))
         (auto-ct (cdr encoded))
         (with-auth (nelisp-http--apply-auth (or headers nil) auth))
         (final-headers
          (let ((h with-auth))
            (when (and accept (not (assoc "Accept" h)))
              (setq h (cons (cons "Accept" accept) h)))
            (let ((ct (or content-type auto-ct)))
              (when (and ct (not (assoc "Content-Type" h)))
                (setq h (cons (cons "Content-Type" ct) h))))
            h))
         (timeout (or timeout-sec nelisp-http-timeout-sec))
         (start (float-time))
         (resp (nelisp-http--request "POST" url final-headers timeout data))
         (elapsed-ms (round (* 1000 (- (float-time) start))))
         (status (plist-get resp :status)))
    (cond
     ((and (integerp status) (>= status 200) (< status 300))
      (list :status status
            :headers (plist-get resp :headers)
            :body (plist-get resp :body)
            :from-cache nil
            :cached-at nil
            :final-url (plist-get resp :final-url)
            :elapsed-ms elapsed-ms))
     (t
      (error "nelisp-http: HTTP %s for POST %s" status url)))))

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
(defun nelisp-http-robots-check (url &optional ua)
  "Evaluate URL against its origin's robots.txt for UA.
Returns the same plist as `nelisp-http--robots-evaluate' — does not
signal on Disallow, just reports."
  (nelisp-http--check-url url)
  (nelisp-state-enable)
  (nelisp-http--robots-evaluate url (or ua nelisp-http-user-agent)))

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
