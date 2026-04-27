;;; nelisp-http-test.el --- ERT tests for nelisp-http  -*- lexical-binding: t; -*-

;; Phase 6.2.1 — anvil-http-test.el の MVP 該当部分を移植 + NeLisp 固有
;; (network test は cl-letf で `nelisp-http--request' を stub し、純粋に
;; cache layer / URL hygiene / response-plist shape を検証する)。

(require 'ert)
(require 'cl-lib)
(require 'nelisp-http)

(defvar nelisp-http-test--tmpdir nil)

(defmacro nelisp-http-test--with-fresh-db (&rest body)
  "Bind `nelisp-state-db-path' to a fresh temp DB and disable robots
so the http cache + robots namespaces start empty for each test."
  (declare (indent 0) (debug t))
  `(let* ((nelisp-http-test--tmpdir
           (make-temp-file "nelisp-http-test-" t))
          (nelisp-state-db-path
           (expand-file-name "test-state.db" nelisp-http-test--tmpdir))
          (nelisp-http-respect-robots-txt nil))
     (unwind-protect
         (progn
           (nelisp-state--close)
           ,@body)
       (nelisp-state--close)
       (when (file-directory-p nelisp-http-test--tmpdir)
         (delete-directory nelisp-http-test--tmpdir t)))))

(defun nelisp-http-test--stub-200 (url body &optional headers)
  "Build a stub response plist mimicking `nelisp-http--request'."
  (list :status 200
        :headers (or headers '(:content-type "text/plain"))
        :body body
        :final-url url))

;;;; URL hygiene

(ert-deftest nelisp-http-test-check-url-rejects-empty ()
  "Empty / non-string URL signals user-error."
  (should-error (nelisp-http--check-url "") :type 'user-error)
  (should-error (nelisp-http--check-url nil) :type 'user-error))

(ert-deftest nelisp-http-test-check-url-rejects-bad-scheme ()
  "ftp:// scheme is not in `nelisp-http-allowed-schemes'."
  (should-error (nelisp-http--check-url "ftp://example.com/")
                :type 'user-error))

(ert-deftest nelisp-http-test-normalize-url-canonicalizes ()
  "Scheme + host lowercased, fragment dropped, path+query kept."
  (should (equal (nelisp-http--normalize-url
                  "HTTPS://Example.COM/A/b?q=1#frag")
                 "https://example.com/A/b?q=1")))

;;;; cache layer

(ert-deftest nelisp-http-test-cache-roundtrip ()
  "cache-put then cache-get returns the entry."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (nelisp-http-test--with-fresh-db
    (nelisp-state-enable)
    (let ((entry (nelisp-http--cache-entry
                  200 '(:content-type "text/plain") "ok"
                  (truncate (float-time)) "https://example.com/")))
      (nelisp-http--cache-put "https://example.com/" entry)
      (let ((got (nelisp-http--cache-get "https://example.com/")))
        (should (equal (plist-get got :status) 200))
        (should (equal (plist-get got :body) "ok"))))))

(ert-deftest nelisp-http-test-cache-list-and-clear ()
  "cache-list reports stored URLs; cache-clear empties the namespace."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (nelisp-http-test--with-fresh-db
    (nelisp-state-enable)
    (dolist (url '("https://a.example.com/" "https://b.example.com/"))
      (nelisp-http--cache-put
       url (nelisp-http--cache-entry 200 nil "x"
                                     (truncate (float-time)) url)))
    (should (= (length (nelisp-http-fetch-cache-list)) 2))
    (nelisp-http--cache-clear)
    (should (null (nelisp-http-fetch-cache-list)))))

;;;; fetch (stubbed network)

(ert-deftest nelisp-http-test-fetch-cache-miss-then-hit ()
  "First call hits stub, second call within TTL is served from cache."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (nelisp-http-test--with-fresh-db
    (nelisp-state-enable)
    (let ((calls 0))
      (cl-letf (((symbol-function 'nelisp-http--request)
                 (lambda (_method url _extra _timeout)
                   (cl-incf calls)
                   (nelisp-http-test--stub-200 url "fresh"))))
        (let ((r1 (nelisp-http-fetch "https://example.com/path")))
          (should (eq (plist-get r1 :from-cache) nil))
          (should (equal (plist-get r1 :body) "fresh")))
        (let ((r2 (nelisp-http-fetch "https://example.com/path")))
          (should (eq (plist-get r2 :from-cache) t))
          (should (equal (plist-get r2 :body) "fresh")))
        (should (= calls 1))))))

(ert-deftest nelisp-http-test-fetch-no-cache-skips-cache-read ()
  "`:no-cache t' bypasses both cache read and write."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (nelisp-http-test--with-fresh-db
    (nelisp-state-enable)
    (let ((calls 0))
      (cl-letf (((symbol-function 'nelisp-http--request)
                 (lambda (_m url _e _t)
                   (cl-incf calls)
                   (nelisp-http-test--stub-200 url "live"))))
        (nelisp-http-fetch "https://example.com/n" :no-cache t)
        (nelisp-http-fetch "https://example.com/n" :no-cache t)
        (should (= calls 2))
        (should (null (nelisp-http-fetch-cache-list)))))))

(ert-deftest nelisp-http-test-fetch-ttl-zero-revalidates ()
  "`:ttl 0' forces revalidation; cache entry still updated on 200."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (nelisp-http-test--with-fresh-db
    (nelisp-state-enable)
    (let ((calls 0))
      (cl-letf (((symbol-function 'nelisp-http--request)
                 (lambda (_m url _e _t)
                   (cl-incf calls)
                   (nelisp-http-test--stub-200
                    url (format "v%d" calls)))))
        (let ((r1 (nelisp-http-fetch "https://example.com/t" :ttl 0)))
          (should (equal (plist-get r1 :body) "v1")))
        (let ((r2 (nelisp-http-fetch "https://example.com/t" :ttl 0)))
          (should (equal (plist-get r2 :body) "v2")))
        (should (= calls 2))))))

(ert-deftest nelisp-http-test-fetch-non-2xx-errors ()
  "404 surfaces as an error and does not pollute the cache."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (nelisp-http-test--with-fresh-db
    (nelisp-state-enable)
    (cl-letf (((symbol-function 'nelisp-http--request)
               (lambda (_m url _e _t)
                 (list :status 404 :headers nil :body "not found"
                       :final-url url))))
      (should-error (nelisp-http-fetch "https://example.com/missing"))
      (should (null (nelisp-http-fetch-cache-list))))))

;;;; response-plist shape

(ert-deftest nelisp-http-test-response-plist-shape ()
  "Public fields are the documented :status :headers :body :from-cache
:cached-at :final-url :elapsed-ms set."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (nelisp-http-test--with-fresh-db
    (nelisp-state-enable)
    (cl-letf (((symbol-function 'nelisp-http--request)
               (lambda (_m url _e _t)
                 (nelisp-http-test--stub-200 url "shape"))))
      (let ((r (nelisp-http-fetch "https://example.com/shape")))
        (dolist (k '(:status :headers :body :from-cache
                     :cached-at :final-url :elapsed-ms))
          (should (plist-member r k)))))))

;;;; HEAD (Phase 6.2.2)

(ert-deftest nelisp-http-test-head-returns-headers-only ()
  "HEAD returns :status :headers :final-url :elapsed-ms; no :body."
  (cl-letf (((symbol-function 'nelisp-http--request)
             (lambda (method url _e _t)
               (should (equal method "HEAD"))
               (list :status 200
                     :headers '(:content-length "1024"
                                :content-type "image/png")
                     :body ""
                     :final-url url))))
    (let ((r (nelisp-http-fetch-head "https://example.com/a.png")))
      (should (= (plist-get r :status) 200))
      (should (equal (plist-get (plist-get r :headers) :content-length)
                     "1024"))
      (should (null (plist-get r :body)))
      (should (equal (plist-get r :final-url)
                     "https://example.com/a.png")))))

(ert-deftest nelisp-http-test-head-skips-cache ()
  "HEAD must not write to the cache namespace."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (nelisp-http-test--with-fresh-db
    (nelisp-state-enable)
    (cl-letf (((symbol-function 'nelisp-http--request)
               (lambda (_m url _e _t)
                 (list :status 200 :headers nil :body ""
                       :final-url url))))
      (nelisp-http-fetch-head "https://example.com/x")
      (should (null (nelisp-http-fetch-cache-list))))))

(ert-deftest nelisp-http-test-head-rejects-bad-url ()
  "HEAD performs the same URL hygiene as fetch."
  (should-error (nelisp-http-fetch-head "") :type 'user-error)
  (should-error (nelisp-http-fetch-head "ftp://example.com/")
                :type 'user-error))

;;;; POST (Phase 6.2.8)

(ert-deftest nelisp-http-test-encode-body-string ()
  (should (equal (nelisp-http--encode-body nil) '(nil . nil)))
  (should (equal (nelisp-http--encode-body "raw") '("raw" . nil))))

(ert-deftest nelisp-http-test-encode-body-form ()
  "Alist of (KEY . VAL) → form-urlencoded."
  (let ((enc (nelisp-http--encode-body '(("a" . "1") ("b" . "x y")))))
    (should (equal (cdr enc) "application/x-www-form-urlencoded"))
    (should (string-match-p "a=1" (car enc)))
    (should (string-match-p "b=x%20y" (car enc)))))

(ert-deftest nelisp-http-test-encode-body-json ()
  "Plist (starts with keyword) → JSON."
  (let ((enc (nelisp-http--encode-body '(:k 1 :s "v"))))
    (should (equal (cdr enc) "application/json"))
    ;; Hash-table iteration order is not guaranteed, so compare via re-decode.
    (let ((decoded (json-parse-string (car enc))))
      (should (= (gethash "k" decoded) 1))
      (should (equal (gethash "s" decoded) "v")))))

(ert-deftest nelisp-http-test-apply-auth-bearer ()
  (let ((h (nelisp-http--apply-auth nil '(:bearer "abc"))))
    (should (equal (cdr (assoc "Authorization" h)) "Bearer abc"))))

(ert-deftest nelisp-http-test-apply-auth-basic ()
  (let* ((h (nelisp-http--apply-auth nil '(:basic ("alice" . "pw"))))
         (auth (cdr (assoc "Authorization" h))))
    (should (string-match "\\`Basic " auth))
    (should (equal (base64-decode-string (substring auth 6))
                   "alice:pw"))))

(ert-deftest nelisp-http-test-fetch-post-roundtrip ()
  "POST sends method + body via stub, returns 2xx response."
  (let (sent-method sent-body sent-headers)
    (cl-letf (((symbol-function 'nelisp-http--request)
               (lambda (method url headers _t &optional body)
                 (setq sent-method method
                       sent-body body
                       sent-headers headers)
                 (list :status 201
                       :headers '(:content-type "application/json")
                       :body "{\"id\":42}"
                       :final-url url))))
      (let ((r (nelisp-http-fetch-post
                "https://example.com/api"
                :body '(:name "x" :n 1)
                :auth '(:bearer "tok"))))
        (should (= (plist-get r :status) 201))
        (should (eq (plist-get r :from-cache) nil))
        (should (equal sent-method "POST"))
        (should (string-match-p "\"name\"" sent-body))
        (should (equal (cdr (assoc "Authorization" sent-headers))
                       "Bearer tok"))
        (should (equal (cdr (assoc "Content-Type" sent-headers))
                       "application/json"))))))

(ert-deftest nelisp-http-test-fetch-post-non-2xx-errors ()
  (cl-letf (((symbol-function 'nelisp-http--request)
             (lambda (_m url _h _t &optional _b)
               (list :status 500 :headers nil :body "err"
                     :final-url url))))
    (should-error (nelisp-http-fetch-post
                   "https://example.com/api" :body "{}"))))

;;;; Extract pipeline (Phase 6.2.7)

(ert-deftest nelisp-http-test-parse-selector-cases ()
  (should (equal (nelisp-http--parse-selector "div") '(:tag div)))
  (should (equal (nelisp-http--parse-selector ".item") '(:class "item")))
  (should (equal (nelisp-http--parse-selector "#main") '(:id "main")))
  (should (equal (nelisp-http--parse-selector "p.warn")
                 '(:tag-class p "warn")))
  (should (equal (nelisp-http--parse-selector "section#hero")
                 '(:tag-id section "hero")))
  (should (null (nelisp-http--parse-selector "div > p")))
  (should (null (nelisp-http--parse-selector ":hover")))
  (should (null (nelisp-http--parse-selector ""))))

(ert-deftest nelisp-http-test-split-json-path-tokens ()
  (should (equal (nelisp-http--split-json-path "data.results")
                 '((:key "data") (:key "results"))))
  (should (equal (nelisp-http--split-json-path "items[0].title")
                 '((:key "items") (:index 0) (:key "title"))))
  (should (equal (nelisp-http--split-json-path "users.0.name")
                 '((:key "users") (:index 0) (:key "name"))))
  (should (equal (nelisp-http--split-json-path "list[*].id")
                 '((:key "list") (:wildcard) (:key "id")))))

(ert-deftest nelisp-http-test-select-json-dotted-key ()
  (should (equal (nelisp-http--select-json-dotted
                  "{\"a\":{\"b\":42}}" "a.b")
                 "42")))

(ert-deftest nelisp-http-test-select-json-dotted-index ()
  (should (equal (nelisp-http--select-json-dotted
                  "{\"items\":[{\"id\":1},{\"id\":2}]}" "items[1].id")
                 "2")))

(ert-deftest nelisp-http-test-select-json-dotted-wildcard ()
  (let ((result (nelisp-http--select-json-dotted
                 "{\"list\":[{\"n\":1},{\"n\":2},{\"n\":3}]}"
                 "list[*].n")))
    (should (equal result "[1,2,3]"))))

(ert-deftest nelisp-http-test-select-html-libxml-class ()
  (skip-unless (fboundp 'libxml-parse-html-region))
  (let ((html "<html><body><p class=\"hit\">A</p><span class=\"hit\">B</span></body></html>"))
    (should (equal (nelisp-http--select-html-libxml html ".hit")
                   "A\n\nB"))))

(ert-deftest nelisp-http-test-select-html-fallback-tag ()
  (let ((html "<html><body><h1>One</h1><h1>Two</h1></body></html>"))
    (should (equal (nelisp-http--select-html-fallback html "h1")
                   "One\n\nTwo"))))

(ert-deftest nelisp-http-test-fetch-extract-html-success ()
  "Fetch with :selector replaces :body with extracted text and tags meta."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (nelisp-http-test--with-fresh-db
    (nelisp-state-enable)
    (cl-letf (((symbol-function 'nelisp-http--request)
               (lambda (_m url _h _t &optional _b)
                 (list :status 200
                       :headers '(:content-type "text/html; charset=utf-8")
                       :body "<html><body><h1>Title</h1><p>Body text</p></body></html>"
                       :final-url url))))
      (let ((r (nelisp-http-fetch "https://example.com/x" :selector "h1")))
        (should (equal (plist-get r :body) "Title"))
        (should (eq (plist-get r :extract-mode) 'selector))
        (should (memq (plist-get r :extract-engine) '(libxml regex-subset)))))))

(ert-deftest nelisp-http-test-fetch-extract-json-path ()
  "Fetch with :json-path returns JSON-serialized sub-tree."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (nelisp-http-test--with-fresh-db
    (nelisp-state-enable)
    (cl-letf (((symbol-function 'nelisp-http--request)
               (lambda (_m url _h _t &optional _b)
                 (list :status 200
                       :headers '(:content-type "application/json")
                       :body "{\"data\":{\"results\":[{\"id\":7}]}}"
                       :final-url url))))
      (let ((r (nelisp-http-fetch "https://example.com/api"
                                  :json-path "data.results[0].id")))
        (should (equal (plist-get r :body) "7"))
        (should (eq (plist-get r :extract-mode) 'json-path))))))

(ert-deftest nelisp-http-test-fetch-extract-content-type-mismatch ()
  "Selector against application/json content-type is flagged extract-miss."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (nelisp-http-test--with-fresh-db
    (nelisp-state-enable)
    (cl-letf (((symbol-function 'nelisp-http--request)
               (lambda (_m url _h _t &optional _b)
                 (list :status 200
                       :headers '(:content-type "application/json")
                       :body "{\"x\":1}"
                       :final-url url))))
      (let ((r (nelisp-http-fetch "https://example.com/api"
                                  :selector "h1")))
        (should (eq (plist-get r :extract-miss) t))
        (should (eq (plist-get r :extract-engine) 'content-type-mismatch))))))

;;;; robots.txt (Phase 6.2.5)

(ert-deftest nelisp-http-test-url-origin-and-path ()
  (should (equal (nelisp-http--url-origin "https://example.com/foo?x=1")
                 "https://example.com"))
  (should (equal (nelisp-http--url-origin "http://example.com:8080/")
                 "http://example.com:8080"))
  (should (equal (nelisp-http--url-origin "https://example.com:443/x")
                 "https://example.com"))
  (should (equal (nelisp-http--url-path "https://example.com/foo/bar?x=1")
                 "/foo/bar?x=1"))
  (should (equal (nelisp-http--url-path "https://example.com") "/")))

(ert-deftest nelisp-http-test-is-robots-url ()
  (should (nelisp-http--is-robots-url-p "https://example.com/robots.txt"))
  (should-not (nelisp-http--is-robots-url-p "https://example.com/other")))

(ert-deftest nelisp-http-test-robots-parse-basic ()
  (let* ((text "User-agent: *\nDisallow: /private\nAllow: /private/ok\n\nUser-agent: BadBot\nDisallow: /\n")
         (groups (nelisp-http--robots-parse text)))
    (should (= (length groups) 2))
    (let ((star (cl-find-if (lambda (g) (member "*" (car g))) groups)))
      (should star)
      (should (equal (cdr star)
                     '((disallow . "/private")
                       (allow . "/private/ok")))))))

(ert-deftest nelisp-http-test-robots-pattern-to-regex ()
  (should (equal (nelisp-http--robots-pattern-to-regex "/foo/")
                 "\\`/foo/"))
  (should (equal (nelisp-http--robots-pattern-to-regex "/*.pdf$")
                 "\\`/.*\\.pdf\\'"))
  (should (null (nelisp-http--robots-pattern-to-regex "")))
  (should (null (nelisp-http--robots-pattern-to-regex nil))))

(ert-deftest nelisp-http-test-robots-match-allow-beats-disallow ()
  "Equal-length tie → Allow wins (RFC 9309)."
  (let ((rules '((disallow . "/foo") (allow . "/foo"))))
    (should (eq (car (nelisp-http--robots-match rules "/foo")) t))))

(ert-deftest nelisp-http-test-robots-match-longest-wins ()
  (let ((rules '((disallow . "/") (allow . "/public"))))
    (should (eq (car (nelisp-http--robots-match rules "/public/x")) t))
    (should (eq (car (nelisp-http--robots-match rules "/secret")) nil))))

(ert-deftest nelisp-http-test-robots-evaluate-fail-open ()
  "Empty / failing robots.txt → :allowed t :robots-present nil."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (nelisp-http-test--with-fresh-db
    (let ((nelisp-http-respect-robots-txt t))
      (nelisp-state-enable)
      (cl-letf (((symbol-function 'nelisp-http--request)
                 (lambda (_m url _h _t &optional _b)
                   (list :status 404 :headers nil :body nil :final-url url))))
        (let ((r (nelisp-http--robots-evaluate
                  "https://example.com/x" "test-agent")))
          (should (eq (plist-get r :allowed) t))
          (should (eq (plist-get r :robots-present) nil)))))))

(ert-deftest nelisp-http-test-robots-disallow-signals ()
  "Disallow-matched URL through fetch raises user-error."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (nelisp-http-test--with-fresh-db
    (let ((nelisp-http-respect-robots-txt t))
      (nelisp-state-enable)
      (cl-letf (((symbol-function 'nelisp-http--request)
                 (lambda (_m url _h _t &optional _b)
                   (cond
                    ((string-suffix-p "/robots.txt" url)
                     (list :status 200
                           :headers '(:content-type "text/plain")
                           :body "User-agent: *\nDisallow: /forbidden\n"
                           :final-url url))
                    (t (list :status 200 :headers nil :body "ok"
                             :final-url url))))))
        (should-error
         (nelisp-http-fetch "https://example.com/forbidden")
         :type 'user-error)))))

;;;; batch fetch (Phase 6.2.6)

(ert-deftest nelisp-http-test-fetch-batch-roundtrip ()
  "Each URL gets its own response plist; per-URL errors isolated."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (nelisp-http-test--with-fresh-db
    (nelisp-state-enable)
    (cl-letf (((symbol-function 'nelisp-http--request)
               (lambda (_m url _h _t &optional _b)
                 (if (string-match "fail" url)
                     (list :status 500 :headers nil :body "" :final-url url)
                   (list :status 200
                         :headers '(:content-type "text/plain")
                         :body (format "body=%s" url)
                         :final-url url)))))
      (let* ((urls '("https://example.com/a"
                     "https://example.com/fail"
                     "https://example.com/b"))
             (results (nelisp-http-fetch-batch urls)))
        (should (= (length results) 3))
        ;; OK entry shape
        (should (= (plist-get (nth 0 results) :status) 200))
        (should (string-match-p "/a" (plist-get (nth 0 results) :body)))
        ;; Error entry surfaces (:url :error)
        (should (equal (plist-get (nth 1 results) :url)
                       "https://example.com/fail"))
        (should (stringp (plist-get (nth 1 results) :error)))
        ;; Third URL still ran successfully
        (should (= (plist-get (nth 2 results) :status) 200))))))

(ert-deftest nelisp-http-test-fetch-batch-cap-exceeded ()
  "URL count above `nelisp-http-batch-max' raises user-error."
  (let ((nelisp-http-batch-max 2))
    (should-error
     (nelisp-http-fetch-batch '("a" "b" "c"))
     :type 'user-error)))

(provide 'nelisp-http-test)
;;; nelisp-http-test.el ends here
