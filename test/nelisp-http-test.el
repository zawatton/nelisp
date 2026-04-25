;;; nelisp-http-test.el --- ERT tests for nelisp-http  -*- lexical-binding: t; -*-

;; Phase 6.2.1 — anvil-http-test.el の MVP 該当部分を移植 + NeLisp 固有
;; (network test は cl-letf で `nelisp-http--request' を stub し、純粋に
;; cache layer / URL hygiene / response-plist shape を検証する)。

(require 'ert)
(require 'cl-lib)
(require 'nelisp-http)

(defvar nelisp-http-test--tmpdir nil)

(defmacro nelisp-http-test--with-fresh-db (&rest body)
  "Bind `nelisp-state-db-path' to a fresh temp DB so the http cache
namespace starts empty for each test."
  (declare (indent 0) (debug t))
  `(let* ((nelisp-http-test--tmpdir
           (make-temp-file "nelisp-http-test-" t))
          (nelisp-state-db-path
           (expand-file-name "test-state.db" nelisp-http-test--tmpdir)))
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

(provide 'nelisp-http-test)
;;; nelisp-http-test.el ends here
