;;; nelisp-m365-test.el --- Tests for nelisp-m365  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Offline tests for the pure layers: substrate shims, HTML rendering,
;; OData query construction and MCP protocol dispatch.  Nothing here
;; touches the network or the token cache, so the suite runs anywhere.
;;
;; The Graph request path itself is exercised by the smoke drivers under
;; the standalone runtime, since its interesting behaviour -- 401 retry,
;; 429 backoff, nextLink paging -- is only meaningful against a real
;; endpoint.

;;; Code:

(require 'ert)
(require 'nelisp-m365-compat)
(require 'nelisp-m365-curl)
(require 'nelisp-m365-auth)
(require 'nelisp-m365-graph)
(require 'nelisp-m365-tools)
(require 'nelisp-m365-mcp)

;;; Substrate shims -----------------------------------------------------

(ert-deftest nelisp-m365-test-curl-lf-headers-preserve-body ()
  (should (equal (nelisp-m365-curl--split-response
                  "HTTP/1.1 200 OK\nX-Test: value\n\na\r\nb")
                 '(200 (("x-test" . "value")) "a\r\nb"))))

(ert-deftest nelisp-m365-test-curl-body-file-cleanup ()
  (let ((file nil))
    (cl-letf (((symbol-function 'nelisp-m365-compat-curl-program) (lambda () "fake"))
              ((symbol-function 'nelisp-m365-compat-write-file)
               (lambda (path text private)
                 (should private)
                 (should (equal text "a=quoted&b=tail"))
                 (write-region text nil path)))
              ((symbol-function 'nelisp-m365-compat-run-program)
               (lambda (argv &optional _stderr)
                 (setq file (substring (cadr (member "--data-binary" argv)) 1))
                 (should (equal (nelisp-m365-compat-read-file file) "a=quoted&b=tail"))
                 '(0 . "HTTP/1.1 200 OK\r\n\r\n{}"))))
      (should (= (plist-get (nelisp-m365-curl-request
                            "POST" "https://offline.invalid" :body "a=quoted&b=tail") :status) 200)))
    (should-not (nelisp-m365-compat-read-file file))))

(ert-deftest nelisp-m365-test-curl-failure-cleans-body ()
  (let ((file nil))
    (cl-letf (((symbol-function 'nelisp-m365-compat-curl-program) (lambda () "fake"))
              ((symbol-function 'nelisp-m365-compat-write-file)
               (lambda (path text _private) (write-region text nil path)))
              ((symbol-function 'nelisp-m365-compat-run-program)
               (lambda (argv &optional _stderr)
                 (setq file (substring (cadr (member "--data-binary" argv)) 1))
                 '(23 . ""))))
      (should-error (nelisp-m365-curl-request "POST" "https://offline.invalid" :body "body")
                    :type 'nelisp-m365-http-error))
    (should-not (nelisp-m365-compat-read-file file))))

(ert-deftest nelisp-m365-test-private-write-fails-closed ()
  (let ((file (make-temp-file "m365-private-")))
    (unwind-protect
        (progn
          (write-region "old" nil file)
          (cl-letf (((symbol-function 'nelisp-m365-compat-windows-p) (lambda () t))
                    ((symbol-function 'nelisp-m365-compat--protect-file)
                     (lambda (_) (error "ACL failure"))))
            (should-error (nelisp-m365-compat-write-file file "secret" t)))
          (should (equal (nelisp-m365-compat-read-file file) "old")))
      (delete-file file))))

(ert-deftest nelisp-m365-test-iso8601-known-epochs ()
  "ISO 8601 rendering matches known instants."
  (should (equal (nelisp-m365-compat-iso8601-utc 0) "1970-01-01T00:00:00Z"))
  (should (equal (nelisp-m365-compat-iso8601-utc 1) "1970-01-01T00:00:01Z"))
  (should (equal (nelisp-m365-compat-iso8601-utc 946684800)
                 "2000-01-01T00:00:00Z"))
  (should (equal (nelisp-m365-compat-iso8601-utc 1787497090)
                 "2026-08-23T14:58:10Z")))

(ert-deftest nelisp-m365-test-iso8601-leap-day ()
  "The civil calendar conversion handles leap days.
2000 is a leap year under the 400-rule and 1900 is not, which is where a
naive conversion drifts."
  (should (equal (nelisp-m365-compat-iso8601-utc 951782400)
                 "2000-02-29T00:00:00Z"))
  (should (equal (nelisp-m365-compat-iso8601-utc 1709164800)
                 "2024-02-29T00:00:00Z"))
  (should (equal (nelisp-m365-compat-iso8601-utc 1709251199)
                 "2024-02-29T23:59:59Z")))

(ert-deftest nelisp-m365-test-utf8-bytes ()
  "Characters encode to the UTF-8 byte sequences the URL encoder needs."
  (should (equal (nelisp-m365-compat-string-to-utf8-bytes "A") '(65)))
  (should (equal (nelisp-m365-compat-string-to-utf8-bytes "é") '(195 169)))
  (should (equal (nelisp-m365-compat-string-to-utf8-bytes "年")
                 '(229 185 180)))
  (should (equal (nelisp-m365-compat-string-to-utf8-bytes "😀")
                 '(240 159 152 128))))

(ert-deftest nelisp-m365-test-url-encode ()
  "Percent-encoding leaves unreserved characters alone."
  (should (equal (nelisp-m365-compat-url-encode "abcXYZ019-_.~")
                 "abcXYZ019-_.~"))
  (should (equal (nelisp-m365-compat-url-encode "a b&c=d/e")
                 "a%20b%26c%3Dd%2Fe"))
  (should (equal (nelisp-m365-compat-url-encode "年次")
                 "%E5%B9%B4%E6%AC%A1")))

(ert-deftest nelisp-m365-test-form-encode ()
  "Form bodies join encoded pairs with ampersands."
  (should (equal (nelisp-m365-compat-form-encode
                  '(("grant_type" . "refresh_token") ("client_id" . "a b")))
                 "grant_type=refresh_token&client_id=a%20b"))
  (should (equal (nelisp-m365-compat-form-encode nil) "")))

(ert-deftest nelisp-m365-test-split-helpers ()
  "Literal splitting stands in for the unavailable regexp capture groups."
  (should (equal (nelisp-m365-compat-split-once "HTTP/1.1 200 OK" " ")
                 '("HTTP/1.1" . "200 OK")))
  (should (equal (nelisp-m365-compat-split-once "abc" "|") nil))
  (should (equal (nelisp-m365-compat-split-all "a\r\nb\r\nc" "\r\n")
                 '("a" "b" "c")))
  (should (equal (nelisp-m365-compat-split-all "" ",") '(""))))

(ert-deftest nelisp-m365-test-ascii-escaping ()
  "Non-ASCII escapes to \\uXXXX so a body can be written to disk.
The runtime's `write-region' compares bytes written against characters
given and signals when they differ, so a request body containing
Japanese cannot reach a file unescaped."
  (should (nelisp-m365-compat-ascii-p "plain ascii"))
  (should-not (nelisp-m365-compat-ascii-p "年次"))
  (should (equal (nelisp-m365-compat-escape-non-ascii "abc") "abc"))
  (should (equal (nelisp-m365-compat-escape-non-ascii "a年b")
                 "a\\u5E74b"))
  (should (equal (nelisp-m365-compat-escape-non-ascii "é") "\\u00E9"))
  ;; Outside the BMP JSON has no \U, so a surrogate pair is required.
  (should (equal (nelisp-m365-compat-escape-non-ascii "😀")
                 "\\uD83D\\uDE00"))
  ;; The escaped form must parse back to the original text.
  (let* ((value (list (cons "subject" "年次点検 ✓")))
         (encoded (nelisp-m365-compat-json-encode-ascii value)))
    (should (nelisp-m365-compat-ascii-p encoded))
    (should (equal (cdr (assoc "subject"
                               (nelisp-m365-compat-json-parse encoded)))
                   "年次点検 ✓"))))

(ert-deftest nelisp-m365-test-json-shapes ()
  "Arrays encode from vectors and booleans avoid null."
  (should (equal (nelisp-m365-compat-json-array '(1 2)) [1 2]))
  (should (equal (nelisp-m365-compat-json-array nil) []))
  (should (equal (nelisp-m365-compat-json-array [3]) [3]))
  (should (eq (nelisp-m365-compat-json-bool "x") t))
  (should (eq (nelisp-m365-compat-json-bool nil) :json-false)))

;;; HTTP response parsing ------------------------------------------------

(ert-deftest nelisp-m365-test-parse-status-line ()
  "Status codes are recovered without regexp groups."
  (should (equal (nelisp-m365-curl--status-of "HTTP/1.1 200 OK") 200))
  (should (equal (nelisp-m365-curl--status-of "HTTP/2 404 Not Found") 404))
  (should (equal (nelisp-m365-curl--status-of "HTTP/2 204") 204))
  (should (equal (nelisp-m365-curl--status-of "garbage") nil)))

(ert-deftest nelisp-m365-test-split-response ()
  "Header and body are separated, and stacked blocks collapse to the last."
  (let ((parts (nelisp-m365-curl--split-response
                "HTTP/1.1 200 OK\r\nContent-Type: application/json\r\n\r\n{\"a\":1}")))
    (should (equal (nth 0 parts) 200))
    (should (equal (cdr (assoc "content-type" (nth 1 parts)))
                   "application/json"))
    (should (equal (nth 2 parts) "{\"a\":1}")))
  ;; A followed redirect leaves two header blocks ahead of the body.
  (let ((parts (nelisp-m365-curl--split-response
                (concat "HTTP/1.1 302 Found\r\nLocation: https://x\r\n\r\n"
                        "HTTP/1.1 200 OK\r\nEtag: \"z\"\r\n\r\nbody"))))
    (should (equal (nth 0 parts) 200))
    (should (equal (cdr (assoc "etag" (nth 1 parts))) "\"z\""))
    (should (equal (nth 2 parts) "body"))))

;;; Graph query construction ----------------------------------------------

(ert-deftest nelisp-m365-test-graph-query ()
  "Empty options drop out so callers can pass them unconditionally."
  (should (equal (nelisp-m365-graph-query nil) ""))
  (should (equal (nelisp-m365-graph-query '(("$top" . nil) ("$skip" . "")))
                 ""))
  (should (equal (nelisp-m365-graph-query '(("$top" . 10)))
                 "?$top=10"))
  (should (equal (nelisp-m365-graph-query
                  '(("$select" . "id,subject") ("$top" . 5)))
                 "?$select=id%2Csubject&$top=5")))

(ert-deftest nelisp-m365-test-escape-odata ()
  "Single quotes double inside an OData string literal."
  (should (equal (nelisp-m365-graph-escape-odata "O'Brien") "O''Brien"))
  (should (equal (nelisp-m365-graph-escape-odata "plain") "plain")))

(ert-deftest nelisp-m365-test-graph-pick ()
  "Field trimming keeps requested keys and drops absent ones."
  (let ((row '(("id" . "1") ("subject" . "s") ("noise" . "n"))))
    (should (equal (nelisp-m365-graph-pick row '("id" "subject" "missing"))
                   '(("id" . "1") ("subject" . "s"))))))

(ert-deftest nelisp-m365-test-graph-addresses ()
  "Recipient arrays flatten to plain address strings.
The parser hands these over as vectors, but a list must work too."
  (let ((recipients [((("emailAddress" . (("address" . "a@example.com")))))
                     ((("emailAddress" . (("address" . "b@example.com")))))]))
    (should (equal (nelisp-m365-graph-addresses
                    (vector (car (aref recipients 0))
                            (car (aref recipients 1))))
                   '("a@example.com" "b@example.com"))))
  (should (equal (nelisp-m365-graph-addresses
                  '((("emailAddress" . (("address" . "c@example.com"))))))
                 '("c@example.com"))))

(ert-deftest nelisp-m365-test-json-round-trip-nested-arrays ()
  "A Graph row with a nested array of objects survives parse and re-encode.
This is the shape that broke `m365_list_contacts': `emailAddresses' is
an array of objects, and parsing arrays as lists made it unencodable."
  (let* ((raw (concat "{\"displayName\":\"X\",\"emailAddresses\":"
                      "[{\"name\":\"a\",\"address\":\"a@example.com\"}],"
                      "\"categories\":[],\"companyName\":null,"
                      "\"flagged\":false}"))
         (parsed (nelisp-m365-compat-json-parse raw)))
    (should (vectorp (cdr (assoc "emailAddresses" parsed))))
    (should (null (cdr (assoc "companyName" parsed))))
    (should (eq (cdr (assoc "flagged" parsed)) :json-false))
    ;; The encoder must accept it back without complaint.
    (let ((again (nelisp-m365-compat-json-parse
                  (nelisp-m365-compat-json-encode parsed))))
      (should (equal (cdr (assoc "address"
                                 (aref (cdr (assoc "emailAddresses" again)) 0)))
                     "a@example.com"))
      (should (eq (cdr (assoc "flagged" again)) :json-false)))))

(ert-deftest nelisp-m365-test-to-list ()
  "Array iteration accepts either shape."
  (should (equal (nelisp-m365-compat-to-list [1 2 3]) '(1 2 3)))
  (should (equal (nelisp-m365-compat-to-list '(1 2 3)) '(1 2 3)))
  (should (equal (nelisp-m365-compat-to-list []) nil)))

;;; HTML rendering ----------------------------------------------------------

(ert-deftest nelisp-m365-test-html-strips-tags ()
  "Markup is removed and block boundaries become newlines."
  ;; Both the closing and the opening tag contribute a break, which is
  ;; what makes paragraphs read as paragraphs.
  (should (equal (nelisp-m365-tools-html-to-text "<p>one</p><p>two</p>")
                 "one\n\ntwo"))
  (should (equal (nelisp-m365-tools-html-to-text "a<br>b") "a\nb"))
  (should (equal (nelisp-m365-tools-html-to-text "<b>bold</b>") "bold"))
  (should (equal (nelisp-m365-tools-html-to-text "") "")))

(ert-deftest nelisp-m365-test-html-drops-active-content ()
  "Script and style bodies never reach the caller."
  (should (equal (nelisp-m365-tools-html-to-text
                  "<div>keep<script>alert('x')</script>this</div>")
                 "keep\nthis"))
  (should (equal (nelisp-m365-tools-html-to-text
                  "<style>body{color:red}</style>text")
                 "text"))
  ;; An unclosed script must not leak its payload either.
  (should (equal (nelisp-m365-tools-html-to-text "ok<script>secret")
                 "ok")))

(ert-deftest nelisp-m365-test-html-decodes-entities ()
  "Entities decode, and an encoded entity does not become live markup."
  (should (equal (nelisp-m365-tools-html-to-text "a&amp;b") "a&b"))
  (should (equal (nelisp-m365-tools-html-to-text "&lt;tag&gt;") "<tag>"))
  (should (equal (nelisp-m365-tools-html-to-text "&amp;lt;") "&lt;"))
  (should (equal (nelisp-m365-tools-html-to-text "x&nbsp;y") "x y")))

(ert-deftest nelisp-m365-test-html-entity-edge-cases ()
  "The single-pass entity decoder handles the awkward inputs.
Rewritten from a chain of `string-replace' calls, which allocated
gigabytes; these cases pin the behaviour that rewrite has to preserve."
  ;; A bare ampersand in prose is left alone, and does not send the
  ;; decoder scanning to the end of the document looking for a semicolon.
  (should (equal (nelisp-m365-tools-html-to-text "Tom & Jerry") "Tom & Jerry"))
  ;; An unknown entity is passed through rather than eaten.
  (should (equal (nelisp-m365-tools-html-to-text "a&zzz;b") "a&zzz;b"))
  ;; A semicolon far away is not treated as an entity terminator.
  (should (equal (nelisp-m365-tools-html-to-text "a & b ; c") "a & b ; c"))
  ;; Adjacent entities both decode.
  (should (equal (nelisp-m365-tools-html-to-text "&lt;&gt;") "<>"))
  ;; Text produced by a replacement is never re-examined, so an encoded
  ;; entity cannot become live markup.
  (should (equal (nelisp-m365-tools-html-to-text "&amp;lt;") "&lt;"))
  (should (equal (nelisp-m365-tools-html-to-text "&amp;amp;") "&amp;")))

(ert-deftest nelisp-m365-test-html-collapses-blank-runs ()
  "Long runs of blank lines collapse to a single separator."
  (should (equal (nelisp-m365-tools-html-to-text "<p>a</p><p></p><p></p><p>b</p>")
                 "a\n\nb"))
  (should (equal (nelisp-m365-tools-html-to-text "a<br><br><br><br>b")
                 "a\n\nb")))

(ert-deftest nelisp-m365-test-html-drops-uppercase-script ()
  "Container skipping is case-insensitive without copying the document.
The downcased twin this used to rely on cost about 160 MB per 7 KB of
input, so the tag name is now normalised one tag at a time."
  (should (equal (nelisp-m365-tools-html-to-text
                  "<DIV>keep<SCRIPT>alert('x')</SCRIPT>this</DIV>")
                 "keep\nthis"))
  (should (equal (nelisp-m365-tools-html-to-text
                  "a<Style>p{color:red}</Style>b")
                 "a\nb")))

(ert-deftest nelisp-m365-test-html-keeps-japanese ()
  "Non-ASCII text survives the tag walk unchanged."
  (should (equal (nelisp-m365-tools-html-to-text
                  "<p>年次点検</p>")
                 "年次点検")))

;;; Tool registry ------------------------------------------------------------

(ert-deftest nelisp-m365-test-registry-well-formed ()
  "Every tool has the fields the MCP layer reads."
  (let ((names nil))
    (dolist (tool (nelisp-m365-tools-registry))
      (let ((name (plist-get tool :name)))
        (should (stringp name))
        (should (not (member name names)))
        (push name names)
        (should (stringp (plist-get tool :title)))
        (should (stringp (plist-get tool :description)))
        (should (functionp (plist-get tool :handler)))
        (let ((schema (plist-get tool :schema)))
          (should (equal (cdr (assoc "type" schema)) "object"))
          (should (assoc "properties" schema))
          (should (vectorp (cdr (assoc "required" schema)))))))
    (should (= (length names) 34))))

(ert-deftest nelisp-m365-test-write-tools-are-off-by-default ()
  "The write tools are absent until they are switched on.
Sending mail is outward-facing and irreversible, so it should not be
sitting in the registry of a session that only meant to read."
  (let ((nelisp-m365-write-enabled nil))
    (let ((names (mapcar (lambda (tool) (plist-get tool :name))
                         (nelisp-m365-tools-registry))))
      (should (= (length names) 34))
      (should-not (member "m365_send_mail" names))
      (should-not (member "m365_create_draft" names))))
  (let ((nelisp-m365-write-enabled t))
    (let ((names (mapcar (lambda (tool) (plist-get tool :name))
                         (nelisp-m365-tools-registry))))
      (should (= (length names) 47))
      (should (member "m365_send_mail" names))
      (should (member "m365_create_draft" names)))))

(ert-deftest nelisp-m365-test-write-tools-well-formed ()
  "Write tools declare themselves as writes and carry usable schemas."
  (let ((nelisp-m365-write-enabled t))
    (dolist (tool (nelisp-m365-tools--write-registry))
      (should (stringp (plist-get tool :name)))
      (should (functionp (plist-get tool :handler)))
      (should-not (plist-get tool :read-only))
      (let* ((schema (plist-get tool :schema))
             (props (cdr (assoc "properties" schema)))
             (required (cdr (assoc "required" schema))))
        (should (vectorp required))
        (dolist (name (append required nil))
          (should (assoc name props)))))
    ;; Everything that removes existing data says so, and nothing else
    ;; claims to -- the annotation is what a client shows before asking
    ;; the user to confirm.
    (dolist (name '("m365_delete_event" "m365_delete_mail"
                    "m365_delete_onedrive_item" "m365_delete_todo_task"))
      (should (plist-get (nelisp-m365-mcp--find-tool name) :destructive)))
    (dolist (name '("m365_create_draft" "m365_send_draft" "m365_send_mail"
                    "m365_create_event" "m365_upload_onedrive_file"
                    "m365_create_todo_task" "m365_complete_todo_task"))
      (should-not (plist-get (nelisp-m365-mcp--find-tool name)
                             :destructive)))))

(ert-deftest nelisp-m365-test-create-folder-request ()
  "Create exactly one named folder and fail on server name conflicts."
  (cl-letf (((symbol-function 'nelisp-m365-tools-onedrive-metadata)
             (lambda (args)
               (should (equal args '(("itemId" . "parent"))))
               '(("id" . "parent") ("folder" . nil))))
            ((symbol-function 'nelisp-m365-graph-post)
             (lambda (path body)
               (should (equal path "/me/drive/items/parent/children"))
               (should (equal (cdr (assoc "name" body)) "Customers"))
               (should (hash-table-p (cdr (assoc "folder" body))))
               (should (equal (cdr (assoc "@microsoft.graph.conflictBehavior" body)) "fail"))
               '(("id" . "child") ("name" . "Customers") ("folder" . nil)))))
    (should (equal (cdr (assoc "id" (nelisp-m365-tools-create-onedrive-folder
                                    '(("parentId" . "parent") ("name" . "Customers")))))
                   "child"))))

(ert-deftest nelisp-m365-test-create-folder-invalid-name ()
  "Reject path traversal and invalid components before any request."
  (cl-letf (((symbol-function 'nelisp-m365-tools-onedrive-metadata)
             (lambda (&rest _) (ert-fail "Unexpected network request"))))
    (dolist (name '("" "." ".." "a/b" "a\\b" "a:b" "a?b" "a\nb" "tail." "tail "))
      (should-error (nelisp-m365-tools-create-onedrive-folder
                     (list (cons "parentId" "parent") (cons "name" name)))))))

(ert-deftest nelisp-m365-test-create-folder-file-parent ()
  "A file parent cannot reach the write request."
  (cl-letf (((symbol-function 'nelisp-m365-tools-onedrive-metadata)
             (lambda (_) '(("id" . "parent") ("file" . nil))))
            ((symbol-function 'nelisp-m365-graph-post)
             (lambda (&rest _) (ert-fail "Unexpected write request"))))
    (should-error (nelisp-m365-tools-create-onedrive-folder
                   '(("parentId" . "parent") ("name" . "Customers"))))))

(ert-deftest nelisp-m365-test-create-folder-conflict ()
  "An existing name fails once; it is never replaced or renamed."
  (let ((writes 0))
    (cl-letf (((symbol-function 'nelisp-m365-tools-onedrive-metadata)
               (lambda (_) '(("id" . "parent") ("folder" . nil))))
              ((symbol-function 'nelisp-m365-graph-post)
               (lambda (&rest _) (setq writes (1+ writes)) (error "409 nameAlreadyExists"))))
      (should-error (nelisp-m365-tools-create-onedrive-folder
                     '(("parentId" . "parent") ("name" . "Customers"))))
      (should (= writes 1)))))

(ert-deftest nelisp-m365-test-recipient-shapes ()
  "A recipient argument accepts one address or several."
  ;; Checked through the reader rather than against a literal: the two
  ;; have to agree on the shape, which is the property that matters.
  (let ((one (nelisp-m365-tools--recipients "a@example.com")))
    (should (vectorp one))
    (should (= (length one) 1))
    (should (equal (nelisp-m365-graph-address (aref one 0))
                   "a@example.com")))
  (let ((many (nelisp-m365-tools--recipients ["a@example.com" "b@example.com"])))
    (should (= (length many) 2))
    (should (equal (nelisp-m365-graph-address (aref many 1))
                   "b@example.com")))
  (should (equal (length (nelisp-m365-tools--recipients [])) 0)))

(ert-deftest nelisp-m365-test-message-body-shape ()
  "A composed message carries the fields Graph expects."
  (let ((msg (nelisp-m365-tools--message-body
              '(("to" . "a@example.com") ("subject" . "件名")
                ("body" . "本文")))))
    (should (equal (cdr (assoc "subject" msg)) "件名"))
    (should (equal (cdr (assoc "contentType" (cdr (assoc "body" msg))))
                   "Text"))
    (should (vectorp (cdr (assoc "toRecipients" msg))))
    ;; Absent optional recipients are omitted, not sent as null.
    (should-not (assoc "ccRecipients" msg)))
  (let ((msg (nelisp-m365-tools--message-body
              '(("to" . "a@example.com") ("html" . t)))))
    (should (equal (cdr (assoc "contentType" (cdr (assoc "body" msg))))
                   "HTML"))))

(ert-deftest nelisp-m365-test-registry-required-args-declared ()
  "Tools that require an argument also declare it in properties."
  (dolist (tool (nelisp-m365-tools-registry))
    (let* ((schema (plist-get tool :schema))
           (props (cdr (assoc "properties" schema)))
           (required (cdr (assoc "required" schema))))
      (dolist (name (append required nil))
        (should (assoc name props))))))

(ert-deftest nelisp-m365-test-int-arg-clamping ()
  "maxResults style arguments are clamped rather than trusted."
  (should (= (nelisp-m365-tools--int-arg '(("n" . 5)) "n" 10 1 50) 5))
  (should (= (nelisp-m365-tools--int-arg '(("n" . 999)) "n" 10 1 50) 50))
  (should (= (nelisp-m365-tools--int-arg '(("n" . 0)) "n" 10 1 50) 1))
  (should (= (nelisp-m365-tools--int-arg nil "n" 10 1 50) 10))
  (should (= (nelisp-m365-tools--int-arg '(("n" . "7")) "n" 10 1 50) 7))
  (should (= (nelisp-m365-tools--int-arg '(("n" . "junk")) "n" 10 1 50) 1)))

(ert-deftest nelisp-m365-test-thread-ordering ()
  "Thread messages sort oldest first on the client.
Exchange answers `InefficientFilter' when `$orderby' is combined with a
`conversationId' filter, so the ordering has to happen here."
  ;; Built with `list' rather than quoted: `sort' is destructive, and a
  ;; quoted literal is shared across calls to the enclosing function.
  (let ((rows (list (list (cons "receivedDateTime" "2026-08-03T10:00:00Z")
                          (cons "id" "c"))
                    (list (cons "receivedDateTime" "2026-08-01T09:00:00Z")
                          (cons "id" "a"))
                    (list (cons "receivedDateTime" "2026-08-02T23:59:59Z")
                          (cons "id" "b")))))
    (should (equal (mapcar (lambda (r) (cdr (assoc "id" r)))
                           (sort rows #'nelisp-m365-tools--by-received))
                   '("a" "b" "c"))))
  ;; A row missing the field must not break the comparison.
  (should (listp (sort (list '(("id" . "x"))
                             '(("receivedDateTime" . "2026-01-01T00:00:00Z")))
                       #'nelisp-m365-tools--by-received))))

;;; Download safety ---------------------------------------------------------

(ert-deftest nelisp-m365-test-safe-basename ()
  "A hostile attachment name cannot steer a write out of the directory.
Attachment and drive item names are chosen by whoever sent the mail or
shared the file, so path separators, drive letters and dot segments have
to be stripped rather than trusted."
  (should (equal (nelisp-m365-tools--safe-basename "report.pdf") "report.pdf"))
  (should (equal (nelisp-m365-tools--safe-basename "年次点検.pdf") "年次点検.pdf"))
  (should-not (string-search "/" (nelisp-m365-tools--safe-basename
                                  "../../etc/passwd")))
  (should-not (string-search "\\" (nelisp-m365-tools--safe-basename
                                   "..\\..\\windows\\system32\\evil.dll")))
  (should-not (string-prefix-p "." (nelisp-m365-tools--safe-basename
                                    ".bashrc")))
  (should-not (string-search ":" (nelisp-m365-tools--safe-basename
                                  "C:/Windows/evil.exe")))
  ;; A name made only of structural characters still yields a usable one.
  (should (equal (nelisp-m365-tools--safe-basename "..") "download"))
  (should (equal (nelisp-m365-tools--safe-basename "") "download"))
  (should (equal (nelisp-m365-tools--safe-basename nil) "download")))

(ert-deftest nelisp-m365-test-download-requires-configured-dir ()
  "Downloads refuse to run when no directory is configured."
  (let ((nelisp-m365-download-dir nil))
    (should-error (nelisp-m365-tools--download-path "x.pdf"))))

;;; MCP dispatch ---------------------------------------------------------------

(ert-deftest nelisp-m365-test-mcp-initialize ()
  "Initialization echoes a supported protocol version and names the server."
  (let* ((resp (nelisp-m365-mcp-handle
                '(("jsonrpc" . "2.0") ("id" . 1) ("method" . "initialize")
                  ("params" . (("protocolVersion" . "2025-06-18"))))))
         (result (cdr (assoc "result" resp))))
    (should (equal (cdr (assoc "protocolVersion" result)) "2025-06-18"))
    (should (equal (cdr (assoc "name" (cdr (assoc "serverInfo" result))))
                   "nelisp-m365"))))

(ert-deftest nelisp-m365-test-mcp-initialize-unknown-version ()
  "An unrecognised client version falls back to our newest, not an error."
  (let* ((resp (nelisp-m365-mcp-handle
                '(("jsonrpc" . "2.0") ("id" . 1) ("method" . "initialize")
                  ("params" . (("protocolVersion" . "1999-01-01"))))))
         (result (cdr (assoc "result" resp))))
    (should (equal (cdr (assoc "protocolVersion" result)) "2025-06-18"))))

(ert-deftest nelisp-m365-test-mcp-tools-list ()
  "tools/list returns a JSON array of descriptors."
  (let* ((resp (nelisp-m365-mcp-handle
                '(("jsonrpc" . "2.0") ("id" . 2) ("method" . "tools/list"))))
         (tools (cdr (assoc "tools" (cdr (assoc "result" resp))))))
    (should (vectorp tools))
    (should (= (length tools) 34))
    (should (equal (cdr (assoc "name" (aref tools 0))) "m365_authenticate"))))

(ert-deftest nelisp-m365-test-mcp-notification-has-no-reply ()
  "Notifications must not be answered."
  (should (null (nelisp-m365-mcp-handle
                 '(("jsonrpc" . "2.0") ("method" . "notifications/initialized")))))
  (should (null (nelisp-m365-mcp-handle '(("jsonrpc" . "2.0"))))))

(ert-deftest nelisp-m365-test-mcp-unknown-method ()
  "An unknown method is a JSON-RPC error, not a tool result."
  (let* ((resp (nelisp-m365-mcp-handle
                '(("jsonrpc" . "2.0") ("id" . 9) ("method" . "nope"))))
         (err (cdr (assoc "error" resp))))
    (should (equal (cdr (assoc "code" err)) -32601))))

(ert-deftest nelisp-m365-test-mcp-unknown-tool-is-tool-error ()
  "An unknown tool comes back as isError so the model can read why."
  (let* ((resp (nelisp-m365-mcp-handle
                '(("jsonrpc" . "2.0") ("id" . 3) ("method" . "tools/call")
                  ("params" . (("name" . "nope") ("arguments" . nil))))))
         (result (cdr (assoc "result" resp))))
    (should (eq (cdr (assoc "isError" result)) t))))

(ert-deftest nelisp-m365-test-mcp-tool-error-is-reported ()
  "A signalling handler becomes a readable tool error, not a crash."
  (let* ((nelisp-m365-token-cache-file nil)
         (resp (nelisp-m365-mcp-handle
                '(("jsonrpc" . "2.0") ("id" . 4) ("method" . "tools/call")
                  ("params" . (("name" . "m365_profile") ("arguments" . nil))))))
         (result (cdr (assoc "result" resp))))
    (should (eq (cdr (assoc "isError" result)) t))
    (should (stringp (cdr (assoc "text" (aref (cdr (assoc "content" result))
                                              0)))))))

(ert-deftest nelisp-m365-test-mcp-untrusted-wrapping ()
  "Content-bearing tools carry the untrusted-input warning."
  (let* ((tool (nelisp-m365-mcp--find-tool "m365_get_mail"))
         (payload (nelisp-m365-mcp--tool-payload tool '(("subject" . "hi"))))
         (structured (cdr (assoc "structuredContent" payload))))
    (should (equal (cdr (assoc "warning" structured))
                   nelisp-m365-mcp-untrusted-warning))
    (should (equal (cdr (assoc "data" structured)) '(("subject" . "hi")))))
  ;; Status reports no account content, so it is not wrapped.
  (let* ((tool (nelisp-m365-mcp--find-tool "m365_status"))
         (payload (nelisp-m365-mcp--tool-payload tool '(("connected" . t))))
         (structured (cdr (assoc "structuredContent" payload))))
    (should (null (assoc "warning" structured)))))

;;; Authentication configuration --------------------------------------------

(ert-deftest nelisp-m365-test-auth-endpoint-is-consumers ()
  "Endpoints target the consumers tenant, which is what makes this a
personal-account connector rather than a work or school one."
  (let ((nelisp-m365-tenant "consumers"))
    (should (equal (nelisp-m365-auth--endpoint "devicecode")
                   "https://login.microsoftonline.com/consumers/oauth2/v2.0/devicecode"))
    (should (equal (nelisp-m365-auth--endpoint "token")
                   "https://login.microsoftonline.com/consumers/oauth2/v2.0/token"))))

(ert-deftest nelisp-m365-test-auth-requires-client-id ()
  "A missing client id is reported, never defaulted to an embedded one."
  (let ((nelisp-m365-client-id nil))
    (should-error (nelisp-m365-auth--require-client-id)
                  :type 'nelisp-m365-auth-error)))

(ert-deftest nelisp-m365-test-auth-scopes-are-read-only ()
  "The default scope set grants no write access."
  (dolist (scope nelisp-m365-scopes)
    (should-not (string-suffix-p ".ReadWrite" scope))
    (should-not (string-suffix-p ".Send" scope))))

(ert-deftest nelisp-m365-test-timestamps-are-integers ()
  "Persisted timestamps must be integers.
`nelisp-json-encode' rejects a float as NaN-or-infinity, and these
values are written straight into the token cache."
  (should (integerp (nelisp-m365-compat-now)))
  (let ((token (nelisp-m365-auth--token-from-response
                '(("access_token" . "a") ("expires_in" . 3599)) nil)))
    (should (integerp (cdr (assoc "expires_at" token))))
    (should (integerp (cdr (assoc "obtained_at" token))))))

(ert-deftest nelisp-m365-test-token-keeps-refresh-token ()
  "A refresh response without a refresh_token reuses the stored one."
  (let ((token (nelisp-m365-auth--token-from-response
                '(("access_token" . "new") ("expires_in" . 3600))
                '(("refresh_token" . "keep")))))
    (should (equal (cdr (assoc "access_token" token)) "new"))
    (should (equal (cdr (assoc "refresh_token" token)) "keep"))
    (should (numberp (cdr (assoc "expires_at" token))))))

(ert-deftest nelisp-m365-test-conditional-upload ()
  "An exact version reaches Graph; absent/wildcard conditions never upload."
  (let (calls)
    (cl-letf (((symbol-function 'nelisp-m365-compat-file-size) (lambda (_) 12))
              ((symbol-function 'nelisp-m365-graph-upload)
               (lambda (path source &rest options)
                 (push (list path source options) calls)
                 '(("id" . "file-id") ("eTag" . "new-tag")))))
      (dolist (args '((("path" . "fixture.org") ("itemId" . "file-id"))
                      (("path" . "fixture.org") ("destination" . "fixture.org") ("expectedETag" . "tag"))
                      (("path" . "fixture.org") ("itemId" . "file-id") ("expectedETag" . "*"))
                      (("path" . "fixture.org") ("itemId" . "file-id") ("expectedETag" . " * "))
                      (("path" . "fixture.org") ("itemId" . "file-id") ("expectedETag" . "\"tag\r\n\""))))
        (should-error (nelisp-m365-tools-upload-onedrive args)))
      (should-not calls)
      (let ((result (nelisp-m365-tools-upload-onedrive
                     '(("path" . "fixture.org") ("itemId" . "file-id") ("expectedETag" . "\"old-tag\"")))))
        (should (equal (cdr (assoc "eTag" result)) "new-tag"))
        (should (equal (caar calls) "/me/drive/items/file-id/content"))
        (should (equal (plist-get (nth 2 (car calls)) :headers) '(("If-Match" . "\"old-tag\""))))))))

(ert-deftest nelisp-m365-test-stale-upload-propagates ()
  "A Graph precondition failure remains an error, with no fallback upload."
  (let ((calls 0))
    (cl-letf (((symbol-function 'nelisp-m365-compat-file-size) (lambda (_) 12))
              ((symbol-function 'nelisp-m365-graph-upload)
               (lambda (&rest _)
                 (setq calls (1+ calls))
                 (error "HTTP 412 Precondition Failed"))))
      (should-error (nelisp-m365-tools-upload-onedrive
                     '(("path" . "fixture.org") ("itemId" . "file-id") ("expectedETag" . "\"stale-tag\""))))
      (should (= calls 1)))))

(ert-deftest nelisp-m365-test-text-version-stability ()
  "Text reads expose a stable version and reject concurrent modifications."
  (dolist (changed '(nil t))
    (let ((calls 0))
      (cl-letf (((symbol-function 'nelisp-m365-graph-get)
                 (lambda (&rest _)
                   (setq calls (1+ calls))
                   (if (= calls 1)
                       '(("id" . "file-id") ("name" . "fixture.org") ("size" . 12)
                         ("file" . (("mimeType" . "text/plain"))) ("eTag" . "tag"))
                     (list (cons "eTag" (if changed "changed-tag" "tag"))))))
                ((symbol-function 'nelisp-m365-graph-download) (lambda (&rest _) 200))
                ((symbol-function 'nelisp-m365-compat-read-file) (lambda (_) "fixture text")))
        (if changed
            (should-error (nelisp-m365-tools-get-onedrive-text '(("itemId" . "file-id"))))
          (let ((result (nelisp-m365-tools-get-onedrive-text '(("itemId" . "file-id")))))
            (should (equal (cdr (assoc "text" result)) "fixture text"))
            (should (equal (cdr (assoc "eTag" (cdr (assoc "metadata" result)))) "tag"))))))))

(ert-deftest nelisp-m365-test-create-only-upload ()
  "Creation forwards an absence condition and cannot masquerade as an update."
  (let (headers upload-path)
    (cl-letf (((symbol-function 'nelisp-m365-compat-file-size) (lambda (_) 12))
              ((symbol-function 'nelisp-m365-graph-upload)
               (lambda (path _source &rest options)
                 (setq upload-path path)
                 (setq headers (plist-get options :headers))
                 '(("id" . "new-file")))))
      (nelisp-m365-tools-upload-onedrive '(("path" . "fixture.org") ("destination" . "fixture.org") ("createOnly" . t)))
      (should (equal headers '(("If-None-Match" . "*"))))
      (should (string-match-p "conflictBehavior=fail" upload-path))
      (should-error (nelisp-m365-tools-upload-onedrive
                     '(("path" . "fixture.org") ("itemId" . "file-id") ("expectedETag" . "\"tag\"") ("createOnly" . t)))))))

(ert-deftest nelisp-m365-test-text-preserves-windows-bytes ()
  "UTF-8 BOM and CRLF survive text reading for byte-exact publication checks."
  (let ((text "\ufeff* TODO Fixture\r\nSCHEDULED: <2026-10-02 Fri>\r\n"))
    (cl-letf (((symbol-function 'nelisp-m365-graph-get)
               (lambda (&rest _)
                 '(("id" . "file-id") ("name" . "fixture.org") ("size" . 100)
                   ("file" . (("mimeType" . "text/plain"))) ("eTag" . "\"tag\""))))
              ((symbol-function 'nelisp-m365-graph-download)
               (lambda (_path dest)
                 (let ((coding-system-for-write 'utf-8-unix))
                   (write-region text nil dest nil 'silent))
                 200)))
      (should (equal (cdr (assoc "text" (nelisp-m365-tools-get-onedrive-text '(("itemId" . "file-id"))))) text)))))

(ert-deftest nelisp-m365-test-stable-file-download ()
  "The temporary download keeps raw bytes and removes a failed snapshot."
  (dolist (changed '(nil t))
    (let ((calls 0) path (bytes "line one\r\nline two\r\n"))
      (cl-letf (((symbol-function 'nelisp-m365-tools-onedrive-metadata)
                 (lambda (&rest _)
                   (setq calls (1+ calls))
                   (list (cons "id" "file-id") (cons "size" (length bytes))
                         (cons "file" '(("mimeType" . "text/plain")))
                         (cons "eTag" (if (and changed (> calls 1)) "\"changed\"" "\"tag\"")))))
                ((symbol-function 'nelisp-m365-graph-download)
                 (lambda (_target dest)
                   (setq path dest)
                   (let ((coding-system-for-write 'no-conversion))
                     (write-region bytes nil dest nil 'silent))
                   200)))
        (if changed
            (progn
              (should-error (nelisp-m365-tools-download-onedrive '(("itemId" . "file-id"))))
              (should-not (file-exists-p path)))
          (let ((result (nelisp-m365-tools-download-onedrive '(("itemId" . "file-id")))))
            (unwind-protect
                (progn
                  (should (equal (cdr (assoc "localPath" result)) path))
                  (let ((coding-system-for-read 'no-conversion))
                    (should (equal (nelisp-m365-compat-read-file path) bytes))))
              (delete-file path))))))))

(ert-deftest nelisp-m365-test-compact-paged-folder-list ()
  "Compact listings expose versions and keep each HTTP page bounded."
  (let (path requested)
    (cl-letf (((symbol-function 'nelisp-m365-graph-collection)
               (lambda (query limit &rest _)
                 (setq path query requested limit)
                 '(:items nil :truncated nil))))
      (nelisp-m365-tools-list-onedrive '(("path" . "Documents/Notes-AI/capture/web") ("maxResults" . 10000) ("compact" . t)))
      (should (= requested 10000))
      (should (string-match-p "top=200" path))
      (should (string-match-p "eTag" path))
      (should-not (string-match-p "webUrl" path)))))

(ert-deftest nelisp-m365-test-listing-snapshot-guards ()
  "Reject foreign endpoints before transport and remove failed snapshots."
  (let ((calls 0) path)
    (cl-letf (((symbol-function 'nelisp-m365-graph-download)
               (lambda (_target dest)
                 (setq calls (1+ calls) path dest)
                 (write-region "{}" nil dest nil 'silent)
                 200)))
      (dolist (cursor '("https://evil.example/me/drive/root/children?$top=200"
                        "https://graph.microsoft.com/v1.0/users/x/messages?$top=200"
                        "https://graph.microsoft.com/v1.0/me/drive/items/../children?$top=200"
                        "https://graph.microsoft.com/v1.0/me/drive/?$top=200"))
        (should-error (nelisp-m365-tools-download-listing (list (cons "nextLink" cursor)))))
      (should (= calls 0))
      (let ((result (nelisp-m365-tools-download-listing '(("path" . "Documents/Notes-AI")))))
        (unwind-protect
            (should (= (cdr (assoc "size" result)) 2))
          (delete-file path))))
    (dolist (status '(503 200))
      (cl-letf (((symbol-function 'nelisp-m365-graph-download)
                 (lambda (_target dest)
                   (setq path dest)
                   (write-region (make-string 1025 ?x) nil dest nil 'silent)
                   status)))
        (should-error (nelisp-m365-tools-download-listing '(("maxBytes" . 1024))))
        (should-not (file-exists-p path))))))

(ert-deftest nelisp-m365-mcp-respond-returns-response-text ()
  "The shared daemon answers through `nelisp-m365-mcp-respond'."
  (should (equal "" (nelisp-m365-mcp-respond "  ")))
  (should (equal "" (nelisp-m365-mcp-respond
                     "{\"jsonrpc\":\"2.0\",\"method\":\"notifications/initialized\"}")))
  (should (string-match-p "\"result\""
                          (nelisp-m365-mcp-respond
                           "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"ping\"}")))
  (should (string-match-p "-32700" (nelisp-m365-mcp-respond "{bad")))
  ;; The cached tools/list answer equals the uncached one, every time.
  (let* ((line "{\"jsonrpc\":\"2.0\",\"id\":5,\"method\":\"tools/list\"}")
         (expected (nelisp-m365-compat-json-parse
                    (nelisp-m365-compat-json-encode
                     (nelisp-m365-mcp-handle (nelisp-m365-compat-json-parse line))))))
    (setq nelisp-m365-mcp--tools-json nil)
    (should (equal expected (nelisp-m365-compat-json-parse (nelisp-m365-mcp-respond line))))
    (should (equal expected (nelisp-m365-compat-json-parse (nelisp-m365-mcp-respond line))))))

(provide 'nelisp-m365-test)

;;; nelisp-m365-test.el ends here
