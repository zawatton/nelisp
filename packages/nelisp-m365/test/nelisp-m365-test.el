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
  (should (equal (nelisp-m365-compat-string-to-utf8-bytes "\U0001F600")
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
    (should (= (length names) 18))))

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
    (should (= (length tools) 18))
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

(provide 'nelisp-m365-test)

;;; nelisp-m365-test.el ends here
