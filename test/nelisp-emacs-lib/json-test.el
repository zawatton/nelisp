;;; json-test.el --- ERT for nelisp-emacs json.el  -*- lexical-binding: t; -*-

;; Phase B2 (= Doc anvil-runtime pure-elisp roadmap, 2026-05-09)
;; Coverage for the new `json-read-from-string' parser.  Behaviour
;; mirrors GNU Emacs json.el defaults (alist with symbol keys,
;; arrays as lists, true → t, false → :json-false, null → :json-null).

(require 'ert)
(require 'json)

;;;; primitive scalars

(ert-deftest json-read-true ()
  (should (eq (json-read-from-string "true") t)))

(ert-deftest json-read-false ()
  (should (eq (json-read-from-string "false") json-false))
  (should (eq (json-read-from-string "false") :json-false)))

(ert-deftest json-read-null ()
  (should (eq (json-read-from-string "null") json-null))
  (should (eq (json-read-from-string "null") :json-null)))

(ert-deftest json-read-int ()
  (should (= (json-read-from-string "42") 42))
  (should (= (json-read-from-string "-7") -7))
  (should (= (json-read-from-string "0") 0)))

(ert-deftest json-read-float ()
  (should (= (json-read-from-string "3.14") 3.14))
  (should (= (json-read-from-string "-0.5") -0.5))
  (should (= (json-read-from-string "1e3") 1000.0))
  (should (= (json-read-from-string "1.5e2") 150.0)))

(ert-deftest json-read-string ()
  (should (string= (json-read-from-string "\"hello\"") "hello")))

(ert-deftest json-read-string-escapes ()
  (should (string= (json-read-from-string "\"a\\nb\"") "a\nb"))
  (should (string= (json-read-from-string "\"a\\tb\"") "a\tb"))
  (should (string= (json-read-from-string "\"\\\"\"") "\""))
  (should (string= (json-read-from-string "\"\\\\\"") "\\"))
  (should (string= (json-read-from-string "\"\\u0041\"") "A")))

(ert-deftest json-read-string-empty ()
  (should (string= (json-read-from-string "\"\"") "")))

;;;; arrays

(ert-deftest json-read-array-empty ()
  (should (null (json-read-from-string "[]"))))

(ert-deftest json-read-array-numbers ()
  (should (equal (json-read-from-string "[1, 2, 3]") '(1 2 3))))

(ert-deftest json-read-array-mixed ()
  (should (equal (json-read-from-string "[1, \"a\", true, null]")
                 (list 1 "a" t json-null))))

(ert-deftest json-read-array-nested ()
  (should (equal (json-read-from-string "[[1,2],[3,4]]") '((1 2) (3 4)))))

;;;; objects

(ert-deftest json-read-object-empty ()
  (should (null (json-read-from-string "{}"))))

(ert-deftest json-read-object-flat ()
  (let ((obj (json-read-from-string "{\"a\": 1, \"b\": 2}")))
    (should (= (alist-get 'a obj) 1))
    (should (= (alist-get 'b obj) 2))))

(ert-deftest json-read-object-symbol-keys ()
  ;; Anvil-server expects (alist-get 'jsonrpc request) — symbol keys.
  (let ((obj (json-read-from-string "{\"jsonrpc\":\"2.0\",\"id\":1}")))
    (should (string= (alist-get 'jsonrpc obj) "2.0"))
    (should (= (alist-get 'id obj) 1))))

(ert-deftest json-read-object-nested ()
  (let* ((obj (json-read-from-string "{\"x\":{\"y\":42}}"))
         (inner (alist-get 'x obj)))
    (should (= (alist-get 'y inner) 42))))

;;;; whitespace tolerance

(ert-deftest json-read-skips-whitespace ()
  (should (= (json-read-from-string "   42   ") 42))
  (should (equal (json-read-from-string " [ 1 , 2 ] ") '(1 2))))

(ert-deftest json-read-handles-newlines ()
  (should (= (alist-get 'a (json-read-from-string "{\n  \"a\": 1\n}")) 1)))

;;;; round-trip with encoder

(ert-deftest json-roundtrip-mcp-frame ()
  ;; The frame shape `anvil-server-process-jsonrpc' actually parses.
  (let* ((src "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"initialize\",\"params\":{\"protocolVersion\":\"2024-11-05\"}}")
         (obj (json-read-from-string src)))
    (should (string= (alist-get 'jsonrpc obj) "2.0"))
    (should (= (alist-get 'id obj) 1))
    (should (string= (alist-get 'method obj) "initialize"))
    (let ((params (alist-get 'params obj)))
      (should (string= (alist-get 'protocolVersion params) "2024-11-05")))))

;;;; error handling

(ert-deftest json-read-malformed-trailing-garbage ()
  (should-error (json-read-from-string "42 trailing") :type 'json-error))

(ert-deftest json-read-unterminated-string ()
  (should-error (json-read-from-string "\"unterminated") :type 'json-error))

(ert-deftest json-read-unexpected-eof ()
  (should-error (json-read-from-string "[1,") :type 'json-error))

(ert-deftest json-read-missing-comma-in-object ()
  (should-error (json-read-from-string "{\"a\":1 \"b\":2}") :type 'json-error))

(ert-deftest json-encode-empty-hash-table-emits-curly ()
  "Phase B6 (2026-05-10): empty hash-table → `{}'.
This is the canonical encoding for `(make-hash-table)' values that
anvil-server uses to mean an empty MCP capabilities sub-object."
  (should (equal (json-encode (make-hash-table)) "{}")))

(ert-deftest json-encode-populated-hash-table-emits-pairs ()
  "Phase B6 (2026-05-10): non-empty hash-table → JSON object."
  (let ((h (make-hash-table :test 'equal)))
    (puthash "a" 1 h)
    (let ((s (json-encode h)))
      (should (string-match-p "\\`{" s))
      (should (string-match-p "\"a\":1" s))
      (should (string-match-p "}\\'" s)))))

(ert-deftest json-encode-hash-table-inside-alist ()
  "Phase B6 (2026-05-10): nested empty hash inside an alist value.
Mirrors the exact shape `anvil-server--handle-initialize' produces
when the server has at least one tool registered."
  (let* ((h (make-hash-table))
         (s (json-encode `((capabilities . ((tools . ,h)))))))
    (should (equal s "{\"capabilities\":{\"tools\":{}}}"))))

(provide 'json-test)
;;; json-test.el ends here
