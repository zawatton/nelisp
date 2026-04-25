;;; nelisp-json-test.el --- ERT tests for nelisp-json -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Coverage for `src/nelisp-json.el', the pure Lisp JSON layer that
;; unblocks NeLisp standalone JSON-RPC without host Emacs `json.el'.
;; Tests focus on the shipped public API, sentinel semantics, empty
;; object handling, unicode escape decoding, and JSON-RPC round-trips.

;;; Code:

(require 'ert)
(require 'nelisp-json)

(defmacro nelisp-json-test--should-parse-error (json)
  "Assert that parsing JSON signals `nelisp-json-parse-error'."
  `(should-error (nelisp-json-parse-string ,json)
                 :type 'nelisp-json-parse-error))

(defun nelisp-json-test--hash (&rest pairs)
  "Build an `equal' hash table from alternating string/value PAIRS."
  (let ((table (make-hash-table :test 'equal)))
    (while pairs
      (puthash (car pairs) (cadr pairs) table)
      (setq pairs (cddr pairs)))
    table))

(defun nelisp-json-test--sorted-hash-keys (table)
  "Return sorted string keys from hash TABLE."
  (let (keys)
    (maphash (lambda (key _value) (push key keys)) table)
    (sort keys #'string<)))

;;; Encode -----------------------------------------------------------

(ert-deftest nelisp-json-encode-primitives ()
  (should (equal (nelisp-json-encode nil) "null"))
  (should (equal (nelisp-json-encode t) "true"))
  (should (equal (nelisp-json-encode :json-false) "false"))
  (should (equal (nelisp-json-encode :false) "false"))
  (should (equal (nelisp-json-encode 42) "42"))
  (should (equal (nelisp-json-encode 3.14) "3.14"))
  (should (equal (nelisp-json-encode "hello") "\"hello\"")))

(ert-deftest nelisp-json-encode-string-escape ()
  (should (equal (nelisp-json-encode "a\"b") "\"a\\\"b\""))
  (should (equal (nelisp-json-encode "a\\b") "\"a\\\\b\""))
  (should (equal (nelisp-json-encode "a\nb") "\"a\\nb\""))
  (should (equal (nelisp-json-encode "\b\f\r\t")
                 "\"\\b\\f\\r\\t\"")))

(ert-deftest nelisp-json-encode-string-control-char-as-unicode ()
  (should (equal (nelisp-json-encode-string (string 1 31))
                 "\"\\u0001\\u001F\"")))

(ert-deftest nelisp-json-encode-string-preserves-multibyte ()
  (should (equal (nelisp-json-encode "日本語") "\"日本語\""))
  (should (equal (nelisp-json-encode "😀") "\"😀\"")))

(ert-deftest nelisp-json-encode-vector ()
  (should (equal (nelisp-json-encode [1 2 3]) "[1,2,3]"))
  (should (equal (nelisp-json-encode [1 [2 3] "x"])
                 "[1,[2,3],\"x\"]")))

(ert-deftest nelisp-json-encode-list-as-array ()
  (should (equal (nelisp-json-encode '(1 2 3)) "[1,2,3]"))
  (should (equal (nelisp-json-encode '("a" nil :false))
                 "[\"a\",null,false]")))

(ert-deftest nelisp-json-encode-empty-hash ()
  (should (equal (nelisp-json-encode (make-hash-table :test 'equal)) "{}")))

(ert-deftest nelisp-json-encode-hash-with-entries ()
  (let ((h (make-hash-table :test 'equal)))
    (puthash "key" "value" h)
    (should (equal (nelisp-json-encode h) "{\"key\":\"value\"}"))))

(ert-deftest nelisp-json-encode-alist-object ()
  (should (equal (nelisp-json-encode '(("k" . 42) ("ok" . t)))
                 "{\"k\":42,\"ok\":true}")))

(ert-deftest nelisp-json-encode-plist-object ()
  (should (equal (nelisp-json-encode '(:k 42 :ok t))
                 "{\"k\":42,\"ok\":true}")))

(ert-deftest nelisp-json-encode-plist-nil-value-still-object ()
  (should (equal (nelisp-json-encode '(:items nil))
                 "{\"items\":null}")))

(ert-deftest nelisp-json-encode-nil-is-null-not-empty-object ()
  (should (equal (nelisp-json-encode nil) "null"))
  (should-not (equal (nelisp-json-encode nil) "{}")))

(ert-deftest nelisp-json-encode-nested-jsonrpc-shape ()
  (let* ((params (nelisp-json-test--hash "path" "/tmp/a.txt"))
         (msg (list :jsonrpc "2.0"
                    :id 7
                    :method "tools/call"
                    :params params))
         (encoded (nelisp-json-encode msg)))
    (should (string-match-p "\"jsonrpc\":\"2.0\"" encoded))
    (should (string-match-p "\"method\":\"tools/call\"" encoded))
    (should (string-match-p "\"path\":\"/tmp/a.txt\"" encoded))))

(ert-deftest nelisp-json-encode-rejects-invalid-key ()
  (should-error (nelisp-json-encode '((1 . "bad")))
                :type 'nelisp-json-encode-error))

(ert-deftest nelisp-json-encode-rejects-invalid-type ()
  (should-error (nelisp-json-encode #'identity)
                :type 'nelisp-json-encode-error))

;;; Parse ------------------------------------------------------------

(ert-deftest nelisp-json-parse-primitives ()
  (should (eq (nelisp-json-parse-string "null") :null))
  (should (eq (nelisp-json-parse-string "true") t))
  (should (eq (nelisp-json-parse-string "false") :false))
  (should (= (nelisp-json-parse-string "42") 42))
  (should (= (nelisp-json-parse-string "3.14") 3.14))
  (should (equal (nelisp-json-parse-string "\"hello\"") "hello")))

(ert-deftest nelisp-json-parse-string-escape ()
  (should (equal (nelisp-json-parse-string "\"a\\\"b\"") "a\"b"))
  (should (equal (nelisp-json-parse-string "\"a\\\\b\"") "a\\b"))
  (should (equal (nelisp-json-parse-string "\"a\\nb\"") "a\nb"))
  (should (equal (nelisp-json-parse-string "\"\\/\"") "/")))

(ert-deftest nelisp-json-parse-unicode-escape-basic ()
  (should (equal (nelisp-json-parse-string "\"\\u65E5\\u672C\\u8A9E\"")
                 "日本語")))

(ert-deftest nelisp-json-parse-unicode-surrogate-pair ()
  (should (equal (nelisp-json-parse-string "\"\\uD83D\\uDE00\"")
                 "😀")))

(ert-deftest nelisp-json-parse-number-forms ()
  (should (= (nelisp-json-parse-string "-7") -7))
  (should (= (nelisp-json-parse-string "6.02e23") 6.02e23))
  (should (= (nelisp-json-parse-string "-1.5E-2") -0.015)))

(ert-deftest nelisp-json-parse-array ()
  (should (equal (nelisp-json-parse-string "[1,2,3]") [1 2 3])))

(ert-deftest nelisp-json-parse-array-with-list-option ()
  (should (equal (nelisp-json-parse-string "[1,2,3]" :array-type 'list)
                 '(1 2 3))))

(ert-deftest nelisp-json-parse-empty-object ()
  (let ((h (nelisp-json-parse-string "{}")))
    (should (hash-table-p h))
    (should (= (hash-table-count h) 0))))

(ert-deftest nelisp-json-parse-object ()
  (let ((h (nelisp-json-parse-string "{\"k\":42}")))
    (should (hash-table-p h))
    (should (= (gethash "k" h) 42))))

(ert-deftest nelisp-json-parse-object-as-alist ()
  (should (equal (nelisp-json-parse-string "{\"k\":42,\"ok\":true}"
                                           :object-type 'alist)
                 '(("k" . 42) ("ok" . t)))))

(ert-deftest nelisp-json-parse-object-as-plist ()
  (should (equal (nelisp-json-parse-string "{\"k\":42,\"ok\":true}"
                                           :object-type 'plist)
                 '(:k 42 :ok t))))

(ert-deftest nelisp-json-parse-object-custom-sentinels ()
  (let ((v (nelisp-json-parse-string "{\"n\":null,\"f\":false}"
                                     :null-object nil
                                     :false-object :json-false)))
    (should (eq (gethash "n" v) nil))
    (should (eq (gethash "f" v) :json-false))))

(ert-deftest nelisp-json-parse-nested ()
  (let ((v (nelisp-json-parse-string "{\"a\":[1,{\"b\":true}]}")))
    (should (vectorp (gethash "a" v)))
    (should (eq (gethash "b" (aref (gethash "a" v) 1)) t))))

(ert-deftest nelisp-json-parse-whitespace-around-value ()
  (let ((h (nelisp-json-parse-string " \n\t {\"k\" : [1, 2]} \r ")))
    (should (equal (gethash "k" h) [1 2]))))

(ert-deftest nelisp-json-read-from-string-is-alias ()
  (should (equal (nelisp-json-read-from-string "[1,true,false]")
                 [1 t :false])))

(ert-deftest nelisp-json-serialize-is-alias ()
  (should (equal (nelisp-json-serialize '(:a 1 :b 2))
                 "{\"a\":1,\"b\":2}")))

;;; Pretty print -----------------------------------------------------

(ert-deftest nelisp-json-pretty-print-string-object ()
  (should
   (equal
    (nelisp-json-pretty-print-string "{\"a\":[1,true],\"b\":{\"c\":null}}")
    "{\n  \"a\": [\n    1,\n    true\n  ],\n  \"b\": {\n    \"c\": null\n  }\n}")))

(ert-deftest nelisp-json-pretty-print-string-array ()
  (should
   (equal
    (nelisp-json-pretty-print-string "[{\"a\":1},2]")
    "[\n  {\n    \"a\": 1\n  },\n  2\n]")))

(ert-deftest nelisp-json-pretty-print-preserves-empty-containers ()
  (should (equal (nelisp-json-pretty-print-string "{}") "{}"))
  (should (equal (nelisp-json-pretty-print-string "[]") "[]")))

;;; Errors -----------------------------------------------------------

(ert-deftest nelisp-json-parse-errors-invalid-literal ()
  (nelisp-json-test--should-parse-error "tru"))

(ert-deftest nelisp-json-parse-errors-leading-zero ()
  (nelisp-json-test--should-parse-error "01"))

(ert-deftest nelisp-json-parse-errors-bad-fraction ()
  (nelisp-json-test--should-parse-error "1."))

(ert-deftest nelisp-json-parse-errors-bad-exponent ()
  (nelisp-json-test--should-parse-error "1e"))

(ert-deftest nelisp-json-parse-errors-trailing-data ()
  (nelisp-json-test--should-parse-error "true false"))

(ert-deftest nelisp-json-parse-errors-unterminated-string ()
  (nelisp-json-test--should-parse-error "\"abc"))

(ert-deftest nelisp-json-parse-errors-invalid-escape ()
  (nelisp-json-test--should-parse-error "\"\\x\""))

(ert-deftest nelisp-json-parse-errors-unexpected-low-surrogate ()
  (nelisp-json-test--should-parse-error "\"\\uDE00\""))

(ert-deftest nelisp-json-parse-errors-missing-low-surrogate ()
  (nelisp-json-test--should-parse-error "\"\\uD83Dabc\""))

(ert-deftest nelisp-json-parse-errors-array-trailing-comma ()
  (nelisp-json-test--should-parse-error "[1,]"))

(ert-deftest nelisp-json-parse-errors-object-trailing-comma ()
  (nelisp-json-test--should-parse-error "{\"a\":1,}"))

(ert-deftest nelisp-json-parse-errors-object-key-must-be-string ()
  (nelisp-json-test--should-parse-error "{a:1}"))

;;; Round-trip -------------------------------------------------------

(ert-deftest nelisp-json-roundtrip-mcp-jsonrpc ()
  (let* ((msg "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"tools/list\"}")
         (parsed (nelisp-json-parse-string msg))
         (encoded (nelisp-json-encode parsed))
         (reparsed (nelisp-json-parse-string encoded)))
    (should (equal (gethash "jsonrpc" parsed) "2.0"))
    (should (equal (gethash "method" parsed) "tools/list"))
    (should (= (gethash "id" parsed) 1))
    (should (equal (gethash "jsonrpc" reparsed) "2.0"))
    (should (equal (gethash "method" reparsed) "tools/list"))
    (should (= (gethash "id" reparsed) 1))))

(ert-deftest nelisp-json-roundtrip-false-null-and-empty-object ()
  (let* ((value (list :ok t
                      :changed :false
                      :meta (make-hash-table :test 'equal)
                      :payload nil))
         (round (nelisp-json-parse-string (nelisp-json-encode value))))
    (should (eq (gethash "changed" round) :false))
    (should (eq (gethash "payload" round) :null))
    (let ((meta (gethash "meta" round)))
      (should (hash-table-p meta))
      (should (= (hash-table-count meta) 0)))))

(ert-deftest nelisp-json-roundtrip-hash-retains-keys ()
  (let* ((table (nelisp-json-test--hash "b" 2 "a" 1))
         (parsed (nelisp-json-parse-string (nelisp-json-encode table))))
    (should (equal (nelisp-json-test--sorted-hash-keys parsed)
                   '("a" "b")))
    (should (= (gethash "a" parsed) 1))
    (should (= (gethash "b" parsed) 2))))

(provide 'nelisp-json-test)

;;; nelisp-json-test.el ends here
