;;; nelisp-deftool-test.el --- Tests for nelisp-deftool macro -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Phase 5-E §3.2 ERT coverage for `nelisp-deftool' macro and
;; associated installer.  Server spine lifecycle / dispatcher /
;; JSON wire are exercised in `nelisp-server-test.el'; these tests
;; focus on the declarative tool-registration surface.

;;; Code:

(require 'ert)
(require 'json)
(require 'nelisp-server)

;;; Helpers ----------------------------------------------------------

(defmacro nelisp-deftool-test--with-fresh-registry (&rest body)
  "Clear the tool registry around BODY so tests do not share state."
  (declare (indent 0))
  `(unwind-protect
       (progn
         (clrhash nelisp-server--tool-registry)
         ,@body)
     (clrhash nelisp-server--tool-registry)))

;;; Name normalization ----------------------------------------------

(ert-deftest nelisp-deftool-test-normalize-name-symbol ()
  (should (string= "file-read"
                   (nelisp-server--normalize-tool-name 'file-read))))

(ert-deftest nelisp-deftool-test-normalize-name-string ()
  (should (string= "git.log"
                   (nelisp-server--normalize-tool-name "git.log"))))

(ert-deftest nelisp-deftool-test-normalize-name-rejects-bogus ()
  (should-error
   (nelisp-server--normalize-tool-name 42)))

;;; Macro expansion / registration ---------------------------------

(ert-deftest nelisp-deftool-test-registers-symbol-name ()
  (nelisp-deftool-test--with-fresh-registry
    (nelisp-deftool alpha
      :description "first"
      :handler (lambda (_args) (list :ok 1)))
    (let ((spec (gethash "alpha" nelisp-server--tool-registry)))
      (should spec)
      (should (string= "first" (plist-get spec :description)))
      (should (functionp (plist-get spec :handler))))))

(ert-deftest nelisp-deftool-test-registers-string-name ()
  (nelisp-deftool-test--with-fresh-registry
    (nelisp-deftool "anvil.echo"
      :description "string-named"
      :handler (lambda (args) args))
    (should (gethash "anvil.echo" nelisp-server--tool-registry))))

(ert-deftest nelisp-deftool-test-missing-handler-errors-at-expand ()
  "Macro-expand-time error when :handler is omitted."
  (should-error
   (macroexpand-1
    '(nelisp-deftool noop :description "missing handler"))))

(ert-deftest nelisp-deftool-test-duplicate-errors-at-install ()
  "Re-registering the same name signals an error without silently
overwriting the previous spec."
  (nelisp-deftool-test--with-fresh-registry
    (nelisp-deftool reg-dup
      :description "first install"
      :handler (lambda (_args) 1))
    (should-error
     (nelisp-deftool reg-dup
       :description "second install"
       :handler (lambda (_args) 2))
     :type 'error)
    ;; Registry still holds the first spec.
    (let ((spec (gethash "reg-dup" nelisp-server--tool-registry)))
      (should (string= "first install" (plist-get spec :description))))))

(ert-deftest nelisp-deftool-test-input-schema-pass-through ()
  "`:input-schema' is stored verbatim; tools/list will plist->alist
it at encode time."
  (nelisp-deftool-test--with-fresh-registry
    (nelisp-deftool sch-test
      :description "schema test"
      :input-schema (list :type "object"
                          :properties (list :path (list :type "string"))
                          :required ["path"])
      :handler (lambda (_args) nil))
    (let* ((spec (gethash "sch-test" nelisp-server--tool-registry))
           (schema (plist-get spec :input-schema)))
      (should (string= "object" (plist-get schema :type)))
      (should (equal ["path"] (plist-get schema :required))))))

;;; End-to-end through server dispatcher ---------------------------

(ert-deftest nelisp-deftool-test-visible-via-tools-list ()
  "Two `nelisp-deftool' forms appear in dispatched `tools/list'."
  (nelisp-server-start)
  (unwind-protect
      (nelisp-deftool-test--with-fresh-registry
        (nelisp-deftool tool-a
          :description "tool alpha"
          :handler (lambda (_args) (list :who "a")))
        (nelisp-deftool tool-b
          :description "tool beta"
          :handler (lambda (_args) (list :who "b")))
        (let* ((result (nelisp-server-dispatch "tools/list" nil))
               (names (sort (mapcar (lambda (t) (plist-get t :name))
                                    (plist-get result :tools))
                            #'string<)))
          (should (equal '("tool-a" "tool-b") names))))
    (nelisp-server-stop)))

(ert-deftest nelisp-deftool-test-callable-via-tools-call ()
  "`tools/call' reaches the deftool handler; arguments alist is
passed through; return value wrapped in MCP :content envelope."
  (nelisp-server-start)
  (unwind-protect
      (nelisp-deftool-test--with-fresh-registry
        (nelisp-deftool add-pair
          :description "adds two keys"
          :handler (lambda (args)
                     (list :sum (+ (alist-get 'a args)
                                   (alist-get 'b args)))))
        (let* ((result (nelisp-server-dispatch
                        "tools/call"
                        '((name . "add-pair")
                          (arguments . ((a . 7) (b . 5))))))
               (content (plist-get result :content))
               (text (plist-get (car content) :text)))
          (should (string-match-p ":sum 12" text))))
    (nelisp-server-stop)))

(ert-deftest nelisp-deftool-test-handler-error-wraps-32603 ()
  "A handler that raises is caught by the dispatcher and surfaced
as a -32603 JSON-RPC error."
  (nelisp-server-start)
  (unwind-protect
      (nelisp-deftool-test--with-fresh-registry
        (nelisp-deftool crashy
          :description "always errors"
          :handler (lambda (_args) (error "intentional boom")))
        (let* ((result (nelisp-server-dispatch
                        "tools/call"
                        '((name . "crashy")
                          (arguments . nil))))
               (err (plist-get result :error)))
          (should err)
          (should (= -32603 (plist-get err :code)))))
    (nelisp-server-stop)))

(ert-deftest nelisp-deftool-test-json-round-trip ()
  "Full JSON-RPC round trip: tools/call hits a deftool handler and
the response serialises back to valid JSON."
  (nelisp-server-start)
  (unwind-protect
      (nelisp-deftool-test--with-fresh-registry
        (nelisp-deftool json-rt
          :description "for json round trip"
          :handler (lambda (_args) (list :hello "world")))
        (let* ((req (concat
                     "{\"jsonrpc\":\"2.0\",\"id\":42,"
                     "\"method\":\"tools/call\","
                     "\"params\":{\"name\":\"json-rt\","
                     "\"arguments\":{}}}"))
               (resp (nelisp-server-call-json req))
               (parsed (json-read-from-string resp)))
          (should (= 42 (alist-get 'id parsed)))
          (should (alist-get 'result parsed))))
    (nelisp-server-stop)))

(provide 'nelisp-deftool-test)

;;; nelisp-deftool-test.el ends here
