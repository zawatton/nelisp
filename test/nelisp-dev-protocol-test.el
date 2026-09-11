;;; nelisp-dev-protocol-test.el --- P1 protocol/adapter contracts -*- lexical-binding: t; -*-
(require 'ert)
(require 'nelisp-dev)

(defconst nelisp-dev-protocol-test--root
  (file-name-directory
   (directory-file-name
    (file-name-directory (or load-file-name buffer-file-name)))))

(ert-deftest nelisp-dev-protocol-cli-subprocess-contracts ()
  "Run the JSON CLI negative cases through the existing full ERT gate."
  (let ((python (executable-find "python3")))
    (unless (and python (not (eq system-type 'windows-nt)))
      (ert-skip "DEV CLI contract requires POSIX execution and Python 3"))
    (with-temp-buffer
      (let ((code (call-process python nil t nil
                                (expand-file-name "test/nelisp-dev-cli-test.py"
                                                  nelisp-dev-protocol-test--root))))
        (should (equal code 0))
        (should (string-match-p "Ran [1-9][0-9]* tests" (buffer-string)))))))

(defun nelisp-dev-protocol-test--request (op &optional args session)
  (list (cons "schema_version" "1") (cons "operation" op)
        (cons "request_id" "ert-request") (cons "arguments" (or args nil))
        (cons "target" "host-emacs") (cons "session_id" (or session :null))
        (cons "limits" nil)))

(ert-deftest nelisp-dev-protocol-invalid-types-and-json ()
  (should (equal "failed"
                 (cdr (assoc "status"
                             (nelisp-dev-dispatch 7 (list :target "host-emacs"))))))
  (should (string-match-p "schema_version"
                          (nelisp-dev-protocol-json
                           (nelisp-dev-dispatch 7 (list :target "host-emacs"))))))

(ert-deftest nelisp-dev-protocol-live-session-refuses-cli ()
  (let ((result (nelisp-dev-dispatch
                 (nelisp-dev-protocol-test--request "test" nil "session-x")
                 (list :target "host-emacs" :session-id nil))))
    (should (equal "unsupported" (cdr (assoc "status" result))))
    (should (equal "repl" (cdr (assoc "required_interface" (cdr (assoc "data" result))))))))

(ert-deftest nelisp-dev-protocol-adapter-hook-and-missing-gate ()
  (let ((context (list :root default-directory :target "host-emacs"
                       :adapters (list (cons "custom"
                                             (lambda (_request _context)
                                               (nelisp-dev-protocol-envelope
                                                "custom" "hook" "ok" nil nil [] nil [])))))))
    (should (equal "ok" (cdr (assoc "status"
                                    (nelisp-dev-dispatch
                                     (nelisp-dev-protocol-test--request "custom") context))))))
  (let ((result (nelisp-dev-dispatch
                 (nelisp-dev-protocol-test--request "test" '(("gate" . "missing")))
                 (list :root default-directory :target "host-emacs"))))
    (should (equal "inconclusive" (cdr (assoc "status" result))))))

(ert-deftest nelisp-dev-protocol-detail-bounded-ttl-and-clear ()
  (nelisp-dev-protocol-detail-clear)
  (should (nelisp-dev-protocol-detail-put "one" '(a b)))
  (should (equal '(a b) (nelisp-dev-protocol-detail-get "one")))
  (should (nelisp-dev-protocol-detail-put "old" 'expired))
  (setcar (gethash "old" nelisp-dev-protocol--details) (- (float-time) 901))
  (should-not (nelisp-dev-protocol-detail-get "old"))
  (should (= 1 (nelisp-dev-protocol-detail-clear)))
  (should-not (nelisp-dev-protocol-detail-get "one")))

(ert-deftest nelisp-dev-protocol-rejects-malformed-contracts ()
  (dolist (value '(nil 1 [1] (a) (("operation" . "capabilities"))
                  (("operation" . "capabilities") . broken)))
    (let ((result (nelisp-dev-dispatch value nil)))
      (should (equal "failed" (cdr (assoc "status" result))))
      (should (stringp (nelisp-dev-protocol-json result)))))
  (dolist (pair '(("target" . 7) ("session_id" . [])
                  ("arguments" . 1) ("limits" . [1])
                  ("limits" . (("page_size" . 0)))
                  ("limits" . (("bytes" . 100000)))
                  ("operation" . "") ("request_id" . 7)))
    (let ((request (nelisp-dev-protocol-test--request "capabilities")))
      (setf (alist-get (car pair) request nil nil #'equal) (cdr pair))
      (should (equal "failed" (cdr (assoc "status" (nelisp-dev-dispatch request nil))))))))

(ert-deftest nelisp-dev-protocol-json-preserves-null-false-and-array ()
  (let* ((json-null :null) (json-false :false) (json-array-type 'vector)
         (encoded (nelisp-dev-protocol-json '(("null" . :null) ("false" . :false)
                                              ("empty" . []) ("object"))))
         (value (json-read-from-string encoded)))
    (should (eq :null (alist-get 'null value)))
    (should (eq :false (alist-get 'false value)))
    (should (null (alist-get 'object value)))
    (should (equal [] (alist-get 'empty value)))))

(ert-deftest nelisp-dev-protocol-detail-overwrite-expire-and-capacity ()
  (let ((nelisp-dev-protocol--details (make-hash-table :test #'equal))
        (nelisp-dev-protocol--detail-bytes 0))
    (dotimes (i 64) (should (nelisp-dev-protocol-detail-put (number-to-string i) "x")))
    (should-not (nelisp-dev-protocol-detail-put "overflow" "x"))
    (let ((bytes nelisp-dev-protocol--detail-bytes))
      (should (nelisp-dev-protocol-detail-put "0" "x"))
      (should (= bytes nelisp-dev-protocol--detail-bytes)))
    (setcar (gethash "0" nelisp-dev-protocol--details) (- (float-time) 901))
    (should (nelisp-dev-protocol-detail-put "new" "x"))
    (should (= 64 (hash-table-count nelisp-dev-protocol--details)))
    (should-not (nelisp-dev-protocol-detail-put "new" (make-string 1048577 ?x)))
    (nelisp-dev-protocol-detail-clear)
    (should (= 0 nelisp-dev-protocol--detail-bytes))))

(ert-deftest nelisp-dev-protocol-paging-budget-and-stale-cursor ()
  (let* ((rows (vconcat (mapcar (lambda (i) (list (cons "code" (number-to-string i))
                                                 (cons "message" (make-string 30 ?界))))
                               (number-sequence 1 70))))
         (result (nelisp-dev-protocol-envelope "check" "test" "failed" nil
                                               '(("errors" . 70)) rows nil []))
         (request (nelisp-dev-protocol-test--request "check"))
         (limits '(("bytes" . 2048) ("page_size" . 10)))
         (context '(:session-id "one")))
    (setq rows (cdr (assoc "diagnostics" result)))
    (setf (alist-get "limits" request nil nil #'equal) limits)
    (let* ((page (nelisp-dev-protocol-page result request context))
           (cursor (cdr (assoc "next_cursor" page)))
           (count (length (cdr (assoc "diagnostics" page)))))
      (should (< 0 count 11))
      (should (<= (string-bytes (nelisp-dev-protocol-json page)) 2048))
      (should (= 70 (cdr (assoc "errors" (cdr (assoc "summary" page))))))
      (setf (alist-get "cursor" limits nil nil #'equal) cursor)
      (setf (alist-get "limits" request nil nil #'equal) limits)
      (let ((next (nelisp-dev-protocol-page result request context)))
        (should (equal (aref rows count) (aref (cdr (assoc "diagnostics" next)) 0))))
      (should-error (nelisp-dev-protocol-page result request '(:session-id "other")))
      (let ((changed (copy-tree result t)))
        (setf (alist-get "identity" changed nil nil #'equal) '(("source_hash" . "changed")))
        (should-error (nelisp-dev-protocol-page changed request context)))
      (let ((nelisp-dev-protocol--epoch (1+ nelisp-dev-protocol--epoch)))
        (should-error (nelisp-dev-protocol-page result request context))))))

(ert-deftest nelisp-dev-protocol-oversized-response-is-bounded-inconclusive ()
  (let* ((result (nelisp-dev-protocol-envelope "describe" "id" "ok" nil nil []
                                              (list (cons "text" (make-string 50000 ?a))) []))
         (page (nelisp-dev-protocol-page result (nelisp-dev-protocol-test--request "describe") nil)))
    (should (equal "inconclusive" (cdr (assoc "status" page))))
    (should (<= (string-bytes (nelisp-dev-protocol-json page)) 16384))))

(ert-deftest nelisp-dev-protocol-legacy-reports-never-invent-a-fresh-run ()
  (let ((root (make-temp-file "nelisp-dev-gates-" t)))
    (unwind-protect
        (progn
          (make-directory (expand-file-name "target/gates" root) t)
          (dolist (case '(("pass" 2 0 0 "" "inconclusive")
                          ("fail" 2 1 0 "" "failed")
                          ("pass" 0 0 0 "" "inconclusive")
                          ("pass" 2 0 1 "" "inconclusive")
                          ("pass" 2 0 0 "timeout" "inconclusive")
                          ("pass" 2 0 0 "stale" "inconclusive")))
            (with-temp-file (expand-file-name "target/gates/fixture.json" root)
              (insert (json-encode `((schema . "nelisp-gate/1") (name . "fixture")
                                    (status . ,(nth 0 case)) (ran . ,(nth 1 case))
                                    (passed . ,(nth 1 case)) (failed . ,(nth 2 case))
                                    (skipped . ,(nth 3 case)) (reason . ,(nth 4 case))))))
            (let ((result (nelisp-dev-dispatch
                           (nelisp-dev-protocol-test--request "test" '(("gate" . "fixture")))
                           (list :root root))))
              (should (equal (nth 5 case) (cdr (assoc "status" result))))
              (should (eq :false (cdr (assoc "executed" (cdr (assoc "data" result)))))))))
      (delete-directory root t))))
