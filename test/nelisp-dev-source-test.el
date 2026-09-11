;;; nelisp-dev-source-test.el --- nonexecuting source contracts -*- lexical-binding: t; -*-
(require 'ert)
(require 'nelisp-dev-source)

(defun nelisp-dev-source-test--run (text op &optional symbol)
  (let ((root (make-temp-file "nelisp-source-test-" t)))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "fixture.el" root)
            (let ((coding-system-for-write 'utf-8-unix)) (insert text)))
          (nelisp-dev-source-dispatch
           (list (cons "operation" op) (cons "request_id" "test")
                 (cons "arguments" (list (cons "path" "fixture.el") (cons "symbol" symbol))))
           (list :root root :target "host-emacs")))
      (delete-directory root t))))

(ert-deftest nelisp-dev-source-reader-distinguishes-boundary-eof ()
  (dolist (text '("(defun bad (x)" "(defun bad (x) (list x)" "\"" "(a . )" ")"))
    (let ((result (nelisp-dev-source-test--run text "check")))
      (should (equal "failed" (cdr (assoc "status" result))))
      (should (equal "NELISP-CHECK-SYNTAX"
                     (cdr (assoc "code" (aref (cdr (assoc "diagnostics" result)) 0)))))))
  (dolist (text '("" "; comment (\n" "42 ; trailing (\n"
                  "(defun f () \"(\") ; )\n" "(list ?\\()"))
    (should (equal "inconclusive"
                   (cdr (assoc "status" (nelisp-dev-source-test--run text "check")))))))

(ert-deftest nelisp-dev-source-exact-unicode-span-and-raw-hash ()
  (let* ((text "\"界\" (defun  α (x) x)\n(defun other () nil)\n")
         (result (nelisp-dev-source-test--run text "describe" "α"))
         (definition (aref (cdr (assoc "definitions" (cdr (assoc "data" result)))) 0))
         (start (cdr (assoc "start" definition))) (end (cdr (assoc "end" definition))))
    (should (equal "ok" (cdr (assoc "status" result))))
    (should (= 1 (cdr (assoc "line" start))))
    (should (= 5 (cdr (assoc "column" start))))
    (should (= 6 (cdr (assoc "byte_offset" start))))
    (should (= 1 (cdr (assoc "line" end))))
    (should (= 21 (cdr (assoc "column" end))))
    (should (equal (concat "sha256:" (secure-hash 'sha256 (encode-coding-string text 'utf-8-unix)))
                   (cdr (assoc "source_content_hash" (cdr (assoc "identity" result))))))))

(ert-deftest nelisp-dev-source-duplicates-are-not-a-loaded-definition ()
  (let* ((result (nelisp-dev-source-test--run "(defun f () 1)\n(defun f () 2)" "describe" "f"))
         (definitions (cdr (assoc "definitions" (cdr (assoc "data" result))))))
    (should (equal "inconclusive" (cdr (assoc "status" result))))
    (should (= 2 (length definitions)))
    (should (= 2 (cdr (assoc "line" (cdr (assoc "start" (aref definitions 1)))))))))

(ert-deftest nelisp-dev-source-never-executes-application-or-macro ()
  (let ((symbol (intern (make-temp-name "nelisp-source-test-effect-"))))
    (set symbol 0)
    (dolist (op '("check" "describe" "impact"))
      (let* ((text (format "(setq %s 9)\n(defmacro evil () (error \"executed\"))\n(evil)\n(defun f () (funcall callback))"
                           (symbol-name symbol)))
             (result (nelisp-dev-source-test--run text op "f")))
        (should-not (equal "failed" (cdr (assoc "status" result))))))
    (unwind-protect (should (= 0 (symbol-value symbol)))
      (unintern symbol obarray))))

(ert-deftest nelisp-dev-source-impact-ignores-quoted-and-variable-references ()
  (let* ((text "(defun quoted () '(callee x))\n(defun variable (callee) (list callee))\n(defun caller (x) (callee x))")
         (result (nelisp-dev-source-test--run text "impact" "callee"))
         (edges (cdr (assoc "callers" (cdr (assoc "data" result))))))
    (should (equal "inconclusive" (cdr (assoc "status" result))))
    (should (= 1 (length edges)))
    (should (equal "caller" (cdr (assoc "caller" (aref edges 0)))))
    (should (equal "syntactic-call-candidate" (cdr (assoc "kind" (aref edges 0)))))))

(ert-deftest nelisp-dev-source-stable-diagnostic-id-and-input-limits ()
  (let* ((a (nelisp-dev-source-test--run "(defun broken (" "check"))
         (b (nelisp-dev-source-test--run "(defun broken (" "check")))
    (should (equal (cdr (assoc "id" (aref (cdr (assoc "diagnostics" a)) 0)))
                   (cdr (assoc "id" (aref (cdr (assoc "diagnostics" b)) 0))))))
  (let ((root (make-temp-file "nelisp-source-limit-" t)))
    (unwind-protect
        (dolist (path '(nil "../outside.el" "missing.el"))
          (let ((result (nelisp-dev-source-dispatch
                         (list (cons "operation" "check") (cons "request_id" "test")
                               (cons "arguments" (list (cons "path" path))))
                         (list :root root))))
            (should (equal "failed" (cdr (assoc "status" result))))))
      (delete-directory root t))))
