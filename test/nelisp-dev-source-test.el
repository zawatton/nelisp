;;; nelisp-dev-source-test.el --- nonexecuting source contracts -*- lexical-binding: t; -*-
(require 'ert)
(require 'nelisp-dev-source)

(ert-deftest nelisp-dev-source-arity-scope-and-stable-occurrences ()
  (let* ((text "(defun f (x) x)\n(defun caller () (f) (f) 42)\n")
         (first (nelisp-dev-source-test--run text "check"))
         (second (nelisp-dev-source-test--run text "check"))
         (rows (cdr (assoc "diagnostics" first))))
    (should (= 2 (length rows)))
    (should (equal rows (cdr (assoc "diagnostics" second))))
    (should-not (equal (cdr (assoc "id" (aref rows 0)))
                       (cdr (assoc "id" (aref rows 1)))))
    (should (equal "definition" (cdr (assoc "precision" (aref rows 0)))))
    (should (= 0 (length (cdr (assoc "diagnostics"
      (nelisp-dev-source-test--run text "describe" "f"))))))))

(ert-deftest nelisp-dev-source-arity-rejects-unsupported-lambda-lists ()
  (dolist (args '((x &rest rest tail) (x &rest nil) (x &optional &optional y)
                  (&rest r &optional x) (&unknown x) (nil) (t) (x x)
                  ((x)) (x . rest)))
    (should-not (nelisp-dev-source--plain-arity args)))
  (dolist (text '("(defun f (x) x) (defmacro f () nil) (defun caller () (f))"
                  "(defun f (x) x) (cl-defun f () nil) (defun caller () (f))"
                  "(defun f (x) x) (defmacro when (&rest x) nil) (defun caller () (when (f)))"))
    (should (= 0 (length (cdr (assoc "diagnostics"
                                   (nelisp-dev-source-test--run text "check"))))))))

(defun nelisp-dev-source-test--run (text op &optional symbol)
  (let ((root (make-temp-file "nelisp-source-test-" t)))
    (unwind-protect
        (progn
          (let ((coding-system-for-write 'utf-8-unix))
            (with-temp-file (expand-file-name "fixture.el" root)
              (insert text)))
          (nelisp-dev-source-dispatch
           (list (cons "operation" op) (cons "request_id" "test")
                 (cons "arguments" (list (cons "path" "fixture.el") (cons "symbol" symbol))))
           (list :root root :target "host-emacs")))
      (delete-directory root t))))

(defun nelisp-dev-source-test--diagnostics (result)
  (cdr (assoc "diagnostics" result)))

(ert-deftest nelisp-dev-source-check-proves-plain-defun-arities ()
  (let* ((text (concat
                "(defun required (a b) nil)\n"
                "(defun optional (a &optional b) nil)\n"
                "(defun variadic (a &rest rest) nil)\n"
                "(defun caller ()\n"
                "  (required 1)\n"
                "  (required 1 2 3)\n"
                "  (optional)\n"
                "  (optional 1 2 3)\n"
                "  (variadic))\n"))
         (result (nelisp-dev-source-test--run text "check"))
         (diagnostics (nelisp-dev-source-test--diagnostics result)))
    (should (equal "failed" (cdr (assoc "status" result))))
    (should (= 5 (length diagnostics)))
    (should (cl-every (lambda (diagnostic)
                        (equal "NELISP-CHECK-ARITY"
                               (cdr (assoc "code" diagnostic))))
                      diagnostics))))

(ert-deftest nelisp-dev-source-check-keeps-uncertain-calls-inconclusive ()
  (let* ((text (concat
                "(defun target (a b) nil)\n"
                "(defun target (x) nil)\n"
                "(cl-defun destructuring ((a b)) nil)\n"
                "(defmacro known-macro (&rest body) body)\n"
                "(defun caller ()\n"
                "  '(target 1)\n"
                "  (unknown-macro (target 1))\n"
                "  (known-macro (target 1))\n"
                "  (flet ((target (x y) x)) (target 1))\n"
                "  (cl-labels ((target (x y) x)) (target 1))\n"
                "  (destructuring 1))\n"))
         (result (nelisp-dev-source-test--run text "check")))
    (should (equal "inconclusive" (cdr (assoc "status" result))))
    (should (= 0 (length (nelisp-dev-source-test--diagnostics result))))))

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
