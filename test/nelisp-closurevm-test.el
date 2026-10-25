;;; nelisp-closurevm-test.el --- Anonymous closures on the word VM -*- lexical-binding: t; -*-
;; Phase 1 of fxvm S1.3: an anonymous lambda whose captured environment holds
;; no lexical binding is registered by its body object's identity on its second
;; call when it holds a loop, else after `nelisp--fx-anon-calls' calls (a
;; boot's small lambdas are never compiled).  A closure over a variable stays on the interpreter (the shared Cell
;; cannot be handed to the VM safely yet), and says so before any effect.
(require 'ert)
(require 'cl-lib)
(require 'macroexp)

(defconst closurevm-root
  (file-name-directory (directory-file-name
                        (file-name-directory (or load-file-name buffer-file-name)))))

;; Load only the FX definitions, never the prelude's bootstrap side effects.
(let ((file (or (getenv "CLOSUREVM_PRELUDE")
                (expand-file-name "scripts/nelisp-stdlib-prelude.el" closurevm-root))))
  (with-temp-buffer
    (insert-file-contents file)
    (condition-case nil
        (while t
          (let ((form (read (current-buffer))))
            (when (and (memq (car-safe form) '(defun defvar defconst))
                       (symbolp (cadr form))
                       (or (string-prefix-p "nelisp--fx-" (symbol-name (cadr form)))
                           (eq (cadr form) 'nelisp-fx-enabled)))
              (eval form t))))
      (end-of-file nil))))

(defmacro closurevm-isolated (&rest body)
  `(let ((nelisp--fx-table (make-hash-table :test 'eq))
         (nelisp--fx-pending (make-hash-table :test 'eq))
         (nelisp--fx-pending-tries (make-hash-table :test 'eq))
         (nelisp--fx-callees (make-hash-table :test 'eq))
         (nelisp--fx-anon-callees (make-hash-table :test 'eq))
         (nelisp--fx-anon-seen (make-hash-table :test 'eq))
         (nelisp--fx-named (make-hash-table :test 'eq))
         (nelisp--fx-anon-ready t)
         (nelisp--fx-anon-calls 2)
         (nelisp--fx-waiting (make-hash-table :test 'eq))
         (nelisp--fx-errored nil) (nelisp--fx-env-disabled nil)
         (nelisp-fx-enabled t))
     (cl-letf (((symbol-function 'nelisp--fx-run) #'ignore)) ,@body)))

(ert-deftest closurevm-anonymous-code-identity ()
  (closurevm-isolated
   (let ((body '((+ cv-x 1))))
     (nelisp--fx-first-call body '(t) '(a))
     (should-not (gethash body nelisp--fx-table))
     (nelisp--fx-first-call body '(t) '(a))
     (should (vectorp (gethash body nelisp--fx-table)))
     (should-not (gethash (copy-tree body) nelisp--fx-table))
     (let ((record (gethash body nelisp--fx-table)))
       (nelisp--fx-first-call body '(t) '(a))
       (should (eq record (gethash body nelisp--fx-table)))))))

;; A function the prelude defined before the hooks, or a macro expander, is
;; named: its body is never compiled as an anonymous lambda.
(ert-deftest closurevm-named-body-not-compiled-anonymously ()
  (closurevm-isolated
   (let* ((body '((+ cv-x 1)))
          (fn (cons 'closure (cons '(t) (cons '(a) body)))))
     (nelisp--fx-note-named fn)
     (nelisp--fx-note-named (cons 'macro (cons 'closure (cons '(t) (cons '(a) '((* a 2)))))))
     (nelisp--fx-first-call body '(t) '(a))
     (nelisp--fx-first-call body '(t) '(a))
     (should (eq 'none (gethash body nelisp--fx-table))))))

(ert-deftest closurevm-loop-compiles-on-second-call-others-wait ()
  (closurevm-isolated
   (let ((nelisp--fx-anon-calls 5)
         (loopy '((let ((i 0)) (while (< i 3) (setq i (1+ i))) i)))
         (plain '((+ cv-x 1))))
     (dotimes (_ 2) (nelisp--fx-first-call loopy '(t) nil))
     (should (vectorp (gethash loopy nelisp--fx-table)))
     (dotimes (_ 4) (nelisp--fx-first-call plain '(t) nil))
     (should-not (gethash plain nelisp--fx-table))
     (nelisp--fx-first-call plain '(t) nil)
     (should (vectorp (gethash plain nelisp--fx-table))))))

(ert-deftest closurevm-not-ready-stays-interpreted ()
  (closurevm-isolated
   (let ((nelisp--fx-anon-ready nil) (body '((+ cv-x 1))))
     (nelisp--fx-first-call body '(t) nil)
     (nelisp--fx-first-call body '(t) nil)
     (should (eq 'none (gethash body nelisp--fx-table))))))

(ert-deftest closurevm-bare-declaration-env ()
  (closurevm-isolated
   (let ((body '((+ cv-x 1))))
     (nelisp--fx-first-call body '(cv-decl t) nil)
     (nelisp--fx-first-call body '(cv-decl t) nil)
     (should (vectorp (gethash body nelisp--fx-table))))))

(ert-deftest closurevm-captured-binding-stays-interpreted ()
  (closurevm-isolated
   (let ((body '((setq cv-x (+ cv-x 1)))))
     (nelisp--fx-first-call body (list (cons 'cv-x 1) t) nil)
     (nelisp--fx-first-call body (list (cons 'cv-x 1) t) nil)
     (should (eq 'none (gethash body nelisp--fx-table))))))

(ert-deftest closurevm-unsupported-before-effects ()
  (closurevm-isolated
   (let ((body '((unwind-protect cv-x nil))))
     (nelisp--fx-first-call body '(t) nil)
     (nelisp--fx-first-call body '(t) nil)
     (should (eq 'none (gethash body nelisp--fx-table))))))

(ert-deftest closurevm-disabled-registration ()
  (closurevm-isolated
   (let ((nelisp-fx-enabled nil) (body '((+ cv-x 1))))
     (nelisp--fx-first-call body '(t) nil)
     (nelisp--fx-first-call body '(t) nil)
     (should-not (vectorp (gethash body nelisp--fx-table))))))

(ert-deftest closurevm-callee-becoming-macro-retires-anonymous ()
  (closurevm-isolated
   (let ((body '((cv-callee 1))))
     (cl-letf (((symbol-function 'cv-callee) (lambda (x) x)))
       (nelisp--fx-first-call body '(t) nil)
       (nelisp--fx-first-call body '(t) nil)
       (should (vectorp (gethash body nelisp--fx-table)))
       (cl-letf (((symbol-function 'cv-callee) '(macro . (lambda (x) x))))
         (nelisp--fx-check-macro 'cv-callee)
         (should-not (gethash body nelisp--fx-table)))))))

(ert-deftest closurevm-live-semantics ()
  (skip-unless (getenv "CLOSUREVM_BINARY"))
  (let ((process-environment (copy-sequence process-environment)))
    (setenv "CV_REQUIRE_VM" "1")
    (with-temp-buffer
      (let ((rc (process-file "timeout" nil (current-buffer) nil "120"
                              (getenv "CLOSUREVM_BINARY") "--load"
                              (expand-file-name "test/nelisp-closurevm-fixture.el" closurevm-root)
                              "--")))
        (should (equal rc 0))
        (should (string-match-p "CLOSUREVM COMPLETE checked=" (buffer-string)))))))

(provide 'nelisp-closurevm-test)
