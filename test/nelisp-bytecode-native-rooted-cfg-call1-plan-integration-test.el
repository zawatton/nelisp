;;; nelisp-bytecode-native-rooted-cfg-call1-plan-integration-test.el --- sealed CALL1 layout integration -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'nelisp-bytecode-compiler-input)
(require 'nelisp-bytecode-native-consumer)
(require 'nelisp-bytecode-native-rooted-cfg-plan)
(require 'nelisp-bytecode-native-rooted-cfg-emit)
(require 'nelisp-bytecode-native-rooted-cfg-shared-emit)

(defun nelisp-bytecode-native-rooted-cfg-call1-plan-test--fixture (form)
  (let* ((dir (make-temp-file "r9-call1-fixture-" t))
         (source (expand-file-name "fixture.el" dir))
         (elc (concat source "c")))
    (with-temp-file source
      (insert ";;; -*- lexical-binding: t; -*-\n(defun r9-call1-fixture "
              (prin1-to-string (cadr form)) " "
              (mapconcat #'prin1-to-string (cddr form) " ") ")\n"))
    (let ((byte-compile-verbose nil) (byte-compile-warnings nil))
      (unless (byte-compile-file source) (error "fixture compilation failed")))
    (delete-file source)
    (list dir elc)))

(defun nelisp-bytecode-native-rooted-cfg-call1-plan-test--input (form)
  (let* ((fixture (nelisp-bytecode-native-rooted-cfg-call1-plan-test--fixture form))
         (dir (car fixture)) (elc (cadr fixture)))
    (unwind-protect
        (nelisp-bytecode-compiler-input-build
         (cdr (assq 'r9-call1-fixture
                    (nelisp-bytecode-native-consumer-read-elc-functions elc))))
      (delete-directory dir t))))

(defun nelisp-bytecode-native-rooted-cfg-call1-plan-test--child (source body)
  (let ((buffer (generate-new-buffer " *r9-call1-child*")) status output)
    (unwind-protect
        (progn
          (setq status
                (call-process "emacs" nil buffer nil "-Q" "--batch" "-L" "lisp"
                              "--eval"
                              (format "(progn (setq load-prefer-newer t) (require 'nelisp-bytecode-compiler-input) (require 'nelisp-bytecode-native-consumer) (load %S nil t t) %s)"
                                      (expand-file-name source) body)))
          (setq output (with-current-buffer buffer (buffer-string)))
          (cons status output))
      (kill-buffer buffer))))

(defun nelisp-bytecode-native-rooted-cfg-call1-plan-test--replace-once
    (text old new)
  (let ((position (string-match (regexp-quote old) text)))
    (unless position (error "source-mutant anchor absent"))
    (concat (substring text 0 position) new
            (substring text (+ position (length old))))))

(ert-deftest nelisp-bytecode-native-rooted-cfg-call1-plan/allocates-seven-roots-and-keeps-aliases ()
  (skip-unless (equal emacs-version "31.1"))
  (let* ((input (nelisp-bytecode-native-rooted-cfg-call1-plan-test--input
                 '(lambda (f x) (funcall f x))))
         (before (copy-tree input t))
         (plan (nelisp-bytecode-native-rooted-cfg-plan input))
         (block (car (plist-get plan :blocks)))
         (ops (plist-get block :operations))
         (call (nth 2 ops)))
    (should (eq (plist-get plan :status) 'complete))
    (should (equal (mapcar (lambda (op) (plist-get op :opcode)) ops)
                   '(stack-ref stack-ref call1 return)))
    (should (equal (mapcar (lambda (op) (list (plist-get op :input-roots)
                                              (plist-get op :output-root)))
                           (list (nth 0 ops) (nth 1 ops) call (nth 3 ops)))
                   '(((1) 1) ((2) 2) ((1 2) 3) ((3) nil))))
    (should (equal (list (plist-get plan :final-root)
                         (plist-get plan :required-root-count)
                         (plist-get plan :call-exit-root-base)
                         (plist-get plan :call-exit-root-count)) '(3 7 4 3)))
    (should-not (plist-member plan :exit-root-base))
    (should (= (plist-get (plist-get plan :entry-ast) :required-root-count) 7))
    (should (equal (list (plist-get call :argument-count) (plist-get call :call-base)
                         (plist-get call :provider) (plist-get call :exit-root-base))
                   '(1 1 nl_native_call_v2 4)))
    (dolist (op (list (nth 0 ops) (nth 1 ops) (nth 3 ops)))
      (dolist (key '(:argument-count :call-base :provider :exit-root-base))
        (should-not (plist-member op key))))
    (should-not (plist-member plan :arithmetic-context))
    (should-not (member "nl_native_call_v2" (plist-get plan :gateway-imports)))
    (should (equal input before))))

(ert-deftest nelisp-bytecode-native-rooted-cfg-call1-plan/routes-wider-shapes-through-f1 ()
  (skip-unless (equal emacs-version "31.1"))
  (dolist (form '((lambda (f) (funcall f))
                  (lambda (f x) (funcall f x x))
                  (lambda (f x) (funcall f 17))
                  (lambda (f x) (progn (funcall f x) (funcall f x)))))
    (let* ((input (nelisp-bytecode-native-rooted-cfg-call1-plan-test--input form))
           (plan (nelisp-bytecode-native-rooted-cfg-plan input)))
      (should (eq (plist-get input :status) 'complete))
      (should (eq (plist-get plan :status) 'complete))
      (should (equal (plist-get plan :gateway-imports) '("nl_native_funcall_v2"))))))

(ert-deftest nelisp-bytecode-native-rooted-cfg-call1-plan/refuses-rebound-layout-functions ()
  (skip-unless (equal emacs-version "31.1"))
  (let ((input (nelisp-bytecode-native-rooted-cfg-call1-plan-test--input
                '(lambda (f x) (funcall f x)))))
    (dolist (name '(nelisp-bytecode-native-call1-layout
                    nelisp-bytecode-native-call1-layout--bounded-plist-p
                    nelisp-bytecode-native-call1-layout--token-p))
      (cl-letf (((symbol-function name) (lambda (&rest _) nil)))
        (should (eq (plist-get (nelisp-bytecode-native-rooted-cfg-plan input) :status)
                    'unsupported))))))

(ert-deftest nelisp-bytecode-native-rooted-cfg-call1-plan/matches-pristine-non-call-plans-in-fresh-processes ()
  (skip-unless (equal emacs-version "31.1"))
  (let ((baseline "target/progress/r9-call1-plan-integration/before/nelisp-bytecode-native-rooted-cfg-plan.el")
        (candidate "lisp/nelisp-bytecode-native-rooted-cfg-plan.el"))
    (dolist (form '("(lambda (x) (car x))" "(lambda (x) (1+ x))"))
      (let ((body (format "(let* ((input (nelisp-bytecode-compiler-input-build (byte-compile %s))) (plan (nelisp-bytecode-native-rooted-cfg-plan input))) (plist-put plan :input :elided) (princ (prin1-to-string plan)))" form)))
        (let ((old (nelisp-bytecode-native-rooted-cfg-call1-plan-test--child baseline body))
              (new (nelisp-bytecode-native-rooted-cfg-call1-plan-test--child candidate body)))
          (should (= (car old) 0))
          (should (= (car new) 0))
          (should (equal (cdr old) (cdr new))))))))

(ert-deftest nelisp-bytecode-native-rooted-cfg-call1-plan/source-mutant-fails-positive-oracle ()
  (skip-unless (equal emacs-version "31.1"))
  (let* ((source (with-temp-buffer
                   (insert-file-contents "lisp/nelisp-bytecode-native-rooted-cfg-plan.el")
                   (buffer-string)))
         (old "((eq kind 'call)\n                (if (and")
         (new "((eq kind 'call)\n                (setq failure \"source mutant disables CALL1\"))\n               ((eq kind 'call)\n                (if (and")
         (mutant (make-temp-file "r9-call1-planner-mutant-" nil ".el"))
         (fixture (nelisp-bytecode-native-rooted-cfg-call1-plan-test--fixture
                   '(lambda (f x) (funcall f x))))
         (elc (cadr fixture))
         (body (format "(let* ((fn (cdr (assq 'r9-call1-fixture (nelisp-bytecode-native-consumer-read-elc-functions %S)))) (input (nelisp-bytecode-compiler-input-build fn)) (plan (nelisp-bytecode-native-rooted-cfg-plan input))) (unless (and (eq (plist-get plan :status) 'complete) (= (plist-get plan :required-root-count) 7) (= (plist-get plan :final-root) 3)) (error \"same CALL1 positive oracle failed\")) (princ \"CALL1-PLAN-PASS\"))" elc))
         (main (nelisp-bytecode-native-rooted-cfg-call1-plan-test--child
                "lisp/nelisp-bytecode-native-rooted-cfg-plan.el" body)))
    (unwind-protect
        (progn
          (should (= (car main) 0))
          (with-temp-file mutant (insert (nelisp-bytecode-native-rooted-cfg-call1-plan-test--replace-once source old new)))
          (should-not (= (car (nelisp-bytecode-native-rooted-cfg-call1-plan-test--child mutant body)) 0)))
      (delete-file mutant)
      (delete-directory (car fixture) t))))

(ert-deftest nelisp-bytecode-native-rooted-cfg-call1-plan/emitter-explicitly-refuses-call1 ()
  (skip-unless (equal emacs-version "31.1"))
  (let* ((input (nelisp-bytecode-native-rooted-cfg-call1-plan-test--input
                 '(lambda (f x) (funcall f x))))
         (plan (nelisp-bytecode-native-rooted-cfg-plan input))
         (emitted (nelisp-bytecode-native-rooted-cfg-emit plan "r9_call1_probe"))
         (shared (nelisp-bytecode-native-rooted-cfg-shared-emit-build
                  plan "r9_call1_probe")))
    (should (eq (plist-get plan :status) 'complete))
    (should (eq (plist-get emitted :status) 'unsupported))
    (should (equal (plist-get emitted :reason) "unsupported planned operation call1"))
    (should (eq (plist-get shared :status) 'unsupported))
    (should (equal (plist-get shared :reason) "unsupported operation call1"))))

(provide 'nelisp-bytecode-native-rooted-cfg-call1-plan-integration-test)
;;; nelisp-bytecode-native-rooted-cfg-call1-plan-integration-test.el ends here
