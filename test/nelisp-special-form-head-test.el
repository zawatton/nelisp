;;; nelisp-special-form-head-test.el --- compiler reader-head precedence -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'nelisp-bytecode)
(require 'nelisp-artifact)
(require 'nelisp-aot-compiler)
(require 'nelisp-prelude-bytecode)
(defvar nelisp--fx-called nil)
(defvar nelisp--fx-missing nil)

;; Load only the VM compiler and its data, without running the prelude on Emacs.
(let ((names '(nelisp--fx-inline-ops nelisp--fx-declared nelisp--fx-special-p
               nelisp--fx-special-gen nelisp--fx-expand nelisp--fx-native-heads
               nelisp--fx-native-macro-environment nelisp--fx-compile)))
  (with-temp-buffer
    (insert-file-contents
     (expand-file-name "../scripts/nelisp-stdlib-prelude.el"
                       (file-name-directory (or load-file-name buffer-file-name))))
    (goto-char (point-min))
    (condition-case nil
        (while names
          (let ((form (read (current-buffer))))
            (when (memq (cadr form) names)
              (eval form t)
              (setq names (delq (cadr form) names)))))
      (end-of-file nil))))
;; Expand the compiler's own cl-labels before mocking source macro expansion.
(byte-compile 'nelisp--fx-compile)

(ert-deftest special-head/fx-declines-unlowered-syntax ()
  (dolist (form '((throw 'tag 42) (lambda () 42) (defalias 'x 'car)))
    (should-not (nelisp--fx-compile form))))

(ert-deftest special-head/fx-shields-native-macros ()
  (dolist (form '((when t 42) (unless nil 42) (prog2 1 42 3)
                  (dolist (x '(1 2)) (+ x 1))))
    (let* ((head (car form))
           (expected (nelisp--fx-compile form)))
      (should expected)
      (let ((original (symbol-function 'macroexpand-all)))
        (cl-letf (((symbol-function 'macroexpand-all)
                   (lambda (source &optional env)
                     (funcall original source
                              (append env (list (cons head (lambda (&rest _) '(quote wrong)))))))))
          (let ((actual (nelisp--fx-compile form)))
            (should actual)
            ;; Entry guards retain fresh dolist gensyms; compare the code.
            (aset (car expected) 1 nil)
            (aset (car actual) 1 nil)
            (should (equal expected actual))))))))

(ert-deftest special-head/private-bytecode-declines-native-calls ()
  (dolist (form '((defalias 'x 'car) (dolist (x '(1)) x)))
    (should-error (nelisp-bc-compile form) :type 'nelisp-bc-unimplemented)))

(ert-deftest special-head/artifact-shields-native-macros ()
  (let ((form '(when t 42)))
    (should (equal form (nelisp-artifact--macroexpand-body form)))))

(ert-deftest special-head/private-bytecode-host-shields-native-macros ()
  (let ((saved nelisp--fx-native-macro-environment))
    (unwind-protect
        (progn
          (makunbound 'nelisp--fx-native-macro-environment)
          (cl-letf (((symbol-function 'when) '(macro lambda (&rest _) '(quote wrong))))
            (should (equal '(when t 42) (nelisp-bc-macroexpand-source '(when t 42))))))
      (setq nelisp--fx-native-macro-environment saved))))

(ert-deftest special-head/short-prog2-has-no-effects ()
  (should (equal (nelisp--fx-compile '(prog2 (error "not evaluated")))
                 (nelisp--fx-compile nil)))
  (should (equal 0 (nelisp-aot-compiler--preprocess-source '(prog2 (error "not evaluated")))))
  (should-not (nelisp-bc-run (nelisp-bc-compile '(prog2 (error "not evaluated"))))))

(ert-deftest special-head/gnu-adoption-retains-source ()
  (dolist (body '((throw 'tag 42) (defalias 'x 'car)))
    (let* ((source (format "(defun source-head () %S)\n" body))
           (result (nelisp-prelude-bytecode-transform source "head.el" nil t)))
      (should (= 0 (nth 1 result)))
      (should (equal source (car result)))
      (should (equal "native-special-head-requires-source" (nth 4 (car (nth 2 result)))))))
  (should-not (nelisp-prelude-bytecode--unlowered-native-head-p '(funcall #'throw 'tag 42)))
  (should-not (nelisp-prelude-bytecode--unlowered-native-head-p '(quote (throw tag 42)))))

(ert-deftest special-head/gnu-adoption-retains-macro-generated-throw ()
  (let* ((source "(defmacro vsh-escape () (list 'throw (list 'quote 'tag) 42))\n(defun vsh-generated () (vsh-escape))\n")
         (result (nelisp-prelude-bytecode-transform source "generated.el" nil t)))
    (should (= 0 (nth 1 result)))
    (should (equal "native-special-head-requires-source" (nth 4 (car (nth 2 result)))))))

(ert-deftest special-head/gnu-whole-file-retains-replaced-native-macros ()
  (dolist (form '((when t 42) (unless nil 42) (prog2 1 42 3)
                  (dolist (x '(1 2)) 42)))
    (let* ((source (format "(defmacro %S (&rest ignored) '(quote wrong))\n(defun vsh-vendor () %S)\n"
                           (car form) form))
           (result (nelisp-prelude-bytecode-vendor-transform source "head.el" nil 15)))
      (should (= 0 (nth 1 result)))
      (should (string-match-p "defun vsh-vendor" (car result)))
      (should (equal "native-special-head-macro-requires-source" (nth 4 (car (last (nth 2 result)))))))))

(ert-deftest special-head/aot-throw-is-not-tail-self-call ()
  (let ((found (list nil)))
    (should (equal '(throw tag value)
                   (nelisp-aot-compiler--tco-walk
                    '(throw tag value) 'throw '(tag value) 2 'continue '(a b) found)))
    (should-not (car found))))

(ert-deftest special-head/aot-same-unit-throw-uses-exit-bridge ()
  (let ((ir (nelisp-aot-compiler--parse
             '(seq
               (defun throw ((out :type sexp) (mirror :type sexp)
                             (frames :type sexp) (scratch :type sexp)
                             (name_slot :type sexp) (value :type sexp))
                 (throw 'done value)))))
        (calls nil))
    (nelisp-aot-compiler--walk-ir
     ir (lambda (node)
          (when (eq (nelisp-aot-compiler--ir-kind node) 'extern-call)
            (push (nelisp-aot-compiler--ir-get node :name) calls))))
    (should (memq 'nelisp_aot_throw calls))))

(ert-deftest special-head/aot-does-not-expand-native-heads ()
  (let ((original (symbol-function 'macroexpand)))
    (cl-letf (((symbol-function 'macroexpand)
               (lambda (form &optional env)
                 (if (memq (car-safe form) '(when unless prog2 dolist throw))
                     '(quote wrong)
                   (funcall original form env)))))
      (dolist (form '((when t 42) (unless nil 42) (prog2 1 42 3)
                     (dolist (x '(1 2)) x) (throw 'tag 42)))
        (should-not (equal '(quote wrong)
                           (nelisp-aot-compiler--preprocess-source form)))))))

(ert-deftest special-head/aot-declines-nested-defalias ()
  (should-error (nelisp-aot-compiler--parse
                 '(defun caller () (defalias 'x 'car)))
                :type 'nelisp-aot-compiler-error))

(provide 'nelisp-special-form-head-test)
;;; nelisp-special-form-head-test.el ends here
