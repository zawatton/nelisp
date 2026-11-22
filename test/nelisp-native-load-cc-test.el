;;; nelisp-native-load-cc-test.el --- native condition-case, known defect  -*- lexical-binding: t; -*-

;; A native `condition-case' catches a signal raised by a builtin it calls.
;; Since the signal-stash flip (6fa5b5116) the pending exit lives in the BSS
;; link symbol nl_exit_values; the handler (lisp-exit-control-flow in
;; nelisp-aot-compiler.el) reads its two WORDs there, not the stale arena
;; views at arena+24/+56.  Needs NELISP_AOT_DIFF_HOME.

(require 'ert)
(require 'nelisp-artifact)
(require 'nelisp-aot-compiler)
(require 'nelisp-native-load)

(ert-deftest nelisp-native-load-cc/exit-values-offset-matches-loader ()
  (should (= nelisp-aot-compiler--exit-values-offset
             nelisp-native-load--exit-values-offset)))

(ert-deftest nelisp-native-load-cc/condition-case-catches-builtin-signal ()
  (let ((home (getenv "NELISP_AOT_DIFF_HOME")))
    (unless (and home (not (string-empty-p home)))
      (ert-skip "NELISP_AOT_DIFF_HOME unset"))
    (unless (file-executable-p (expand-file-name "target/nelisp" home))
      (error "NELISP_AOT_DIFF_HOME=%s has no executable target/nelisp" home))
    (let* ((dir (make-temp-file "cc" t))
           (src (expand-file-name "cc.el" dir))
           (art (expand-file-name "cc.neln" dir))
           (probe (expand-file-name "probe.el" dir))
           (lisp (file-name-directory (locate-library "nelisp-native-load.el"))))
      (unwind-protect
          (progn
            (with-temp-file src
              (insert "(defun cc-try (a) (condition-case e (+ a 1) (error (list 'caught (car e)))))\n"
                      "(defun cc-data (a) (condition-case e (+ a 1) (error e)))\n"
                      "(defun cc-ok (a) (condition-case e (+ a 1) (error 'bad)))\n"
                      ;; the inner pad declines (no arith-error), the outer one catches
                      "(defun cc-nested (a) (condition-case e (condition-case e2 (+ a 1) (arith-error 'inner)) (error (list 'outer (car e)))))\n"
                      "(defun cc-inner (a) (condition-case e (condition-case e2 (+ a 1) (wrong-type-argument (list 'inner (cdr e2)))) (error 'outer)))\n"))
            (nelisp-artifact-compile-file src art nil nil nil nil nil 'neln 'required)
            (with-temp-file probe
              (insert (format "(load %S nil t)\n" (expand-file-name "nelisp-native-load.el" lisp)))
              ;; The image enforces the settings the artifact was built with.
              (insert (format "(setq nelisp-native-load-expected-codegen-settings '%S)\n"
                              (nelisp-native-load-codegen-settings)))
              (dolist (c '(("cc-try" x) ("cc-data" x) ("cc-ok" 1) ("cc-nested" x) ("cc-inner" x)))
                (insert (format "(princ (format \"R %%S\\n\" (condition-case e (nelisp-native-load-call (nelisp-native-load-artifact %S %S) (list '%S)) (error (list 'ESCAPED (car e))))))\n"
                                art (car c) (cadr c)))))
            (let ((out (with-temp-buffer
                         (let ((default-directory (file-name-as-directory home)))
                           (call-process "bash" nil t nil "-c"
                                         (format "ulimit -v 8388608; exec timeout 300 target/nelisp --load %s -- < /dev/null"
                                                 (shell-quote-argument probe))))
                         (buffer-string))))
              ;; GNU answers, in order.
              (should (string-match-p "^R (caught wrong-type-argument)$" out))
              (should (string-match-p "^R (wrong-type-argument number-or-marker-p x)$" out))
              (should (string-match-p "^R 2$" out))
              (should (string-match-p "^R (outer wrong-type-argument)$" out))
              (should (string-match-p "^R (inner (number-or-marker-p x))$" out))))
        (delete-directory dir t)))))

(provide 'nelisp-native-load-cc-test)
