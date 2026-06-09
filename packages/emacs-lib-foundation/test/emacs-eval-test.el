;;; emacs-eval-test.el --- Tests for emacs-eval  -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for the `defalias' / `fset' / `declare-function' polyfills.
;;
;; Each Emacs-side `unless (fboundp ...)' guard means the host Emacs
;; uses its real C primitives and the polyfill stays inert.  The
;; assertions below still exercise the symbols so any unintended
;; divergence between the polyfill and Emacs surfaces under host
;; Emacs ERT runs.

;;; Code:

(require 'ert)
(require 'emacs-eval)

;;;; --- fset / defalias ----------------------------------------------------

(ert-deftest emacs-eval-test/fset-installs-callable ()
  (defun emacs-eval-test--src (x) (* x 2))
  (let ((sym (make-symbol "emacs-eval-test--dst")))
    (fset sym #'emacs-eval-test--src)
    (should (= (funcall sym 21) 42))))

(ert-deftest emacs-eval-test/defalias-creates-working-alias ()
  (defun emacs-eval-test--add (a b) (+ a b))
  (let ((sym (make-symbol "emacs-eval-test--alias")))
    (defalias sym #'emacs-eval-test--add)
    (should (= (funcall sym 3 4) 7))))

(ert-deftest emacs-eval-test/defalias-returns-symbol ()
  (defun emacs-eval-test--id (x) x)
  (let ((sym (make-symbol "emacs-eval-test--ret")))
    (should (eq (defalias sym #'emacs-eval-test--id) sym))))

(ert-deftest emacs-eval-test/defalias-accepts-forward-symbol ()
  (let ((sym (make-symbol "emacs-eval-test--forward")))
    (should (eq (defalias sym 'emacs-eval-test--forward-target) sym))
    (defun emacs-eval-test--forward-target () 42)
    (should (= (funcall sym) 42))))


;;;; --- declare-function ---------------------------------------------------

(ert-deftest emacs-eval-test/declare-function-is-noop ()
  ;; Should expand to nil and produce no error / no side effect.
  (should (null (declare-function nonexistent-fn "imaginary-file" (a b))))
  (should (null (declare-function another-fn "wherever"))))


;;;; --- metadata no-ops ----------------------------------------------------

(ert-deftest emacs-eval-test/internal-make-var-non-special-is-callable ()
  (should (fboundp 'internal-make-var-non-special))
  (should (null (internal-make-var-non-special
                 'emacs-eval-test--non-special))))

(ert-deftest emacs-eval-test/runtime-callable-fallbacks-are-callable ()
  (should (eq 'x (purecopy 'x)))
  (should (string-equal "value 42" (format-message "value %d" 42)))
  (should (= 42 (with-local-quit 42)))
  (should (null (with-demoted-errors "demoted %s"
                  (error "boom")))))


(ert-deftest emacs-eval-test/autoload-lazy-loads-on-call ()
  (let* ((dir (make-temp-file "nemacs-al-" t))
         (file (expand-file-name "alfeat.el" dir)))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert "(defun emacs-eval-test--al-fn (x) (* x 2))\n"
                    "(provide 'alfeat)\n"))
          (fmakunbound 'emacs-eval-test--al-fn)
          (autoload 'emacs-eval-test--al-fn file)
          ;; a thunk is installed before the file is loaded
          (should (fboundp 'emacs-eval-test--al-fn))
          ;; calling it triggers the load and runs the real definition
          (should (= 6 (emacs-eval-test--al-fn 3))))
      (fmakunbound 'emacs-eval-test--al-fn)
      (when (file-directory-p dir) (delete-directory dir t)))))

(ert-deftest emacs-eval-test/autoload-leaves-defined-function ()
  (defun emacs-eval-test--al-already () 'original)
  (unwind-protect
      (progn
        ;; autoload is a no-op for an already-defined function
        (autoload 'emacs-eval-test--al-already "nonexistent-file")
        (should (eq 'original (emacs-eval-test--al-already))))
    (fmakunbound 'emacs-eval-test--al-already)))

(provide 'emacs-eval-test)

;;; emacs-eval-test.el ends here
