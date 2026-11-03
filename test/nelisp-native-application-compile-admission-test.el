;;; nelisp-native-application-compile-admission-test.el --- FX admission -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

(require 'ert)
(require 'nelisp-native-application-producer)
(require 'nelisp-cc-fxir-analysis)

(defun nnap-ca--closure (form)
  (cons 'closure (cons '(t) (cons (nth 2 form) (nthcdr 3 form)))))

(defun nnap-ca--plan (form)
  (list :status :validated-inputs
        :names (list (nth 1 form)) :forms (list (cons (nth 1 form) form))
        :form-hashes (list (cons (nth 1 form) (nelisp-cc-fxir-admission--digest form)))))

(defun nnap-ca--compile (plan constructor)
  "Host-only compiler fixture: publish the exact prepared closures, then admit."
  (let* ((candidates (nelisp-native-application-producer--prepare-candidates plan constructor))
         (adapter (nelisp-native-application-producer--host-adapter)))
    (unwind-protect
        (progn
          (dolist (candidate candidates)
            (fset (plist-get candidate :name) (plist-get candidate :closure)))
          (nelisp-native-application-producer--compile-candidates
           plan candidates (plist-get adapter :lookup)))
      (dolist (candidate candidates) (fmakunbound (plist-get candidate :name))))))

(ert-deftest nnap-ca-admits-valid-loop-ir-with-optimization-refusals ()
  ;; Compiler APIs are mocked here: this checks producer orchestration only.
  (let* ((form '(defun nnap-ca-loop (input)
                  (let ((state input))
                    (while state (setq state (+ state -1)))
                    state)))
         (code (vector 16 (vector '+ nil nil nil nil) 1))
         (ir '(:while (:slot 0) (:setslot 0 (:call 1 1 (:slot 0)))))
         candidates)
    (cl-letf (((symbol-function 'nelisp--fx-macros-used) (lambda (_) nil))
              ((symbol-function 'nelisp-fxir-recompile)
               (lambda (closure)
                 (should (equal (nthcdr 3 closure) (nthcdr 3 form)))
                 (list code 1 ir)))
              ((symbol-function 'nelisp-fxir-problems) (lambda (_ir _code _locals) nil))
              ((symbol-function 'nelisp-fxir-callees) (lambda (_ir _code) '(+))))
      (setq candidates
            (nnap-ca--compile
             (nnap-ca--plan form) #'nnap-ca--closure)))
    (should (= (length candidates) 1))
    (should (plist-get (plist-get (car candidates) :compiler-analysis) :valid))
    (should (memq :loop
                  (plist-get (plist-get (car candidates) :compiler-analysis) :refusals)))
    (should (eq (plist-get (car (plist-get (car candidates) :provider-witnesses))
                           :identity)
                (symbol-function '+)))))

(ert-deftest nnap-ca-refuses-macro-and-mutable-global-contexts ()
  ;; These compiler API mocks do not exercise a NeLisp image or compiler.
  (let* ((macro-form '(defun nnap-ca-macro (x) (when x x)))
         (global-form '(defun nnap-ca-global (x) nnap-ca-global-value)))
    (cl-letf (((symbol-function 'nelisp--fx-macros-used) (lambda (_) '(when)))
              ((symbol-function 'nelisp-fxir-recompile)
               (lambda (_) (error "macro form reached compiler"))))
      (should-error
       (nnap-ca--compile
        (nnap-ca--plan macro-form) #'nnap-ca--closure)))
    (cl-letf (((symbol-function 'nelisp--fx-macros-used) (lambda (_) nil))
              ((symbol-function 'nelisp-fxir-recompile)
               (lambda (_) (list (vector 19 'nnap-ca-global-value) 1 '(:varref 1))))
              ((symbol-function 'nelisp-fxir-problems) (lambda (_ir _code _locals) nil))
              ((symbol-function 'nelisp-fxir-callees) (lambda (_ir _code) nil)))
      (should-error
       (nnap-ca--compile
        (nnap-ca--plan global-form) #'nnap-ca--closure)))))

(ert-deftest nnap-ca-real-analysis-refuses-noncallable-alias-and-invalid-ir ()
  (let* ((provider (make-symbol "nnap-ca-invalid-provider"))
         (form '(defun nnap-ca-provider-user (x) x))
         (plan (nnap-ca--plan form))
         (code (vector 16 (vector provider nil nil nil nil) 1))
         (ir '(:call 1 1 (:slot 0))))
    (unwind-protect
        (cl-letf (((symbol-function 'nelisp--fx-macros-used) (lambda (_) nil))
                  ((symbol-function 'nelisp-fxir-problems) (lambda (&rest _) nil))
                  ((symbol-function 'nelisp-fxir-callees) (lambda (&rest _) (list provider)))
                  ((symbol-function 'nelisp-fxir-recompile) (lambda (_) (list code 1 ir))))
          (dolist (cell (list 17 '+ '(macro lambda (x) x)))
            (fset provider cell)
            (should-error (nnap-ca--compile plan #'nnap-ca--closure)))
          (fset provider (lambda (x) x))
          (setq code [] ir '(:unknown-node))
          (should-error (nnap-ca--compile plan #'nnap-ca--closure)))
      (fmakunbound provider))))

(ert-deftest nnap-ca-real-analysis-detects-provider-mutation ()
  (let* ((provider (make-symbol "nnap-ca-provider-drift"))
         (form '(defun nnap-ca-drift-user (x) x))
         (original-analysis (symbol-function 'nelisp-cc-fxir-analysis-row))
         (code (vector 16 (vector provider nil nil nil nil) 1)))
    (fset provider (lambda (x) x))
    (unwind-protect
        (cl-letf (((symbol-function 'nelisp--fx-macros-used) (lambda (_) nil))
                  ((symbol-function 'nelisp-fxir-problems) (lambda (&rest _) nil))
                  ((symbol-function 'nelisp-fxir-callees) (lambda (&rest _) (list provider)))
                  ((symbol-function 'nelisp-fxir-recompile)
                   (lambda (_) (list code 1 '(:call 1 1 (:slot 0)))))
                  ((symbol-function 'nelisp-cc-fxir-analysis-row)
                   (lambda (&rest args)
                     (fset provider (lambda (_) :changed))
                     (apply original-analysis args))))
          (should-error (nnap-ca--compile (nnap-ca--plan form) #'nnap-ca--closure)))
      (fmakunbound provider))))

(provide 'nelisp-native-application-compile-admission-test)
;;; nelisp-native-application-compile-admission-test.el ends here
