;;; Authenticated context source slots. -*- lexical-binding: t; -*-
(require 'ert)
(require 'nelisp-bytecode-native-consumer)
(require 'nelisp-bytecode-native-rooted-cfg-native)
(require 'nelisp-bytecode-native-rooted-cfg-call)

(defun nelisp-context-shape-test--prefix (body)
  ;; Evaluate the genuine lexical prefix without redefining any public owner.
  (let* ((path (or (getenv "NELISP_CONTEXT_SHAPE_SOURCE")
                   (locate-library "nelisp-bytecode-native-rooted-cfg-plan")))
         (form (with-temp-buffer
                 (insert-file-contents path)
                 (let (value)
                   (while (not (and (eq (car-safe value) 'let)
                                   (assq 'guard-owners (cadr value))))
                     (setq value (read (current-buffer)))) value)))
         (labels (caddr form)))
    (eval `(let ,(cadr form)
             (cl-labels ,(cadr labels) ,(car (last (cddr labels))) ,body)) t)))

(ert-deftest nelisp-context-shape/exact-source-slots ()
  (nelisp-context-shape-test--prefix
   '(let ((provider (nelisp-native-arithmetic-v2-dependency-context)))
      (should (= (length provider) 16))
      (should (provider-source-context-p provider))
      (dotimes (index 16)
        (should (eq (not (null (source-slot-data-p provider index)))
                    (not (null (memq index '(7 11 14)))))))
      (should (guard-valid-p))
      (should (guard-valid-p (guard-context-copy guard-context)))
      (should-not (provider-source-context-p (cl-subseq provider 0 10))))))

(ert-deftest nelisp-context-shape/source-mutation-and-cycle-refusal ()
  (nelisp-context-shape-test--prefix
   '(dolist (index '(7 11 14))
      (dolist (cycle '(nil t))
        (let* ((copy (guard-context-copy guard-context))
               (provider (aref (aref copy 6) 4))
               (source (aref provider index)))
          (should (consp source))
          (if cycle (setcdr source source) (setcar source 'counterfeit))
          (should-not (guard-valid-p copy)))))))

(ert-deftest nelisp-context-shape/opaque-owner-refusal ()
  (nelisp-context-shape-test--prefix
   '(dolist (index '(0 3 10 12 13))
      (let* ((copy (guard-context-copy guard-context))
             (provider (aref (aref copy 6) 4)))
        (aset provider index (lambda (&rest ignored) ignored))
        (should-not (provider-source-context-p provider))
        (should-not (guard-valid-p copy))))))
