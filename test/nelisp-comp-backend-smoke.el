;;; nelisp-comp-backend-smoke.el --- S11 leaf publication -*- lexical-binding: t; -*-

;; Run with NELISP_COMP_TEST_ELN set to an absolute output path.  NeLisp
;; needs the dynamic reader and an artifact image.  A native-enabled GNU
;; process with the pinned ABI can run this file against that same output.

(defun nelisp-comp-smoke-constant ()
  17)

(let ((output (getenv "NELISP_COMP_TEST_ELN")))
  (unless output (error "Set NELISP_COMP_TEST_ELN"))
  (let ((function
         (if (fboundp 'nelisp--native-subr-arity)
             (progn
               (require 'nelisp-comp)
               (let ((comp-running-batch-compilation t))
                 (native-compile 'nelisp-comp-smoke-constant output)))
           (require 'comp)
           (unless (and (native-comp-available-p)
                        (equal comp-abi-hash "ba35c031"))
             (error "GNU Emacs does not match the ELN producer profile"))
           (native-elisp-load output))))
    (unless (and (subrp function)
                 (native-comp-function-p function)
                 (eq function (symbol-function 'nelisp-comp-smoke-constant))
                 (equal (subr-arity function) '(0 . 0))
                 (eql 17 (funcall function)))
      (error "ELN failed native registration or constant execution"))
    (garbage-collect)
    (unless (eql 17 (funcall function))
      (error "Native code disappeared during collection")))
  (when (fboundp 'nelisp--native-subr-arity)
    (let ((comp-running-batch-compilation t)
          (before (with-temp-buffer
                    (insert-file-contents-literally output)
                    (secure-hash 'sha256 (current-buffer)))))
      (unless (eq 'native-ice
                  (condition-case err
                      (progn (native-compile '(lambda (x) (1+ x)) output) nil)
                    (native-ice (car err))))
        (error "Unsupported primitive call was not rejected"))
      (unless (equal before (with-temp-buffer
                              (insert-file-contents-literally output)
                              (secure-hash 'sha256 (current-buffer))))
        (error "Rejected compilation changed the previous artifact"))))
  (princ "S11 native constant passed\n"))
