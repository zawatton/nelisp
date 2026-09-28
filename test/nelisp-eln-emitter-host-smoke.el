;;; nelisp-eln-emitter-host-smoke.el --- Verify emitted GNU ELN -*- lexical-binding: t; -*-

(require 'comp)

(unless (and (equal emacs-version "31.1")
             (equal comp-abi-hash "ba35c031"))
  (error "Host ABI does not match pinned GNU 31.1 profile"))

(let* ((path (getenv "NELISP_ELN_OUT"))
       (name 'nelisp-eln-emitter-standalone-smoke))
  (unless (and path (file-exists-p path))
    (error "Standalone emitter did not create its output"))
  (load path nil t t)
  (let ((fn (symbol-function name)))
    (unless (and (subrp fn)
                 (native-comp-function-p fn)
                 (equal (subr-arity fn) '(1 . 1))
                 (null (documentation fn))
                 (not (commandp fn))
                 (= (funcall fn most-negative-fixnum) most-negative-fixnum)
                 (= (funcall fn -1) -1)
                 (= (funcall fn 0) 0)
                 (= (funcall fn 1) 1)
                 (= (funcall fn most-positive-fixnum) most-positive-fixnum))
      (error "Generated ELN failed GNU registration/execution checks"))
    (let ((object (cons 'left 'right))
          (string (copy-sequence "same Lisp_Object")))
      (unless (and (eq (funcall fn object) object)
                   (eq (funcall fn string) string))
        (error "Generated ELN copied or reboxed its argument")))
    (unless (and (eq (car (condition-case data
                             (progn (funcall name) nil)
                           (error data)))
                     'wrong-number-of-arguments)
                 (eq (car (condition-case data
                              (progn (funcall name 1 2) nil)
                       (error data)))
                     'wrong-number-of-arguments))
      (error "Generated ELN has incorrect arity handling")))
  (let ((bad (expand-file-name "corrupt.eln" (file-name-directory path))))
    (unwind-protect
        (progn
          (copy-file path bad t)
          (with-temp-buffer
            (set-buffer-multibyte nil)
            (insert-file-contents-literally bad)
            (goto-char (point-min))
            (unless (search-forward "ba35c031" nil t)
              (error "Generated ELN has no ABI hash"))
            (replace-match "ca35c031" t t)
            (write-region (point-min) (point-max) bad nil 'silent))
          (unless (eq (car (condition-case data
                               (progn (load bad nil t t) nil)
                             (error data)))
                      'native-lisp-file-inconsistent)
            (error "GNU loader accepted corrupted ABI metadata")))
      (when (file-exists-p bad)
        (delete-file bad))))
  (princ "GNU-ELN-LOAD-PASS\n"))

;;; nelisp-eln-emitter-host-smoke.el ends here
