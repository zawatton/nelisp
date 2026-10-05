;;; -*- lexical-binding: t; -*-
(princ (format "buffer-constructor-arity=%S\n" (subr-arity (symbol-function 'get-buffer-create))))
(princ (format "ordinary-lambda-error=%S\n" (condition-case error
    (subr-arity (lambda (x) x)) (error (car error)))))
(let ((before (symbol-function 'make-temp-file)) file)
  (unwind-protect
      (progn
        (fset 'make-temp-file (lambda (&rest args) (error "high-level temp reentry")))
        (setq file (make-temp-file-internal "s52b-internal-" nil ".txt" "abc"))
        (princ (format "primitive-temp=%S\n"
                       (list (not (file-name-absolute-p file))
                             (= (logand (file-modes file) #o777) #o600)
                             (= (nth 7 (file-attributes file)) 3)))))
    (fset 'make-temp-file before)
    (when file (delete-file file))))
(princ "GUI-PACKAGE-RUNTIME-PROBE-DONE\n")
