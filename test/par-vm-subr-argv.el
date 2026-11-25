;;; par-vm-subr-argv.el --- GNU VM calls on evaluated Subr operands -*- lexical-binding: t; -*-

(defconst par-vm-subr-call2 (make-byte-code 771 (unibyte-string 34 135) [] 3))
(defconst par-vm-subr-call1 (make-byte-code 514 (unibyte-string 33 135) [] 2))
(defvar par-vm-subr-special 'outer)

(defun par-vm-subr-callback (x)
  (garbage-collect)
  (list x par-vm-subr-special (backtrace-frame 0 'mapcar)))

(defalias 'par-vm-subr-alias 'string<)
(prin1 (list 'calls (funcall par-vm-subr-call2 'string< "cat" "dog")
             (funcall par-vm-subr-call2 (symbol-function 'string<) "dog" "cat")
             (funcall par-vm-subr-call2 'par-vm-subr-alias "cat" "dog")
             (progn
               (fset 'par-vm-subr-alias (lambda (a b) (list b a)))
               (funcall par-vm-subr-call2 'par-vm-subr-alias "cat" "dog"))))
(terpri)

(prin1 (list 'errors
             (condition-case err (funcall par-vm-subr-call2 'string< "cat" [dog])
               (error err))
             (condition-case err (funcall par-vm-subr-call1 'string< "cat")
               (wrong-number-of-arguments
                ;; The runtimes print the underlying Subr differently.
                (let ((operator (nth 1 err)))
                  (list (car err)
                        (if (subrp operator) (intern (subr-name operator)) operator)
                        (nth 2 err)))))))
(terpri)

(let ((trace nil))
  (prin1
   (list 'unwind
         (condition-case err
             (let ((par-vm-subr-special 'inner))
               (unwind-protect
                   (funcall par-vm-subr-call2 'mapcar #'par-vm-subr-callback '(one two))
                 (push par-vm-subr-special trace)))
           (error err))
         trace par-vm-subr-special)))
(terpri)

;; Exercise both the live Subr cell and a held callable.
(prin1 (list 'direct
             (funcall par-vm-subr-call2 'assoc-string "B" '("A" "B"))
             (funcall par-vm-subr-call2 (symbol-function 'assoc-string)
                      "C" '("A" "B"))))
(terpri)

;; The callback's signal must preserve the active Subr frame.
(defun par-vm-subr-raising-callback (x)
  (signal 'error (list x par-vm-subr-special (backtrace-frame 0 'mapcar))))

(let ((trace nil))
  (prin1
   (list 'caught
         (condition-case err
             (let ((par-vm-subr-special 'inner))
               (unwind-protect
                   (funcall par-vm-subr-call2 'mapcar #'par-vm-subr-raising-callback '(one))
                 (push par-vm-subr-special trace)))
           (error err))
         trace par-vm-subr-special)))
(terpri)
