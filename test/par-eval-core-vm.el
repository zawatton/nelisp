;;; par-eval-core-vm.el --- GNU bytecode handler and specpdl order -*- lexical-binding: t; -*-

(require 'bytecomp)

(define-error 'par-eval-parent "Parent")
(define-error 'par-eval-leaf "Leaf" 'par-eval-parent)
(defvar par-eval-bind 'outer)
(defvar par-eval-trace nil)

(let ((fn
       (byte-compile
        '(lambda (throwp)
           (let ((par-eval-trace nil)
                 (tag (list 'fresh)))
             (list
              (catch tag
                (condition-case err
                    (let ((par-eval-bind 'outer-frame))
                      (unwind-protect
                          (let ((par-eval-bind 'inner-frame))
                            (unwind-protect
                                (if throwp
                                    (throw tag (list 'value par-eval-bind))
                                  (signal 'par-eval-leaf (list 'data par-eval-bind)))
                              (push par-eval-bind par-eval-trace)
                              (garbage-collect)))
                        (push par-eval-bind par-eval-trace)))
                  (par-eval-parent (list err par-eval-bind))
                  (par-eval-leaf 'wrong-handler)))
              par-eval-trace
              par-eval-bind))))))
  (unless (byte-code-function-p fn)
    (error "Expected a byte-code function"))
  (prin1 (list 'vm (funcall fn t) (funcall fn nil)))
  (terpri))
