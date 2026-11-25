;;; par-eval-core2-specpdl.el --- Reused VM handlers and cleanup records -*- lexical-binding: t; -*-

(require 'bytecomp)

(define-error 'par-core2-leaf "Leaf")
(defvar par-core2-binding 'outside)
(defvar par-core2-trace nil)

(let ((fn
       (byte-compile
        '(lambda (n)
           (let ((par-core2-trace nil)
                 (out nil))
             (dotimes (i n)
               (let ((tag (list 'heap-tag i)))
                 (push
                  (catch tag
                    (condition-case e
                        (let ((par-core2-binding i))
                          (unwind-protect
                              (catch tag
                                (unwind-protect
                                    (if (zerop (% i 2))
                                        (throw tag 'abandoned)
                                      (signal 'par-core2-leaf (list i)))
                                  (if (zerop (% i 2))
                                      (signal 'par-core2-leaf (list i))
                                    (push (list 'inner par-core2-binding)
                                          par-core2-trace))))
                            (garbage-collect)
                            (push (list 'outer par-core2-binding)
                                  par-core2-trace)))
                      (par-core2-leaf
                       (throw tag (list e par-core2-binding)))))
                  out)))
             (list (nreverse out) (nreverse par-core2-trace)))))))
  (unless (byte-code-function-p fn) (error "Expected bytecode"))
  (prin1 (cons 'reuse (funcall fn 6)))
  (terpri))

;; More pending cleanups than fit in the VM's in-place specpdl. Both
;; the spilled and the remaining in-place records must run on the exit.
(let ((body '(signal 'par-core2-leaf '(payload)))
      (par-core2-trace nil))
  (dotimes (i 30)
    (setq body `(unwind-protect ,body
                  (push ,i par-core2-trace))))
  (let* ((fn (byte-compile `(lambda () (condition-case e ,body
                                       (par-core2-leaf e)))))
         (result (funcall fn)))
    (unless (byte-code-function-p fn) (error "Expected bytecode"))
    (prin1 (list 'spill result par-core2-trace par-core2-binding))
    (terpri)))
