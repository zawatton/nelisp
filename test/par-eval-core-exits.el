;;; par-eval-core-exits.el --- Evaluator unwind and handler order -*- lexical-binding: t; -*-

(define-error 'par-eval-parent "Parent")
(define-error 'par-eval-leaf "Leaf" 'par-eval-parent)
(defvar par-eval-bind 'outer)
(defvar par-eval-trace nil)

(defun par-eval-row (tag value)
  (prin1 (list tag value))
  (terpri))

;; eval's argument is quoted so eager expansion and the FX compiler cannot
;; substitute a different implementation of the source special forms.
(par-eval-row
 'source
 (eval '(let ((par-eval-trace nil))
          (list
           (condition-case err
               (let ((par-eval-bind 'first))
                 (unwind-protect
                     (let ((par-eval-bind 'second))
                       (unwind-protect
                           (signal 'par-eval-leaf (list 'body par-eval-bind))
                         (push par-eval-bind par-eval-trace)
                         (garbage-collect)))
                   (push par-eval-bind par-eval-trace)))
             (par-eval-parent (list err par-eval-bind))
             (par-eval-leaf 'wrong-clause))
           par-eval-trace par-eval-bind))
       t))

(par-eval-row
 'replace
 (eval '(let ((par-eval-trace nil)
              (tag (list 'fresh-tag)))
          (list
           (catch tag
             (condition-case err
                 (unwind-protect
                     (signal 'par-eval-leaf '(replace-me))
                   (condition-case local
                       (signal 'par-eval-parent '(nested))
                     (par-eval-parent (push local par-eval-trace)))
                   (garbage-collect)
                   (throw tag (list 'replacement par-eval-trace))
                   (push 'unreachable par-eval-trace))
               (t (list 'wrong-exit err))))
           par-eval-trace))
       t))

(par-eval-row
 'success
 (eval '(condition-case value
            (unwind-protect (list 'kept (make-string 8 ?x))
              (garbage-collect))
          (error 'wrong-exit)
          (:success (garbage-collect) (list :ok value)))
       t))

(par-eval-row
 'signal-from-cleanup
 (eval '(let ((par-eval-trace nil))
          (list
           (condition-case err
               (unwind-protect
                   (unwind-protect 'ok
                     (signal 'par-eval-leaf '(cleanup))
                     (push 'unreachable par-eval-trace))
                 (push 'outer-cleanup par-eval-trace))
             ((par-eval-parent error) err))
           par-eval-trace))
       t))

(par-eval-row
 'no-catch
 (eval '(condition-case err (catch nil (throw nil 29))
          (no-catch err))
       t))
