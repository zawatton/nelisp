(throw
 (catch 'sample (throw 'sample 42))
 (catch 'sample (funcall #'throw 'sample 42))
 (catch 'sample (apply #'throw '(sample 42)))
 (catch nil (funcall #'throw nil 'nil-tag))
 (let ((tag (list 'identity)))
   (catch tag (funcall #'throw tag 'identity-tag)))
 (let ((cleaned nil))
   (list (catch 'sample
           (unwind-protect (funcall #'throw 'sample 'value)
             (setq cleaned t)))
         cleaned))
 (catch 'outer
   (catch 'inner (funcall #'throw 'outer 'outer-value))
   'unreachable)
 (condition-case err (funcall #'throw 'missing 'value) (error err))
 (condition-case err (funcall #'throw)
   (error (list (car err) (eq (cadr err) (symbol-function 'throw)) (caddr err))))
 (condition-case err (funcall #'throw 'sample)
   (error (list (car err) (eq (cadr err) (symbol-function 'throw)) (caddr err))))
 (condition-case err (funcall #'throw 'sample 1 2)
   (error (list (car err) (eq (cadr err) (symbol-function 'throw)) (caddr err)))))

(throw
 (condition-case err (catch nil (throw nil 'nil-tag)) (no-catch err))
 (catch 'outer
   (condition-case err (throw 'missing 9) (no-catch err)))
 (let ((tag (copy-sequence "identity")))
   (catch tag (garbage-collect) (throw tag 'rooted-string)))
 (let ((tag (list 'identity)))
   (catch tag (garbage-collect) (funcall #'throw tag 'rooted-list)))
 (let ((tag (copy-sequence "identity")))
   (catch tag
     (condition-case err (throw (copy-sequence tag) 5) (no-catch err))))
 (let ((cleanup nil))
   (list (catch 'outer
           (unwind-protect (throw 'outer 12)
             (setq cleanup
                   (condition-case err (throw 'missing 13) (no-catch err)))))
         cleanup))
 (progn (catch 'completed 1)
        (condition-case err (throw 'completed 14) (no-catch err)))
 (progn (condition-case nil (catch 'aborted (error "abort")) (error nil))
        (condition-case err (throw 'aborted 15) (no-catch err)))
 (condition-case err (catch (throw 'not-active 16) 1) (no-catch err))
 (catch 'outer (catch (throw 'outer 17) 'unreachable))
 (let ((count 0))
   (list (condition-case err (throw 'missing (setq count (1+ count)))
           (no-catch err))
         count))
 (let ((count 0))
   (list (catch 'outer
           (condition-case nil (throw 'missing 18)
             (no-catch (setq count (1+ count))))
           (garbage-collect)
           (throw 'outer 19))
         count)))

;; Direct special-form arity must signal before evaluating its operands,
;; independently of the callable function-cell bridge above.
(throw
 (condition-case err (throw) (error err))
 (condition-case err (throw 'sample) (error err))
 (condition-case err (throw 'sample 1 2) (error err))
 (let ((count 0))
   (list (condition-case err (throw (setq count (1+ count))) (error err))
         count)))
