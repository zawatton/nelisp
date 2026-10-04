;;; nelisp-catch-target-regression.el --- Catch target lifetime parity -*- lexical-binding: t; -*-

;; Run the same source on GNU Emacs and the standalone reader.  Missing
;; targets must signal at the throw site, before condition-case unwinds.
(dolist
    (form
     '((condition-case err (throw 'missing 7) (no-catch err))
       (catch 'outer
         (condition-case err (throw 'missing 9) (error err)))
       (condition-case err (catch nil (throw nil 'nil-tag)) (no-catch err))
       (catch t (throw t 'true-tag))
       (let ((tag (list 'identity)))
         (catch tag (garbage-collect) (throw tag 'rooted-tag)))
       (let ((tag (copy-sequence "identity")))
         (catch tag (garbage-collect) (throw tag 'rooted-string)))
       (let ((tag (copy-sequence "identity")))
         (catch tag
           (condition-case err (throw (copy-sequence tag) 5)
             (no-catch (list (car err) (cadr err) (caddr err))))))
       (catch 'outer (catch 'inner (throw 'outer 11)) 'unreachable)
       (let ((cleanup nil))
         (list (catch 'outer
                 (unwind-protect (throw 'outer 12)
                   (setq cleanup
                         (condition-case err (throw 'missing 13)
                           (no-catch err)))))
               cleanup))
       (progn (catch 'completed 1)
              (condition-case err (throw 'completed 14) (no-catch err)))
       (progn (condition-case nil (catch 'aborted (error "abort"))
                (error nil))
              (condition-case err (throw 'aborted 15) (no-catch err)))
       (condition-case err (catch (throw 'not-active 16) 1) (no-catch err))
       (catch 'outer
         (catch (throw 'outer 17) 'unreachable))
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
               count))
       (condition-case err (throw) (error err))
       (condition-case err (throw 'sample) (error err))
       (condition-case err (throw 'sample 1 2) (error err))
       (condition-case err (eval '(throw . bad)) (error err))
       (condition-case err (eval '(throw 'sample . bad)) (error err))
       (condition-case err (catch) (error err))
       (condition-case err (eval '(catch . bad)) (error err))
       (let ((count 0))
         (list (condition-case err (throw (setq count (1+ count))) (error err))
               count))
       (let ((count 0))
         (list (condition-case err
                   (eval '(catch (progn (setq count (1+ count)) 'sample) . bad)
                         (list (cons 'count count)))
                 (error err))
               count))
       (catch 'empty)
       (condition-case err (throw unbound-tag 1 2) (error err))))
  (prin1 (eval form))
  (terpri))

;;; nelisp-catch-target-regression.el ends here
