;;; standalone-condition-case-list-smoke.el --- Condition list matching -*- lexical-binding: t; -*-

(let ((wanted '(wrong-type-argument number-or-marker-p "x"))
      (cases 0))
  (unless (equal
           (condition-case err
               (funcall (symbol-function '1+) "x")
             ((error quit) err))
           wanted)
    (error "combined error/quit selector lost wrong-type-argument"))
  (setq cases (1+ cases))
  (unless (equal
           (condition-case err
               (signal 'wrong-type-argument '(marker))
             ((error) err))
           '(wrong-type-argument marker))
    (error "singleton parent selector failed"))
  (setq cases (1+ cases))
  (unless (equal
           (condition-case err
               (signal 'wrong-type-argument '(marker))
             ((wrong-type-argument) err))
           '(wrong-type-argument marker))
    (error "singleton exact selector failed"))
  (setq cases (1+ cases))
  (unless (equal
           (condition-case err
               (signal 'quit nil)
             ((quit) err))
           '(quit))
    (error "quit selector failed"))
  (setq cases (1+ cases))
  (unless (equal
           (condition-case err
               (signal 'quit nil)
             ((t) err))
           '(quit))
    (error "t selector failed for quit"))
  (setq cases (1+ cases))
  (unless (equal
           (condition-case err
               (condition-case inner
                   (signal 'wrong-type-argument '(nested marker))
                 ((quit) 'wrong-clause))
             ((error quit) err))
           '(wrong-type-argument nested marker))
    (error "nested handler did not preserve error data"))
  (setq cases (1+ cases))
  (unless (equal
           (condition-case err
               (signal 'wrong-type-argument '(ordered))
             ((quit) 'wrong-first-clause)
             ((error) err))
           '(wrong-type-argument ordered))
    (error "nonmatching first clause prevented later parent match"))
  (setq cases (1+ cases))
  (put 'leaf-test-child 'error-conditions
       '(leaf-test-child leaf-test-parent error))
  (unless (equal
           (condition-case err
               (signal 'leaf-test-child '(hierarchy))
             ((leaf-test-parent quit) err))
           '(leaf-test-child hierarchy))
    (error "parent condition in selector list failed"))
  (setq cases (1+ cases))
  (unless (= cases 8) (error "wrong case count: %S" cases))
  (princ (format "CONDITION-CASE-LIST-SMOKE-PASS %d\n" cases)))
