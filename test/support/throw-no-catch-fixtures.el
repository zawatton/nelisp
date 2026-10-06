;;; throw-no-catch-fixtures.el --- GNU throw-site oracle -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
(defvar throw-nocatch-effects nil)
(defun throw-nocatch-callback (tag)
  (garbage-collect)
  (throw tag 17))
(defun throw-nocatch-fixture-missing ()
  (condition-case e (throw 'missing 5) (no-catch e)))
(defun throw-nocatch-fixture-nil ()
  (condition-case e (catch nil (throw nil 5)) (no-catch e)))
(defun throw-nocatch-fixture-nested ()
  (catch 'outer (catch 'inner (throw 'outer 7))))
(defun throw-nocatch-fixture-funcall ()
  (catch 'outer (funcall #'throw-nocatch-callback 'outer)))
(defun throw-nocatch-fixture-cleanup ()
  (let ((throw-nocatch-effects nil))
    (list (condition-case e
              (unwind-protect (throw 'missing 9)
                (push 'cleanup throw-nocatch-effects)
                (garbage-collect))
            (no-catch e))
          throw-nocatch-effects)))
(defun throw-nocatch-fixture-normal () (catch 'normal 11))
(defun throw-nocatch-fixture-error ()
  (condition-case e (catch 'normal (signal 'arith-error '(12)))
    (arith-error e)))
(defun throw-nocatch-fixture-cross () (throw 'outer 19))
(defun throw-nocatch-fixture-replacement ()
  (catch 'outer
    (condition-case e
        (catch 'inner
          (unwind-protect (throw 'inner 1)
            (throw 'inner 2)))
      (no-catch (throw 'outer e)))))
(defun throw-nocatch-fixture-condition-pop ()
  (catch 'outer
    (condition-case e (catch 'inner (signal 'arith-error nil))
      (arith-error (throw 'outer (car e))))))
(defun throw-nocatch-fixture-same-tag ()
  (catch 'same (catch 'same (throw 'same 21))))
(defun throw-nocatch-fixture-nil-outer ()
  (catch 'outer (catch nil (throw 'outer 22))))
(defun throw-nocatch-fixture-pop ()
  (progn (catch 'stale 1)
         (condition-case e (throw 'stale 2) (no-catch e))))
(defun throw-nocatch-fixture-crossed ()
  (condition-case e
      (catch 'outer
        (unwind-protect (catch 'inner (throw 'outer 1)) (throw 'inner 2)))
    (no-catch e)))
(defun throw-nocatch-fixture-error-throw ()
  (condition-case e
      (catch 'inner (unwind-protect (signal 'arith-error nil) (throw 'inner 2)))
    (error e)))
(defun throw-nocatch-fixture-throw-signal ()
  (let ((throw-nocatch-effects nil))
    (list
     (catch 'out
       (unwind-protect
           (condition-case nil
               (unwind-protect (throw 'out 1) (signal 'arith-error '(inner)))
             (arith-error (push 'handler throw-nocatch-effects) 2))
         (push 'outer-cleanup throw-nocatch-effects)))
     (reverse throw-nocatch-effects))))
(defun throw-nocatch-fixture-signal-throw ()
  (let ((throw-nocatch-effects nil))
    (list
     (condition-case e
         (unwind-protect
             (catch 'inner
               (unwind-protect (signal 'arith-error nil) (throw 'inner 2)))
           (push 'outer-cleanup throw-nocatch-effects))
       (error e))
     (reverse throw-nocatch-effects))))
(defun throw-nocatch-fixture-vector ()
  (let ((a (vector 1)) (b (vector 2)))
    (catch a (condition-case nil (throw b 3) (no-catch 'caught)))))
(defun throw-nocatch-fixture-vector-same ()
  (let ((a (vector 1))) (catch a (garbage-collect) (throw a 3))))
(defun throw-nocatch-fixture-loop ()
  (let ((n 65540))
    (while (> n 0)
      (catch 'loop
        (setq n (1- n))
        (when (= (% n 4096) 0) (garbage-collect))
        (throw 'loop n)))
    n))
(defun throw-nocatch-observe (phase)
  (dolist (name '(missing nil nested funcall cleanup normal error replacement
                         condition-pop same-tag nil-outer pop crossed error-throw
                         throw-signal signal-throw vector vector-same))
    (princ (format "THROW-OBS %s %S %S\n" phase name
                   (funcall (intern (concat "throw-nocatch-fixture-"
                                            (symbol-name name)))))))
  ;; Evaluator catch -> VM throw after VM functions have been installed.
  (princ (format "THROW-OBS %s cross %S\n" phase
                 (catch 'outer (throw-nocatch-fixture-cross))))
  (princ (format "THROW-COMPLETE %s\n" phase)))
(provide 'throw-no-catch-fixtures)
