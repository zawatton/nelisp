;;; nelisp-region-test.el --- host tests for `nelisp-with-region' -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
(require 'ert)
(require 'cl-lib)
(require 'nelisp-region)

(defvar nelisp-region-test--table nil "Fake slots: vector of (GEN LIVE).")
(defvar nelisp-region-test--log nil "Log of primitive calls, newest first.")

(defun nelisp-region-test--valid (h)
  (and (integerp h)
       (let ((s (aref nelisp-region-test--table (logand h 63))))
         (and (cadr s) (= (car s) (ash h -6))))))

(defmacro nelisp-region-test--with-fake (&rest body)
  "Run BODY with generation-checked fake region primitives."
  `(let ((nelisp-region-test--table (make-vector 64 (list 0 nil)))
         (nelisp-region-test--log nil))
     (cl-letf (((symbol-function 'nelisp--region-open)
                (lambda (n)
                  (push (list 'open n) nelisp-region-test--log)
                  (let ((i 0))
                    (while (cadr (aref nelisp-region-test--table i)) (setq i (1+ i)))
                    (let ((g (car (aref nelisp-region-test--table i))))
                      (aset nelisp-region-test--table i (list g t))
                      (logior (ash g 6) i)))))
               ((symbol-function 'nelisp--region-live-p)
                (lambda (h) (and (nelisp-region-test--valid h) t)))
               ((symbol-function 'nelisp--region-close)
                (lambda (h)
                  (push (list 'close h) nelisp-region-test--log)
                  (unless (nelisp-region-test--valid h)
                    (signal 'wrong-type-argument (list 'nelisp-region-live-p h)))
                  (let ((i (logand h 63)))
                    (aset nelisp-region-test--table i (list (1+ (car (aref nelisp-region-test--table i))) nil)))
                  t))
               ((symbol-function 'nelisp--region-move)
                (lambda (h)
                  (unless (nelisp-region-test--valid h)
                    (signal 'wrong-type-argument (list 'nelisp-region-live-p h)))
                  (let* ((i (logand h 63)) (g (1+ (car (aref nelisp-region-test--table i)))))
                    (aset nelisp-region-test--table i (list g t))
                    (logior (ash g 6) i)))))
       ,@body)))

(defun nelisp-region-test--open-count ()
  (cl-count-if #'cadr nelisp-region-test--table))

(ert-deftest nelisp-region-normal-exit-closes ()
  (nelisp-region-test--with-fake
   (should (eq 'done (nelisp-with-region (r 4096) 'done)))
   (should (= 0 (nelisp-region-test--open-count)))
   (should (= 1 (cl-count 'close nelisp-region-test--log :key #'car)))))

(ert-deftest nelisp-region-throw-closes ()
  (nelisp-region-test--with-fake
   (should (eq 'thrown (catch 'tag (nelisp-with-region (r 8) (throw 'tag 'thrown)))))
   (should (= 0 (nelisp-region-test--open-count)))))

(ert-deftest nelisp-region-signal-closes-and-propagates ()
  (nelisp-region-test--with-fake
   (should (equal '(error "boom")
                  (condition-case e (nelisp-with-region (r 8) (error "boom")) (error e))))
   (should (= 0 (nelisp-region-test--open-count)))))

(ert-deftest nelisp-region-moved-region-stays-open ()
  (nelisp-region-test--with-fake
   (let (new old)
     (nelisp-with-region (r 8)
       (setq old r)
       (setq new (nelisp--region-move r)))
     (should-not (nelisp--region-live-p old))
     (should (nelisp--region-live-p new))
     (should (= 1 (nelisp-region-test--open-count)))
     (should (= 0 (cl-count 'close nelisp-region-test--log :key #'car)))
     (should-error (nelisp--region-close old) :type 'wrong-type-argument)
     (nelisp--region-close new)
     (should (= 0 (nelisp-region-test--open-count))))))

(ert-deftest nelisp-region-closed-by-body-not-double-closed ()
  (nelisp-region-test--with-fake
   (nelisp-with-region (r 8) (nelisp--region-close r))
   (should (= 1 (cl-count 'close nelisp-region-test--log :key #'car)))
   (should (= 0 (nelisp-region-test--open-count)))))

(ert-deftest nelisp-region-bytes-evaluated-once ()
  (nelisp-region-test--with-fake
   (let ((n 0))
     (nelisp-with-region (r (progn (setq n (1+ n)) 64)) nil)
     (should (= 1 n))
     (should (equal '(open 64) (car (last nelisp-region-test--log)))))))

(ert-deftest nelisp-region-var-not-captured-by-cleanup ()
  "A body variable named like internal helpers does not disturb cleanup."
  (nelisp-region-test--with-fake
   (let ((unwind-protect 1) (progn 2))
     (nelisp-with-region (x 8) (list unwind-protect progn)))
   (should (= 0 (nelisp-region-test--open-count)))))

(provide 'nelisp-region-test)
;;; nelisp-region-test.el ends here
