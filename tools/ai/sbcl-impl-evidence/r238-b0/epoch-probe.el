;;; epoch-probe.el --- Doc 238 B0 function-epoch probe  -*- lexical-binding: t; -*-
;; Run on a NeLisp image built with the B0 patch, e.g.
;;   (ulimit -v 8388608; ./target/nelisp-standalone-eval --load epoch-probe.el)
;; Prints OK/FAIL per operation: the epoch must strictly increase.
(defvar epoch-probe--fails 0)
(defun epoch-probe--check (name thunk)
  (let* ((before (nelisp--function-epoch))
         (err (condition-case e (progn (funcall thunk) nil) (error e)))
         (after (nelisp--function-epoch)))
    (cond (err (setq epoch-probe--fails (1+ epoch-probe--fails))
               (princ (format "FAIL %s: error %S\n" name err)))
          ((> after before) (princ (format "OK   %s: %d -> %d\n" name before after)))
          (t (setq epoch-probe--fails (1+ epoch-probe--fails))
             (princ (format "FAIL %s: epoch unchanged (%d)\n" name before))))))
(defun epoch-probe--skip (name why) (princ (format "SKIP %s: %s\n" name why)))

(defun epoch-probe--target (x) (+ x 1))
(let ((a (nelisp--function-epoch)) (b (nelisp--function-epoch)))
  (princ (if (= a b) "OK   read-only read is stable\n"
           (setq epoch-probe--fails (1+ epoch-probe--fails)) "FAIL epoch moved between two reads\n")))
(epoch-probe--check "fset" (lambda () (fset 'epoch-probe--f1 (lambda (x) x))))
(epoch-probe--check "fset-redefine" (lambda () (fset 'epoch-probe--f1 (lambda (x) (+ x 1)))))
(epoch-probe--check "defalias" (lambda () (defalias 'epoch-probe--f2 'epoch-probe--target)))
(epoch-probe--check "defun" (lambda () (eval '(defun epoch-probe--f3 (x) x) t)))
(epoch-probe--check "fmakunbound" (lambda () (fmakunbound 'epoch-probe--f1)))
(if (fboundp 'advice-add)
    (progn
      (epoch-probe--check "advice-add"
        (lambda () (advice-add 'epoch-probe--target :around (lambda (f &rest a) (apply f a)) '((name . ep)))))
      (epoch-probe--check "advice-remove" (lambda () (advice-remove 'epoch-probe--target 'ep))))
  (epoch-probe--skip "advice-add/remove" "advice-add not fboundp"))
(let ((file (make-temp-file "epoch-probe-load" nil ".el")))
  (unwind-protect
      (progn
        (with-temp-file file (insert "(defun epoch-probe--f3 (x) (* x 2))\n"))
        (epoch-probe--check "load-redefine" (lambda () (load file nil t))))
    (delete-file file)))
(if (and (fboundp 'debug-on-entry) (fboundp 'cancel-debug-on-entry))
    (progn
      (epoch-probe--check "debug-on-entry" (lambda () (debug-on-entry 'epoch-probe--f3)))
      (epoch-probe--check "cancel-debug-on-entry" (lambda () (cancel-debug-on-entry 'epoch-probe--f3))))
  (epoch-probe--skip "debug-on-entry" "not fboundp"))
(epoch-probe--skip "image-restore" "run this probe again from a cold-loaded image; first epoch read must be >> 0 (restore adds monotonic ns)")
(princ (format "epoch at end: %d\n" (nelisp--function-epoch)))
(princ (if (= epoch-probe--fails 0) "PROBE OK\n" (format "PROBE FAIL (%d)\n" epoch-probe--fails)))
