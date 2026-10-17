;;; nelisp-bt-value-smoke.el --- Doc 214 S3 closures and values on threads -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 214 S3 acceptance.  Worker ID 3 runs `(funcall CLOSURE)' on a real
;; OS thread and keeps its full result.  The joiner copies the value, or
;; the error TAG/VAL, out of the worker with `nelisp-thread-worker-sexp'.
;; Checks:
;;   - a closure over a lexical variable returns a list holding a string,
;;     a vector and a symbol;
;;   - another returns a fresh string;
;;   - a third signals `nl-bt-smoke-error' with data (1 2), which the
;;     joiner re-signals and catches with the same data.
;; The values must survive the section's end and a full collection, now
;; rooted by the joiner's own bindings.  Prints BT-VALUE-PASS and
;; BT-ERROR-PASS.

;;; Code:

(define-error 'nl-bt-smoke-error "nl-bt smoke error")

(defun nl-bt-value-smoke--form (fn)
  "Return the worker form that calls FN."
  `(let ((nelisp--last-signal-data nil))
     (funcall ',fn)))

(defun nl-bt-value-smoke--result (slot)
  "Return (STATUS . HANDLE) published by an ID 3 worker in SLOT."
  (cons (nelisp-thread-atomic-read slot)
        (nelisp-thread-atomic-read (+ slot 8))))

(if (not (member system-configuration
                 '("x86_64-pc-linux-gnu" "x86_64-pc-windows-msvc")))
    (princ "BT-VALUE-SKIP no native threads on this target\n")
  (let* ((x 40)
         (f-list (lambda () (list (+ x 2) "two" (vector 3) 'four)))
         (f-string (lambda () (concat "ab" (make-string 2 ?c))))
         (f-error (lambda () (signal 'nl-bt-smoke-error (list 1 2))))
         (shared (nelisp-thread-shared-alloc 64))
         (r0 shared) (r1 (+ shared 16)) (r2 (+ shared 32)) (done (+ shared 48))
         (section nil) v0 v1 err)
    (unwind-protect
        (progn
          (unless (= (nelisp-thread-gc-inhibit 1) 1)
            (error "parallel section did not begin"))
          (setq section t)
          (dolist (job (list (cons f-list r0) (cons f-string r1) (cons f-error r2)))
            (unless (> (nelisp-thread-spawn
                        3 0 (nl-bt-value-smoke--form (car job)) (cdr job) done)
                       0)
              (error "worker spawn failed")))
          (nelisp-thread-join done 3)
          (let ((a (nl-bt-value-smoke--result r0))
                (b (nl-bt-value-smoke--result r1))
                (c (nl-bt-value-smoke--result r2)))
            (unless (and (= (car a) 1) (= (car b) 1) (= (car c) 2))
              (error "unexpected worker status %S %S %S" (car a) (car b) (car c)))
            (setq v0 (nelisp-thread-worker-sexp (cdr a) 128))
            (setq v1 (nelisp-thread-worker-sexp (cdr b) 128))
            (setq err (cons (nelisp-thread-worker-sexp (cdr c) 2072)
                            (nelisp-thread-worker-sexp (cdr c) 2104))))
          (nelisp-thread-gc-inhibit 0)
          (setq section nil))
      (when section (nelisp-thread-gc-inhibit 0)))
    (garbage-collect)
    (princ (format "BT-VALUE values %S %S\n" v0 v1))
    (if (and (equal v0 '(42 "two" [3] four)) (equal v1 "abcc"))
        (princ "BT-VALUE-PASS\n")
      (princ "BT-VALUE-FAIL\n"))
    (let ((caught (condition-case e
                      (signal (car err) (cdr err))
                    (nl-bt-smoke-error e))))
      (princ (format "BT-ERROR caught %S\n" caught))
      (if (equal caught '(nl-bt-smoke-error 1 2))
          (princ "BT-ERROR-PASS\n")
        (princ "BT-ERROR-FAIL\n")))))

;;; nelisp-bt-value-smoke.el ends here
