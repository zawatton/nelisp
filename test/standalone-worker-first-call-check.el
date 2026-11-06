;;; standalone-worker-first-call-check.el -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
;; The first form of a fresh process: the environment-keyed prelude caches are
;; already current, so a registry worker's first closure apply never writes an
;; existing global (the mirror-mutation guard refuses that and the worker would
;; publish the -1001 failure sentinel).  Nothing may run before these forms.
(unless (eq (car nelisp--fx-env-disabled) process-environment)
  (error "nelisp--fx-env-disabled is stale at process start"))
(unless (eq (car nelisp--fx-env-trace) process-environment)
  (error "nelisp--fx-env-trace is stale at process start"))
(let* ((shared (nelisp-thread-shared-alloc 64))
       (result (+ shared 8)) (done (+ shared 32)))
  (unless (> shared 0) (error "worker allocation failed"))
  (unless (> (nelisp-thread-spawn 2 0 '(funcall (lambda (x) x) 3) result done) 0)
    (error "worker did not spawn"))
  (nelisp-thread-join done 1)
  (unless (= (nelisp-thread-atomic-read result) 3)
    (error "first worker closure apply returned %S"
           (nelisp-thread-atomic-read result))))
(princ "WORKER-FIRST-CALL checked=3 PASS\n")
