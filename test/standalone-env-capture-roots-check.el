;;; standalone-env-capture-roots-check.el --- Env root capture -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;; `nelisp--env-capture-roots' returns the five heap-image roots
;; (globals frames unbound-marker max-recursion use-elisp-apply).  Since the
;; S4.2 frame stack, the frames root is a heap snapshot of the live
;; activation records.  A scope error in that capture compiled its result
;; variable as a dynamic lookup through uninitialized hidden slots, so every
;; call died with SIGSEGV (final2).  The shape checks hold for both the old
;; live frames record and the snapshot, so they do not pin its slot count.

(unless (fboundp 'nelisp--env-capture-roots)
  (error "nelisp--env-capture-roots is not available"))

(defvar env-capture-check-count 0)

(defun env-capture-check (ok what)
  (unless ok (error "env capture: %s" what))
  (setq env-capture-check-count (1+ env-capture-check-count)))

(defun env-capture-check-shape (roots)
  (env-capture-check (= (length roots) 5) "root count")
  (let ((globals (nth 0 roots)) (frames (nth 1 roots)))
    (env-capture-check (symbolp (aref globals 0)) "globals type WORD")
    (env-capture-check (symbolp (aref frames 0)) "frames type WORD")
    (env-capture-check (vectorp (aref frames 1)) "frames backing vector")
    (env-capture-check (and (integerp (aref frames 2)) (>= (aref frames 2) 0)
                            (<= (aref frames 2) (length (aref frames 1))))
                       "frames depth within its backing"))
  (env-capture-check (symbolp (nth 2 roots)) "unbound marker")
  (env-capture-check (integerp (nth 3 roots)) "max recursion")
  (env-capture-check (integerp (nth 4 roots)) "use-elisp-apply"))

(defun env-capture-check-signature (roots)
  (list (mapcar #'type-of roots) (nth 3 roots) (nth 4 roots)))

(defun env-capture-check-nested (n)
  (if (= n 0) (nelisp--env-capture-roots)
    (let ((next (1- n))) (car (list (env-capture-check-nested next))))))

;; The original minimal reproducer: the call itself must return.
(env-capture-check (= (progn (nelisp--env-capture-roots) 1) 1) "plain call")

(let* ((first (nelisp--env-capture-roots))
       (signature (env-capture-check-signature first)))
  (env-capture-check-shape first)
  ;; Allocate and collect while the first capture is only reachable from
  ;; this frame; it must stay intact, and a later capture must agree.
  (let ((junk nil) (i 0))
    (while (< i 2000) (setq junk (cons (make-string 8 ?x) junk) i (1+ i)))
    (garbage-collect)
    (env-capture-check (= (length junk) 2000) "allocation"))
  (env-capture-check-shape first)
  (env-capture-check (equal (env-capture-check-signature first) signature)
                     "first capture changed across GC")
  (let ((second (nelisp--env-capture-roots)))
    (env-capture-check-shape second)
    (env-capture-check (equal (env-capture-check-signature second) signature)
                       "repeated capture shape")
    (env-capture-check (not (eq first second)) "fresh result list"))
  ;; Other entries: funcall/apply dispatch and a capture under live
  ;; nested activations.
  (dolist (roots (list (funcall #'nelisp--env-capture-roots)
                       (apply #'nelisp--env-capture-roots nil)
                       (env-capture-check-nested 4)))
    (env-capture-check-shape roots)
    (env-capture-check (equal (env-capture-check-signature roots) signature)
                       "capture through another entry")))

(princ (format "ENV-CAPTURE-ROOTS checked=%d PASS\n" env-capture-check-count))
nil
