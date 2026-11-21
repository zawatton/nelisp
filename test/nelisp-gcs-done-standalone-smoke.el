;;; nelisp-gcs-done-standalone-smoke.el --- GNU gcs-done GC counter -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; The standalone runtime exposes GNU's `gcs-done' (collections completed so
;; far) as a forwarded integer variable.  This smoke checks that it is bound
;; to an integer, that an explicit `garbage-collect' advances it by at least
;; one, that an allocation-heavy loop with no explicit collection advances it
;; too, and that a non-integer `setq' is refused as in GNU.
;;
;; Run from the repository root:
;;
;;   ./target/nelisp --load test/nelisp-gcs-done-standalone-smoke.el
;;
;; Prints a GATE-COUNT line and `nelisp-gcs-done-standalone-smoke: PASS'; any
;; failure signals an error so the process exits non-zero.

;;; Code:

(defvar nelisp-gcs-done-smoke--checked 0)

(defun nelisp-gcs-done-smoke--check (label ok)
  (setq nelisp-gcs-done-smoke--checked (1+ nelisp-gcs-done-smoke--checked))
  (unless ok
    (princ (format "nelisp-gcs-done-standalone-smoke: FAIL %s\n" label))
    (error "nelisp-gcs-done-standalone-smoke: FAIL %s" label)))

(nelisp-gcs-done-smoke--check "gcs-done bound" (boundp 'gcs-done))
(nelisp-gcs-done-smoke--check "gcs-done integer" (integerp gcs-done))

(let ((before gcs-done))
  (garbage-collect)
  (nelisp-gcs-done-smoke--check "explicit garbage-collect advances gcs-done"
                                (>= (- gcs-done before) 1)))

(let ((before gcs-done) (i 0) (acc nil))
  (while (< i 3000000)
    (setq acc (cons (make-string 64 ?x) nil))
    (setq i (1+ i)))
  (nelisp-gcs-done-smoke--check "allocation pressure advances gcs-done"
                                (>= (- gcs-done before) 1)))

(nelisp-gcs-done-smoke--check
 "non-integer setq is refused"
 (eq 'wrong-type-argument
     (condition-case e (progn (setq gcs-done 'x) 'no-error) (error (car e)))))

(princ (format "GATE-COUNT checked=%d findings=0\n"
               nelisp-gcs-done-smoke--checked))
(princ "nelisp-gcs-done-standalone-smoke: PASS\n")

;;; nelisp-gcs-done-standalone-smoke.el ends here
