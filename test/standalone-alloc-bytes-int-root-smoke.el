;;; standalone-alloc-bytes-int-root-smoke.el --- int-held buffers survive GC -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; `alloc-bytes' hands Lisp a buffer as a plain integer address.  A
;; collection that runs while that integer is still live -- mid-form, at a
;; `while' back-edge, or through `garbage-collect' -- must not free the
;; buffer.  Before the fix the collector never treated an integer as a
;; reference, so every buffer below was swept and its first word overwritten
;; with the free-list link: 200 of 200 corrupted (measured 2026-11-02).  The
;; same defect made `nl-ffi-loader--cstring's path buffer lose its first
;; eight bytes before `openat', so a real file intermittently read as ENOENT.
;;
;; The markers stay inside the fixnum range: a bignum marker would never
;; compare equal after `ptr-read-u64' and fail for the wrong reason.
;;
;; Prints "alloc-bytes-int-root: PASS ..." or signals.

;;; Code:

(let ((bufs nil) (i 0) (bad 0) (n 200))
  (while (< i n)
    (let ((b (alloc-bytes 48 8)))
      (ptr-write-u64 b 0 (+ 1229782938247303441 i))
      (ptr-write-u64 b 8 (+ 81985529216486895 i))
      (setq bufs (cons (cons i b) bufs)))
    (setq i (1+ i)))
  (garbage-collect)
  ;; Allocate the same size again so a wrongly freed block is also reused.
  (let ((j 0)) (while (< j n) (alloc-bytes 48 8) (setq j (1+ j))))
  (dolist (e bufs)
    (unless (and (= (ptr-read-u64 (cdr e) 0) (+ 1229782938247303441 (car e)))
                 (= (ptr-read-u64 (cdr e) 8) (+ 81985529216486895 (car e))))
      (setq bad (1+ bad))))
  (if (= bad 0)
      (princ (format "alloc-bytes-int-root: PASS (%d buffers)\n" n))
    (error "alloc-bytes-int-root: %d of %d int-held buffers corrupted" bad n)))

;;; standalone-alloc-bytes-int-root-smoke.el ends here
