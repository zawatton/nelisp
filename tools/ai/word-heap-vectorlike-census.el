;;; word-heap-vectorlike-census.el --- Retained S4 roots -*- lexical-binding: t; -*-

;; The launcher resolves existing ELF symbols.  The pinned physical view is
;; only root discovery at an existing FFI boundary; the census reads the
;; canonical child WORDs and their actual allocated blocks independently.
(defvar whv--keep (make-vector 48 nil))
(dotimes (i 16)
  (aset whv--keep (* i 3) (make-vector 1 nil))
  (aset whv--keep (+ (* i 3) 1) (make-record 'whv 1 nil))
  (aset whv--keep (+ (* i 3) 2) (make-char-table 'whv nil)))
(nelisp--debug-switch 28)
(garbage-collect)
(garbage-collect)
(defvar whv--env (nelisp--native-env))
(defvar whv--ticket
  (ptr-call (string-to-number (getenv "WHV_PIN_BEGIN")) whv--env 0 0 0 0 0))
(unless (> whv--ticket 0) (error "Cannot pin census root"))
(defvar whv--view (nelisp--native-pin-copy whv--env whv--ticket whv--keep))
(dotimes (i 48)
  (princ (format "WHV-ROOT %d %d\n" i
                 (ptr-call (string-to-number (getenv "WHV_VECTOR_SLOT"))
                           whv--view i 0 0 0 0))))
(princ "WHV-CENSUS-READY\n")
nil
