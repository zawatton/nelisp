;;; standalone-native-load-slot-pin-smoke.el --- strings boxed into raw slots survive GC  -*- lexical-binding: t; -*-

;;; Commentary:

;; `nelisp-native-load-box' materializes a string with the runtime's
;; `nl_alloc_str' and leaves the only reference to it in the payload word
;; of a raw Sexp slot.  The GC does not scan raw buffers, so before
;; `nelisp-native-load--slot-pins' a mid-form collection at a `while'
;; back-edge (or `garbage-collect') freed the string while the slot still
;; pointed at it: on the unfixed loader about 184 of 200 strings came back
;; wrong with no explicit GC at all, and 200 of 200 after one.
;; Run by `make standalone-reader-native-load-slot-pin-smoke', which loads
;; lisp/nelisp-native-load.el first.

;;; Code:

(defun native-load-slot-pin--name (i)
  (format "slot-pin-%04d-abcdefghijklmnopqrstuvwxyz" i))

(defun native-load-slot-pin--box-all (n)
  (let ((i 0) (cells nil))
    (while (< i n)
      (let ((cell (alloc-bytes 32 8)))
        (nelisp-native-load-box cell (native-load-slot-pin--name i))
        (setq cells (cons cell cells)))
      (setq i (1+ i)))
    (nreverse cells)))

(defun native-load-slot-pin--bad (cells)
  (let ((i 0) (bad 0))
    (while cells
      (unless (equal (condition-case nil
                         (nelisp-native-load-unbox (car cells))
                       (error :error))
                     (native-load-slot-pin--name i))
        (setq bad (1+ bad)))
      (setq cells (cdr cells) i (1+ i)))
    bad))

(defun native-load-slot-pin--churn (n)
  (let ((i 0) (keep nil))
    (while (< i n)
      (let ((cell (alloc-bytes 32 8)))
        (nelisp-native-load-box cell "zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz")
        (setq keep (cons cell keep)))
      (setq i (1+ i)))
    (length keep)))

(let* ((n 200)
       (first (native-load-slot-pin--box-all n))
       (first-bad (native-load-slot-pin--bad first))
       (second (native-load-slot-pin--box-all n))
       (_ (garbage-collect))
       (_ (native-load-slot-pin--churn (* 4 n)))
       (second-bad (native-load-slot-pin--bad second)))
  (if (= 0 (+ first-bad second-bad))
      (princ (format "native-load-slot-pin: PASS (%d strings)\n" n))
    (signal 'error
            (list (format "native-load-slot-pin: FAIL %d/%d wrong under mid-form GC, %d/%d after garbage-collect"
                          first-bad n second-bad n)))))

;;; standalone-native-load-slot-pin-smoke.el ends here
