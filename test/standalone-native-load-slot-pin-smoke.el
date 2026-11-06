;;; standalone-native-load-slot-pin-smoke.el --- strings boxed into raw slots survive GC  -*- lexical-binding: t; -*-

;;; Commentary:

;; `nelisp-native-load-box' materializes a string with the runtime's
;; `nl_alloc_str' and leaves the only reference to it in the payload word
;; of a raw Sexp slot.  The GC does not scan raw buffers, so before
;; `nelisp-native-load--slot-pins' a mid-form collection at a `while'
;; back-edge (or `garbage-collect') freed the string while the slot still
;; pointed at it: on the unfixed loader about 184 of 200 strings came back
;; wrong with no explicit GC at all, and 200 of 200 after one.
;; `nelisp-native-load--make-scratch-vector' has the same gap: the loader's
;; slots are mmap'd, outside the arena, so the scratch vector's box was
;; reclaimed (49 of 50 on a3f79db87) until it was pinned the same way.
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

(defun native-load-slot-pin--scratch-bad ()
  "Scratch vectors in an mmap page (as the loader's slots are) whose box
no longer reads its capacity after GC and a 32-byte churn."
  (let* ((n 50) (page (nelisp-native-load--mmap 4096 nil)) (k 0) (bad 0))
    (while (< k n)
      (nelisp-native-load--make-scratch-vector (+ page (* 32 k)))
      (setq k (1+ k)))
    (garbage-collect)
    (let ((i 0) (keep nil))
      (while (< i 20000)
        (let ((b (alloc-bytes 32 8)))
          (ptr-write-u64 b 0 99)
          (setq keep (cons b keep)))
        (setq i (1+ i))))
    (setq k 0)
    (while (< k n)
      ;; A heap vector's word 0 is its header: kind in the low byte, the
      ;; element count from bit 8.
      (unless (= (ash (ptr-read-u64 (ptr-read-u64 (+ page (* 32 k)) 8) 0) -8)
                 nelisp-native-load-scratch-slots)
        (setq bad (1+ bad)))
      (setq k (1+ k)))
    bad))

(let* ((n 200)
       (first (native-load-slot-pin--box-all n))
       (first-bad (native-load-slot-pin--bad first))
       (second (native-load-slot-pin--box-all n))
       (_ (garbage-collect))
       (_ (native-load-slot-pin--churn (* 4 n)))
       (second-bad (native-load-slot-pin--bad second))
       (scratch-bad (native-load-slot-pin--scratch-bad)))
  (if (= 0 (+ first-bad second-bad scratch-bad))
      (princ (format "native-load-slot-pin: PASS (%d strings, 50 scratch vectors)\n" n))
    (signal 'error
            (list (format "native-load-slot-pin: FAIL %d/%d wrong under mid-form GC, %d/%d after garbage-collect, %d/50 scratch vectors freed"
                          first-bad n second-bad n scratch-bad)))))

;;; standalone-native-load-slot-pin-smoke.el ends here
