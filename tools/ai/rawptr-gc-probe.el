;;; rawptr-gc-probe.el --- does the GC free objects reachable only from raw buffers?  -*- lexical-binding: t; -*-

;; Run on target/nelisp after loading lisp/nelisp-native-load.el:
;;   tools/ai/rawptr-gc-probe.sh [BIN]
;; Case A: `nelisp-native-load-box' puts a runtime string into a raw
;; 32-byte cell; only the cell (an integer) is held from Lisp.
;; Case B: `nelisp-native-load--make-scratch-vector' leaves the vector
;; box pointer only at ADDR+8, and the loader's ADDR is in mmap'd slots.
;; The "no explicit GC" rows still run under mid-form GC (while
;; back-edges), which on 0a304e554 alone frees most of case A; with
;; `(nelisp--debug-switch 6)' those rows read 0/200, which is what shows
;; the probe reads the strings correctly.  Prints one line per case and
;; exits through `kill-emacs' with 0 only when nothing was clobbered.

(defvar rawptr-probe-n 200)

(defun rawptr-probe--name (i)
  (format "rawptr-probe-%04d-abcdefghijklmnopqrstuvwxyz" i))

(defun rawptr-probe--box-all ()
  (let ((i 0) (cells nil))
    (while (< i rawptr-probe-n)
      (let ((cell (alloc-bytes 32 8)))
        (nelisp-native-load-box cell (rawptr-probe--name i))
        (setq cells (cons cell cells)))
      (setq i (1+ i)))
    (nreverse cells)))

(defun rawptr-probe--count-bad (cells)
  (let ((i 0) (bad 0))
    (while cells
      (let ((got (condition-case nil
                     (nelisp-native-load-unbox (car cells))
                   (error :error))))
        (unless (equal got (rawptr-probe--name i))
          (setq bad (1+ bad))))
      (setq cells (cdr cells) i (1+ i)))
    bad))

(defun rawptr-probe--churn ()
  ;; Reuse freed blocks of the same sizes through the same paths.
  (let ((i 0) (keep nil))
    (while (< i (* 4 rawptr-probe-n))
      (let ((cell (alloc-bytes 32 8)))
        (nelisp-native-load-box cell "zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz")
        (setq keep (cons cell keep)))
      (setq keep (cons (make-vector 16 i) keep))
      (setq i (1+ i)))
    (length keep)))

(defun rawptr-probe--snapshot (box words)
  (let ((i 0) (acc nil))
    (while (< i words)
      (setq acc (cons (ptr-read-u64 box (* 8 i)) acc))
      (setq i (1+ i)))
    (nreverse acc)))

(defun rawptr-probe--scratch (gc)
  "Return the number of scratch vector boxes reclaimed.
The scratch vectors sit in an mmap page, as the loader's slots do; the
GC does not scan that memory.  A box is a 32-byte arena block whose
word 0 is its capacity, so after GC and a churn that refills 32-byte
blocks with 99, a reclaimed box no longer reads the capacity."
  (let* ((n 50) (page (nelisp-native-load--mmap 4096 nil)) (k 0) (bad 0))
    (while (< k n)
      (nelisp-native-load--make-scratch-vector (+ page (* 32 k)))
      (setq k (1+ k)))
    (when gc
      (garbage-collect)
      (rawptr-probe--churn32))
    (setq k 0)
    (while (< k n)
      (unless (= (ptr-read-u64 (ptr-read-u64 (+ page (* 32 k)) 8) 0)
                 nelisp-native-load-scratch-slots)
        (setq bad (1+ bad)))
      (setq k (1+ k)))
    bad))

(defun rawptr-probe--churn32 ()
  (let ((i 0) (keep nil))
    (while (< i 20000)
      (let ((b (alloc-bytes 32 8)))
        (ptr-write-u64 b 0 99)
        (setq keep (cons b keep)))
      (setq i (1+ i)))
    (length keep)))

(defun rawptr-probe--hidden-control ()
  "Positive control: 32-byte blocks whose addresses are stored XOR-masked.
Nothing holds them, so GC must reclaim them; returns how many it did.
A result of 0 would mean the scratch check above cannot see a free."
  (let* ((n 50) (mask 1229782938247303441) (hid (alloc-bytes (* 8 n) 8))
         (k 0) (freed 0))
    (while (< k n)
      (let ((b (alloc-bytes 32 8)))
        (ptr-write-u64 b 0 16)
        (ptr-write-u64 hid (* 8 k) (logxor b mask)))
      (setq k (1+ k)))
    (garbage-collect)
    (rawptr-probe--churn32)
    (setq k 0)
    (while (< k n)
      (unless (= (ptr-read-u64 (logxor (ptr-read-u64 hid (* 8 k)) mask) 0) 16)
        (setq freed (1+ freed)))
      (setq k (1+ k)))
    freed))

(let* ((ctl-cells (rawptr-probe--box-all))
       (ctl-bad (rawptr-probe--count-bad ctl-cells))
       (cells (rawptr-probe--box-all))
       (_ (garbage-collect))
       (_ (rawptr-probe--churn))
       (a-bad (rawptr-probe--count-bad cells))
       (b-ctl (rawptr-probe--scratch nil))
       (b-bad (rawptr-probe--scratch t))
       (hidden (rawptr-probe--hidden-control)))
  (princ (format "case-A no explicit GC: %d/%d bad\n" ctl-bad rawptr-probe-n))
  (princ (format "case-A string-in-raw-cell after GC: %d/%d bad\n" a-bad rawptr-probe-n))
  (princ (format "case-B no explicit GC: %d/50 bad\n" b-ctl))
  (princ (format "case-B scratch-vector in mmap slots after GC: %d/50 bad\n" b-bad))
  (princ (format "positive control (unreferenced blocks reclaimed): %d/50\n" hidden))
  (kill-emacs (cond ((= hidden 0) 2)
                    ((= 0 (+ ctl-bad a-bad b-ctl b-bad)) 0)
                    (t 1))))
