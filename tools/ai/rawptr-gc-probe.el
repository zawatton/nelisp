;;; rawptr-gc-probe.el --- does the GC free objects reachable only from raw buffers?  -*- lexical-binding: t; -*-

;; Run on target/nelisp after loading lisp/nelisp-native-load.el:
;;   tools/ai/rawptr-gc-probe.sh [BIN]
;; Case A: `nelisp-native-load-box' puts a runtime string into a raw
;; 32-byte cell; only the cell (an integer) is held from Lisp.
;; Case B: `nelisp-native-load--make-scratch-vector' leaves the vector
;; box pointer only at ADDR+8.
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
  "Return the number of scratch vectors whose box words changed."
  (let ((i 0) (slots nil) (bad 0))
    (while (< i 50)
      (let ((addr (alloc-bytes 32 8)))
        (nelisp-native-load--make-scratch-vector addr)
        (let ((box (ptr-read-u64 addr 8)))
          (setq slots (cons (list addr box (rawptr-probe--snapshot box 4)) slots))))
      (setq i (1+ i)))
    (when gc
      (garbage-collect)
      (rawptr-probe--churn))
    (dolist (s slots)
      (unless (and (= (ptr-read-u64 (nth 0 s) 8) (nth 1 s))
                   (equal (rawptr-probe--snapshot (nth 1 s) 4) (nth 2 s)))
        (setq bad (1+ bad))))
    bad))

(let* ((ctl-cells (rawptr-probe--box-all))
       (ctl-bad (rawptr-probe--count-bad ctl-cells))
       (cells (rawptr-probe--box-all))
       (_ (garbage-collect))
       (_ (rawptr-probe--churn))
       (a-bad (rawptr-probe--count-bad cells))
       (b-ctl (rawptr-probe--scratch nil))
       (b-bad (rawptr-probe--scratch t)))
  (princ (format "case-A no explicit GC: %d/%d bad\n" ctl-bad rawptr-probe-n))
  (princ (format "case-A string-in-raw-cell after GC: %d/%d bad\n" a-bad rawptr-probe-n))
  (princ (format "case-B no explicit GC: %d/50 bad\n" b-ctl))
  (princ (format "case-B scratch-vector after GC: %d/50 bad\n" b-bad))
  (kill-emacs (if (= 0 (+ ctl-bad a-bad b-ctl b-bad)) 0 1)))
