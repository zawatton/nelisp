;;; gc-outslot-probe.el --- is a native result in the mmap'd out slot safe until unboxed?  -*- lexical-binding: t; -*-

;; Simulates `nelisp-native-load-call''s return path.  Native code leaves a
;; fresh string Sexp in the out slot, inside the mmap'd slots the GC never
;; scans, and nothing pins it.  Lisp then calls `nelisp-native-load-unbox'.
;; The probe boxes a string into an mmap slot, drops the slot pin (native
;; code takes none), and unboxes at once, N times, with mid-form GC armed.
;; Positive control: the same with a `garbage-collect' plus churn between
;; box and unbox, which must corrupt results.  That shows a GC in the gap
;; would be caught.

(defvar gop-n 2000)
(defvar gop-s "abcdefgh-0123456789-0123456789-0123456789!!")

(defun gop-churn ()
  (let ((i 0) (keep nil))
    (while (< i 4000)
      (let ((b (alloc-bytes 44 1))) (ptr-write-u64 b 0 99) (setq keep (cons b keep)))
      (setq i (1+ i)))
    (length keep)))

(defun gop-run (gc-in-gap)
  (let* ((page (nelisp-native-load--mmap 4096 nil)) (slot (+ page 64))
         (i 0) (bad 0) (n (if gc-in-gap 50 gop-n)))
    (while (< i n)
      (nelisp-native-load-box slot gop-s)
      (remhash slot nelisp-native-load--slot-pins)
      (when gc-in-gap (garbage-collect) (gop-churn))
      (unless (equal (condition-case nil (nelisp-native-load-unbox slot) (error :error)) gop-s)
        (setq bad (1+ bad)))
      (setq i (1+ i)))
    (cons bad n)))

(let ((direct (gop-run nil)) (ctl (gop-run t)))
  (princ (format "out-slot unbox at once (mid-form GC armed): bad=%d/%d\n" (car direct) (cdr direct)))
  (princ (format "positive control, GC in the gap: bad=%d/%d\n" (car ctl) (cdr ctl)))
  (kill-emacs (cond ((= (car ctl) 0) 2) ((> (car direct) 0) 1) (t 0))))
