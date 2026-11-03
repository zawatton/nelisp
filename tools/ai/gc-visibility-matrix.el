;;; gc-visibility-matrix.el --- which references keep an arena block alive?  -*- lexical-binding: t; -*-

;; Run by tools/ai/gc-visibility-matrix.sh after loading
;; lisp/nelisp-native-load.el.  Each row stores N references to fresh
;; target blocks in one kind of container, drops every Lisp handle to the
;; targets, runs `garbage-collect', churns same-shaped blocks with a
;; different word 0, and counts targets whose word 0 changed (= reclaimed).
;; Containers are always held from Lisp (as integers), so they survive.
;;
;; Targets:  ab32a8  (alloc-bytes 32 8)    ab44a1  (alloc-bytes 44 1)
;;           str     the payload `nl_alloc_str' leaves in a boxed slot
;; Holders:  buf     a plain word in ONE 400-byte buffer held by a let
;;                   variable, so the native-stack conservative scan finds the
;;                   buffer, queues it and scans its words
;;           cell16  word 2 (offset 16) of its own 32-byte cell; the cells are
;;                   held only as integers in a Lisp list, so they are reached
;;                   through `nl_gc_pin_int_addr' (the precise path), as a
;;                   string slot keeps its payload pointer
;;           xor     an XOR-masked word in a 400-byte buffer (positive control)

(defvar gvm-n 50)
(defvar gvm-mask 1229782938247303441)
(defvar gvm-marker 81985529216486895)
;; Must be a fixnum: a bignum marker is not written by `ptr-write-u64' as
;; the same word, so every row read "reclaimed" (first run, 2026-11-03).

(defun gvm--target (kind)
  "Return (ADDR . BUF) for a fresh target of KIND with word 0 = marker.
BUF is the alloc-bytes buffer passed to nl_alloc_str, else nil."
  (cond
   ((eq kind 'ab32a8) (let ((b (alloc-bytes 32 8))) (ptr-write-u64 b 0 gvm-marker) (cons b nil)))
   ((eq kind 'ab44a1) (let ((b (alloc-bytes 44 1))) (ptr-write-u64 b 0 gvm-marker) (cons b nil)))
   ((eq kind 'str)
    (let ((cell (alloc-bytes 32 8)))
      (nelisp-native-load-box cell "abcdefgh-0123456789-0123456789-0123456789!!")
      (remhash cell nelisp-native-load--slot-pins)
      (cons (ptr-read-u64 cell nelisp-native-load-payload-ptr) cell)))))

(defun gvm--expect (kind addr)
  "The word 0 a surviving target of KIND at ADDR still reads.
A string payload starts with its own bytes, not the marker; comparing it
to the marker made every str row read \"reclaimed\" (2026-11-03)."
  (if (eq kind 'str) (ptr-read-u64 addr 0) gvm-marker))

(defun gvm--churn (kind)
  (let ((i 0) (keep nil))
    (while (< i 20000)
      (let ((b (cond ((eq kind 'ab32a8) (alloc-bytes 32 8))
                     (t (alloc-bytes 44 1)))))
        (ptr-write-u64 b 0 99)
        (setq keep (cons b keep)))
      (setq i (1+ i)))
    (length keep)))

(defun gvm-row (kind holder)
  (let* ((buf (alloc-bytes (* 8 gvm-n) 8))
         (cells nil) (extra nil) (expect nil) (k 0) (freed 0))
    (while (< k gvm-n)
      (let* ((tg (gvm--target kind)) (a (car tg)))
        (when (eq kind 'str) (setq extra (cons (cdr tg) extra)))
        (setq expect (cons (gvm--expect kind a) expect))
        (cond
         ((eq holder 'buf) (ptr-write-u64 buf (* 8 k) a))
         ((eq holder 'xor) (ptr-write-u64 buf (* 8 k) (logxor a gvm-mask)))
         ((eq holder 'cell16)
          (let ((c (alloc-bytes 32 8)))
            (ptr-write-u64 c 16 a)
            (setq cells (cons c cells))
            (ptr-write-u64 buf (* 8 k) (logxor c gvm-mask))))))
      (setq k (1+ k)))
    ;; For `str' the original slots must not be what keeps the payload:
    ;; zero them, so the holder under test is the only reference.
    (dolist (c extra) (ptr-write-u64 c 16 0))
    (garbage-collect)
    (gvm--churn kind)
    (setq k 0)
    (let ((cs (reverse cells)) (ex (reverse expect)))
      (while (< k gvm-n)
        (let ((a (cond ((eq holder 'buf) (ptr-read-u64 buf (* 8 k)))
                       ((eq holder 'xor) (logxor (ptr-read-u64 buf (* 8 k)) gvm-mask))
                       (t (prog1 (ptr-read-u64 (car cs) 16) (setq cs (cdr cs)))))))
          (unless (= (ptr-read-u64 a 0) (car ex)) (setq freed (1+ freed)))
          (setq ex (cdr ex)))
        (setq k (1+ k))))
    (princ (format "%-7s %-7s reclaimed=%d/%d\n" kind holder freed gvm-n))
    freed))

(let ((pc 0) (kept 0))
  (dolist (kind '(ab32a8 ab44a1 str))
    (dolist (holder '(buf cell16 xor))
      (let ((f (gvm-row kind holder)))
        (if (eq holder 'xor)
            (setq pc (+ pc f))
          (setq kept (+ kept (- gvm-n f)))))))
  ;; Lines the ledger checks: XOR rows must show frees (the probe can see
  ;; one), and some plain row must show survival (the marker reads back).
  (princ (format "positive-control reclaimed=%d\n" pc))
  (princ (format "plain-rows kept=%d\n" kept)))
