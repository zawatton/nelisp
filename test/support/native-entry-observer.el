;;; native-entry-observer.el --- Observe real raw-v2 entries -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Test-only SysV x86-64 interposer. The authenticated descriptor's entry
;; jumps through a recorder and tail-jumps to its original machine address.
;; No Lisp callback, boxing, allocation, GC, or additional activation occurs
;; at entry. Production sources, binaries and authenticated artifacts stay
;; unchanged. The interposer uses only caller-clobbered r10/r11 and preserves
;; all six argument registers and the caller's stack/return address.
(require 'nelisp-native-cache)
(defvar nelisp-test-native-entry-traces nil)
(defvar nelisp-test-native-entry-gc-once nil
  "When non-nil at load, collect once after staging and before machine entry.")
(defvar nelisp-test-native-entry-constructor
  (and (fboundp 'nelisp--native-subr-create)
       (symbol-function 'nelisp--native-subr-create)))

(defun nelisp-test-native-entry-instrument (descriptor name module bridge)
  "Record entries of DESCRIPTOR without changing its raw-v2 ABI."
  (unless (and (vectorp descriptor) (= (length descriptor) 9))
    (error "Native entry observer requires a raw-v2 descriptor"))
  (let* ((entry (aref descriptor 0))
         (data (nelisp-native-load-map-anonymous (+ 16 (* 256 56)) nil))
         (code (nelisp-native-load-map-anonymous 4096 nil))
         (copy (copy-sequence descriptor)) (gc-offset nil)
         ;; movabs r10,data; mov r11,[r10]; incq [r10]; and r11,255;
         ;; imul r11,56; lea r10,[r10+r11+16]; save rdi,rsi,rdx,rcx,r8,r9;
         ;; movabs r11,entry; save entry; jmp r11.
         (bytes (unibyte-string
                 #x49 #xba 0 0 0 0 0 0 0 0
                 #x4d #x8b #x1a #x49 #xff #x02
                 #x49 #x81 #xe3 #xff 0 0 0
                 #x4d #x6b #xdb #x38
                 #x4f #x8d #x54 #x1a #x10
                 #x49 #x89 #x3a #x49 #x89 #x72 #x08
                 #x49 #x89 #x52 #x10 #x49 #x89 #x4a #x18
                 #x4d #x89 #x42 #x20 #x4d #x89 #x4a #x28
                 #x49 #xbb 0 0 0 0 0 0 0 0
                 #x4d #x89 #x5a #x30 #x41 #xff #xe3)))
    (when nelisp-test-native-entry-gc-once
      ;; The same recorded-root collector used by garbage-collect. Preserve
      ;; every argument register and SysV stack alignment across its call.
      (setq gc-offset (- (length bytes) 3)
            bytes
            (concat (substring bytes 0 gc-offset)
                    (unibyte-string
                     #x49 #xba 0 0 0 0 0 0 0 0
                     #x49 #x83 #x7a #x08 0 #x75 #x2f
                     #x49 #xc7 #x42 #x08 1 0 0 0
                     #x57 #x56 #x52 #x51 #x41 #x50 #x41 #x51
                     #x48 #x83 #xec #x08 #x31 #xff
                     #x49 #xbb 0 0 0 0 0 0 0 0 #x41 #xff #xd3
                     #x48 #x83 #xc4 #x08
                     #x41 #x59 #x41 #x58 #x59 #x5a #x5e #x5f
                     #x49 #xbb 0 0 0 0 0 0 0 0 #x41 #xff #xe3))))
    (nelisp-native-load--poke-string code 0 bytes)
    (ptr-write-u64 code 2 data)
    (ptr-write-u64 code 57 entry)
    (when nelisp-test-native-entry-gc-once
      (ptr-write-u64 code (+ gc-offset 2) data)
      (ptr-write-u64 code (+ gc-offset 41) (nelisp-native-load--symbol-addr "nl_gc_collect_from_recorded_roots"))
      (ptr-write-u64 code (+ gc-offset 66) entry))
    (nelisp-native-load--mprotect-rx code 4096)
    (aset copy 0 code)
    ;; Retain the original mapping owner, and record the interposer identity.
    (aset copy 8 (list (aref descriptor 8) code data entry))
    (push (list data entry code nelisp-test-native-entry-gc-once) nelisp-test-native-entry-traces)
    (funcall nelisp-test-native-entry-constructor copy name module bridge)))

(when nelisp-test-native-entry-constructor
  (fset 'nelisp--native-subr-create #'nelisp-test-native-entry-instrument))

(defun nelisp-test-native-entry-snapshot ()
  "Snapshot all interposer counters."
  (mapcar (lambda (trace) (cons (car trace) (ptr-read-u64 (car trace) 0)))
          nelisp-test-native-entry-traces))

(defun nelisp-test-native-entry-collections ()
  "Count requested collections that actually reached the staged-entry hook."
  (let ((count 0))
    (dolist (trace nelisp-test-native-entry-traces)
      (when (and (nth 3 trace) (= (ptr-read-u64 (car trace) 8) 1))
        (setq count (1+ count))))
    count))

(defun nelisp-test-native-entry-events (before)
  "Return every real entry since BEFORE, refusing overwritten evidence."
  (let (events)
    (dolist (trace nelisp-test-native-entry-traces)
      (let* ((data (car trace)) (start (or (cdr (assq data before)) 0))
             (end (ptr-read-u64 data 0)))
        (unless (<= 0 (- end start) 256)
          (error "Native entry observation overflow"))
        (while (< start end)
          (let ((slot (+ data 16 (* (logand start 255) 56))))
            (push (list (ptr-read-u64 slot 48)
                        (ptr-read-u64 slot 0) (ptr-read-u64 slot 8)
                        (ptr-read-u64 slot 16) (ptr-read-u64 slot 24)
                        (ptr-read-u64 slot 32) (ptr-read-u64 slot 40)) events))
          (setq start (1+ start)))))
    (nreverse events)))

(defmacro nelisp-test-with-native-entry-observer (observer &rest body)
  "Run BODY and deliver real machine-entry ABI events to OBSERVER.
For a legacy runtime without direct subrs, observe its actual ptr-call.
OBSERVER must only observe, never call the entry a second time."
  (declare (indent 1))
  (let ((callback (make-symbol "observer")) (before (make-symbol "before"))
        (pointer (make-symbol "pointer")))
    `(let ((,callback ,observer))
       (if nelisp-test-native-entry-constructor
           (let ((,before (nelisp-test-native-entry-snapshot)))
             (unwind-protect (progn ,@body)
               (dolist (event (nelisp-test-native-entry-events ,before))
                 (apply ,callback event))))
         (let ((,pointer (symbol-function 'ptr-call)))
           (cl-letf (((symbol-function 'ptr-call)
                      (lambda (address env ticket argc roots x y)
                        (funcall ,callback address env ticket argc roots x y)
                        (funcall ,pointer address env ticket argc roots x y))))
             ,@body))))))

(defun nelisp-test-native-poison (function setup cleanup)
  "Return a callable that runs FUNCTION with poisoned public cells.
SETUP runs immediately before the native dispatcher stages its activation;
CLEANUP restores the cells before the surrounding oracle observes the result.
The descriptor already holds the immutable providers captured at cache load."
  (let ((invoke (symbol-function 'apply)))
    (lambda (&rest arguments)
      (unwind-protect
          (progn (funcall setup) (funcall invoke function arguments))
        (funcall cleanup)))))

(provide 'native-entry-observer)
