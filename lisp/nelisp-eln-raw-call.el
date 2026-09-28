;;; nelisp-eln-raw-call.el --- lossless GNU word calls -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; This narrow bridge captures a native function's complete RAX value in
;; external memory before the standalone Lisp integer boundary can narrow it.
;; It supports six SysV integer/pointer arguments on Linux x86_64. It does not
;; provide GNU nonlocal exits, Lisp callbacks, or general .eln execution.

;;; Code:

(require 'nelisp-eln-abi)
(require 'nl-ffi-memory)

(declare-function ptr-call "ext:nelisp-runtime" (fn a b c d e f g))
(declare-function ptr-read-u8 "ext:nelisp-runtime" (ptr offset))
(declare-function ptr-read-u32 "ext:nelisp-runtime" (ptr offset))
(declare-function ptr-write-u8 "ext:nelisp-runtime" (ptr offset value))
(declare-function ptr-write-u32 "ext:nelisp-runtime" (ptr offset value))
(declare-function syscall-direct "ext:nelisp-runtime"
                  (number a b c d e f))
(declare-function nelisp-eln-metadata-arena-projected-word-p
                  "nelisp-eln-metadata-arena" (word))

(define-error 'nelisp-eln-raw-call-error "Raw native call failed")

(defconst nelisp-eln-raw-call--context-marker 'nelisp-eln-raw-call-context)
(defconst nelisp-eln-raw-call--sys-mprotect 10)
(defconst nelisp-eln-raw-call--prot-read-exec 5)
(defconst nelisp-eln-raw-call--trampoline-bytes
  ;; SysV entry: RDI=target, RSI=argv[6], RDX=out. Save RBX (also aligns
  ;; RSP), preserve out in RBX, load six GP arguments, call target, store
  ;; full RAX, restore RBX and return status 0.
  [#x53 #x48 #x89 #xd3 #x49 #x89 #xf2 #x49 #x89 #xfb
   #x49 #x8b #x3a #x49 #x8b #x72 #x08 #x49 #x8b #x52 #x10
   #x49 #x8b #x4a #x18 #x4d #x8b #x42 #x20 #x4d #x8b #x4a #x28
   #x41 #xff #xd3 #x48 #x89 #x03 #x5b #x31 #xc0 #xc3])

(defvar nelisp-eln-raw-call--trampoline-owner nil
  "Retained RX mapping for the immutable six-argument trampoline.")
(defvar nelisp-eln-raw-call--pending-cleanup nil
  "External mappings whose release failed and can be retried later.")

(defun nelisp-eln-raw-call--release-or-retain (owner)
  "Release OWNER, or retain it for retry without masking another error."
  (condition-case nil
      (progn (nl-ffi-memory-release owner) t)
    (error
     (push owner nelisp-eln-raw-call--pending-cleanup)
     nil)))

(defun nelisp-eln-raw-call-retry-cleanup ()
  "Retry cleanup of mappings retained after an earlier release failure."
  (let ((pending nelisp-eln-raw-call--pending-cleanup)
        (failed nil))
    (setq nelisp-eln-raw-call--pending-cleanup nil)
    (while pending
      (unless (nelisp-eln-raw-call--release-or-retain (car pending))
        (setq failed t))
      (setq pending (cdr pending)))
    (if failed
        (signal 'nelisp-eln-raw-call-error (list "some unmaps still fail"))
      t)))

(defun nelisp-eln-raw-call--ensure-trampoline ()
  "Return the process-lifetime RX trampoline address."
  (unless nelisp-eln-raw-call--trampoline-owner
    (let ((owner (nl-ffi-memory-allocate
                  (length nelisp-eln-raw-call--trampoline-bytes)))
          (complete nil))
      (unwind-protect
          (let ((address nil) (i 0))
            (setq address (nl-ffi-memory-address owner))
            (while (< i (length nelisp-eln-raw-call--trampoline-bytes))
              (ptr-write-u8 address i
                            (aref nelisp-eln-raw-call--trampoline-bytes i))
              (setq i (1+ i)))
            (setq i 0)
            (while (< i (length nelisp-eln-raw-call--trampoline-bytes))
              (unless (= (ptr-read-u8 address i)
                         (aref nelisp-eln-raw-call--trampoline-bytes i))
                (signal 'nelisp-eln-raw-call-error
                        (list "trampoline byte verification failed" i)))
              (setq i (1+ i)))
            (let ((rc (syscall-direct nelisp-eln-raw-call--sys-mprotect
                                      address (aref owner 2)
                                      nelisp-eln-raw-call--prot-read-exec
                                      0 0 0)))
              (unless (= rc 0)
                (signal 'nelisp-eln-raw-call-error
                        (list "mprotect RX failed" rc))))
            (setq nelisp-eln-raw-call--trampoline-owner owner)
            (setq complete t)
            address)
        (unless complete
          (nelisp-eln-raw-call--release-or-retain owner)))))
  (nl-ffi-memory-address nelisp-eln-raw-call--trampoline-owner))

(defun nelisp-eln-raw-call--context-p (context)
  (and (vectorp context) (= (length context) 5)
       (eq (aref context 0) nelisp-eln-raw-call--context-marker)))

(defun nelisp-eln-raw-call--live-context (context)
  (unless (and (nelisp-eln-raw-call--context-p context)
               (not (aref context 4))
               (aref context 1)
               (aref context 2))
    (signal 'nelisp-eln-raw-call-error (list "closed or invalid context")))
  context)

(defun nelisp-eln-raw-call-context-create ()
  "Create a raw-call context with owned six-word argv and one-word result."
  (let ((argv (nl-ffi-memory-allocate 48))
        (out nil)
        (complete nil))
    (unwind-protect
        (progn
          (setq out (nl-ffi-memory-allocate 8))
          (let ((context (vector nelisp-eln-raw-call--context-marker
                                 argv out nil nil)))
            (nelisp-eln-raw-call--ensure-trampoline)
            (setq complete t)
            context))
      (unless complete
        (when argv
          (nelisp-eln-raw-call--release-or-retain argv))
        (when out
          (nelisp-eln-raw-call--release-or-retain out))))))

(defun nelisp-eln-raw-call--write-word (address offset word)
  "Store WORD as two exact u32 halves."
  (nelisp-eln-abi-write-word address offset word))

(defun nelisp-eln-raw-call--read-word (address offset)
  "Return an unsigned word reconstructed from two u32 reads."
  (nelisp-eln-abi-read-word address offset))

(defun nelisp-eln-raw-call-word (context function-address arguments)
  "Call FUNCTION-ADDRESS with up to six raw integer ARGUMENTS.
Return the full unsigned 64-bit RAX word, captured in external memory.
CONTEXT must not already be active."
  (nelisp-eln-raw-call--live-context context)
  (when (aref context 3)
    (signal 'nelisp-eln-raw-call-error (list "context is busy")))
  (unless (and (integerp function-address)
               (> function-address 4096))
    (signal 'nelisp-eln-raw-call-error (list "invalid function address")))
  (unless (and (boundp 'most-positive-fixnum)
               (<= function-address most-positive-fixnum))
    (signal 'nelisp-eln-raw-call-error
            (list "function address is not a transport-safe fixnum")))
  (unless (and (listp arguments) (<= (length arguments) 6))
    (signal 'nelisp-eln-raw-call-error (list "expected at most six arguments")))
  (dolist (argument arguments)
    (let ((word (nelisp-eln-abi-normalize-word argument)))
      ;; Keep raw-call loadable without metadata support: the arena module
      ;; depends on objects, which may itself depend on this bridge.
      (when (and (fboundp 'nelisp-eln-metadata-arena-projected-word-p)
                 (nelisp-eln-metadata-arena-projected-word-p word))
        (signal 'nelisp-eln-raw-call-error
                (list "metadata arena word is not a raw-call argument" word)))))
  (let* ((argv-owner (aref context 1))
         (out-owner (aref context 2))
         (argv (nl-ffi-memory-address argv-owner))
         (out (nl-ffi-memory-address out-owner))
         (values (append arguments (make-list (- 6 (length arguments)) 0)))
         (trampoline (nelisp-eln-raw-call--ensure-trampoline))
         (status nil)
         (i 0))
    (aset context 3 t)
    (unwind-protect
        (progn
          (while values
            (nelisp-eln-raw-call--write-word argv (* 8 i) (car values))
            (setq i (1+ i) values (cdr values)))
          (setq status (ptr-call trampoline function-address argv out 0 0 0))
          (unless (eql status 0)
            (signal 'nelisp-eln-raw-call-error
                    (list "trampoline failed" status)))
          (nelisp-eln-raw-call--read-word out 0))
      (aset context 3 nil))))

(defun nelisp-eln-raw-call-context-release (context)
  "Release CONTEXT's argument/result memory; reject release while busy."
  (unless (and (nelisp-eln-raw-call--context-p context)
               (not (eq (aref context 4) t)))
    (signal 'nelisp-eln-raw-call-error (list "closed or invalid context")))
  (when (aref context 3)
    (signal 'nelisp-eln-raw-call-error (list "context is busy")))
  ;; Quarantine this context before attempting either unmap. Failed owners
  ;; move to the module-rooted retry list, so no partly released context can
  ;; ever be reused.
  (let ((failed nil))
    (aset context 4 'releasing)
    (when (aref context 1)
      (unless (nelisp-eln-raw-call--release-or-retain (aref context 1))
        (setq failed t))
      (aset context 1 nil))
    (when (aref context 2)
      (unless (nelisp-eln-raw-call--release-or-retain (aref context 2))
        (setq failed t))
      (aset context 2 nil))
    (aset context 4 t)
    (if failed
        (signal 'nelisp-eln-raw-call-error
                (list "context mappings quarantined for retry"))
      t)))

(provide 'nelisp-eln-raw-call)

;;; nelisp-eln-raw-call.el ends here
