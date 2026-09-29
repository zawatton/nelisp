;;; nelisp-eln-handler-substrate.el --- Doc 210 S8 native handler substrate -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 210 (docs/design/210-eln-native-handlers.org) stage S8: the memory
;; and the binding that GNU-native `condition-case' code reads, and nothing
;; else.  No handler semantics live here (push_handler minting, matching,
;; unwinding and the divert are S9).
;;
;; - One shadow `struct thread_state' block whose `handlerlist' (+0x68)
;;   starts at a runtime-owned sentinel handler, one 8-byte `current_thread'
;;   cell holding the thread block's address, and a small pool of GNU-layout
;;   handler blocks.  All of it lives in a private anonymous mapping made
;;   with `nelisp-native-load-map-anonymous': outside the GC arena, never scanned,
;;   address-stable for the process lifetime (Doc 210 section 7, R1-R4).
;; - `nelisp-eln-handler-substrate-bind-chain' writes the artifact's own
;;   `current_thread_reloc' cell exactly as GNU's loader does (the cell must
;;   be zero first) so the three dependent loads of the native pop sequence
;;   reach the thread block.
;; - The private assembler `_setjmp'/`_longjmp' pair (A1,
;;   `nelisp-native-jump-x86_64.el') is written into an executable page
;;   (R8) and its `_setjmp' entry is stored into the artifact's single
;;   `.got.plt' JUMP_SLOT word, then read back.  The stub is reachable only
;;   through that GOT word; its address is never handed out as a callable.
;;   The GOT is patched only for an artifact whose whole `_setjmp' surface
;;   and native bodies are admitted by
;;   `nelisp-eln-registration-setjmp-surface-admitted-p'.

;;; Code:

(require 'cl-lib)
(require 'nl-ffi)
(require 'nelisp-eln-system-loader)
(require 'nelisp-eln-registration)
(require 'nelisp-native-load)
(require 'nelisp-asm-x86_64)
(require 'nelisp-native-jump-x86_64)

(define-error 'nelisp-eln-handler-substrate-error
  "NeLisp .eln handler substrate error")

(defconst nelisp-eln-handler-substrate-thread-size #x80)
(defconst nelisp-eln-handler-substrate-handlerlist-offset #x68)
(defconst nelisp-eln-handler-substrate-handler-size #x140)
(defconst nelisp-eln-handler-substrate-handler-next-offset #x20)
(defconst nelisp-eln-handler-substrate-handler-jmp-offset #x40)
(defconst nelisp-eln-handler-substrate--region-size 8192)
(defconst nelisp-eln-handler-substrate--cell-offset #x80)
(defconst nelisp-eln-handler-substrate--sentinel-offset #x100)
(defconst nelisp-eln-handler-substrate--pool-offset #x240)
(defconst nelisp-eln-handler-substrate--got-plt-slot-offset 24
  "Byte offset of the `_setjmp' JUMP_SLOT inside `.got.plt' (the fourth word).")

(defvar nelisp-eln-handler-substrate--state nil
  "Process-wide substrate plist, created once by
`nelisp-eln-handler-substrate-ensure'.")

(defun nelisp-eln-handler-substrate--fail (reason &optional detail)
  (signal 'nelisp-eln-handler-substrate-error (list reason detail)))

;;;; Stub page (A1) ---------------------------------------------------

(defun nelisp-eln-handler-substrate--page-round (n)
  "Round N up to whole pages, with a floor of one page."
  (let* ((page nelisp-native-load-page-bytes)
         (pages (/ (+ n (- page 1)) page)))
    (* page (if (< pages 1) 1 pages))))

(defun nelisp-eln-handler-substrate--write-bytes (address bytes)
  "Write the unibyte string BYTES at ADDRESS."
  (let ((i 0) (n (length bytes)))
    (while (< i n)
      (ptr-write-u8 address i (aref bytes i))
      (setq i (1+ i)))))

(defun nelisp-eln-handler-substrate--stub-bytes ()
  "Assemble the private setjmp/longjmp pair; return (BYTES SETJMP LONGJMP)."
  (let ((buf (nelisp-asm-x86_64-make-buffer)))
    (nelisp-native-jump-x86_64-emit-setjmp buf 'substrate-setjmp)
    (nelisp-native-jump-x86_64-emit-longjmp buf 'substrate-longjmp)
    (let* ((bytes (nelisp-asm-x86_64-resolve-fixups buf))
           (labels (nelisp-asm-x86_64-buffer-labels buf)))
      (list bytes
            (cdr (assq 'substrate-setjmp labels))
            (cdr (assq 'substrate-longjmp labels))))))

(defun nelisp-eln-handler-substrate--make-stub-page ()
  "Map one page, write the stub pair, make it R|X; return (PAGE SETJMP LONGJMP)."
  (let* ((assembled (nelisp-eln-handler-substrate--stub-bytes))
         (bytes (nth 0 assembled))
         (page (nelisp-native-load-map-anonymous
                nelisp-native-load-page-bytes nil)))
    (unless (< (length bytes) nelisp-native-load-page-bytes)
      (nelisp-eln-handler-substrate--fail 'stub-too-large (length bytes)))
    (nelisp-eln-handler-substrate--write-bytes page bytes)
    (unless (= (syscall-direct 10 page nelisp-native-load-page-bytes 5 0 0 0) 0)
      (nelisp-eln-handler-substrate--fail 'stub-page-mprotect-failed page))
    (list page (+ page (nth 1 assembled)) (+ page (nth 2 assembled)))))

;;;; Shadow blocks ----------------------------------------------------

(defun nelisp-eln-handler-substrate-ensure ()
  "Create (once) and return the substrate plist.
Keys: :region :thread :cell :sentinel :stub-page :setjmp :longjmp :pool-next."
  (or nelisp-eln-handler-substrate--state
      (let* ((region (nelisp-native-load-map-anonymous
                      nelisp-eln-handler-substrate--region-size nil))
             (thread region)
             (cell (+ region nelisp-eln-handler-substrate--cell-offset))
             (sentinel (+ region nelisp-eln-handler-substrate--sentinel-offset))
             (stubs (nelisp-eln-handler-substrate--make-stub-page)))
        ;; mmap memory is zero-filled: `type', `next' and `val' of the
        ;; sentinel are 0 and the thread block starts all-zero.
        (ptr-write-u64 cell 0 thread)
        (ptr-write-u64 thread nelisp-eln-handler-substrate-handlerlist-offset
                       sentinel)
        (setq nelisp-eln-handler-substrate--state
              (list :region region :thread thread :cell cell :sentinel sentinel
                    :stub-page (nth 0 stubs) :setjmp (nth 1 stubs)
                    :longjmp (nth 2 stubs)
                    :pool-next (+ region
                                  nelisp-eln-handler-substrate--pool-offset))))))

(defun nelisp-eln-handler-substrate-thread-address ()
  (plist-get (nelisp-eln-handler-substrate-ensure) :thread))

(defun nelisp-eln-handler-substrate-current-thread-cell ()
  (plist-get (nelisp-eln-handler-substrate-ensure) :cell))

(defun nelisp-eln-handler-substrate-sentinel-address ()
  (plist-get (nelisp-eln-handler-substrate-ensure) :sentinel))

(defun nelisp-eln-handler-substrate-handlerlist ()
  "Return the shadow thread block's current `handlerlist' word."
  (ptr-read-u64 (nelisp-eln-handler-substrate-thread-address)
                nelisp-eln-handler-substrate-handlerlist-offset))

(defun nelisp-eln-handler-substrate-allocate-block ()
  "Return a zeroed 16-aligned GNU-layout handler block from the substrate pool.
Bump allocation with no release: S9 owns real minting and reclamation."
  (let* ((state (nelisp-eln-handler-substrate-ensure))
         (next (plist-get state :pool-next))
         (end (+ (plist-get state :region)
                 nelisp-eln-handler-substrate--region-size)))
    (unless (<= (+ next nelisp-eln-handler-substrate-handler-size) end)
      (nelisp-eln-handler-substrate--fail 'handler-pool-exhausted next))
    (plist-put state :pool-next
               (+ next nelisp-eln-handler-substrate-handler-size))
    next))

(defun nelisp-eln-handler-substrate--stub-address (kind)
  "Return the private stub address for KIND (`setjmp' or `longjmp').
Internal: only the loader's GOT binding and the adapter divert (S9) may use it."
  (plist-get (nelisp-eln-handler-substrate-ensure)
             (cond ((eq kind 'setjmp) :setjmp)
                   ((eq kind 'longjmp) :longjmp)
                   (t (nelisp-eln-handler-substrate--fail 'unknown-stub kind)))))

;;;; Artifact-side helpers -------------------------------------------

(defun nelisp-eln-handler-substrate--state-of (handle)
  (nelisp-eln-system-loader-layout handle))

(defun nelisp-eln-handler-substrate--writable-after-load-p (handle vaddr)
  "True when VADDR (an ELF virtual address of HANDLE) is in a PF_W PT_LOAD
and outside the page range ld.so makes read-only for GNU_RELRO."
  (let* ((state (nelisp-eln-handler-substrate--state-of handle))
         (loads (plist-get state :loads))
         (bytes (plist-get state :file-bytes))
         (relro (nelisp-eln-registration-relro-range bytes))
         (page nelisp-native-load-page-bytes)
         (in-writable-load
          (cl-some (lambda (row)
                     (and (/= 0 (logand (nth 4 row) 2))
                          (>= vaddr (nth 0 row))
                          (< vaddr (+ (nth 0 row) (nth 3 row)))))
                   loads)))
    (and in-writable-load
         (or (null relro)
             (let ((start (* page (/ (car relro) page)))
                   (end (* page (/ (cdr relro) page))))
               (or (< vaddr start) (>= vaddr end)))))))

;;;; S8.1: current_thread_reloc chain --------------------------------

(defun nelisp-eln-handler-substrate-bind-chain (handle)
  "Write HANDLE's `current_thread_reloc' cell with &current_thread, as GNU does.
The cell must be an 8-byte writable object that is still zero.  Return its
address."
  (let* ((state (nelisp-eln-handler-substrate--state-of handle))
         (sub (nelisp-eln-handler-substrate-ensure))
         (info (nelisp-eln-system-loader-symbol-info
                handle "current_thread_reloc"))
         (address (plist-get info :address))
         (value (- address (plist-get state :bias))))
    (unless (= (plist-get info :size) 8)
      (nelisp-eln-handler-substrate--fail 'current-thread-reloc-size
                                          (plist-get info :size)))
    (unless (nelisp-eln-handler-substrate--writable-after-load-p handle value)
      (nelisp-eln-handler-substrate--fail 'current-thread-reloc-not-writable
                                          value))
    (unless (= (ptr-read-u64 address 0) 0)
      (nelisp-eln-handler-substrate--fail 'current-thread-reloc-not-zero
                                          (ptr-read-u64 address 0)))
    (ptr-write-u64 address 0 (plist-get sub :cell))
    (unless (= (ptr-read-u64 address 0) (plist-get sub :cell))
      (nelisp-eln-handler-substrate--fail 'current-thread-reloc-readback
                                          (ptr-read-u64 address 0)))
    address))

(defun nelisp-eln-handler-substrate-artifact-handlerlist (handle)
  "Read `thread->handlerlist' through HANDLE's own GOT, exactly as the native
pop sequence does: GOT[current_thread_reloc] -> cell -> &current_thread ->
thread -> +0x68.  Every hop is checked against the substrate."
  (let* ((state (nelisp-eln-handler-substrate--state-of handle))
         (sub (nelisp-eln-handler-substrate-ensure))
         (slot (nelisp-eln-registration-glob-dat-slot
                (plist-get state :file-bytes) "current_thread_reloc"))
         (info (nelisp-eln-system-loader-symbol-info
                handle "current_thread_reloc")))
    (unless slot
      (nelisp-eln-handler-substrate--fail 'no-current-thread-glob-dat))
    (let* ((reloc-address (ptr-read-u64 (+ (plist-get state :bias) slot) 0))
           (current-thread (ptr-read-u64 reloc-address 0))
           (thread (and (> current-thread 0) (ptr-read-u64 current-thread 0))))
      (unless (= reloc-address (plist-get info :address))
        (nelisp-eln-handler-substrate--fail 'chain-got-slot-mismatch
                                            reloc-address))
      (unless (= current-thread (plist-get sub :cell))
        (nelisp-eln-handler-substrate--fail 'chain-current-thread-mismatch
                                            current-thread))
      (unless (eql thread (plist-get sub :thread))
        (nelisp-eln-handler-substrate--fail 'chain-thread-mismatch thread))
      (ptr-read-u64 thread nelisp-eln-handler-substrate-handlerlist-offset))))

;;;; S8.2/S8.3: the _setjmp GOT binding ------------------------------

(defun nelisp-eln-handler-substrate--setjmp-slot (handle)
  "Return the live address of HANDLE's `_setjmp' `.got.plt' word."
  (let* ((state (nelisp-eln-handler-substrate--state-of handle))
         (got (nelisp-eln-registration-section
               (plist-get state :file-bytes) ".got.plt")))
    (unless got
      (nelisp-eln-handler-substrate--fail 'no-got-plt))
    (+ (plist-get state :bias) (nth 0 got)
       nelisp-eln-handler-substrate--got-plt-slot-offset)))

(defun nelisp-eln-handler-substrate--libc-setjmp-address ()
  (nelisp-eln-system-loader-libc-symbol "_setjmp"))

(defun nelisp-eln-handler-substrate-verify-setjmp-binding (handle)
  "Signal unless HANDLE's `_setjmp' GOT word holds the private stub."
  (let ((slot (nelisp-eln-handler-substrate--setjmp-slot handle))
        (stub (nelisp-eln-handler-substrate--stub-address 'setjmp)))
    (unless (= (ptr-read-u64 slot 0) stub)
      (nelisp-eln-handler-substrate--fail 'setjmp-slot-readback
                                          (list (ptr-read-u64 slot 0) stub)))
    slot))

(defun nelisp-eln-handler-substrate-bind-setjmp (handle)
  "Bind the private `_setjmp' stub to HANDLE's single JUMP_SLOT and read it back.
Refuses unless the file's `_setjmp' surface and bodies are admitted, the slot
is writable after load and ld.so had bound it to libc's `_setjmp'.  Return
the slot address."
  (let* ((state (nelisp-eln-handler-substrate--state-of handle))
         (bytes (plist-get state :file-bytes))
         (got (nelisp-eln-registration-section bytes ".got.plt")))
    (unless (nelisp-eln-registration-setjmp-surface-admitted-p bytes)
      (nelisp-eln-handler-substrate--fail 'setjmp-surface-not-admitted
                                          (plist-get state :path)))
    (unless (nelisp-eln-handler-substrate--writable-after-load-p
             handle (+ (nth 0 got)
                       nelisp-eln-handler-substrate--got-plt-slot-offset))
      (nelisp-eln-handler-substrate--fail 'setjmp-slot-not-writable
                                          (nth 0 got)))
    (let* ((slot (nelisp-eln-handler-substrate--setjmp-slot handle))
           (before (ptr-read-u64 slot 0))
           (libc (nelisp-eln-handler-substrate--libc-setjmp-address)))
      (unless (and (integerp libc) (> libc 0) (= before libc))
        (nelisp-eln-handler-substrate--fail 'setjmp-slot-not-libc-bound
                                            (list before libc)))
      (ptr-write-u64 slot 0 (nelisp-eln-handler-substrate--stub-address 'setjmp))
      (condition-case failure
          (nelisp-eln-handler-substrate-verify-setjmp-binding handle)
        (nelisp-eln-handler-substrate-error
         (ptr-write-u64 slot 0 before)
         (signal (car failure) (cdr failure))))
      slot)))

(provide 'nelisp-eln-handler-substrate)

;;; nelisp-eln-handler-substrate.el ends here
