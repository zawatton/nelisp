;;; nelisp-cc-runtime-test.el --- ERT for Phase 7.1.4 runtime layer  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Phase 7.1.4 *integration tests* for `nelisp-cc-runtime'.  Doc 28
;; §3.4 commits +6 ERT smoke; this file ships nine assertions (the six
;; required by the doc plus three covering encoding edge cases that
;; surfaced during scaffolding):
;;
;;   1. simulator alloc/free round-trip records every required syscall
;;      (`mmap' → `mprotect' → `munmap' on Linux, `mmap_jit' →
;;       `jit_write_protect' → `clear_icache' on macOS Apple Silicon).
;;   2. tail-call x86_64 rewrites trailing CALL+RET to JMP and drops
;;      the optional MOV rax, X return-value harvest.
;;   3. tail-call arm64 rewrites trailing BL+RET to B and drops the
;;      optional MOV X0, X.
;;   4. safe-point insertion finds back-edges by RPO comparison.
;;   5. GC metadata format matches the Doc 28 §2.9 contract shape.
;;   6. compile-and-allocate end-to-end produces a final byte vector
;;      with entry/exit gc-poll stubs.
;;   7. NOP stub bytes match the architectural encodings (x86_64 0x90,
;;      arm64 LE 0xD503201F).
;;   8. tail-call rewriter is idempotent.
;;   9. free-exec-page is one-shot (double-free signals).
;;
;; The test pipeline is fully simulator-driven: no real `mmap' /
;; `mprotect' / cache-invalidate ever runs.  Phase 7.5 will replace
;; the simulator with the FFI bridge while keeping these assertions
;; as integration regression coverage.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'nelisp-cc)
(require 'nelisp-cc-x86_64)
(require 'nelisp-cc-arm64)
(require 'nelisp-cc-runtime)

;;; (1) Simulator alloc/free round-trip ----------------------------

(ert-deftest nelisp-cc-runtime-alloc-exec-page-simulator-linux-x86_64 ()
  "Linux x86_64 alloc trace = (mmap → mprotect); free appends munmap.
The page must reach `:executable' state and copy the input bytes
verbatim into the prefix of its backing buffer."
  (let* ((bytes (vector #x90 #x90 #xC3))
         (page  (nelisp-cc-runtime--alloc-exec-page bytes 'linux-x86_64))
         (trace (nelisp-cc-runtime--exec-page-syscall-trace page)))
    (should (eq (nelisp-cc-runtime--exec-page-state page) :executable))
    (should (= (nelisp-cc-runtime--exec-page-length page) 3))
    (should (eq (car (nth 0 trace)) 'mmap))
    (should (eq (car (nth 1 trace)) 'mprotect))
    ;; Bytes survive the copy.
    (let ((buf (nelisp-cc-runtime--exec-page-bytes page)))
      (should (= (aref buf 0) #x90))
      (should (= (aref buf 1) #x90))
      (should (= (aref buf 2) #xC3)))
    ;; Free transitions to :freed and zeroes the buffer.
    (nelisp-cc-runtime--free-exec-page page)
    (should (eq (nelisp-cc-runtime--exec-page-state page) :freed))
    (should (= (aref (nelisp-cc-runtime--exec-page-bytes page) 0) 0))
    (let ((trace2 (nelisp-cc-runtime--exec-page-syscall-trace page)))
      (should (eq (car (car (last trace2))) 'munmap)))))

(ert-deftest nelisp-cc-runtime-alloc-exec-page-simulator-macos-arm64 ()
  "macOS Apple Silicon trace = mmap_jit → jit_write_protect 0 → … →
jit_write_protect 1 → clear_icache.  This locks the W^X protocol so
Phase 7.5 can wire the FFI without re-deriving it from the doc."
  (let* ((bytes (vector #x1F #x20 #x03 #xD5 #xC0 #x03 #x5F #xD6))
         (page  (nelisp-cc-runtime--alloc-exec-page bytes 'macos-arm64))
         (trace (nelisp-cc-runtime--exec-page-syscall-trace page))
         (calls (mapcar #'car trace)))
    (should (equal calls
                   '(mmap_jit jit_write_protect jit_write_protect
                              clear_icache)))
    (should (eq (nelisp-cc-runtime--exec-page-state page) :executable))))

;;; (2) Tail-call x86_64 -------------------------------------------

(ert-deftest nelisp-cc-runtime-tail-call-x86_64-replaces-call-with-jmp ()
  "Trailing 0xE8 disp32 + RET → 0xE9 disp32; the RET is dropped.

Input:  [E8 11 22 33 44 C3]      = CALL +0x44332211; RET
Output: [E9 11 22 33 44]         = JMP  +0x44332211
The displacement bytes survive verbatim; only the opcode flips and
the RET is truncated."
  (let* ((input  (vector #xE8 #x11 #x22 #x33 #x44 #xC3))
         (result (nelisp-cc-runtime--lower-tail-call input 'x86_64))
         (out    (car result))
         (rewrote-p (cdr result)))
    (should rewrote-p)
    (should (= (length out) 5))
    (should (= (aref out 0) #xE9))
    (should (= (aref out 1) #x11))
    (should (= (aref out 2) #x22))
    (should (= (aref out 3) #x33))
    (should (= (aref out 4) #x44))))

(ert-deftest nelisp-cc-runtime-tail-call-x86_64-with-mov-return-harvest ()
  "Trailing CALL + (MOV ret-reg, rax) + RET still gets rewritten.

Input pattern:
   E8 d0 d1 d2 d3            ; CALL rel32
   48 89 D8                  ; MOV rax, rbx (3-byte MOV reg64,reg64)
   C3                        ; RET
Output: [E9 d0 d1 d2 d3]
The MOV is dead post-rewrite (the callee's RET goes straight up)."
  (let* ((input (vector #xE8 #xAA #xBB #xCC #xDD
                        #x48 #x89 #xD8
                        #xC3))
         (out (car (nelisp-cc-runtime--lower-tail-call input 'x86_64))))
    (should (= (length out) 5))
    (should (= (aref out 0) #xE9))
    (should (= (aref out 1) #xAA))))

;;; (3) Tail-call arm64 --------------------------------------------

(ert-deftest nelisp-cc-runtime-tail-call-arm64-replaces-bl-with-b ()
  "Trailing BL + RET-X30 → B; bottom 26 bits of the BL imm26 survive.

Input words (little-endian byte expansion):
   BL  #+0x10                = 0x94000004
   RET X30                   = 0xD65F03C0
Output:
   B   #+0x10                = 0x14000004"
  (let* ((bl-word #x94000004)   ; BL #+0x10
         (ret-word #xD65F03C0)
         (input (apply #'vector
                       (append
                        (list (logand bl-word #xFF)
                              (logand (ash bl-word -8) #xFF)
                              (logand (ash bl-word -16) #xFF)
                              (logand (ash bl-word -24) #xFF))
                        (list (logand ret-word #xFF)
                              (logand (ash ret-word -8) #xFF)
                              (logand (ash ret-word -16) #xFF)
                              (logand (ash ret-word -24) #xFF)))))
         (result (nelisp-cc-runtime--lower-tail-call input 'arm64))
         (out (car result))
         (rewrote-p (cdr result)))
    (should rewrote-p)
    (should (= (length out) 4))
    (let ((w (logior (aref out 0)
                     (ash (aref out 1) 8)
                     (ash (aref out 2) 16)
                     (ash (aref out 3) 24))))
      (should (= w #x14000004)))))

;;; (4) Safe-point insertion: loop back-edge -----------------------

(ert-deftest nelisp-cc-runtime-insert-safe-points-loop-back-edge ()
  "A back-edge from block N to an earlier block in RPO becomes a
`back-edge' kind safe-point.  We construct a tiny SSA function with
exactly one back-edge to avoid relying on the AST→SSA frontend
producing one (the scaffold lowers `if' / `let' but not yet `while')."
  (let* ((fn (nelisp-cc--ssa-make-function 'loop-fn '(nil)))
         (entry (nelisp-cc--ssa-function-entry fn))
         (loop  (nelisp-cc--ssa-make-block fn "loop-head"))
         (exit  (nelisp-cc--ssa-make-block fn "exit"))
         (cval  (nelisp-cc--ssa-make-value fn nil)))
    (nelisp-cc--ssa-link-blocks entry loop)
    (nelisp-cc--ssa-link-blocks loop loop) ; self-loop = back-edge
    (nelisp-cc--ssa-link-blocks loop exit)
    ;; Need a return so the function passes the verifier — but the
    ;; verifier here is permissive enough that we can skip running it.
    (nelisp-cc--ssa-add-instr fn entry 'jump nil nil)
    (nelisp-cc--ssa-add-instr fn loop 'jump nil nil)
    (nelisp-cc--ssa-add-instr fn exit 'return (list cval) nil)
    ;; The cval needs a def-point so verifier rules are obeyed; mark
    ;; it as the return operand by attaching it to a const instr.
    (nelisp-cc--ssa-add-instr fn entry 'const nil cval)
    (let* ((meta (nelisp-cc-runtime--insert-safe-points fn))
           (sps  (plist-get meta :safe-points))
           (kinds (mapcar (lambda (sp) (plist-get sp :kind)) sps)))
      (should (memq 'entry kinds))
      (should (memq 'back-edge kinds))
      (should (memq 'exit kinds)))))

;;; (5) GC metadata format ----------------------------------------

(ert-deftest nelisp-cc-runtime-gc-metadata-format-shape ()
  "GC metadata is a plist with `:gc-metadata-version 1' and a
`:safe-points' list whose entries each carry the Doc 28 §2.9 keys
(`:id', `:kind', `:block-id', `:instr-id', `:pc-offset',
 `:live-roots' as a bool-vector, `:frame-size')."
  (let* ((fn (nelisp-cc-build-ssa-from-ast '(lambda (x) x)))
         (meta (nelisp-cc-runtime--insert-safe-points fn)))
    (should (eq (plist-get meta :gc-metadata-version)
                nelisp-cc-runtime-gc-metadata-version))
    (let* ((sps (plist-get meta :safe-points))
           (sp0 (car sps)))
      (should sp0)
      (should (integerp (plist-get sp0 :id)))
      (should (memq (plist-get sp0 :kind) '(entry exit back-edge)))
      (should (integerp (plist-get sp0 :block-id)))
      (should (integerp (plist-get sp0 :pc-offset)))
      (should (bool-vector-p (plist-get sp0 :live-roots)))
      (should (integerp (plist-get sp0 :frame-size))))))

;;; (6) End-to-end compile-and-allocate ----------------------------

(ert-deftest nelisp-cc-runtime-compile-and-allocate-end-to-end ()
  "`(lambda (x) x)' compiles end-to-end on x86_64: the simulator
returns a page in `:executable' state, the GC metadata has at least
one safe-point, and the final byte vector ends with the architectural
RET (post-injection of the exit gc-poll NOP)."
  (let* ((result (nelisp-cc-runtime-compile-and-allocate
                  '(lambda (x) x) 'x86_64))
         (page   (plist-get result :exec-page))
         (final  (plist-get result :final-bytes))
         (raw    (plist-get result :raw-bytes))
         (meta   (plist-get result :gc-metadata)))
    (should (nelisp-cc-runtime--exec-page-p page))
    (should (eq (nelisp-cc-runtime--exec-page-state page) :executable))
    (should (vectorp final))
    (should (vectorp raw))
    (should (> (length final) 0))
    ;; The first byte should be the entry NOP gc-poll stub.
    (should (= (aref final 0) #x90))
    ;; The last byte should still be RET (the injection moved RET
    ;; one byte later, but the byte itself is preserved).
    (should (= (aref final (- (length final) 1)) #xC3))
    ;; And one byte before RET = the exit NOP gc-poll stub.
    (should (= (aref final (- (length final) 2)) #x90))
    (should (consp (plist-get meta :safe-points)))))

;;; (7) Stub-byte encoding ----------------------------------------

(ert-deftest nelisp-cc-runtime-emit-gc-poll-stub-encodings ()
  "x86_64 NOP = (#x90); arm64 HINT #0 = (#x1F #x20 #x03 #xD5)."
  (should (equal (nelisp-cc-runtime--emit-gc-poll-stub 'x86_64)
                 (list #x90)))
  (should (equal (nelisp-cc-runtime--emit-gc-poll-stub 'arm64)
                 (list #x1F #x20 #x03 #xD5))))

;;; (8) Tail-call idempotence -------------------------------------

(ert-deftest nelisp-cc-runtime-tail-call-idempotent ()
  "Re-running the lowering on already-rewritten bytes leaves them
untouched and reports REWROTE-P=nil (the trailing RET is gone, so
the recogniser short-circuits)."
  (let* ((input (vector #xE8 #x10 #x20 #x30 #x40 #xC3))
         (first (nelisp-cc-runtime--lower-tail-call input 'x86_64))
         (second (nelisp-cc-runtime--lower-tail-call (car first)
                                                     'x86_64)))
    (should (cdr first))
    (should (not (cdr second)))
    (should (equal (car first) (car second)))))

;;; (9) free-exec-page is one-shot --------------------------------

(ert-deftest nelisp-cc-runtime-free-exec-page-double-free-errors ()
  "Calling `--free-exec-page' twice on the same page signals
`nelisp-cc-runtime-error'.  Stops a real-life double-munmap from
silently corrupting the simulator state."
  (let* ((bytes (vector #x90 #xC3))
         (page  (nelisp-cc-runtime--alloc-exec-page bytes 'linux-x86_64)))
    (nelisp-cc-runtime--free-exec-page page)
    (should-error (nelisp-cc-runtime--free-exec-page page)
                  :type 'nelisp-cc-runtime-error)))

(provide 'nelisp-cc-runtime-test)
;;; nelisp-cc-runtime-test.el ends here
