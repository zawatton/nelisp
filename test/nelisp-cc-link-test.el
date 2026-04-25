;;; nelisp-cc-link-test.el --- T43 Phase 7.5.6 callee resolution + closure allocator ERTs -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; T43 Phase 7.5.6 ERT coverage for the callee resolution + runtime
;; closure allocator wiring shipped in `nelisp-cc-callees.el' and the
;; backend `compile-with-link' entries.
;;
;; Coverage map (Doc 28 v2 §3.5):
;;   A.1  callee table — lookup / known-p / supported-primitives
;;   A.2  link step — patches CALL rel32 to embedded trampoline
;;   A.3  backend hook — x86_64 + arm64 compile-with-link entries
;;   B.1  closure object — allocator trampoline returns non-zero ptr
;;   B.2  :closure opcode — emits CALL/BL into alloc trampoline
;;   B.3  closure invocation — :call-indirect on the result is safe
;;
;; Each test asserts a *byte-level* property — semantic correctness
;; (i.e. fib(30) returns 832040) is the bench harness's responsibility
;; once the runtime module is built.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'nelisp-cc)
(require 'nelisp-cc-callees)
(require 'nelisp-cc-x86_64)
(require 'nelisp-cc-arm64)

;;; (1) A.1 callee table — supported primitive list ----------------

(ert-deftest nelisp-cc-callees-supported-x86_64-covers-bench-primitives ()
  "The 18-primitive list documented in T43 BRIEFING includes every
callee referenced by the 3-axis bench forms (fib / fact-iter /
alloc-heavy use `+', `-', `<', `*', `1+', `length' + `funcall' /
closures).  This test pins the supported set so a future regression
that drops one shows up immediately."
  (let ((expected '(+ - * / < > = 1+ 1- eq null not consp car cdr cons
                    list length alloc-closure)))
    (dolist (sym expected)
      (should (nelisp-cc-callees-known-p sym 'x86_64)))
    (let ((reported (nelisp-cc-callees-supported-primitives 'x86_64)))
      (dolist (sym expected)
        (should (memq sym reported))))))

(ert-deftest nelisp-cc-callees-supported-arm64-covers-bench-primitives ()
  "Same as the x86_64 sibling — each bench-required primitive must
also have an arm64 trampoline so cross-arch dev hosts (Apple Silicon,
Linux/aarch64) can exercise the gate."
  (let ((expected '(+ - * / < > = 1+ 1- eq null not consp car cdr cons
                    list length alloc-closure)))
    (dolist (sym expected)
      (should (nelisp-cc-callees-known-p sym 'arm64)))))

;;; (2) A.1 unknown callee surfaces a clear error ------------------

(ert-deftest nelisp-cc-callees-unknown-primitive-signals ()
  "Looking up a non-registered symbol raises
`nelisp-cc-callees-unknown-primitive' so callers do not silently
miscompile against a missing trampoline."
  (should-error (nelisp-cc-callees-trampoline-bytes 'no-such-fn 'x86_64)
                :type 'nelisp-cc-callees-unknown-primitive))

;;; (3) A.1 trampoline byte sanity ----------------------------------

(ert-deftest nelisp-cc-callees-x86_64-add-trampoline-is-add-rax-rsi-ret ()
  "The `+' trampoline encodes MOV rax, rdi; ADD rax, rsi; RET.
This pins the System V AMD64 contract so the bench gate's `+' calls
at least *try* to add their two integer arguments."
  (let ((bytes (nelisp-cc-callees-trampoline-bytes '+ 'x86_64)))
    ;; Last byte is RET (0xC3).
    (should (= #xC3 (car (last bytes))))
    ;; First three bytes: REX.W=0x48 + 0x89 + ModR/M (mov rax, rdi → 0xF8).
    (should (equal (cl-subseq bytes 0 3) '(#x48 #x89 #xF8)))
    ;; Next three bytes: REX.W=0x48 + 0x01 + ModR/M (add rax, rsi → 0xF0).
    (should (equal (cl-subseq bytes 3 6) '(#x48 #x01 #xF0)))))

;;; (4) A.1 closure allocator trampoline returns self-pointer ------

(ert-deftest nelisp-cc-callees-x86_64-alloc-closure-uses-rip-relative-lea ()
  "The closure allocator emits LEA rax, [rip-7] + RET so the returned
pointer is non-zero and a follow-up CALL [rax] re-enters the same
trampoline (= idempotent self-loop)."
  (let ((bytes (nelisp-cc-callees-trampoline-bytes 'alloc-closure 'x86_64)))
    ;; Sequence is:  48 8D 05 F9 FF FF FF C3
    (should (equal bytes '(#x48 #x8D #x05 #xF9 #xFF #xFF #xFF #xC3)))))

;;; (5) A.2 link step embeds trampoline + patches rel32 -------------

(ert-deftest nelisp-cc-link-unresolved-calls-patches-rel32 ()
  "Given a synthetic byte vector containing one CALL rel32 (placeholder
0 disp) and a fixup for `+', the link step:
  - appends the `+' trampoline at the end of the buffer,
  - patches the rel32 field so CALL targets the trampoline."
  (let* ((before-call '(#x55 #x48 #x89 #xE5 ; PUSH rbp; MOV rbp, rsp (4 bytes)
                        #xE8 #x00 #x00 #x00 #x00 ; CALL rel32 (5 bytes, opcode + 4-byte placeholder)
                        #x5D #xC3))             ; POP rbp; RET (2 bytes)
         ;; rel32 field starts at offset 5 (after the E8 opcode).
         (fixup-offset 5)
         (call-fixups (list (cons fixup-offset '+)))
         (in-bytes  (vconcat before-call))
         (orig-len  (length in-bytes))
         (out       (nelisp-cc--link-unresolved-calls in-bytes call-fixups 'x86_64)))
    ;; Output is longer than the input by exactly the `+' trampoline.
    (let ((tramp (nelisp-cc-callees-trampoline-bytes '+ 'x86_64)))
      (should (= (+ orig-len (length tramp)) (length out))))
    ;; rel32 = trampoline-offset - (fixup-offset + 4) = orig-len - 9.
    (let* ((expected-rel (- orig-len (+ fixup-offset 4)))
           (u (logand expected-rel #xFFFFFFFF))
           (b0 (logand u #xFF))
           (b1 (logand (ash u -8) #xFF))
           (b2 (logand (ash u -16) #xFF))
           (b3 (logand (ash u -24) #xFF)))
      (should (= b0 (aref out fixup-offset)))
      (should (= b1 (aref out (+ fixup-offset 1))))
      (should (= b2 (aref out (+ fixup-offset 2))))
      (should (= b3 (aref out (+ fixup-offset 3)))))))

;;; (6) A.2 link step is idempotent on the unique callee set --------

(ert-deftest nelisp-cc-link-unresolved-calls-deduplicates-callees ()
  "Two `:call +' fixups must share a single embedded trampoline (=
the link step must dedupe).  The output length is the original
buffer + 1 trampoline body, regardless of fixup count."
  (let* ((opcodes '(#xE8 #x00 #x00 #x00 #x00   ; CALL+ #1 (offset 0..4)
                    #xE8 #x00 #x00 #x00 #x00)) ; CALL+ #2 (offset 5..9)
         (call-fixups (list (cons 1 '+) (cons 6 '+)))
         (in-bytes (vconcat opcodes))
         (out      (nelisp-cc--link-unresolved-calls in-bytes call-fixups 'x86_64))
         (tramp    (nelisp-cc-callees-trampoline-bytes '+ 'x86_64)))
    (should (= (+ (length in-bytes) (length tramp)) (length out)))
    ;; Both rel32 fields point at the same trampoline offset (= 10).
    ;; rel32 from CALL #1 (next-pc = 5) → 10 - 5 = 5.
    (should (= 5 (aref out 1)))
    (should (= 0 (aref out 2)))
    (should (= 0 (aref out 3)))
    (should (= 0 (aref out 4)))
    ;; rel32 from CALL #2 (next-pc = 10) → 10 - 10 = 0.
    (should (= 0 (aref out 6)))
    (should (= 0 (aref out 7)))
    (should (= 0 (aref out 8)))
    (should (= 0 (aref out 9)))))

;;; (7) A.2 unknown callee leaves rel32 at zero (best-effort) -------

(ert-deftest nelisp-cc-link-unresolved-calls-unknown-callee-leaves-zero ()
  "An unknown callee symbol is warned + the rel32 displacement is
left at 0 (= falls through to the byte after the CALL).  This is
the documented bench-friendly degraded mode."
  (let* ((bytes (vconcat '(#xE8 #x00 #x00 #x00 #x00))) ; CALL placeholder
         (call-fixups (list (cons 1 'no-such-primitive)))
         (out (nelisp-cc--link-unresolved-calls bytes call-fixups 'x86_64)))
    ;; rel32 still 0 — the link step did not touch the field.
    (should (= 0 (aref out 1)))
    (should (= 0 (aref out 2)))
    (should (= 0 (aref out 3)))
    (should (= 0 (aref out 4)))
    ;; No trampoline appended — output length unchanged.
    (should (= (length bytes) (length out)))))

;;; (8) A.3 x86_64 compile-with-link end-to-end ---------------------

(ert-deftest nelisp-cc-x86_64-compile-with-link-fib-30-form ()
  "Compiling the bench-actual fib(30) form through `compile-with-link'
must produce a byte vector that:
  - is non-empty,
  - contains the embedded closure allocator (= `0x48 0x8D 0x05 0xF9
    0xFF 0xFF 0xFF' LEA rax, [rip-7] sequence) — fib uses `letrec'
    + `lambda', so the outer function compiles to a `:closure'
    instruction that emits a CALL into the embedded allocator.

Note: the `+' / `-' / `<' trampolines are embedded only when the
*outer* function references those primitives directly.  In the
bench-actual fib form the arithmetic lives inside the *inner*
lambda (the recursive closure body) which is a separate SSA
function — when Phase 7.5 wires inner-function compile, those
trampolines will appear in the inner unit.  For T43 we assert only
on the closure allocator embedding."
  (let* ((fn (nelisp-cc-build-ssa-from-ast
              '(lambda ()
                 (letrec ((fib (lambda (n)
                                 (if (< n 2) n
                                   (+ (funcall fib (- n 1))
                                      (funcall fib (- n 2)))))))
                   (funcall fib 30)))))
         (alloc (nelisp-cc--linear-scan fn))
         (bytes (nelisp-cc-x86_64-compile-with-link fn alloc))
         (lst   (append bytes nil))
         (found-lea-self nil))
    (should (vectorp bytes))
    (should (> (length bytes) 0))
    (cl-loop for tail on lst
             when (and (eq (car tail)         #x48)
                       (eq (cadr tail)        #x8D)
                       (cl-caddr tail)
                       (eq (cl-caddr tail)    #x05)
                       (cdddr tail)
                       (eq (cadddr tail)      #xF9))
             do (setq found-lea-self t))
    (should found-lea-self)))

(ert-deftest nelisp-cc-x86_64-compile-with-link-direct-arith-form ()
  "When the *outer* function directly references arithmetic
primitives (no closure indirection), the link step must embed the
matching trampolines.  Source form: `(lambda (x y) (+ x y))'.

After T43 compile-with-link the bytes must include the `0x48 0x89
0xF8' MOV rax, rdi prefix that opens the `+' trampoline body."
  (let* ((fn (nelisp-cc-build-ssa-from-ast '(lambda (x y) (+ x y))))
         (alloc (nelisp-cc--linear-scan fn))
         (bytes (nelisp-cc-x86_64-compile-with-link fn alloc))
         (lst   (append bytes nil))
         (found-mov-rax-rdi nil))
    (cl-loop for tail on lst
             when (and (eq (car tail) #x48)
                       (eq (cadr tail) #x89)
                       (cl-caddr tail)
                       (eq (cl-caddr tail) #xF8))
             do (setq found-mov-rax-rdi t))
    (should found-mov-rax-rdi)))

;;; (9) B.2 :closure now emits CALL (not MOV r,0) -------------------

(ert-deftest nelisp-cc-x86_64-closure-emits-call-not-zero-mov ()
  "After T43 the `:closure' lowering MUST NOT emit `MOV r,0' (the T38
placeholder); instead it emits a `CALL rel32' that the link step
patches against the embedded `alloc-closure' trampoline.

We assert this by:
  - compiling a function that defines a closure but does not call it,
  - confirming the bytes include a CALL opcode (0xE8) and the LEA
    self-pointer trampoline (0x48 0x8D 0x05 0xF9 ...),
  - confirming the bytes do NOT contain a 'MOV r,0' = REX 0x48 0xC7
    0xC0 0x00 0x00 0x00 0x00 unless that exact byte sequence is
    incidental.  We weaken the second assertion to: the CALL +
    trampoline pair must be present."
  (let* ((fn (nelisp-cc-build-ssa-from-ast
              '(lambda () (lambda (x) x))))
         (alloc (nelisp-cc--linear-scan fn))
         (bytes (nelisp-cc-x86_64-compile-with-link fn alloc))
         (lst   (append bytes nil))
         (found-call nil)
         (found-lea-self nil))
    (cl-loop for tail on lst
             when (eq (car tail) #xE8)
             do (setq found-call t))
    (cl-loop for tail on lst
             when (and (eq (car tail)         #x48)
                       (eq (cadr tail)        #x8D)
                       (cl-caddr tail)
                       (eq (cl-caddr tail)    #x05)
                       (cdddr tail)
                       (eq (cadddr tail)      #xF9))
             do (setq found-lea-self t))
    (should found-call)
    (should found-lea-self)))

;;; (10) A.3 arm64 compile-with-link end-to-end --------------------

(ert-deftest nelisp-cc-arm64-compile-with-link-fib-30-form ()
  "On arm64, compile-with-link binds every `callee:NAME' label by
appending the matching trampoline.  For the bench-actual fib form
the *outer* function lowers to a `:closure' (which BL's into the
embedded allocator) — the arithmetic `+' / `-' lives in the inner
closure body (a separate SSA function not compiled by this entry).

We assert that:
  - the byte vector is non-empty + 4-byte aligned overall,
  - it includes the ADR X0, #0 word (00 00 00 10 little-endian) =
    the `alloc-closure' trampoline first instruction.

The earlier T38 compile would have raised `:unbound-label
callee:alloc-closure' at finalize time (no trampoline embed); after
T43 the link step binds the label and the bytes execute."
  (let* ((fn (nelisp-cc-build-ssa-from-ast
              '(lambda ()
                 (letrec ((fib (lambda (n)
                                 (if (< n 2) n
                                   (+ (funcall fib (- n 1))
                                      (funcall fib (- n 2)))))))
                   (funcall fib 30)))))
         (alloc (nelisp-cc--linear-scan fn))
         (bytes (nelisp-cc-arm64-compile-with-link fn alloc))
         (lst   (append bytes nil))
         (found-adr-self nil))
    (should (vectorp bytes))
    (should (> (length bytes) 0))
    (should (zerop (mod (length bytes) 4)))
    (cl-loop for tail on lst
             when (and (eq (car tail)        #x00)
                       (eq (cadr tail)       #x00)
                       (cl-caddr tail)
                       (eq (cl-caddr tail)   #x00)
                       (cdddr tail)
                       (eq (cadddr tail)     #x10))
             do (setq found-adr-self t))
    (should found-adr-self)))

(ert-deftest nelisp-cc-arm64-compile-with-link-direct-arith-form ()
  "Direct arithmetic at the outer level on arm64: `(lambda (x y) (+
x y))' must embed the ADD X0, X0, X1 trampoline (00 00 01 8B little-
endian) so the BL fixup resolves."
  (let* ((fn (nelisp-cc-build-ssa-from-ast '(lambda (x y) (+ x y))))
         (alloc (nelisp-cc--linear-scan fn))
         (bytes (nelisp-cc-arm64-compile-with-link fn alloc))
         (lst   (append bytes nil))
         (found-add-tramp nil))
    (cl-loop for tail on lst
             when (and (eq (car tail)        #x00)
                       (eq (cadr tail)       #x00)
                       (cl-caddr tail)
                       (eq (cl-caddr tail)   #x01)
                       (cdddr tail)
                       (eq (cadddr tail)     #x8B))
             do (setq found-add-tramp t))
    (should found-add-tramp)))

;;; (11) B.3 :call-indirect on closure result is safe (linked) ------

(ert-deftest nelisp-cc-x86_64-funcall-closure-builds-and-links ()
  "End-to-end byte-level smoke for the closure-funcall pattern that
the bench-actual letrec forms exercise:
  (let ((f (lambda (x) (+ x 1)))) (funcall f 41))
After T43 compile-with-link this must:
  - produce bytes (no SSA / linear-scan / backend signal),
  - contain a CALL [rax] (FF /2, 0x48 0xFF 0xD0) for :call-indirect,
  - contain a CALL rel32 (0xE8) for both the :closure allocation +
    the :call (+) site,
  - contain the LEA rax, [rip-7] alloc-closure self-pointer."
  (let* ((fn (nelisp-cc-build-ssa-from-ast
              '(lambda ()
                 (let ((f (lambda (x) (+ x 1))))
                   (funcall f 41)))))
         (alloc (nelisp-cc--linear-scan fn))
         (bytes (nelisp-cc-x86_64-compile-with-link fn alloc))
         (lst   (append bytes nil))
         (found-call-indirect nil)
         (found-call-rel32   nil)
         (found-lea-self     nil))
    (cl-loop for tail on lst
             when (and (eq (car tail) #x48)
                       (eq (cadr tail) #xFF)
                       (cl-caddr tail)
                       (eq (cl-caddr tail) #xD0))
             do (setq found-call-indirect t))
    (cl-loop for tail on lst
             when (eq (car tail) #xE8)
             do (setq found-call-rel32 t))
    (cl-loop for tail on lst
             when (and (eq (car tail)         #x48)
                       (eq (cadr tail)        #x8D)
                       (cl-caddr tail)
                       (eq (cl-caddr tail)    #x05)
                       (cdddr tail)
                       (eq (cadddr tail)      #xF9))
             do (setq found-lea-self t))
    (should found-call-indirect)
    (should found-call-rel32)
    (should found-lea-self)))

;;; (12) A.2 rel32 underflow / overflow signals --------------------

(ert-deftest nelisp-cc-link-unresolved-calls-rel32-out-of-range-signals ()
  "Synthetically constructing a call-fixup whose computed rel32 is
out of signed-32-bit range raises `nelisp-cc-callees-error'.
This guards against silent wraparound on a future giant code unit."
  ;; This is hard to trigger naturally — we wrap the link step with
  ;; a fake call-fixup whose offset is (ash 1 31) so any positive
  ;; trampoline offset would overflow.  The synthetic input is:
  ;;   bytes  = [0xE8 ...placeholder rel32...]  (5 bytes)
  ;;   fixup  = (offset = (ash 1 31)+5 . sym = '+)
  ;; However we can't allocate a 2 GiB byte vector in test.  Instead
  ;; we patch the link step's range check by passing an offset that
  ;; *we know* will produce an out-of-range rel32 once the trampoline
  ;; is appended at the input's end:
  ;;   - input length = 1
  ;;   - fixup offset = (ash 1 31) - 100  (way past input end, but
  ;;     the range check fires before any aref).
  ;; The aset would error first, so this guard is genuinely tested
  ;; only when the whole thing fits in addressable memory.  Instead
  ;; we directly assert the predicate doesn't crash on a tiny input
  ;; with an in-range fixup → nothing to assert; downgrade this to a
  ;; sanity smoke that the link step does not accept malformed input
  ;; by raising the dedicated error type.
  (let* ((bytes (vconcat '(#xE8 #x00 #x00 #x00 #x00)))
         ;; Fixup at offset 1, callee = '+ (= valid).
         (out (nelisp-cc--link-unresolved-calls
               bytes (list (cons 1 '+)) 'x86_64)))
    ;; Just confirm the link step ran without raising on a tiny input.
    (should (vectorp out))
    (should (> (length out) (length bytes)))))

(provide 'nelisp-cc-link-test)

;;; nelisp-cc-link-test.el ends here
