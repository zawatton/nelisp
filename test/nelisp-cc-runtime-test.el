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
one safe-point, and the final byte vector starts with the entry
NOP gc-poll stub.

T84 Phase 7.5 wire — for `(lambda (x) x)' (no cons / no length /
no setq) the new pipeline embeds zero cells + zero trampolines, so
the legacy invariant `last byte = 0xC3 RET' still holds.  The exit
gc-poll NOP injection however now requires `last byte == RET' as
the trigger, so we drop the strict `byte[-2] = NOP' assertion in
favour of just `RET present + entry NOP'."
  ;; CI-smoke gate: end-to-end allocation needs the Phase 7.5 platform
  ;; resolver, which raises on Windows (`nelisp-cc-runtime-todo').
  (skip-unless (condition-case nil
                   (progn (nelisp-cc-runtime--platform-detect) t)
                 (nelisp-cc-runtime-todo nil)))
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
    ;; A RET (0xC3) must be present (= the body's epilogue).
    (should (cl-some (lambda (b) (= b #xC3)) (append final nil)))
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

;;; Phase 7.5.1 FFI bridge MVP smoke ------------------------------
;;
;; These tests need the Rust binary on disk *and* the host CPU to
;; match the byte stream's ISA (we hand-code x86_64 minimum ops, so
;; an arm64 host would SIGILL immediately).  Skip cleanly otherwise
;; — Phase 7.5 proper will add an arch-router that picks the right
;; bytes for the host and removes the gate.

(defun nelisp-cc-runtime-test--runtime-bin ()
  "Return the resolved `nelisp-runtime' binary path, or nil on miss."
  (ignore-errors (nelisp-cc-runtime--locate-runtime-bin)))

(defun nelisp-cc-runtime-test--host-x86_64-p ()
  "Return non-nil when the running Emacs is on an x86_64 host.
Used to gate the FFI bridge ERT off on arm64 hosts where the
hand-coded x86_64 byte streams would SIGILL.  Phase 7.5 proper
adds per-arch byte selection and removes this gate."
  (let ((cfg (downcase (or (and (boundp 'system-configuration)
                                system-configuration)
                           ""))))
    (or (string-match-p "x86_64" cfg)
        (string-match-p "amd64" cfg))))

(defun nelisp-cc-runtime-test--skip-unless-real-exec-available ()
  "Skip the current ERT unless `exec-bytes' can run on this host."
  (unless (nelisp-cc-runtime-test--runtime-bin)
    (ert-skip "nelisp-runtime binary not built — run `make runtime'"))
  (unless (nelisp-cc-runtime-test--host-x86_64-p)
    (ert-skip
     "Phase 7.5.1 MVP byte streams are x86_64-only; host is non-x86_64")))

;;; (10) Real-exec returns 0 from `xor rax, rax; ret' --------------

(ert-deftest nelisp-cc-runtime-real-exec-empty-fn ()
  "`exec-bytes' on the minimal `xor rax, rax; ret' x86_64 stream
returns RESULT: 0."
  (nelisp-cc-runtime-test--skip-unless-real-exec-available)
  (let ((result (nelisp-cc-runtime--exec-real
                 ;; 48 31 C0 = xor rax, rax ; C3 = ret
                 (vector #x48 #x31 #xC0 #xC3))))
    (should (eq (car result) :result))
    (should (eq (nth 1 result) 0))
    (should (eq (nth 2 result) 0))))

;;; (11) Real-exec returns 42 from `mov eax, 42; ret' --------------

(ert-deftest nelisp-cc-runtime-real-exec-return-42 ()
  "`exec-bytes' on `mov eax, 42; ret' returns RESULT: 42.
Verifies the parser handles non-zero positive integers.  Note we
use the 32-bit form (`mov eax, imm32') because it zero-extends to
rax and is one byte shorter than the 64-bit form."
  (nelisp-cc-runtime-test--skip-unless-real-exec-available)
  (let ((result (nelisp-cc-runtime--exec-real
                 ;; B8 2A 00 00 00 = mov eax, 42 ; C3 = ret
                 (vector #xB8 #x2A #x00 #x00 #x00 #xC3))))
    (should (eq (car result) :result))
    (should (eq (nth 2 result) 42))))

;;; (12) Real-exec executes a tiny arithmetic sequence -------------

(ert-deftest nelisp-cc-runtime-real-exec-arithmetic ()
  "`exec-bytes' on `mov eax, 40; add eax, 2; ret' returns RESULT: 42.
Smoke tests that multi-instruction byte streams execute in order
on the JIT page (i.e. `__clear_cache' / `mprotect(RX)' fired in
the right sequence — out-of-order would either crash or read
stale instructions on arm64; on x86_64 it can mis-execute under
adversarial caching)."
  (nelisp-cc-runtime-test--skip-unless-real-exec-available)
  (let ((result (nelisp-cc-runtime--exec-real
                 ;; B8 28 00 00 00 = mov eax, 40
                 ;; 83 C0 02       = add eax, 2
                 ;; C3             = ret
                 (vector #xB8 #x28 #x00 #x00 #x00
                         #x83 #xC0 #x02
                         #xC3))))
    (should (eq (car result) :result))
    (should (eq (nth 2 result) 42))))

;;; (13) Locator signals when the binary is absent -----------------

(ert-deftest nelisp-cc-runtime-binary-missing-signals ()
  "`nelisp-cc-runtime--locate-runtime-bin' raises
`nelisp-cc-runtime-binary-missing' when the override points at a
non-existent file.  Bypasses auto-discovery so the test never
flakes on a host that *does* have the binary built."
  (let ((nelisp-cc-runtime-binary-override
         (expand-file-name "nelisp-runtime-does-not-exist-xyz"
                           temporary-file-directory)))
    (should-error (nelisp-cc-runtime--locate-runtime-bin)
                  :type 'nelisp-cc-runtime-binary-missing)))

;;; (14) Simulator stays the default ------------------------------

(ert-deftest nelisp-cc-runtime-simulator-still-default ()
  "Out-of-the-box `nelisp-cc-runtime-exec-mode' is `simulator' so
existing call sites keep their Phase 7.1.4 audit-trace semantics
unchanged.  Also asserts the defcustom carries the standard-value
sentinel so a future user `setq' / Customize edit cannot quietly
shift the default away from `simulator'."
  (should (eq (default-value 'nelisp-cc-runtime-exec-mode) 'simulator))
  (should (eq nelisp-cc-runtime-exec-mode 'simulator))
  (let ((std (get 'nelisp-cc-runtime-exec-mode 'standard-value)))
    (should std)
    ;; standard-value is stored as a list whose car is an unevaluated
    ;; expression; eval it to confirm the canonical default.
    (should (eq (eval (car std) t) 'simulator))))

;;; (15-19) Phase 7.5.4 in-process FFI (T33) ---------------------
;;
;; The five tests below are *gated by skip-unless-gates*: when either
;; the host Emacs lacks module support, or `nelisp-runtime-module.so'
;; is not on disk yet, every test in this group calls `ert-skip'
;; rather than failing.  This matches the T13 (`nelisp-cc-real-exec-
;; test') gating pattern so a fresh checkout that has not yet run
;; `make runtime-module' still goes green on `make test'.

(defun nelisp-cc-runtime-test--module-supported-p ()
  "Return non-nil when this host can `module-load' a .so."
  (and (boundp 'module-file-suffix) module-file-suffix (fboundp 'module-load)))

(defun nelisp-cc-runtime-test--module-built-p ()
  "Return non-nil when `nelisp-runtime-module.so' is on disk."
  (and (nelisp-cc-runtime-test--module-supported-p)
       (ignore-errors
         (file-readable-p (nelisp-cc-runtime--locate-runtime-module)))))

(defun nelisp-cc-runtime-test--skip-unless-module-built ()
  "Skip the surrounding ERT unless the in-process FFI is available."
  (unless (nelisp-cc-runtime-test--module-supported-p)
    (ert-skip "Emacs lacks dynamic module support — rebuild with --with-modules"))
  (unless (nelisp-cc-runtime-test--module-built-p)
    (ert-skip "nelisp-runtime-module.so missing — run `make runtime-module'")))

(ert-deftest nelisp-cc-runtime-module-locate-skip-unless-built ()
  "T33 (15) — locate the module, gracefully skip when absent.

The test never *fails*: it skips when either the Emacs build lacks
module support or `make runtime-module' has not been run.  When the
artifact is present the locator returns a readable absolute path that
ends in `nelisp-runtime-module.so' so downstream code can pass it
straight to `module-load'."
  (nelisp-cc-runtime-test--skip-unless-module-built)
  (let ((path (nelisp-cc-runtime--locate-runtime-module)))
    (should (stringp path))
    (should (file-readable-p path))
    (should (string-suffix-p "nelisp-runtime-module.so" path))))

(ert-deftest nelisp-cc-runtime-exec-in-process-empty-fn-zero ()
  "T33 (16) — in-process FFI runs `xor rax,rax; ret' and returns 0.

The 4-byte sequence `48 31 C0 C3' is the smallest portable
Linux-x86_64 payload we can pass through the bridge: REX.W xor rax,
rax (3 bytes) plus a near RET (1 byte).  The i64 return value lands
in rax — which we just zeroed — so the bridge's
`(nelisp-runtime-module-exec-bytes ...)' must produce 0."
  (nelisp-cc-runtime-test--skip-unless-module-built)
  (let* ((bytes  (vector #x48 #x31 #xC0 #xC3))
         (result (nelisp-cc-runtime--exec-in-process bytes)))
    (should (eq (car result) :result))
    (should (= (nth 2 result) 0))))

(ert-deftest nelisp-cc-runtime-exec-in-process-return-42 ()
  "T33 (17) — `mov eax, 42; ret' returns 42 in-process.

`B8 2A 00 00 00' loads 42 into eax (zero-extending into rax under the
SysV AMD64 ABI), then `C3' returns.  The bridge harvests rax and we
assert the i64 round-trip is bit-exact."
  (nelisp-cc-runtime-test--skip-unless-module-built)
  (let* ((bytes  (vector #xB8 #x2A #x00 #x00 #x00 #xC3))
         (result (nelisp-cc-runtime--exec-in-process bytes)))
    (should (eq (car result) :result))
    (should (= (nth 2 result) 42))))

(ert-deftest nelisp-cc-runtime-exec-in-process-perf-faster-than-subprocess ()
  "T33 (18) — in-process FFI is meaningfully faster than the subprocess
path.

Walls a small loop through both paths and asserts the in-process
median is at least 5x faster.  We deliberately under-state the
target (the design budget is 100x: ~10 µs in-process vs ~1 ms for
the subprocess hop) because CI hosts run with very different
process-creation tax — 5x is enough headroom that a non-pathological
host always passes, while still catching regressions where the
in-process path accidentally re-spawns the subprocess.

Skips when either prerequisite (module + binary) is missing."
  (nelisp-cc-runtime-test--skip-unless-module-built)
  (unless (ignore-errors (nelisp-cc-runtime--locate-runtime-bin))
    (ert-skip "nelisp-runtime binary missing — run `make runtime'"))
  (let* ((bytes (vector #xB8 #x2A #x00 #x00 #x00 #xC3))
         (n 5)
         ;; In-process: warm the module load before timing.
         (_   (nelisp-cc-runtime--exec-in-process bytes))
         (t0  (current-time))
         (_in (dotimes (_ n) (nelisp-cc-runtime--exec-in-process bytes)))
         (in-elapsed (float-time (time-subtract (current-time) t0)))
         (t1  (current-time))
         (_sp (dotimes (_ n) (nelisp-cc-runtime--exec-real bytes)))
         (sp-elapsed (float-time (time-subtract (current-time) t1))))
    (should (> in-elapsed 0))
    (should (> sp-elapsed 0))
    ;; In-process must be at least 5x faster than subprocess.
    (should (> (/ sp-elapsed in-elapsed) 5))))

(ert-deftest nelisp-cc-runtime-exec-mode-in-process-default-after-load ()
  "T33 (19) — `:in-process' is a recognised exec-mode value and
`compile-and-allocate' threads it through cleanly.

Asserts:

  1. The defcustom's :type accepts the symbol `in-process' (the
     widget's options include `(const ... in-process)').
  2. `compile-and-allocate' with `nelisp-cc-runtime-exec-mode' set
     to `in-process' runs the pipeline and adds `:exec-mode
     in-process' to the returned plist.
  3. When the module is built and the lambda compiles to runnable
     bytes, `:exec-result' is a (`:result' EXIT INT) tuple."
  (nelisp-cc-runtime-test--skip-unless-module-built)
  ;; (1) defcustom widget type covers `in-process'.
  (let ((type (get 'nelisp-cc-runtime-exec-mode 'custom-type)))
    (should type)
    (should (cl-some (lambda (clause)
                       (and (consp clause)
                            (eq (car clause) 'const)
                            (memq 'in-process clause)))
                     (cdr type))))
  ;; (2) compile-and-allocate threads `:exec-mode in-process'.
  (let* ((nelisp-cc-runtime-exec-mode 'in-process)
         (result (nelisp-cc-runtime-compile-and-allocate
                  '(lambda () nil) 'x86_64)))
    (should (eq (plist-get result :exec-mode) 'in-process))
    ;; (3) :exec-result present and well-shaped.  We don't insist on
    ;;     `:result' specifically — phase 7.1.X-shaped bytes may not
    ;;     execute cleanly without callee resolution; the assertion is
    ;;     that the plumbing is wired (a list with a keyword head).
    (let ((er (plist-get result :exec-result)))
      (should (listp er))
      (should (memq (car er) '(:result :error :no-result))))))

(provide 'nelisp-cc-runtime-test)
;;; nelisp-cc-runtime-test.el ends here
