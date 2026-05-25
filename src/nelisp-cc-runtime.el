;;; nelisp-cc-runtime.el --- Phase 7.1.4 mmap exec page + tail-call + safe-point + GC metadata  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; Author: zawatton <kurozawawo@gmail.com>

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Phase 7.1.4 *integration layer* — see docs/design/28-phase7.1-native-compiler.org
;; §3.4 (LOCKED-2026-04-25-v2).  Stitches Phase 7.1.1 (`nelisp-cc.el')
;; SSA frontend + linear-scan, Phase 7.1.2 (`nelisp-cc-x86_64.el')
;; x86_64 backend, and Phase 7.1.3 (`nelisp-cc-arm64.el') arm64 backend
;; into a single end-to-end "AST → bytes → executable page" pipeline.
;;
;; In scope (this file, ~600 LOC):
;;
;;   1. mmap exec page allocator + W^X transition (simulator).
;;      `nelisp-cc-runtime--alloc-exec-page' / `--free-exec-page'.
;;      The simulator records (state RW → RX) transitions in a struct
;;      so ERT can verify the W^X protocol without invoking real syscalls.
;;      The `:syscall-trace' field captures every `nelisp_syscall_*'
;;      symbol the runtime *would* call on a real run; Phase 7.5 will
;;      wire these to the FFI.
;;
;;   2. Proper tail-call lowering (byte-level post-process pass).
;;      `nelisp-cc-runtime--lower-tail-call' detects the canonical
;;      "trailing CALL/BL + (optional MOV ret-reg, X) + RET" pattern at
;;      the end of a function's bytes and rewrites it to JMP/B without
;;      saving a return address.  This reuses the caller's stack frame
;;      and avoids stack growth in deep tail-recursion (Doc 28 §2.6
;;      proper tail-call requirement, e.g. `nelisp-defs-index' tree
;;      walker).
;;
;;   3. Safe-point insertion (analysis pass).
;;      `nelisp-cc-runtime--insert-safe-points' walks the SSA function's
;;      blocks and identifies safe-point positions (function entry, every
;;      :return, every loop back-edge — a back-edge is a block whose
;;      successor is its dominator-equivalent ancestor in the RPO order;
;;      the scaffold approximates this with "successor.id ≤ block.id").
;;      Returns a metadata plist matching the Doc 28 §2.9 contract.
;;
;;   4. GC poll stub emit.  `nelisp-cc-runtime--emit-gc-poll-stub'
;;      returns the byte sequence for a no-op safe-point check (Phase
;;      7.3 will lift this to an actual root-scan poll).  x86_64 = 0x90
;;      (NOP, 1 byte); arm64 = 0xD503201F (HINT #0, 4-byte NOP).
;;
;;   5. End-to-end pipeline.  `nelisp-cc-runtime-compile-and-allocate'
;;      runs (1)–(4) plus the existing T6 / T4 / T9-or-T10 substrate in
;;      one call and returns `(EXEC-PAGE-PTR . GC-METADATA)'.  Backend
;;      defaults to x86_64 on Linux/x86_64 and arm64 on macOS Apple
;;      Silicon / Linux ARM, matching Doc 28 §2.1 (dual ISA first-class).
;;
;; Out of scope (deferred):
;;
;;   - Real mmap PROT_EXEC syscalls — Phase 7.5 (FFI integration, the
;;     `nelisp-runtime/' Rust crate is already shipped, only the bridge
;;     remains).
;;   - Spill / reload prologue+epilogue + stack frame setup — Phase
;;     7.1.5 (the simulator pipeline raises `nelisp-cc-runtime-todo'
;;     when a spilled value reaches `--lower-tail-call' so the band of
;;     supported lambdas is unambiguously surfaced).
;;   - Phi resolution (graph-coloring with copy insertion) — Phase 7.1.5.
;;   - Actual root scanning + GC trigger inside `gc-poll' stubs —
;;     Phase 7.3 (the metadata contract is fixed here; the consumer
;;     side ships separately).
;;
;; Module convention:
;;   - `nelisp-cc-runtime-' = public API
;;   - `nelisp-cc-runtime--' = private helper
;;   - errors derive from `nelisp-cc-runtime-error', a sibling of
;;     `nelisp-cc-error' (the Phase 7.1.1 parent).

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'pcase)
(require 'nelisp-cc)
(require 'nelisp-cc-x86_64)
(require 'nelisp-cc-arm64)
(require 'nelisp-cc-pipeline)
(require 'nelisp-closure)
(require 'nelisp-gc)

;; Phase 7.1.6.a — Rust-side primitive available only when running under
;; standalone NeLisp (= `nelisp' binary).  All callers gate via
;; `(unless (fboundp 'nelisp-cc--dlsym-resolve) ...)' before invocation.
(declare-function nelisp-cc--dlsym-resolve "ext:nelisp-runtime" (symbol-name))

;;; Constants -------------------------------------------------------

(defconst nelisp-cc-runtime-gc-metadata-version 1
  "Current GC metadata format version (Doc 28 §2.9 contract).
Bumped on any breaking change to the safe-point metadata shape.
Phase 7.3 root scanner reads this field first and refuses to consume
any GC metadata whose version it does not understand, so a forward-
compatible bump strategy is mandatory.")

(defconst nelisp-cc-runtime-page-size 4096
  "Exec page allocation granularity used by the simulator.
Real mmap on Linux x86_64 / Linux arm64 / macOS Apple Silicon all
default to 16 KiB or 4 KiB depending on kernel config; the simulator
fixes the size to the smallest portable value so ERT predictions stay
deterministic.  Phase 7.5 will replace this with the runtime page
size queried via `sysconf(_SC_PAGESIZE)'.")

(defconst nelisp-cc-runtime--x86_64-nop-byte #x90
  "Single-byte x86_64 NOP encoding (used as the gc-poll stub).")

(defconst nelisp-cc-runtime--x86_64-ret-byte #xC3
  "Single-byte x86_64 RET encoding (used to locate the function exit).")

(defconst nelisp-cc-runtime--x86_64-call-rel32-opcode #xE8
  "x86_64 CALL rel32 opcode (followed by 4-byte LE displacement).")

(defconst nelisp-cc-runtime--x86_64-jmp-rel32-opcode #xE9
  "x86_64 JMP rel32 opcode (followed by 4-byte LE displacement).
The tail-call lowering rewrites CALL → JMP in place by patching
this single byte.")

(defconst nelisp-cc-runtime--arm64-nop #xD503201F
  "arm64 HINT #0 (NOP) instruction word — used as the gc-poll stub.
Decodes to 4 bytes 0x1F 0x20 0x03 0xD5 in little-endian memory order.")

(defconst nelisp-cc-runtime--arm64-ret-x30 #xD65F03C0
  "arm64 RET (X30) instruction word.  LE bytes: 0xC0 0x03 0x5F 0xD6.")

(defconst nelisp-cc-runtime--arm64-bl-mask #xFC000000
  "arm64 BL opcode mask.  BL #imm26 has top 6 bits = 0b100101 → 0x94000000.")

(defconst nelisp-cc-runtime--arm64-bl-pattern #x94000000
  "arm64 BL opcode pattern (after `--arm64-bl-mask').")

(defconst nelisp-cc-runtime--arm64-b-pattern #x14000000
  "arm64 B (unconditional) opcode pattern (after `--arm64-bl-mask').
Tail-call lowering rewrites BL → B by clearing bit 31 (the only bit
that differs between the two encodings).")

(defvar nelisp-cc-runtime--aot-custom-table (make-hash-table :test 'eq)
  "NAME symbol -> AOT defcustom metadata descriptor.
Doc 129.3J uses this as the runtime-side customization metadata store
for Phase 47 AOT modules.  It intentionally stores metadata only, not
native call-boundary context pointers.")

(defvar nelisp-cc-runtime--aot-handler-stack nil
  "Innermost-first handler stack for Doc 129.8 AOT exception machinery.
Each entry is a plist with `:kind' (`catch', `condition', or `unwind'),
plus kind-specific fields such as `:tag', `:conditions',
`:landing-pad', `:saved-sp', and `:cleanup'.  The elisp runtime bridge
uses this as a deterministic simulator for the native handler-stack ABI;
native code will use the same logical record shape but jump to landing
pads instead of returning descriptors.")

(defvar nelisp-cc-runtime--aot-special-stack nil
  "Innermost-first special binding stack for Doc 129.4 AOT `let'.
Each entry records the symbol name, whether it was previously bound,
and the old value needed to restore the value cell on normal exit.")

;;; Errors ----------------------------------------------------------

(define-error 'nelisp-cc-runtime-error
  "NeLisp Phase 7.1.4 runtime layer error" 'nelisp-cc-error)

(define-error 'nelisp-cc-runtime-todo
  "NeLisp Phase 7.1.4 deferred path (Phase 7.1.5 / 7.5)"
  'nelisp-cc-runtime-error)

(define-error 'nelisp-cc-runtime-bad-bytes
  "NeLisp Phase 7.1.4 byte stream did not match the expected pattern"
  'nelisp-cc-runtime-error)

(define-error 'nelisp-cc-runtime-binary-missing
  "NeLisp Phase 7.5.1 nelisp-runtime binary not found (run `make runtime')"
  'nelisp-cc-runtime-error)

(define-error 'nelisp-cc-runtime-exec-failed
  "NeLisp Phase 7.5.1 subprocess exec-bytes failed (non-zero exit)"
  'nelisp-cc-runtime-error)

(define-error 'nelisp-cc-runtime-module-missing
  "NeLisp Phase 7.5.4 nelisp-runtime-module.so not found (run `make runtime-module')"
  'nelisp-cc-runtime-error)

(define-error 'nelisp-cc-runtime-module-unsupported
  "NeLisp Phase 7.5.4 in-process FFI requires Emacs built with module support"
  'nelisp-cc-runtime-error)

;;; Phase 7.5.1 FFI bridge MVP — exec mode -------------------------
;;
;; The simulator path (T11 SHIPPED) records the W^X protocol without
;; ever calling mmap; the *real* path (this MVP) hands the bytes to
;; the `nelisp-exec-bytes' subprocess bridge which
;; mmaps PROT_EXEC, jumps in, and writes `RESULT: <i64>' to stdout.
;;
;; Phase 7.5 proper will replace the subprocess hop with an in-process
;; FFI module (Emacs dynamic module loading the cdylib + dlsym).  This
;; MVP is deliberately scoped to the subprocess path so Phase 7.1.X
;; backend tests can validate generated bytes against real silicon
;; today, ~10 ms / call overhead being acceptable for ERT smoke.

(defgroup nelisp-cc-runtime nil
  "Phase 7.1.4 / 7.5.1 NeLisp native compiler runtime layer."
  :group 'nelisp-cc
  :prefix "nelisp-cc-runtime-")

(defcustom nelisp-cc-runtime-exec-mode 'simulator
  "Execution mode for the Phase 7.1.4 native code pipeline.

`simulator' — Phase 7.1.4 default.  Allocates a simulated exec page
(no real mmap) and records the W^X syscall trace for ERT
introspection.  Bytes are *not* executed.  Safe on any host.

`real' — Phase 7.5.1 MVP.  Writes the produced bytes to a temp file
and shells out to `nelisp-exec-bytes <file>', which mmaps
PROT_EXEC, calls the bytes as `extern \"C\" fn() -> i64', and prints
the i64 return value.  Requires the Rust binary to have been built
(`make runtime').

`in-process' — Phase 7.5.4 (T33).  Loads the Emacs module wrapper
\(`nelisp-runtime-module.so') via `module-load' and executes the
bytes in-process through the cdylib's `nelisp_syscall_*' surface +
`nelisp-runtime-module-exec-bytes'.  Round-trip is ~10 µs / call
\(~100x faster than the subprocess hop) and the i64 return value is
produced as the function's value directly — no temp file, no
`RESULT:' parsing.  Requires both the Rust cdylib and the C
wrapper to have been built (`make runtime' + `make runtime-module')."
  :type '(choice (const :tag "Simulator (no real exec)" simulator)
                 (const :tag "Real subprocess exec" real)
                 (const :tag "In-process Emacs module" in-process))
  :group 'nelisp-cc-runtime)

(defcustom nelisp-cc-runtime-module-path nil
  "Path to `nelisp-runtime-module.so' (Phase 7.5.4 Emacs module wrapper).

When nil, `nelisp-cc-runtime--locate-runtime-module' auto-discovers it
in the same `target/release' directory as the Rust binary.  When set,
this absolute path is used verbatim — useful for out-of-tree builds
and CI matrices where the module sits elsewhere.

The module wraps `libnelisp_runtime.so' (the Rust cdylib produced by
`cargo build --release') via `dlopen'/`dlsym' and exposes
`nelisp-runtime-module-exec-bytes' for in-process execution.  The
bytes still follow the System V AMD64 / AAPCS64 `extern \"C\" fn() ->
i64' calling convention; the difference vs `real' mode is purely
the elimination of the subprocess hop."
  :type '(choice (const :tag "Auto-detect" nil)
                 (file  :tag "Explicit path"))
  :group 'nelisp-cc-runtime)

(defcustom nelisp-cc-runtime-binary-override nil
  "When non-nil, an absolute path to the `nelisp-runtime' binary.

Bypasses the auto-discovery in `nelisp-cc-runtime--locate-runtime-bin'
and is mostly useful for tests / out-of-tree builds.  When nil the
locator walks up from the loading file looking for `Cargo.toml' and
expects `nelisp-runtime/target/release/nelisp-runtime' below that."
  :type '(choice (const :tag "Auto-discover" nil)
                 (file  :tag "Explicit path"))
  :group 'nelisp-cc-runtime)

(defcustom nelisp-cc-runtime-exec-bytes-binary-override nil
  "When non-nil, an absolute path to the `nelisp-exec-bytes' binary.

Doc 49 moves the raw native-code subprocess bridge out of the
Rust-min `nelisp-runtime' core.  This override bypasses
`nelisp-cc-runtime--locate-exec-bytes-bin' auto-discovery."
  :type '(choice (const :tag "Auto-discover" nil)
                 (file  :tag "Explicit path"))
  :group 'nelisp-cc-runtime)

;;; Exec page (simulator) ------------------------------------------
;;
;; The simulator models the Linux / macOS mmap PROT_EXEC lifecycle
;; without ever calling mmap.  The `state' field tracks the W^X stage:
;;
;;     :allocated   — mmap PROT_READ|WRITE returned a fresh page (RW)
;;     :writable    — same as :allocated; macOS MAP_JIT enters here
;;                    after `jit_write_protect 0' (the page is RW from
;;                    the *current thread's* perspective)
;;     :executable  — mprotect PROT_READ|EXEC has happened (Linux), or
;;                    `jit_write_protect 1' has happened (macOS).  The
;;                    bytes are now read-only + executable.
;;     :freed       — munmap called; further reads / writes are illegal.
;;
;; The `:syscall-trace' is an ordered list of (SYSCALL-SYM . ARGS)
;; records — the contract Phase 7.5 will assert against the real FFI.

(cl-defstruct (nelisp-cc-runtime--exec-page
               (:constructor nelisp-cc-runtime--exec-page-make)
               (:copier nil))
  "Simulated mmap PROT_EXEC page.

BYTES is the writable-vector backing store (always equal to the
caller-supplied byte vector + page-size padding to the next 4 KiB
multiple).  LENGTH is the *requested* byte length (the meaningful
prefix of BYTES; the rest is zero padding).  STATE is the W^X
lifecycle keyword (see commentary above).  PLATFORM is `linux-x86_64'
/ `linux-arm64' / `macos-arm64' — the simulator branches on this to
decide which syscall sequence to record.  SYSCALL-TRACE is the
append-only audit log; tests assert against it to prove the W^X
protocol is correct *before* the FFI is wired."
  (bytes nil)
  (length 0)
  (state :allocated)
  (platform 'linux-x86_64)
  (syscall-trace nil))

(defun nelisp-cc-runtime--platform-detect ()
  "Detect the host platform for the simulator default.
Returns one of `linux-x86_64' / `linux-arm64' / `macos-arm64'.
Unsupported platforms (notably Windows) raise
`nelisp-cc-runtime-todo' — Phase 7.5 lands Win64 ABI."
  (let ((sys (symbol-name system-type))
        ;; `system-configuration' is the canonical machine triple on
        ;; every platform NeLisp targets — `x86_64-pc-linux-gnu',
        ;; `aarch64-apple-darwin', etc.
        (cfg (downcase (or (and (boundp 'system-configuration)
                                system-configuration)
                           ""))))
    (cond
     ((and (string-match-p "darwin" sys)
           (or (string-match-p "aarch64" cfg)
               (string-match-p "arm64" cfg)))
      'macos-arm64)
     ((and (string-match-p "linux\\|gnu" sys)
           (or (string-match-p "aarch64" cfg)
               (string-match-p "arm64" cfg)))
      'linux-arm64)
     ((string-match-p "linux\\|gnu" sys)
      'linux-x86_64)
     (t (signal 'nelisp-cc-runtime-todo
                (list :platform-not-supported sys cfg :phase '7.5))))))

(defun nelisp-cc-runtime--page-align (n)
  "Round N up to the next multiple of `nelisp-cc-runtime-page-size'."
  (let ((p nelisp-cc-runtime-page-size))
    (* p (/ (+ n p -1) p))))

(defun nelisp-cc-runtime--alloc-exec-page (bytes &optional platform)
  "Allocate a simulated exec page for the byte vector BYTES.

PLATFORM is `linux-x86_64' / `linux-arm64' / `macos-arm64' (defaults
to `nelisp-cc-runtime--platform-detect`).  The returned page starts
in the `:executable' state — i.e. the W^X transition has already
been recorded in the syscall trace, matching the steady-state Phase
7.5 will hand off to the JIT.  The simulator records every syscall
the *real* runtime would invoke:

  Linux x86_64  : `mmap'(PROT_READ|WRITE) → write → `mprotect'(PROT_READ|EXEC)
  Linux arm64   : same as above + `clear_icache'(start, end)
  macOS arm64   : `mmap_jit' → `jit_write_protect'(0)
                  → write
                  → `jit_write_protect'(1) → `clear_icache'

Returns a `nelisp-cc-runtime--exec-page' struct.  Phase 7.5 wires
each `(SYSCALL . ARGS)' entry to its real `nelisp_syscall_*' export."
  (unless (vectorp bytes)
    (signal 'nelisp-cc-runtime-error
            (list :alloc-not-a-vector bytes)))
  (let* ((plat (or platform (nelisp-cc-runtime--platform-detect)))
         (n    (length bytes))
         (page-len (nelisp-cc-runtime--page-align (max n 1)))
         (page (make-vector page-len 0))
         (page-obj (nelisp-cc-runtime--exec-page-make
                    :bytes page
                    :length n
                    :state :allocated
                    :platform plat
                    :syscall-trace nil)))
    ;; Step 1: allocate the page (RW).
    (pcase plat
      ('macos-arm64
       (push (list 'mmap_jit :len page-len :prot 'rw)
             (nelisp-cc-runtime--exec-page-syscall-trace page-obj))
       ;; macOS Apple Silicon: `MAP_JIT' pages start *non-writable* on
       ;; non-current threads; the current thread must opt in via
       ;; `jit_write_protect 0' before each write batch.
       (push (list 'jit_write_protect :enabled 0)
             (nelisp-cc-runtime--exec-page-syscall-trace page-obj))
       (setf (nelisp-cc-runtime--exec-page-state page-obj) :writable))
      (_
       (push (list 'mmap :len page-len :prot 'rw)
             (nelisp-cc-runtime--exec-page-syscall-trace page-obj))
       (setf (nelisp-cc-runtime--exec-page-state page-obj) :writable)))
    ;; Step 2: copy the bytes into the page.
    (dotimes (i n)
      (aset page i (aref bytes i)))
    ;; Step 3: transition the page to RX.
    (pcase plat
      ('macos-arm64
       (push (list 'jit_write_protect :enabled 1)
             (nelisp-cc-runtime--exec-page-syscall-trace page-obj))
       (push (list 'clear_icache :start 0 :end n)
             (nelisp-cc-runtime--exec-page-syscall-trace page-obj)))
      ('linux-arm64
       (push (list 'mprotect :len page-len :prot 'rx)
             (nelisp-cc-runtime--exec-page-syscall-trace page-obj))
       (push (list 'clear_icache :start 0 :end n)
             (nelisp-cc-runtime--exec-page-syscall-trace page-obj)))
      (_
       (push (list 'mprotect :len page-len :prot 'rx)
             (nelisp-cc-runtime--exec-page-syscall-trace page-obj))))
    (setf (nelisp-cc-runtime--exec-page-state page-obj) :executable)
    ;; Reverse the trace so it reads in syscall-order (oldest first).
    (setf (nelisp-cc-runtime--exec-page-syscall-trace page-obj)
          (nreverse (nelisp-cc-runtime--exec-page-syscall-trace page-obj)))
    page-obj))

(defun nelisp-cc-runtime--free-exec-page (page)
  "Release the simulated exec PAGE (transitions to `:freed' state).

Records a `munmap' syscall in the audit trace and zeroes the bytes
buffer so subsequent accidental reads through the simulator surface
as obvious all-zero data (rather than stale instructions).  Returns
the updated PAGE struct so callers can chain assertions in tests."
  (unless (nelisp-cc-runtime--exec-page-p page)
    (signal 'nelisp-cc-runtime-error
            (list :free-not-a-page page)))
  (when (eq (nelisp-cc-runtime--exec-page-state page) :freed)
    (signal 'nelisp-cc-runtime-error
            (list :double-free page)))
  (let ((page-len (length (nelisp-cc-runtime--exec-page-bytes page))))
    (setf (nelisp-cc-runtime--exec-page-syscall-trace page)
          (append (nelisp-cc-runtime--exec-page-syscall-trace page)
                  (list (list 'munmap :len page-len)))))
  ;; Zero the backing bytes — defence-in-depth against a stale read.
  (let ((buf (nelisp-cc-runtime--exec-page-bytes page)))
    (dotimes (i (length buf)) (aset buf i 0)))
  (setf (nelisp-cc-runtime--exec-page-state page) :freed)
  page)

;;; GC poll stub emit -----------------------------------------------

(defun nelisp-cc-runtime--emit-gc-poll-stub (backend)
  "Return the byte sequence for a Phase 7.1.4 `gc-poll' stub.

BACKEND is `x86_64' or `arm64'.  The stub is a single architectural
NOP — Phase 7.3 lifts this to an actual safe-point check + root-scan
trigger.  Doc 28 §2.9 commits the contract so Phase 7.3 can replace
the byte sequence in place without disturbing the caller's frame
layout (NOP is exactly the size of the smallest meaningful poll).

x86_64 → (#x90)            ; 1-byte NOP
arm64  → (#x1F #x20 #x03 #xD5) ; 4-byte HINT #0 (LE order)"
  (pcase backend
    ('x86_64 (list nelisp-cc-runtime--x86_64-nop-byte))
    ('arm64
     (let ((w nelisp-cc-runtime--arm64-nop))
       (list (logand w #xFF)
             (logand (ash w -8) #xFF)
             (logand (ash w -16) #xFF)
             (logand (ash w -24) #xFF))))
    (_ (signal 'nelisp-cc-runtime-error
               (list :unknown-backend backend)))))

;;; Safe-point insertion (analysis pass) ---------------------------
;;
;; This pass is *non-destructive* w.r.t. the SSA IR: it returns a
;; metadata plist describing where Phase 7.3 root scanner should poll
;; for GC, without modifying any block / instruction.  The IR-level
;; safe-point would attach a `:gc-poll' opcode at each location, but
;; the backends (T9 / T10) are frozen and reject unknown opcodes —
;; so the integration layer expresses safe-points as metadata only.
;;
;; Detection rules (Doc 28 §2.9):
;;
;;   1. function entry  — every function gets safe-point id 0 at
;;                        pc-offset 0 (before the function prologue
;;                        executes; Phase 7.3 polls before the call
;;                        even returns to the caller's caller).
;;   2. function exit   — every `:return' instruction is a safe-point
;;                        (between the stack restoration and the RET
;;                        proper, where Phase 7.3 can still scan the
;;                        return-value register).
;;   3. loop back-edge  — every block whose successor is itself or an
;;                        earlier block in RPO order (a back-edge in
;;                        Tarjan's terminology).  The scaffold uses a
;;                        block-id comparison rather than full Tarjan
;;                        because the IR is already RPO-ordered when
;;                        this pass runs.

(defun nelisp-cc-runtime--block-rpo-index (rpo block)
  "Return BLOCK's index in RPO (the reverse-postorder list).
Returns nil when BLOCK is unreachable from entry — those blocks have
no safe-points by definition (the verifier already rejects orphan
blocks via `nelisp-cc--ssa-verify-function')."
  (cl-position block rpo))

(defun nelisp-cc-runtime--collect-back-edges (function)
  "Return ((SRC-BLK . DST-BLK) ...) for every back-edge in FUNCTION.

A back-edge is a control-flow edge SRC → DST whose DST appears
*earlier* in the reverse-postorder linearisation than SRC.  This is
the standard textbook definition; loops manifest as exactly this
pattern (the loop-header is the dominator-equivalent ancestor in
RPO).  The scaffold trusts the existing verifier to enforce that
every successor referenced is in the block table.

The pass is O(blocks · successors-per-block) — cheap enough to run
unconditionally on every compilation."
  (let* ((rpo (nelisp-cc--ssa--reverse-postorder function))
         (acc nil))
    (dolist (src rpo)
      (let ((src-idx (nelisp-cc-runtime--block-rpo-index rpo src)))
        (dolist (dst (nelisp-cc--ssa-block-successors src))
          (let ((dst-idx (nelisp-cc-runtime--block-rpo-index rpo dst)))
            (when (and dst-idx src-idx (<= dst-idx src-idx))
              (push (cons src dst) acc))))))
    (nreverse acc)))

(defun nelisp-cc-runtime--count-instrs-before (block target-instr)
  "Return the count of instructions in BLOCK that precede TARGET-INSTR.
TARGET-INSTR may be nil, in which case every instr precedes it
(returns the block's length).  Used by safe-point pc-offset
estimation, which is `block-start-offset + count * average-instr-bytes'
in the scaffold."
  (let ((acc 0))
    (cl-block done
      (dolist (instr (nelisp-cc--ssa-block-instrs block))
        (when (eq instr target-instr) (cl-return-from done acc))
        (cl-incf acc)))
    acc))

(defun nelisp-cc-runtime--insert-safe-points (function)
  "Compute Phase 7.3 GC safe-point metadata for FUNCTION.

FUNCTION is a `nelisp-cc--ssa-function'.  This is an *analysis* pass
— FUNCTION is not modified.  Returns a plist matching the Doc 28
§2.9 contract:

  (:gc-metadata-version VERSION
   :function-name NAME
   :safe-points
   ((:id N
     :kind KIND               ; one of `entry', `exit', `back-edge'
     :block-id BID
     :instr-id IID-or-nil
     :pc-offset OFF           ; estimated byte offset, refined by
                              ; backend at compile time
     :live-roots BITMAP       ; bool-vector, bit i set ⟺ value-id i
                              ; holds a Lisp object live at this point
     :frame-size SIZE)        ; in bytes; 0 when prologue/epilogue
                              ;          are not yet emitted
    ...))

The `:live-roots' bitmap is conservatively computed as \"every value
that is live at this point\" — Phase 7.3 will narrow this with type
information when constant folding / type inference matures.

Safe-point IDs are sequential, allocated in (entry, back-edges in
SSA order, exits in SSA order) order so Phase 7.3 can stable-sort
them by ID and binary-search the resulting array."
  (let* ((rpo (nelisp-cc--ssa--reverse-postorder function))
         (back-edges (nelisp-cc-runtime--collect-back-edges function))
         (next-id 0)
         (safe-points nil)
         (entry-blk (nelisp-cc--ssa-function-entry function))
         (max-vid (nelisp-cc--ssa-function-next-value-id function))
         (params (nelisp-cc--ssa-function-params function)))
    ;; Helper: build a live-roots bitmap.  At the entry safe-point all
    ;; parameters are live; at later points the scaffold over-approxi-
    ;; mates by marking every value whose def-point precedes the safe
    ;; point.  Phase 7.3 + a future liveness analysis pass will trim.
    (cl-flet ((make-bitmap
                (live-vids)
                (let ((bv (make-bool-vector (max max-vid 1) nil)))
                  (dolist (vid live-vids)
                    (when (and (integerp vid) (< vid (length bv)))
                      (aset bv vid t)))
                  bv)))
      ;; (1) Function entry — id 0, all params are live.
      (push (list :id next-id
                  :kind 'entry
                  :block-id (nelisp-cc--ssa-block-id entry-blk)
                  :instr-id nil
                  :pc-offset 0
                  :live-roots (make-bitmap
                               (mapcar #'nelisp-cc--ssa-value-id params))
                  :frame-size 0)
            safe-points)
      (cl-incf next-id)
      ;; (2) Loop back-edges — a single safe-point at each src block.
      (dolist (edge back-edges)
        (let* ((src (car edge))
               (live-vids
                ;; Approximate: every value defined in any block
                ;; appearing at-or-before SRC in RPO order.  Phase
                ;; 7.1.5 will replace this with a proper liveness
                ;; fixed-point; the over-approximation is sound
                ;; because Phase 7.3 only *trims* the bitmap when
                ;; type information lets it.
                (let ((acc (mapcar #'nelisp-cc--ssa-value-id params))
                      (seen nil)
                      (done nil))
                  (dolist (blk rpo)
                    (unless done
                      (push blk seen)
                      (when (eq blk src) (setq done t))))
                  (dolist (blk seen)
                    (dolist (instr (nelisp-cc--ssa-block-instrs blk))
                      (let ((d (nelisp-cc--ssa-instr-def instr)))
                        (when d
                          (push (nelisp-cc--ssa-value-id d) acc)))))
                  acc)))
          (push (list :id next-id
                      :kind 'back-edge
                      :block-id (nelisp-cc--ssa-block-id src)
                      :instr-id nil
                      ;; The scaffold leaves :pc-offset symbolic
                      ;; (block-id + 1 nudges past the entry safe-point
                      ;; in the absence of a real codegen size table).
                      :pc-offset (1+ (nelisp-cc--ssa-block-id src))
                      :live-roots (make-bitmap live-vids)
                      :frame-size 0)
                safe-points)
          (cl-incf next-id)))
      ;; (3) Function exits — every :return instruction.
      (dolist (blk rpo)
        (dolist (instr (nelisp-cc--ssa-block-instrs blk))
          (when (eq (nelisp-cc--ssa-instr-opcode instr) 'return)
            (let ((live-vids
                   ;; The return value's operand is live at exit; the
                   ;; conservative scaffold also keeps every preceding
                   ;; def alive (Phase 7.3 will trim).
                   (let ((acc (mapcar #'nelisp-cc--ssa-value-id
                                      (nelisp-cc--ssa-instr-operands
                                       instr))))
                     (append acc
                             (mapcar #'nelisp-cc--ssa-value-id params)))))
              (push (list :id next-id
                          :kind 'exit
                          :block-id (nelisp-cc--ssa-block-id blk)
                          :instr-id (nelisp-cc--ssa-instr-id instr)
                          :pc-offset
                          ;; -1 sentinel: backend resolves the actual
                          ;; offset of the RET byte at finalize time.
                          -1
                          :live-roots (make-bitmap live-vids)
                          :frame-size 0)
                    safe-points)
              (cl-incf next-id))))))
    ;; Reverse so safe-points appear in id order.
    (list :gc-metadata-version nelisp-cc-runtime-gc-metadata-version
          :function-name (nelisp-cc--ssa-function-name function)
          :safe-points (nreverse safe-points))))

(cl-defun nelisp-cc-runtime--insert-safe-points-with-meta
    (function &key frame-size call-pc-offsets)
  "Compute safe-point metadata, threading real frame layout info.
T63 Phase 7.5.7 — Doc 28 §2.9 critical fix #4 minimum closure.

The pre-T63 `--insert-safe-points' hard-coded `:frame-size 0' for
every safe-point and emitted no `:kind call' entries, leaving a
moving collector blind to the live-roots layout at every call site.
This helper layers the real frame-size + per-call PC offsets on top
of the existing analysis pass without touching its internal
structure.

Arguments (keyword-only):
  :frame-size       integer — total spill-frame size in bytes
                    (output of `nelisp-cc--allocate-stack-slots').
                    Threaded into every safe-point's `:frame-size'.
  :call-pc-offsets  alist of (CALL-INSTR-ID . PC-OFFSET) — populated
                    by the backend post-codegen so a moving GC can
                    locate the precise PC immediately after each
                    CALL / CALL-indirect / BL / BLR.  Each entry
                    becomes an extra safe-point of `:kind call' in
                    the result.  When nil, the helper still scans
                    the SSA function for call instructions and emits
                    one `:kind call' per call with `:pc-offset -1'
                    (sentinel: backend resolves later).

Returns a plist matching `--insert-safe-points''s contract:
  (:gc-metadata-version VERSION
   :function-name NAME
   :safe-points (PLIST ...))

Each safe-point plist now also carries `:frame-size' (from the
argument).  Call safe-points are appended to the existing entry /
back-edge / exit set, sorted by `:id' in append order so consumers
can binary-search by id without re-sorting."
  (let* ((base (nelisp-cc-runtime--insert-safe-points function))
         (sps (plist-get base :safe-points))
         (next-id (length sps))
         (call-sps nil))
    ;; Patch every existing safe-point's :frame-size.
    (when frame-size
      (setq sps
            (mapcar
             (lambda (sp)
               (let ((sp-copy (copy-sequence sp)))
                 (plist-put sp-copy :frame-size frame-size)))
             sps)))
    ;; Synthesize one safe-point per `:call' / `:call-indirect'.
    (let ((rpo (nelisp-cc--ssa--reverse-postorder function))
          (params (nelisp-cc--ssa-function-params function))
          (max-vid (nelisp-cc--ssa-function-next-value-id function)))
      (cl-flet ((bitmap (vids)
                  (let ((bv (make-bool-vector (max max-vid 1) nil)))
                    (dolist (v vids)
                      (when (and (integerp v) (< v (length bv)))
                        (aset bv v t)))
                    bv)))
        (dolist (blk rpo)
          (dolist (instr (nelisp-cc--ssa-block-instrs blk))
            (when (memq (nelisp-cc--ssa-instr-opcode instr)
                        '(call call-indirect))
              (let* ((iid (nelisp-cc--ssa-instr-id instr))
                     (pc (or (cdr (assq iid call-pc-offsets)) -1))
                     (operand-vids
                      (mapcar #'nelisp-cc--ssa-value-id
                              (nelisp-cc--ssa-instr-operands instr)))
                     (param-vids
                      (mapcar #'nelisp-cc--ssa-value-id params))
                     (live (append operand-vids param-vids)))
                (push (list :id next-id
                            :kind 'call
                            :block-id (nelisp-cc--ssa-block-id blk)
                            :instr-id iid
                            :pc-offset pc
                            :live-roots (bitmap live)
                            :frame-size (or frame-size 0))
                      call-sps)
                (cl-incf next-id)))))))
    (list :gc-metadata-version nelisp-cc-runtime-gc-metadata-version
          :function-name (nelisp-cc--ssa-function-name function)
          :safe-points (append sps (nreverse call-sps)))))

;;; Tail-call lowering (byte-level post-process pass) ---------------
;;
;; The proper-tail-call rewrite recognises the trailing tail position
;; from the *bytes* the backend produced and patches it in place.  We
;; do not modify the SSA IR (the backends are frozen) and we do not
;; modify the backend (likewise frozen).  Instead, the runtime layer
;; runs after `nelisp-cc-x86_64-compile-with-meta' / `nelisp-cc-arm64-
;; compile' and recognises the canonical trailing pattern:
;;
;;   x86_64:
;;     ... 0xE8 d0 d1 d2 d3              ; CALL rel32 (5 bytes)
;;         [REX.W 0x89 modrm]            ; optional MOV ret-reg, X
;;                                       ;   (return value harvest)
;;         0xC3                          ; RET (1 byte)
;;     →
;;     ... 0xE9 d0 d1 d2 d3              ; JMP rel32 (5 bytes)
;;     and the trailing MOV + RET are dropped (the callee's RET
;;     returns directly to the *grandparent* frame).
;;
;;   arm64:
;;     ...  W = 0x9400_xxxx              ; BL #imm26
;;          [W = 0xAA xx 03 Ed]          ; optional MOV X0, X (canonical
;;                                       ;   alias for ORR via xzr)
;;          W = 0xD65F03C0               ; RET X30
;;     →
;;     ...  W = 0x1400_xxxx              ; B #imm26
;;     (the trailing MOV + RET are dropped.)
;;
;; The tail-call pass *opt-in*: it only fires when the SSA :call's
;; META carries `:tail t' (the frontend marks tail position; for the
;; scaffold we treat the very last :call before :return in the entry
;; block as a tail-call candidate when META has either `:tail t' or
;; the backend signals it via the trailing pattern).  The byte-level
;; recogniser is the canonical detector; the SSA flag is advisory.

(defun nelisp-cc-runtime--bytes-as-list (bytes)
  "Convert a unibyte vector / list / string into a list of integers.
Accepts whatever shape the backends happen to return.  Defensive
because Phase 7.1.2 / 7.1.3 each settled on a different return type
during scaffolding (`apply #\\='vector LIST' vs `vconcat'), and the
runtime is the integration point that has to bridge both."
  (cond
   ((listp bytes) (copy-sequence bytes))
   ((vectorp bytes) (append bytes nil))
   ((stringp bytes)
    (let ((acc nil))
      (dotimes (i (length bytes))
        (push (aref bytes i) acc))
      (nreverse acc)))
   (t (signal 'nelisp-cc-runtime-error
              (list :bytes-not-recognised (type-of bytes))))))

(defun nelisp-cc-runtime--list-to-vector (lst)
  "Inverse of `--bytes-as-list': return a fresh unibyte-friendly vector."
  (apply #'vector lst))

(defun nelisp-cc-runtime--lower-tail-call-x86_64 (bytes)
  "Rewrite the trailing CALL+RET sequence in x86_64 BYTES to JMP.

BYTES is a vector or list of integers in [0, 255].  Returns
(NEW-BYTES . REWROTE-P).  REWROTE-P is t when the pattern matched and
the rewrite was applied; nil when no eligible tail-call was found
(the bytes are returned unchanged in that case).

Pattern (bytes counted from the *end*):
  - last byte = 0xC3 (RET)
  - optional 3-byte MOV reg,reg sequence immediately before
    (REX.W 0x89 modrm — i.e. byte[-4]=0x48, byte[-3]=0x89, byte[-2]=any)
  - 5 bytes immediately before that = 0xE8 d0 d1 d2 d3 (CALL rel32)

When matched, the CALL opcode 0xE8 is patched to 0xE9 (JMP rel32);
the optional MOV and the trailing RET are *removed* (the callee's
own RET will return all the way up).  When the caller has frame-pointer
spills (Phase 7.1.5) this rewrite is unsound — the simulator pipeline
flags such cases via the `--insert-safe-points` `:frame-size` field
(non-zero), and the integration layer suppresses the rewrite."
  (let* ((lst   (nelisp-cc-runtime--bytes-as-list bytes))
         (n     (length lst))
         (vec   (nelisp-cc-runtime--list-to-vector lst)))
    (cond
     ;; Need at least 6 bytes (5-byte CALL + 1-byte RET).
     ((< n 6) (cons vec nil))
     ;; Last byte must be RET.
     ((not (= (aref vec (- n 1)) nelisp-cc-runtime--x86_64-ret-byte))
      (cons vec nil))
     (t
      ;; Try the long pattern first: CALL + MOV + RET (5 + 3 + 1 = 9 bytes).
      (let* ((has-mov (and (>= n 9)
                           (= (aref vec (- n 4)) #x48)
                           (= (aref vec (- n 3)) #x89)))
             (call-end (if has-mov (- n 4) (- n 1)))
             (call-start (- call-end 5)))
        (cond
         ((< call-start 0) (cons vec nil))
         ((not (= (aref vec call-start)
                  nelisp-cc-runtime--x86_64-call-rel32-opcode))
          (cons vec nil))
         (t
          ;; Patch CALL → JMP, then truncate the trailing MOV (if any)
          ;; and the RET.  The displacement is preserved verbatim.
          (let ((out (make-vector (+ call-start 5) 0)))
            (dotimes (i call-start) (aset out i (aref vec i)))
            (aset out call-start nelisp-cc-runtime--x86_64-jmp-rel32-opcode)
            (aset out (+ call-start 1) (aref vec (+ call-start 1)))
            (aset out (+ call-start 2) (aref vec (+ call-start 2)))
            (aset out (+ call-start 3) (aref vec (+ call-start 3)))
            (aset out (+ call-start 4) (aref vec (+ call-start 4)))
            (cons out t)))))))))

(defun nelisp-cc-runtime--arm64-word-at (vec idx)
  "Read a little-endian 4-byte instruction word from VEC starting at IDX.
Returns the unsigned 32-bit integer.  IDX must satisfy
0 ≤ IDX ≤ (length VEC) − 4 — the caller checks bounds."
  (logior (aref vec idx)
          (ash (aref vec (+ idx 1))  8)
          (ash (aref vec (+ idx 2)) 16)
          (ash (aref vec (+ idx 3)) 24)))

(defun nelisp-cc-runtime--arm64-write-word (vec idx word)
  "Write the unsigned 32-bit instruction WORD into VEC at IDX (little-endian)."
  (aset vec    idx       (logand word #xFF))
  (aset vec (+ idx 1)    (logand (ash word -8) #xFF))
  (aset vec (+ idx 2)    (logand (ash word -16) #xFF))
  (aset vec (+ idx 3)    (logand (ash word -24) #xFF))
  vec)

(defun nelisp-cc-runtime--lower-tail-call-arm64 (bytes)
  "Rewrite the trailing BL+RET sequence in arm64 BYTES to a B branch.

BYTES is a vector / list of integers in [0, 255].  Returns
(NEW-BYTES . REWROTE-P).

Pattern (instructions counted from the *end*):
  - last word = 0xD65F03C0 (RET X30)
  - optional MOV X?, X? word immediately before (top 8 bits = 0xAA,
    bottom 5 bits in the Xd slot match the canonical alias shape)
  - word immediately before that = BL (top 6 bits = 0b100101 → match
    against `--arm64-bl-pattern' under `--arm64-bl-mask')

When matched, BL is rewritten to B (clear bit 31 — the only opcode
bit that differs) and the trailing MOV + RET are removed."
  (let* ((lst (nelisp-cc-runtime--bytes-as-list bytes))
         (n   (length lst))
         (vec (nelisp-cc-runtime--list-to-vector lst)))
    (cond
     ((or (< n 8) (not (zerop (mod n 4)))) (cons vec nil))
     ((not (= (nelisp-cc-runtime--arm64-word-at vec (- n 4))
              nelisp-cc-runtime--arm64-ret-x30))
      (cons vec nil))
     (t
      ;; Try the long pattern: BL + MOV + RET.
      (let* ((mov-idx (- n 8))
             (mov-word (when (>= mov-idx 0)
                         (nelisp-cc-runtime--arm64-word-at vec mov-idx)))
             ;; Recognise MOV Xd, Xm = ORR Xd, XZR, Xm via the top
             ;; byte 0xAA and the constant `... 03 ...' in the Xn slot.
             (has-mov (and mov-word
                           (= (logand mov-word #xFF000000) #xAA000000)
                           ;; Xn=XZR=31 → the imm6/Xn nibble at bits
                           ;; [9:5] reads back as 0x1F when shifted into
                           ;; place; we only insist on the ORR opcode
                           ;; family + xzr in Xn.
                           (= (logand (ash mov-word -5) #x1F) 31)))
             (bl-idx (if has-mov (- mov-idx 4) (- n 8)))
             (bl-word (when (>= bl-idx 0)
                        (nelisp-cc-runtime--arm64-word-at vec bl-idx))))
        (cond
         ((or (null bl-word) (< bl-idx 0)) (cons vec nil))
         ((not (= (logand bl-word nelisp-cc-runtime--arm64-bl-mask)
                  nelisp-cc-runtime--arm64-bl-pattern))
          (cons vec nil))
         (t
          ;; Rewrite: BL → B (clear bit 31), drop the trailing MOV+RET.
          (let* ((b-word (logand bl-word
                                 (logxor #xFFFFFFFF
                                         (logxor
                                          nelisp-cc-runtime--arm64-bl-pattern
                                          nelisp-cc-runtime--arm64-b-pattern))))
                 (out (make-vector (+ bl-idx 4) 0)))
            (dotimes (i bl-idx) (aset out i (aref vec i)))
            (nelisp-cc-runtime--arm64-write-word out bl-idx b-word)
            (cons out t)))))))))

(defun nelisp-cc-runtime--lower-tail-call (bytes backend)
  "Apply tail-call lowering to BYTES for BACKEND (`x86_64' / `arm64').
Returns (NEW-BYTES . REWROTE-P).  Idempotent: a second call on the
already-rewritten output returns it unchanged + REWROTE-P=nil
(because the trailing RET is already gone)."
  (pcase backend
    ('x86_64 (nelisp-cc-runtime--lower-tail-call-x86_64 bytes))
    ('arm64  (nelisp-cc-runtime--lower-tail-call-arm64  bytes))
    (_ (signal 'nelisp-cc-runtime-error
               (list :unknown-backend backend)))))

;;; Function-entry / exit safe-point byte injection ----------------
;;
;; Once the backend produces bytes, the runtime layer prepends a
;; `gc-poll' stub at offset 0 (entry safe-point) and inserts another
;; immediately before the trailing RET (exit safe-point — for the
;; scaffold a single one suffices; multi-RET lambdas trip the band
;; and signal `nelisp-cc-runtime-todo').
;;
;; This cannot violate the backend's CALL fixup table because:
;;   - x86_64 fixups are recorded as absolute offsets *into the
;;     produced byte vector* by `nelisp-cc-x86_64-compile-with-meta'
;;     — they are computed pre-injection.  The runtime adjusts every
;;     fixup offset by the size of the entry stub (1 byte for x86_64,
;;     4 bytes for arm64) so the post-injection bytes still resolve.
;;   - arm64 fixups are PC-relative and emitted in the buffer's own
;;     resolve pass before the bytes leave the backend, so the runtime
;;     only has to shift the recorded *byte offsets* of the unresolved
;;     callee fixups (Phase 7.5 patch table).

(defun nelisp-cc-runtime--inject-safepoint-stubs (bytes backend)
  "Prepend an entry NOP and inject an exit NOP before the final RET.

BYTES is the backend's output (vector of integers).  BACKEND is
`x86_64' or `arm64'.  Returns the new byte vector with the stubs
embedded.  When BYTES is empty / has no terminal RET the function
returns BYTES unchanged (the safe-point sentinel will register the
omission via `--insert-safe-points' `:pc-offset' = -1)."
  (let* ((stub (nelisp-cc-runtime--emit-gc-poll-stub backend))
         (stub-len (length stub))
         (vec (nelisp-cc-runtime--list-to-vector
               (nelisp-cc-runtime--bytes-as-list bytes)))
         (n   (length vec)))
    (cond
     ((zerop n) vec)
     (t
      ;; Locate the trailing RET (last byte for x86_64, last word for
      ;; arm64).  When absent, we still inject the entry stub but
      ;; skip the exit one.
      (let* ((has-ret
              (pcase backend
                ('x86_64 (= (aref vec (- n 1))
                            nelisp-cc-runtime--x86_64-ret-byte))
                ('arm64  (and (>= n 4)
                              (= (nelisp-cc-runtime--arm64-word-at
                                  vec (- n 4))
                                 nelisp-cc-runtime--arm64-ret-x30)))))
             (ret-len (pcase backend ('x86_64 1) ('arm64 4)))
             ;; Final size = entry-stub + body (sans RET) + exit-stub + RET
             ;;            = stub-len + (n - ret-len) + stub-len + ret-len
             ;; When no RET, no exit-stub injection.
             (out-len (if has-ret
                          (+ stub-len n stub-len)
                        (+ stub-len n)))
             (out (make-vector out-len 0))
             (cursor 0))
        ;; Entry stub.
        (dolist (b stub) (aset out cursor b) (cl-incf cursor))
        (cond
         (has-ret
          ;; Body without the trailing RET.
          (dotimes (i (- n ret-len))
            (aset out cursor (aref vec i))
            (cl-incf cursor))
          ;; Exit stub.
          (dolist (b stub) (aset out cursor b) (cl-incf cursor))
          ;; The original RET.
          (dotimes (i ret-len)
            (aset out cursor (aref vec (+ (- n ret-len) i)))
            (cl-incf cursor)))
         (t
          (dotimes (i n)
            (aset out cursor (aref vec i))
            (cl-incf cursor))))
        out)))))

;;; Phase 7.5.1 FFI bridge MVP — subprocess exec ------------------
;;
;; The MVP path is a three-step shell-out:
;;
;;   1. Write a flat byte stream (unibyte string) to a temp file.
;;   2. `call-process' the `nelisp-exec-bytes' binary
;;      <tmp>'.  The subprocess mmaps PROT_EXEC, runs the bytes,
;;      prints `RESULT: <i64>' on success.
;;   3. Parse the stdout `RESULT:' line and return the integer; or
;;      bubble the (exit-code . stdout) up so the caller can decide
;;      whether to error / log / retry.
;;
;; Performance is ~10 ms per call (process spawn + read + mmap +
;; ret).  Phase 7.5 proper switches to an in-process Emacs module
;; so the same bytes execute in microseconds; for now MVP correctness
;; is what matters — the byte streams Phase 7.1.X emits get validated
;; against a real CPU rather than the simulator's audit trace alone.

(defvar nelisp-cc-runtime--this-file
  (or load-file-name buffer-file-name)
  "Path to this source file, captured at load time.
Survives `require' / batch-mode where `load-file-name' is nil at
runtime.  The locator uses this to walk up to the worktree root
when `nelisp-cc-runtime-binary-override' is nil and the
`NELISP_REPO_ROOT' environment variable is unset.")

(defun nelisp-cc-runtime--locate-runtime-bin ()
  "Locate the `nelisp-runtime' Rust binary.

Resolution order (first hit wins):

  1. `nelisp-cc-runtime-binary-override' (defcustom).
  2. `NELISP_REPO_ROOT' env var, joined with the standard Cargo
     `nelisp-runtime/target/release/nelisp-runtime' path.
  3. `locate-dominating-file' starting from this source file's
     directory, looking for a `Makefile' alongside a
     `nelisp-runtime/' directory.
  4. Same dominator search starting from `default-directory'
     (covers `make test' from the worktree root).

Signals `nelisp-cc-runtime-binary-missing' when no candidate is
executable so callers can present an actionable error (typically:
`make runtime')."
  (let* ((override nelisp-cc-runtime-binary-override)
         (env-root (getenv "NELISP_REPO_ROOT"))
         (predicate (lambda (dir)
                      (and (file-exists-p (expand-file-name "Makefile" dir))
                           (file-directory-p
                            (expand-file-name "nelisp-runtime" dir)))))
         (start-dirs (delq nil
                           (list (and nelisp-cc-runtime--this-file
                                      (file-name-directory
                                       nelisp-cc-runtime--this-file))
                                 default-directory)))
         (root (or (and env-root (file-name-as-directory env-root))
                   (cl-some (lambda (d) (locate-dominating-file d predicate))
                            start-dirs)))
         (bin (or override
                  (and root
                       (expand-file-name
                        "nelisp-runtime/target/release/nelisp-runtime"
                        root)))))
    (cond
     ((and bin (file-executable-p bin)) bin)
     (t
      (signal 'nelisp-cc-runtime-binary-missing
              (list :looked-at bin
                    :hint "run `make runtime' from the NeLisp worktree root"))))))

(defun nelisp-cc-runtime--locate-exec-bytes-bin ()
  "Locate the `nelisp-exec-bytes' subprocess bridge.

Doc 49 Phase 49.2 keeps the raw native-code execution bridge out of
the Rust-min `nelisp-runtime' binary.  Resolution checks the explicit
override first, then both workspace-level and member-local Cargo
target directories so it works with normal workspace builds and older
member-local artifacts."
  (let* ((override nelisp-cc-runtime-exec-bytes-binary-override)
         (env-root (getenv "NELISP_REPO_ROOT"))
         (predicate (lambda (dir)
                      (and (file-exists-p (expand-file-name "Makefile" dir))
                           (file-directory-p
                            (expand-file-name "nelisp-runtime-cli" dir)))))
         (start-dirs (delq nil
                           (list (and nelisp-cc-runtime--this-file
                                      (file-name-directory
                                       nelisp-cc-runtime--this-file))
                                 default-directory)))
         (root (or (and env-root (file-name-as-directory env-root))
                   (cl-some (lambda (d) (locate-dominating-file d predicate))
                            start-dirs)))
         (candidates (delq
                      nil
                      (list
                       override
                       (and root
                            (expand-file-name
                             "target/release/nelisp-exec-bytes" root))
                       (and root
                            (expand-file-name
                             "nelisp-runtime-cli/target/release/nelisp-exec-bytes"
                             root)))))
         (bin (cl-find-if #'file-executable-p candidates)))
    (cond
     (bin bin)
     (t
      (signal 'nelisp-cc-runtime-binary-missing
              (list :looked-at candidates
                    :hint "run `make runtime-cli' from the NeLisp worktree root"))))))

(defun nelisp-cc-runtime--bytes-to-unibyte-string (bytes)
  "Coerce BYTES (a list / vector of small ints 0..255) to a unibyte string.
Used to write the machine code stream to disk verbatim — every
element must round-trip through `write-region' with no encoding
mangling."
  (let* ((lst (nelisp-cc-runtime--bytes-as-list bytes))
         (str (apply #'unibyte-string lst)))
    str))

(defun nelisp-cc-runtime--exec-real (bytes &optional args)
  "Execute BYTES via the `nelisp-exec-bytes' subprocess.

BYTES is a vector / list of small ints (0..255) representing a
ready-to-run machine code stream that follows the System V AMD64 /
AAPCS64 `extern \"C\" fn(i64, i64, i64, i64, i64, i64) -> i64'
calling convention (i64 return in rax / x0).

ARGS is a list of up to six integer arguments.  Missing arguments
are zero-filled by the Rust bridge.  Passing nil preserves the old
zero-argument behavior; payloads that ignore arguments remain ABI
compatible.

Returns one of:

  (:result EXIT INT)    — exit 0, parsed integer payload
  (:error  EXIT STDOUT) — non-zero exit, raw stdout for diagnostics
  (:no-result EXIT STDOUT) — exit 0 but no `RESULT:' line found
                            (should never happen with the v1 binary,
                             included so a future protocol drift
                             surfaces deterministically)

The function is *side-effect free with respect to the worktree*: the
temp file is created in `temporary-file-directory' and unconditionally
deleted via `unwind-protect'."
  (let ((tmp-file (make-temp-file "nelisp-cc-bytes-" nil ".bin"))
        (bin (nelisp-cc-runtime--locate-exec-bytes-bin)))
    (unwind-protect
        (progn
          ;; 1. Write bytes verbatim — no coding-system conversion.
          (let ((coding-system-for-write 'binary)
                (write-region-annotate-functions nil)
                (write-region-post-annotation-function nil))
            (write-region (nelisp-cc-runtime--bytes-to-unibyte-string bytes)
                          nil tmp-file nil 'silent))
          ;; 2. Subprocess.
          (with-temp-buffer
            (when (> (length args) 6)
              (signal 'nelisp-cc-runtime-error
                      (list :too-many-exec-args (length args)
                            :max 6)))
            (dolist (arg args)
              (unless (integerp arg)
                (signal 'nelisp-cc-runtime-error
                        (list :non-integer-exec-arg arg))))
            (let* ((argv (cons tmp-file (mapcar #'number-to-string args)))
                   (exit (apply #'call-process bin nil t nil argv))
                   (output (buffer-substring-no-properties
                            (point-min) (point-max))))
              (cond
               ((and (eq exit 0)
                     (string-match "^RESULT: \\(-?[0-9]+\\)" output))
                (list :result exit
                      (string-to-number (match-string 1 output))))
               ((eq exit 0)
                (list :no-result exit output))
               (t
                (list :error exit output))))))
      (when (file-exists-p tmp-file)
        (ignore-errors (delete-file tmp-file))))))

;;; Phase 7.5.4 in-process FFI — Emacs module wrapper -------------
;;
;; `nelisp-runtime-module.so' (built by `make runtime-module') is a
;; thin C wrapper around `libnelisp_runtime.so' that lets Emacs Lisp
;; call the cdylib in-process via the Emacs module API (Emacs 25+).
;; Round-trip is ~10 µs / call vs ~1 ms for the subprocess path —
;; Doc 32 v2 §7's bench gate (≥100 tool calls/sec) becomes trivially
;; reachable, and tight test loops no longer pay process-spawn tax.
;;
;; The locator follows the same dominator-walk pattern as the binary
;; locator so `make' invocations from anywhere in the worktree still
;; find the artifact.  If the module was never built we signal
;; `nelisp-cc-runtime-module-missing' with an actionable hint, and
;; the caller is expected to either build it (`make runtime-module')
;; or fall back to the subprocess path.

(defun nelisp-cc-runtime--locate-runtime-module ()
  "Locate `nelisp-runtime-module.so' (the Phase 7.5.4 Emacs module).

Resolution order (first hit wins):

  1. `nelisp-cc-runtime-module-path' (defcustom).
  2. `NELISP_REPO_ROOT' env var, joined with the standard Cargo
     `nelisp-runtime/target/release/nelisp-runtime-module.so' path.
  3. `locate-dominating-file' starting from this source file's
     directory, then `default-directory', looking for the same
     Makefile-and-nelisp-runtime/ pair the binary locator uses.

Signals `nelisp-cc-runtime-module-missing' when no candidate file
exists so callers can present `make runtime-module' as the fix."
  (let* ((override nelisp-cc-runtime-module-path)
         (env-root (getenv "NELISP_REPO_ROOT"))
         (predicate (lambda (dir)
                      (and (file-exists-p (expand-file-name "Makefile" dir))
                           (file-directory-p
                            (expand-file-name "nelisp-runtime" dir)))))
         (start-dirs (delq nil
                           (list (and nelisp-cc-runtime--this-file
                                      (file-name-directory
                                       nelisp-cc-runtime--this-file))
                                 default-directory)))
         (root (or (and env-root (file-name-as-directory env-root))
                   (cl-some (lambda (d) (locate-dominating-file d predicate))
                            start-dirs)))
         (path (or override
                   (and root
                        (expand-file-name
                         "nelisp-runtime/target/release/nelisp-runtime-module.so"
                         root)))))
    (cond
     ((and path (file-readable-p path)) path)
     (t
      (signal 'nelisp-cc-runtime-module-missing
              (list :looked-at path
                    :hint "run `make runtime-module' from the NeLisp worktree root"))))))

(defun nelisp-cc-runtime--module-supported-p ()
  "Return non-nil when the host Emacs supports dynamic modules.

Module support requires Emacs ≥ 25 built with `--with-modules', which
populates `module-file-suffix' and exposes `module-load'.  Hosts where
either is missing fall back to the subprocess path automatically; no
external configuration is required."
  (and (boundp 'module-file-suffix)
       module-file-suffix
       (fboundp 'module-load)))

(defvar nelisp-cc-runtime--module-loaded-p nil
  "Non-nil when `nelisp-runtime-module.so' has been `module-load'-ed.

Tracked separately from `featurep' because the module's
`provide_feature' call is a side effect of `emacs_module_init' and
not under our control: we want a boolean we set ourselves so the
caching is deterministic across Emacs versions.")

(defun nelisp-cc-runtime--ensure-module-loaded ()
  "Ensure `nelisp-runtime-module' is loaded, idempotently.

Walks the locator on first call, then `module-load's the result; on
subsequent calls returns immediately (the module is process-global,
no need to re-load).  Signals `nelisp-cc-runtime-module-unsupported'
when the host Emacs lacks module support, and
`nelisp-cc-runtime-module-missing' when the .so is not on disk yet."
  (unless (nelisp-cc-runtime--module-supported-p)
    (signal 'nelisp-cc-runtime-module-unsupported
            (list :emacs-version emacs-version
                  :hint "rebuild Emacs with --with-modules")))
  (unless (or nelisp-cc-runtime--module-loaded-p
              (fboundp 'nelisp-runtime-module-exec-bytes))
    (let* ((path (nelisp-cc-runtime--locate-runtime-module))
           ;; The cdylib lives in the same `target/release' directory
           ;; as the module wrapper.  We pass its absolute path
           ;; explicitly via `nelisp-runtime-module-load-cdylib' —
           ;; Emacs `setenv' only updates `process-environment', not
           ;; libc's env, so the env-var hook on the C side is
           ;; unreliable.  This is the documented bootstrap contract.
           (so-path (expand-file-name "libnelisp_runtime.so"
                                      (file-name-directory path))))
      (module-load path)
      (when (and (fboundp 'nelisp-runtime-module-load-cdylib)
                 (file-readable-p so-path))
        (funcall (intern "nelisp-runtime-module-load-cdylib") so-path))
      (setq nelisp-cc-runtime--module-loaded-p t)))
  ;; Belt-and-suspenders: even if `featurep' lies (the C side
  ;; provides 'nelisp-runtime-module unconditionally on success), the
  ;; defun must exist or all bets are off.
  (unless (fboundp 'nelisp-runtime-module-exec-bytes)
    (signal 'nelisp-cc-runtime-module-missing
            (list :hint "module loaded but exec-bytes binding absent — stale .so?"))))

(defun nelisp-cc-runtime--exec-in-process (bytes)
  "Execute BYTES via the in-process Emacs module wrapper (Phase 7.5.4).

BYTES is a vector / list / unibyte string of small ints (0..255)
representing a ready-to-run machine code stream that follows the
System V AMD64 / AAPCS64 `extern \"C\" fn() -> i64' calling
convention (no args, i64 return in rax / x0).

Returns a tuple matching `nelisp-cc-runtime--exec-real' so callers
can branch on the same shape regardless of which mode is active:

  (:result 0 INT)        — module exec succeeded, INT is the i64 return
  (:error  -1 MSG)       — module exec signalled (error MSG)
  (:no-result -1 MSG)    — module loaded but the exec function is
                           absent (should never happen, included for
                           protocol drift surfacing)

Performance: ~10 µs / call on Linux x86_64, ~100x faster than the
subprocess path.  The module is `module-load'-ed once on first call
and cached process-globally."
  (condition-case err
      (progn
        (nelisp-cc-runtime--ensure-module-loaded)
        (if (fboundp 'nelisp-runtime-module-exec-bytes)
            (let* ((str (nelisp-cc-runtime--bytes-to-unibyte-string bytes))
                   (result (funcall (intern "nelisp-runtime-module-exec-bytes")
                                    str)))
              (list :result 0 result))
          (list :no-result -1 "nelisp-runtime-module-exec-bytes unbound")))
    (error
     (list :error -1 (error-message-string err)))))

(defun nelisp-cc-runtime--default-backend (&optional platform)
  "Pick a default backend for PLATFORM.
`linux-x86_64' → `x86_64'; everything else → `arm64'."
  (let ((p (or platform (nelisp-cc-runtime--platform-detect))))
    (pcase p
      ('linux-x86_64 'x86_64)
      ('linux-arm64  'arm64)
      ('macos-arm64  'arm64)
      (_ (signal 'nelisp-cc-runtime-todo
                 (list :no-default-backend-for p :phase '7.5))))))

(defun nelisp-cc-runtime--compile-bytes (function alloc-state backend)
  "Run the BACKEND on FUNCTION with ALLOC-STATE.  Returns the byte vector.

T43 Phase 7.5.6 — threads through the new `compile-with-link' entry
on each backend so primitive `:call' / `:closure' sites get patched
to embedded trampolines (= bytes execute end-to-end without SIGSEGV
at the bench harness layer).

The legacy `compile-with-meta' / `compile' entries are preserved for
ERT golden tests that pin specific byte sequences."
  (pcase backend
    ('x86_64
     (require 'nelisp-cc-callees)
     (let ((bytes (nelisp-cc-x86_64-compile-with-link function alloc-state)))
       (nelisp-cc-runtime--list-to-vector
        (nelisp-cc-runtime--bytes-as-list bytes))))
    ('arm64
     (require 'nelisp-cc-callees)
     (let ((bytes (nelisp-cc-arm64-compile-with-link function alloc-state)))
       (nelisp-cc-runtime--list-to-vector
        (nelisp-cc-runtime--bytes-as-list bytes))))
    (_ (signal 'nelisp-cc-runtime-error
               (list :unknown-backend backend)))))

;;; Doc 81 Stage 81.1 — entry-ABI mode dispatch ----------------------
;;
;; `nelisp-cc-runtime-compile-and-allocate' historically produced
;; `extern "C" fn(i64, ..., i64) -> i64' entry points (`:host-int'
;; ABI mode).  Doc 81 §5.1.1 adds `:trampoline-unary' for the
;; primitive-trampoline shape:
;;
;;   :host-int          extern "C" fn(i64, ..., i64) -> i64    (default)
;;   :trampoline-unary  extern "C" fn(*const Sexp, *mut Sexp) -> i64
;;
;; In the `:trampoline-unary' shape, arg0 = a *const Sexp pointer
;; (rdi / x0), arg1 = a *mut Sexp out-buffer (rsi / x1), and the
;; return value (rax / x0) carries TRAMPOLINE_OK (0) or TRAMPOLINE_ERR
;; (>0).  This is the calling convention the Cranelift `nl_jit_cons_*'
;; primitives already use, so trampoline emit lined up with Phase
;; 7.1.6.a (cons.rs takeover) keeps caller layout invariant.
;;
;; The Stage 81.1 PoC does *not* yet thread the ABI mode through to
;; backend prologue/epilogue emission — frame layout on
;; `:trampoline-unary' just records arg/out-pointer slot positions
;; for downstream stages.  The ABI mode is currently used as a
;; metadata channel that the recognition pass (Stage 81.3) consults
;; when selecting the trampoline shape for a given primitive.

(defconst nelisp-cc-runtime-trampoline-ok 0
  "Status returned in rax/x0 by a `:trampoline-unary' entry on success.

Mirrors the convention of the existing Cranelift `nl_jit_cons_car'
trampoline: out-buffer is written, then the function returns 0.
Doc 81 §5.1.1 critical decision — staying compatible with the
extant FFI shape lets Phase 7.1.6.a take over without changing the
Rust callee-side fixups.")

(defconst nelisp-cc-runtime-trampoline-err 1
  "Status returned by a `:trampoline-unary' entry on failure.

The trampoline returns this value when the input Sexp does not
match the primitive's expected variant (e.g. `car' on an Int).  The
out-buffer contents are then unspecified — callers must check the
status before reading the buffer.")

(defconst nelisp-cc-runtime--entry-abi-modes
  '(:host-int
    :trampoline-unary
    :trampoline-binary-ctor
    :trampoline-binary-mut
    :trampoline-binary-aref
    :trampoline-ternary-aset
    :trampoline-binary-float-arith
    :trampoline-unary-float
    :trampoline-binary-float-cmp
    :trampoline-format-float)
  "Allowed values for the `:entry-abi' keyword to
`nelisp-cc-runtime-compile-and-allocate'.

`:host-int' is the legacy default (`extern \"C\" fn(i64, ..., i64) -> i64').

`:trampoline-unary' (Doc 81 §5.1.1) is `extern \"C\" fn(*const Sexp,
*mut Sexp) -> i64' for unary primitive trampolines (= car / cdr /
length / etc).  arg0 = *const Sexp pointer to the read-only argument,
arg1 = *mut Sexp out-buffer the trampoline writes into; return = i64
TRAMPOLINE_OK / TRAMPOLINE_ERR status.

`:trampoline-binary-ctor' (Doc 81 §5.2.1) is `extern \"C\"
fn(*const Sexp, *const Sexp, *mut Sexp) -> i64' for binary
constructors (= cons).  arg0/arg1 = read-only inputs, arg2 = *mut
Sexp out-buffer, return = i64 status.  The trampoline allocates a
fresh box and writes the new Sexp into the out-slot.

`:trampoline-binary-mut' (Doc 81 §5.2.1) is `extern \"C\"
fn(*mut Sexp, *const Sexp) -> i64' for binary mutators (= setcar /
setcdr).  arg0 = *mut Sexp cons cell to mutate, arg1 = *const Sexp
new value; return = i64 status (= TRAMPOLINE_OK on success).  No
out-buffer slot — the caller's eval-time return value is the
unchanged arg1 (= V), so the recognition pass in Stage 81.3 just
threads V back as the SSA def of the parent `(setcar C V)' call.

`:trampoline-binary-aref' (Doc 81 §5.3.1) is `extern \"C\"
fn(*const Sexp, i64, *mut Sexp) -> i64' for vector / array indexed
reads (= aref / elt).  arg0 = *const Sexp container, arg1 = i64 raw
index (= NOT a Sexp — the recognition pass unboxes a const-folded
small-integer or threads through a known-i64 SSA def), arg2 = *mut
Sexp out-buffer; return = i64 status.  The shape mirrors the
existing Cranelift `nl_jit_access_aref' /
`nl_jit_access_elt' contracts in build-tool/src/jit/access.rs so
Phase 7.1.6.b (vector cluster takeover) can wire elisp-emit
verbatim against the same Rust trampolines.

`:trampoline-ternary-aset' (Doc 81 §5.3.1) is `extern \"C\"
fn(*const Sexp, i64, *const Sexp, *mut Sexp) -> i64' for vector /
array indexed writes (= aset).  arg0 = *const Sexp container, arg1
= i64 raw index, arg2 = *const Sexp value to store, arg3 = *mut
Sexp out-buffer; return = i64 status.  Per Emacs' `aset' contract
the trampoline writes the value into the out-slot so the eval-time
return value is (= V).  The shape mirrors `nl_jit_access_aset'.

`:trampoline-binary-float-arith' (Doc 84 §84.1, 2026-05-10) is
`extern \"C\" fn(f64, f64) -> f64' for binary Float arithmetic
(= add / sub / mul).  System V AMD64 passes the two f64 args in
xmm0/xmm1 and returns the f64 result in xmm0; arm64 AAPCS uses
d0/d1 → d0.  This is the first xmm-register-using ABI mode in the
nelisp-cc trampoline family.  Backing trampolines live in
`build-tool/src/jit/float.rs' (`nl_jit_float_{add,sub,mul}'),
invoked through the `nl-jit-call-float-float' bridge primitive.

`:trampoline-unary-float' (Doc 87 §5, 2026-05-10) is
`extern \"C\" fn(f64) -> f64' for unary Float math primitives
(= float / exp / log).  System V AMD64 passes the f64 arg in xmm0
and returns the f64 result in xmm0; arm64 AAPCS uses d0 → d0.  The
trampoline body is the only xmm-using ABI shape that survives the
plain `--emit-prologue' / `--lower-return' frame layout untouched
(= int-class register pushes don't disturb xmm0/v0).  Backing
trampolines live in `build-tool/src/jit/math.rs'
(`nl_jit_float_{float,exp,log}'), invoked through the
`nl-jit-call-float-unary' bridge primitive (Doc 87 §5.3).

`:trampoline-binary-float-cmp' (Doc 84 §84.1, 2026-05-10) is
`extern \"C\" fn(f64, f64) -> i64' for binary Float comparisons
(= = / < / > / <= / >=).  args in xmm0/xmm1, i64 result (0 or 1)
in rax/x0.  The `=` arm uses an `1e-15' epsilon to mirror the
deleted `bi_num_eq2_float'.  Backing trampolines live in
`build-tool/src/jit/float.rs' (`nl_jit_float_{eq_eps,lt,gt,le,ge}'),
invoked through the `nl-jit-call-float-cmp' bridge primitive.

`:trampoline-format-float' (Doc 86 §86.1.e / Doc 87 §5.1, 2026-05-10)
is `extern \"C\" fn(f64, char, i64, *mut Sexp) -> i64' for the
`format' float-conversion body builder (= ?f / ?F / ?e / ?E / ?g /
?G).  arg0 = f64 magnitude in xmm0, arg1 = i64 conv codepoint in
rsi, arg2 = i64 precision in rdx, arg3 = *mut Sexp out-buffer in
rcx; return = i64 status.  The trampoline writes a fresh
`Sexp::Str' (= unsigned, unpadded body) into the out-slot.  Sole
consumer is `lisp/nelisp-stdlib-format.el' `nelisp--format-float-
body'.  Like the two `:trampoline-binary-float-*' modes above, the
cc backend never emits an entry of this shape — the path is
bridge-only via the `nl-jit-call-format-float' primitive in
`bridge.rs', so registering the keyword here is for documentation
/ validation symmetry.")

(defun nelisp-cc-runtime--validate-entry-abi (mode)
  "Signal `nelisp-cc-runtime-error' if MODE is not a valid entry-ABI keyword."
  (unless (memq mode nelisp-cc-runtime--entry-abi-modes)
    (signal 'nelisp-cc-runtime-error
            (list :unknown-entry-abi mode
                  :valid nelisp-cc-runtime--entry-abi-modes))))

(defun nelisp-cc-runtime-aot-root-vector-from-slots (frame-values root-slots)
  "Build an AOT root vector from FRAME-VALUES at ROOT-SLOTS.
FRAME-VALUES is the boundary-visible frame vector for a compiled
function invocation.  ROOT-SLOTS is the `:slots' list from a Phase 47
GC root descriptor.  The returned vector is suitable for
`nelisp-cc-runtime-call-with-aot-roots'."
  (unless (vectorp frame-values)
    (signal 'nelisp-cc-runtime-error
            (list :aot-frame-values-not-vector frame-values)))
  (unless (listp root-slots)
    (signal 'nelisp-cc-runtime-error
            (list :aot-root-slots-not-list root-slots)))
  (vconcat
   (mapcar
    (lambda (slot)
      (unless (and (integerp slot)
                   (<= 0 slot)
                   (< slot (length frame-values)))
        (signal 'nelisp-cc-runtime-error
                (list :aot-root-slot-out-of-range
                      slot :frame-size (length frame-values))))
      (aref frame-values slot))
    root-slots)))

(defun nelisp-cc-runtime-aot-root-vector-from-frame-slots
    (frames root-slots)
  "Build an AOT root vector from hash-table FRAMES at ROOT-SLOTS.
ROOT-SLOTS may contain integer frame slots or symbol keys.  This is the
runtime-side substrate for non-boundary native frames that expose their
live values through the shared FRAMES handle instead of a caller-owned
frame vector."
  (unless (hash-table-p frames)
    (signal 'nelisp-cc-runtime-error
            (list :aot-frame-roots-frames-not-hash-table frames)))
  (unless (listp root-slots)
    (signal 'nelisp-cc-runtime-error
            (list :aot-frame-root-slots-not-list root-slots)))
  (vconcat
   (mapcar
    (lambda (slot)
      (unless (or (integerp slot) (symbolp slot))
        (signal 'nelisp-cc-runtime-error
                (list :aot-frame-root-slot-not-key slot)))
      (let ((missing (list :missing slot)))
        (let ((value (gethash slot frames missing)))
          (when (eq value missing)
            (signal 'nelisp-cc-runtime-error
                    (list :aot-frame-root-slot-missing slot)))
          value)))
    root-slots)))

(defun nelisp-cc-runtime-call-with-aot-roots (roots thunk)
  "Call THUNK while ROOTS is registered as an active AOT GC frame.
This is the Emacs-side call-boundary hook for Doc 129.5C.  Native
prologue/epilogue emission can later lower to equivalent push/pop
hooks, but the dynamic extent and root-set contract are fixed here."
  (unless (vectorp roots)
    (signal 'nelisp-cc-runtime-error
            (list :aot-roots-not-vector roots)))
  (unless (functionp thunk)
    (signal 'nelisp-cc-runtime-error
            (list :aot-root-thunk-not-function thunk)))
  (nelisp-gc--with-active-aot-frame roots
    (funcall thunk)))

(defun nelisp-cc-runtime-aot-materialize-roots-boundary
    (mirror frames count out scratch &rest roots)
  "Runtime bridge for Doc 129.5 root-vector materialisation.
MIRROR, FRAMES, COUNT, OUT, SCRATCH, and ROOTS mirror the native ABI:

  nelisp_aot_materialize_roots(mirror, frames, count, out, scratch,
                              root0, root1, ...)

COUNT must match the number of ROOTS.  The bridge returns a fresh
vector containing ROOTS, writes it to OUT[0], and leaves push/pop stack
registration to `nelisp-cc-runtime-aot-push-roots-boundary'."
  (unless (and (integerp count) (<= 0 count))
    (signal 'nelisp-cc-runtime-error
            (list :aot-materialize-roots-bad-count count)))
  (unless (= count (length roots))
    (signal 'nelisp-cc-runtime-error
            (list :aot-materialize-roots-count-mismatch
                  :count count :roots (length roots))))
  (unless (and (vectorp out) (> (length out) 0))
    (signal 'nelisp-cc-runtime-error
            (list :aot-materialize-roots-out-not-vector out)))
  (ignore mirror frames scratch)
  (let ((root-vector (vconcat roots)))
    (aset out 0 root-vector)
    root-vector))

(defun nelisp-cc-runtime-aot-materialize-frame-roots-boundary
    (mirror frames count out scratch &rest root-slots)
  "Runtime bridge for Doc 129.5 hash-FRAMES root materialisation.
MIRROR, FRAMES, COUNT, OUT, SCRATCH, and ROOT-SLOTS mirror:

  nelisp_aot_materialize_frame_roots(mirror, frames, count,
                                    out, scratch, root_slot...)

COUNT must match the number of ROOT-SLOTS.  The bridge reads each root
slot from hash-table FRAMES, writes the fresh root vector to OUT[0],
and returns that vector."
  (unless (and (integerp count) (<= 0 count))
    (signal 'nelisp-cc-runtime-error
            (list :aot-materialize-frame-roots-bad-count count)))
  (unless (= count (length root-slots))
    (signal 'nelisp-cc-runtime-error
            (list :aot-materialize-frame-roots-count-mismatch
                  :count count :root-slots (length root-slots))))
  (unless (and (vectorp out) (> (length out) 0))
    (signal 'nelisp-cc-runtime-error
            (list :aot-materialize-frame-roots-out-not-vector out)))
  (ignore mirror scratch)
  (let ((root-vector
         (nelisp-cc-runtime-aot-root-vector-from-frame-slots
          frames root-slots)))
    (aset out 0 root-vector)
    root-vector))

(defun nelisp-cc-runtime-aot-root-stack-snapshot ()
  "Return a shallow snapshot of the active Doc 129.5 AOT root stack."
  (copy-sequence nelisp-gc--active-aot-frames))

(defun nelisp-cc-runtime-aot-reset-root-stack ()
  "Clear the Doc 129.5 AOT root stack and return nil."
  (setq nelisp-gc--active-aot-frames nil))

(defun nelisp-cc-runtime-aot-push-roots-boundary
    (mirror frames roots out scratch &optional dispatcher)
  "Runtime bridge for the Doc 129.5 `nelisp_aot_push_roots' ABI.
MIRROR, FRAMES, ROOTS, OUT, and SCRATCH mirror the native ABI:

  nelisp_aot_push_roots(mirror, frames, roots, out, scratch)

ROOTS must be a vector containing the live Sexp roots for one native
Phase 47 frame.  The bridge pushes ROOTS onto the active AOT root stack,
writes ROOTS to OUT[0], and returns OUT.

DISPATCHER, when non-nil, is called as `(DISPATCHER ROOTS CONTEXT)'."
  (unless (vectorp roots)
    (signal 'nelisp-cc-runtime-error
            (list :aot-push-roots-not-vector roots)))
  (unless (and (vectorp out) (> (length out) 0))
    (signal 'nelisp-cc-runtime-error
            (list :aot-push-roots-out-not-vector out)))
  (when (and dispatcher (not (functionp dispatcher)))
    (signal 'nelisp-cc-runtime-error
            (list :aot-push-roots-dispatcher-not-function dispatcher)))
  (let* ((context (list :mirror mirror
                        :frames frames
                        :roots roots
                        :out out
                        :scratch scratch))
         (pushed (if dispatcher
                     (funcall dispatcher roots context)
                   (nelisp-gc--push-active-aot-frame roots))))
    (unless (eq pushed roots)
      (signal 'nelisp-cc-runtime-error
              (list :aot-push-roots-dispatcher-mismatch
                    :expected roots :actual pushed)))
    (aset out 0 roots)
    out))

(defun nelisp-cc-runtime-aot-pop-roots-boundary
    (mirror frames expected-roots out scratch &optional dispatcher)
  "Runtime bridge for the Doc 129.5 `nelisp_aot_pop_roots' ABI.
MIRROR, FRAMES, EXPECTED-ROOTS, OUT, and SCRATCH mirror the native ABI:

  nelisp_aot_pop_roots(mirror, frames, expected_roots, out, scratch)

The bridge pops the innermost AOT root vector, verifies that it is
EXPECTED-ROOTS when EXPECTED-ROOTS is non-nil, writes the popped vector
to OUT[0], and returns OUT.

DISPATCHER, when non-nil, is called as
`(DISPATCHER EXPECTED-ROOTS CONTEXT)'."
  (when (and expected-roots (not (vectorp expected-roots)))
    (signal 'nelisp-cc-runtime-error
            (list :aot-pop-roots-expected-not-vector expected-roots)))
  (unless (and (vectorp out) (> (length out) 0))
    (signal 'nelisp-cc-runtime-error
            (list :aot-pop-roots-out-not-vector out)))
  (when (and dispatcher (not (functionp dispatcher)))
    (signal 'nelisp-cc-runtime-error
            (list :aot-pop-roots-dispatcher-not-function dispatcher)))
  (let* ((context (list :mirror mirror
                        :frames frames
                        :expected-roots expected-roots
                        :out out
                        :scratch scratch))
         (popped
          (condition-case err
              (if dispatcher
                  (funcall dispatcher expected-roots context)
                (nelisp-gc--pop-active-aot-frame expected-roots))
            (error
             (signal 'nelisp-cc-runtime-error
                     (list :aot-pop-roots-error err))))))
    (when (and expected-roots (not (eq popped expected-roots)))
      (signal 'nelisp-cc-runtime-error
              (list :aot-pop-roots-mismatch
                    :expected expected-roots :actual popped)))
    (aset out 0 popped)
    out))

;;; Doc 129.4C — AOT special binding substrate ----------------------

(defun nelisp-cc-runtime-aot-special-stack-snapshot ()
  "Return a shallow snapshot of the current Doc 129.4 special stack."
  (copy-sequence nelisp-cc-runtime--aot-special-stack))

(defun nelisp-cc-runtime-aot-reset-special-stack ()
  "Clear the Doc 129.4 special stack and return nil."
  (setq nelisp-cc-runtime--aot-special-stack nil))

(defun nelisp-cc-runtime-aot-push-special-boundary
    (mirror frames name value out scratch &optional dispatcher)
  "Runtime bridge for the Doc 129.4 `nelisp_aot_push_special' ABI.
MIRROR, FRAMES, NAME, VALUE, OUT, and SCRATCH mirror the native ABI:

  nelisp_aot_push_special(mirror, frames, name, value, out, scratch)

The default simulator dynamically binds NAME's host value cell to
VALUE, pushes a restore record, writes that record to OUT[0], and
returns OUT.  Native code will use the same logical record shape but
restore through the runtime environment value cell.

DISPATCHER, when non-nil, is called as
`(DISPATCHER NAME VALUE CONTEXT)'."
  (unless (symbolp name)
    (signal 'nelisp-cc-runtime-error
            (list :aot-push-special-name-not-symbol name)))
  (unless (and (vectorp out) (> (length out) 0))
    (signal 'nelisp-cc-runtime-error
            (list :aot-push-special-out-not-vector out)))
  (when (and dispatcher (not (functionp dispatcher)))
    (signal 'nelisp-cc-runtime-error
            (list :aot-push-special-dispatcher-not-function dispatcher)))
  (let* ((had-binding (boundp name))
         (old-value (when had-binding (symbol-value name)))
         (context (list :mirror mirror
                        :frames frames
                        :name name
                        :value value
                        :out out
                        :scratch scratch))
         (record (if dispatcher
                     (funcall dispatcher name value context)
                   (list :kind 'special
                         :name name
                         :had-binding had-binding
                         :old-value old-value
                         :new-value value
                         :context context))))
    (unless (and (listp record)
                 (eq (plist-get record :kind) 'special)
                 (eq (plist-get record :name) name))
      (signal 'nelisp-cc-runtime-error
              (list :aot-push-special-bad-record record)))
    (set name value)
    (push record nelisp-cc-runtime--aot-special-stack)
    (aset out 0 record)
    out))

(defun nelisp-cc-runtime-aot-pop-special-boundary
    (mirror frames expected-record out scratch &optional dispatcher)
  "Runtime bridge for the Doc 129.4 `nelisp_aot_pop_special' ABI.
MIRROR, FRAMES, EXPECTED-RECORD, OUT, and SCRATCH mirror the native ABI:

  nelisp_aot_pop_special(mirror, frames, expected_record, out, scratch)

The bridge pops the innermost special binding record, verifies it
against EXPECTED-RECORD when non-nil, restores the previous host value
cell state, writes the popped record to OUT[0], and returns OUT.

DISPATCHER, when non-nil, is called as
`(DISPATCHER EXPECTED-RECORD CONTEXT)'."
  (unless (and (vectorp out) (> (length out) 0))
    (signal 'nelisp-cc-runtime-error
            (list :aot-pop-special-out-not-vector out)))
  (when (and dispatcher (not (functionp dispatcher)))
    (signal 'nelisp-cc-runtime-error
            (list :aot-pop-special-dispatcher-not-function dispatcher)))
  (let* ((context (list :mirror mirror
                        :frames frames
                        :expected-record expected-record
                        :out out
                        :scratch scratch))
         (record (if dispatcher
                     (funcall dispatcher expected-record context)
                   (car nelisp-cc-runtime--aot-special-stack))))
    (unless record
      (signal 'nelisp-cc-runtime-error
              (list :aot-pop-special-empty)))
    (when (and expected-record
               (not (equal expected-record 0))
               (not (eq record expected-record)))
      (signal 'nelisp-cc-runtime-error
              (list :aot-pop-special-record-mismatch
                    :expected expected-record :actual record)))
    (unless dispatcher
      (pop nelisp-cc-runtime--aot-special-stack))
    (let ((name (plist-get record :name)))
      (unless (symbolp name)
        (signal 'nelisp-cc-runtime-error
                (list :aot-pop-special-name-not-symbol name)))
      (if (plist-get record :had-binding)
          (set name (plist-get record :old-value))
        (makunbound name)))
    (aset out 0 record)
    out))

(defun nelisp-cc-runtime--aot-default-builtin-dispatch1 (builtin arg)
  "Dispatch one-argument BUILTIN to ARG through the host/NeLisp function table."
  (let ((fn nil)
        (found nil))
    (when (and (boundp 'nelisp--functions)
               (hash-table-p (symbol-value 'nelisp--functions)))
      (let ((candidate (gethash builtin (symbol-value 'nelisp--functions)
                                :nelisp-cc-runtime--missing)))
        (unless (eq candidate :nelisp-cc-runtime--missing)
          (setq fn candidate
                found t))))
    (unless found
      (if (fboundp builtin)
          (setq fn (symbol-function builtin)
                found t)
        (signal 'nelisp-cc-runtime-error
                (list :aot-builtin-not-found builtin))))
    (if (fboundp 'nelisp--apply)
        (funcall (symbol-function 'nelisp--apply) fn (list arg))
      (funcall fn arg))))

(defun nelisp-cc-runtime-aot-builtin-call1
    (mirror frames name arg out scratch &optional dispatcher)
  "Runtime bridge for the Doc 129.6 `nelisp_aot_builtin_call1' ABI.
MIRROR, FRAMES, NAME, ARG, OUT, and SCRATCH mirror the native ABI order:

  nelisp_aot_builtin_call1(mirror, frames, name, arg, out, scratch)

NAME must be the builtin symbol materialized by compiled code.  OUT is
represented on the Emacs side as a caller-owned vector with at least one
slot.  The dispatcher result is written to `(aref OUT 0)' and OUT is
returned, matching the native boxed-boundary convention.

DISPATCHER, when non-nil, is called as
`(DISPATCHER NAME ARG CONTEXT)' where CONTEXT carries the six boundary
values.  When DISPATCHER is nil, host/NeLisp function lookup is used."
  (unless (symbolp name)
    (signal 'nelisp-cc-runtime-error
            (list :aot-builtin-name-not-symbol name)))
  (unless (and (vectorp out) (> (length out) 0))
    (signal 'nelisp-cc-runtime-error
            (list :aot-builtin-out-not-vector out)))
  (when (and dispatcher (not (functionp dispatcher)))
    (signal 'nelisp-cc-runtime-error
            (list :aot-builtin-dispatcher-not-function dispatcher)))
  (let* ((context (list :mirror mirror
                        :frames frames
                        :name name
                        :arg arg
                        :out out
                        :scratch scratch))
         (result (if dispatcher
                     (funcall dispatcher name arg context)
                   (nelisp-cc-runtime--aot-default-builtin-dispatch1
                    name arg))))
    (aset out 0 result)
    out))

(defun nelisp-cc-runtime--aot-default-builtin-dispatchn (builtin args)
  "Dispatch BUILTIN to ARGS through the host/NeLisp function table."
  (let ((fn nil)
        (found nil))
    (when (and (boundp 'nelisp--functions)
               (hash-table-p (symbol-value 'nelisp--functions)))
      (let ((candidate (gethash builtin (symbol-value 'nelisp--functions)
                                :nelisp-cc-runtime--missing)))
        (unless (eq candidate :nelisp-cc-runtime--missing)
          (setq fn candidate
                found t))))
    (unless found
      (if (fboundp builtin)
          (setq fn (symbol-function builtin)
                found t)
        (signal 'nelisp-cc-runtime-error
                (list :aot-builtin-not-found builtin))))
    (if (fboundp 'nelisp--apply)
        (funcall (symbol-function 'nelisp--apply) fn args)
      (apply fn args))))

(defun nelisp-cc-runtime-aot-builtin-calln
    (mirror frames name argc out scratch &rest args)
  "Runtime bridge for the Doc 129.6 `nelisp_aot_builtin_calln' ABI.
MIRROR, FRAMES, NAME, ARGC, OUT, SCRATCH, and ARGS mirror the native ABI:

  nelisp_aot_builtin_calln(mirror, frames, name, argc, out, scratch, arg...)

NAME must be the builtin symbol materialized by compiled code.  ARGC
must match the number of trailing ARGS.  OUT is an Emacs-side
caller-owned vector with at least one slot; the dispatch result is
written to OUT[0] and OUT is returned.

This bridge deliberately has no optional dispatcher argument because
every trailing value after SCRATCH is a user argument in the native ABI."
  (unless (symbolp name)
    (signal 'nelisp-cc-runtime-error
            (list :aot-builtin-name-not-symbol name)))
  (unless (and (integerp argc) (<= 0 argc))
    (signal 'nelisp-cc-runtime-error
            (list :aot-builtin-calln-bad-argc argc)))
  (unless (= argc (length args))
    (signal 'nelisp-cc-runtime-error
            (list :aot-builtin-calln-argc-mismatch
                  :argc argc :actual (length args))))
  (unless (and (vectorp out) (> (length out) 0))
    (signal 'nelisp-cc-runtime-error
            (list :aot-builtin-out-not-vector out)))
  (ignore mirror frames scratch)
  (let ((result (nelisp-cc-runtime--aot-default-builtin-dispatchn
                 name args)))
    (aset out 0 result)
    out))

(defun nelisp-cc-runtime--aot-closure-p (fn)
  "Return non-nil when FN is a canonical NeLisp closure record."
  (and (fboundp 'nelisp-closure-p)
       (funcall (symbol-function 'nelisp-closure-p) fn)))

(defun nelisp-cc-runtime--aot-closure-apply (fn args)
  "Apply canonical NeLisp closure FN to ARGS."
  (funcall (symbol-function 'nelisp-closure-apply) fn args))

(defconst nelisp-cc-runtime--aot-capture-cell-tag
  'nelisp-aot-capture-cell
  "Vector tag shared with `nelisp-special-forms' for AOT capture cells.")

(defun nelisp-cc-runtime-aot-capture-cell-p (value)
  "Return non-nil when VALUE is a Doc 129 AOT capture cell."
  (and (vectorp value)
       (<= 4 (length value))
       (eq (aref value 0)
           nelisp-cc-runtime--aot-capture-cell-tag)))

(defun nelisp-cc-runtime-aot-capture-cell
    (name value &optional writer)
  "Build a Doc 129 AOT capture cell for NAME.
VALUE is the current captured value.  WRITER, when non-nil, is called
as `(WRITER NAME NEW-VALUE)' after closure `setq' updates the cell."
  (unless (symbolp name)
    (signal 'nelisp-cc-runtime-error
            (list :aot-capture-cell-name-not-symbol name)))
  (when (and writer (not (functionp writer)))
    (signal 'nelisp-cc-runtime-error
            (list :aot-capture-cell-writer-not-function writer)))
  (vector nelisp-cc-runtime--aot-capture-cell-tag name value writer))

(defun nelisp-cc-runtime-aot-capture-cell-value (cell)
  "Return the current value stored in AOT capture CELL."
  (unless (nelisp-cc-runtime-aot-capture-cell-p cell)
    (signal 'nelisp-cc-runtime-error
            (list :aot-capture-cell-bad-cell cell)))
  (aref cell 2))

(defun nelisp-cc-runtime-aot-capture-cell-set (cell value)
  "Store VALUE in AOT capture CELL and run its writer."
  (unless (nelisp-cc-runtime-aot-capture-cell-p cell)
    (signal 'nelisp-cc-runtime-error
            (list :aot-capture-cell-bad-cell cell)))
  (aset cell 2 value)
  (let ((writer (aref cell 3)))
    (when writer
      (funcall writer (aref cell 1) value)))
  value)

(defun nelisp-cc-runtime--aot-capture-cell-frame-writer (frames name)
  "Return a capture-cell writer that stores NAME in FRAMES, or nil."
  (when (hash-table-p frames)
    (lambda (_name value)
      (puthash name value frames)
      value)))

(defun nelisp-cc-runtime-aot-capture-cell-boundary
    (mirror frames name value out scratch &optional writer)
  "Runtime bridge for the Doc 129.7 `nelisp_aot_capture_cell' ABI.
MIRROR, FRAMES, NAME, VALUE, OUT, and SCRATCH mirror the native ABI:

  nelisp_aot_capture_cell(mirror, frames, name, value, out, scratch)

The bridge builds a Doc 129 AOT capture cell for NAME and VALUE, writes
it to OUT[0], and returns OUT.  When FRAMES is a hash table, the bridge
also initializes FRAMES[NAME] and uses it as the default write-through
target.  WRITER is an optional host-side callback that overrides the
default frame writer."
  (unless (symbolp name)
    (signal 'nelisp-cc-runtime-error
            (list :aot-capture-cell-name-not-symbol name)))
  (unless (and (vectorp out) (> (length out) 0))
    (signal 'nelisp-cc-runtime-error
            (list :aot-capture-cell-out-not-vector out)))
  (ignore mirror scratch)
  (when (hash-table-p frames)
    (puthash name value frames))
  (aset out 0
        (nelisp-cc-runtime-aot-capture-cell
         name value
         (or writer
             (nelisp-cc-runtime--aot-capture-cell-frame-writer
              frames name))))
  out)

(defun nelisp-cc-runtime--aot-normalize-closure-capture (capture)
  "Return CAPTURE, unwrapping an OUT vector that holds an AOT capture cell."
  (if (and (vectorp capture)
           (> (length capture) 0)
           (nelisp-cc-runtime-aot-capture-cell-p (aref capture 0)))
      (aref capture 0)
    capture))

(defun nelisp-cc-runtime--aot-default-funcall-dispatch1 (fn arg)
  "Dispatch one-argument FN to ARG using NeLisp-aware apply when available."
  (cond
   ((nelisp-cc-runtime--aot-closure-p fn)
    (nelisp-cc-runtime--aot-closure-apply fn (list arg)))
   ((fboundp 'nelisp--apply)
    (funcall (symbol-function 'nelisp--apply) fn (list arg)))
   ((symbolp fn)
    (if (fboundp fn)
        (funcall (symbol-function fn) arg)
      (signal 'nelisp-cc-runtime-error
              (list :aot-funcall-function-not-found fn))))
   ((functionp fn)
    (funcall fn arg))
   (t
    (signal 'nelisp-cc-runtime-error
            (list :aot-funcall-not-function fn)))))

(defun nelisp-cc-runtime-aot-funcall1
    (mirror frames fn arg out scratch &optional dispatcher)
  "Runtime bridge for the Doc 129.7 `nelisp_aot_funcall1' ABI.
MIRROR, FRAMES, FN, ARG, OUT, and SCRATCH mirror the native ABI order:

  nelisp_aot_funcall1(mirror, frames, fn, arg, out, scratch)

FN is the function designator/value supplied by compiled code.  OUT is
represented on the Emacs side as a caller-owned vector with at least one
slot.  The dispatch result is written to `(aref OUT 0)' and OUT is
returned.

DISPATCHER, when non-nil, is called as
`(DISPATCHER FN ARG CONTEXT)' where CONTEXT carries the six boundary
values.  When DISPATCHER is nil, `nelisp--apply' is used if available,
with host `funcall' as the fallback."
  (unless (and (vectorp out) (> (length out) 0))
    (signal 'nelisp-cc-runtime-error
            (list :aot-funcall-out-not-vector out)))
  (when (and dispatcher (not (functionp dispatcher)))
    (signal 'nelisp-cc-runtime-error
            (list :aot-funcall-dispatcher-not-function dispatcher)))
  (let* ((context (list :mirror mirror
                        :frames frames
                        :fn fn
                        :arg arg
                        :out out
                        :scratch scratch))
         (result (if dispatcher
                     (funcall dispatcher fn arg context)
                   (nelisp-cc-runtime--aot-default-funcall-dispatch1
                    fn arg))))
    (aset out 0 result)
    out))

(defun nelisp-cc-runtime--aot-default-funcall-dispatch2 (fn arg0 arg1)
  "Dispatch two-argument FN to ARG0 and ARG1."
  (cond
   ((nelisp-cc-runtime--aot-closure-p fn)
    (nelisp-cc-runtime--aot-closure-apply fn (list arg0 arg1)))
   ((fboundp 'nelisp--apply)
    (funcall (symbol-function 'nelisp--apply) fn (list arg0 arg1)))
   ((symbolp fn)
    (if (fboundp fn)
        (funcall (symbol-function fn) arg0 arg1)
      (signal 'nelisp-cc-runtime-error
              (list :aot-funcall-function-not-found fn))))
   ((functionp fn)
    (funcall fn arg0 arg1))
   (t
    (signal 'nelisp-cc-runtime-error
            (list :aot-funcall-not-function fn)))))

(defun nelisp-cc-runtime-aot-funcall2
    (mirror frames fn arg0 arg1 out &optional dispatcher)
  "Runtime bridge for the Doc 129.7 `nelisp_aot_funcall2' ABI.
MIRROR, FRAMES, FN, ARG0, ARG1, and OUT mirror the native ABI order:

  nelisp_aot_funcall2(mirror, frames, fn, arg0, arg1, out)

This fixed-arity bridge deliberately omits SCRATCH so it stays inside
the current six-GP-argument Phase 47 extern-call budget.  OUT is an
Emacs-side caller-owned vector with at least one slot; the dispatch
result is written to `(aref OUT 0)' and OUT is returned.

DISPATCHER, when non-nil, is called as
`(DISPATCHER FN ARG0 ARG1 CONTEXT)'."
  (unless (and (vectorp out) (> (length out) 0))
    (signal 'nelisp-cc-runtime-error
            (list :aot-funcall-out-not-vector out)))
  (when (and dispatcher (not (functionp dispatcher)))
    (signal 'nelisp-cc-runtime-error
            (list :aot-funcall-dispatcher-not-function dispatcher)))
  (let* ((context (list :mirror mirror
                        :frames frames
                        :fn fn
                        :arg0 arg0
                        :arg1 arg1
                        :out out))
         (result (if dispatcher
                     (funcall dispatcher fn arg0 arg1 context)
                   (nelisp-cc-runtime--aot-default-funcall-dispatch2
                    fn arg0 arg1))))
    (aset out 0 result)
    out))

(defun nelisp-cc-runtime--aot-default-funcall-dispatch3 (fn arg0 arg1 arg2)
  "Dispatch three-argument FN to ARG0, ARG1, and ARG2."
  (cond
   ((nelisp-cc-runtime--aot-closure-p fn)
    (nelisp-cc-runtime--aot-closure-apply fn (list arg0 arg1 arg2)))
   ((fboundp 'nelisp--apply)
    (funcall (symbol-function 'nelisp--apply) fn (list arg0 arg1 arg2)))
   ((symbolp fn)
    (if (fboundp fn)
        (funcall (symbol-function fn) arg0 arg1 arg2)
      (signal 'nelisp-cc-runtime-error
              (list :aot-funcall-function-not-found fn))))
   ((functionp fn)
    (funcall fn arg0 arg1 arg2))
   (t
    (signal 'nelisp-cc-runtime-error
            (list :aot-funcall-not-function fn)))))

(defun nelisp-cc-runtime-aot-funcall3
    (mirror frames fn arg0 arg1 arg2 out &optional dispatcher)
  "Runtime bridge for the Doc 129.7 `nelisp_aot_funcall3' ABI.
MIRROR, FRAMES, FN, ARG0, ARG1, ARG2, and OUT mirror the native ABI:

  nelisp_aot_funcall3(mirror, frames, fn, arg0, arg1, arg2, out)

This fixed-arity bridge uses the first SysV stack GP argument in native
object output.  OUT is an Emacs-side caller-owned vector with at least
one slot; the dispatch result is written to `(aref OUT 0)' and OUT is
returned.

DISPATCHER, when non-nil, is called as
`(DISPATCHER FN ARG0 ARG1 ARG2 CONTEXT)'."
  (unless (and (vectorp out) (> (length out) 0))
    (signal 'nelisp-cc-runtime-error
            (list :aot-funcall-out-not-vector out)))
  (when (and dispatcher (not (functionp dispatcher)))
    (signal 'nelisp-cc-runtime-error
            (list :aot-funcall-dispatcher-not-function dispatcher)))
  (let* ((context (list :mirror mirror
                        :frames frames
                        :fn fn
                        :arg0 arg0
                        :arg1 arg1
                        :arg2 arg2
                        :out out))
         (result (if dispatcher
                     (funcall dispatcher fn arg0 arg1 arg2 context)
                   (nelisp-cc-runtime--aot-default-funcall-dispatch3
                    fn arg0 arg1 arg2))))
    (aset out 0 result)
    out))

(defun nelisp-cc-runtime-aot-funcalln
    (mirror frames fn argc out scratch &rest args)
  "Runtime bridge for the Doc 129.7 `nelisp_aot_funcalln' ABI.
MIRROR, FRAMES, FN, ARGC, OUT, SCRATCH, and ARGS mirror the native ABI:

  nelisp_aot_funcalln(mirror, frames, fn, argc, out, scratch, arg...)

ARGC is the number of following boxed Sexp args.  The bridge constructs
the rest-list visible to the dispatcher from ARGS, writes the dispatch
result to `(aref OUT 0)', and returns OUT.

This bridge deliberately has no optional dispatcher argument because
every trailing value after SCRATCH is a user argument in the native ABI."
  (unless (and (vectorp out) (> (length out) 0))
    (signal 'nelisp-cc-runtime-error
            (list :aot-funcall-out-not-vector out)))
  (unless (and (integerp argc) (<= 0 argc))
    (signal 'nelisp-cc-runtime-error
            (list :aot-funcalln-argc-not-nonnegative argc)))
  (unless (= argc (length args))
    (signal 'nelisp-cc-runtime-error
            (list :aot-funcalln-argc-mismatch
                  :argc argc
                  :got (length args))))
  (ignore mirror frames scratch)
  (let* ((args-list args)
         (result (nelisp-cc-runtime--aot-default-apply-dispatch
                  fn args-list)))
    (aset out 0 result)
    out))

(defun nelisp-cc-runtime--aot-default-apply-dispatch (fn args-list)
  "Dispatch FN to ARGS-LIST using NeLisp-aware apply when available."
  (unless (listp args-list)
    (signal 'nelisp-cc-runtime-error
            (list :aot-apply-args-not-list args-list)))
  (cond
   ((nelisp-cc-runtime--aot-closure-p fn)
    (nelisp-cc-runtime--aot-closure-apply fn args-list))
   ((fboundp 'nelisp--apply)
    (funcall (symbol-function 'nelisp--apply) fn args-list))
   ((symbolp fn)
    (if (fboundp fn)
        (apply (symbol-function fn) args-list)
      (signal 'nelisp-cc-runtime-error
              (list :aot-apply-function-not-found fn))))
   ((functionp fn)
    (apply fn args-list))
   (t
    (signal 'nelisp-cc-runtime-error
            (list :aot-apply-not-function fn)))))

(defun nelisp-cc-runtime-aot-apply
    (mirror frames fn args-list out scratch &optional dispatcher)
  "Runtime bridge for the Doc 129.7 `nelisp_aot_apply' ABI.
MIRROR, FRAMES, FN, ARGS-LIST, OUT, and SCRATCH mirror the native ABI:

  nelisp_aot_apply(mirror, frames, fn, args_list, out, scratch)

ARGS-LIST is the already-materialized argument list supplied by compiled
code.  OUT is an Emacs-side caller-owned vector with at least one slot;
the dispatch result is written to `(aref OUT 0)' and OUT is returned.

DISPATCHER, when non-nil, is called as
`(DISPATCHER FN ARGS-LIST CONTEXT)'."
  (unless (and (vectorp out) (> (length out) 0))
    (signal 'nelisp-cc-runtime-error
            (list :aot-apply-out-not-vector out)))
  (when (and dispatcher (not (functionp dispatcher)))
    (signal 'nelisp-cc-runtime-error
            (list :aot-apply-dispatcher-not-function dispatcher)))
  (let* ((context (list :mirror mirror
                        :frames frames
                        :fn fn
                        :args-list args-list
                        :out out
                        :scratch scratch))
         (result (if dispatcher
                     (funcall dispatcher fn args-list context)
                    (nelisp-cc-runtime--aot-default-apply-dispatch
                     fn args-list))))
    (aset out 0 result)
    out))

(defun nelisp-cc-runtime-aot-applyn
    (mirror frames fn argc out scratch &rest args)
  "Runtime bridge for the Doc 129.7 `nelisp_aot_applyn' ABI.
MIRROR, FRAMES, FN, ARGC, OUT, SCRATCH, and ARGS mirror the native ABI:

  nelisp_aot_applyn(mirror, frames, fn, argc, out, scratch, arg...)

ARGC is the number of following apply arguments.  The final apply
argument must be the list tail; any preceding ARGS are spliced in front
of that list before dispatch."
  (unless (and (vectorp out) (> (length out) 0))
    (signal 'nelisp-cc-runtime-error
            (list :aot-applyn-out-not-vector out)))
  (unless (and (integerp argc) (< 0 argc))
    (signal 'nelisp-cc-runtime-error
            (list :aot-applyn-argc-not-positive argc)))
  (unless (= argc (length args))
    (signal 'nelisp-cc-runtime-error
            (list :aot-applyn-argc-mismatch
                  :argc argc
                  :got (length args))))
  (let* ((tail (car (last args)))
         (fixed (butlast args)))
    (unless (listp tail)
      (signal 'nelisp-cc-runtime-error
              (list :aot-applyn-tail-not-list tail)))
    (ignore mirror frames scratch)
    (let ((result (nelisp-cc-runtime--aot-default-apply-dispatch
                   fn (append fixed tail))))
      (aset out 0 result)
      out)))

(defun nelisp-cc-runtime-aot-listn
    (mirror frames argc out scratch &rest args)
  "Runtime bridge for the Doc 129.7 `nelisp_aot_listn' ABI.
MIRROR, FRAMES, ARGC, OUT, SCRATCH, and ARGS mirror the native ABI:

  nelisp_aot_listn(mirror, frames, argc, out, scratch, arg...)

ARGC is the number of following boxed Sexp args.  The bridge writes the
newly constructed rest list to `(aref OUT 0)' and returns OUT."
  (unless (and (vectorp out) (> (length out) 0))
    (signal 'nelisp-cc-runtime-error
            (list :aot-listn-out-not-vector out)))
  (unless (and (integerp argc) (<= 0 argc))
    (signal 'nelisp-cc-runtime-error
            (list :aot-listn-argc-not-nonnegative argc)))
  (unless (= argc (length args))
    (signal 'nelisp-cc-runtime-error
            (list :aot-listn-argc-mismatch
                  :argc argc
                  :got (length args))))
  (ignore mirror frames scratch)
  (aset out 0 args)
  out)

;;; Doc 129.8A — AOT exception handler-stack substrate ---------------

(defun nelisp-cc-runtime-aot-handler-stack-snapshot ()
  "Return a shallow snapshot of the current Doc 129.8 AOT handler stack."
  (copy-sequence nelisp-cc-runtime--aot-handler-stack))

(defun nelisp-cc-runtime-aot-reset-handler-stack ()
  "Clear the Doc 129.8 AOT handler stack and return nil."
  (setq nelisp-cc-runtime--aot-handler-stack nil))

(defun nelisp-cc-runtime--aot-push-handler (handler)
  "Push HANDLER on the AOT handler stack and return HANDLER."
  (push handler nelisp-cc-runtime--aot-handler-stack)
  handler)

(defun nelisp-cc-runtime-aot-push-catch
    (tag landing-pad saved-sp &optional metadata)
  "Push a Doc 129.8 catch handler.
TAG is compared with `eq' by `nelisp-cc-runtime-aot-throw'.
LANDING-PAD and SAVED-SP are opaque simulator values that mirror the
native landing address and stack pointer restore point.  METADATA is
threaded through unchanged for compiler/runtime diagnostics."
  (nelisp-cc-runtime--aot-push-handler
   (list :kind 'catch
         :tag tag
         :landing-pad landing-pad
         :saved-sp saved-sp
         :metadata metadata)))

(defun nelisp-cc-runtime-aot-push-condition
    (conditions landing-pad saved-sp &optional metadata)
  "Push a Doc 129.8 condition handler.
CONDITIONS is a symbol, a list of symbols, or t, matching the handler
specifier accepted by `condition-case'."
  (unless (or (eq conditions t)
              (symbolp conditions)
              (and (listp conditions)
                   (cl-every #'symbolp conditions)))
    (signal 'nelisp-cc-runtime-error
            (list :aot-condition-handler-bad-conditions conditions)))
  (nelisp-cc-runtime--aot-push-handler
   (list :kind 'condition
         :conditions conditions
         :landing-pad landing-pad
         :saved-sp saved-sp
         :metadata metadata)))

(defun nelisp-cc-runtime-aot-push-unwind
    (cleanup landing-pad saved-sp &optional metadata)
  "Push a Doc 129.8 unwind-protect cleanup handler.
CLEANUP must be a function called with one context plist when a
non-local exit crosses this handler.  The context includes `:reason',
`:tag', `:data', and `:handler' when applicable."
  (unless (functionp cleanup)
    (signal 'nelisp-cc-runtime-error
            (list :aot-unwind-cleanup-not-function cleanup)))
  (nelisp-cc-runtime--aot-push-handler
   (list :kind 'unwind
         :cleanup cleanup
         :landing-pad landing-pad
         :saved-sp saved-sp
         :metadata metadata)))

(defun nelisp-cc-runtime-aot-pop-handler (&optional expected-kind)
  "Pop and return the innermost AOT handler.
When EXPECTED-KIND is non-nil, signal if the popped handler has a
different `:kind'."
  (unless nelisp-cc-runtime--aot-handler-stack
    (signal 'nelisp-cc-runtime-error
            (list :aot-handler-pop-empty)))
  (let ((handler (pop nelisp-cc-runtime--aot-handler-stack)))
    (when (and expected-kind
               (not (eq (plist-get handler :kind) expected-kind)))
      (signal 'nelisp-cc-runtime-error
              (list :aot-handler-pop-kind-mismatch
                    :expected expected-kind
                    :actual (plist-get handler :kind))))
    handler))

(defun nelisp-cc-runtime--aot-run-cleanup (handler context)
  "Run HANDLER's cleanup with CONTEXT and return its result."
  (let ((cleanup (plist-get handler :cleanup)))
    (unless (functionp cleanup)
      (signal 'nelisp-cc-runtime-error
              (list :aot-unwind-cleanup-not-function cleanup)))
    (funcall cleanup (append context (list :handler handler)))))

(defun nelisp-cc-runtime-aot-push-catch-boundary
    (mirror frames tag landing-pad saved-sp scratch &optional dispatcher)
  "Runtime bridge for the Doc 129.8 `nelisp_aot_push_catch' ABI.
MIRROR, FRAMES, TAG, LANDING-PAD, SAVED-SP, and SCRATCH mirror the
native ABI:

  nelisp_aot_push_catch(mirror, frames, tag, landing_pad, saved_sp, scratch)

The bridge pushes a catch handler on the process-local AOT handler
stack and returns the handler descriptor.

DISPATCHER, when non-nil, is called as
`(DISPATCHER TAG LANDING-PAD SAVED-SP CONTEXT)'."
  (when (and dispatcher (not (functionp dispatcher)))
    (signal 'nelisp-cc-runtime-error
            (list :aot-push-catch-dispatcher-not-function dispatcher)))
  (let ((context (list :mirror mirror
                       :frames frames
                       :tag tag
                       :landing-pad landing-pad
                       :saved-sp saved-sp
                       :scratch scratch)))
    (if dispatcher
        (funcall dispatcher tag landing-pad saved-sp context)
      (nelisp-cc-runtime-aot-push-catch
       tag landing-pad saved-sp context))))

(defun nelisp-cc-runtime-aot-push-condition-boundary
    (mirror frames conditions landing-pad saved-sp scratch &optional dispatcher)
  "Runtime bridge for the Doc 129.8 `nelisp_aot_push_condition' ABI.
MIRROR, FRAMES, CONDITIONS, LANDING-PAD, SAVED-SP, and SCRATCH mirror
the native ABI:

  nelisp_aot_push_condition(mirror, frames, conditions, landing_pad, saved_sp, scratch)

CONDITIONS is forwarded to `nelisp-cc-runtime-aot-push-condition'.
DISPATCHER, when non-nil, is called as
`(DISPATCHER CONDITIONS LANDING-PAD SAVED-SP CONTEXT)'."
  (when (and dispatcher (not (functionp dispatcher)))
    (signal 'nelisp-cc-runtime-error
            (list :aot-push-condition-dispatcher-not-function dispatcher)))
  (let ((context (list :mirror mirror
                       :frames frames
                       :conditions conditions
                       :landing-pad landing-pad
                       :saved-sp saved-sp
                       :scratch scratch)))
    (if dispatcher
        (funcall dispatcher conditions landing-pad saved-sp context)
      (nelisp-cc-runtime-aot-push-condition
       conditions landing-pad saved-sp context))))

(defun nelisp-cc-runtime-aot-push-unwind-boundary
    (mirror frames cleanup landing-pad saved-sp scratch &optional dispatcher)
  "Runtime bridge for the Doc 129.8 `nelisp_aot_push_unwind' ABI.
MIRROR, FRAMES, CLEANUP, LANDING-PAD, SAVED-SP, and SCRATCH mirror the
native ABI:

  nelisp_aot_push_unwind(mirror, frames, cleanup, landing_pad, saved_sp, scratch)

CLEANUP is forwarded to `nelisp-cc-runtime-aot-push-unwind'.  DISPATCHER,
when non-nil, is called as
`(DISPATCHER CLEANUP LANDING-PAD SAVED-SP CONTEXT)'."
  (when (and dispatcher (not (functionp dispatcher)))
    (signal 'nelisp-cc-runtime-error
            (list :aot-push-unwind-dispatcher-not-function dispatcher)))
  (let ((context (list :mirror mirror
                       :frames frames
                       :cleanup cleanup
                       :landing-pad landing-pad
                       :saved-sp saved-sp
                       :scratch scratch)))
    (if dispatcher
        (funcall dispatcher cleanup landing-pad saved-sp context)
      (nelisp-cc-runtime-aot-push-unwind
       cleanup landing-pad saved-sp context))))

(defun nelisp-cc-runtime-aot-pop-handler-boundary
    (mirror frames expected-kind out scratch &optional dispatcher)
  "Runtime bridge for the Doc 129.8 `nelisp_aot_pop_handler' ABI.
MIRROR, FRAMES, EXPECTED-KIND, OUT, and SCRATCH mirror the native ABI:

  nelisp_aot_pop_handler(mirror, frames, expected_kind, out, scratch)

The bridge pops the innermost AOT handler, writes the popped descriptor
to OUT, and returns OUT.  EXPECTED-KIND is forwarded to
`nelisp-cc-runtime-aot-pop-handler' and may be nil to skip kind
checking.

DISPATCHER, when non-nil, is called as
`(DISPATCHER EXPECTED-KIND CONTEXT)'."
  (unless (and (vectorp out) (> (length out) 0))
    (signal 'nelisp-cc-runtime-error
            (list :aot-pop-handler-out-not-vector out)))
  (when (and dispatcher (not (functionp dispatcher)))
    (signal 'nelisp-cc-runtime-error
            (list :aot-pop-handler-dispatcher-not-function dispatcher)))
  (let* ((context (list :mirror mirror
                        :frames frames
                        :expected-kind expected-kind
                        :out out
                        :scratch scratch))
         (handler (if dispatcher
                      (funcall dispatcher expected-kind context)
                    (nelisp-cc-runtime-aot-pop-handler expected-kind))))
    (aset out 0 handler)
    out))

(defun nelisp-cc-runtime--aot-condition-match-p (spec tag)
  "Return non-nil when condition handler SPEC catches TAG."
  (cond
   ((eq spec t) t)
   ((symbolp spec)
    (if (fboundp 'condition-of-p)
        (funcall (symbol-function 'condition-of-p) spec tag)
      (or (eq spec tag)
          (and (eq spec 'error) (not (eq tag 'quit))))))
   ((listp spec)
    (catch 'matched
      (dolist (s spec)
        (when (nelisp-cc-runtime--aot-condition-match-p s tag)
          (throw 'matched t)))
      nil))
   (t nil)))

(defun nelisp-cc-runtime--aot-landing-descriptor
    (handler reason payload cleanups)
  "Build a simulator landing descriptor for HANDLER."
  (append (list :kind (plist-get handler :kind)
                :landing-pad (plist-get handler :landing-pad)
                :saved-sp (plist-get handler :saved-sp)
                :metadata (plist-get handler :metadata)
                :reason reason
                :cleanups (nreverse cleanups))
          payload))

(defun nelisp-cc-runtime-aot-throw (tag value)
  "Simulate Doc 129.8 non-local `(throw TAG VALUE)'.
Walks the innermost-first handler stack, runs crossed unwind cleanups,
pops all crossed handlers plus the matching catch, and returns a landing
descriptor.  If no matching catch exists, crossed cleanups still run and
`nelisp-cc-runtime-error' is signalled with a `:aot-no-catch' payload."
  (let ((rest nelisp-cc-runtime--aot-handler-stack)
        (cleanups nil)
        (matched nil))
    (while (and rest (not matched))
      (let ((handler (car rest)))
        (pcase (plist-get handler :kind)
          ('unwind
           (push (nelisp-cc-runtime--aot-run-cleanup
                  handler
                  (list :reason 'throw :tag tag :value value))
                 cleanups)
           (setq rest (cdr rest)))
          ('catch
           (if (eq (plist-get handler :tag) tag)
               (setq matched handler
                     rest (cdr rest))
             (setq rest (cdr rest))))
          (_
           (setq rest (cdr rest))))))
    (setq nelisp-cc-runtime--aot-handler-stack rest)
    (if matched
        (nelisp-cc-runtime--aot-landing-descriptor
         matched 'throw
         (list :tag tag :value value)
         cleanups)
      (signal 'nelisp-cc-runtime-error
              (list :aot-no-catch tag value :cleanups (nreverse cleanups))))))

(defun nelisp-cc-runtime-aot-signal (tag data)
  "Simulate Doc 129.8 condition signalling.
TAG is the condition symbol and DATA is the signal data list.  Crossed
unwind handlers run before the matching condition landing descriptor is
returned.  Without a matching condition handler, the stack is unwound and
`nelisp-cc-runtime-error' is signalled."
  (unless (symbolp tag)
    (signal 'nelisp-cc-runtime-error
            (list :aot-signal-tag-not-symbol tag)))
  (let ((rest nelisp-cc-runtime--aot-handler-stack)
        (cleanups nil)
        (matched nil))
    (while (and rest (not matched))
      (let ((handler (car rest)))
        (pcase (plist-get handler :kind)
          ('unwind
           (push (nelisp-cc-runtime--aot-run-cleanup
                  handler
                  (list :reason 'signal :tag tag :data data))
                 cleanups)
           (setq rest (cdr rest)))
          ('condition
           (if (nelisp-cc-runtime--aot-condition-match-p
                (plist-get handler :conditions) tag)
               (setq matched handler
                     rest (cdr rest))
             (setq rest (cdr rest))))
          (_
           (setq rest (cdr rest))))))
    (setq nelisp-cc-runtime--aot-handler-stack rest)
    (if matched
        (nelisp-cc-runtime--aot-landing-descriptor
         matched 'signal
         (list :tag tag :data data :error (cons tag data))
         cleanups)
      (signal 'nelisp-cc-runtime-error
              (list :aot-unhandled-signal tag data
                    :cleanups (nreverse cleanups))))))

(defun nelisp-cc-runtime-aot-throw-boundary
    (mirror frames tag value out scratch &optional dispatcher)
  "Runtime bridge for the Doc 129.8 `nelisp_aot_throw' ABI.
MIRROR, FRAMES, TAG, VALUE, OUT, and SCRATCH mirror the native ABI:

  nelisp_aot_throw(mirror, frames, tag, value, out, scratch)

The elisp bridge writes the simulator landing descriptor into OUT and
returns OUT.  Native code will eventually use the same logical result to
restore the saved stack pointer and jump to the landing pad.

DISPATCHER, when non-nil, is called as
`(DISPATCHER TAG VALUE CONTEXT)'."
  (unless (and (vectorp out) (> (length out) 0))
    (signal 'nelisp-cc-runtime-error
            (list :aot-throw-out-not-vector out)))
  (when (and dispatcher (not (functionp dispatcher)))
    (signal 'nelisp-cc-runtime-error
            (list :aot-throw-dispatcher-not-function dispatcher)))
  (let* ((context (list :mirror mirror
                        :frames frames
                        :tag tag
                        :value value
                        :out out
                        :scratch scratch))
         (landing (if dispatcher
                      (funcall dispatcher tag value context)
                    (nelisp-cc-runtime-aot-throw tag value))))
    (aset out 0 landing)
    out))

(defun nelisp-cc-runtime-aot-signal-boundary
    (mirror frames tag data out scratch &optional dispatcher)
  "Runtime bridge for the Doc 129.8 `nelisp_aot_signal' ABI.
MIRROR, FRAMES, TAG, DATA, OUT, and SCRATCH mirror the native ABI:

  nelisp_aot_signal(mirror, frames, tag, data, out, scratch)

The elisp bridge writes the simulator landing descriptor into OUT and
returns OUT.  DISPATCHER, when non-nil, is called as
`(DISPATCHER TAG DATA CONTEXT)'."
  (unless (and (vectorp out) (> (length out) 0))
    (signal 'nelisp-cc-runtime-error
            (list :aot-signal-out-not-vector out)))
  (when (and dispatcher (not (functionp dispatcher)))
    (signal 'nelisp-cc-runtime-error
            (list :aot-signal-dispatcher-not-function dispatcher)))
  (let* ((context (list :mirror mirror
                        :frames frames
                        :tag tag
                        :data data
                        :out out
                        :scratch scratch))
         (landing (if dispatcher
                      (funcall dispatcher tag data context)
                    (nelisp-cc-runtime-aot-signal tag data))))
    (aset out 0 landing)
    out))

(defun nelisp-cc-runtime-aot-landing-value-boundary
    (mirror frames landing out scratch)
  "Runtime bridge for extracting a catch value from a landing descriptor.
MIRROR, FRAMES, LANDING, OUT, and SCRATCH mirror the native ABI:

  nelisp_aot_landing_value(mirror, frames, landing, out, scratch)

LANDING is either a landing descriptor plist or an OUT vector whose
first slot contains one.  The bridge writes the descriptor's `:value'
payload into OUT[0] and returns OUT."
  (unless (and (vectorp out) (> (length out) 0))
    (signal 'nelisp-cc-runtime-error
            (list :aot-landing-value-out-not-vector out)))
  (ignore mirror frames scratch)
  (let ((descriptor (if (and (vectorp landing) (> (length landing) 0))
                        (aref landing 0)
                      landing)))
    (unless (and (consp descriptor)
                 (plist-member descriptor :value))
      (signal 'nelisp-cc-runtime-error
              (list :aot-landing-value-missing descriptor)))
    (aset out 0 (plist-get descriptor :value))
    out))

(defun nelisp-cc-runtime-aot-landing-error-boundary
    (mirror frames landing out scratch)
  "Runtime bridge for extracting condition data from a landing descriptor.
MIRROR, FRAMES, LANDING, OUT, and SCRATCH mirror the native ABI:

  nelisp_aot_landing_error(mirror, frames, landing, out, scratch)

LANDING is either a landing descriptor plist or an OUT vector whose
first slot contains one.  The bridge writes the descriptor's `:error'
payload into OUT[0] and returns OUT."
  (unless (and (vectorp out) (> (length out) 0))
    (signal 'nelisp-cc-runtime-error
            (list :aot-landing-error-out-not-vector out)))
  (ignore mirror frames scratch)
  (let ((descriptor (if (and (vectorp landing) (> (length landing) 0))
                        (aref landing 0)
                      landing)))
    (unless (and (consp descriptor)
                 (plist-member descriptor :error))
      (signal 'nelisp-cc-runtime-error
              (list :aot-landing-error-missing descriptor)))
    (aset out 0 (plist-get descriptor :error))
    out))

(defun nelisp-cc-runtime--aot-error-data (args)
  "Return the signal data list for Doc 129 formatted error ARGS."
  (list
   (cond
    ((null args) "")
    ((stringp (car args)) (apply #'format args))
    (t (prin1-to-string (car args))))))

(defun nelisp-cc-runtime-aot-errorn-boundary
    (mirror frames argc out scratch &rest args)
  "Runtime bridge for the Doc 129.8 `nelisp_aot_errorn' ABI.
MIRROR, FRAMES, ARGC, OUT, SCRATCH, and ARGS mirror the native ABI:

  nelisp_aot_errorn(mirror, frames, argc, out, scratch, arg...)

ARGC is the number of following boxed Sexp args.  The bridge builds the
standard `error' signal data list, delegates to the signal boundary, and
returns OUT."
  (unless (and (vectorp out) (> (length out) 0))
    (signal 'nelisp-cc-runtime-error
            (list :aot-errorn-out-not-vector out)))
  (unless (and (integerp argc) (<= 0 argc))
    (signal 'nelisp-cc-runtime-error
            (list :aot-errorn-argc-not-nonnegative argc)))
  (unless (= argc (length args))
    (signal 'nelisp-cc-runtime-error
            (list :aot-errorn-argc-mismatch
                  :argc argc
                  :got (length args))))
  (nelisp-cc-runtime-aot-signal-boundary
   mirror frames 'error
   (nelisp-cc-runtime--aot-error-data args)
   out scratch))

(defvar nelisp-cc-runtime--aot-closure-descriptors
  (make-hash-table :test 'eq)
  "Registered Doc 129.7 AOT heap-closure descriptors.")

(defconst nelisp-cc-runtime--aot-closure-root-vectors-key
  :nelisp-aot-closure-root-vectors
  "Hash-table FRAMES key for Doc 129 AOT closure root vectors.")

(defun nelisp-cc-runtime-register-aot-closure-descriptor (descriptor)
  "Register one Doc 129.7 AOT heap-closure DESCRIPTOR.
DESCRIPTOR is a plist with `:name', `:arglist', `:body', and
`:captures'."
  (nelisp-cc-runtime--validate-aot-closure-descriptor descriptor)
  (let ((name (plist-get descriptor :name)))
    (puthash name (copy-sequence descriptor)
             nelisp-cc-runtime--aot-closure-descriptors)
    descriptor))

(defun nelisp-cc-runtime-clear-aot-closure-descriptors ()
  "Clear registered Doc 129.7 AOT heap-closure descriptors."
  (clrhash nelisp-cc-runtime--aot-closure-descriptors)
  nil)

(defun nelisp-cc-runtime-aot-closure-descriptor (name)
  "Return registered Doc 129.7 closure descriptor NAME, or nil."
  (unless (symbolp name)
    (signal 'nelisp-cc-runtime-error
            (list :aot-closure-descriptor-name-not-symbol name)))
  (let ((descriptor
         (gethash name nelisp-cc-runtime--aot-closure-descriptors)))
    (when descriptor
      (copy-sequence descriptor))))

(defun nelisp-cc-runtime-aot-closure-root-vectors (frames)
  "Return closure root vectors recorded in hash-table FRAMES."
  (unless (hash-table-p frames)
    (signal 'nelisp-cc-runtime-error
            (list :aot-closure-frames-not-hash-table frames)))
  (copy-sequence
   (gethash nelisp-cc-runtime--aot-closure-root-vectors-key frames)))

(defun nelisp-cc-runtime-clear-aot-pinned-closure-roots ()
  "Clear module-lifetime AOT closure root vectors from the GC root set."
  (nelisp-gc--clear-aot-pinned-root-vectors))

(defun nelisp-cc-runtime--aot-pin-closure-root (frames closure)
  "Pin CLOSURE through FRAMES when FRAMES is a hash-table frame handle."
  (when (hash-table-p frames)
    (let ((roots (vector closure)))
      (puthash nelisp-cc-runtime--aot-closure-root-vectors-key
               (cons roots
                     (gethash
                      nelisp-cc-runtime--aot-closure-root-vectors-key
                      frames))
               frames)
      (nelisp-gc--pin-aot-root-vector roots))))

(defun nelisp-cc-runtime--aot-closure-descriptor-name (descriptor)
  "Return the symbol NAME represented by DESCRIPTOR."
  (cond
   ((symbolp descriptor) descriptor)
   ((and (consp descriptor) (plist-get descriptor :name))
    (plist-get descriptor :name))
   (t
    (signal 'nelisp-cc-runtime-error
            (list :aot-closure-bad-descriptor descriptor)))))

(defun nelisp-cc-runtime-aot-make-closure
    (mirror frames descriptor argc out scratch &rest captures)
  "Runtime bridge for the Doc 129.7 `nelisp_aot_make_closure' ABI.
MIRROR, FRAMES, DESCRIPTOR, ARGC, OUT, SCRATCH, and CAPTURES mirror:

  nelisp_aot_make_closure(mirror, frames, descriptor, argc,
                          out, scratch, capture...)

DESCRIPTOR names a registered closure descriptor.  The bridge pairs its
`:captures' symbols with CAPTURES, builds a canonical `nelisp-closure',
writes it to OUT[0], and returns OUT."
  (unless (and (integerp argc) (<= 0 argc))
    (signal 'nelisp-cc-runtime-error
            (list :aot-closure-bad-argc argc)))
  (unless (= argc (length captures))
    (signal 'nelisp-cc-runtime-error
            (list :aot-closure-argc-mismatch
                  :argc argc :got (length captures))))
  (unless (and (vectorp out) (> (length out) 0))
    (signal 'nelisp-cc-runtime-error
            (list :aot-closure-out-not-vector out)))
  (ignore mirror scratch)
  (let* ((name (nelisp-cc-runtime--aot-closure-descriptor-name
                descriptor))
         (registered
          (or (and (consp descriptor) descriptor)
              (gethash name nelisp-cc-runtime--aot-closure-descriptors))))
    (unless registered
      (signal 'nelisp-cc-runtime-error
              (list :aot-closure-descriptor-not-found name)))
    (let* ((capture-names (plist-get registered :captures))
           (arglist (plist-get registered :arglist))
           (body (plist-get registered :body)))
      (unless (= (length capture-names) argc)
        (signal 'nelisp-cc-runtime-error
                (list :aot-closure-descriptor-capture-mismatch
                      :descriptor name
                      :expected (length capture-names)
                      :got argc)))
      (let ((closure (nelisp-closure-make
                      (cl-mapcar
                       #'cons
                       capture-names
                       (mapcar
                        #'nelisp-cc-runtime--aot-normalize-closure-capture
                        captures))
                      arglist
                      body)))
        (nelisp-cc-runtime--aot-pin-closure-root frames closure)
        (aset out 0 closure)
        out))))

;;; Doc 129.6Q — exported AOT C ABI descriptor table -----------------

(defconst nelisp-cc-runtime--aot-c-abi-descriptors
  '((:symbol nelisp_aot_materialize_roots
     :function nelisp-cc-runtime-aot-materialize-roots-boundary
     :fixed-argc 5 :rest t
     :args (mirror frames count out scratch roots...))
    (:symbol nelisp_aot_materialize_frame_roots
     :function nelisp-cc-runtime-aot-materialize-frame-roots-boundary
     :fixed-argc 5 :rest t
     :args (mirror frames count out scratch root-slots...))
    (:symbol nelisp_aot_push_roots
     :function nelisp-cc-runtime-aot-push-roots-boundary
     :fixed-argc 5 :rest nil
     :args (mirror frames roots out scratch))
    (:symbol nelisp_aot_pop_roots
     :function nelisp-cc-runtime-aot-pop-roots-boundary
     :fixed-argc 5 :rest nil
     :args (mirror frames expected-roots out scratch))
    (:symbol nelisp_aot_push_special
     :function nelisp-cc-runtime-aot-push-special-boundary
     :fixed-argc 6 :rest nil
     :args (mirror frames name value out scratch))
    (:symbol nelisp_aot_pop_special
     :function nelisp-cc-runtime-aot-pop-special-boundary
     :fixed-argc 5 :rest nil
     :args (mirror frames expected-record out scratch))
    (:symbol nelisp_aot_builtin_call1
     :function nelisp-cc-runtime-aot-builtin-call1
     :fixed-argc 6 :rest nil
     :args (mirror frames name arg out scratch))
    (:symbol nelisp_aot_builtin_calln
     :function nelisp-cc-runtime-aot-builtin-calln
     :fixed-argc 6 :rest t
     :args (mirror frames name argc out scratch args...))
    (:symbol nelisp_aot_funcall1
     :function nelisp-cc-runtime-aot-funcall1
     :fixed-argc 6 :rest nil
     :args (mirror frames fn arg out scratch))
    (:symbol nelisp_aot_funcall2
     :function nelisp-cc-runtime-aot-funcall2
     :fixed-argc 6 :rest nil
     :args (mirror frames fn arg0 arg1 out))
    (:symbol nelisp_aot_funcall3
     :function nelisp-cc-runtime-aot-funcall3
     :fixed-argc 7 :rest nil
     :args (mirror frames fn arg0 arg1 arg2 out))
    (:symbol nelisp_aot_funcalln
     :function nelisp-cc-runtime-aot-funcalln
     :fixed-argc 6 :rest t
     :args (mirror frames fn argc out scratch args...))
    (:symbol nelisp_aot_apply
     :function nelisp-cc-runtime-aot-apply
     :fixed-argc 6 :rest nil
     :args (mirror frames fn args-list out scratch))
    (:symbol nelisp_aot_applyn
     :function nelisp-cc-runtime-aot-applyn
     :fixed-argc 6 :rest t
     :args (mirror frames fn argc out scratch args...))
    (:symbol nelisp_aot_listn
     :function nelisp-cc-runtime-aot-listn
     :fixed-argc 5 :rest t
     :args (mirror frames argc out scratch args...))
    (:symbol nelisp_aot_make_closure
     :function nelisp-cc-runtime-aot-make-closure
     :fixed-argc 6 :rest t
     :args (mirror frames descriptor argc out scratch captures...))
    (:symbol nelisp_aot_capture_cell
     :function nelisp-cc-runtime-aot-capture-cell-boundary
     :fixed-argc 6 :rest nil
     :args (mirror frames name value out scratch))
    (:symbol nelisp_aot_push_catch
     :function nelisp-cc-runtime-aot-push-catch-boundary
     :fixed-argc 6 :rest nil
     :args (mirror frames tag landing-pad saved-sp scratch))
    (:symbol nelisp_aot_push_condition
     :function nelisp-cc-runtime-aot-push-condition-boundary
     :fixed-argc 6 :rest nil
     :args (mirror frames conditions landing-pad saved-sp scratch))
    (:symbol nelisp_aot_push_unwind
     :function nelisp-cc-runtime-aot-push-unwind-boundary
     :fixed-argc 6 :rest nil
     :args (mirror frames cleanup landing-pad saved-sp scratch))
    (:symbol nelisp_aot_pop_handler
     :function nelisp-cc-runtime-aot-pop-handler-boundary
     :fixed-argc 5 :rest nil
     :args (mirror frames expected-kind out scratch))
    (:symbol nelisp_aot_throw
     :function nelisp-cc-runtime-aot-throw-boundary
     :fixed-argc 6 :rest nil
     :args (mirror frames tag value out scratch))
    (:symbol nelisp_aot_signal
     :function nelisp-cc-runtime-aot-signal-boundary
     :fixed-argc 6 :rest nil
     :args (mirror frames tag data out scratch))
    (:symbol nelisp_aot_landing_value
     :function nelisp-cc-runtime-aot-landing-value-boundary
     :fixed-argc 5 :rest nil
     :args (mirror frames landing out scratch))
    (:symbol nelisp_aot_landing_error
     :function nelisp-cc-runtime-aot-landing-error-boundary
     :fixed-argc 5 :rest nil
     :args (mirror frames landing out scratch))
    (:symbol nelisp_aot_errorn
     :function nelisp-cc-runtime-aot-errorn-boundary
     :fixed-argc 5 :rest t
     :args (mirror frames argc out scratch args...)))
  "Doc 129 AOT extern C ABI descriptors known to the elisp runtime.
Each descriptor maps the symbol emitted by Phase 47 object output to
the Emacs-side bridge function that simulates the same boundary.  The
table is metadata only: real address resolution still goes through
`nelisp-cc-runtime-resolve-symbol' and the standalone runtime's dlsym
hook.")

(defvar nelisp-cc-runtime--aot-c-abi-exports
  (make-hash-table :test 'eq)
  "Installed Doc 129 AOT C ABI exports keyed by native symbol.")

(defun nelisp-cc-runtime-aot-c-abi-descriptors ()
  "Return a copy of the Doc 129 AOT C ABI descriptor table."
  (mapcar #'copy-sequence nelisp-cc-runtime--aot-c-abi-descriptors))

(defun nelisp-cc-runtime-aot-c-abi-descriptor (symbol-name)
  "Return the Doc 129 AOT C ABI descriptor for SYMBOL-NAME, or nil."
  (unless (symbolp symbol-name)
    (signal 'nelisp-cc-runtime-error
            (list :aot-c-abi-symbol-not-symbol symbol-name)))
  (let ((descriptor
         (cl-find-if (lambda (entry)
                       (eq (plist-get entry :symbol) symbol-name))
                     nelisp-cc-runtime--aot-c-abi-descriptors)))
    (when descriptor
      (copy-sequence descriptor))))

(defun nelisp-cc-runtime--validate-aot-c-abi-descriptor (descriptor)
  "Validate one Doc 129 AOT C ABI DESCRIPTOR."
  (let ((symbol (plist-get descriptor :symbol))
        (fn (plist-get descriptor :function))
        (fixed (plist-get descriptor :fixed-argc)))
    (unless (and (listp descriptor)
                 (symbolp symbol)
                 (symbolp fn)
                 (integerp fixed)
                 (<= 0 fixed)
                 (plist-member descriptor :rest)
                 (listp (plist-get descriptor :args)))
      (signal 'nelisp-cc-runtime-error
              (list :bad-aot-c-abi-descriptor descriptor)))
    (unless (fboundp fn)
      (signal 'nelisp-cc-runtime-error
              (list :aot-c-abi-function-unbound fn)))
    descriptor))

(defun nelisp-cc-runtime-validate-aot-c-abi-descriptors ()
  "Validate the Doc 129 AOT C ABI descriptor table.
Signals `nelisp-cc-runtime-error' on duplicate native symbols, malformed
arity metadata, or an Emacs bridge function that is not currently
defined.  Returns t on success."
  (let ((seen nil))
    (dolist (descriptor nelisp-cc-runtime--aot-c-abi-descriptors)
      (let ((symbol (plist-get descriptor :symbol)))
        (nelisp-cc-runtime--validate-aot-c-abi-descriptor descriptor)
        (when (memq symbol seen)
          (signal 'nelisp-cc-runtime-error
                  (list :duplicate-aot-c-abi-symbol symbol)))
        (push symbol seen)))
    t))

(defun nelisp-cc-runtime-resolve-aot-c-abi-descriptor
    (descriptor &optional resolver)
  "Resolve one Doc 129 AOT C ABI DESCRIPTOR.
RESOLVER defaults to `nelisp-cc-runtime-resolve-symbol'.  The returned
plist keeps the descriptor metadata together with `:status' and `:addr',
so host-stub and standalone dlsym-backed runtimes expose the same ABI
shape to callers."
  (nelisp-cc-runtime--validate-aot-c-abi-descriptor descriptor)
  (when (and resolver (not (functionp resolver)))
    (signal 'nelisp-cc-runtime-error
            (list :aot-c-abi-resolver-not-function resolver)))
  (let* ((symbol (plist-get descriptor :symbol))
         (status-addr (funcall (or resolver
                                   #'nelisp-cc-runtime-resolve-symbol)
                               symbol))
         (status (car-safe status-addr))
         (addr (cdr-safe status-addr)))
    (unless (consp status-addr)
      (signal 'nelisp-cc-runtime-error
              (list :aot-c-abi-resolution-bad-result
                    symbol status-addr)))
    (unless (memq status '(:resolved :host-stub :not-found))
      (signal 'nelisp-cc-runtime-error
              (list :aot-c-abi-resolution-bad-status
                    symbol status-addr)))
    (when (and (memq status '(:resolved :host-stub))
               (not (and (integerp addr) (> addr 0))))
      (signal 'nelisp-cc-runtime-error
              (list :aot-c-abi-resolution-bad-address
                    symbol status-addr)))
    (when (and (eq status :not-found) addr)
      (signal 'nelisp-cc-runtime-error
              (list :aot-c-abi-resolution-not-found-has-address
                    symbol status-addr)))
    (list :symbol symbol
          :function (plist-get descriptor :function)
          :status status
          :addr addr
          :fixed-argc (plist-get descriptor :fixed-argc)
          :rest (plist-get descriptor :rest)
          :args (copy-sequence (plist-get descriptor :args))
          :descriptor (copy-sequence descriptor))))

(defun nelisp-cc-runtime-resolve-aot-c-abi-table (&optional resolver)
  "Return the Doc 129 AOT C ABI descriptor table with resolved addresses.
Each entry is produced by
`nelisp-cc-runtime-resolve-aot-c-abi-descriptor'.  RESOLVER defaults to
`nelisp-cc-runtime-resolve-symbol', preserving the descriptor order."
  (nelisp-cc-runtime-validate-aot-c-abi-descriptors)
  (mapcar (lambda (descriptor)
            (nelisp-cc-runtime-resolve-aot-c-abi-descriptor
             descriptor resolver))
          nelisp-cc-runtime--aot-c-abi-descriptors))

(defun nelisp-cc-runtime-clear-aot-c-abi-exports ()
  "Clear installed Doc 129 AOT C ABI exports."
  (clrhash nelisp-cc-runtime--aot-c-abi-exports)
  nil)

(defun nelisp-cc-runtime-aot-c-abi-export (symbol-name)
  "Return the installed Doc 129 AOT C ABI export for SYMBOL-NAME, or nil."
  (unless (symbolp symbol-name)
    (signal 'nelisp-cc-runtime-error
            (list :aot-c-abi-export-symbol-not-symbol symbol-name)))
  (let ((entry (gethash symbol-name nelisp-cc-runtime--aot-c-abi-exports)))
    (when entry
      (copy-sequence entry))))

(defun nelisp-cc-runtime-aot-c-abi-export-table-snapshot ()
  "Return installed Doc 129 AOT C ABI exports in descriptor order."
  (let (out)
    (dolist (descriptor nelisp-cc-runtime--aot-c-abi-descriptors)
      (let* ((symbol (plist-get descriptor :symbol))
             (entry (gethash symbol
                             nelisp-cc-runtime--aot-c-abi-exports)))
        (when entry
          (push (copy-sequence entry) out))))
    (nreverse out)))

(defun nelisp-cc-runtime--validate-aot-c-abi-export-resolution
    (resolution allow-host-stub)
  "Validate RESOLUTION for installation into the AOT C ABI export table."
  (let ((symbol (plist-get resolution :symbol))
        (status (plist-get resolution :status))
        (addr (plist-get resolution :addr)))
    (unless (and (listp resolution)
                 (symbolp symbol)
                 (memq status '(:resolved :host-stub))
                 (integerp addr)
                 (> addr 0)
                 (plist-get resolution :descriptor))
      (signal 'nelisp-cc-runtime-error
              (list :bad-aot-c-abi-export-resolution resolution)))
    (when (and (eq status :host-stub) (not allow-host-stub))
      (signal 'nelisp-cc-runtime-error
              (list :aot-c-abi-export-host-stub-not-native symbol)))
    resolution))

(defun nelisp-cc-runtime-install-aot-c-abi-exports
    (&optional resolver allow-host-stub)
  "Resolve and install Doc 129 AOT C ABI exports.
RESOLVER is forwarded to `nelisp-cc-runtime-resolve-aot-c-abi-table'.
Every resolved entry must have status `:resolved'.  When
ALLOW-HOST-STUB is non-nil, host-stub addresses are accepted for
simulator inspection; standalone/native loader callers leave it nil."
  (let ((resolutions
         (nelisp-cc-runtime-resolve-aot-c-abi-table resolver)))
    (nelisp-cc-runtime-clear-aot-c-abi-exports)
    (dolist (resolution resolutions)
      (nelisp-cc-runtime--validate-aot-c-abi-export-resolution
       resolution allow-host-stub)
      (puthash (plist-get resolution :symbol)
               (copy-sequence resolution)
               nelisp-cc-runtime--aot-c-abi-exports))
    (nelisp-cc-runtime-aot-c-abi-export-table-snapshot)))

(defun nelisp-cc-runtime--validate-aot-init-helper-descriptor (descriptor)
  "Validate one Doc 129 AOT init helper DESCRIPTOR."
  (unless (and (listp descriptor)
               (memq (plist-get descriptor :kind)
                     '(defvar defconst defcustom))
               (symbolp (plist-get descriptor :name))
               (symbolp (plist-get descriptor :helper))
               (integerp (plist-get descriptor :index))
               (<= 0 (plist-get descriptor :index)))
    (signal 'nelisp-cc-runtime-error
            (list :bad-aot-init-helper-descriptor descriptor)))
  descriptor)

(defun nelisp-cc-runtime--validate-aot-custom-metadata (descriptor helpers)
  "Validate one custom metadata DESCRIPTOR against known HELPERS."
  (unless (and (listp descriptor)
               (symbolp (plist-get descriptor :name))
               (symbolp (plist-get descriptor :helper))
               (stringp (plist-get descriptor :docstring))
               (listp (plist-get descriptor :options)))
    (signal 'nelisp-cc-runtime-error
            (list :bad-aot-custom-metadata descriptor)))
  (unless (memq (plist-get descriptor :helper) helpers)
    (signal 'nelisp-cc-runtime-error
            (list :aot-custom-metadata-helper-without-init
                  (plist-get descriptor :helper))))
  descriptor)

(defun nelisp-cc-runtime--validate-aot-root-descriptor (descriptor)
  "Validate one Doc 129.5C root DESCRIPTOR."
  (unless (and (listp descriptor)
               (symbolp (plist-get descriptor :name))
               (listp (plist-get descriptor :slots))
               (integerp (plist-get descriptor :param-count))
               (<= 0 (plist-get descriptor :param-count))
               (integerp (plist-get descriptor :rt-slot-count))
               (<= 0 (plist-get descriptor :rt-slot-count)))
    (signal 'nelisp-cc-runtime-error
            (list :bad-aot-root-descriptor descriptor)))
  (dolist (slot (plist-get descriptor :slots))
    (unless (and (integerp slot)
                 (<= 0 slot)
                 (< slot (+ (plist-get descriptor :param-count)
                            (plist-get descriptor :rt-slot-count))))
      (signal 'nelisp-cc-runtime-error
              (list :bad-aot-root-slot slot :descriptor descriptor))))
  descriptor)

(defun nelisp-cc-runtime--validate-aot-closure-descriptor (descriptor)
  "Validate one Doc 129.7 AOT heap-closure DESCRIPTOR."
  (unless (and (listp descriptor)
               (symbolp (plist-get descriptor :name))
               (listp (plist-get descriptor :arglist))
               (listp (plist-get descriptor :body))
               (listp (plist-get descriptor :captures))
               (cl-every #'symbolp (plist-get descriptor :arglist))
               (cl-every #'symbolp (plist-get descriptor :captures)))
    (signal 'nelisp-cc-runtime-error
            (list :bad-aot-closure-descriptor descriptor)))
  descriptor)

(defun nelisp-cc-runtime-aot-module-init-plan
    (init-helpers &optional custom-metadata root-descriptors closure-descriptors)
  "Build the Doc 99 module-init plan for a Phase 47 AOT module.
INIT-HELPERS is the ordered list from
`nelisp-phase47-compiler--init-helper-descriptors'.  CUSTOM-METADATA
is the list from `nelisp-phase47-compiler--custom-metadata-descriptors'.
ROOT-DESCRIPTORS is the list from
`nelisp-phase47-compiler--gc-root-descriptors'.  CLOSURE-DESCRIPTORS
is the list from `nelisp-phase47-compiler--closure-descriptors'.

The returned plist is pure metadata; it does not call native code.
Doc 99's loader/dispatcher consumes `:helper-order' to run generated
init helpers, `:custom-by-helper' to attach customization metadata to
matching helpers, and `:root-descriptors' for call-boundary root
registration.  `:closure-descriptors' is registered by
`nelisp-cc-runtime-run-aot-module-init-plan' before AOT closure
materialization."
  (unless (listp init-helpers)
    (signal 'nelisp-cc-runtime-error
            (list :aot-init-helpers-not-list init-helpers)))
  (unless (listp custom-metadata)
    (signal 'nelisp-cc-runtime-error
            (list :aot-custom-metadata-not-list custom-metadata)))
  (unless (listp root-descriptors)
    (signal 'nelisp-cc-runtime-error
            (list :aot-root-descriptors-not-list root-descriptors)))
  (unless (listp closure-descriptors)
    (signal 'nelisp-cc-runtime-error
            (list :aot-closure-descriptors-not-list closure-descriptors)))
  (let* ((validated-init
          (mapcar #'nelisp-cc-runtime--validate-aot-init-helper-descriptor
                  init-helpers))
         (helpers (mapcar (lambda (d) (plist-get d :helper))
                          validated-init))
         (validated-custom
          (mapcar (lambda (d)
                    (nelisp-cc-runtime--validate-aot-custom-metadata
                     d helpers))
                  custom-metadata))
         (validated-roots
          (mapcar #'nelisp-cc-runtime--validate-aot-root-descriptor
                  root-descriptors))
         (validated-closures
          (mapcar #'nelisp-cc-runtime--validate-aot-closure-descriptor
                  closure-descriptors)))
    (list :init-helpers validated-init
          :helper-order helpers
          :custom-metadata validated-custom
          :custom-by-helper
          (mapcar (lambda (d) (cons (plist-get d :helper) d))
                  validated-custom)
          :root-descriptors validated-roots
          :closure-descriptors validated-closures)))

(defun nelisp-cc-runtime-clear-aot-custom-table ()
  "Clear the runtime AOT custom metadata table."
  (clrhash nelisp-cc-runtime--aot-custom-table)
  nil)

(defun nelisp-cc-runtime-aot-custom-metadata (name)
  "Return stored AOT custom metadata for NAME, or nil."
  (unless (symbolp name)
    (signal 'nelisp-cc-runtime-error
            (list :aot-custom-name-not-symbol name)))
  (let ((entry (gethash name nelisp-cc-runtime--aot-custom-table)))
    (when entry
      (copy-sequence entry))))

(defun nelisp-cc-runtime-aot-custom-table-snapshot ()
  "Return a deterministic snapshot of stored AOT custom metadata."
  (let (entries)
    (maphash (lambda (_name descriptor)
               (push (copy-sequence descriptor) entries))
             nelisp-cc-runtime--aot-custom-table)
    (sort entries
          (lambda (a b)
            (string< (symbol-name (plist-get a :name))
                     (symbol-name (plist-get b :name)))))))

(defun nelisp-cc-runtime-register-aot-custom-metadata
    (custom-descriptor _context init-descriptor)
  "Store CUSTOM-DESCRIPTOR in the runtime AOT custom metadata table.
INIT-DESCRIPTOR is the generated helper descriptor that just ran.  The
stored entry is metadata-only and deliberately omits the transient
call-boundary CONTEXT."
  (nelisp-cc-runtime--validate-aot-custom-metadata
   custom-descriptor (list (plist-get custom-descriptor :helper)))
  (unless (and (listp init-descriptor)
               (eq (plist-get init-descriptor :kind) 'defcustom)
               (eq (plist-get init-descriptor :name)
                   (plist-get custom-descriptor :name))
               (eq (plist-get init-descriptor :helper)
                   (plist-get custom-descriptor :helper)))
    (signal 'nelisp-cc-runtime-error
            (list :aot-custom-init-descriptor-mismatch
                  :custom custom-descriptor
                  :init init-descriptor)))
  (let ((entry (append custom-descriptor nil)))
    (setq entry (plist-put entry :init-index
                           (plist-get init-descriptor :index)))
    (setq entry (plist-put entry :init-kind
                           (plist-get init-descriptor :kind)))
    (puthash (plist-get custom-descriptor :name)
             entry
             nelisp-cc-runtime--aot-custom-table)
    (copy-sequence entry)))

(defun nelisp-cc-runtime-resolve-aot-init-helper
    (descriptor &optional resolver)
  "Resolve DESCRIPTOR's AOT init helper symbol.
RESOLVER defaults to `nelisp-cc-runtime-resolve-symbol'.  The returned
plist contains `:helper', `:status', `:addr', and `:descriptor'."
  (nelisp-cc-runtime--validate-aot-init-helper-descriptor descriptor)
  (when (and resolver (not (functionp resolver)))
    (signal 'nelisp-cc-runtime-error
            (list :aot-helper-resolver-not-function resolver)))
  (let* ((helper (plist-get descriptor :helper))
         (status-addr (funcall (or resolver
                                   #'nelisp-cc-runtime-resolve-symbol)
                               helper))
         (status (car-safe status-addr))
         (addr (cdr-safe status-addr)))
    (unless (memq status '(:resolved :host-stub :not-found))
      (signal 'nelisp-cc-runtime-error
              (list :aot-helper-resolve-bad-status
                    helper status-addr)))
    (when (and (memq status '(:resolved :host-stub))
               (not (and (integerp addr) (> addr 0))))
      (signal 'nelisp-cc-runtime-error
              (list :aot-helper-resolve-bad-address
                    helper status-addr)))
    (when (and (eq status :not-found) addr)
      (signal 'nelisp-cc-runtime-error
              (list :aot-helper-resolve-not-found-has-address
                    helper status-addr)))
    (list :helper helper
          :status status
          :addr addr
          :descriptor descriptor)))

(defun nelisp-cc-runtime-call-aot-init-helper
    (helper context descriptor &optional native-call resolver)
  "Resolve and optionally call one AOT init HELPER.
CONTEXT is the same boundary context accepted by
`nelisp-cc-runtime-run-aot-module-init-plan'.  DESCRIPTOR must be the
matching init-helper descriptor.  NATIVE-CALL, when non-nil, is called
as `(NATIVE-CALL RESOLUTION CONTEXT DESCRIPTOR)' after successful
resolution.  RESOLUTION carries `:abi-argv', the standard helper call
argument list `(OUT MIRROR FRAMES SCRATCH NAME-SLOT)'.  RESOLVER
defaults to `nelisp-cc-runtime-resolve-symbol'."
  (unless (symbolp helper)
    (signal 'nelisp-cc-runtime-error
            (list :aot-helper-not-symbol helper)))
  (nelisp-cc-runtime--validate-aot-init-context context)
  (nelisp-cc-runtime--validate-aot-init-helper-descriptor descriptor)
  (unless (eq helper (plist-get descriptor :helper))
    (signal 'nelisp-cc-runtime-error
            (list :aot-helper-descriptor-mismatch
                  :helper helper
                  :descriptor descriptor)))
  (when (and native-call (not (functionp native-call)))
    (signal 'nelisp-cc-runtime-error
            (list :aot-native-call-not-function native-call)))
  (let ((resolution
         (plist-put
          (copy-sequence
           (nelisp-cc-runtime-resolve-aot-init-helper descriptor resolver))
          :abi-argv
          (nelisp-cc-runtime-aot-init-helper-argv context))))
    (when (eq (plist-get resolution :status) :not-found)
      (signal 'nelisp-cc-runtime-error
              (list :aot-init-helper-symbol-not-found helper)))
    (if native-call
        (funcall native-call resolution context descriptor)
      resolution)))

(defun nelisp-cc-runtime-aot-init-helper-caller
    (&optional native-call resolver)
  "Return a `run-aot-module-init-plan' CALL-HELPER callback.
NATIVE-CALL and RESOLVER are forwarded to
`nelisp-cc-runtime-call-aot-init-helper'."
  (when (and native-call (not (functionp native-call)))
    (signal 'nelisp-cc-runtime-error
            (list :aot-native-call-not-function native-call)))
  (when (and resolver (not (functionp resolver)))
    (signal 'nelisp-cc-runtime-error
            (list :aot-helper-resolver-not-function resolver)))
  (lambda (helper context descriptor)
    (nelisp-cc-runtime-call-aot-init-helper
     helper context descriptor native-call resolver)))

(defun nelisp-cc-runtime-make-aot-environment-handles
    (&optional mirror frames)
  "Return Doc 129 AOT environment handles for MIRROR and FRAMES.
Callers may pass existing opaque handles.  When omitted, the runtime
allocates hash-table handles that the host-side AOT bridges can share
for frame slots, capture-cell writers, closure roots, and later loader
metadata."
  (list :mirror (or mirror (make-hash-table :test 'eq))
        :frames (or frames (make-hash-table :test 'eq))))

(defun nelisp-cc-runtime-make-aot-init-context
    (&optional mirror frames out scratch name-slot)
  "Return a default Doc 129.3 AOT init boundary context.
MIRROR and FRAMES are the environment handles forwarded to generated
init helpers.  When omitted, they default to host hash-table handles.
OUT, SCRATCH, and NAME-SLOT default to fresh one-slot vectors so callers
can run module initialization without separately allocating the standard
boxed-boundary slots."
  (let* ((handles
          (nelisp-cc-runtime-make-aot-environment-handles
           mirror frames))
         (context (list :out (or out (vector nil))
                        :mirror (plist-get handles :mirror)
                        :frames (plist-get handles :frames)
                        :scratch (or scratch (vector nil))
                        :name-slot (or name-slot (vector nil)))))
    (nelisp-cc-runtime--validate-aot-init-context context)))

(defun nelisp-cc-runtime--validate-aot-init-context (context)
  "Validate CONTEXT for running Doc 129 AOT init helpers."
  (unless (and (listp context)
               (plist-member context :out)
               (plist-member context :mirror)
               (plist-member context :frames)
               (plist-member context :scratch)
               (plist-member context :name-slot))
    (signal 'nelisp-cc-runtime-error
            (list :bad-aot-init-context context)))
  context)

(defun nelisp-cc-runtime-aot-init-helper-argv (context)
  "Return the standard native argument list for an AOT init CONTEXT.
Generated Doc 129.3 init helpers have the fixed boxed-boundary
signature `(OUT MIRROR FRAMES SCRATCH NAME-SLOT)'.  This helper is the
runtime-side single source of truth for the argument order used by
`nelisp-cc-runtime-call-aot-init-helper' before a real native-call
callback is invoked."
  (nelisp-cc-runtime--validate-aot-init-context context)
  (list (plist-get context :out)
        (plist-get context :mirror)
        (plist-get context :frames)
        (plist-get context :scratch)
        (plist-get context :name-slot)))

(defun nelisp-cc-runtime-run-aot-module-init-plan-with-default-context
    (plan call-helper &optional register-custom mirror frames out scratch name-slot)
  "Run PLAN with a freshly allocated Doc 129.3 init context.
CALL-HELPER and REGISTER-CUSTOM have the same meaning as in
`nelisp-cc-runtime-run-aot-module-init-plan'.  MIRROR, FRAMES, OUT,
SCRATCH, and NAME-SLOT are forwarded to
`nelisp-cc-runtime-make-aot-init-context'."
  (nelisp-cc-runtime-run-aot-module-init-plan
   plan
   (nelisp-cc-runtime-make-aot-init-context
    mirror frames out scratch name-slot)
   call-helper
   register-custom))

(defun nelisp-cc-runtime-run-aot-module-init-plan
    (plan context call-helper &optional register-custom register-closure)
  "Run a Doc 129 AOT module-init PLAN through callback hooks.
PLAN is the plist returned by `nelisp-cc-runtime-aot-module-init-plan'.
CONTEXT must carry `:out', `:mirror', `:frames', `:scratch', and
`:name-slot'; these are the boundary values passed to every generated
init helper.  CALL-HELPER is called as

  (CALL-HELPER HELPER-SYMBOL CONTEXT INIT-DESCRIPTOR)

for each helper in plan order.  REGISTER-CUSTOM, when non-nil, is
called after a matching `defcustom' helper succeeds:

  (REGISTER-CUSTOM CUSTOM-DESCRIPTOR CONTEXT INIT-DESCRIPTOR)

When REGISTER-CUSTOM is nil, custom metadata is stored via
`nelisp-cc-runtime-register-aot-custom-metadata'.
REGISTER-CLOSURE, when non-nil, is called as

  (REGISTER-CLOSURE CLOSURE-DESCRIPTOR CONTEXT)

for each `:closure-descriptors' entry.  When nil, closure descriptors
are stored via `nelisp-cc-runtime-register-aot-closure-descriptor'.

The function returns a plist with ordered `:init-results' and
`:custom-results' plus `:closure-results'.  It does not resolve or call
native code itself; that remains the Doc 99 loader's responsibility."
  (unless (listp plan)
    (signal 'nelisp-cc-runtime-error
            (list :aot-module-init-plan-not-plist plan)))
  (nelisp-cc-runtime--validate-aot-init-context context)
  (unless (functionp call-helper)
    (signal 'nelisp-cc-runtime-error
            (list :aot-call-helper-not-function call-helper)))
  (when (and register-custom (not (functionp register-custom)))
    (signal 'nelisp-cc-runtime-error
            (list :aot-register-custom-not-function register-custom)))
  (when (and register-closure (not (functionp register-closure)))
    (signal 'nelisp-cc-runtime-error
            (list :aot-register-closure-not-function register-closure)))
  (let* ((normalized-plan
          (nelisp-cc-runtime-aot-module-init-plan
           (plist-get plan :init-helpers)
           (plist-get plan :custom-metadata)
           (plist-get plan :root-descriptors)
           (plist-get plan :closure-descriptors)))
         (custom-by-helper (plist-get normalized-plan :custom-by-helper))
         (register-custom-fn
          (or register-custom
              #'nelisp-cc-runtime-register-aot-custom-metadata))
         (register-closure-fn
          (or register-closure
              (lambda (descriptor _context)
                (nelisp-cc-runtime-register-aot-closure-descriptor
                 descriptor))))
         (init-results nil)
         (custom-results nil)
         (closure-results nil))
    (dolist (descriptor (plist-get normalized-plan :closure-descriptors))
      (push (cons (plist-get descriptor :name)
                  (funcall register-closure-fn descriptor context))
            closure-results))
    (dolist (descriptor (plist-get normalized-plan :init-helpers))
      (let* ((helper (plist-get descriptor :helper))
             (result (funcall call-helper helper context descriptor))
             (custom (cdr (assq helper custom-by-helper))))
        (push (cons helper result) init-results)
        (when custom
          (push (cons helper
                      (funcall register-custom-fn custom context descriptor))
                custom-results))))
    (list :plan normalized-plan
          :init-results (nreverse init-results)
          :custom-results (nreverse custom-results)
          :closure-results (nreverse closure-results))))

(cl-defun nelisp-cc-runtime-compile-and-allocate
    (lambda-form &optional backend exec-args &key (entry-abi :host-int))
  "Run the full Phase 7.1.4 pipeline on LAMBDA-FORM.

Steps:
  1. `nelisp-cc-build-ssa-from-ast' (T6 frontend)
  2. `nelisp-cc-runtime--insert-safe-points' (analysis only)
  3. `nelisp-cc--compute-intervals' + `nelisp-cc--linear-scan' (T4)
  4. backend compile (T9 x86_64 or T10 arm64)
  5. tail-call rewrite (`--lower-tail-call')
  6. safe-point stub injection (`--inject-safepoint-stubs')
  7. exec-page allocation + W^X transition (`--alloc-exec-page')
  8. (when `nelisp-cc-runtime-exec-mode' = `real') subprocess
     `nelisp-exec-bytes' to actually execute the bytes;
     the parsed integer return value is added under `:exec-result'.

BACKEND is `x86_64' / `arm64' (default = host inference).  EXEC-ARGS
is forwarded to `nelisp-cc-runtime--exec-real' when MODE is `real';
nil preserves the old zero-argument behavior.

ENTRY-ABI (Doc 81 Stage 81.1 / 81.2 / 81.3) selects the produced
entry-point's calling convention:
  :host-int                (default) extern \"C\" fn(i64, ..., i64) -> i64
  :trampoline-unary        extern \"C\" fn(*const Sexp, *mut Sexp) -> i64
                           unary primitive trampoline (= car / cdr / length).
  :trampoline-binary-ctor  extern \"C\" fn(*const Sexp, *const Sexp,
                                            *mut Sexp) -> i64
                           binary constructor (= cons), Stage 81.2.
  :trampoline-binary-mut   extern \"C\" fn(*mut Sexp, *const Sexp) -> i64
                           binary mutator (= setcar / setcdr), Stage 81.2.
  :trampoline-binary-aref  extern \"C\" fn(*const Sexp, i64, *mut Sexp) -> i64
                           vector indexed read (= aref / elt), Stage 81.3.
  :trampoline-ternary-aset extern \"C\" fn(*const Sexp, i64, *const Sexp,
                                            *mut Sexp) -> i64
                           vector indexed write (= aset), Stage 81.3.
  :trampoline-binary-float-arith
                           extern \"C\" fn(f64, f64) -> f64
                           binary Float arith (= add/sub/mul), Doc 84 §84.1.
  :trampoline-unary-float  extern \"C\" fn(f64) -> f64
                           unary Float math (= float/exp/log), Doc 87 §5.
  :trampoline-binary-float-cmp
                           extern \"C\" fn(f64, f64) -> i64
                           binary Float cmp (= eq-eps/lt/gt/le/ge),
                           Doc 84 §84.1.
  :trampoline-format-float
                           extern \"C\" fn(f64, char, i64, *mut Sexp) -> i64
                           format float body (= %f/%F/%e/%E/%g/%G),
                           Doc 86 §86.1.e / Doc 87 §5.1.

Stage 81.1/81.2/81.3 records the mode in the result plist under
`:entry-abi' but does NOT yet alter prologue/epilogue emit; the
backend frame layout is taken over in a later stage once the
recognition pass starts producing trampoline-shape calls.

Returns:

  (:exec-page PAGE         ; `nelisp-cc-runtime--exec-page' simulator
   :gc-metadata META       ; plist from `--insert-safe-points'
   :tail-call-rewritten BOOL ; t when step 5 fired
   :backend BACKEND
   :ssa-function FN        ; for downstream test introspection
   :raw-bytes RAW          ; pre-rewrite, pre-injection bytes
   :final-bytes FINAL      ; what the page now holds
   :exec-mode MODE         ; `simulator' / `real'
   :exec-result RESULT)    ; only present when MODE = `real';
                           ; one of the (:result EXIT INT) /
                           ; (:error EXIT STDOUT) / (:no-result ...)
                           ; tuples documented on
                           ; `nelisp-cc-runtime--exec-real'.

The `:final-bytes' equals `(--exec-page-bytes PAGE)' restricted to
`(--exec-page-length PAGE)' — exposed separately so test code does
not have to slice the page buffer manually."
  (nelisp-cc-runtime--validate-entry-abi entry-abi)
  (let* ((be (or backend (nelisp-cc-runtime--default-backend)))
         (fn (nelisp-cc-build-ssa-from-ast lambda-form))
         ;; T158 — Phase 7.7 SSA passes (escape + inline + rec-inline +
         ;; lift) wired between the SSA build and the linear-scan.  When
         ;; `nelisp-cc-enable-7.7-passes' is nil the call is a no-op
         ;; that returns FN unchanged + zero stats — see Doc 42 §3.
         (pipeline-result (nelisp-cc-pipeline-run-7.7-passes fn))
         (pipeline-stats (cdr pipeline-result))
         (gc-meta (nelisp-cc-runtime--insert-safe-points fn))
         (alloc (nelisp-cc--linear-scan fn))
         (raw   (nelisp-cc-runtime--compile-bytes fn alloc be))
         (tail  (nelisp-cc-runtime--lower-tail-call raw be))
         (post-tail (car tail))
         (rewrote-p (cdr tail))
         (final (nelisp-cc-runtime--inject-safepoint-stubs post-tail be))
         (page (nelisp-cc-runtime--alloc-exec-page final))
         (mode nelisp-cc-runtime-exec-mode)
         (exec-result (cond
                       ((eq mode 'real)
                        (nelisp-cc-runtime--exec-real final exec-args))
                       ((eq mode 'in-process)
                        (nelisp-cc-runtime--exec-in-process final))
                       (t nil))))
    (append
     (list :exec-mode mode)
     (when exec-result (list :exec-result exec-result))
     (list :exec-page page
           :gc-metadata gc-meta
           :tail-call-rewritten rewrote-p
           :backend be
           :entry-abi entry-abi
           :ssa-function fn
           :pipeline-stats pipeline-stats
           :raw-bytes raw
           :final-bytes final))))

;;; Doc 81 Stage 81.4 — dlsym bridge (resolve-symbol stub) -------------
;;
;; When the recognition pass rewrites `(call cons …)' to
;; `(ssa-call-primitive :symbol nl_jit_cons_make …)', the backend
;; emitter records a *call-fixup* keyed on the C symbol name.  At link
;; time the fixup must be resolved to an *absolute address* so the CALL
;; rel32 / BL placeholder can be patched.  The legacy (Stage 81.1〜81.3)
;; fixup path piggybacks on `nelisp-defs-index' which only resolves
;; same-page elisp letrec entries — extern C trampolines like
;; `nl_jit_cons_make' need a different lookup.
;;
;; `nelisp-cc-runtime-resolve-symbol' is the *contract surface* for that
;; lookup.  On host Emacs it is a *stub* — Emacs has no general way to
;; ask the running process for the address of an arbitrary C symbol
;; without loading a dynamic module that calls dlsym(3) itself, and
;; Doc 81 hard-constrains zero Rust LOC delta in Stage 81.4.  The host
;; stub therefore returns a *sentinel* address (= the first valid
;; non-NULL u64 unlikely to collide with any real exec page) and a
;; status keyword indicating the lookup was deferred.  Real exec on
;; host Emacs is gated upstream by `nelisp-cc-runtime-exec-mode' (=
;; `simulator' / `in-process' modes never attempt to invoke the
;; trampolines so the sentinel is never dereferenced).
;;
;; On standalone NeLisp (= Phase 7.1.6.a's takeover target) the stub
;; will be *overridden* by a runtime hook that calls into a thin Rust
;; shim exposing `dlsym(RTLD_DEFAULT, SYMBOL-NAME)'.  Phase 7.1.6.a is
;; what introduces the Rust hook (= part of the cluster takeover
;; PR's Rust delta budget); Stage 81.4 itself stays at zero Rust LOC.
;;
;; The override contract is:
;;
;;   - Standalone NeLisp shall let-bind or `setq' the dynamic variable
;;     `nelisp-cc-runtime-resolve-symbol-function' to a function with
;;     signature `(SYMBOL-NAME) -> (STATUS . ADDR-OR-NIL)'.
;;   - STATUS is `:resolved' (ADDR is a non-zero unsigned i64) or
;;     `:not-found' (ADDR is nil) or `:host-stub' (host Emacs default —
;;     ADDR is the sentinel `nelisp-cc-runtime--resolve-symbol-stub-addr').
;;   - The override MUST be idempotent on the same SYMBOL-NAME within a
;;     single page allocation (= the link pass may query it more than
;;     once for the same fixup; returning a different ADDR between
;;     queries silently corrupts CALL rel32 patching).
;;   - The override is invoked at link time, *not* at SSA compile time;
;;     it must therefore tolerate being called with arbitrary symbol
;;     names — unknown symbols return `(:not-found . nil)' rather than
;;     erroring out, so the link pass can decide whether to patch a
;;     trampoline indirection (= Doc 81 §6.3 fallback) or signal.
;;
;; Phase 7.1.6.a will provide the Rust override via
;; `nelisp-cc-runtime-resolve-symbol-via-dlsym' (registered through
;; `nelisp-cc-runtime-resolve-symbol-function').  Until then host Emacs
;; uses the stub which returns the sentinel address — sufficient for
;; ERT / link-table inspection but never executed (= simulator /
;; in-process exec modes are the host default).

(defconst nelisp-cc-runtime--resolve-symbol-stub-addr #x0DEADBEEF
  "Sentinel pointer returned by `nelisp-cc-runtime-resolve-symbol' on host.

The value is chosen to be:
  - non-zero (= distinguishable from `:not-found')
  - misaligned (= would SEGV if accidentally CALLed, so a host Emacs
    bug that strays into trampoline exec hits an obvious crash rather
    than silent wrong-result)
  - inside the i32 representable range so test code can compare bytes
    without 64-bit subtleties

Standalone NeLisp's override returns real dlsym addresses — this
sentinel is never observed in production once Phase 7.1.6.a's hook
is installed.")

(defvar nelisp-cc-runtime-resolve-symbol-function nil
  "Function used by `nelisp-cc-runtime-resolve-symbol' to do the lookup.

When non-nil, called with one argument (= SYMBOL-NAME, a symbol such
as `nl_jit_cons_make') and must return `(STATUS . ADDR-OR-NIL)'
following the contract documented on `nelisp-cc-runtime-resolve-symbol'.

Default = nil → host stub path (= return the sentinel address).
Standalone NeLisp registers a real dlsym implementation here at
runtime startup; ERT can let-bind it for fault-injection
(= `:not-found' fallback testing).")

(defun nelisp-cc-runtime-resolve-symbol (symbol-name)
  "Resolve a primitive C SYMBOL-NAME to a runtime address.

SYMBOL-NAME is a symbol (e.g. `nl_jit_cons_make') typically extracted
from an `:ssa-call-primitive' instruction's `:symbol' meta during
the link pass.  Returns `(STATUS . ADDR-OR-NIL)':

  STATUS = `:resolved'   — ADDR is a non-zero unsigned i64
                            host-process address ready for fixup patching
  STATUS = `:host-stub'  — host Emacs has no dlsym; ADDR is the
                            sentinel
                            `nelisp-cc-runtime--resolve-symbol-stub-addr'.
                            The link pass treats this as a *deferred*
                            fixup (= patches the placeholder with the
                            sentinel and records the symbol on a
                            `:unresolved-extern-c-symbols' list in the
                            page meta so the simulator never executes it).
  STATUS = `:not-found'  — ADDR is nil; SYMBOL-NAME is not exported by
                            the running process.  Phase 7.1.6.a's link
                            pass converts this into a runtime error
                            keyed on the page allocation site.

When `nelisp-cc-runtime-resolve-symbol-function' is non-nil, the
function is delegated to (= override hook for standalone NeLisp).
Otherwise the host stub returns `(:host-stub
. nelisp-cc-runtime--resolve-symbol-stub-addr)' for *any* symbol
name (= the host has no dlsym available without a Rust dynamic
module, which Doc 81 §0 STATUS forbids in Stage 81.4 — Rust delta
must be 0)."
  (unless (and symbol-name (symbolp symbol-name))
    (signal 'nelisp-cc-runtime-error
            (list :resolve-symbol-bad-type symbol-name)))
  (if nelisp-cc-runtime-resolve-symbol-function
      (funcall nelisp-cc-runtime-resolve-symbol-function symbol-name)
    (cons :host-stub nelisp-cc-runtime--resolve-symbol-stub-addr)))

(defun nelisp-cc-runtime-resolve-symbol-host-stub-p (status-addr)
  "Return non-nil when STATUS-ADDR (= a `resolve-symbol' return value)
is the host-Emacs sentinel response.

Convenience predicate for ERT / link-pass code so callers do not have
to remember the sentinel address constant or the `:host-stub' tag
key."
  (and (consp status-addr)
       (eq (car status-addr) :host-stub)
       (eq (cdr status-addr)
           nelisp-cc-runtime--resolve-symbol-stub-addr)))

;;; Phase 7.1.6.a — dlsym bridge wiring -------------------------------
;;
;; The standalone NeLisp `nelisp' binary registers the Rust-side
;; `nelisp-cc--dlsym-resolve' primitive (= libc::dlsym(RTLD_DEFAULT,
;; ...)) as the resolver hook so the Doc 81 §5.4 link pass gets real
;; addresses for primitive trampoline symbols (= `nl_jit_cons_*' etc.).
;;
;; The Rust shim's contract:
;;
;;   (nelisp-cc--dlsym-resolve SYMBOL-NAME) -> (STATUS . ADDR-OR-NIL)
;;     STATUS = `:resolved' (ADDR is unsigned i64), or
;;     STATUS = `:not-found' (ADDR is nil).
;;
;; Matches the override contract documented on
;; `nelisp-cc-runtime-resolve-symbol' verbatim, so the wiring is a
;; one-liner: just install the primitive as the hook function when
;; available.
;;
;; On host Emacs the primitive is absent (= `fboundp' nil) — the
;; resolve-symbol path stays on the `:host-stub' default per Doc 81
;; §5.4.  Integration smoke ERT is deferred to Phase 7.1.6.a.2 (=
;; cluster takeover commit) when the standalone NeLisp wiring activates
;; with `#[no_mangle]' trampolines + `-rdynamic' link.
;;
;; Phase 7.1.6.a.2 (= cluster takeover) follows up by adding
;; `#[no_mangle]' to the cons trampolines and `-rdynamic' link so the
;; binary's dynamic symbol table actually exposes them.  Until then
;; this resolver returns `:not-found' on host Emacs for cons cluster
;; lookups, which the link pass treats as deferred fallback (Doc 81
;; §6.3) rather than an error.

(defun nelisp-cc-runtime-resolve-symbol-via-dlsym (symbol-name)
  "Doc 81 §5.4 + Phase 7.1.6.a — dlsym-backed resolve-symbol implementation.

Calls the Rust-side `nelisp-cc--dlsym-resolve' primitive (= a thin
libc::dlsym(RTLD_DEFAULT, ...) shim shipped in
`build-tool/src/eval/dlsym_bridge.rs').  Returns the
`(STATUS . ADDR-OR-NIL)' pair verbatim per the contract documented on
`nelisp-cc-runtime-resolve-symbol'.

Caller is expected to install this as
`nelisp-cc-runtime-resolve-symbol-function' on a host where the Rust
primitive is available (= standalone NeLisp via
`nelisp-cc-runtime-install-dlsym-resolver').  ERT may also bind it
locally to exercise the resolved-path of the contract without
mocking."
  (unless (fboundp 'nelisp-cc--dlsym-resolve)
    (signal 'nelisp-cc-runtime-error
            (list :resolve-symbol-via-dlsym-primitive-absent
                  symbol-name)))
  (nelisp-cc--dlsym-resolve symbol-name))

(defun nelisp-cc-runtime-install-dlsym-resolver ()
  "Phase 7.1.6.a — install the dlsym-backed resolver as the override hook.

Sets `nelisp-cc-runtime-resolve-symbol-function' to
`nelisp-cc-runtime-resolve-symbol-via-dlsym' when the underlying Rust
primitive `nelisp-cc--dlsym-resolve' is available (= we are running
under standalone NeLisp's `nelisp' binary).

Returns non-nil on success (= hook was installed), nil if the Rust
primitive is absent (= host Emacs).  Idempotent — safe to call
multiple times.  Callers (= standalone NeLisp startup) invoke this
once at boot.  ERT may invoke it conditionally on a `fboundp'
guard."
  (when (fboundp 'nelisp-cc--dlsym-resolve)
    (setq nelisp-cc-runtime-resolve-symbol-function
          #'nelisp-cc-runtime-resolve-symbol-via-dlsym)
    t))

;; Auto-wire the dlsym resolver at load time when the Rust primitive is
;; reachable.  Host Emacs (= no nelisp binary in the picture) keeps the
;; default `:host-stub' path unchanged; standalone NeLisp installs the
;; real lookup transparently.
(nelisp-cc-runtime-install-dlsym-resolver)

(provide 'nelisp-cc-runtime)
;;; nelisp-cc-runtime.el ends here
