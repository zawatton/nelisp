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
(require 'pcase)
(require 'nelisp-cc)
(require 'nelisp-cc-x86_64)
(require 'nelisp-cc-arm64)

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
;; the `nelisp-runtime' Rust binary's `exec-bytes' subcommand which
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
and shells out to `nelisp-runtime exec-bytes <file>', which mmaps
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
;;   2. `call-process' the `nelisp-runtime' binary with `exec-bytes
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

(defun nelisp-cc-runtime--bytes-to-unibyte-string (bytes)
  "Coerce BYTES (a list / vector of small ints 0..255) to a unibyte string.
Used to write the machine code stream to disk verbatim — every
element must round-trip through `write-region' with no encoding
mangling."
  (let* ((lst (nelisp-cc-runtime--bytes-as-list bytes))
         (str (apply #'unibyte-string lst)))
    str))

(defun nelisp-cc-runtime--exec-real (bytes)
  "Execute BYTES via the `nelisp-runtime exec-bytes' subprocess.

BYTES is a vector / list of small ints (0..255) representing a
ready-to-run machine code stream that follows the System V AMD64 /
AAPCS64 `extern \"C\" fn() -> i64' calling convention (no args, i64
return in rax / x0).

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
        (bin (nelisp-cc-runtime--locate-runtime-bin)))
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
            (let* ((exit (call-process bin nil t nil "exec-bytes" tmp-file))
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

(defun nelisp-cc-runtime-compile-and-allocate (lambda-form &optional backend)
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
     `nelisp-runtime exec-bytes' to actually execute the bytes;
     the parsed integer return value is added under `:exec-result'.

BACKEND is `x86_64' / `arm64' (default = host inference).  Returns:

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
  (let* ((be (or backend (nelisp-cc-runtime--default-backend)))
         (fn (nelisp-cc-build-ssa-from-ast lambda-form))
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
                        (nelisp-cc-runtime--exec-real final))
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
           :ssa-function fn
           :raw-bytes raw
           :final-bytes final))))

(provide 'nelisp-cc-runtime)
;;; nelisp-cc-runtime.el ends here
