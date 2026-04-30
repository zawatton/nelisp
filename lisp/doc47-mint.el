;;; doc47-mint.el --- Doc 47 image mint, Elisp-side  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; Author: zawatton <kurozawawo@gmail.com>
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Stage 9a-9g (commits e79b5fa..bedec84) wrongly grew ~2,100 LOC of
;; translator/orchestration logic in the Rust build-tool, violating
;; the user's principle "Rust core を最小化、Elisp で書ける物は全部
;; Elisp で書く" (= memory `feedback_rust_core_minimization_principle').
;; The Rust path duplicated functionality already present in the
;; self-hosted Elisp native compiler at `src/nelisp-cc-*.el'.
;;
;; This file is the Elisp replacement for the Rust mint-from-ast logic.
;; It is intentionally a *walking-skeleton* that demonstrates the
;; Elisp-first architecture, NOT a full native compiler:
;;
;;   1. Take an Elisp lambda + a list of integer heap values.
;;   2. Build-time evaluate the lambda on the values (= Emacs `apply').
;;   3. Lower the resulting integer to a tiny `mov eax, IMM; ret'
;;      (x86_64) or `MOVZ w0,#imm; MOVK w0,#imm,lsl#16; RET' (aarch64)
;;      code segment.  This is the only Rust-substrate primitive we
;;      need (= per-arch raw byte constants, allowed by the principle).
;;   4. Construct the full NlImage v1 binary entirely in Elisp:
;;      header (104 bytes) + zero-pad + code segment.
;;   5. Write via `write-region' (= POSIX file syscall, allowed
;;      substrate at the Emacs/runtime layer).
;;
;; The image is then bootable via the existing Rust runtime
;; (`target/release/nelisp-runtime boot-from-image PATH'); the boot
;; exit code equals the precomputed integer (mod 256, per POSIX).
;;
;; A future stage will replace the precompute with a real native
;; compilation path that calls into `nelisp-cc-runtime-compile-and-
;; allocate' (= existing self-hosted Elisp compiler) plus a small ABI
;; bridge that adapts its tagged-int return to Doc 47's raw-int exit
;; code.  That extension lives entirely in Elisp; no Rust additions
;; required.
;;
;; Format constants are sourced from
;; `nelisp-runtime/src/image/format.rs' (NL_IMAGE_MAGIC,
;; NL_IMAGE_HEADER_SIZE, NL_IMAGE_PAGE_SIZE, etc.).  See
;; `nelisp-runtime/src/image/dumper.rs' lines 60-99 for the layout
;; that `doc47-mint--build-image' mirrors.

;;; Code:

(require 'cl-lib)

;; ---------------------------------------------------------------------------
;; NlImage v1 format constants
;;
;; Source: nelisp-runtime/src/image/format.rs (re-encoded; not loaded
;; from Rust).  Cite line numbers in comments so a future audit can
;; verify parity.
;; ---------------------------------------------------------------------------

(defconst doc47-mint--magic
  (unibyte-string ?N ?L ?I ?M ?A ?G ?E 0)
  "NL_IMAGE_MAGIC = b\"NLIMAGE\\0\" — format.rs:27.")

(defconst doc47-mint--abi-version 1
  "NL_IMAGE_ABI_VERSION = 1 — format.rs:37.")

(defconst doc47-mint--compression-none 0
  "NL_IMAGE_COMPRESSION_NONE = 0 — format.rs:48.")

(defconst doc47-mint--header-size 104
  "NL_IMAGE_HEADER_SIZE = 104 — format.rs:129.")

(defconst doc47-mint--page-size 4096
  "NL_IMAGE_PAGE_SIZE = 4096 — format.rs:55.")

;; ---------------------------------------------------------------------------
;; Per-arch native byte emitters
;;
;; These are the *only* per-arch byte tables.  They are pure data —
;; integer constants spelled out so a future audit can byte-compare
;; against the (deleted) Rust `native_emit::emit_return_i32' that they
;; replace.  No translation logic lives here.
;; ---------------------------------------------------------------------------

(defun doc47-mint--emit-return-i32-x86_64 (value)
  "Emit `mov eax, VALUE; ret' as a 6-byte unibyte string (x86_64).

Encoding (cf. Stage 9a Rust emit_return_i32 in the deleted
`build-tool/src/native_emit.rs'):
  b8 NN NN NN NN   mov eax, imm32   (5 bytes)
  c3               ret              (1 byte)
Upper 32 bits of rax are zero-cleared by the implicit 32-bit-write
semantics of `mov-to-32-bit-reg', so the i32 return travels back to
the caller intact through eax/rax."
  (let* ((u32 (logand value #xFFFFFFFF)))
    (unibyte-string
     #xb8
     (logand u32 #xFF)
     (logand (ash u32 -8) #xFF)
     (logand (ash u32 -16) #xFF)
     (logand (ash u32 -24) #xFF)
     #xc3)))

(defun doc47-mint--emit-return-i32-aarch64 (value)
  "Emit MOVZ w0,#low16; MOVK w0,#high16,lsl#16; RET as 12 bytes.

Encoding (cf. Stage 9a Rust emit_return_i32):
  MOVZ w0, #low16          0x52800000 | (low16 << 5)
  MOVK w0, #high16, lsl 16 0x72A00000 | (high16 << 5)
  RET                      0xD65F03C0
We always emit MOVZ+MOVK regardless of high16 = 0 to keep the byte
size deterministic at 12, which simplifies future patching code that
needs to know the prologue length up-front."
  (let* ((u32 (logand value #xFFFFFFFF))
         (low16 (logand u32 #xFFFF))
         (high16 (logand (ash u32 -16) #xFFFF))
         (movz (logior #x52800000 (ash low16 5)))
         (movk (logior #x72A00000 (ash high16 5)))
         (ret  #xD65F03C0))
    (apply #'unibyte-string
           (append (doc47-mint--u32-le-bytes movz)
                   (doc47-mint--u32-le-bytes movk)
                   (doc47-mint--u32-le-bytes ret)))))

(defun doc47-mint--u32-le-bytes (n)
  "Return the four little-endian bytes of unsigned 32-bit N as a list."
  (let ((u (logand n #xFFFFFFFF)))
    (list (logand u #xFF)
          (logand (ash u -8) #xFF)
          (logand (ash u -16) #xFF)
          (logand (ash u -24) #xFF))))

(defun doc47-mint--u64-le-bytes (n)
  "Return the eight little-endian bytes of unsigned 64-bit N as a list."
  ;; Emacs integers can be > 32 bits on 64-bit hosts; ash + logand chain
  ;; below keeps every step within fixnum range so the result is portable.
  (let ((u (logand n (1- (ash 1 64)))))
    (cl-loop for i from 0 below 8
             collect (logand (ash u (* -8 i)) #xFF))))

(defun doc47-mint--detect-arch ()
  "Return the symbol `x86_64' or `aarch64' for the current host.
Falls back to `x86_64' if `system-configuration' is uninformative."
  (let ((cfg (or (and (boundp 'system-configuration) system-configuration) "")))
    (cond
     ((string-match-p "aarch64\\|arm64" cfg) 'aarch64)
     ((string-match-p "x86_64\\|amd64" cfg) 'x86_64)
     (t 'x86_64))))

(defun doc47-mint--emit-return-i32 (value &optional arch)
  "Emit a `return VALUE as i32' code segment for ARCH (default = host)."
  (let ((arch (or arch (doc47-mint--detect-arch))))
    (pcase arch
      ('x86_64  (doc47-mint--emit-return-i32-x86_64 value))
      ('aarch64 (doc47-mint--emit-return-i32-aarch64 value))
      (_ (error "doc47-mint: unsupported arch %S" arch)))))

;; ---------------------------------------------------------------------------
;; NlImage v1 binary writer
;;
;; Mirrors `write_image_with_native_entry' from
;; nelisp-runtime/src/image/dumper.rs:66-99 byte-for-byte.  Pure
;; Elisp; the only file-system call is `write-region' (= legitimate
;; substrate).
;; ---------------------------------------------------------------------------

(defun doc47-mint--build-header (code-size)
  "Construct the 104-byte NlImage v1 header for a code-only image.
CODE-SIZE is the byte length of the native code segment.

Layout (offsets per NlImageHeader in nelisp-runtime/src/image/format.rs):
  0..8   magic                        = NLIMAGE\\0
  8..12  abi_version u32              = 1
  12..16 compression u32              = 0
  16..24 payload_len u64              = (code_offset - HEADER) + code_size
  24..32 heap_offset u64              = 0 (no heap)
  32..40 heap_size u64                = 0
  40..48 code_offset u64              = PAGE_SIZE
  48..56 code_size u64                = code_size
  56..64 reloc_offset u64             = 0
  64..72 reloc_count u64              = 0
  72..80 entry_offset u64             = 0
  80..88 signal_vector_offset u64     = 0
  88..92 signal_vector_count u32      = 0
  92..96 required_syscall_abi u32     = 0
  96..104 _reserved [8]               = zeros"
  (let* ((code-offset doc47-mint--page-size)
         (payload-len (+ (- code-offset doc47-mint--header-size) code-size))
         (out (make-string doc47-mint--header-size 0 t))) ; unibyte zero
    (cl-flet ((set-bytes (offset bytes)
                (cl-loop for byte in bytes
                         for i from 0
                         do (aset out (+ offset i) byte))))
      ;; offset 0..8: magic
      (set-bytes 0 (append doc47-mint--magic nil))
      ;; offset 8..12: abi_version u32
      (set-bytes 8 (doc47-mint--u32-le-bytes doc47-mint--abi-version))
      ;; offset 12..16: compression u32
      (set-bytes 12 (doc47-mint--u32-le-bytes doc47-mint--compression-none))
      ;; offset 16..24: payload_len u64
      (set-bytes 16 (doc47-mint--u64-le-bytes payload-len))
      ;; offset 40..48: code_offset u64
      (set-bytes 40 (doc47-mint--u64-le-bytes code-offset))
      ;; offset 48..56: code_size u64
      (set-bytes 48 (doc47-mint--u64-le-bytes code-size))
      ;; remaining fields stay 0 (heap_*, reloc_*, entry, signal_vector_*,
      ;; required_syscall_abi, _reserved) — already zero-filled.
      out)))

(defun doc47-mint--build-image (code-bytes)
  "Build the full NlImage v1 binary for a CODE-BYTES code segment.
Header + zero pad to PAGE_SIZE + code segment.  Returns a unibyte
string ready for `write-region' with `binary' coding."
  (let* ((code-size (length code-bytes))
         (header (doc47-mint--build-header code-size))
         (pad-size (- doc47-mint--page-size doc47-mint--header-size))
         (pad (make-string pad-size 0 t))) ; unibyte zero
    (concat header pad code-bytes)))

;; ---------------------------------------------------------------------------
;; Public API — `doc47-mint-from-ast'
;; ---------------------------------------------------------------------------

(defun doc47-mint--apply-lambda (lambda-form heap-values)
  "Evaluate LAMBDA-FORM applied to HEAP-VALUES via Emacs `apply'.
LAMBDA-FORM must be an Elisp lambda; HEAP-VALUES is a list of
integers.  Returns the result integer."
  (apply (eval lambda-form t) heap-values))

(defun doc47-mint-from-ast (lambda-form heap-values out-path &optional arch)
  "Mint a Doc 47 NlImage v1 binary at OUT-PATH.

Walking-skeleton path:
  1. Evaluate LAMBDA-FORM on HEAP-VALUES via Emacs `apply'.
  2. Lower the result integer to a `return i32' code segment for
     ARCH (default = host arch).
  3. Construct the NlImage v1 binary (header + pad + code).
  4. Write to OUT-PATH via `write-region' with `binary' coding.

Returns a plist documenting the operation:
  (:result-int N        ; the value the boot will exit with (mod 256)
   :code-size BYTES
   :image-size BYTES
   :arch ARCH
   :out-path OUT-PATH)"
  (let* ((arch (or arch (doc47-mint--detect-arch)))
         (result-int (doc47-mint--apply-lambda lambda-form heap-values))
         (i32 (cond
               ((not (integerp result-int))
                (error "doc47-mint: lambda result %S not an integer" result-int))
               ((or (< result-int -2147483648) (> result-int 2147483647))
                (error "doc47-mint: result %d out of i32 range" result-int))
               (t result-int)))
         (code (doc47-mint--emit-return-i32 i32 arch))
         (image (doc47-mint--build-image code))
         (coding-system-for-write 'no-conversion))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (insert image)
      (write-region (point-min) (point-max) out-path nil 'silent))
    (list :result-int i32
          :code-size (length code)
          :image-size (length image)
          :arch arch
          :out-path out-path)))

(provide 'doc47-mint)
;;; doc47-mint.el ends here
