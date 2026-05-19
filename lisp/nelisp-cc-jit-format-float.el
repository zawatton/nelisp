;;; nelisp-cc-jit-format-float.el --- Phase 47 body for nl_jit_format_float  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Phase 47 elisp migration of `nl_jit_format_float' from
;; `build-tool/src/jit/strings.rs' (~35 LOC Rust body deleted).
;;
;; Trampoline signature (Phase 47 entry):
;;   `(x-bits: i64, conv: i64, prec: i64, out: *mut Sexp) -> i64'
;;   TRAMPOLINE_OK=0 / TRAMPOLINE_ERR=1.
;;
;; The Rust bridge in `jit/bridge.rs::bi_nl_jit_call_format_float'
;; passes `x' as `x.to_bits() as i64' so the f64 value arrives in
;; rdi (GP register) as its raw IEEE-754 bit pattern.  The elisp
;; body reconstructs the f64 for snprintf via `(:varargs (:f64 x-bits))'
;; in the §122.C `extern-call': the emit path loads x-bits (i64) from
;; the GP frame slot into rax then `MOVQ rax → xmm0', preserving the
;; 64-bit bit pattern as the f64 expected by snprintf's variadic arg.
;;
;; This is the same bit-cast trick used by §122.G `nelisp_sexp_write_float'
;; (= pointer bit-cast through xmm0), applied in reverse: an i64 GP
;; value is injected into xmm0 for an `extern-call' `(:f64 ...)' vararg.
;;
;; Integer division note:
;;   Phase 47 GP-class arith ops are: `+' `-' `*' `logior' `logand'
;;   `logxor' `shl' `sar'.  Division `/` is NOT available.
;;   PREC digits are written by computing the tens digit via tail-recursive
;;   subtraction (`nl_jit_format_float_tens': subtract 10 until < 10,
;;   counting iterations); the units digit uses the available `*' op:
;;     units = prec - tens * 10.
;;
;; Algorithm (Plan C — snprintf via §122.C `extern-call' varargs):
;;
;;   Inputs:
;;     X-BITS — i64: IEEE-754 bit pattern of the f64 to format.
;;     CONV   — i64: char code: 'f'=102 'F'=70 'e'=101 'E'=69
;;              'g'=103 'G'=71.  Others → TRAMPOLINE_ERR.
;;     PREC   — i64: decimal precision.  < 0 or > 99 → TRAMPOLINE_ERR.
;;     OUT    — *mut Sexp: receives the result Sexp::Str.
;;
;;   Runtime values thread through function parameters rather than
;;   `let' bindings (Phase 47 `let' is compile-time constant only).
;;   snprintf is called exactly once; its return value flows as
;;   parameter N to the copy helper.
;;
;;   Function chain:
;;
;;     nl_jit_format_float(x-bits conv prec out)              arity 4
;;       Validate CONV + PREC → ERR=1 on bad input.
;;       Alloc 16-byte FMT-BUF and call:
;;     nl_jit_format_float_fmt(x-bits conv prec out fmt-buf)  arity 5
;;       Write '%' at 0, '.' at 1.  Call write_prec to write digits
;;       starting at 2, passing digit-end result to:
;;     nl_jit_format_float_post_prec(x-bits conv out fmt-buf digit-end)
;;                                                             arity 5
;;       Write CONV at digit-end, NUL at digit-end+1.
;;       Alloc 128-byte OUT-BUF and call:
;;     nl_jit_format_float_snprintf(x-bits out fmt-buf out-buf)
;;                                                             arity 4
;;       Init MutStr; call snprintf once, passing n to:
;;     nl_jit_format_float_copy(out-buf fmt-buf out n)        arity 4
;;       copy_bytes [0..min(n,127)); finalize; dealloc both.
;;
;;   Precision helper chain:
;;     nl_jit_format_float_tens(prec acc)                     arity 2
;;       Tail-recursive tens digit: sub 10 until < 10.
;;     nl_jit_format_float_write_prec_1d(fmt-buf prec idx)    arity 3
;;       1-digit path: write `48 + prec'.
;;     nl_jit_format_float_write_prec_2d(fmt-buf prec tens idx)
;;                                                             arity 4
;;       2-digit path: write tens digit + units digit (prec - tens*10).
;;     nl_jit_format_float_write_prec(fmt-buf prec idx)       arity 3
;;       Dispatch 1-digit or 2-digit; return next IDX.
;;
;;   Copy helper:
;;     nl_jit_format_float_copy_bytes(out-buf n i out)        arity 4
;;       Tail-recursive: walk bytes [i..n) into MutStr builder OUT.
;;
;; Grammar ops consumed (all existing — no new opcode needed):
;;   §101.C  `ptr-read-u8'                — read snprintf output byte.
;;   §122.B  `mut-str-make-empty',        — build Sexp::Str result.
;;           `mut-str-push-byte',
;;           `mut-str-finalize'
;;   §122.C  `extern-call' + varargs      — libc `snprintf' call.
;;           + `(:f64 EXPR)'              — f64 vararg from GP bits.
;;   §122.E  `ptr-write-u8'               — write format string bytes.
;;   §125.A  `alloc-bytes', `dealloc-bytes' — heap buffers.
;;   Phase 47 arith: `+', `-', `*',
;;                   `>', `>=', `<', `=', `not', `and', `if'
;;
;; Build wiring:
;;   `scripts/compile-elisp-objects.el' manifest: one entry →
;;   `nl_jit_format_float.o' (Linux x86_64 only).
;;   `build-tool/build.rs' manifest_sources: entry for
;;   `nelisp-cc-jit-format-float.el'.
;;   `build-tool/src/jit/bridge.rs':
;;     - `extern "C" fn nl_jit_format_float' in anchor block.
;;     - `_ELISP_ARCHIVE_ANCHOR' count 59→60.
;;     - `bi_nl_jit_call_format_float' cast changed to
;;       `fn(i64, i64, i64, *mut Sexp) -> i64';
;;       x passed as `x.to_bits() as i64'.
;;   `build-tool/src/jit/strings.rs': Rust body deleted.

;;; Code:

(defconst nelisp-cc-jit-format-float--source
  '(seq
    ;; ---- tens digit via tail-recursive subtraction -----------------------
    ;;
    ;; Count how many times 10 can be subtracted from PREC until < 10.
    ;; This is the tens digit for PREC ∈ [10, 99].
    ;; Phase 47 has no `/` op, so division is replaced by iteration.
    ;;
    ;;   nl_jit_format_float_tens(10, 0) → 1
    ;;   nl_jit_format_float_tens(25, 0) → 2
    ;;   nl_jit_format_float_tens(99, 0) → 9
    ;;
    ;; Caller guarantees PREC ≥ 10 before the first call (dispatched by
    ;; `nl_jit_format_float_write_prec').  ACC starts at 0.
    ;; Arity 2 (even).
    (defun nl_jit_format_float_tens (prec acc)
      (if (< prec 10)
          acc
        (nl_jit_format_float_tens (- prec 10) (+ acc 1))))

    ;; ---- write 1-digit precision -----------------------------------------
    ;;
    ;; Write the single ASCII digit `48 + PREC' into FMT-BUF at IDX.
    ;; Returns IDX + 1 (= next free byte offset).
    ;; Called when PREC < 10.
    ;; Arity 3 (odd — rsp alignment correction).
    (defun nl_jit_format_float_write_prec_1d (fmt-buf prec idx)
      (and
       (ptr-write-u8 fmt-buf idx (+ 48 prec))
       (+ idx 1)))

    ;; ---- write 2-digit precision -----------------------------------------
    ;;
    ;; Write TENS digit then UNITS digit into FMT-BUF at IDX.
    ;; TENS is the tens digit (0..9); UNITS = PREC - TENS * 10.
    ;; `*' is available in Phase 47, so `TENS * 10' is a single arith op.
    ;; Returns IDX + 2 (= next free byte offset).
    ;; Called when PREC ≥ 10.
    ;; Arity 4 (even).
    (defun nl_jit_format_float_write_prec_2d (fmt-buf prec tens idx)
      (and
       (ptr-write-u8 fmt-buf idx (+ 48 tens))
       (ptr-write-u8 fmt-buf (+ idx 1)
                     (+ 48 (- prec (* tens 10))))
       (+ idx 2)))

    ;; ---- dispatch 1-digit or 2-digit precision ---------------------------
    ;;
    ;; Dispatches to _1d or _2d based on PREC.
    ;; For the 2-digit path, `nl_jit_format_float_tens' is called with PREC
    ;; and 0 as arguments; its return value (the tens digit) is passed
    ;; directly as the TENS parameter to `_2d' (no `let' binding needed —
    ;; the call result flows through the argument position).
    ;; Returns the updated IDX (either IDX+1 or IDX+2).
    ;; Arity 3 (odd — rsp alignment correction).
    (defun nl_jit_format_float_write_prec (fmt-buf prec idx)
      (if (< prec 10)
          (nl_jit_format_float_write_prec_1d fmt-buf prec idx)
        (nl_jit_format_float_write_prec_2d
         fmt-buf prec
         (nl_jit_format_float_tens prec 0)
         idx)))

    ;; ---- output-buffer byte copy loop ------------------------------------
    ;;
    ;; Walk bytes [i..n) from OUT-BUF (*const u8 heap buf) into the MutStr
    ;; builder at OUT (*mut Sexp).  Tail-recursive; base case i >= n.
    ;; Returns 1 (truthy sentinel) for `and'-chain composition.
    ;; N is clamped to [0, 127] by the caller.
    ;; Arity 4 (even).
    (defun nl_jit_format_float_copy_bytes (out-buf n i out)
      (if (>= i n)
          1
        (and
         (mut-str-push-byte out (ptr-read-u8 out-buf i))
         (nl_jit_format_float_copy_bytes out-buf n (+ i 1) out))))

    ;; ---- stage 4: copy n bytes, finalize, dealloc both bufs --------------
    ;;
    ;; N is the snprintf return value (passed directly from snprintf call
    ;; in `nl_jit_format_float_snprintf' — no `let' binding needed).
    ;; Clamp to [0, 127]: snprintf with 128-byte buffer writes ≤ 127
    ;; payload bytes (C99 NUL terminator takes 1 byte of the 128).
    ;; `mut-str-finalize' writes the completed Sexp::Str into *OUT.
    ;; Frees OUT-BUF (128 bytes, align 1) then FMT-BUF (16 bytes, align 1).
    ;; Returns 0 (TRAMPOLINE_OK).
    ;; Arity 4 (even).
    (defun nl_jit_format_float_copy (out-buf fmt-buf out n)
      (and
       (nl_jit_format_float_copy_bytes out-buf
                                       (if (> n 127) 127 n)
                                       0
                                       out)
       (mut-str-finalize out out)
       (dealloc-bytes out-buf 128 1)
       (dealloc-bytes fmt-buf 16 1)
       0))

    ;; ---- stage 3: init MutStr, call snprintf once, pass n ---------------
    ;;
    ;; FMT-BUF is fully written (NUL-terminated).  OUT-BUF is freshly alloc'd.
    ;; `mut-str-make-empty' initialises OUT as a MutStr builder.
    ;; snprintf is called exactly once; its return value is passed as
    ;; parameter N to `nl_jit_format_float_copy' (no `let' binding needed
    ;; — the return value flows through the argument position at the call
    ;; site, following the pattern established by `nelisp_cstr_from_sexp_inner').
    ;;
    ;; SysV AMD64 snprintf call frame (§122.C):
    ;;   rdi = out-buf  (char *str)
    ;;   rsi = 128      (size_t size)
    ;;   rdx = fmt-buf  (const char *format)
    ;;   xmm0 = x       (variadic double, from MOVQ of x-bits i64 in rax)
    ;;   al  = 1        (SysV ABI §3.5.7: count of f64 variadic args)
    ;;
    ;; Arity 4 (even).
    (defun nl_jit_format_float_snprintf (x-bits out fmt-buf out-buf)
      (and
       (mut-str-make-empty out 0)
       (nl_jit_format_float_copy
        out-buf fmt-buf out
        (extern-call snprintf out-buf 128 fmt-buf
                     (:varargs (:f64 x-bits))))))

    ;; ---- stage 2: write CONV + NUL, alloc OUT-BUF, call snprintf --------
    ;;
    ;; DIGIT-END is the byte offset returned by `nl_jit_format_float_write_prec'
    ;; (= first byte after the precision digits in FMT-BUF).
    ;; Writes CONV letter at DIGIT-END and NUL at DIGIT-END+1, completing
    ;; the format string `"%.Nf"' / `"%.Ne"' / `"%.Ng"' etc.
    ;; Then allocs a 128-byte OUT-BUF and delegates to _snprintf.
    ;; Arity 5 (odd — rsp alignment correction).
    (defun nl_jit_format_float_post_prec (x-bits conv out fmt-buf digit-end)
      (and
       (ptr-write-u8 fmt-buf digit-end conv)
       (ptr-write-u8 fmt-buf (+ digit-end 1) 0)
       (nl_jit_format_float_snprintf
        x-bits out fmt-buf (alloc-bytes 128 1))))

    ;; ---- stage 1: write `%' `.', call write_prec, pass digit-end --------
    ;;
    ;; FMT-BUF is the freshly-allocated 16-byte heap buffer.
    ;; Writes `%' (37) at offset 0 and `.' (46) at offset 1.
    ;; Calls `nl_jit_format_float_write_prec' which writes PREC digits at
    ;; offset 2 and returns the DIGIT-END offset; this is passed directly
    ;; as argument to `nl_jit_format_float_post_prec' (no `let' needed).
    ;; Arity 5 (odd — rsp alignment correction).
    (defun nl_jit_format_float_fmt (x-bits conv prec out fmt-buf)
      (and
       (ptr-write-u8 fmt-buf 0 37)  ; '%'
       (ptr-write-u8 fmt-buf 1 46)  ; '.'
       (nl_jit_format_float_post_prec
        x-bits conv out fmt-buf
        (nl_jit_format_float_write_prec fmt-buf prec 2))))

    ;; ---- public trampoline -----------------------------------------------
    ;;
    ;; Signature: (x-bits: i64, conv: i64, prec: i64, out: *mut Sexp) -> i64
    ;; Returns 0 (TRAMPOLINE_OK) or 1 (TRAMPOLINE_ERR).
    ;;
    ;; Called from the modified Rust bridge
    ;; `bi_nl_jit_call_format_float' which casts to
    ;; `fn(i64, i64, i64, *mut Sexp) -> i64' and passes x as
    ;; `x.to_bits() as i64'.
    ;;
    ;; Validation:
    ;;   CONV must be one of: 102('f') 70('F') 101('e') 69('E')
    ;;                        103('g') 71('G').  Others → ERR.
    ;;   PREC must be 0..99 (100+ exceeds the 16-byte FMT-BUF limit;
    ;;   format `"%.NNf"' = 7 bytes + NUL = 8 bytes, well within 16).
    ;;   Negative PREC → ERR.
    ;;
    ;; On success: allocs FMT-BUF (16 bytes, align 1) and delegates to
    ;; `nl_jit_format_float_fmt' which threads it through the chain.
    ;; Arity 4 (even).
    (defun nl_jit_format_float (x-bits conv prec out)
      ;; Validate CONV: must be one of 102('f') 70('F') 101('e') 69('E')
      ;; 103('g') 71('G').  Phase 47 has no `not' op — invalid-conv is
      ;; expressed as nested `if' returning 1 for unrecognised char codes.
      ;; (if (= conv 102) 0 (if (= conv 70) 0 ... 1)) = 1 iff invalid.
      (if (or (< prec 0)
              (> prec 99)
              (if (= conv 102) 0   ; 'f' → valid (0 = falsy)
                (if (= conv  70) 0 ; 'F'
                  (if (= conv 101) 0 ; 'e'
                    (if (= conv  69) 0 ; 'E'
                      (if (= conv 103) 0 ; 'g'
                        (if (= conv  71) 0 ; 'G'
                          1)))))))  ; unknown conv → truthy (= invalid)
          1
        (nl_jit_format_float_fmt x-bits conv prec out (alloc-bytes 16 1)))))
  "Phase 47 source for the `nl_jit_format_float' trampoline.

Ten-entry `(seq DEFUN ...)' manifest:
  `nl_jit_format_float_tens (prec acc)'
    — tail-recursive tens digit via subtraction (no `/` op in Phase 47).
  `nl_jit_format_float_write_prec_1d (fmt-buf prec idx)'
    — write 1-digit precision; return idx+1.
  `nl_jit_format_float_write_prec_2d (fmt-buf prec tens idx)'
    — write 2-digit precision; return idx+2.
  `nl_jit_format_float_write_prec (fmt-buf prec idx)'
    — dispatch to _1d or _2d; call tens() for 2-digit path.
  `nl_jit_format_float_copy_bytes (out-buf n i out)'
    — tail-recursive byte copy [i..n) from OUT-BUF into MutStr OUT.
  `nl_jit_format_float_copy (out-buf fmt-buf out n)'
    — clamp n to [0,127]; copy; finalize; dealloc both bufs; return 0.
  `nl_jit_format_float_snprintf (x-bits out fmt-buf out-buf)'
    — init MutStr; call snprintf once; pass n to _copy.
  `nl_jit_format_float_post_prec (x-bits conv out fmt-buf digit-end)'
    — write CONV + NUL into FMT-BUF; alloc OUT-BUF; call _snprintf.
  `nl_jit_format_float_fmt (x-bits conv prec out fmt-buf)'
    — write `%' `.'; call write_prec; pass digit-end to _post_prec.
  `nl_jit_format_float (x-bits conv prec out)'
    — validate; alloc FMT-BUF (16 bytes); call _fmt.

Algorithm: GP-class defun receives the f64 value as IEEE-754 bit pattern
in x-bits (i64, rdi).  The §122.C `(:varargs (:f64 x-bits))' path MOVQ's
the GP value into xmm0, reconstructing the f64 for libc `snprintf'.
snprintf is called once; its return value threads as parameter N through
the call chain (Phase 47 `let' is compile-time constants only).

Integer division: Phase 47 has no `/` op.  Tens digit is computed via
tail-recursive subtraction of 10 (`nl_jit_format_float_tens'); units digit
uses `*' (available): units = prec - tens * 10.

PREC must be 0..99.  CONV must be one of 102/70/101/69/103/71.
Invalid → TRAMPOLINE_ERR=1.

Rust bridge change: `bi_nl_jit_call_format_float' casts to
`fn(i64, i64, i64, *mut Sexp) -> i64' and passes `x.to_bits() as i64'.")

(provide 'nelisp-cc-jit-format-float)

;;; nelisp-cc-jit-format-float.el ends here
