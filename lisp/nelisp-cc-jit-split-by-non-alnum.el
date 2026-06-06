;;; nelisp-cc-jit-split-by-non-alnum.el --- AOT body for nl_jit_split_by_non_alnum  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; AOT elisp migration of `nl_jit_split_by_non_alnum' from
;; `build-tool/src/jit/strings.rs'.
;;
;; Trampoline signature: `(*const Sexp, *const Sexp, *mut Sexp) -> i64'
;; (OK=0 / ERR=1), reached via `nl-jit-call-out-2' from
;; `nelisp-jit-strategy.el'.
;;
;; Rust contract:
;;   STR-ARG   — Str/Symbol/MutStr to split.
;;   OMIT-ARG  — Nil → retain empty parts; non-Nil → drop empties.
;;   OUT       — written with a proper cons list of Sexp::Str parts.
;;
;; Algorithm:
;;   1. Validate STR-ARG tag: accept Str (5), Symbol (4), MutStr (6).
;;      Any other tag → return 1 (TRAMPOLINE_ERR).
;;   2. Compute OMIT-NIL-P = 0 if OMIT-ARG is Nil (tag=0), else 1.
;;   3. Allocate a 32-byte scratch Sexp slot (SSLOT) via `alloc-bytes'.
;;   4. Initialise *OUT = Nil via `sexp-write-nil'.
;;   5. Walk all bytes [0..n) of STR-ARG:
;;        — alphanumeric byte (ASCII [0-9A-Za-z] or byte >= 128):
;;            advance i, keep wstart unchanged.
;;        — non-alphanumeric delimiter byte:
;;            emit [wstart..i) as `Sexp::Str' iff OMIT-NIL-P=0 or i>wstart;
;;            `sexp-write-str' into SSLOT, `cons-make(sslot, out, out)' to
;;            prepend onto the reversed result list, then `ptr-write-u8(sslot,
;;            0, 0)' to clear SSLOT tag (RC invariant: sole ownership of the
;;            new Str moves into the cons box car field).
;;            Advance wstart and i to i+1.
;;   6. After the loop, emit final part [wstart..n) on the same rule.
;;   7. Free SSLOT via `dealloc-bytes'.
;;   8. Return 0 (TRAMPOLINE_OK).
;;
;; Ownership note (cons-make + ptr-write-u8 pair):
;;   `cons-make' uses MVP byte-copy semantics (no refcount bump).  After
;;   `cons-make(sslot, out, out)':
;;     • new_box.car = raw copy of sslot's Sexp::Str (RC=1, two holders).
;;     • new_box.cdr = raw copy of old *OUT (RC=1, two holders).
;;     • *OUT overwritten with new Sexp::Cons (old value now only in cdr).
;;   Clearing SSLOT via `ptr-write-u8(sslot, 0, 0)' makes tag=0 (Nil),
;;   leaving new_box.car as the sole Str reference.  The old *OUT value
;;   is the sole cdr reference since *OUT was overwritten.  Both RC=1.
;;   Safe within the single-frame trampoline context (no GC interrupts).
;;
;; Result order:
;;   The cons list in *OUT is REVERSED (last part first).  The
;;   Elisp wrapper `nl-split-by-non-alnum' in
;;   `lisp/nelisp-stdlib-plist-str.el' calls `nreverse' on the result
;;   to restore forward order before returning to callers.
;;
;; Alphanumeric predicate (byte b):
;;   b in [48..57] (0-9) or [65..90] (A-Z) or [97..122] (a-z)
;;   or b >= 128 (non-ASCII byte treated as word-continuation).
;;
;; Walker arity (6, even) — str-len + str-bytes-ptr computed inside
;;   each walker call from str-arg rather than pre-cached, keeping the
;;   arity within the AOT GP register limit of 6.
;;
;; Tag constants (pinned by §62.5 ABI assert tests):
;;   SEXP_TAG_NIL    = 0
;;   SEXP_TAG_SYMBOL = 4
;;   SEXP_TAG_STR    = 5
;;   SEXP_TAG_MUT_STR = 6
;;
;; Grammar ops consumed (all existing — no new opcode needed):
;;   §101.C  `sexp-tag', `str-len', `str-bytes-ptr', `ptr-read-u8'
;;   §122.A  `sexp-write-str', `sexp-write-nil'
;;   §101.D  `cons-make'
;;   §122.E  `ptr-write-u8'
;;   §125.A  `alloc-bytes', `dealloc-bytes'
;;   AOT arith: `+', `-', `>=', `<=', `>', `=', `if', `or', `and'
;;
;; Build wiring:
;;   `scripts/compile-elisp-objects.el' manifest: one entry →
;;   `nl_jit_split_by_non_alnum.o' (Linux x86_64 only).
;;   `build-tool/build.rs' manifest_sources: entry for
;;   `nelisp-cc-jit-split-by-non-alnum.el'.
;;   `build-tool/src/jit/bridge.rs': `extern "C" fn nl_jit_split_by_non_alnum'
;;   + `_ELISP_ARCHIVE_ANCHOR' count 54→55.
;;   `build-tool/src/jit/strings.rs': Rust body deleted.
;;   `lisp/nelisp-stdlib-plist-str.el': `nl-split-by-non-alnum' wrapper
;;   applies `nreverse' to restore forward order.

;;; Code:

(defconst nelisp-cc-jit-split-by-non-alnum--source
  '(seq
    ;; ---- alphanumeric predicate -------------------------------------------
    ;;
    ;; Returns 1 if byte B is alphanumeric (ASCII [0-9A-Za-z] or >=128),
    ;; 0 otherwise.  Bytes >= 128 are treated as word-continuation to
    ;; avoid splitting multi-byte UTF-8 sequences mid-codepoint.
    (defun nl_jit_split_is_alnum (b)
      (if (or (and (>= b 48) (<= b 57))    ; '0'..'9'
              (and (>= b 65) (<= b 90))    ; 'A'..'Z'
              (and (>= b 97) (<= b 122))   ; 'a'..'z'
              (>= b 128))                  ; non-ASCII continuation
          1
        0))

    ;; ---- byte walker -------------------------------------------------------
    ;;
    ;; Walk bytes [i..n) of STR-ARG (n = str-len re-read each call).
    ;; WSTART is the start of the current word accumulation.
    ;; OMIT-NIL-P: 0 = emit always (including empty), 1 = skip empties.
    ;; SSLOT: 32-byte scratch Sexp slot for `sexp-write-str' staging.
    ;;
    ;; At each byte:
    ;;   alphanumeric → advance i, keep wstart.
    ;;   delimiter    → emit [wstart..i) if eligible; wstart=i+1, i=i+1.
    ;; End of string  → emit [wstart..n) if eligible; return 1.
    ;;
    ;; Eligibility: (= omit-nil-p 0) OR (> i wstart) (non-empty).
    ;;
    ;; RC invariant after each emit:
    ;;   sexp-write-str → SSLOT holds fresh Str (RC=1).
    ;;   cons-make(sslot, out, out) → byte-copies SSLOT into new box.car
    ;;     and old *OUT into new box.cdr; overwrites *OUT with new Cons.
    ;;   ptr-write-u8(sslot, 0, 0) → clears SSLOT tag to Nil; sole
    ;;     ownership of the Str NlStr* moves to new_box.car.  RC=1.
    ;;
    ;; Arity 6 (even) — compiler does not need an rsp alignment pad at
    ;; the prologue spill step.  str-len + str-bytes-ptr are re-derived
    ;; from str-arg each call to stay within the 6-param GP limit.
    ;;
    ;; Returns 1 (truthy sentinel) for `and'-chain composition in the
    ;; caller `nl_jit_split_inner'.
    (defun nl_jit_split_walk (str-arg omit-nil-p out wstart i sslot)
      (if (>= i (str-len str-arg))
          ;; End of string: emit final part [wstart..n) if eligible.
          (if (or (= omit-nil-p 0) (> (str-len str-arg) wstart))
              (and
               (sexp-write-str sslot
                               (+ (str-bytes-ptr str-arg) wstart)
                               (- (str-len str-arg) wstart))
               (cons-make sslot out out)
               (ptr-write-u8 sslot 0 0))
            1)
        ;; Still have bytes: check if byte[i] is alphanumeric.
        (if (= (nl_jit_split_is_alnum
                (ptr-read-u8 (str-bytes-ptr str-arg) i))
               1)
            ;; Alphanumeric: extend current word (advance i only).
            (nl_jit_split_walk str-arg omit-nil-p out wstart (+ i 1) sslot)
          ;; Non-alphanumeric delimiter: emit [wstart..i) if eligible,
          ;; then continue from i+1 with wstart = i+1.
          (and
           (if (or (= omit-nil-p 0) (> i wstart))
               (and
                (sexp-write-str sslot
                                (+ (str-bytes-ptr str-arg) wstart)
                                (- i wstart))
                (cons-make sslot out out)
                (ptr-write-u8 sslot 0 0))
             1)
           (nl_jit_split_walk str-arg omit-nil-p out
                              (+ i 1) (+ i 1) sslot)))))

    ;; ---- inner driver -------------------------------------------------------
    ;;
    ;; Initialises *OUT to Nil, runs the walker, then frees SSLOT.
    ;; Returns 0 (TRAMPOLINE_OK).
    ;;
    ;; The `and' chain: `sexp-write-nil' returns OUT (non-zero truthy);
    ;; `nl_jit_split_walk' returns 1 (truthy) on success; `dealloc-bytes'
    ;; returns 1 (truthy); the final `0' is TRAMPOLINE_OK.
    ;;
    ;; Arity 4 (even).
    (defun nl_jit_split_inner (str-arg omit-nil-p out sslot)
      (and
       (sexp-write-nil out)
       (nl_jit_split_walk str-arg omit-nil-p out 0 0 sslot)
       (dealloc-bytes sslot 32 8)
       0))

    ;; ---- alloc driver -------------------------------------------------------
    ;;
    ;; Allocates the 32-byte scratch slot and delegates to
    ;; `nl_jit_split_inner'.  Arity 3 (odd); compiler inserts
    ;; rsp alignment correction.
    (defun nl_jit_split_alloc (str-arg omit-nil-p out)
      (nl_jit_split_inner str-arg omit-nil-p out (alloc-bytes 32 8)))

    ;; ---- public trampoline --------------------------------------------------
    ;;
    ;; Signature: (str-arg: *const Sexp, omit-arg: *const Sexp,
    ;;             out: *mut Sexp) -> i64
    ;; Returns 0 (TRAMPOLINE_OK) on success, 1 (TRAMPOLINE_ERR) on
    ;; wrong-type input.  Accepts Str (tag=5), Symbol (tag=4),
    ;; MutStr (tag=6).  All other tags return TRAMPOLINE_ERR=1.
    ;;
    ;; OMIT-ARG Nil (tag=0) → omit-nil-p=0 (retain empty parts).
    ;; OMIT-ARG non-Nil    → omit-nil-p=1 (drop empty parts).
    ;;
    ;; Result in *OUT is in REVERSE order; the Elisp wrapper
    ;; `nl-split-by-non-alnum' applies `nreverse' before returning.
    ;;
    ;; Arity 3 (odd); compiler inserts rsp alignment correction.
    (defun nl_jit_split_by_non_alnum (str-arg omit-arg out)
      (if (or (= (sexp-tag str-arg) 5)  ; Str
              (= (sexp-tag str-arg) 4)  ; Symbol
              (= (sexp-tag str-arg) 6)) ; MutStr
          (nl_jit_split_alloc str-arg
                              (if (= (sexp-tag omit-arg) 0) 0 1)
                              out)
        1)))
  "AOT source for the `nl_jit_split_by_non_alnum' trampoline.

Five-entry `(seq DEFUN ...)' manifest:
  `nl_jit_split_is_alnum (b)'                       — byte predicate.
  `nl_jit_split_walk (str-arg omit-nil-p out wstart i sslot)'
                                                     — recursive byte walker.
  `nl_jit_split_inner (str-arg omit-nil-p out sslot)' — init + walk + free.
  `nl_jit_split_alloc (str-arg omit-nil-p out)'     — alloc scratch + delegate.
  `nl_jit_split_by_non_alnum (str-arg omit-arg out)' — public trampoline.

Algorithm: walk bytes of STR-ARG, split at non-alphanumeric bytes
(ASCII [0-9A-Za-z] + bytes >= 128 as word-continuation), build a
REVERSED cons list of Sexp::Str parts in *OUT.  The Elisp wrapper
applies `nreverse' to restore forward order.  Empty parts retained
when OMIT-ARG is Nil (tag=0); dropped otherwise.

Tag constants: SEXP_TAG_NIL=0, SEXP_TAG_SYMBOL=4, SEXP_TAG_STR=5,
SEXP_TAG_MUT_STR=6.  Scratch SSLOT: 32 bytes (= sizeof::<Sexp>()),
alignment 8, allocated per-call and freed before TRAMPOLINE_OK return.")

(provide 'nelisp-cc-jit-split-by-non-alnum)

;;; nelisp-cc-jit-split-by-non-alnum.el ends here
