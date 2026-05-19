;;; nelisp-cc-jit-string-match-p.el --- Phase 47 body for nl_jit_string_match_p  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Phase 47 elisp migration of `nl_jit_string_match_p' from
;; `build-tool/src/jit/regex.rs'.
;;
;; Trampoline signature: `(*const Sexp, *const Sexp, *mut Sexp) -> i64'
;; (OK=0 / ERR=1), reached via `nl-jit-call-out-2 "nl_jit_string_match_p"'
;; from `lisp/nelisp-stdlib-regex.el'.
;;
;; The Rust body uses NO regex engine.  It has literal fast paths for
;; six well-known patterns plus a generic fallback that strips anchor
;; metacharacters to produce a plain literal string and then does
;; anchored / unanchored substring matching — all pure byte ops.
;;
;; Phase 47 constraint: `(let ((X EXPR)) BODY)' only accepts compile-time
;; constant EXPR.  Runtime values must be threaded through call arguments.
;; `ptr-write-u8' always returns 1 (sentinel, not the written byte).
;;
;; Scratch buffer layout for the generic fallback:
;;   scratch[0 .. ll-1] — literal bytes (output of nl_smp_lit_copy)
;;   scratch[plen]      — anchor_flags: as*2 + ae  (written before lit_copy)
;;   scratch[plen+1]    — spare
;; Allocation: plen + 8 bytes (8 bytes of headroom for metadata at the end).
;;
;; Tag constants (pinned by §62.5 ABI assert tests):
;;   SEXP_TAG_NIL    = 0
;;   SEXP_TAG_T      = 1
;;   SEXP_TAG_SYMBOL = 4
;;   SEXP_TAG_STR    = 5
;;   SEXP_TAG_MUT_STR = 6
;;
;; Nil  → "nil" (len 3, bytes 110 105 108)
;; T    → "t"   (len 1, byte 116)
;; 4/5/6 → str-bytes-ptr / str-len directly
;; other → TRAMPOLINE_ERR = 1
;;
;; Fast paths (matched by exact pattern byte content):
;;
;;   P1 (len=25): "\\`-?[0-9]+\\(\\.[0-9]+\\)?\\'"
;;       → number: optional '-', one+ ASCII digits, optional '.' + digits
;;
;;   P2 (len=8):  "\\`{.*}\\'"
;;       = [92 96 123 46 42 125 92 39]
;;       → starts with '{' AND ends with '}'
;;
;;   P3 (len=34): "\\`[0-9]+\\.[0-9]+\\.[0-9]+\\.[0-9]+\\'"
;;       → IPv4 dotted-quad (four digit groups separated by '.')
;;
;;   P4a (len=12): "^[[:space:]]*$"
;;       = [94 91 58 115 112 97 99 101 58 93 42 36]
;;       → all whitespace (byte < 33, byte == 127, or U+00A0 NBSP)
;;
;;   P4b (len=14): "\\`[[:space:]]*\\'"
;;       = [92 96 91 58 115 112 97 99 101 58 93 42 92 39]
;;       → same all-whitespace condition
;;
;;   P5 (len=7):  "^[\u{00A0}]*$"
;;       = [94 91 194 160 93 42 36]
;;       → all U+00A0 NBSP bytes (UTF-8 pairs 194, 160)
;;
;;   P6 (len=4):  "[\n\r]"
;;       = [91 10 13 93]
;;       → contains newline (10) or carriage-return (13)
;;
;; Generic path (`_' branch):
;;   1. Detect anchored_start: pat starts with [92 96] or [94]
;;   2. Detect anchored_end:   pat ends   with [92 39] or [36]
;;   3. Strip front/back anchor bytes to determine literal body range [li..le)
;;   4. Copy literal (treating [92 x] as escape → x; other bytes verbatim)
;;   5. Match: exact / startswith / endswith / contains per anchor flags
;;
;; anchor_flags encoding (stored in scratch[plen]):
;;   0 = no anchors, 1 = end only, 2 = start only, 3 = both
;;   as = flags / 2 = (if (>= flags 2) 1 0)
;;   ae = flags - as*2 = (if (or (= flags 1) (= flags 3)) 1 0)
;;      = flags - (* (if (>= flags 2) 1 0) 2)
;;
;; Grammar ops consumed:
;;   `sexp-tag'          — read tag byte at offset 0
;;   `str-bytes-ptr'     — *const u8 data pointer of Str/MutStr/Symbol
;;   `str-len'           — byte length of Str/MutStr/Symbol
;;   `ptr-read-u8'       — load u8 from raw pointer + byte offset
;;   `ptr-write-u8'      — store u8 into raw buffer (returns 1 sentinel)
;;   `sexp-write-t'      — write Sexp::T into *out slot
;;   `sexp-write-nil'    — write Sexp::Nil into *out slot
;;   `alloc-bytes'       — allocate scratch buffer
;;   `dealloc-bytes'     — free scratch buffer
;;   Phase 47 arith: `+', `-', `*', `>=', `<=', `>', `<', `=', `if', `or', `and'
;;
;; Build wiring:
;;   `scripts/compile-elisp-objects.el' manifest: entry →
;;   `nl_jit_string_match_p.o' (Linux x86_64 only).
;;   `build-tool/build.rs' manifest_sources: entry for
;;   `nelisp-cc-jit-string-match-p.el'.
;;   `build-tool/src/jit/bridge.rs': `extern "C" fn nl_jit_string_match_p'
;;   + `_ELISP_ARCHIVE_ANCHOR' count 61→62.
;;   `build-tool/src/jit/regex.rs': Rust body deleted.

;;; Code:

(defconst nelisp-cc-jit-string-match-p--source
  '(seq
    ;; ===========================================================
    ;; §A. Low-level byte comparison helpers
    ;; ===========================================================

    ;; ---- nl_smp_bytes_eq (p1 p2 i n) ------------------------------
    ;;
    ;; Compare bytes [i..n) of raw buffers P1 and P2.
    ;; Returns 1 if all bytes equal, 0 otherwise.
    ;; Arity 4 (even).
    (defun nl_smp_bytes_eq (p1 p2 i n)
      (if (>= i n)
          1
        (if (= (ptr-read-u8 p1 i) (ptr-read-u8 p2 i))
            (nl_smp_bytes_eq p1 p2 (+ i 1) n)
          0)))

    ;; ---- nl_smp_starts_with (tp tl pp pl) -------------------------
    ;;
    ;; Returns 1 iff text [tp..tp+tl) starts with prefix [pp..pp+pl).
    ;; Arity 4 (even).
    (defun nl_smp_starts_with (tp tl pp pl)
      (if (> pl tl)
          0
        (nl_smp_bytes_eq tp pp 0 pl)))

    ;; ---- nl_smp_ends_with (tp tl pp pl) ---------------------------
    ;;
    ;; Returns 1 iff text [tp..tp+tl) ends with suffix [pp..pp+pl).
    ;; Shifts text base pointer by (tl-pl) and compares [0..pl).
    ;; Arity 4 (even).
    (defun nl_smp_ends_with (tp tl pp pl)
      (if (> pl tl)
          0
        (nl_smp_bytes_eq (+ tp (- tl pl)) pp 0 pl)))

    ;; ---- nl_smp_find_at (tp pp pl start i) ------------------------
    ;;
    ;; Returns 1 if needle pp[0..pl) matches text at position start,
    ;; comparing byte index i within the needle (initial call: i=0).
    ;; Arity 5 (odd).
    (defun nl_smp_find_at (tp pp pl start i)
      (if (>= i pl)
          1
        (if (= (ptr-read-u8 tp (+ start i)) (ptr-read-u8 pp i))
            (nl_smp_find_at tp pp pl start (+ i 1))
          0)))

    ;; ---- nl_smp_contains (tp tl pp pl pos) ------------------------
    ;;
    ;; Returns 1 iff text [tp..tp+tl) contains needle [pp..pp+pl).
    ;; POS is the current trial start (initial call: 0).
    ;; Arity 5 (odd).
    (defun nl_smp_contains (tp tl pp pl pos)
      (if (> (+ pos pl) tl)
          0
        (if (= (nl_smp_find_at tp pp pl pos 0) 1)
            1
          (nl_smp_contains tp tl pp pl (+ pos 1)))))

    ;; ===========================================================
    ;; §B. Nil / T literal byte writers
    ;; ===========================================================

    ;; ---- nl_smp_write_nil (buf) ------------------------------------
    ;;
    ;; Write "nil" bytes [110 105 108] into 4-byte scratch BUF.
    ;; Returns BUF (pointer value) for chaining.
    ;; Arity 1 (odd).
    (defun nl_smp_write_nil (buf)
      (and
       (ptr-write-u8 buf 0 110)
       (ptr-write-u8 buf 1 105)
       (ptr-write-u8 buf 2 108)
       buf))

    ;; ---- nl_smp_write_t (buf) --------------------------------------
    ;;
    ;; Write "t" byte [116] into scratch BUF.
    ;; Returns BUF (pointer value) for chaining.
    ;; Arity 1 (odd).
    (defun nl_smp_write_t (buf)
      (and (ptr-write-u8 buf 0 116) buf))

    ;; ===========================================================
    ;; §C. Argument bytes-ptr and length accessors
    ;; ===========================================================

    ;; ---- nl_smp_arg_bytes_ptr (arg nilbuf) ------------------------
    ;;
    ;; Returns the *const u8 data pointer for ARG:
    ;;   tag=0 (Nil) → write "nil" into NILBUF, return NILBUF
    ;;   tag=1 (T)   → write "t"   into NILBUF, return NILBUF
    ;;   tag=4/5/6   → str-bytes-ptr(arg)
    ;;   other       → 0 (caller must check via nl_smp_arg_len returning -1)
    ;; Arity 2 (even).
    (defun nl_smp_arg_bytes_ptr (arg nilbuf)
      (if (= (sexp-tag arg) 0)
          (nl_smp_write_nil nilbuf)
        (if (= (sexp-tag arg) 1)
            (nl_smp_write_t nilbuf)
          (if (or (= (sexp-tag arg) 4)
                  (= (sexp-tag arg) 5)
                  (= (sexp-tag arg) 6))
              (str-bytes-ptr arg)
            0))))

    ;; ---- nl_smp_arg_len (arg) -------------------------------------
    ;;
    ;; Returns byte length for ARG:
    ;;   tag=0 → 3, tag=1 → 1, tag=4/5/6 → str-len(arg), other → -1
    ;; Arity 1 (odd).
    (defun nl_smp_arg_len (arg)
      (if (= (sexp-tag arg) 0)
          3
        (if (= (sexp-tag arg) 1)
            1
          (if (or (= (sexp-tag arg) 4)
                  (= (sexp-tag arg) 5)
                  (= (sexp-tag arg) 6))
              (str-len arg)
            -1))))

    ;; ===========================================================
    ;; §D. Fast-path matchers
    ;; ===========================================================

    ;; ---- nl_smp_digits (tp tl i need1) ----------------------------
    ;;
    ;; Walk ASCII digits from index I in text [tp..tp+tl).
    ;; NEED1=1: at least one digit required; NEED1=0: zero ok.
    ;; Returns final I (past last digit) or -1 if needed but not found.
    ;; Arity 4 (even).
    (defun nl_smp_digits (tp tl i need1)
      (if (>= i tl)
          (if (= need1 0) i -1)
        (if (and (>= (ptr-read-u8 tp i) 48)
                 (<= (ptr-read-u8 tp i) 57))
            (nl_smp_digits tp tl (+ i 1) 0)
          (if (= need1 1) -1 i))))

    ;; ---- nl_smp_num_tail (tp tl i) --------------------------------
    ;;
    ;; Called after integer part ending at I.
    ;; Optionally consumes '.' + one+ digits and checks end of text.
    ;; Returns 1 if complete match, 0 otherwise.
    ;; Arity 3 (odd).
    (defun nl_smp_num_tail (tp tl i)
      (if (= i tl)
          1
        (if (and (< i tl) (= (ptr-read-u8 tp i) 46))
            (nl_smp_num_dec tp tl (nl_smp_digits tp tl (+ i 1) 1))
          0)))

    ;; ---- nl_smp_num_dec (tp tl i) ---------------------------------
    ;;
    ;; After scanning decimal digits (result I, -1 on error).
    ;; Returns 1 iff I = tl (text fully consumed).
    ;; Arity 3 (odd).
    (defun nl_smp_num_dec (tp tl i)
      (if (= i -1) 0 (if (= i tl) 1 0)))

    ;; ---- nl_smp_match_number (tp tl) ------------------------------
    ;;
    ;; P1: optional '-', one+ digits, optional '.' + one+ digits.
    ;; Arity 2 (even).
    (defun nl_smp_match_number (tp tl)
      (nl_smp_num_tail tp tl
                       (nl_smp_digits tp tl
                                      (if (and (< 0 tl)
                                               (= (ptr-read-u8 tp 0) 45))
                                          1 0)
                                      1)))

    ;; ---- nl_smp_match_ipv4 (tp tl) --------------------------------
    ;;
    ;; P3: four digit groups separated by '.', nothing else.
    ;; Arity 2 (even).
    (defun nl_smp_match_ipv4 (tp tl)
      (nl_smp_ipv4_g1 tp tl (nl_smp_digits tp tl 0 1)))

    ;; ---- nl_smp_ipv4_g1 (tp tl i) ---------------------------------
    ;; Arity 3 (odd).
    (defun nl_smp_ipv4_g1 (tp tl i)
      (if (= i -1)
          0
        (if (and (< i tl) (= (ptr-read-u8 tp i) 46))
            (nl_smp_ipv4_g2 tp tl (nl_smp_digits tp tl (+ i 1) 1))
          0)))

    ;; ---- nl_smp_ipv4_g2 (tp tl i) ---------------------------------
    ;; Arity 3 (odd).
    (defun nl_smp_ipv4_g2 (tp tl i)
      (if (= i -1)
          0
        (if (and (< i tl) (= (ptr-read-u8 tp i) 46))
            (nl_smp_ipv4_g3 tp tl (nl_smp_digits tp tl (+ i 1) 1))
          0)))

    ;; ---- nl_smp_ipv4_g3 (tp tl i) ---------------------------------
    ;; Arity 3 (odd).
    (defun nl_smp_ipv4_g3 (tp tl i)
      (if (= i -1)
          0
        (if (and (< i tl) (= (ptr-read-u8 tp i) 46))
            (nl_smp_ipv4_g4 tp tl (nl_smp_digits tp tl (+ i 1) 1))
          0)))

    ;; ---- nl_smp_ipv4_g4 (tp tl i) ---------------------------------
    ;; Arity 3 (odd).
    (defun nl_smp_ipv4_g4 (tp tl i)
      (if (= i -1) 0 (if (= i tl) 1 0)))

    ;; ---- nl_smp_all_ws (tp tl i) ----------------------------------
    ;;
    ;; P4a/P4b: returns 1 iff text[i..tl) is all ASCII whitespace
    ;; (byte < 33 or byte == 127) or U+00A0 NBSP ([194 160] pairs).
    ;; Arity 3 (odd).
    (defun nl_smp_all_ws (tp tl i)
      (if (>= i tl)
          1
        (if (or (< (ptr-read-u8 tp i) 33)
                (= (ptr-read-u8 tp i) 127))
            (nl_smp_all_ws tp tl (+ i 1))
          (if (and (= (ptr-read-u8 tp i) 194)
                   (< (+ i 1) tl)
                   (= (ptr-read-u8 tp (+ i 1)) 160))
              (nl_smp_all_ws tp tl (+ i 2))
            0))))

    ;; ---- nl_smp_all_nbsp (tp tl i) --------------------------------
    ;;
    ;; P5: returns 1 iff text is all U+00A0 NBSP [194 160] pairs.
    ;; Arity 3 (odd).
    (defun nl_smp_all_nbsp (tp tl i)
      (if (>= i tl)
          1
        (if (and (= (ptr-read-u8 tp i) 194)
                 (< (+ i 1) tl)
                 (= (ptr-read-u8 tp (+ i 1)) 160))
            (nl_smp_all_nbsp tp tl (+ i 2))
          0)))

    ;; ---- nl_smp_has_newline (tp tl i) -----------------------------
    ;;
    ;; P6: returns 1 iff text has byte 10 (\n) or 13 (\r).
    ;; Arity 3 (odd).
    (defun nl_smp_has_newline (tp tl i)
      (if (>= i tl)
          0
        (if (or (= (ptr-read-u8 tp i) 10)
                (= (ptr-read-u8 tp i) 13))
            1
          (nl_smp_has_newline tp tl (+ i 1)))))

    ;; ===========================================================
    ;; §E. Generic fallback — strip anchors, literal match
    ;; ===========================================================

    ;; ---- nl_smp_front_skip (pp plen) ------------------------------
    ;;
    ;; Returns byte index where literal body starts (0, 1, or 2).
    ;; [92 96] at front → 2; [94] at front → 1; else → 0.
    ;; Arity 2 (even).
    (defun nl_smp_front_skip (pp plen)
      (if (>= plen 2)
          (if (and (= (ptr-read-u8 pp 0) 92)
                   (= (ptr-read-u8 pp 1) 96))
              2
            (if (= (ptr-read-u8 pp 0) 94) 1 0))
        (if (and (>= plen 1) (= (ptr-read-u8 pp 0) 94)) 1 0)))

    ;; ---- nl_smp_back_end (pp plen) --------------------------------
    ;;
    ;; Returns end index (exclusive) of literal body.
    ;; ends with [92 39] → plen-2; ends with [36] → plen-1; else → plen.
    ;; Arity 2 (even).
    (defun nl_smp_back_end (pp plen)
      (if (>= plen 2)
          (if (and (= (ptr-read-u8 pp (- plen 1)) 39)
                   (= (ptr-read-u8 pp (- plen 2)) 92))
              (- plen 2)
            (if (= (ptr-read-u8 pp (- plen 1)) 36)
                (- plen 1)
              plen))
        (if (and (>= plen 1) (= (ptr-read-u8 pp (- plen 1)) 36))
            (- plen 1)
          plen)))

    ;; ---- nl_smp_anchor_flags (pp plen) ----------------------------
    ;;
    ;; Compute anchor_flags = as*2 + ae where:
    ;;   as = 1 if pat starts with [92 96] or [94], else 0
    ;;   ae = 1 if pat ends   with [92 39] or [36],  else 0
    ;; as = (front_skip > 0); ae = (back_end < plen).
    ;; Arity 2 (even).
    (defun nl_smp_anchor_flags (pp plen)
      (+ (* (if (> (nl_smp_front_skip pp plen) 0) 1 0) 2)
         (if (< (nl_smp_back_end pp plen) plen) 1 0)))

    ;; ---- nl_smp_lit_count (pp le i acc) ---------------------------
    ;;
    ;; Count output bytes in pattern range [i..le):
    ;;   [92 x] escape pair → 1 output byte (advance 2 source bytes)
    ;;   other              → 1 output byte (advance 1 source byte)
    ;; Arity 4 (even).
    (defun nl_smp_lit_count (pp le i acc)
      (if (>= i le)
          acc
        (if (and (= (ptr-read-u8 pp i) 92)
                 (< (+ i 1) le))
            (nl_smp_lit_count pp le (+ i 2) (+ acc 1))
          (nl_smp_lit_count pp le (+ i 1) (+ acc 1)))))

    ;; ---- nl_smp_lit_copy (pp le pi buf bi) ------------------------
    ;;
    ;; Copy literal bytes from pattern range [pi..le) into BUF at BI.
    ;; [92 x] → write x; other bytes → write verbatim.
    ;; Returns final BI (= number of bytes written = literal length).
    ;; ptr-write-u8 returns 1 (sentinel), so `and' always proceeds.
    ;; Arity 5 (odd).
    (defun nl_smp_lit_copy (pp le pi buf bi)
      (if (>= pi le)
          bi
        (if (and (= (ptr-read-u8 pp pi) 92)
                 (< (+ pi 1) le))
            (and
             (ptr-write-u8 buf bi (ptr-read-u8 pp (+ pi 1)))
             (nl_smp_lit_copy pp le (+ pi 2) buf (+ bi 1)))
          (and
           (ptr-write-u8 buf bi (ptr-read-u8 pp pi))
           (nl_smp_lit_copy pp le (+ pi 1) buf (+ bi 1))))))

    ;; ---- nl_smp_generic_do_match (tp tl buf ll as ae) -------------
    ;;
    ;; Final dispatch: match text [tp..tp+tl) against literal [buf..buf+ll)
    ;; with anchor flags as (start) and ae (end).
    ;; Returns 1 if match, 0 otherwise.
    ;; Arity 6 (even).
    (defun nl_smp_generic_do_match (tp tl buf ll as ae)
      (if (and (= as 1) (= ae 1))
          ;; anchored both → exact equality
          (if (= ll tl) (nl_smp_bytes_eq tp buf 0 ll) 0)
        (if (= as 1)
            ;; anchored start → startswith
            (nl_smp_starts_with tp tl buf ll)
          (if (= ae 1)
              ;; anchored end → endswith
              (nl_smp_ends_with tp tl buf ll)
            ;; unanchored → contains
            (nl_smp_contains tp tl buf ll 0)))))

    ;; ---- nl_smp_gen_copy (pp tp tl scratch flags li le) -----------
    ;;
    ;; (7 values needed → decompose)
    ;;
    ;; Entry with li and le available (from step above).
    ;; Copies literal into scratch, then reads flags from scratch[plen].
    ;; But we don't have plen here!
    ;;
    ;; Instead we pass flags directly through the chain.
    ;; Arity-6 limit: use (pp tp tl scratch li le) and recompute flags.
    ;; as = (if (> li 0) 1 0); ae = flags - as*2 ...but flags is dropped.
    ;;
    ;; Alternative: as = (if (> li 0) 1 0) (no plen needed!)
    ;; ae: back_end result le < plen means ae=1.
    ;;     We don't have plen, but we know le was computed from back_end.
    ;;     If there was a trailing anchor, le < original_plen.
    ;;     Equivalently: check pp[le] and pp[le+1] — if those bytes are
    ;;     the anchor, ae=1.  But we can't read pp[le] without plen guard.
    ;;
    ;; THE SOLUTION: store anchor_flags in scratch[plen] BEFORE calling
    ;; gen_copy, using nl_smp_anchor_flags(pp plen) computed when plen
    ;; is available.  Then gen_copy reads it back from scratch[plen].
    ;; But plen is not in scope in gen_copy.
    ;;
    ;; TWO-STAGE approach:
    ;;   Stage 1 (nl_smp_gen_prep): has pp+plen.  Computes flags, li, le.
    ;;     Writes flags into scratch[plen].  Then calls Stage 2 with
    ;;     (pp tp tl scratch li le) — 6 args.
    ;;   Stage 2 (nl_smp_gen_exec): has pp, tp, tl, scratch, li, le.
    ;;     Needs flags (= scratch[plen]).  But no plen to index scratch!
    ;;
    ;; ONLY solution without plen in Stage 2: write flags at a FIXED
    ;; offset rather than plen-relative.  E.g., scratch[-1] (= one byte
    ;; before the buffer — but that's pre-allocation, unsafe).
    ;;
    ;; ACTUAL SOLUTION: change scratch allocation to reserve 8 bytes
    ;; at the END.  The caller (nl_smp_with_scratch) allocates (plen+8)
    ;; bytes.  Store flags at scratch[plen].  In gen_exec, reconstruct
    ;; from (pp+li): as = (li>0 ? 1 : 0); ae from check at pp[le..le+1].
    ;;
    ;; Since we checked: ae = back_end(pp plen) < plen.  In gen_exec,
    ;; we have le = back_end result.  We also have pp.  We can read
    ;; pp[le] IF le is valid (< allocation size).  The allocation is
    ;; plen+8, so pp[le] is valid since le <= plen < plen+8.
    ;; pp[le] == 36 OR (pp[le]==92 AND pp[le+1]==39) → ae=1.
    ;; pp[le] == OTHER → ae=0 (= le was already plen, no trailing anchor).
    ;;
    ;; But wait: we're checking the PATTERN pointer (pp), not scratch.
    ;; pp is the original pattern, and le = back_end(pp plen) is an
    ;; index into pp.  Reading pp[le] might be out of bounds if le=plen.
    ;; The pattern is allocated by the caller as a Sexp::Str payload;
    ;; reading one byte past the end is typically safe (there's a NUL
    ;; or padding), but it's technically UB.
    ;;
    ;; CLEAN SOLUTION: just pass ae explicitly through the chain.
    ;; This means ONE of the 6 args in gen_exec must be ae.
    ;; That means we need to drop one of (pp, tp, tl, scratch, li, le).
    ;; Only candidate to drop: pp (if lit_copy doesn't need it post-call).
    ;; lit_copy(pp le pi buf bi) DOES need pp throughout the copy.
    ;; But if we CALL lit_copy FROM gen_prep (where we have pp), we can
    ;; then call gen_exec with the result ll.
    ;;
    ;; FINAL FINAL FINAL approach:
    ;;
    ;; nl_smp_gen_prep(pp plen tp tl scratch) [5 args, odd]:
    ;;   flags = nl_smp_anchor_flags(pp plen)
    ;;   li    = nl_smp_front_skip(pp plen)
    ;;   le    = nl_smp_back_end(pp plen)
    ;;   ll    = nl_smp_lit_copy(pp le li scratch 0)
    ;;   → nl_smp_gen_exec(tp tl scratch ll flags)  [5 args, odd]
    ;;
    ;; nl_smp_gen_exec(tp tl scratch ll flags) [5 args, odd]:
    ;;   as = (if (>= flags 2) 1 0)
    ;;   ae = (- flags (* (if (>= flags 2) 1 0) 2))
    ;;     = (if (or (= flags 1) (= flags 3)) 1 0)
    ;;   → nl_smp_generic_do_match(tp tl scratch ll as ae) [6 args, even]
    ;;
    ;; PROBLEM: gen_prep must call lit_copy (which returns ll), then
    ;; pass ll to gen_exec.  But gen_prep can't do this without let*.
    ;; With call chaining: gen_prep calls gen_exec_with_ll which takes
    ;; (tp tl scratch flags ll) — with ll = return value of lit_copy.
    ;;
    ;; With NESTED call expression:
    ;;   (nl_smp_gen_exec tp tl scratch
    ;;                    (nl_smp_lit_copy pp le li scratch 0)
    ;;                    flags)
    ;; This works IF gen_prep has tp, tl, scratch, flags, AND the
    ;; intermediate values pp, le, li needed for lit_copy.
    ;;
    ;; gen_prep args: (pp plen tp tl scratch) — has pp, plen.
    ;; Need to compute flags, li, le SEPARATELY and pass them as
    ;; arguments to a nested call.  But each one is a function call result.
    ;;
    ;; In Phase 47, function arguments are evaluated LEFT TO RIGHT.
    ;; The call site evaluates all args before the call.
    ;;
    ;; So we can write:
    ;;   (nl_smp_gen_exec tp tl scratch
    ;;                    (nl_smp_lit_copy_range pp plen scratch)
    ;;                    (nl_smp_anchor_flags pp plen))
    ;; where nl_smp_lit_copy_range(pp plen scratch) [3 odd]:
    ;;   = nl_smp_lit_copy(pp (back_end pp plen) (front_skip pp plen) scratch 0)
    ;; This is a 5-arg call to lit_copy, with back_end and front_skip
    ;; as nested calls.  VALID in Phase 47 (args are arbitrary expressions).
    ;;
    ;; And nl_smp_gen_exec(tp tl scratch ll flags) [5 odd]:
    ;;   calls do_match with decoded as/ae.
    ;;
    ;; This decomposition WORKS and needs only:
    ;;   nl_smp_lit_copy_range(pp plen scratch) [3 odd]
    ;;   nl_smp_gen_exec(tp tl scratch ll flags) [5 odd]
    ;;   nl_smp_generic_match(pp plen tp tl scratch) [5 odd]:
    ;;     calls nl_smp_gen_exec(tp tl scratch
    ;;                           (nl_smp_lit_copy_range pp plen scratch)
    ;;                           (nl_smp_anchor_flags pp plen))
    ;;
    ;; Let's implement this.

    ;; ---- nl_smp_lit_copy_range (pp plen scratch) -------------------
    ;;
    ;; Combines front_skip, back_end, and lit_copy into one call:
    ;;   li = front_skip(pp plen)
    ;;   le = back_end(pp plen)
    ;;   returns lit_copy(pp le li scratch 0) = literal length (ll)
    ;; Arity 3 (odd).
    (defun nl_smp_lit_copy_range (pp plen scratch)
      (nl_smp_lit_copy pp
                       (nl_smp_back_end   pp plen)
                       (nl_smp_front_skip pp plen)
                       scratch 0))

    ;; ---- nl_smp_gen_exec (tp tl scratch ll flags) -----------------
    ;;
    ;; Decodes anchor flags and calls nl_smp_generic_do_match.
    ;; flags: 0=none, 1=end, 2=start, 3=both.
    ;; as = (if (>= flags 2) 1 0)
    ;; ae = (flags - as*2) = (if (or (= flags 1) (= flags 3)) 1 0)
    ;; Arity 5 (odd).
    (defun nl_smp_gen_exec (tp tl scratch ll flags)
      (nl_smp_generic_do_match tp tl scratch ll
                               (if (>= flags 2) 1 0)
                               (if (or (= flags 1) (= flags 3)) 1 0)))

    ;; ---- nl_smp_generic_match (pp plen tp tl scratch) -------------
    ;;
    ;; Entry point for the generic fallback.
    ;; Evaluates lit_copy_range (copies literal into scratch, returns ll)
    ;; and anchor_flags IN PARALLEL AS ARGS to gen_exec.
    ;; In SysV AMD64 arg evaluation order (left-to-right):
    ;;   1. tp  (from caller stack slot)
    ;;   2. tl  (from caller stack slot)
    ;;   3. scratch (from caller stack slot)
    ;;   4. ll  = nl_smp_lit_copy_range(pp plen scratch)  [runs first of nested]
    ;;   5. flags = nl_smp_anchor_flags(pp plen)          [runs second]
    ;; lit_copy_range fills scratch[0..ll-1] before flags is computed.
    ;; Arity 5 (odd).
    (defun nl_smp_generic_match (pp plen tp tl scratch)
      (nl_smp_gen_exec tp tl scratch
                       (nl_smp_lit_copy_range pp plen scratch)
                       (nl_smp_anchor_flags   pp plen)))

    ;; ===========================================================
    ;; §F. Pattern dispatch: fast paths + generic fallback
    ;; ===========================================================

    ;; ---- nl_smp_dispatch (pp plen tp tl scratch) ------------------
    ;;
    ;; Central dispatch.  Checks fast-path pattern lengths and byte
    ;; sequences, delegating to specific matchers or the generic fallback.
    ;; Returns 1 if match, 0 otherwise.
    ;; Arity 5 (odd).
    (defun nl_smp_dispatch (pp plen tp tl scratch)
      (if (= plen 4)
          ;; P6: "[\n\r]" = [91 10 13 93]
          (if (and (= (ptr-read-u8 pp 0) 91)
                   (= (ptr-read-u8 pp 1) 10)
                   (= (ptr-read-u8 pp 2) 13)
                   (= (ptr-read-u8 pp 3) 93))
              (nl_smp_has_newline tp tl 0)
            (nl_smp_generic_match pp plen tp tl scratch))
        (if (= plen 7)
            ;; P5: "^[\u{00A0}]*$" = [94 91 194 160 93 42 36]
            (if (and (= (ptr-read-u8 pp 0) 94)
                     (= (ptr-read-u8 pp 1) 91)
                     (= (ptr-read-u8 pp 2) 194)
                     (= (ptr-read-u8 pp 3) 160)
                     (= (ptr-read-u8 pp 4) 93)
                     (= (ptr-read-u8 pp 5) 42)
                     (= (ptr-read-u8 pp 6) 36))
                (nl_smp_all_nbsp tp tl 0)
              (nl_smp_generic_match pp plen tp tl scratch))
          (if (= plen 8)
              ;; P2: "\\`{.*}\\'" = [92 96 123 46 42 125 92 39]
              (if (and (= (ptr-read-u8 pp 0) 92)
                       (= (ptr-read-u8 pp 1) 96)
                       (= (ptr-read-u8 pp 2) 123)
                       (= (ptr-read-u8 pp 3) 46)
                       (= (ptr-read-u8 pp 4) 42)
                       (= (ptr-read-u8 pp 5) 125)
                       (= (ptr-read-u8 pp 6) 92)
                       (= (ptr-read-u8 pp 7) 39))
                  ;; starts with '{' AND ends with '}'
                  (if (and (> tl 0)
                           (= (ptr-read-u8 tp 0) 123)
                           (= (ptr-read-u8 tp (- tl 1)) 125))
                      1
                    0)
                (nl_smp_generic_match pp plen tp tl scratch))
            (if (= plen 12)
                ;; P4a: "^[[:space:]]*$" = [94 91 58 115 112 97 99 101 58 93 42 36]
                (if (and (= (ptr-read-u8 pp  0) 94)
                         (= (ptr-read-u8 pp  1) 91)
                         (= (ptr-read-u8 pp  2) 58)
                         (= (ptr-read-u8 pp  3) 115)
                         (= (ptr-read-u8 pp  4) 112)
                         (= (ptr-read-u8 pp  5) 97)
                         (= (ptr-read-u8 pp  6) 99)
                         (= (ptr-read-u8 pp  7) 101)
                         (= (ptr-read-u8 pp  8) 58)
                         (= (ptr-read-u8 pp  9) 93)
                         (= (ptr-read-u8 pp 10) 42)
                         (= (ptr-read-u8 pp 11) 36))
                    (nl_smp_all_ws tp tl 0)
                  (nl_smp_generic_match pp plen tp tl scratch))
              (if (= plen 14)
                  ;; P4b: "\\`[[:space:]]*\\'" = [92 96 91 58 115 112 97 99 101 58 93 42 92 39]
                  (if (and (= (ptr-read-u8 pp  0) 92)
                           (= (ptr-read-u8 pp  1) 96)
                           (= (ptr-read-u8 pp  2) 91)
                           (= (ptr-read-u8 pp  3) 58)
                           (= (ptr-read-u8 pp  4) 115)
                           (= (ptr-read-u8 pp  5) 112)
                           (= (ptr-read-u8 pp  6) 97)
                           (= (ptr-read-u8 pp  7) 99)
                           (= (ptr-read-u8 pp  8) 101)
                           (= (ptr-read-u8 pp  9) 58)
                           (= (ptr-read-u8 pp 10) 93)
                           (= (ptr-read-u8 pp 11) 42)
                           (= (ptr-read-u8 pp 12) 92)
                           (= (ptr-read-u8 pp 13) 39))
                      (nl_smp_all_ws tp tl 0)
                    (nl_smp_generic_match pp plen tp tl scratch))
                (if (= plen 25)
                    ;; P1: "\\`-?[0-9]+\\(\\.[0-9]+\\)?\\'":
                    ;; [92 96 45 63 91 48 45 57 93 43 92 40 92 46 91 48 45 57 93 43 92 41 63 92 39]
                    (if (and (= (ptr-read-u8 pp  0) 92)
                             (= (ptr-read-u8 pp  1) 96)
                             (= (ptr-read-u8 pp  2) 45)
                             (= (ptr-read-u8 pp  3) 63)
                             (= (ptr-read-u8 pp  4) 91)
                             (= (ptr-read-u8 pp  5) 48)
                             (= (ptr-read-u8 pp  6) 45)
                             (= (ptr-read-u8 pp  7) 57)
                             (= (ptr-read-u8 pp  8) 93)
                             (= (ptr-read-u8 pp  9) 43)
                             (= (ptr-read-u8 pp 10) 92)
                             (= (ptr-read-u8 pp 11) 40)
                             (= (ptr-read-u8 pp 12) 92)
                             (= (ptr-read-u8 pp 13) 46)
                             (= (ptr-read-u8 pp 14) 91)
                             (= (ptr-read-u8 pp 15) 48)
                             (= (ptr-read-u8 pp 16) 45)
                             (= (ptr-read-u8 pp 17) 57)
                             (= (ptr-read-u8 pp 18) 93)
                             (= (ptr-read-u8 pp 19) 43)
                             (= (ptr-read-u8 pp 20) 92)
                             (= (ptr-read-u8 pp 21) 41)
                             (= (ptr-read-u8 pp 22) 63)
                             (= (ptr-read-u8 pp 23) 92)
                             (= (ptr-read-u8 pp 24) 39))
                        (nl_smp_match_number tp tl)
                      (nl_smp_generic_match pp plen tp tl scratch))
                  (if (= plen 34)
                      ;; P3: "\\`[0-9]+\\.[0-9]+\\.[0-9]+\\.[0-9]+\\'"
                      ;; [92 96 91 48 45 57 93 43 92 46 91 48 45 57 93 43
                      ;;  92 46 91 48 45 57 93 43 92 46 91 48 45 57 93 43 92 39]
                      (if (and (= (ptr-read-u8 pp  0) 92)
                               (= (ptr-read-u8 pp  1) 96)
                               (= (ptr-read-u8 pp  2) 91)
                               (= (ptr-read-u8 pp  3) 48)
                               (= (ptr-read-u8 pp  4) 45)
                               (= (ptr-read-u8 pp  5) 57)
                               (= (ptr-read-u8 pp  6) 93)
                               (= (ptr-read-u8 pp  7) 43)
                               (= (ptr-read-u8 pp  8) 92)
                               (= (ptr-read-u8 pp  9) 46)
                               (= (ptr-read-u8 pp 10) 91)
                               (= (ptr-read-u8 pp 11) 48)
                               (= (ptr-read-u8 pp 12) 45)
                               (= (ptr-read-u8 pp 13) 57)
                               (= (ptr-read-u8 pp 14) 93)
                               (= (ptr-read-u8 pp 15) 43)
                               (= (ptr-read-u8 pp 16) 92)
                               (= (ptr-read-u8 pp 17) 46)
                               (= (ptr-read-u8 pp 18) 91)
                               (= (ptr-read-u8 pp 19) 48)
                               (= (ptr-read-u8 pp 20) 45)
                               (= (ptr-read-u8 pp 21) 57)
                               (= (ptr-read-u8 pp 22) 93)
                               (= (ptr-read-u8 pp 23) 43)
                               (= (ptr-read-u8 pp 24) 92)
                               (= (ptr-read-u8 pp 25) 46)
                               (= (ptr-read-u8 pp 26) 91)
                               (= (ptr-read-u8 pp 27) 48)
                               (= (ptr-read-u8 pp 28) 45)
                               (= (ptr-read-u8 pp 29) 57)
                               (= (ptr-read-u8 pp 30) 93)
                               (= (ptr-read-u8 pp 31) 43)
                               (= (ptr-read-u8 pp 32) 92)
                               (= (ptr-read-u8 pp 33) 39))
                          (nl_smp_match_ipv4 tp tl)
                        (nl_smp_generic_match pp plen tp tl scratch))
                    ;; All other patterns → generic fallback.
                    (nl_smp_generic_match pp plen tp tl scratch)))))))))

    ;; ===========================================================
    ;; §G. Scratch allocator + dispatch wrapper
    ;; ===========================================================

    ;; Wait — the naive approach is wrong: we allocate the scratch INSIDE the
    ;; arg list of nl_smp_free_scratch, so we'd allocate it twice.
    ;; We need to allocate scratch ONCE and pass the same pointer to
    ;; dispatch AND to free_scratch.
    ;;
    ;; Without let*, we must thread via an additional helper:
    ;;
    ;;   nl_smp_with_scratch(pp plen tp tl) [4 even]:
    ;;     calls nl_smp_do_dispatch(pp plen tp tl, alloc-bytes (plen+8) 1)
    ;;
    ;;   nl_smp_do_dispatch(pp plen tp tl scratch) [5 odd]:
    ;;     result = nl_smp_dispatch(pp plen tp tl scratch)
    ;;     dealloc scratch
    ;;     return result
    ;;   But again can't bind result without let*.
    ;;
    ;;   Chain via nl_smp_dealloc_and_return:
    ;;   nl_smp_do_dispatch(pp plen tp tl scratch) [5 odd]:
    ;;     calls nl_smp_dealloc_ret(scratch, (plen+8),
    ;;                              nl_smp_dispatch(pp plen tp tl scratch))
    ;;
    ;;   nl_smp_dealloc_ret(scratch size result) [3 odd]:
    ;;     and (dealloc-bytes scratch size 1) result
    ;;   Returns result (last of `and').
    ;;
    ;; But `and' returns the last truthy value.  If result=0 (no match),
    ;; `and' short-circuits at 0 and returns 0 (falsy).
    ;; Actually: (and (dealloc-bytes ...) result):
    ;;   dealloc-bytes returns 1 (truthy) → and evaluates result.
    ;;   If result=0, and returns 0.  That's the CORRECT return: match=0.
    ;;   If result=1, and returns 1.  Correct: match=1.
    ;; Wait: (and X Y) — if X is truthy, returns Y.  If X is falsy, returns nil.
    ;; dealloc-bytes returns 1 (truthy), so (and (dealloc...) result) = result.
    ;; Correct!
    ;;
    ;; But there's a subtle issue: in nl_smp_dealloc_ret, the args are
    ;; (scratch, size, result).  Phase 47 evaluates args LEFT TO RIGHT.
    ;; So: scratch is evaluated, then size, then nl_smp_dispatch is called.
    ;; dispatch WRITES into scratch, then returns.  Then dealloc_ret runs.
    ;; dealloc_ret frees scratch, then returns result.
    ;; ORDER: evaluate args → call dispatch (uses scratch) → dealloc_ret → free.
    ;; Wait: arg evaluation happens BEFORE the function body.
    ;; So in nl_smp_do_dispatch(pp plen tp tl scratch):
    ;;   CALL nl_smp_dealloc_ret with args:
    ;;     arg0 = scratch (evaluated to pointer value)
    ;;     arg1 = plen+8  (evaluated to size)
    ;;     arg2 = nl_smp_dispatch(pp plen tp tl scratch) (evaluated = runs dispatch)
    ;;   Then nl_smp_dealloc_ret body runs: (and (dealloc scratch size 1) result)
    ;; Order: arg0/arg1 evaluated (no side effects), then arg2 evaluated
    ;; (= dispatch writes into scratch), then dealloc_ret body frees scratch.
    ;; This is correct!

    ;; Fix nl_smp_with_scratch to avoid double alloc:

    ;; ---- nl_smp_dealloc_ret (scratch size result) -----------------
    ;;
    ;; Free SCRATCH of SIZE bytes and return RESULT.
    ;; (and (dealloc-bytes scratch size 1) result):
    ;;   dealloc returns 1 (truthy) → and returns result.
    ;;   result=0 → and returns 0.  result=1 → and returns 1.  Correct.
    ;; Arity 3 (odd).
    (defun nl_smp_dealloc_ret (scratch size result)
      (and (dealloc-bytes scratch size 1) result))

    ;; ---- nl_smp_do_dispatch (pp plen tp tl scratch) ---------------
    ;;
    ;; Calls dispatch (which fills scratch with literal bytes if needed),
    ;; then frees scratch and returns the match result.
    ;; Arity 5 (odd).
    (defun nl_smp_do_dispatch (pp plen tp tl scratch)
      (nl_smp_dealloc_ret scratch (+ plen 8)
                          (nl_smp_dispatch pp plen tp tl scratch)))

    ;; ---- nl_smp_with_scratch (pp plen tp tl) ----------------------
    ;;
    ;; Allocate one (plen+8)-byte scratch buffer, dispatch, free, return.
    ;; Arity 4 (even).
    (defun nl_smp_with_scratch (pp plen tp tl)
      (nl_smp_do_dispatch pp plen tp tl (alloc-bytes (+ plen 8) 1)))

    ;; ===========================================================
    ;; §H. Nil-buf helpers for nil/t literal coercion
    ;; ===========================================================

    ;; ---- nl_smp_pat_dispatch (pat-arg text-arg nilbuf1 nilbuf2) ---
    ;;
    ;; After type validation, get bytes+len for both args and dispatch.
    ;; Arity 4 (even).
    (defun nl_smp_pat_dispatch (pat-arg text-arg nilbuf1 nilbuf2)
      (nl_smp_with_scratch
       (nl_smp_arg_bytes_ptr pat-arg  nilbuf1)
       (nl_smp_arg_len pat-arg)
       (nl_smp_arg_bytes_ptr text-arg nilbuf2)
       (nl_smp_arg_len text-arg)))

    ;; ---- nl_smp_dealloc_nils_and_ret (nb1 nb2 result) ------------
    ;;
    ;; Free both 4-byte nil-literal scratch buffers and return result.
    ;; Arity 3 (odd).
    (defun nl_smp_dealloc_nils_and_ret (nb1 nb2 result)
      (and (dealloc-bytes nb1 4 1)
           (dealloc-bytes nb2 4 1)
           result))

    ;; ---- nl_smp_do_match (pat-arg text-arg nilbuf1 nilbuf2 out) ---
    ;;
    ;; Allocate nil-literal bufs, dispatch, free bufs, write to *out.
    ;; Arity 5 (odd).
    ;;
    ;; The `nl_smp_pat_dispatch' call is arg 3 of dealloc_nils_and_ret,
    ;; so it runs during arg evaluation (before dealloc body), which is
    ;; correct: dispatch uses nilbuf1/nilbuf2, then they are freed.
    ;; Then: (if (= result 1) sexp-write-t sexp-write-nil) → 0 return.
    (defun nl_smp_do_match (pat-arg text-arg nilbuf1 nilbuf2 out)
      (if (= (nl_smp_dealloc_nils_and_ret
              nilbuf1 nilbuf2
              (nl_smp_pat_dispatch pat-arg text-arg nilbuf1 nilbuf2))
             1)
          (and (sexp-write-t out) 0)
        (and (sexp-write-nil out) 0)))

    ;; ===========================================================
    ;; §I. Public trampoline
    ;; ===========================================================

    ;; ---- nl_jit_string_match_p (pat-arg text-arg out) -------------
    ;;
    ;; Signature: (pat-arg: *const Sexp, text-arg: *const Sexp,
    ;;             out: *mut Sexp) -> i64
    ;; Returns 0 (TRAMPOLINE_OK) or 1 (TRAMPOLINE_ERR).
    ;;
    ;; Nil-literal scratch: two independent 4-byte buffers (nilbuf1 for
    ;; pat, nilbuf2 for text) to hold "nil"/"t" bytes when the arg is
    ;; Sexp::Nil / Sexp::T.  Allocated here, freed inside nl_smp_do_match.
    ;; Arity 3 (odd).
    (defun nl_jit_string_match_p (pat-arg text-arg out)
      (if (= (nl_smp_arg_len pat-arg) -1)
          1   ; TRAMPOLINE_ERR: invalid pat-arg type
        (if (= (nl_smp_arg_len text-arg) -1)
            1 ; TRAMPOLINE_ERR: invalid text-arg type
          (nl_smp_do_match pat-arg text-arg
                           (alloc-bytes 4 1)
                           (alloc-bytes 4 1)
                           out)))))

  "Phase 47 source for the `nl_jit_string_match_p' trampoline.

Port of `build-tool/src/jit/regex.rs::nl_jit_string_match_p'.
No POSIX regex or regex crate — pure byte-level string matching
with fast paths for 6 common patterns and a generic fallback that
strips anchor metacharacters to produce a literal for
prefix / suffix / substring / equality matching.

Public trampoline: `nl_jit_string_match_p (pat-arg text-arg out)'
  — arity 3 (odd), out-2 ABI, reached via nl-jit-call-out-2.

Tag constants: SEXP_TAG_NIL=0, SEXP_TAG_T=1, SEXP_TAG_SYMBOL=4,
SEXP_TAG_STR=5, SEXP_TAG_MUT_STR=6.

Scratch allocation: (plen+8) bytes for generic fallback literal
buffer; two 4-byte buffers for Nil→\"nil\" / T→\"t\" coercions.

anchor_flags encoding: 0=none, 1=end-only, 2=start-only, 3=both.
as = (>= flags 2); ae = (flags mod 2) = (or (= flags 1) (= flags 3)).

Fast paths:
  P1 (len=25): number — optional sign + digits + optional decimal.
  P2 (len=8):  brace-enclosed — starts with '{' and ends with '}'.
  P3 (len=34): IPv4 dotted-quad — four digit groups separated by '.'.
  P4a (len=12): all whitespace — ^[[:space:]]*$.
  P4b (len=14): all whitespace — \\`[[:space:]]*\\'.
  P5 (len=7):  all NBSP — ^[\\u{00A0}]*$.
  P6 (len=4):  contains \\n or \\r — [\\n\\r].

Generic fallback: strip anchors (\\` \\' ^ $), process escapes
(\\.→. \\\\→\\), do exact/prefix/suffix/substring match.")

(provide 'nelisp-cc-jit-string-match-p)

;;; nelisp-cc-jit-string-match-p.el ends here
