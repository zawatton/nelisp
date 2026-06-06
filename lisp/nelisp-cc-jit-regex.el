;;; nelisp-cc-jit-regex.el --- AOT body for nl_jit_string_match_p  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; AOT migration of `nl_jit_string_match_p' from
;; `build-tool/src/jit/regex.rs'.
;;
;; Trampoline signature: `(*const Sexp, *const Sexp, *mut Sexp) -> i64'
;; (OK=0 / ERR=1), reached via `nl-jit-call-out-2' from
;; `lisp/nelisp-stdlib-regex.el::string-match-p'.
;;
;; The Rust implementation is a literal / anchored fast-path table — NOT a
;; full regex engine.  Pattern dispatch is by byte-length of the pattern
;; (all seven hard-coded patterns have unique byte lengths):
;;
;;   len 25  \\`-?[0-9]+\\(\\.[0-9]+\\)?\\' → numeric check
;;   len  8  \\`{.*}\\'                      → JSON obj: starts { ends }
;;   len 34  \\`[0-9]+\\.[0-9]+\\.[0-9]+\\.[0-9]+\\' → IPv4 check
;;   len 14  ^[[:space:]]*$                  → all-whitespace
;;   len 16  \\`[[:space:]]*\\'              → all-whitespace
;;   len  7  ^[\xC2\xA0]*$                  → all-NBSP
;;   len  4  [\n\r]                          → contains newline/CR
;;   other   fallback literal match          → anchor-strip + substr
;;
;; Fallback literal match:
;;   anchored_start = pat[0..2)==[92,96] (\\`) OR pat[0]==94 (^)
;;   anchored_end   = pat[-2..)==[92,39] (\\'） OR pat[-1]==36 ($)
;;   Strips anchors, matches processed literal against text.
;;   Escape rules: \\. → `.', \\\\ → `\\'; other bytes literal.
;;   asae flag (packed): as*2+ae (3=both, 2=start, 1=end, 0=neither).
;;
;; No heap allocation in fallback: inline byte-by-byte comparison with
;; escape processing (avoids alloc + 6-GP-arg limit issue).
;;
;; Tag constants (pinned by §62.5 ABI assert tests):
;;   SEXP_TAG_NIL    = 0
;;   SEXP_TAG_SYMBOL = 4
;;   SEXP_TAG_STR    = 5
;;   SEXP_TAG_MUT_STR = 6
;;
;; Build wiring:
;;   `scripts/compile-elisp-objects.el' manifest entry →
;;       `nl_jit_string_match_p.o'
;;   `build-tool/build.rs' manifest_sources: `"nelisp-cc-jit-regex.el"'
;;   `build-tool/src/jit/bridge.rs':
;;       `extern "C" fn nl_jit_string_match_p;'  (extern block)
;;       `nl_jit_string_match_p'                 (anchor array +1 → 68)
;;   `build-tool/src/jit/regex.rs': body deleted (empty file).
;;   `build-tool/src/jit/mod.rs': `mod regex;' removed.
;;
;; Max-arity discipline (AOT GP limit = 6):
;;   All defuns have ≤6 parameters.  No extern-call in any helper
;;   (only nl_jit_string_match_p is an extern target); the odd-arity
;;   stack-alignment footgun does not apply to internal calls.

;;; Code:

(defconst nelisp-cc-jit-regex--source
  '(seq

    ;; ---- Tag / length helpers -----------------------------------------------

    ;; nl_smp_is_str_tag: 1 if tag is an acceptable string tag (4/5/6), else 0.
    (defun nl_smp_is_str_tag (tag)
      (if (or (= tag 4) (= tag 5) (= tag 6)) 1 0))

    ;; nl_smp_str_len: unified byte-length dispatch.
    ;;   tag 4 (Symbol) or 5 (Str) → str-len
    ;;   tag 6 (MutStr)             → mut-str-len
    ;;   other                      → -1 (error sentinel)
    (defun nl_smp_str_len (sptr)
      (if (or (= (sexp-tag sptr) 5)
              (= (sexp-tag sptr) 4))
          (str-len sptr)
        (if (= (sexp-tag sptr) 6)
            (mut-str-len sptr)
          -1)))

    ;; ---- Byte-level character class helpers ---------------------------------

    ;; nl_smp_is_digit: 1 if byte b is ASCII digit [48..57].
    (defun nl_smp_is_digit (b)
      (if (and (>= b 48) (<= b 57)) 1 0))

    ;; nl_smp_is_ws: 1 if byte b is ASCII whitespace (HT LF VT FF CR SP).
    (defun nl_smp_is_ws (b)
      (if (or (= b 32) (and (>= b 9) (<= b 13))) 1 0))

    ;; ---- All-digits scan (recursive) ----------------------------------------

    ;; nl_smp_all_digits: 1 iff all bytes tbytes[i..end) are ASCII digits.
    (defun nl_smp_all_digits (tbytes i end)
      (if (= i end)
          1
        (if (= (nl_smp_is_digit (ptr-read-u8 tbytes i)) 1)
            (nl_smp_all_digits tbytes (+ i 1) end)
          0)))

    ;; ---- Numeric pattern: \\`-?[0-9]+\\(\\.[0-9]+\\)?\\'  -----------------

    ;; nl_smp_match_numeric: match text against the numeric pattern.
    ;; Logic: optional leading '-' (45), then digits, then optionally '.' + digits.
    (defun nl_smp_match_numeric (tbytes tlen)
      (if (= tlen 0)
          0
        (nl_smp_numeric_digits tbytes tlen
                               (if (= (ptr-read-u8 tbytes 0) 45) 1 0)
                               (if (= (ptr-read-u8 tbytes 0) 45) 1 0))))

    ;; nl_smp_numeric_digits: scan first digit run from start/i.
    (defun nl_smp_numeric_digits (tbytes tlen start i)
      (if (= i tlen)
          (if (> i start) 1 0)
        (if (= (nl_smp_is_digit (ptr-read-u8 tbytes i)) 1)
            (nl_smp_numeric_digits tbytes tlen start (+ i 1))
          (if (= i start)
              0
            (if (and (= (ptr-read-u8 tbytes i) 46)
                     (< (+ i 1) tlen))
                (nl_smp_all_digits tbytes (+ i 1) tlen)
              0)))))

    ;; ---- JSON object: \\`{.*}\\'  -------------------------------------------

    ;; nl_smp_match_json: text[0]=='{' (123) and text[tlen-1]=='}' (125).
    (defun nl_smp_match_json (tbytes tlen)
      (if (< tlen 2)
          0
        (if (and (= (ptr-read-u8 tbytes 0) 123)
                 (= (ptr-read-u8 tbytes (- tlen 1)) 125))
            1
          0)))

    ;; ---- IPv4: \\`[0-9]+\\.[0-9]+\\.[0-9]+\\.[0-9]+\\'  -------------------

    ;; nl_smp_match_ipv4: four digit groups separated by '.' (byte 46).
    (defun nl_smp_match_ipv4 (tbytes tlen)
      (if (< tlen 7)
          0
        (nl_smp_ipv4_group tbytes tlen 0 0)))

    (defun nl_smp_ipv4_group (tbytes tlen idx grp)
      (if (or (= idx tlen)
              (= (nl_smp_is_digit (ptr-read-u8 tbytes idx)) 0))
          0
        (nl_smp_ipv4_digits tbytes tlen idx idx grp)))

    (defun nl_smp_ipv4_digits (tbytes tlen start i grp)
      (if (= i tlen)
          (if (and (= grp 3) (> i start)) 1 0)
        (if (= (nl_smp_is_digit (ptr-read-u8 tbytes i)) 1)
            (nl_smp_ipv4_digits tbytes tlen start (+ i 1) grp)
          (if (= i start)
              0
            (if (< grp 3)
                (if (= (ptr-read-u8 tbytes i) 46)
                    (nl_smp_ipv4_group tbytes tlen (+ i 1) (+ grp 1))
                  0)
              0)))))

    ;; ---- Whitespace: all bytes ASCII whitespace  ----------------------------

    ;; nl_smp_match_whitespace: 1 iff all tbytes[i..tlen) are whitespace.
    (defun nl_smp_match_whitespace (tbytes tlen i)
      (if (= i tlen)
          1
        (if (= (nl_smp_is_ws (ptr-read-u8 tbytes i)) 1)
            (nl_smp_match_whitespace tbytes tlen (+ i 1))
          0)))

    ;; ---- NBSP: all chars U+00A0 (UTF-8: 0xC2 0xA0 = bytes 194 160)  -------

    ;; nl_smp_match_nbsp: 1 iff all tbytes[i..tlen) form C2A0 sequences.
    (defun nl_smp_match_nbsp (tbytes tlen i)
      (if (= i tlen)
          1
        (if (and (< (+ i 1) tlen)
                 (= (ptr-read-u8 tbytes i) 194)
                 (= (ptr-read-u8 tbytes (+ i 1)) 160))
            (nl_smp_match_nbsp tbytes tlen (+ i 2))
          0)))

    ;; ---- Newline/CR: text contains byte 10 or 13  --------------------------

    ;; nl_smp_match_newline: 1 iff tbytes[i..tlen) contains \n (10) or \r (13).
    (defun nl_smp_match_newline (tbytes tlen i)
      (if (= i tlen)
          0
        (if (or (= (ptr-read-u8 tbytes i) 10)
                (= (ptr-read-u8 tbytes i) 13))
            1
          (nl_smp_match_newline tbytes tlen (+ i 1)))))

    ;; ---- Fallback: inline byte comparison with escape processing ------------

    ;; nl_smp_fb_cmp: 1 iff processed(pbytes[pi..pe)) == tbytes[ti..ti+llen).
    ;; Escape rules:
    ;;   \\. (92 46): consume two pat bytes, match '.' (46) in text.
    ;;   \\\\ (92 92): consume two pat bytes, match '\\' (92) in text.
    ;;   other byte B: consume one pat byte, match B in text.
    ;; Returns 1 when both pat and text exhausted simultaneously.
    (defun nl_smp_fb_cmp (pbytes pi pe tbytes ti tlen)
      (if (= pi pe)
          ;; Pattern exhausted — success regardless of remaining text
          1
        (if (= ti tlen)
            ;; Text exhausted but pattern remains
            0
          (if (and (= (ptr-read-u8 pbytes pi) 92)
                   (< (+ pi 1) pe)
                   (or (= (ptr-read-u8 pbytes (+ pi 1)) 46)
                       (= (ptr-read-u8 pbytes (+ pi 1)) 92)))
              ;; Recognized escape \\. or \\\\: match second byte in text
              (if (= (ptr-read-u8 tbytes ti) (ptr-read-u8 pbytes (+ pi 1)))
                  (nl_smp_fb_cmp pbytes (+ pi 2) pe tbytes (+ ti 1) tlen)
                0)
            ;; Literal byte: match pat[pi] in text
            (if (= (ptr-read-u8 tbytes ti) (ptr-read-u8 pbytes pi))
                (nl_smp_fb_cmp pbytes (+ pi 1) pe tbytes (+ ti 1) tlen)
              0)))))

    ;; nl_smp_fb_lit_len: compute the processed literal length (output byte count)
    ;; for pbytes[ps..pe).  Used to compute start index for ends_with.
    (defun nl_smp_fb_lit_len (pbytes ps pe)
      (if (= ps pe)
          0
        (if (and (= (ptr-read-u8 pbytes ps) 92)
                 (< (+ ps 1) pe)
                 (or (= (ptr-read-u8 pbytes (+ ps 1)) 46)
                     (= (ptr-read-u8 pbytes (+ ps 1)) 92)))
            (+ 1 (nl_smp_fb_lit_len pbytes (+ ps 2) pe))
          (+ 1 (nl_smp_fb_lit_len pbytes (+ ps 1) pe)))))

    ;; nl_smp_fb_contains: 1 iff tbytes[0..tlen) contains processed(pbytes[ps..pe)).
    ;; Tries nl_smp_fb_cmp starting at each ti from 0..tlen.
    (defun nl_smp_fb_contains (pbytes ps pe tbytes tlen ti)
      (if (= ti tlen)
          ;; Check at end only if pattern is empty
          (if (= ps pe) 1 0)
        (if (= (nl_smp_fb_cmp pbytes ps pe tbytes ti tlen) 1)
            1
          (nl_smp_fb_contains pbytes ps pe tbytes tlen (+ ti 1)))))

    ;; nl_smp_fb_match: dispatch based on asae flag:
    ;;   3 (both anchored) → exact match (starts_with + length check)
    ;;   2 (start only)    → starts_with
    ;;   1 (end only)      → ends_with
    ;;   0 (neither)       → contains
    ;; asae = anchored_start * 2 + anchored_end.
    (defun nl_smp_fb_match (pbytes ps pe tbytes tlen asae)
      (if (= asae 3)
          ;; Exact match: text length must equal literal length
          (if (= tlen (nl_smp_fb_lit_len pbytes ps pe))
              (nl_smp_fb_cmp pbytes ps pe tbytes 0 tlen)
            0)
        (if (= asae 2)
            ;; starts_with: match from position 0
            (nl_smp_fb_cmp pbytes ps pe tbytes 0 tlen)
          (if (= asae 1)
              ;; ends_with: match from (tlen - lit_len)
              (nl_smp_fb_cmp pbytes ps pe tbytes
                             (- tlen (nl_smp_fb_lit_len pbytes ps pe))
                             tlen)
            ;; contains: try every start position
            (nl_smp_fb_contains pbytes ps pe tbytes tlen 0)))))

    ;; ---- Fallback anchor detection  -----------------------------------------

    ;; nl_smp_fallback_slice: dispatch once ps/pe are known.
    ;; asae = anchored_start * 2 + anchored_end.
    (defun nl_smp_fallback_slice (pbytes ps pe tbytes tlen asae)
      (if (>= ps pe)
          ;; Empty literal
          (if (= asae 3)
              (if (= tlen 0) 1 0)
            ;; starts_with / ends_with / contains empty literal → always true
            1)
        (nl_smp_fb_match pbytes ps pe tbytes tlen asae)))

    ;; nl_smp_fallback_anchors: given as_flag / ae_flag (0/1), compute ps/pe.
    ;; asae = as_flag*2 + ae_flag.
    (defun nl_smp_fallback_anchors (pbytes plen tbytes tlen asae)
      (nl_smp_fallback_slice
       pbytes
       ;; ps: skip \\` (bytes 92+96, 2 bytes) or ^ (94, 1 byte)
       (if (= (logand asae 2) 2)
           (if (and (>= plen 2) (= (ptr-read-u8 pbytes 0) 92))
               2
             1)
         0)
       ;; pe: strip \\' (bytes 92+39, 2 bytes) or $ (36, 1 byte)
       (if (= (logand asae 1) 1)
           (if (and (>= plen 2) (= (ptr-read-u8 pbytes (- plen 2)) 92))
               (- plen 2)
             (- plen 1))
         plen)
       tbytes tlen asae))

    ;; nl_smp_fallback: entry for the catch-all pattern.
    ;; Detects anchor bytes from pbytes[0..plen), then dispatches.
    (defun nl_smp_fallback (pbytes plen tbytes tlen)
      (nl_smp_fallback_anchors
       pbytes plen tbytes tlen
       ;; Compute asae = as*2 + ae (packed anchor flags)
       (+ (if (and (>= plen 2)
                   (= (ptr-read-u8 pbytes 0) 92)
                   (= (ptr-read-u8 pbytes 1) 96))
              2
            (if (and (>= plen 1) (= (ptr-read-u8 pbytes 0) 94))
                2
              0))
          (if (and (>= plen 2)
                   (= (ptr-read-u8 pbytes (- plen 2)) 92)
                   (= (ptr-read-u8 pbytes (- plen 1)) 39))
              1
            (if (and (>= plen 1) (= (ptr-read-u8 pbytes (- plen 1)) 36))
                1
              0)))))

    ;; ---- Pattern length dispatch  -------------------------------------------

    ;; nl_smp_dispatch: select checker based on pattern byte-length.
    ;; Returns 1 for match, 0 for no match.
    (defun nl_smp_dispatch (pbytes plen tbytes tlen)
      (if (= plen 25)
          (nl_smp_match_numeric tbytes tlen)
        (if (= plen 8)
            (nl_smp_match_json tbytes tlen)
          (if (= plen 34)
              (nl_smp_match_ipv4 tbytes tlen)
            (if (= plen 14)
                (nl_smp_match_whitespace tbytes tlen 0)
              (if (= plen 16)
                  (nl_smp_match_whitespace tbytes tlen 0)
                (if (= plen 7)
                    (nl_smp_match_nbsp tbytes tlen 0)
                  (if (= plen 4)
                      (nl_smp_match_newline tbytes tlen 0)
                    (nl_smp_fallback pbytes plen tbytes tlen)))))))))

    ;; ---- Result writer  -----------------------------------------------------

    ;; nl_smp_run: write T or Nil to *out based on match result, return 0.
    (defun nl_smp_run (pbytes plen tbytes tlen out)
      (and
       (if (= (nl_smp_dispatch pbytes plen tbytes tlen) 1)
           (sexp-write-t out)
         (sexp-write-nil out))
       0))

    ;; ---- Public trampoline  -------------------------------------------------

    ;; nl_jit_string_match_p: (*const Sexp, *const Sexp, *mut Sexp) -> i64
    ;; Returns 0 (TRAMPOLINE_OK) on success, 1 (TRAMPOLINE_ERR) on wrong type.
    (defun nl_jit_string_match_p (pat-ptr text-ptr out)
      (if (= (nl_smp_is_str_tag (sexp-tag pat-ptr)) 0)
          1
        (if (= (nl_smp_is_str_tag (sexp-tag text-ptr)) 0)
            1
          (nl_smp_run
           (str-bytes-ptr pat-ptr)
           (nl_smp_str_len pat-ptr)
           (str-bytes-ptr text-ptr)
           (nl_smp_str_len text-ptr)
           out)))))

  "AOT source for `nl_jit_string_match_p' (regex.rs migration).

Implements the literal / anchored fast-path table from
`build-tool/src/jit/regex.rs' in pure AOT elisp.  Pattern
dispatch is by unique byte-length.  Seven hard-coded patterns map to
specific checkers; all other patterns use a no-alloc inline fallback
(byte-by-byte comparison with escape processing, avoids heap
allocation and stays within the 6-GP-arg limit).

Net Rust delta: -71 LOC (`regex.rs' fully replaced) + +2 LOC
`bridge.rs' (extern decl + anchor entry) - 1 LOC `mod.rs' (`mod
regex;' removed) = -70 LOC net.")

(provide 'nelisp-cc-jit-regex)

;;; nelisp-cc-jit-regex.el ends here
