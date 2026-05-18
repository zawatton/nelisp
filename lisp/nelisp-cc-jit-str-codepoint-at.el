;;; nelisp-cc-jit-str-codepoint-at.el --- Doc 122 §122.D str-codepoint-at trampoline  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 122 §122.D / Doc 120 §120.B — Phase-47-compiled replacement for
;; the Rust `nl_jit_str_codepoint_at' trampoline in
;; `build-tool/src/jit/box_accessor.rs'.  Same
;; `(*const Sexp, i64, *mut Sexp) -> i64' contract:
;;
;;   1. idx < 0 → TRAMPOLINE_ERR (= 1).
;;   2. Non-Str / non-MutStr tag → ERR.
;;   3. Char-indexed walk: starting from byte 0, decode codepoints
;;      via `str-codepoint-at' (= byte-indexed `nl_str_codepoint_at'
;;      extern) advancing by each codepoint's UTF-8 byte width until
;;      the target character index is reached.
;;   4. OOB (idx ≥ char count) or malformed UTF-8 → ERR.
;;   5. Success: write `Sexp::Int(codepoint)' to `*out' → OK (= 0).
;;
;; The Rust body used `s.chars().nth(idx as usize)' which is O(n)
;; char iteration.  The elisp body replicates this semantics via a
;; tail-recursive helper `nl_jit_str_codepoint_at_walk'.
;;
;; Scratch i64 slots:
;;   `str-codepoint-at' (grammar op) writes to two `*mut i64' out-slots
;;   for the decoded codepoint and UTF-8 byte width.  The caller-owned
;;   `*mut Sexp out' (= 32-byte slot per `nelisp-sexp--slot-size = 32')
;;   has room at offsets +8 and +16 beyond the tag byte.  These are
;;   used as the scratch i64 pointers during the walk, then overwritten
;;   with a clean `Sexp::Int' via `sexp-int-make' on success:
;;
;;     scratch-cp    = out + 8   (*mut i64 for decoded codepoint)
;;     scratch-width = out + 16  (*mut i64 for UTF-8 byte width)
;;
;; Grammar ops used:
;;   `sexp-tag'         — read tag byte of arg Sexp.
;;   `str-codepoint-at' — decode codepoint at byte-idx; write cp + width
;;                        to scratch i64 slots; return 1 on success / 0 fail.
;;   `ptr-read-u64'     — load i64 from scratch slot to recover width.
;;   `sexp-int-make'    — write `Sexp::Int(cp)' to the out slot.
;;
;; Tag-byte constants: 5 = Sexp::Str, 6 = Sexp::MutStr (sexp-layout.el).
;;
;; Linker wiring: `bridge.rs' declares `fn nl_jit_str_codepoint_at()' in
;; the `extern "C"' block and adds it to `_ELISP_ARCHIVE_ANCHOR' so the
;; linker pulls this .o into the binary and `dlsym(RTLD_DEFAULT, ...)'
;; resolves the symbol for `nl-jit-call-out-1i' dispatch from
;; `nelisp-jit-strategy.el'.  The private walk helper
;; `nl_jit_str_codepoint_at_walk' lives in the same .o and is resolved
;; via PLT cross-reference without a separate anchor entry.

;;; Code:

(defconst nelisp-cc-jit-str-codepoint-at--source
  '(seq
    ;; Walk helper: tail-recurse from byte-idx, counting down chars-remaining.
    ;; Scratch cp and width i64 slots live at out+8 and out+16 respectively
    ;; (within the 32-byte *mut Sexp out slot).  On each step we:
    ;;   1. Decode the codepoint at byte-idx via str-codepoint-at.
    ;;   2. If remaining = 0, we are at the target: write Sexp::Int(cp) + OK.
    ;;   3. Else read width, advance byte-idx, recurse with remaining-1.
    ;; Failure of str-codepoint-at (invalid byte-idx / malformed UTF-8 /
    ;; past end) propagates as ERR = 1.
    (defun nl_jit_str_codepoint_at_walk (arg byte-idx remaining out)
      (if (= (str-codepoint-at arg byte-idx (+ out 8) (+ out 16)) 1)
          ;; Decode succeeded.
          (if (= remaining 0)
              ;; At target char: cp was written to out+8.  Finalize out
              ;; as Sexp::Int and return OK.
              (and (sexp-int-make out (ptr-read-u64 (+ out 8) 0)) 0)
            ;; Not at target yet: advance byte-idx by codepoint width.
            (nl_jit_str_codepoint_at_walk
             arg
             (+ byte-idx (ptr-read-u64 (+ out 16) 0))
             (- remaining 1)
             out))
        ;; str-codepoint-at failed: invalid index or end of string → ERR.
        1))

    ;; Public trampoline entry — mirrors Rust `nl_jit_str_codepoint_at'.
    ;; Guards: idx >= 0, Str or MutStr tag.  Then delegates to the walk
    ;; helper starting from byte 0.
    (defun nl_jit_str_codepoint_at (arg idx out)
      ;; arg: *const Sexp.  idx: i64 (char index).  out: *mut Sexp.
      ;; Returns: i64 = 0 on OK, 1 on ERR.
      (if (< idx 0)
          1
        (if (or (= (sexp-tag arg) 5) (= (sexp-tag arg) 6))
            (nl_jit_str_codepoint_at_walk arg 0 idx out)
          1))))
  "Phase 47 source for the §120.B `nl_jit_str_codepoint_at' swap.

Two-entry `(seq DEFUN ...)' manifest:

- `nl_jit_str_codepoint_at_walk (arg byte-idx remaining out) -> i64'
  — tail-recursive char walker.  Uses `str-codepoint-at' (byte-indexed
  §122.D grammar op) to decode each codepoint in turn, advancing the
  byte index by the codepoint's UTF-8 byte width until `remaining'
  reaches 0.  At that point reads the decoded codepoint from the scratch
  i64 slot at `out + 8' and finalises `out' as `Sexp::Int(cp)' via
  `sexp-int-make'.  `str-codepoint-at' returning 0 at any step signals
  an invalid byte boundary or end-of-string → propagate ERR = 1.

- `nl_jit_str_codepoint_at (arg idx out) -> i64'
  — public entry.  Guards: idx >= 0; sexp-tag = Str (5) or MutStr (6).
  Delegates to the walk helper with byte-idx = 0, remaining = idx.

Scratch slot layout inside the 32-byte `*mut Sexp out':
  offset 0 : tag byte (will be overwritten by sexp-int-make on success)
  offset 8 : scratch i64 cp-slot for str-codepoint-at walk
  offset 16: scratch i64 width-slot for byte-advance computation

The `(and SIDE-EFFECT 0)' idiom at the terminal OK arm matches the
§120.A / §120.B conventions established by other box_accessor swaps.

Linker: `bridge.rs::_ELISP_ARCHIVE_ANCHOR' includes the public entry
symbol `nl_jit_str_codepoint_at' so `dlsym(RTLD_DEFAULT, ...)' resolves
at runtime for `nl-jit-call-out-1i' dispatch.  The private walk helper
`nl_jit_str_codepoint_at_walk' is in the same .o and linked via PLT
cross-reference from the public entry.")

(provide 'nelisp-cc-jit-str-codepoint-at)

;;; nelisp-cc-jit-str-codepoint-at.el ends here
