;;; nelisp-cc-bi-nl-write-file.el --- Doc 117 §117.D.gaps.3 open+write+close sweep  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 117 §117.D.gaps.3 — moves the syscall body of `(nl-write-file
;; PATH CONTENT)' (= `build-tool/src/eval/builtins.rs::bi_nl_write_file')
;; into a AOT elisp object.  Third handler in the Doc 122 §122.I
;; CString helper sweep (after `nelisp_bi_syscall_stat' and
;; `nelisp_bi_syscall_canonicalize').
;;
;; The Rust shim keeps:
;;
;;   * arity validation             (2 args)
;;   * `stringp' dispatch           (both args)
;;   * Error-message construction   (when the kernel returns < 0)
;;
;; The elisp body's job is the *three-syscall chain*:
;;   1. open(path, O_WRONLY | O_CREAT | O_TRUNC, 0644)
;;   2. write(fd, content_ptr, content_len)   ; only if open succeeded
;;   3. close(fd)                              ; only if open succeeded
;;   + CString lifecycle (build via §122.I, drop via §125.A
;;     `dealloc-bytes' after all syscalls complete).
;;
;; Linux x86_64 ABI constants (matching `<bits/fcntl-linux.h>`):
;;   O_WRONLY = 1
;;   O_CREAT  = 64    (0o100)
;;   O_TRUNC  = 512   (0o1000)
;;   = bitwise-or  577 (= 0o1101)
;;
;;   Mode bits: 0o644 = 420 (= rw-r--r--).
;;
;; Substrate composition:
;;
;;   §122.I  `nelisp_cstr_from_sexp'  — build path CString.
;;   §122.H  `str-bytes-ptr'          — CONTENT byte data pointer.
;;   §101.C  `str-len'                — byte counts (CONTENT + path).
;;   §125.A  `alloc-bytes' / `dealloc-bytes' — decoded byte buffer + path
;;                                     CString free.
;;   §125.A  `ptr-read-u8' / `ptr-write-u8' — Latin-1 decode loop.
;;   §100.A  `extern-call'            — libc `open' / `write' / `close'.
;;
;; Function contract:
;;   path-ptr:    *const Sexp — caller-validated `Sexp::Str' / MutStr,
;;                normalised path (`stringp' check stays Rust-side).
;;   content-ptr: *const Sexp — caller-validated `Sexp::Str' /
;;                MutStr; byte payload to write to the file.
;;   returns:     i64        —
;;                 >= 0 (= bytes written by `write(2)') on success.
;;                 <  0 (= negative open(2) rc, or write(2) rc if open
;;                       succeeded then write failed) on failure.
;;                The Rust shim maps any < 0 return to a generic
;;                `EvalError::Internal' (matching the pre-swap
;;                `map_err' branch).
;;
;; -------------------------------------------------------------------
;; Wave 15 (2026-05-23) — UTF-8 → raw byte decode at write time.
;; -------------------------------------------------------------------
;;
;; Background.  In NeLisp standalone every string lives in a Rust
;; `String', which is UTF-8 internally.  `(unibyte-string N)' for
;; `N' in `0..255' yields a Rust char `U+00N' whose UTF-8 encoding
;; for `N >= 0x80' is the two-byte sequence `c2/c3 NN-mod' (= Latin-1
;; supplement block).  Binary emitters (`nelisp-elf-write',
;; `nelisp-pe-write', `nelisp-mach-o-write') compose payloads as
;; long concatenations of `(unibyte-string ...)' chunks, so the
;; resulting `String' carries one or two `c2'/`c3' prefix bytes
;; before every byte `>= 0x80'.
;;
;; The pre-Wave-15 body passed `(str-bytes-ptr content-ptr)' +
;; `(str-len content-ptr)' straight to `write(2)', so the prefix
;; bytes leaked into the on-disk file (= `file(1)' reported
;; "corrupted section header size" for every emitted `.o' /
;; executable).
;;
;; Wave 15 fix.  Before calling `write(2)' we allocate a raw
;; `*mut u8' destination buffer (= via §125.A `alloc-bytes', which
;; bypasses Rust's UTF-8 string layer) and walk the source bytes
;; via `ptr-read-u8' (= zero-extend `u8' load from `String::as_ptr')
;; / `ptr-write-u8' (= raw `u8' store).  Each source byte is
;; classified:
;;
;;   src < 0xc2          : pass through unchanged
;;                         (= ASCII 0x00..0x7f, plus stray
;;                         continuation bytes 0x80..0xbf and the
;;                         invalid leading bytes 0xc0/0xc1 — these
;;                         should not occur in valid `Rust::String'
;;                         but a pass-through is conservative).
;;   src == 0xc2         : skip prefix, output src[idx+1] verbatim
;;                         (= 0x80..0xbf decoded to itself, covering
;;                         U+0080..U+00BF).
;;   src == 0xc3         : skip prefix, output src[idx+1] + 0x40
;;                         (= 0x80..0xbf shifted up to 0xc0..0xff,
;;                         covering U+00C0..U+00FF).
;;   src >= 0xe0         : pass through unchanged (= 3+ byte UTF-8;
;;                         only Latin-1 characters can survive the
;;                         `(unibyte-string)' construction path so
;;                         these should not appear, but pass-through
;;                         is again conservative).
;;
;; Worst-case destination size = `src-len' (= no decode shrinks the
;; output past the source byte count; only Latin-1 prefix collapse
;; reduces it).  We allocate `src-len' bytes upfront and the
;; decoder returns the actual written length for `write(2)' and the
;; trailing `dealloc-bytes' call.
;;
;; Scope.  The decode is safe for the existing single caller of
;; `nl-write-file' (= the Wave 13 `write-region' stub in
;; `nelisp-stdlib-misc.el', which writes binary blobs assembled
;; from `unibyte-string' chunks).  No production caller writes
;; multi-byte UTF-8 text via `nl-write-file' today.  If a future
;; caller needs raw UTF-8 passthrough an explicit second entry can
;; be added; for now `nl-write-file' is the binary-emit fast path.
;;
;; Verification.  `file(1) <path>' on the resulting `.o' must not
;; print "corrupted section header size"; `cmp -l' against the
;; host-Emacs-built `.o' must report no byte differences.
;;
;; Per Doc 117 §4.3: pure migration — observable behaviour matches
;; the pre-swap `std::fs::write(&path, content)' (= same flag set,
;; same mode, same `tee'-style semantic of "create or truncate +
;; write all") for ASCII inputs, and now correctly delivers the
;; intended Latin-1 byte sequence for binary payloads built via
;; `unibyte-string'.  Edge cases that differ slightly:
;;   * No retry on EINTR (Rust libstd retries; this kernel doesn't).
;;     The bi_* layer in elisp is the simplest possible composition;
;;     callers that need EINTR-safe writes should use a higher-level
;;     wrapper.  Today no NeLisp caller of `nl-write-file' does long
;;     enough writes to be EINTR-prone in practice.
;;   * No `fsync(2)' (Rust libstd doesn't either).

;;; Code:

(defconst nelisp-cc-bi-nl-write-file--source
  '(seq
    ;; Side-effect sequencer — 2-arg `(val _eff) -> val'.  Used to
    ;; cache a syscall rc while threading a cleanup side-effect.
    (defun nelisp_bi_nl_write_file_prog2 (val _eff) val)

    ;; 3-arg side-effect sequencer — `(val _e1 _e2) -> val'.  Used
    ;; when the negative-fd branch needs to free both the path
    ;; CString and the freshly-allocated raw destination buffer.
    (defun nelisp_bi_nl_write_file_prog3 (val _e1 _e2) val)

    ;; 5-arg sequencer — `(val _e1 _e2 _e3 _e4) -> val'.  Caches the
    ;; `write(2)' rc while sequencing `close(2)' + path
    ;; `dealloc-bytes' + dst `dealloc-bytes' (= the Wave 15
    ;; addition; previously only the path CString needed
    ;; freeing).  The trailing `_e4' slot keeps the arity even for
    ;; the Doc 124.F call-alignment invariant.
    (defun nelisp_bi_nl_write_file_seq5 (val _e1 _e2 _e3 _e4) val)

    ;; ---- Wave 15 UTF-8 → Latin-1 byte decoder --------------------
    ;;
    ;; Recursively walk source bytes at `src-ptr[src-idx..src-len]'
    ;; and emit decoded bytes into `dst-ptr[dst-idx..]'.  Returns
    ;; the final `dst-idx' (= total decoded byte count) which the
    ;; caller passes as `count' to `write(2)'.
    ;;
    ;; Classification (see commentary block above for rationale):
    ;;   b < 0xc2  pass through verbatim, advance 1/1.
    ;;   b == 0xc2 skip prefix, write src[idx+1] verbatim, advance 2/1.
    ;;   b == 0xc3 skip prefix, write src[idx+1]+0x40,    advance 2/1.
    ;;   b >= 0xc4 pass through verbatim, advance 1/1.
    ;;
    ;; Uses `and' for sequencing so the recursive call's return
    ;; value (= the final dst-len) bubbles back through every
    ;; frame unchanged.  `ptr-write-u8' returns the rax = 1
    ;; sentinel so the `and' chain never short-circuits on a
    ;; successful store.
    (defun nelisp_bi_nl_write_file_decode (src-ptr src-len dst-ptr src-idx dst-idx)
      (if (= src-idx src-len)
          dst-idx
        (let ((b (ptr-read-u8 src-ptr src-idx)))
          (if (< b 194)
              ;; ASCII (< 0x80) or stray continuation byte (0x80..0xbf)
              ;; or invalid lead byte 0xc0/0xc1 — pass through.
              (and
               (ptr-write-u8 dst-ptr dst-idx b)
               (nelisp_bi_nl_write_file_decode
                src-ptr src-len dst-ptr (+ src-idx 1) (+ dst-idx 1)))
            (if (= b 194)
                ;; 0xc2 prefix → next byte (0x80..0xbf) is the
                ;; decoded byte verbatim (= U+0080..U+00BF).
                (and
                 (ptr-write-u8 dst-ptr dst-idx
                               (ptr-read-u8 src-ptr (+ src-idx 1)))
                 (nelisp_bi_nl_write_file_decode
                  src-ptr src-len dst-ptr (+ src-idx 2) (+ dst-idx 1)))
              (if (= b 195)
                  ;; 0xc3 prefix → next byte + 0x40 yields the
                  ;; decoded byte (= U+00C0..U+00FF shifted up
                  ;; into 0xc0..0xff).
                  (and
                   (ptr-write-u8 dst-ptr dst-idx
                                 (+ (ptr-read-u8 src-ptr (+ src-idx 1)) 64))
                   (nelisp_bi_nl_write_file_decode
                    src-ptr src-len dst-ptr (+ src-idx 2) (+ dst-idx 1)))
                ;; 0xc4..0xff: 3+ byte UTF-8 lead.  Should not
                ;; appear under the `unibyte-string'-only contract
                ;; (= Latin-1 inputs cap at U+00FF = 2-byte UTF-8)
                ;; but pass-through is conservative.
                (and
                 (ptr-write-u8 dst-ptr dst-idx b)
                 (nelisp_bi_nl_write_file_decode
                  src-ptr src-len dst-ptr (+ src-idx 1) (+ dst-idx 1)))))))))

    ;; 6-arg with-fd dispatcher — branches on the open(2) rc.
    ;; Args (all i64, capped at SysV AMD64's 6 GP arg regs per
    ;; AOT `--arg-regs' limit; pads from the pre-Wave-15 body
    ;; have been dropped to stay within 6 regs):
    ;;   fd:        open(2) rc.  Negative = error; non-negative = file
    ;;              descriptor.
    ;;   cstr:      path CString (= return of §122.I cstr_from_sexp).
    ;;   size:      path CString allocation size (= str-len + 1).
    ;;   dst-ptr:   decoded byte buffer (= `alloc-bytes' result, raw
    ;;              `*mut u8' bypass of the Rust UTF-8 string layer).
    ;;   dst-cap:   destination buffer allocation size (= upper bound
    ;;              on the decoded length, = source byte count).
    ;;              Used for the trailing `dealloc-bytes' (= must
    ;;              match the `alloc-bytes' size argument exactly per
    ;;              §125.A contract).
    ;;   dst-len:   decoded byte count (= return value of
    ;;              `nelisp_bi_nl_write_file_decode'; what `write(2)'
    ;;              should consume).
    (defun nelisp_bi_nl_write_file_with_fd
        (fd cstr size dst-ptr dst-cap dst-len)
      (if (< fd 0)
          ;; Open failed.  Free both the path CString and the
          ;; pre-allocated decoded-byte buffer, then return the
          ;; negative rc so the Rust shim raises EvalError::Internal.
          (nelisp_bi_nl_write_file_prog3
           fd
           (dealloc-bytes cstr size 1)
           (dealloc-bytes dst-ptr dst-cap 1))
        ;; Open succeeded.  Issue write(2), close(2), dealloc both
        ;; buffers in sequence; return write_rc (= bytes written, or
        ;; -1 on err).  SysV ABI evaluates args left-to-right so
        ;; `write' runs before `close' runs before the two
        ;; `dealloc-bytes' frees.
        (nelisp_bi_nl_write_file_seq5
         (extern-call write fd dst-ptr dst-len)
         (extern-call close fd)
         (dealloc-bytes cstr size 1)
         (dealloc-bytes dst-ptr dst-cap 1)
         0)))

    ;; 6-arg inner driver — receives the freshly-allocated path
    ;; CString + size + raw destination buffer + its capacity + its
    ;; populated length, calls open(2), then dispatches to the
    ;; 6-arg `_with_fd' branch.
    ;;
    ;; Pulled out from the public entry so the public entry's body
    ;; is a flat composition of substrate ops (= no nested
    ;; `extern-call' inside an `extern-call' arg) — keeps the
    ;; per-arg eval order obvious for ABI auditors.
    (defun nelisp_bi_nl_write_file_inner
        (cstr size dst-ptr dst-cap dst-len _pad)
      (nelisp_bi_nl_write_file_with_fd
       (extern-call open cstr 577 420)
       cstr
       size
       dst-ptr
       dst-cap
       dst-len))

    ;; 5-arg setup driver — given the freshly-allocated path CString
    ;; + path-cstr-size + raw `dst-ptr' + source byte pointer +
    ;; source byte count, runs the decode walker (= writes into
    ;; `dst-ptr', returns the populated dst-len) and chains into the
    ;; 6-arg inner driver above.
    (defun nelisp_bi_nl_write_file_setup
        (cstr size dst-ptr src-ptr src-len)
      (nelisp_bi_nl_write_file_inner
       cstr
       size
       dst-ptr
       src-len  ; dst-cap = src-len (upper bound on decoded length)
       (nelisp_bi_nl_write_file_decode
        src-ptr src-len dst-ptr 0 0)
       0))

    ;; Public 2-arg entry — builds the path CString, allocates the
    ;; raw destination buffer (= worst-case = source byte count),
    ;; and dispatches to the 5-arg setup driver which runs the
    ;; UTF-8 → Latin-1 decoder and chains into open/write/close.
    (defun nelisp_bi_nl_write_file (path-ptr content-ptr)
      (nelisp_bi_nl_write_file_setup
       (extern-call nelisp_cstr_from_sexp path-ptr)
       (+ (str-len path-ptr) 1)
       (alloc-bytes (str-len content-ptr) 1)
       (str-bytes-ptr content-ptr)
       (str-len content-ptr))))
  "AOT source for the Doc 117 §117.D.gaps.3 `(nl-write-file
PATH CONTENT)' libc syscall body swap, with Wave 15 (2026-05-23)
UTF-8 → Latin-1 byte decoder threaded in front of `write(2)'.

Eight-entry `(seq DEFUN ...)' manifest:
- `nelisp_bi_nl_write_file_prog2 (val _eff) -> val' — 2-arg cache.
- `nelisp_bi_nl_write_file_prog3 (val _e1 _e2) -> val' — 3-arg
  cache used by the open-failed branch (= must free both the
  path CString and the pre-allocated decoded buffer).
- `nelisp_bi_nl_write_file_seq5 (val _e1 _e2 _e3 _e4) -> val' —
  5-arg cache with 4-effect tail (write rc + close eff + path
  dealloc eff + dst dealloc eff + pad).
- `nelisp_bi_nl_write_file_decode (sptr slen dptr sidx didx) ->
  final-dlen' — recursive Latin-1 decoder; rewrites two-byte
  UTF-8 sequences `c2/c3 NN' back to the raw byte they encode,
  passes everything else through verbatim.
- `nelisp_bi_nl_write_file_with_fd (fd cstr size dptr dcap
  dlen)' — 6-arg branch on open(2) rc (= capped at SysV AMD64's
  6 GP arg regs).
- `nelisp_bi_nl_write_file_inner (cstr size dptr dcap dlen _pad)' —
  6-arg driver that calls open(2) + dispatches.
- `nelisp_bi_nl_write_file_setup (cstr size dptr sptr slen)' —
  4-arg driver that runs the decoder, then chains into inner.
- `nelisp_bi_nl_write_file (path-ptr content-ptr)' — 2-arg public
  entry; builds path CString + alloc dst buffer + dispatch.

Composes existing AOT grammar (no new opcode):
- §122.I `nelisp_cstr_from_sexp' — path CString construction (cross-
  `.o' `extern-call').
- §125.A `alloc-bytes' / `dealloc-bytes' — raw `*mut u8' buffer for
  the decoded payload (= bypasses Rust UTF-8 string layer).
- §125.A `ptr-read-u8' / `ptr-write-u8' — byte-level source / dst
  raw memory ops for the decode loop.
- §122.H `str-bytes-ptr' — CONTENT UTF-8 byte data pointer.
- §101.C `str-len' — byte counts.
- §100.A `extern-call' to libc `open' / `write' / `close'.

Linux-x86_64 only — same arch gate as the §122.I substrate.")

(provide 'nelisp-cc-bi-nl-write-file)

;;; nelisp-cc-bi-nl-write-file.el ends here
