;;; nelisp-cc-bi-syscall-stat-mtime.el --- Wave A25.1 stat() mtime extract  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Wave A25.1 (Phase 47 self-application foundation) — pure-elisp
;; `(nelisp_bi_syscall_stat_mtime PATH)' helper.  Composes:
;;
;;   §122.I  `nelisp_cstr_from_sexp'  — build path CString.
;;   §125.A  `alloc-bytes' / `dealloc-bytes' — stat buffer + path CString.
;;   §100.A  `extern-call'            — libc `stat(2)' syscall.
;;   §122.E  `ptr-read-u64'           — read mtime.tv_sec from stat-buf.
;;   §100    `sexp-int-make'          — write Sexp::Int(n) to result slot.
;;
;; struct stat layout (glibc x86_64 Linux):
;;   offset  0  dev_t       st_dev
;;   offset  8  ino_t       st_ino
;;   offset 16  nlink_t     st_nlink
;;   offset 24  mode_t      st_mode + uid_t  st_uid
;;   offset 32  gid_t       st_gid + pad
;;   offset 40  dev_t       st_rdev
;;   offset 48  off_t       st_size
;;   offset 56  blksize_t   st_blksize
;;   offset 64  blkcnt_t    st_blocks
;;   offset 72  timespec    st_atim       (sec)
;;   offset 80              st_atim.nsec
;;   offset 88  timespec    st_mtim       (sec)   <-- TARGET
;;   offset 96              st_mtim.nsec
;;   offset 104 timespec    st_ctim       (sec)
;;   offset 112             st_ctim.nsec
;;   ...
;;   sizeof(struct stat) = 144 (Linux x86_64).
;;
;; Function contract:
;;   path-ptr:    *const Sexp — caller-validated `Sexp::Str' pointing at
;;                a normalised path (Rust shim folds default-directory).
;;   result-slot: *mut Sexp  — caller-owned 32-byte slot, receives
;;                Sexp::Int(mtime_sec) on stat success, Sexp::Int(-1) on
;;                stat error (= file not found, EACCES, etc.).
;;   returns:     result-slot pointer in rax.
;;
;; Linux-x86_64 only — same arch gate as the §122.I parent + the existing
;; `nelisp_bi_syscall_stat' helper.  Composes only existing Phase 47
;; grammar — no new opcode.

;;; Code:

(defconst nelisp-cc-bi-syscall-stat-mtime--source
  '(seq
    ;; Side-effect sequencer — 4-arg `(val _e1 _e2 _e3) -> val'.
    ;; Caches the mtime value while sequencing the two dealloc-bytes
    ;; cleanups (path CString + stat buffer) and a final pad.  Even
    ;; arity (4) — no Doc 124.F alignment workaround needed.
    (defun nelisp_bi_syscall_stat_mtime_seq4 (val _e1 _e2 _e3) val)

    ;; 5-arg inner driver — receives the caller-owned result-slot,
    ;; the freshly-allocated path CString + size, and the freshly-
    ;; allocated 144-byte stat buffer.  Calls libc `stat(cstr, statbuf)',
    ;; reads mtime.tv_sec at offset 88 (= st_mtim.tv_sec for Linux
    ;; glibc x86_64), writes Sexp::Int into the result-slot, frees
    ;; both buffers, returns the slot pointer.
    ;;
    ;; Args (all i64 / pointer, 5 args = SysV uses rdi/rsi/rdx/rcx/r8;
    ;; odd arity → `--needs-align' branch adds rsp pad around the
    ;; outer call into this helper, same pattern as cstr-helpers).
    (defun nelisp_bi_syscall_stat_mtime_inner (cstr statbuf size-plus-one result-slot _pad)
      (nelisp_bi_syscall_stat_mtime_seq4
       (sexp-int-make result-slot
                      ;; Wave A29 — success-sentinel check (`= rc 0')
                      ;; instead of `(< rc 0)'.  libc `stat' returns
                      ;; `int' (= 32-bit), so the SysV AMD64 return
                      ;; only populates eax; the high 32 bits of rax
                      ;; are undefined.  On glibc-x86_64 the failure
                      ;; path emits `mov eax, -1' which zeroes the
                      ;; high 32 bits, leaving rax = 0x00000000FFFFFFFF
                      ;; (= positive 4294967295 under signed `<' — so
                      ;; the original `(< rc 0)' branch never fired
                      ;; for stat-failure).  On success the failure
                      ;; path's `xor eax, eax' zeroes rax cleanly, so
                      ;; `(= rc 0)' is the reliable disambiguator.
                      (if (= (extern-call stat cstr statbuf) 0)
                          (ptr-read-u64 statbuf 88)
                        -1))
       (dealloc-bytes cstr size-plus-one 1)
       (dealloc-bytes statbuf 144 8)
       0))

    ;; Public 2-arg entry — builds the path CString + size, allocates
    ;; the 144-byte stat buffer (align 8 to match glibc layout), and
    ;; dispatches to the 5-arg inner driver.
    (defun nelisp_bi_syscall_stat_mtime (path-ptr result-slot)
      (nelisp_bi_syscall_stat_mtime_inner
       (extern-call nelisp_cstr_from_sexp path-ptr)
       (alloc-bytes 144 8)
       (+ (str-len path-ptr) 1)
       result-slot
       0)))
  "Phase 47 source for the Wave A25.1
`(nelisp_bi_syscall_stat_mtime PATH RESULT-SLOT)' helper.

Three-entry `(seq DEFUN ...)' manifest:
- `nelisp_bi_syscall_stat_mtime_seq4 (val _e1 _e2 _e3) -> val' —
  4-arg side-effect sequencer (= result + 2 dealloc effects + pad).
- `nelisp_bi_syscall_stat_mtime_inner (cstr statbuf size result-slot
  _pad) -> result-slot' — 5-arg syscall + extract + cleanup driver.
- `nelisp_bi_syscall_stat_mtime (path-ptr result-slot) -> result-slot' —
  public 2-arg entry; allocates path CString + stat buffer, dispatches
  to inner driver.

Composes only existing Phase 47 grammar — no new opcode:
- §122.I `nelisp_cstr_from_sexp' — path CString construction (cross-
  `.o' `extern-call').
- §125.A `alloc-bytes' / `dealloc-bytes' — stat buffer + path CString
  lifecycle.
- §100.A `extern-call' to libc `stat' — syscall edge.
- §122.E `ptr-read-u64' — read mtime.tv_sec at struct stat offset 88.
- §100 `sexp-int-make' — write Sexp::Int into result slot.

stat(2) error handling: any non-success rc (= file not found,
EACCES, etc.) writes Sexp::Int(-1) into the result slot.  Caller is
responsible for distinguishing -1 from a (highly unlikely) timestamp
= -1 sentinel.

Wave A29 — success-sentinel disambiguation.  libc `stat' returns
`int' (= 32-bit), so the SysV AMD64 return only populates eax with
the high 32 bits of rax left undefined.  On glibc-x86_64 the
failure path emits `mov eax, -1' which clears high32 and leaves
rax = `0x00000000FFFFFFFF' (= positive 4294967295 under signed
i64), so the pre-A29 `(< rc 0)' branch silently fell through to
read garbage from the uninitialised stat buffer.  The kernel now
uses `(= rc 0)' (= success sentinel) so the disambiguation does
not depend on high-bit sign-extension behaviour.

Linux-x86_64 only — `struct stat' layout (= offset 88 for st_mtim.
tv_sec, sizeof = 144) is glibc-x86_64-specific.  aarch64 + musl
will land with the rest of the Phase 47 aarch64 sweep.")

(provide 'nelisp-cc-bi-syscall-stat-mtime)

;;; nelisp-cc-bi-syscall-stat-mtime.el ends here
