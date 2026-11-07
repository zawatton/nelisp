;;; nelisp-prelude-kernel.el --- Raw-memory helpers for the prelude  -*- lexical-binding: t; -*-

;;; Commentary:

;; Spliced in by scripts/nelisp-standalone-build.el right after the
;; prelude's function-cell bootstrap, i.e. BEFORE the prelude body.  The
;; prelude is not an unsafe-kernel file, so it reaches the stat buffer
;; and the sigaction struct only through the two helpers below.
;;
;; This file must stay prelude-independent: no `require', no `provide',
;; and only native primitives and special forms.

;;; Code:


(defun nelisp-stat-buf-fields (buf)
  "Return the integer fields `file-attributes' needs from stat buffer BUF.
The list is (MODE NLINK UID GID ASEC ANSEC MSEC MNSEC CSEC CNSEC SIZE
INODE DEVICE), read at the Linux x86-64 `struct stat' offsets."
  (list (ptr-read-u32 buf 24)
        (ptr-read-u64 buf 16)
        (ptr-read-u32 buf 28)
        (ptr-read-u32 buf 32)
        (ptr-read-u64 buf 72) (ptr-read-u64 buf 80)
        (ptr-read-u64 buf 88) (ptr-read-u64 buf 96)
        (ptr-read-u64 buf 104) (ptr-read-u64 buf 112)
        (ptr-read-u64 buf 48)
        (ptr-read-u64 buf 8)
        (ptr-read-u64 buf 0)))

(defun nelisp-sigaction-set (syscall-number signum handler)
  "Install HANDLER (0 default, 1 ignore) for SIGNUM with an empty mask.
SYSCALL-NUMBER is the platform's sigaction syscall number.  Return the
raw syscall result."
  (let ((action (alloc-bytes 32 8)))
    (ptr-write-u64 action 0 handler)
    (ptr-write-u64 action 8 0)
    (ptr-write-u64 action 16 0)
    (ptr-write-u64 action 24 0)
    (syscall-direct
     syscall-number
     signum action 0 (if (eq system-type 'darwin) 0 8) 0 0)))

;;; nelisp-prelude-kernel.el ends here
