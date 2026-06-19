;;; nelisp-cc-bi-tty-raw.el --- Wave k tty raw-mode / winsize / jobctrl AOT swap  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Wave k — moves the syscall bodies of the tty builtins from
;; `build-tool/src/eval/builtins.rs' into AOT elisp .o objects.
;;
;; The Rust shim (build-tool/src/eval/tty.rs) retains:
;;
;;   * async-signal-safe signal handlers (atexit_hook, sig_handler,
;;     tstp_handler, cont_handler, winsize handler)
;;   * Once-gated install_hooks_once / install_handler /
;;     install_handlers (use Rust fn pointers)
;;   * Static storage: TERMIOS_SAVED (AtomicI64), TTY_FD (AtomicI64),
;;     SAVED_TERMIOS (144-byte [u8;60] static), WINSIZE_CHANGED (AtomicI64),
;;     SIGCONT_ARRIVED (AtomicI64)
;;   * #[no_mangle] pointer getters so elisp .o can reach the statics
;;
;; This file exports:
;;
;;   nelisp_tty_raw_enter (statbuf: *mut u8) -> i64
;;     — tcgetattr(0,buf) + save + cfmakeraw + VMIN/VTIME + tcsetattr
;;     — args: statbuf = Rust-allocated 60-byte scratch (= zeroed termios)
;;     — returns 0 on success, -1 on tcgetattr failure, -2 on tcsetattr
;;
;;   nelisp_tty_raw_leave (statbuf: *const u8, fd: i64) -> i64
;;     — tcsetattr(fd, TCSANOW, SAVED_TERMIOS) if TERMIOS_SAVED was true
;;     — args: statbuf = nl_tty_saved_termios_ptr(), fd = nl_tty_fd_ptr val
;;     — returns 0 always (errors are suppressed for restore semantics)
;;
;;   nelisp_tty_stdin_byte_avail (timeout_ms: i64, pfd_buf: *mut u8) -> i64
;;     — poll(pfd, 1, timeout_ms) + conditional read(0, byte_slot, 1)
;;     — args: pfd_buf = 8-byte scratch for struct pollfd
;;     — returns: >= 0 = byte value read; -1 = timeout/no-data; -2 = poll err
;;
;;   nelisp_tty_winsize_current (ws_buf: *mut u8) -> i64
;;     — ioctl(0, TIOCGWINSZ, ws_buf); packs (col << 16 | row) into i64
;;     — args: ws_buf = 8-byte scratch for struct winsize (col+row = 4 bytes)
;;     — returns packed cols/rows on success, -1 on error
;;
;;   nelisp_tty_take_atomic (flag_ptr: *mut i64) -> i64
;;     — atomic-compare-exchange(flag_ptr, 1 → 0): returns old value (0 or 1)
;;     — used by take_changed / take_cont / termios_saved_p reads
;;
;; Linux x86_64 ABI constants:
;;   TCSANOW   = 0
;;   VMIN      = 6   (index in c_cc, = byte offset 17+6=23 in termios)
;;   VTIME     = 5   (index in c_cc, = byte offset 17+5=22 in termios)
;;   POLLIN    = 0x0001
;;   POLLHUP   = 0x0010
;;   TIOCGWINSZ = 0x5413
;;
;; struct termios layout (Linux x86_64, 60 bytes):
;;   0: c_iflag (u32)   4: c_oflag (u32)   8: c_cflag (u32)  12: c_lflag (u32)
;;   16: c_line (u8)   17..33: c_cc[17] (u8 each)
;;   36: c_ispeed (u32)  40: c_ospeed (u32)  [+ padding to 60]
;;   => c_cc[VMIN=6] = byte 23; c_cc[VTIME=5] = byte 22
;;
;; struct pollfd layout (Linux, 8 bytes):
;;   0: fd (i32, 4B)   4: events (i16, 2B)   6: revents (i16, 2B)
;;
;; struct winsize layout (Linux, 8 bytes):
;;   0: ws_row (u16)   2: ws_col (u16)   4: ws_xpixel   6: ws_ypixel
;;
;; Substrate ops:
;;   §100.A  extern-call          — tcgetattr / cfmakeraw / tcsetattr / poll /
;;                                  read / ioctl + nl_tty_* pointer getters
;;   §101.C  ptr-read-u8 / ptr-write-u8  — termios byte fields (VMIN, VTIME)
;;   §101.D  ptr-read-u64 / ptr-write-u64  — atomic slot reads/writes
;;   §103.A  atomic-compare-exchange  — CAS for TERMIOS_SAVED flag
;;   §125.A  alloc-bytes / dealloc-bytes  — not needed (Rust provides buffers)
;;
;; Per Doc 117 §4.3: pure migration — observable behaviour matches the
;; pre-swap Rust bodies.

;;; Code:

(defconst nelisp-cc-bi-tty-raw--source
  '(seq
    ;; ------------------------------------------------------------------
    ;; 2-arg sequencer: returns first arg, discards second (side-effect).
    ;; Used to sequence rc-capture + cleanup/flag-set.
    (defun nelisp_tty_prog2 (val _eff) val)

    ;; ------------------------------------------------------------------
    ;; nelisp_tty_take_atomic: atomic swap of a flag slot 1→0.
    ;; Used by take_changed / take_cont / raw_leave flag CAS.
    ;; flag-ptr: *mut i64 (AtomicI64 ptr from Rust)
    ;; Returns old value (0 or 1).
    (defun nelisp_tty_take_atomic (flag-ptr)
      (atomic-compare-exchange flag-ptr 1 0))

    ;; ------------------------------------------------------------------
    ;; nelisp_tty_ptr_read_i64: read an i64 from an atomic pointer.
    ;; Used for TTY_FD load.
    (defun nelisp_tty_ptr_read_i64 (ptr)
      (ptr-read-u64 ptr 0))

    ;; ------------------------------------------------------------------
    ;; Build a NUL-terminated "/dev/tty" C string.
    ;; u64 little-endian bytes: 2f 64 65 76 2f 74 74 79.
    (defun nelisp_tty_dev_tty_path ()
      (let ((buf (alloc-bytes 9 1)))
        (nelisp_tty_prog2
         buf
         (nelisp_tty_prog2
          (ptr-write-u64 buf 0 8751747723086357551)
          (ptr-write-u8 buf 8 0)))))

    ;; ------------------------------------------------------------------
    ;; nelisp_tty_raw_enter: raw-mode entry.
    ;; statbuf: *mut u8 — Rust-allocated 60-byte zeroed termios scratch.
    ;; Protocol:
    ;;   1. open("/dev/tty", O_RDWR)       failure → fall back to fd 0
    ;;   2. tcgetattr(fd, statbuf)         rc=-1 → return -1
    ;;   3. memcpy statbuf → SAVED_TERMIOS (via nl_tty_memcpy_to_saved)
    ;;   4. Store TTY_FD ← fd, TERMIOS_SAVED ← 1
    ;;   5. nl_tty_raw_install_hooks()     (Once-gated, no-op if done)
    ;;   6. cfmakeraw(statbuf)
    ;;   7. ptr-write-u8 statbuf[22] ← 0  (VTIME = 0)
    ;;   8. ptr-write-u8 statbuf[23] ← 1  (VMIN = 1)
    ;;   9. tcsetattr(fd, 0, statbuf)      rc=-1 → clear flag, return -2
    ;;      rc=0  → return 0
    ;;
    ;; After cfmakeraw the c_cc fields may have been set; we override
    ;; them explicitly (matches the Rust: term.c_cc[VMIN]=1; term.c_cc[VTIME]=0).
    (defun nelisp_tty_raw_enter (statbuf)
      (let ((path (nelisp_tty_dev_tty_path)))
        (nelisp_tty_prog2
         (nelisp_tty_raw_enter_with_open statbuf path)
         (dealloc-bytes path 9 1))))

    ;; Open the controlling terminal.  The fd 0 fallback preserves the
    ;; historical behaviour for embeddings where /dev/tty is unavailable.
    (defun nelisp_tty_raw_enter_with_open (statbuf path)
      (nelisp_tty_raw_enter_with_fd
       statbuf
       (extern-call open path 2 0)))

    (defun nelisp_tty_raw_enter_with_fd (statbuf fd)
      (if (< fd 0)
          (nelisp_tty_raw_enter_on_fd statbuf 0)
        (nelisp_tty_raw_enter_on_fd statbuf fd)))

    (defun nelisp_tty_raw_enter_on_fd (statbuf fd)
      (if (< (extern-call tcgetattr fd statbuf) 0)
          -1
        (nelisp_tty_prog2
         (nelisp_tty_raw_enter_after_get statbuf fd)
         (extern-call nl_tty_raw_install_hooks))))

    ;; 2-arg inner: save + cfmakeraw + store flags + tcsetattr.
    ;; Called only after tcgetattr succeeded.
    (defun nelisp_tty_raw_enter_after_get (statbuf fd)
      (nelisp_tty_prog2
       (nelisp_tty_raw_enter_cfmakeraw statbuf fd)
       (nelisp_tty_raw_save_and_arm statbuf fd)))

    ;; Save termios copy + arm the TERMIOS_SAVED flag + TTY_FD.
    ;; Returns 0 always (side-effect only).
    (defun nelisp_tty_raw_save_and_arm (statbuf fd)
      (nelisp_tty_prog2
       (nelisp_tty_prog2
        (extern-call nl_tty_memcpy_to_saved statbuf)
        (ptr-write-u64 (extern-call nl_tty_fd_ptr) 0 fd))
       (ptr-write-u64 (extern-call nl_tty_saved_flag_ptr) 0 1)))

    ;; cfmakeraw + VMIN=1 VTIME=0 + tcsetattr.
    ;; Returns 0 on success, -2 on tcsetattr failure.
    (defun nelisp_tty_raw_enter_cfmakeraw (statbuf fd)
      (nelisp_tty_prog2
       (extern-call cfmakeraw statbuf)
       (nelisp_tty_raw_enter_setattr statbuf fd)))

    ;; Set VMIN/VTIME bytes then call tcsetattr.
    ;; Returns 0 on success, -2 on failure.
    (defun nelisp_tty_raw_enter_setattr (statbuf fd)
      (nelisp_tty_prog2
       (nelisp_tty_prog2
        (ptr-write-u8 statbuf 22 0)
        (ptr-write-u8 statbuf 23 1))
       (nelisp_tty_raw_setattr_or_clear statbuf fd)))

    ;; tcsetattr(fd, TCSANOW=0, statbuf): on failure clear TERMIOS_SAVED and return -2.
    (defun nelisp_tty_raw_setattr_or_clear (statbuf fd)
      (if (< (extern-call tcsetattr fd 0 statbuf) 0)
          (nelisp_tty_prog2
           -2
           (ptr-write-u64 (extern-call nl_tty_saved_flag_ptr) 0 0))
        0))

    ;; ------------------------------------------------------------------
    ;; nelisp_tty_raw_leave: restore saved termios if TERMIOS_SAVED=1.
    ;; saved-buf: *const u8 — nl_tty_saved_termios_ptr() (Rust static).
    ;; Returns 0 always (restore errors are non-fatal per Rust semantics).
    (defun nelisp_tty_raw_leave (saved-buf)
      (if (= (nelisp_tty_take_atomic (extern-call nl_tty_saved_flag_ptr)) 1)
          (nelisp_tty_raw_leave_do saved-buf)
        0))

    ;; Actual tcsetattr with the saved buffer.
    (defun nelisp_tty_raw_leave_do (saved-buf)
      (nelisp_tty_raw_leave_do_fd
       saved-buf
       (nelisp_tty_ptr_read_i64 (extern-call nl_tty_fd_ptr))))

    (defun nelisp_tty_raw_leave_do_fd (saved-buf fd)
      (nelisp_tty_prog2
       0
       (nelisp_tty_prog2
        (extern-call tcsetattr fd 0 saved-buf)
        (nelisp_tty_close_private_fd fd))))

    (defun nelisp_tty_close_private_fd (fd)
      (if (> fd 2)
          (extern-call close fd)
        0))

    ;; ------------------------------------------------------------------
    ;; nelisp_tty_stdin_byte_avail: poll(pfd,1,timeout) + optional read.
    ;; pfd-buf: *mut u8 — 8-byte scratch for struct pollfd.
    ;; timeout: i64 — milliseconds (0 = non-blocking).
    ;; Returns: >= 0 byte value; -1 = no data; -2 = poll error.
    ;;
    ;; pollfd layout: fd(i32,4B) events(i16,2B) revents(i16,2B)
    ;; We write fd=TTY_FD, events=POLLIN(1) before poll.
    ;; After poll: check revents & (POLLIN|POLLHUP) = revents & 0x11
    (defun nelisp_tty_stdin_byte_avail (pfd-buf timeout)
      (nelisp_tty_stdin_poll_after_init
       pfd-buf
       timeout
       (nelisp_tty_pfd_init pfd-buf)))

    ;; Initialize pollfd: fd=TTY_FD, events=1 (POLLIN), revents=0.
    ;; struct pollfd is 8 bytes; events at offset 4 gives 1 << 32.
    (defun nelisp_tty_pfd_init (pfd-buf)
      (ptr-write-u64
       pfd-buf
       0
       (+ (nelisp_tty_ptr_read_i64 (extern-call nl_tty_fd_ptr))
          4294967296)))

    (defun nelisp_tty_stdin_poll_after_init (pfd-buf timeout _ignored)
      (nelisp_tty_stdin_poll pfd-buf timeout))

    ;; poll + check + conditional read.
    (defun nelisp_tty_stdin_poll (pfd-buf timeout)
      (nelisp_tty_poll_dispatch pfd-buf (extern-call poll pfd-buf 1 timeout)))

    ;; Dispatch on poll rc.
    (defun nelisp_tty_poll_dispatch (pfd-buf poll-rc)
      (if (< poll-rc 0)
          -2
        (if (= poll-rc 0)
            -1
          (nelisp_tty_check_revents pfd-buf))))

    ;; Check revents byte at offset 6 for POLLIN(1) | POLLHUP(0x10).
    ;; POLLIN=1, POLLHUP=16, mask=17 (0x11).
    (defun nelisp_tty_check_revents (pfd-buf)
      (if (= (logand (ptr-read-u8 pfd-buf 6) 17) 0)
          -1
        (nelisp_tty_do_read)))

    ;; read(0, byte_scratch, 1) — use pfd-buf offset 7 as 1-byte scratch.
    ;; Returns byte value (0-255), or -1 on EOF/EAGAIN.
    (defun nelisp_tty_do_read ()
      (nelisp_tty_read_dispatch (extern-call nl_tty_read_byte)))

    ;; Map read rc: >0 = byte value (returned by nl_tty_read_byte directly),
    ;; 0 = EOF → -1, <0 = error → -1.
    (defun nelisp_tty_read_dispatch (rc)
      (if (> rc 0)
          rc
        -1))

    ;; ------------------------------------------------------------------
    ;; nelisp_tty_winsize_current: ioctl(TTY_FD, TIOCGWINSZ, ws_buf).
    ;; ws-buf: *mut u8 — 8-byte scratch (struct winsize).
    ;; Returns packed i64: (col << 16) | row, or -1 on error.
    ;; winsize layout: ws_row(u16,0) ws_col(u16,2)
    ;; TIOCGWINSZ = 0x5413
    (defun nelisp_tty_winsize_current (ws-buf)
      (nelisp_tty_winsize_dispatch
       ws-buf
       (extern-call ioctl
                    (nelisp_tty_ptr_read_i64 (extern-call nl_tty_fd_ptr))
                    21523
                    ws-buf)))

    (defun nelisp_tty_winsize_dispatch (ws-buf rc)
      (if (< rc 0)
          -1
        (nelisp_tty_winsize_pack ws-buf)))

    ;; Pack ws_col and ws_row into (col << 16 | row).
    ;; ws_row at offset 0 (u16 LE), ws_col at offset 2 (u16 LE).
    (defun nelisp_tty_winsize_pack (ws-buf)
      (+ (* (+ (* (ptr-read-u8 ws-buf 3) 256)
                (ptr-read-u8 ws-buf 2))
            65536)
         (+ (* (ptr-read-u8 ws-buf 1) 256)
            (ptr-read-u8 ws-buf 0)))))
  "Wave k AOT source for tty raw-mode / winsize syscall bodies.

Exports:
- `nelisp_tty_prog2 (val _eff)' — 2-arg sequencer
- `nelisp_tty_take_atomic (flag-ptr)' — atomic swap 1→0
- `nelisp_tty_ptr_read_i64 (ptr)' — ptr-read-u64 at offset 0
- `nelisp_tty_raw_enter (statbuf)' — tcgetattr+cfmakeraw+tcsetattr
- helper cluster for enter: _after_get / _save_and_arm / _cfmakeraw /
  _setattr / _setattr_or_clear (6 helpers)
- `nelisp_tty_raw_leave (saved-buf)' — CAS flag + tcsetattr
- `nelisp_tty_raw_leave_do (saved-buf)' — inner tcsetattr
- `nelisp_tty_stdin_byte_avail (pfd-buf timeout)' — poll+read
- helper cluster for avail: _pfd_init / _poll / _poll_dispatch /
  _check_revents / _do_read / _read_dispatch (6 helpers)
- `nelisp_tty_winsize_current (ws-buf)' — ioctl TIOCGWINSZ
- `nelisp_tty_winsize_dispatch (ws-buf rc)' — ioctl rc branch
- `nelisp_tty_winsize_pack (ws-buf)' — pack col<<16|row

Rust shim provides via #[no_mangle] getters:
  nl_tty_saved_flag_ptr() -> *mut i64   (TERMIOS_SAVED AtomicI64)
  nl_tty_fd_ptr()         -> *mut i64   (TTY_FD AtomicI64)
  nl_tty_saved_termios_ptr() -> *mut u8 (SAVED_TERMIOS static buf)
  nl_tty_raw_install_hooks()             (Once-gated atexit+sigaction)
  nl_tty_memcpy_to_saved(*const u8)     (copies 60 bytes to SAVED_TERMIOS)
  nl_tty_read_byte() -> i64             (read(0,buf,1) → byte value or <=0)

Linux-x86_64 only — same arch gate as §122.I substrate.")

(provide 'nelisp-cc-bi-tty-raw)

;;; nelisp-cc-bi-tty-raw.el ends here
