;;; nelisp-cc-libc-constants.el --- Doc 122 §122.K libc constants table  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 122 §122.K — hand-curated libc constants table for the
;; Phase 47 elisp side of the substrate.  Doc 117 §117.D.gaps.3
;; left 17 syscall handlers (`sigaction' / `ioctl(TIOCGWINSZ)' /
;; `tcsetattr' / `poll' / `fcntl' …) deferred because their Rust
;; sources still hard-code `libc::SIGINT' / `libc::TIOCGWINSZ' /
;; `libc::SA_RESTART' integer values.  Folding those handlers to
;; pure elisp (= Doc 122.J extern-call grammar) needs the same
;; integer values available as `defconst' on the elisp side.
;;
;; Strategy (Doc 122 §122.K Surface, Strategy A — hand-curated):
;;
;;   * Embed the linux-glibc-x86_64 numeric values directly as
;;     `defconst' here.  All constants are byte-identical to
;;     the values returned by a `cc -E -dM' probe over
;;     <signal.h> / <sys/ioctl.h> / <termios.h> / <poll.h> /
;;     <fcntl.h> on the host glibc.
;;
;;   * `scripts/verify-libc-constants.sh' is the CI / drift
;;     gate — it compiles a tiny C probe that prints each
;;     constant and diffs the output against the table below.
;;     Run from `make verify-libc-constants' or by hand.
;;
;;   * Per-platform value divergence (musl / android / *BSD /
;;     macOS) is recorded inline next to each constant when
;;     the linux-glibc value differs.  Phase 47 currently only
;;     targets linux-glibc-x86_64 so the defconst body picks
;;     the linux-glibc value; cross-target builds gain a
;;     compile-time `cond' switch in a later phase.
;;
;; Naming convention: `nelisp-cc--SIGINT' (= double-dash `--'
;; private prefix per nelisp-cc-* style — these constants are
;; an implementation detail of the Phase 47 lowering passes,
;; not public API).
;;
;; Unlocks (Doc 117 §117.D.gaps.3 deferred handlers):
;;
;;   * `install-sigint-handler' (= sigaction SIGINT + SA_RESTART)
;;   * `install-sigwinch-handler' (= sigaction SIGWINCH)
;;   * `terminal-window-size' (= ioctl TIOCGWINSZ)
;;   * `terminal-set-raw' (= tcsetattr TCSANOW + VMIN + VTIME)
;;   * `poll-stdin-readable' (= poll POLLIN / POLLHUP)
;;   * `nonblocking-fd' (= fcntl F_GETFL / F_SETFL / O_NONBLOCK)
;;
;; Drift detection:
;;
;;   `nelisp-cc--libc-constants-checksum' is the SHA-1 of the
;;   canonical "NAME=VALUE\n…" lines emitted by the C probe.
;;   `scripts/verify-libc-constants.sh' recomputes the same
;;   checksum and `diff's the two; a non-zero exit code from
;;   the script flags drift so `make verify-libc-constants'
;;   fails the build before any handler swap is allowed.

;;; Code:

;; --- Signals ---------------------------------------------------------------
;;
;; Values per POSIX <signal.h>.  linux-glibc-x86_64 numbering;
;; SIGCHLD differs on Alpha/SPARC and a few BSDs (= 20 there),
;; but linux/glibc has parked on the numbers below since the
;; Linux 1.x days so they are effectively ABI-stable.

(defconst nelisp-cc--SIGHUP   1)
(defconst nelisp-cc--SIGINT   2)
(defconst nelisp-cc--SIGQUIT  3)
(defconst nelisp-cc--SIGILL   4)
(defconst nelisp-cc--SIGABRT  6)
(defconst nelisp-cc--SIGFPE   8)
(defconst nelisp-cc--SIGKILL  9)
(defconst nelisp-cc--SIGSEGV  11)
(defconst nelisp-cc--SIGPIPE  13)
(defconst nelisp-cc--SIGALRM  14)
(defconst nelisp-cc--SIGTERM  15)
(defconst nelisp-cc--SIGCHLD  17)
(defconst nelisp-cc--SIGCONT  18)
(defconst nelisp-cc--SIGSTOP  19)
(defconst nelisp-cc--SIGTSTP  20)
(defconst nelisp-cc--SIGWINCH 28)

;; --- sigaction flags --------------------------------------------------------
;;
;; Per <signal.h>.  SA_RESTART is the workhorse flag — it lets
;; an in-flight `read'/`poll' on stdin auto-restart after the
;; SIGINT handler returns so the editor read loop does not see
;; EINTR.  Doc 117 §117.D.gaps.3 quit handler uses this.

(defconst nelisp-cc--SA_NOCLDSTOP #x1)
(defconst nelisp-cc--SA_SIGINFO   #x4)
(defconst nelisp-cc--SA_RESTART   #x10000000)
(defconst nelisp-cc--SA_NODEFER   #x40000000)

;; --- ioctl: terminal window size --------------------------------------------
;;
;; Per <asm-generic/ioctls.h>.  Used by the SIGWINCH handler to
;; re-read the terminal size after a window resize.  Distinct
;; from the BSD value (= 0x40087468 on Darwin) — Phase 47
;; targets only linux-glibc for now.

(defconst nelisp-cc--TIOCGWINSZ #x5413)
(defconst nelisp-cc--TIOCSWINSZ #x5414)

;; --- termios: raw mode toggles ----------------------------------------------
;;
;; Per <termios.h>.  TCSANOW = apply changes immediately
;; (vs. TCSADRAIN / TCSAFLUSH).  VMIN / VTIME are indices into
;; the `c_cc' array — Doc 117 §117.D.gaps raw-mode handler
;; needs `c_cc[VMIN] = 1; c_cc[VTIME] = 0;'.

(defconst nelisp-cc--TCSANOW   0)
(defconst nelisp-cc--TCSADRAIN 1)
(defconst nelisp-cc--TCSAFLUSH 2)
(defconst nelisp-cc--VMIN  6)
(defconst nelisp-cc--VTIME 5)

;; --- poll: event bits -------------------------------------------------------
;;
;; Per <poll.h>.  POLLIN / POLLHUP are the two bits the stdin
;; poll handler needs — readable data vs. peer-closed FD.

(defconst nelisp-cc--POLLIN   #x1)
(defconst nelisp-cc--POLLPRI  #x2)
(defconst nelisp-cc--POLLOUT  #x4)
(defconst nelisp-cc--POLLERR  #x8)
(defconst nelisp-cc--POLLHUP  #x10)
(defconst nelisp-cc--POLLNVAL #x20)

;; --- fcntl: nonblocking fd toggling -----------------------------------------
;;
;; Per <fcntl.h>.  Pair `(F_GETFL, F_SETFL, O_NONBLOCK)' to
;; flip an FD between blocking and nonblocking.  O_NONBLOCK is
;; 0x800 on linux-glibc (= 04000 octal), different on BSD/Darwin.

(defconst nelisp-cc--F_GETFL    3)
(defconst nelisp-cc--F_SETFL    4)
(defconst nelisp-cc--O_NONBLOCK #x800)
(defconst nelisp-cc--O_CLOEXEC  #x80000)

;; --- Drift detection --------------------------------------------------------
;;
;; SHA-1 over the canonical `NAME=VALUE\n' rows in the exact
;; order they are emitted by `scripts/verify-libc-constants.sh'.
;; Recompute on purpose-driven updates: any value change forces
;; a checksum bump in the same commit, which makes a stale or
;; copy-paste error visible in `git diff'.
;;
;; The canonical emission ordering matches the table below
;; (= the same order the C probe prints).  Do NOT alphabetise.

(defconst nelisp-cc--libc-constants-table
  '(("SIGHUP"       . 1)
    ("SIGINT"       . 2)
    ("SIGQUIT"      . 3)
    ("SIGILL"       . 4)
    ("SIGABRT"      . 6)
    ("SIGFPE"       . 8)
    ("SIGKILL"      . 9)
    ("SIGSEGV"      . 11)
    ("SIGPIPE"      . 13)
    ("SIGALRM"      . 14)
    ("SIGTERM"      . 15)
    ("SIGCHLD"      . 17)
    ("SIGCONT"      . 18)
    ("SIGSTOP"      . 19)
    ("SIGTSTP"      . 20)
    ("SIGWINCH"     . 28)
    ("SA_NOCLDSTOP" . #x1)
    ("SA_SIGINFO"   . #x4)
    ("SA_RESTART"   . #x10000000)
    ("SA_NODEFER"   . #x40000000)
    ("TIOCGWINSZ"   . #x5413)
    ("TIOCSWINSZ"   . #x5414)
    ("TCSANOW"      . 0)
    ("TCSADRAIN"    . 1)
    ("TCSAFLUSH"    . 2)
    ("VMIN"         . 6)
    ("VTIME"        . 5)
    ("POLLIN"       . #x1)
    ("POLLPRI"      . #x2)
    ("POLLOUT"      . #x4)
    ("POLLERR"      . #x8)
    ("POLLHUP"      . #x10)
    ("POLLNVAL"     . #x20)
    ("F_GETFL"      . 3)
    ("F_SETFL"      . 4)
    ("O_NONBLOCK"   . #x800)
    ("O_CLOEXEC"    . #x80000))
  "Canonical (NAME . VALUE) table for the Doc 122 §122.K libc constants.
Ordering matches `scripts/verify-libc-constants.sh's C probe emission
so the SHA-1 checksum in `nelisp-cc--libc-constants-checksum' diffs
byte-identically against the probe output.")

(defun nelisp-cc--libc-constants-canonical-text ()
  "Return the canonical \"NAME=VALUE\\n\" text the C probe emits.

Values are formatted to match `scripts/verify-libc-constants.sh':
SA_* / TIOC* / POLL* / O_* are printed in `0x%x' hex; the remaining
constants in decimal.  Used by both the checksum self-test below
and the verifier script to keep the elisp and C sides in lock-step."
  (let ((hex-set '("SA_NOCLDSTOP" "SA_SIGINFO" "SA_RESTART" "SA_NODEFER"
                   "TIOCGWINSZ" "TIOCSWINSZ"
                   "POLLIN" "POLLPRI" "POLLOUT" "POLLERR" "POLLHUP" "POLLNVAL"
                   "O_NONBLOCK" "O_CLOEXEC"))
        (out ""))
    (dolist (row nelisp-cc--libc-constants-table)
      (let* ((name (car row))
             (val  (cdr row))
             (line (if (member name hex-set)
                       (format "%s=0x%x\n" name val)
                     (format "%s=%d\n" name val))))
        (setq out (concat out line))))
    out))

(defconst nelisp-cc--libc-constants-checksum
  "b93cbf45d0af370b220277dedd0437097e3da1bd"
  "SHA-1 over `nelisp-cc--libc-constants-canonical-text' (elisp side).
Pinning the value here lets `git diff' surface any unintentional
table edit even before the verifier is invoked.

Ground truth = `scripts/verify-libc-constants.sh' (= the C probe).
The verifier diffs elisp output against the probe output directly,
so this checksum is informational — kept in sync on purposeful edits
by re-running:

  emacs --batch -Q -L lisp -l nelisp-cc-libc-constants \\
    --eval \"(princ (nelisp-cc--libc-constants-canonical-text))\" \\
    | sha1sum

Computed on linux-glibc-x86_64 against the 37-row canonical table.")

(provide 'nelisp-cc-libc-constants)

;;; nelisp-cc-libc-constants.el ends here
