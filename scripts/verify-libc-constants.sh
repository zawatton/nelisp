#!/bin/sh
# Doc 122 §122.K — verify the elisp-side libc constants table against
# the host's actual libc headers.  Compiles a tiny C probe that prints
# each constant in the canonical "NAME=VALUE\n" form
# `nelisp-cc--libc-constants-canonical-text' emits, then `diff's the
# two.  Exit code 0 = match (= elisp side is in sync with libc),
# non-zero = drift (= refuse to ship handler swaps that assume the
# stale value).
#
# Usage:
#   scripts/verify-libc-constants.sh             # batch / CI mode
#   make verify-libc-constants                   # via Makefile target
#
# Environment:
#   EMACS    — emacs binary (default: emacs)
#   CC       — C compiler  (default: cc)
#   TMPDIR   — scratch dir (default: /tmp)
#
# Output files (kept on success for inspection):
#   $TMPDIR/nelisp-libc-probe.c
#   $TMPDIR/nelisp-libc-probe.out
#   $TMPDIR/nelisp-libc-elisp.txt
#   $TMPDIR/nelisp-libc-c.txt
#
# Exit codes:
#   0 — elisp table matches libc headers
#   1 — drift detected (= run `diff -u' between the two for details)
#   2 — toolchain failure (cc / emacs missing / probe didn't compile)

set -eu

EMACS="${EMACS:-emacs}"
CC="${CC:-cc}"
TMPDIR="${TMPDIR:-/tmp}"
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

PROBE_C="$TMPDIR/nelisp-libc-probe.c"
PROBE_BIN="$TMPDIR/nelisp-libc-probe"
C_OUT="$TMPDIR/nelisp-libc-c.txt"
ELISP_OUT="$TMPDIR/nelisp-libc-elisp.txt"

cleanup() {
  # Keep artifacts for post-mortem on failure, only sweep on success.
  if [ "${KEEP_ARTIFACTS:-0}" = "0" ] && [ "${1:-0}" = "0" ]; then
    rm -f "$PROBE_C" "$PROBE_BIN" "$C_OUT" "$ELISP_OUT"
  fi
}

# Toolchain sanity.
command -v "$CC"    >/dev/null 2>&1 || { echo "verify-libc-constants: $CC not in PATH" >&2; exit 2; }
command -v "$EMACS" >/dev/null 2>&1 || { echo "verify-libc-constants: $EMACS not in PATH" >&2; exit 2; }

# --- 1. C probe ----------------------------------------------------------
#
# Emission order must mirror `nelisp-cc--libc-constants-table' in
# `lisp/nelisp-cc-libc-constants.el'.  Editing this list without
# editing the table is a drift the diff at the bottom catches.

cat > "$PROBE_C" <<'EOF'
#include <stdio.h>
#include <signal.h>
#include <sys/ioctl.h>
#include <termios.h>
#include <poll.h>
#include <fcntl.h>

int main(void) {
    /* Signals — decimal. */
    printf("SIGHUP=%d\n",   SIGHUP);
    printf("SIGINT=%d\n",   SIGINT);
    printf("SIGQUIT=%d\n",  SIGQUIT);
    printf("SIGILL=%d\n",   SIGILL);
    printf("SIGABRT=%d\n",  SIGABRT);
    printf("SIGFPE=%d\n",   SIGFPE);
    printf("SIGKILL=%d\n",  SIGKILL);
    printf("SIGSEGV=%d\n",  SIGSEGV);
    printf("SIGPIPE=%d\n",  SIGPIPE);
    printf("SIGALRM=%d\n",  SIGALRM);
    printf("SIGTERM=%d\n",  SIGTERM);
    printf("SIGCHLD=%d\n",  SIGCHLD);
    printf("SIGCONT=%d\n",  SIGCONT);
    printf("SIGSTOP=%d\n",  SIGSTOP);
    printf("SIGTSTP=%d\n",  SIGTSTP);
    printf("SIGWINCH=%d\n", SIGWINCH);

    /* sigaction flags — hex (= bitfield semantics). */
    printf("SA_NOCLDSTOP=0x%x\n", (unsigned int)SA_NOCLDSTOP);
    printf("SA_SIGINFO=0x%x\n",   (unsigned int)SA_SIGINFO);
    printf("SA_RESTART=0x%x\n",   (unsigned int)SA_RESTART);
    printf("SA_NODEFER=0x%x\n",   (unsigned int)SA_NODEFER);

    /* ioctl: terminal window size — hex (= ioctl number). */
    printf("TIOCGWINSZ=0x%lx\n", (unsigned long)TIOCGWINSZ);
    printf("TIOCSWINSZ=0x%lx\n", (unsigned long)TIOCSWINSZ);

    /* termios — decimal (= small enums + c_cc indices). */
    printf("TCSANOW=%d\n",   TCSANOW);
    printf("TCSADRAIN=%d\n", TCSADRAIN);
    printf("TCSAFLUSH=%d\n", TCSAFLUSH);
    printf("VMIN=%d\n",      VMIN);
    printf("VTIME=%d\n",     VTIME);

    /* poll: event bits — hex. */
    printf("POLLIN=0x%x\n",   POLLIN);
    printf("POLLPRI=0x%x\n",  POLLPRI);
    printf("POLLOUT=0x%x\n",  POLLOUT);
    printf("POLLERR=0x%x\n",  POLLERR);
    printf("POLLHUP=0x%x\n",  POLLHUP);
    printf("POLLNVAL=0x%x\n", POLLNVAL);

    /* fcntl: nonblocking fd toggling. */
    printf("F_GETFL=%d\n",    F_GETFL);
    printf("F_SETFL=%d\n",    F_SETFL);
    printf("O_NONBLOCK=0x%x\n", O_NONBLOCK);
    printf("O_CLOEXEC=0x%x\n",  O_CLOEXEC);

    return 0;
}
EOF

"$CC" "$PROBE_C" -o "$PROBE_BIN" || { echo "verify-libc-constants: probe failed to compile" >&2; exit 2; }
"$PROBE_BIN" > "$C_OUT"

# --- 2. Elisp side ----------------------------------------------------------

"$EMACS" --batch -Q -L "$REPO_ROOT/lisp" \
  -l nelisp-cc-libc-constants \
  --eval "(with-temp-file \"$ELISP_OUT\" (insert (nelisp-cc--libc-constants-canonical-text)))" \
  >/dev/null 2>&1 || { echo "verify-libc-constants: emacs --batch failed" >&2; exit 2; }

# --- 3. Diff ---------------------------------------------------------------

if diff -u "$ELISP_OUT" "$C_OUT" > "$TMPDIR/nelisp-libc-diff.txt"; then
  echo "verify-libc-constants: elisp table matches libc headers (37 constants)"
  cleanup 0
  exit 0
else
  echo "verify-libc-constants: DRIFT — elisp table != libc headers" >&2
  echo "" >&2
  echo "--- elisp ($ELISP_OUT)" >&2
  echo "+++ probe ($C_OUT)" >&2
  cat "$TMPDIR/nelisp-libc-diff.txt" >&2
  echo "" >&2
  echo "Edit lisp/nelisp-cc-libc-constants.el so each (NAME . VALUE) row" >&2
  echo "in nelisp-cc--libc-constants-table matches the probe output," >&2
  echo "then update each individual defconst above it, then re-run." >&2
  cleanup 1
  exit 1
fi
