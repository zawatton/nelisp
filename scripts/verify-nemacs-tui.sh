#!/usr/bin/env bash
# Smoke/infrastructure for nemacs -nw / TUI readiness.
#
# This script is intentionally conservative:
#   - batch -nw is a hard gate and fails on regression.
#   - interactive pty probes report TODO for unfinished command coverage, but
#     any TODO or host-fallback detection makes this gate fail.
#   - terminal mode restoration is checked on a private pty so the caller's
#     shell is not affected.
#
# Usage:
#   scripts/verify-nemacs-tui.sh
#   PATH=/path/to/nelisp-gui/bin:$PATH scripts/verify-nemacs-tui.sh --via-emacs-wrapper
#
# Environment:
#   NEMACS_TUI_DRIVER=host|nelisp  driver for direct bin/nemacs probes
#   NEMACS_TUI_TIMEOUT=SECONDS     pty wait timeout, default 45
#                                  (standalone nelisp REPL startup + image load
#                                  can take 20-60s on a cold box; the probes
#                                  break early once settled, so a generous cap
#                                  only helps slow startups and never slows a
#                                  fast one — 8s flaked into spurious TODOs)

set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
DIRECT_NEMACS="$REPO_ROOT/bin/nemacs"
VIA_EMACS_WRAPPER=0
TIMEOUT="${NEMACS_TUI_TIMEOUT:-45}"
DRIVER="${NEMACS_TUI_DRIVER:-nelisp}"

usage() {
  sed -n '2,20p' "$0"
}

while (($# > 0)); do
  case "$1" in
    --via-emacs-wrapper) VIA_EMACS_WRAPPER=1; shift ;;
    --driver=*) DRIVER="${1#--driver=}"; shift ;;
    --timeout=*) TIMEOUT="${1#--timeout=}"; shift ;;
    --help|-h) usage; exit 0 ;;
    *)
      echo "FAIL: unknown argument: $1" >&2
      usage >&2
      exit 2 ;;
  esac
done

pass=0
fail=0
todo=0

note_pass() { echo "PASS: $*"; pass=$((pass + 1)); }
note_fail() { echo "FAIL: $*"; fail=$((fail + 1)); }
note_todo() { echo "TODO: $*"; todo=$((todo + 1)); }

run_capture() {
  local label="$1"
  shift
  local out rc
  set +e
  out="$("$@" 2>&1)"
  rc=$?
  set -e
  if (( rc == 0 )); then
    printf '%s\n' "$out"
    return 0
  fi
  printf '%s\n' "$out"
  return "$rc"
}

batch_direct() {
  local out
  if out="$(run_capture "direct batch" \
      "$DIRECT_NEMACS" --driver="$DRIVER" --batch --no-banner -nw \
      --eval '(princ "NEMACS_TUI_BATCH_OK\n")')"; then
    if grep -q 'NEMACS_TUI_BATCH_OK' <<<"$out"; then
      note_pass "direct bin/nemacs --batch -nw"
    else
      note_fail "direct bin/nemacs --batch -nw did not print marker"
      printf '%s\n' "$out" | sed 's/^/  /'
    fi
  else
    note_fail "direct bin/nemacs --batch -nw exited non-zero"
    printf '%s\n' "$out" | sed 's/^/  /'
  fi
}

batch_wrapper() {
  local cmd out
  if (( VIA_EMACS_WRAPPER )); then
    cmd="$(command -v emacs || true)"
    if [[ -z "$cmd" ]]; then
      note_fail "--via-emacs-wrapper requested but emacs is not on PATH"
      return
    fi
    if [[ "$cmd" == "/usr/bin/emacs" || "$cmd" == "/bin/emacs" ]]; then
      note_todo "emacs wrapper not found ahead of host Emacs on PATH ($cmd)"
      return
    fi
    if out="$(run_capture "wrapper batch" \
        "$cmd" -Q -nw --batch --eval '(princ "NEMACS_TUI_WRAPPER_BATCH_OK\n")')"; then
      if grep -q 'NEMACS_TUI_WRAPPER_BATCH_OK' <<<"$out"; then
        note_pass "emacs wrapper --batch -nw ($cmd)"
      else
        note_fail "emacs wrapper --batch -nw did not print marker ($cmd)"
        printf '%s\n' "$out" | sed 's/^/  /'
      fi
    else
      note_fail "emacs wrapper --batch -nw exited non-zero ($cmd)"
      printf '%s\n' "$out" | sed 's/^/  /'
    fi
  else
    if out="$(run_capture "bin wrapper batch" \
        "$DIRECT_NEMACS" --driver="$DRIVER" --batch --no-banner -nw \
        --eval '(princ "NEMACS_TUI_WRAPPER_BATCH_OK\n")')"; then
      if grep -q 'NEMACS_TUI_WRAPPER_BATCH_OK' <<<"$out"; then
        note_pass "bin/nemacs wrapper path --batch -nw"
      else
        note_fail "bin/nemacs wrapper path did not print marker"
        printf '%s\n' "$out" | sed 's/^/  /'
      fi
    else
      note_fail "bin/nemacs wrapper path --batch -nw exited non-zero"
      printf '%s\n' "$out" | sed 's/^/  /'
    fi
  fi
}

pty_probe() {
  local mode="$1"
  local launcher="$DIRECT_NEMACS"
  local file
  file="$(mktemp "${TMPDIR:-/tmp}/nemacs-tui-smoke.XXXXXX.txt")"
  printf 'seed\n' > "$file"

  if (( VIA_EMACS_WRAPPER )); then
    launcher="$(command -v emacs || true)"
    if [[ -z "$launcher" ]]; then
      note_fail "interactive pty: emacs wrapper not found on PATH"
      rm -f "$file"
      return
    fi
  fi

  local probe_out rc
  set +e
  probe_out="$(python3 - "$launcher" "$file" "$TIMEOUT" "$DRIVER" "$mode" "$VIA_EMACS_WRAPPER" <<'PY'
import errno
import fcntl
import os
import pty
import select
import signal
import subprocess
import sys
import termios
import time

launcher, path, timeout_s, driver, mode, via = sys.argv[1:]
timeout = float(timeout_s)
via_wrapper = via == "1"

def flags(attrs):
    lflag = attrs[3]
    return {
        "echo": bool(lflag & termios.ECHO),
        "icanon": bool(lflag & termios.ICANON),
    }

def read_available(fd):
    out = bytearray()
    while True:
        r, _, _ = select.select([fd], [], [], 0)
        if not r:
            break
        try:
            chunk = os.read(fd, 4096)
        except OSError as e:
            if e.errno in (errno.EIO, errno.EBADF):
                break
            raise
        if not chunk:
            break
        out.extend(chunk)
    return bytes(out)

master, slave = pty.openpty()
before = flags(termios.tcgetattr(slave))
env = os.environ.copy()
env["TERM"] = "xterm-256color"
env.setdefault("COLUMNS", "80")
env.setdefault("LINES", "24")
env["NEMACS_DRIVER"] = driver

if via_wrapper:
    cmd = [launcher, "-Q", "-nw", path]
else:
    cmd = [launcher, "--driver=" + driver, "-Q", "-nw", path]

def child_setup():
    os.setsid()
    try:
        fcntl.ioctl(slave, termios.TIOCSCTTY, 0)
    except OSError:
        pass

proc = subprocess.Popen(
    cmd,
    stdin=slave,
    stdout=slave,
    stderr=slave,
    cwd=os.path.dirname(os.path.dirname(launcher)) if not via_wrapper else None,
    env=env,
    close_fds=True,
    preexec_fn=child_setup,
)

started = time.time()
buf = bytearray()
settled = False
raw_observed = False
while time.time() - started < timeout:
    r, _, _ = select.select([master], [], [], 0.1)
    if r:
        try:
            chunk = os.read(master, 4096)
        except OSError:
            break
        if not chunk:
            break
        buf.extend(chunk)
        if b"seed" in buf or b"\x1b[" in buf:
            settled = True
    cur = flags(termios.tcgetattr(slave))
    if before.get("echo") and before.get("icanon") and (
        not cur.get("echo") or not cur.get("icanon")
    ):
        raw_observed = True
        if settled:
            break
    if proc.poll() is not None:
        break

mid = flags(termios.tcgetattr(slave))

if mode == "graceful":
    try:
        os.write(master, b"a\x7f\r\x18\x03")
    except OSError:
        pass
else:
    try:
        os.killpg(proc.pid, signal.SIGTERM)
    except ProcessLookupError:
        pass

try:
    proc.wait(timeout=timeout)
    exited = True
except subprocess.TimeoutExpired:
    exited = False
    try:
        os.killpg(proc.pid, signal.SIGTERM)
    except ProcessLookupError:
        pass
    try:
        proc.wait(timeout=2)
    except subprocess.TimeoutExpired:
        try:
            os.killpg(proc.pid, signal.SIGKILL)
        except ProcessLookupError:
            pass
        proc.wait()

buf.extend(read_available(master))
after = flags(termios.tcgetattr(slave))
os.close(master)
os.close(slave)

text = bytes(buf).decode("utf-8", "replace")
host_fallback = "GNU Emacs" in text or "For information about GNU Emacs" in text
raw_seen = before.get("echo") and before.get("icanon") and (
    raw_observed or not mid.get("echo") or not mid.get("icanon")
)
restored = after.get("echo") and after.get("icanon")

print("PTY_RESULT mode=%s rc=%s exited=%s settled=%s raw_seen=%s restored=%s host_fallback=%s" %
      (mode, proc.returncode, exited, settled, raw_seen, restored, host_fallback))
sample = text.replace("\r", "\\r").replace("\n", "\\n")
if len(sample) > 500:
    sample = sample[:500] + "..."
print("PTY_OUTPUT_SAMPLE " + sample)

if not restored:
    sys.exit(10)
if host_fallback:
    sys.exit(20)
if not raw_seen:
    sys.exit(30)
if mode == "graceful" and not exited:
    sys.exit(40)
sys.exit(0)
PY
)"
  rc=$?
  set -e
  printf '%s\n' "$probe_out"
  rm -f "$file"

  if grep -q 'restored=True' <<<"$probe_out"; then
    note_pass "interactive pty $mode terminal restore"
  fi

  case "$rc" in
    0) note_pass "interactive pty $mode raw TUI path" ;;
    10) note_fail "interactive pty $mode left terminal without echo/canonical mode" ;;
    20) note_fail "interactive pty $mode reached host Emacs fallback; nelisp TUI ownership not proven" ;;
    30) note_todo "interactive pty $mode did not enter observable raw/cbreak mode yet" ;;
    40) note_todo "interactive pty $mode did not exit via C-x C-c yet" ;;
    *) note_todo "interactive pty $mode probe ended in current unimplemented/unknown state (rc=$rc)" ;;
  esac
}

pty_fileio_probe() {
  local launcher="$DIRECT_NEMACS"
  local file
  file="$(mktemp "${TMPDIR:-/tmp}/nemacs-tui-fileio.XXXXXX.txt")"
  printf 'seed\n' > "$file"

  if (( VIA_EMACS_WRAPPER )); then
    launcher="$(command -v emacs || true)"
    if [[ -z "$launcher" ]]; then
      note_fail "interactive pty fileio: emacs wrapper not found on PATH"
      rm -f "$file"
      return
    fi
  fi

  local probe_out rc
  set +e
  probe_out="$(python3 - "$launcher" "$file" "$TIMEOUT" "$DRIVER" "$VIA_EMACS_WRAPPER" <<'PY'
import errno
import fcntl
import os
import pty
import select
import signal
import subprocess
import sys
import termios
import time

launcher, path, timeout_s, driver, via = sys.argv[1:]
timeout = float(timeout_s)
via_wrapper = via == "1"
marker = "NEMACS_TUI_FILEIO_MARKER"

def flags(attrs):
    lflag = attrs[3]
    return {
        "echo": bool(lflag & termios.ECHO),
        "icanon": bool(lflag & termios.ICANON),
    }

def read_available(fd):
    out = bytearray()
    while True:
        r, _, _ = select.select([fd], [], [], 0)
        if not r:
            break
        try:
            chunk = os.read(fd, 4096)
        except OSError as e:
            if e.errno in (errno.EIO, errno.EBADF):
                break
            raise
        if not chunk:
            break
        out.extend(chunk)
    return bytes(out)

master, slave = pty.openpty()
before = flags(termios.tcgetattr(slave))
env = os.environ.copy()
env["TERM"] = "xterm-256color"
env.setdefault("COLUMNS", "80")
env.setdefault("LINES", "24")
env["NEMACS_DRIVER"] = driver

if via_wrapper:
    cmd = [launcher, "-Q", "-nw"]
else:
    cmd = [launcher, "--driver=" + driver, "-Q", "-nw"]

def child_setup():
    os.setsid()
    try:
        fcntl.ioctl(slave, termios.TIOCSCTTY, 0)
    except OSError:
        pass

proc = subprocess.Popen(
    cmd,
    stdin=slave,
    stdout=slave,
    stderr=slave,
    cwd=os.path.dirname(os.path.dirname(launcher)) if not via_wrapper else None,
    env=env,
    close_fds=True,
    preexec_fn=child_setup,
)

started = time.time()
buf = bytearray()
settled = False
raw_observed = False
while time.time() - started < timeout:
    r, _, _ = select.select([master], [], [], 0.1)
    if r:
        try:
            chunk = os.read(master, 4096)
        except OSError:
            break
        if not chunk:
            break
        buf.extend(chunk)
        if b"\x1b[" in buf or b"*scratch*" in buf:
            settled = True
    cur = flags(termios.tcgetattr(slave))
    if before.get("echo") and before.get("icanon") and (
        not cur.get("echo") or not cur.get("icanon")
    ):
        raw_observed = True
        if settled:
            break
    if proc.poll() is not None:
        break

mid = flags(termios.tcgetattr(slave))

def pump_until(predicate, seconds):
    deadline = time.time() + seconds
    while time.time() < deadline:
        r, _, _ = select.select([master], [], [], 0.05)
        if r:
            try:
                chunk = os.read(master, 4096)
            except OSError:
                return False
            if not chunk:
                return False
            buf.extend(chunk)
            if predicate(bytes(buf)):
                return True
        if proc.poll() is not None:
            return False
    return predicate(bytes(buf))

prompt_seen = False
try:
    os.write(master, b"\x18\x06")
    prompt_seen = pump_until(lambda data: b"Find file:" in data, min(timeout / 3.0, 5.0))
    os.write(master, path.encode("utf-8") + b"\r")
    pump_until(lambda data: path.encode("utf-8") in data, 0.5)
    time.sleep(0.2)
    os.write(master, marker.encode("ascii"))
    time.sleep(0.2)
    os.write(master, b"\x18\x13")
    time.sleep(0.5)
    os.write(master, b"\x18\x03")
except OSError:
    pass

try:
    proc.wait(timeout=timeout)
    exited = True
except subprocess.TimeoutExpired:
    exited = False
    try:
        os.killpg(proc.pid, signal.SIGTERM)
    except ProcessLookupError:
        pass
    try:
        proc.wait(timeout=2)
    except subprocess.TimeoutExpired:
        try:
            os.killpg(proc.pid, signal.SIGKILL)
        except ProcessLookupError:
            pass
        proc.wait()

buf.extend(read_available(master))
after = flags(termios.tcgetattr(slave))
os.close(master)
os.close(slave)

try:
    with open(path, "r", encoding="utf-8", errors="replace") as f:
        file_text = f.read()
except OSError:
    file_text = ""

text = bytes(buf).decode("utf-8", "replace")
host_fallback = "GNU Emacs" in text or "For information about GNU Emacs" in text
raw_seen = before.get("echo") and before.get("icanon") and (
    raw_observed or not mid.get("echo") or not mid.get("icanon")
)
restored = after.get("echo") and after.get("icanon")
saved = marker in file_text
preserved = "seed\n" in file_text

print("PTY_FILEIO_RESULT rc=%s exited=%s settled=%s raw_seen=%s restored=%s host_fallback=%s saved=%s" %
      (proc.returncode, exited, settled, raw_seen, restored, host_fallback, saved))
print("PTY_FILEIO_PRESERVED %s" % preserved)
print("PTY_FILEIO_PROMPT_SEEN %s" % prompt_seen)
sample = text.replace("\r", "\\r").replace("\n", "\\n")
if len(sample) > 500:
    sample = sample[:500] + "..."
print("PTY_FILEIO_OUTPUT_SAMPLE " + sample)
print("PTY_FILEIO_FILE " + file_text.replace("\n", "\\n"))

if not restored:
    sys.exit(10)
if host_fallback:
    sys.exit(20)
if not raw_seen:
    sys.exit(30)
if not exited:
    sys.exit(40)
if not saved:
    sys.exit(50)
if not preserved:
    sys.exit(51)
sys.exit(0)
PY
)"
  rc=$?
  set -e
  printf '%s\n' "$probe_out"
  rm -f "$file"

  if grep -q 'restored=True' <<<"$probe_out"; then
    note_pass "interactive pty fileio terminal restore"
  fi

  case "$rc" in
    0) note_pass "interactive pty fileio find/edit/save/quit" ;;
    10) note_fail "interactive pty fileio left terminal without echo/canonical mode" ;;
    20) note_fail "interactive pty fileio reached host Emacs fallback; nelisp TUI ownership not proven" ;;
    30) note_todo "interactive pty fileio did not enter observable raw/cbreak mode yet" ;;
    40) note_todo "interactive pty fileio did not exit via C-x C-c yet" ;;
    50) note_todo "interactive pty fileio did not persist edited file via C-x C-s yet" ;;
    51) note_todo "interactive pty fileio did not preserve existing file contents yet" ;;
    *) note_todo "interactive pty fileio probe ended in current unimplemented/unknown state (rc=$rc)" ;;
  esac
}

# Multiple-buffer + save coverage: open two distinct files in sequence
# (C-x C-f), edit each, save each (C-x C-s), then quit (C-x C-c).  Both
# files must end up holding their own marker and their pre-existing seed
# line, which only happens if two independent buffers were created, kept
# distinct, and written back.  Reuses the same proven key path as the
# single-file fileio probe, so a regression here is buffer management,
# not transport.
pty_multibuffer_probe() {
  local launcher="$DIRECT_NEMACS"
  local file1 file2
  file1="$(mktemp "${TMPDIR:-/tmp}/nemacs-tui-mbuf1.XXXXXX.txt")"
  file2="$(mktemp "${TMPDIR:-/tmp}/nemacs-tui-mbuf2.XXXXXX.txt")"
  printf 'seed-one\n' > "$file1"
  printf 'seed-two\n' > "$file2"

  if (( VIA_EMACS_WRAPPER )); then
    launcher="$(command -v emacs || true)"
    if [[ -z "$launcher" ]]; then
      note_fail "interactive pty multibuffer: emacs wrapper not found on PATH"
      rm -f "$file1" "$file2"
      return
    fi
  fi

  local probe_out rc
  set +e
  probe_out="$(python3 - "$launcher" "$file1" "$file2" "$TIMEOUT" "$DRIVER" "$VIA_EMACS_WRAPPER" <<'PY'
import errno
import fcntl
import os
import pty
import select
import signal
import subprocess
import sys
import termios
import time

launcher, path1, path2, timeout_s, driver, via = sys.argv[1:]
timeout = float(timeout_s)
via_wrapper = via == "1"
marker1 = "NEMACS_MBUF_MARKER_ONE"
marker2 = "NEMACS_MBUF_MARKER_TWO"

def flags(attrs):
    lflag = attrs[3]
    return {
        "echo": bool(lflag & termios.ECHO),
        "icanon": bool(lflag & termios.ICANON),
    }

def read_available(fd):
    out = bytearray()
    while True:
        r, _, _ = select.select([fd], [], [], 0)
        if not r:
            break
        try:
            chunk = os.read(fd, 4096)
        except OSError as e:
            if e.errno in (errno.EIO, errno.EBADF):
                break
            raise
        if not chunk:
            break
        out.extend(chunk)
    return bytes(out)

master, slave = pty.openpty()
before = flags(termios.tcgetattr(slave))
env = os.environ.copy()
env["TERM"] = "xterm-256color"
env.setdefault("COLUMNS", "80")
env.setdefault("LINES", "24")
env["NEMACS_DRIVER"] = driver

if via_wrapper:
    cmd = [launcher, "-Q", "-nw"]
else:
    cmd = [launcher, "--driver=" + driver, "-Q", "-nw"]

def child_setup():
    os.setsid()
    try:
        fcntl.ioctl(slave, termios.TIOCSCTTY, 0)
    except OSError:
        pass

proc = subprocess.Popen(
    cmd,
    stdin=slave,
    stdout=slave,
    stderr=slave,
    cwd=os.path.dirname(os.path.dirname(launcher)) if not via_wrapper else None,
    env=env,
    close_fds=True,
    preexec_fn=child_setup,
)

started = time.time()
buf = bytearray()
settled = False
raw_observed = False
while time.time() - started < timeout:
    r, _, _ = select.select([master], [], [], 0.1)
    if r:
        try:
            chunk = os.read(master, 4096)
        except OSError:
            break
        if not chunk:
            break
        buf.extend(chunk)
        if b"\x1b[" in buf or b"*scratch*" in buf:
            settled = True
    cur = flags(termios.tcgetattr(slave))
    if before.get("echo") and before.get("icanon") and (
        not cur.get("echo") or not cur.get("icanon")
    ):
        raw_observed = True
        if settled:
            break
    if proc.poll() is not None:
        break

mid = flags(termios.tcgetattr(slave))

def pump_until(predicate, seconds):
    deadline = time.time() + seconds
    while time.time() < deadline:
        r, _, _ = select.select([master], [], [], 0.05)
        if r:
            try:
                chunk = os.read(master, 4096)
            except OSError:
                return False
            if not chunk:
                return False
            buf.extend(chunk)
            if predicate(bytes(buf)):
                return True
        if proc.poll() is not None:
            return False
    return predicate(bytes(buf))

def visit_and_edit(path, marker):
    # C-x C-f PATH RET <marker> C-x C-s.  Use fixed settle waits rather
    # than pump_until on the cumulative buffer: across two cycles the old
    # "Find file:" prompt text is still in the buffer, so a predicate match
    # would return instantly and race ahead of the freshly opened
    # minibuffer.  Fixed sleeps keep each cycle ordered.
    try:
        os.write(master, b"\x18\x06")               # C-x C-f
        time.sleep(min(timeout / 6.0, 3.0))         # let the minibuffer open
        os.write(master, path.encode("utf-8") + b"\r")
        time.sleep(1.0)                             # let the file load into a buffer
        os.write(master, marker.encode("ascii"))
        time.sleep(0.5)
        os.write(master, b"\x18\x13")               # C-x C-s
        time.sleep(1.2)                             # let the save settle
    except OSError:
        pass

visit_and_edit(path1, marker1)
visit_and_edit(path2, marker2)
try:
    os.write(master, b"\x18\x03")
except OSError:
    pass

try:
    proc.wait(timeout=timeout)
    exited = True
except subprocess.TimeoutExpired:
    exited = False
    try:
        os.killpg(proc.pid, signal.SIGTERM)
    except ProcessLookupError:
        pass
    try:
        proc.wait(timeout=2)
    except subprocess.TimeoutExpired:
        try:
            os.killpg(proc.pid, signal.SIGKILL)
        except ProcessLookupError:
            pass
        proc.wait()

buf.extend(read_available(master))
after = flags(termios.tcgetattr(slave))
os.close(master)
os.close(slave)

def slurp(path):
    try:
        with open(path, "r", encoding="utf-8", errors="replace") as f:
            return f.read()
    except OSError:
        return ""

f1 = slurp(path1)
f2 = slurp(path2)

text = bytes(buf).decode("utf-8", "replace")
host_fallback = "GNU Emacs" in text or "For information about GNU Emacs" in text
raw_seen = before.get("echo") and before.get("icanon") and (
    raw_observed or not mid.get("echo") or not mid.get("icanon")
)
restored = after.get("echo") and after.get("icanon")
saved = (marker1 in f1) and (marker2 in f2)
distinct = (marker2 not in f1) and (marker1 not in f2)
preserved = ("seed-one\n" in f1) and ("seed-two\n" in f2)

print("PTY_MBUF_RESULT rc=%s exited=%s raw_seen=%s restored=%s host_fallback=%s saved=%s distinct=%s preserved=%s" %
      (proc.returncode, exited, raw_seen, restored, host_fallback, saved, distinct, preserved))
print("PTY_MBUF_FILE1 " + f1.replace("\n", "\\n"))
print("PTY_MBUF_FILE2 " + f2.replace("\n", "\\n"))

if not restored:
    sys.exit(10)
if host_fallback:
    sys.exit(20)
if not raw_seen:
    sys.exit(30)
if not exited:
    sys.exit(40)
if not saved:
    sys.exit(50)
if not distinct:
    sys.exit(52)
if not preserved:
    sys.exit(51)
sys.exit(0)
PY
)"
  rc=$?
  set -e
  printf '%s\n' "$probe_out"
  rm -f "$file1" "$file2"

  if grep -q 'restored=True' <<<"$probe_out"; then
    note_pass "interactive pty multibuffer terminal restore"
  fi

  case "$rc" in
    0) note_pass "interactive pty multibuffer two-file find/edit/save/quit" ;;
    10) note_fail "interactive pty multibuffer left terminal without echo/canonical mode" ;;
    20) note_fail "interactive pty multibuffer reached host Emacs fallback; nelisp TUI ownership not proven" ;;
    30) note_todo "interactive pty multibuffer did not enter observable raw/cbreak mode yet" ;;
    40) note_todo "interactive pty multibuffer did not exit via C-x C-c yet" ;;
    50) note_todo "interactive pty multibuffer did not persist both buffers via C-x C-s yet" ;;
    52) note_todo "interactive pty multibuffer leaked a marker across buffers (buffers not distinct) yet" ;;
    51) note_todo "interactive pty multibuffer did not preserve both files' existing contents yet" ;;
    *) note_todo "interactive pty multibuffer probe ended in current unimplemented/unknown state (rc=$rc)" ;;
  esac
}

# Search/replace coverage: open a file holding several "alpha" tokens as a
# command-line argument, run query-replace (M-%) alpha -> BETA with "!"
# (replace all), save (C-x C-s) and quit (C-x C-c).  Verified through the
# file on disk: a real replace turns every "alpha" into "BETA".  Reports
# graceful TODO when the query-replace minibuffer path is not yet driveable
# over the pty, like the other interactive probes.
pty_replace_probe() {
  local launcher="$DIRECT_NEMACS"
  local file
  file="$(mktemp "${TMPDIR:-/tmp}/nemacs-tui-replace.XXXXXX.txt")"
  printf 'alpha alpha alpha\n' > "$file"

  if (( VIA_EMACS_WRAPPER )); then
    launcher="$(command -v emacs || true)"
    if [[ -z "$launcher" ]]; then
      note_fail "interactive pty replace: emacs wrapper not found on PATH"
      rm -f "$file"
      return
    fi
  fi

  local probe_out rc
  set +e
  probe_out="$(python3 - "$launcher" "$file" "$TIMEOUT" "$DRIVER" "$VIA_EMACS_WRAPPER" <<'PY'
import errno
import fcntl
import os
import pty
import select
import signal
import subprocess
import sys
import termios
import time

launcher, path, timeout_s, driver, via = sys.argv[1:]
timeout = float(timeout_s)
via_wrapper = via == "1"

def flags(attrs):
    lflag = attrs[3]
    return {
        "echo": bool(lflag & termios.ECHO),
        "icanon": bool(lflag & termios.ICANON),
    }

def read_available(fd):
    out = bytearray()
    while True:
        r, _, _ = select.select([fd], [], [], 0)
        if not r:
            break
        try:
            chunk = os.read(fd, 4096)
        except OSError as e:
            if e.errno in (errno.EIO, errno.EBADF):
                break
            raise
        if not chunk:
            break
        out.extend(chunk)
    return bytes(out)

master, slave = pty.openpty()
before = flags(termios.tcgetattr(slave))
env = os.environ.copy()
env["TERM"] = "xterm-256color"
env.setdefault("COLUMNS", "80")
env.setdefault("LINES", "24")
env["NEMACS_DRIVER"] = driver

if via_wrapper:
    cmd = [launcher, "-Q", "-nw", path]
else:
    cmd = [launcher, "--driver=" + driver, "-Q", "-nw", path]

def child_setup():
    os.setsid()
    try:
        fcntl.ioctl(slave, termios.TIOCSCTTY, 0)
    except OSError:
        pass

proc = subprocess.Popen(
    cmd,
    stdin=slave,
    stdout=slave,
    stderr=slave,
    cwd=os.path.dirname(os.path.dirname(launcher)) if not via_wrapper else None,
    env=env,
    close_fds=True,
    preexec_fn=child_setup,
)

started = time.time()
buf = bytearray()
settled = False
raw_observed = False
while time.time() - started < timeout:
    r, _, _ = select.select([master], [], [], 0.1)
    if r:
        try:
            chunk = os.read(master, 4096)
        except OSError:
            break
        if not chunk:
            break
        buf.extend(chunk)
        if b"alpha" in buf or b"\x1b[" in buf:
            settled = True
    cur = flags(termios.tcgetattr(slave))
    if before.get("echo") and before.get("icanon") and (
        not cur.get("echo") or not cur.get("icanon")
    ):
        raw_observed = True
        if settled:
            break
    if proc.poll() is not None:
        break

mid = flags(termios.tcgetattr(slave))

try:
    # M-x query-replace RET alpha RET BETA RET ! (replace all)
    os.write(master, b"\x1bx")
    time.sleep(min(timeout / 8.0, 1.5))
    os.write(master, b"query-replace\r")
    time.sleep(min(timeout / 6.0, 2.0))
    os.write(master, b"alpha\r")
    time.sleep(0.4)
    os.write(master, b"BETA\r")
    time.sleep(0.4)
    os.write(master, b"!")
    time.sleep(0.5)
    os.write(master, b"\x18\x13")   # C-x C-s
    time.sleep(1.0)
    os.write(master, b"\x18\x03")   # C-x C-c
except OSError:
    pass

try:
    proc.wait(timeout=timeout)
    exited = True
except subprocess.TimeoutExpired:
    exited = False
    try:
        os.killpg(proc.pid, signal.SIGTERM)
    except ProcessLookupError:
        pass
    try:
        proc.wait(timeout=2)
    except subprocess.TimeoutExpired:
        try:
            os.killpg(proc.pid, signal.SIGKILL)
        except ProcessLookupError:
            pass
        proc.wait()

buf.extend(read_available(master))
after = flags(termios.tcgetattr(slave))
os.close(master)
os.close(slave)

try:
    with open(path, "r", encoding="utf-8", errors="replace") as f:
        file_text = f.read()
except OSError:
    file_text = ""

text = bytes(buf).decode("utf-8", "replace")
host_fallback = "GNU Emacs" in text or "For information about GNU Emacs" in text
raw_seen = before.get("echo") and before.get("icanon") and (
    raw_observed or not mid.get("echo") or not mid.get("icanon")
)
restored = after.get("echo") and after.get("icanon")
replaced = ("BETA" in file_text) and ("alpha" not in file_text)

print("PTY_REPLACE_RESULT rc=%s exited=%s raw_seen=%s restored=%s host_fallback=%s replaced=%s" %
      (proc.returncode, exited, raw_seen, restored, host_fallback, replaced))
print("PTY_REPLACE_FILE " + file_text.replace("\n", "\\n"))

if not restored:
    sys.exit(10)
if host_fallback:
    sys.exit(20)
if not raw_seen:
    sys.exit(30)
if not exited:
    sys.exit(40)
if not replaced:
    sys.exit(50)
sys.exit(0)
PY
)"
  rc=$?
  set -e
  printf '%s\n' "$probe_out"
  rm -f "$file"

  if grep -q 'restored=True' <<<"$probe_out"; then
    note_pass "interactive pty replace terminal restore"
  fi

  case "$rc" in
    0) note_pass "interactive pty replace query-replace/save/quit" ;;
    10) note_fail "interactive pty replace left terminal without echo/canonical mode" ;;
    20) note_fail "interactive pty replace reached host Emacs fallback; nelisp TUI ownership not proven" ;;
    30) note_todo "interactive pty replace did not enter observable raw/cbreak mode yet" ;;
    40) note_todo "interactive pty replace did not exit via C-x C-c yet" ;;
    50) note_todo "interactive pty replace did not rewrite tokens via query-replace yet" ;;
    *) note_todo "interactive pty replace probe ended in current unimplemented/unknown state (rc=$rc)" ;;
  esac
}

# Generic interactive command probe: start -nw on an empty frame, settle,
# send KEYSEQ (python-escaped bytes, e.g. '\x08k\x06' for C-h k C-f), wait
# for EXPECT to render somewhere in the screen output, then abort/quit
# (C-g C-x C-c).  Covers Help / Dired / Info command families from a single
# parameterized path.  EXPECT may be empty to only assert the command ran
# without crashing or falling back to host Emacs.  Reports graceful TODO
# when the command path is not yet observable, consistent with the other
# interactive probes.
pty_command_probe() {
  local label="$1" keyseq="$2" expect="$3"
  local launcher="$DIRECT_NEMACS"
  if (( VIA_EMACS_WRAPPER )); then
    launcher="$(command -v emacs || true)"
    if [[ -z "$launcher" ]]; then
      note_fail "interactive pty $label: emacs wrapper not found on PATH"
      return
    fi
  fi

  local probe_out rc
  set +e
  probe_out="$(python3 - "$launcher" "$TIMEOUT" "$DRIVER" "$VIA_EMACS_WRAPPER" "$keyseq" "$expect" "$label" <<'PY'
import errno
import os
import fcntl
import pty
import select
import signal
import subprocess
import sys
import termios
import time

launcher, timeout_s, driver, via, keyseq_s, expect, label = sys.argv[1:]
timeout = float(timeout_s)
via_wrapper = via == "1"
keyseq = keyseq_s.encode("utf-8").decode("unicode_escape").encode("latin-1")

def flags(attrs):
    lflag = attrs[3]
    return {
        "echo": bool(lflag & termios.ECHO),
        "icanon": bool(lflag & termios.ICANON),
    }

def read_available(fd):
    out = bytearray()
    while True:
        r, _, _ = select.select([fd], [], [], 0)
        if not r:
            break
        try:
            chunk = os.read(fd, 4096)
        except OSError as e:
            if e.errno in (errno.EIO, errno.EBADF):
                break
            raise
        if not chunk:
            break
        out.extend(chunk)
    return bytes(out)

master, slave = pty.openpty()
before = flags(termios.tcgetattr(slave))
env = os.environ.copy()
env["TERM"] = "xterm-256color"
env.setdefault("COLUMNS", "80")
env.setdefault("LINES", "24")
env["NEMACS_DRIVER"] = driver

if via_wrapper:
    cmd = [launcher, "-Q", "-nw"]
else:
    cmd = [launcher, "--driver=" + driver, "-Q", "-nw"]

def child_setup():
    os.setsid()
    try:
        fcntl.ioctl(slave, termios.TIOCSCTTY, 0)
    except OSError:
        pass

proc = subprocess.Popen(
    cmd,
    stdin=slave,
    stdout=slave,
    stderr=slave,
    cwd=os.path.dirname(os.path.dirname(launcher)) if not via_wrapper else None,
    env=env,
    close_fds=True,
    preexec_fn=child_setup,
)

started = time.time()
buf = bytearray()
settled = False
raw_observed = False
while time.time() - started < timeout:
    r, _, _ = select.select([master], [], [], 0.1)
    if r:
        try:
            chunk = os.read(master, 4096)
        except OSError:
            break
        if not chunk:
            break
        buf.extend(chunk)
        if b"\x1b[" in buf or b"*scratch*" in buf:
            settled = True
    cur = flags(termios.tcgetattr(slave))
    if before.get("echo") and before.get("icanon") and (
        not cur.get("echo") or not cur.get("icanon")
    ):
        raw_observed = True
        if settled:
            break
    if proc.poll() is not None:
        break

mid = flags(termios.tcgetattr(slave))

def pump_until(predicate, seconds):
    deadline = time.time() + seconds
    while time.time() < deadline:
        r, _, _ = select.select([master], [], [], 0.05)
        if r:
            try:
                chunk = os.read(master, 4096)
            except OSError:
                return False
            if not chunk:
                return False
            buf.extend(chunk)
            if predicate(bytes(buf)):
                return True
        if proc.poll() is not None:
            return False
    return predicate(bytes(buf))

found = False
try:
    os.write(master, keyseq)
    if expect:
        found = pump_until(
            lambda data: expect.encode("utf-8", "replace") in data,
            min(timeout / 2.0, 20.0),
        )
    else:
        time.sleep(min(timeout / 6.0, 2.0))
        found = True
    time.sleep(0.3)
    os.write(master, b"\x07")        # C-g  (abort any open minibuffer)
    time.sleep(0.2)
    os.write(master, b"\x18\x03")    # C-x C-c
except OSError:
    pass

try:
    proc.wait(timeout=timeout)
    exited = True
except subprocess.TimeoutExpired:
    exited = False
    try:
        os.killpg(proc.pid, signal.SIGTERM)
    except ProcessLookupError:
        pass
    try:
        proc.wait(timeout=2)
    except subprocess.TimeoutExpired:
        try:
            os.killpg(proc.pid, signal.SIGKILL)
        except ProcessLookupError:
            pass
        proc.wait()

buf.extend(read_available(master))
after = flags(termios.tcgetattr(slave))
os.close(master)
os.close(slave)

text = bytes(buf).decode("utf-8", "replace")
host_fallback = "GNU Emacs" in text or "For information about GNU Emacs" in text
raw_seen = before.get("echo") and before.get("icanon") and (
    raw_observed or not mid.get("echo") or not mid.get("icanon")
)
restored = after.get("echo") and after.get("icanon")

print("PTY_CMD_RESULT label=%s rc=%s exited=%s raw_seen=%s restored=%s host_fallback=%s found=%s" %
      (label, proc.returncode, exited, raw_seen, restored, host_fallback, found))
sample = text.replace("\r", "\\r").replace("\n", "\\n")
if len(sample) > 400:
    sample = sample[:400] + "..."
print("PTY_CMD_SAMPLE " + sample)

if not restored:
    sys.exit(10)
if host_fallback:
    sys.exit(20)
if not raw_seen:
    sys.exit(30)
if not found:
    sys.exit(60)
sys.exit(0)
PY
)"
  rc=$?
  set -e
  printf '%s\n' "$probe_out"

  if grep -q 'restored=True' <<<"$probe_out"; then
    note_pass "interactive pty $label terminal restore"
  fi

  case "$rc" in
    0) note_pass "interactive pty $label command path" ;;
    10) note_fail "interactive pty $label left terminal without echo/canonical mode" ;;
    20) note_fail "interactive pty $label reached host Emacs fallback; nelisp TUI ownership not proven" ;;
    30) note_todo "interactive pty $label did not enter observable raw/cbreak mode yet" ;;
    60) note_todo "interactive pty $label did not render expected marker ('$expect') yet" ;;
    *) note_todo "interactive pty $label probe ended in current unimplemented/unknown state (rc=$rc)" ;;
  esac
}

echo "nemacs TUI smoke"
echo "  repo: $REPO_ROOT"
echo "  driver: $DRIVER"
if (( VIA_EMACS_WRAPPER )); then
  echo "  wrapper: $(command -v emacs || printf '<missing>')"
else
  echo "  wrapper: $DIRECT_NEMACS"
fi

batch_direct
batch_wrapper
pty_probe graceful
pty_probe sigterm
pty_fileio_probe
pty_multibuffer_probe
pty_replace_probe
pty_command_probe switch-buffer '\x18b*scratch*\r' '*scratch*'
pty_command_probe m-x-shell-command '\x1bxshell-command\rprintf NEMACS_MX_SHELL_OK\r' 'NEMACS_MX_SHELL_OK'
pty_command_probe help-describe-key '\x08k\x06' 'forward-char'
pty_command_probe dired "\\x1bxdired\\r${REPO_ROOT}/\\r" 'Makefile'
pty_command_probe info '\x1bxInfo-directory\r' 'Info Directory'
pty_command_probe shell-command '\x1b!printf NEMACS_SHELL_OK\r' 'NEMACS_SHELL_OK'

daily_dired_dir="$(mktemp -d "${TMPDIR:-/tmp}/nemacs-tui-dired.XXXXXX")"
printf 'dired real workflow\n' > "$daily_dired_dir/real-file.txt"
pty_command_probe dired-real-directory "\\x1bxdired\\r${daily_dired_dir}/\\r\\x18b*Dired*\\r" 'real-file.txt'
rm -rf "$daily_dired_dir"

daily_info_file="$(mktemp "${TMPDIR:-/tmp}/nemacs-tui-info.XXXXXX.info")"
printf '\037\nFile: sample.info,  Node: Top,  Next: Second,\nTop node marker\n\037\nFile: sample.info,  Node: Second,  Prev: Top,  Up: Top,\nSecond node marker NEMACS_INFO_SECOND\n' > "$daily_info_file"
pty_command_probe info-node-navigation "\\x1bxinfo\\r${daily_info_file}\\r\\x1bxInfo-next\\r" 'NEMACS_INFO_SECOND'
rm -f "$daily_info_file"

pty_command_probe help-buffer-navigation '\x08k\x06\x18b*Help*\r' 'forward-char'
pty_command_probe shell-output-buffer '\x1b!printf NEMACS_SHELL_BUFFER_OK\r\x18b*Shell Output*\r' 'NEMACS_SHELL_BUFFER_OK'

echo "----"
echo "nemacs TUI smoke: PASS=$pass FAIL=$fail TODO=$todo"
if (( fail > 0 )); then
  exit 1
fi
if (( todo > 0 )); then
  exit 2
fi
exit 0
