#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
GUI_ROOT=$(CDPATH= cd -- "$SCRIPT_DIR/.." && pwd)
NEMACS_EMACS_ROOT=${NEMACS_EMACS_ROOT:-$GUI_ROOT/../nelisp-emacs}
NEMACS_RUNTIME_IMAGE=${NEMACS_RUNTIME_IMAGE:-$NEMACS_EMACS_ROOT/build/nemacs-gui-file-bridge.nlri}
NEMACS_SESSION_STRESS_COUNT=${NEMACS_SESSION_STRESS_COUNT:-120}
NEMACS_SESSION_STRESS_TIMEOUT=${NEMACS_SESSION_STRESS_TIMEOUT:-8}
NEMACS_TRANSPORT_LOCK=${NEMACS_TRANSPORT_LOCK:-/tmp/nemacs-transport.lock}
NEMACS_TRANSPORT_LOCK_WAIT_SECONDS=${NEMACS_TRANSPORT_LOCK_WAIT_SECONDS:-300}

transport_dir=/tmp
lock_held=0
request_index=0
session_pids=""

usage() {
  cat <<EOF
Usage: $0 [count]

Run a fixed-/tmp persistent session bridge stress test. COUNT must be 100..500
and defaults to NEMACS_SESSION_STRESS_COUNT (${NEMACS_SESSION_STRESS_COUNT}).

This is intentionally separate from scripts/verify-nemacs-gui.sh. It holds
${NEMACS_TRANSPORT_LOCK}, mutates /tmp/nemacs-*, runs many bridge requests,
injects an invalid key, exercises minibuffer C-g, restarts the session, and
checks that the session processes it spawned do not survive cleanup.

Typical runtime on a warm runtime image: 10-60 seconds for 120 commands,
roughly 1-4 minutes for 500 commands depending on the NeLisp binary.
EOF
}

case "${1:-}" in
  -h|--help)
    usage
    exit 0
    ;;
  "")
    ;;
  *[!0-9]*)
    usage >&2
    exit 2
    ;;
  *)
    NEMACS_SESSION_STRESS_COUNT=$1
    ;;
esac

if [ "$NEMACS_SESSION_STRESS_COUNT" -lt 100 ] ||
   [ "$NEMACS_SESSION_STRESS_COUNT" -gt 500 ]; then
  echo "count must be between 100 and 500: $NEMACS_SESSION_STRESS_COUNT" >&2
  exit 2
fi

cd "$GUI_ROOT"

cleanup_transport_lock() {
  if [ "$lock_held" = "1" ]; then
    rm -rf "$NEMACS_TRANSPORT_LOCK"
  fi
}

transport_lock_stale_p() {
  lock_pid=$(cat "$NEMACS_TRANSPORT_LOCK/pid" 2>/dev/null || true)
  [ "$lock_pid" ] || return 0
  kill -0 "$lock_pid" 2>/dev/null && return 1
  return 0
}

acquire_transport_lock() {
  waited=0
  while ! mkdir "$NEMACS_TRANSPORT_LOCK" 2>/dev/null; do
    if transport_lock_stale_p; then
      rm -rf "$NEMACS_TRANSPORT_LOCK"
      continue
    fi
    if [ "$waited" -ge "$NEMACS_TRANSPORT_LOCK_WAIT_SECONDS" ]; then
      echo "timed out waiting for $NEMACS_TRANSPORT_LOCK" >&2
      exit 1
    fi
    sleep 1
    waited=$((waited + 1))
  done
  lock_held=1
  printf '%s\n' "$$" >"$NEMACS_TRANSPORT_LOCK/pid"
}

session_pid() {
  cat "$transport_dir/nemacs-session-pid" 2>/dev/null || true
}

remember_session_pid() {
  pid=$(session_pid)
  if [ "$pid" ]; then
    case " $session_pids " in
      *" $pid "*) ;;
      *) session_pids="${session_pids}${session_pids:+ }$pid" ;;
    esac
  fi
}

shutdown_session() {
  printf '1' >"$transport_dir/nemacs-session-shutdown" 2>/dev/null || true
  if [ -p "$transport_dir/nemacs-session-request" ]; then
    timeout 1 sh -c "printf shutdown >'$transport_dir/nemacs-session-request'" >/dev/null 2>&1 || true
  fi
  pid=$(session_pid)
  tries=0
  while [ "$tries" -lt 300 ]; do
    if [ "$(cat "$transport_dir/nemacs-session-ready" 2>/dev/null || true)" = "0" ]; then
      break
    fi
    if [ "$pid" ] && ! kill -0 "$pid" 2>/dev/null; then
      break
    fi
    tries=$((tries + 1))
    sleep 0.01
  done
  if [ "$pid" ] && kill -0 "$pid" 2>/dev/null; then
    kill "$pid" 2>/dev/null || true
    for _ in $(seq 1 100); do
      kill -0 "$pid" 2>/dev/null || break
      sleep 0.01
    done
  fi
}

cleanup() {
  shutdown_session
  cleanup_transport_lock
}
trap cleanup EXIT

assert_no_spawned_orphans() {
  for pid in $session_pids; do
    if kill -0 "$pid" 2>/dev/null; then
      echo "orphan nemacs session process still alive: $pid" >&2
      return 1
    fi
  done
}

reset_transport() {
  shutdown_session
  rm -f \
    "$transport_dir/nemacs-cmd" \
    "$transport_dir/nemacs-keys" \
    "$transport_dir/nemacs-arg" \
    "$transport_dir/nemacs-minibuffer-text" \
    "$transport_dir/nemacs-minibuffer-arg" \
    "$transport_dir/nemacs-status" \
    "$transport_dir/nemacs-session-pid" \
    "$transport_dir/nemacs-session-ready" \
    "$transport_dir/nemacs-session-request" \
    "$transport_dir/nemacs-session-response" \
    "$transport_dir/nemacs-session-shutdown" \
    "$transport_dir/nemacs-session.out" \
    "$transport_dir/nemacs-session.err"
  rm -rf \
    "$transport_dir/nemacs-buffer-store" \
    "$transport_dir/nemacs-buffer-file-store" \
    "$transport_dir/nemacs-buffer-point-store" \
    "$transport_dir/nemacs-buffer-mark-store" \
    "$transport_dir/nemacs-buffer-window-start-store" \
    "$transport_dir/nemacs-buffer-read-only-store" \
    "$transport_dir/nemacs-buffer-narrow-active-store" \
    "$transport_dir/nemacs-buffer-narrow-start-store" \
    "$transport_dir/nemacs-buffer-narrow-end-store" \
    "$transport_dir/nemacs-buffer-narrow-full-store" \
    "$transport_dir/nemacs-register-store" \
    "$transport_dir/nemacs-bookmark-store"
  mkdir -p \
    "$transport_dir/nemacs-buffer-store" \
    "$transport_dir/nemacs-buffer-file-store" \
    "$transport_dir/nemacs-buffer-point-store" \
    "$transport_dir/nemacs-buffer-mark-store" \
    "$transport_dir/nemacs-buffer-window-start-store" \
    "$transport_dir/nemacs-buffer-read-only-store" \
    "$transport_dir/nemacs-buffer-narrow-active-store" \
    "$transport_dir/nemacs-buffer-narrow-start-store" \
    "$transport_dir/nemacs-buffer-narrow-end-store" \
    "$transport_dir/nemacs-buffer-narrow-full-store" \
    "$transport_dir/nemacs-register-store" \
    "$transport_dir/nemacs-bookmark-store"

  : >"$transport_dir/nemacs-cmd"
  : >"$transport_dir/nemacs-keys"
  : >"$transport_dir/nemacs-arg"
  : >"$transport_dir/nemacs-minibuffer-text"
  : >"$transport_dir/nemacs-minibuffer-arg"
  printf 'stress\nline two\nline three\n' >"$transport_dir/nemacs-buf"
  printf '0' >"$transport_dir/nemacs-point"
  printf '0' >"$transport_dir/nemacs-mark"
  printf '0' >"$transport_dir/nemacs-read-only"
  printf '0' >"$transport_dir/nemacs-exit"
  printf '0' >"$transport_dir/nemacs-window-start"
  printf '0' >"$transport_dir/nemacs-window-hscroll"
  printf '0' >"$transport_dir/nemacs-window-split-delta"
  printf 'single' >"$transport_dir/nemacs-window-layout"
  printf '0' >"$transport_dir/nemacs-window-selected"
  printf '0\t1\t1' >"$transport_dir/nemacs-tab-state"
  printf 'main' >"$transport_dir/nemacs-buffer-name"
  printf 'main\n' >"$transport_dir/nemacs-buffer-list"
  : >"$transport_dir/nemacs-file"
  : >"$transport_dir/nemacs-modeline"
  : >"$transport_dir/nemacs-cursor"
  : >"$transport_dir/nemacs-kill"
  : >"$transport_dir/nemacs-kill-ring"
  printf '0' >"$transport_dir/nemacs-kill-ring-index"
  : >"$transport_dir/nemacs-rectangle-kill"
  printf '0' >"$transport_dir/nemacs-rectangle-mark-mode"
  printf '0' >"$transport_dir/nemacs-undo-ready"
  : >"$transport_dir/nemacs-undo-buf"
  printf '0' >"$transport_dir/nemacs-undo-point"
  printf '0' >"$transport_dir/nemacs-undo-mark"
  : >"$transport_dir/nemacs-last-command"
  : >"$transport_dir/nemacs-prefix-arg"
  : >"$transport_dir/nemacs-goal-column"
  : >"$transport_dir/nemacs-global-mark"
  printf '0' >"$transport_dir/nemacs-truncate-lines"
  printf '0' >"$transport_dir/nemacs-kmacro-recording"
  : >"$transport_dir/nemacs-kmacro-keys"
  printf '0' >"$transport_dir/nemacs-minibuffer-active"
  : >"$transport_dir/nemacs-minibuffer-prompt"
  : >"$transport_dir/nemacs-minibuffer-state"
  : >"$transport_dir/nemacs-minibuffer-purpose"
  printf '0' >"$transport_dir/nemacs-minibuffer-cursor"
  : >"$transport_dir/nemacs-minibuffer-candidates"
  : >"$transport_dir/nemacs-minibuffer-history"
  printf '0' >"$transport_dir/nemacs-minibuffer-require-match"
  printf '0' >"$transport_dir/nemacs-session-shutdown"
  printf 'main\n' >"$transport_dir/nemacs-buffer-store/main"
  : >"$transport_dir/nemacs-buffer-file-store/main"
  printf '0' >"$transport_dir/nemacs-buffer-point-store/main"
  printf '0' >"$transport_dir/nemacs-buffer-mark-store/main"
  printf '0' >"$transport_dir/nemacs-buffer-window-start-store/main"
  printf '0' >"$transport_dir/nemacs-buffer-read-only-store/main"
  : >"$transport_dir/nemacs-bookmark-list"
}

session_alive() {
  pid=$(session_pid)
  [ "$pid" ] &&
    kill -0 "$pid" 2>/dev/null &&
    [ "$(cat "$transport_dir/nemacs-session-ready" 2>/dev/null || true)" = "1" ]
}

run_key_expect() {
  key=$1
  expected=$2
  printf 'stale-direct-command' >"$transport_dir/nemacs-cmd"
  printf '%s' "$key" >"$transport_dir/nemacs-keys"
  set +e
  timeout "$NEMACS_SESSION_STRESS_TIMEOUT" env \
    NEMACS_TRANSPORT_DIR="$transport_dir" \
    NEMACS_BRIDGE_BACKEND=session \
    NEMACS_RUNTIME_IMAGE="$NEMACS_RUNTIME_IMAGE" \
    NEMACS_EMACS_ROOT="$NEMACS_EMACS_ROOT" \
    ./nemacs-mx.sh >"$transport_dir/nemacs-stress-mx.out" 2>"$transport_dir/nemacs-stress-mx.err"
  rc=$?
  set -e
  if [ "$rc" != "$expected" ]; then
    echo "unexpected rc for key '$key': got $rc expected $expected" >&2
    cat "$transport_dir/nemacs-stress-mx.err" >&2 || true
    exit 1
  fi
  request_index=$((request_index + 1))
  remember_session_pid
  session_alive
  [ ! -s "$transport_dir/nemacs-cmd" ]
}

run_key() {
  run_key_expect "$1" 0
}

run_invalid_key() {
  run_key_expect 'C-x 9' 0
  [ "$(cat "$transport_dir/nemacs-status" 2>/dev/null || true)" = "unsupported" ]
  session_alive
}

run_cancelled_minibuffer() {
  run_key 'M-x'
  grep -qx '1' "$transport_dir/nemacs-minibuffer-active"
  run_key 'n'
  run_key 'o'
  run_key 't'
  run_key 'C-g'
  grep -qx '0' "$transport_dir/nemacs-minibuffer-active"
  session_alive
}

key_for_index() {
  case $(( $1 % 12 )) in
    0) printf 'C-f' ;;
    1) printf 'C-b' ;;
    2) printf 'C-e' ;;
    3) printf 'C-a' ;;
    4) printf 'C-n' ;;
    5) printf 'C-p' ;;
    6) printf 'C-l' ;;
    7) printf 'M-<' ;;
    8) printf 'M->' ;;
    9) printf 'C-v' ;;
    10) printf 'M-v' ;;
    *) printf 'C-g' ;;
  esac
}

if [ ! -f "$NEMACS_RUNTIME_IMAGE" ]; then
  make -C "$NEMACS_EMACS_ROOT" build/nemacs-gui-file-bridge.nlri
fi

acquire_transport_lock
reset_transport

invalid_at=$((NEMACS_SESSION_STRESS_COUNT / 3))
cancel_at=$((NEMACS_SESSION_STRESS_COUNT * 2 / 3))

for i in $(seq 1 "$NEMACS_SESSION_STRESS_COUNT"); do
  if [ "$i" -eq "$invalid_at" ]; then
    run_invalid_key
  fi
  if [ "$i" -eq "$cancel_at" ]; then
    run_cancelled_minibuffer
  fi
  run_key "$(key_for_index "$i")"
done

first_pid=$(session_pid)
[ "$first_pid" ]
kill -0 "$first_pid"
session_alive

shutdown_session
grep -qx '0' "$transport_dir/nemacs-session-ready"
if kill -0 "$first_pid" 2>/dev/null; then
  echo "first session pid survived shutdown: $first_pid" >&2
  exit 1
fi

printf '0' >"$transport_dir/nemacs-session-shutdown"
run_key 'C-f'
second_pid=$(session_pid)
[ "$second_pid" ]
kill -0 "$second_pid"
[ "$second_pid" != "$first_pid" ]

shutdown_session
grep -qx '0' "$transport_dir/nemacs-session-ready"
if kill -0 "$second_pid" 2>/dev/null; then
  echo "second session pid survived shutdown: $second_pid" >&2
  exit 1
fi
assert_no_spawned_orphans

echo "session-stress-ok count=$NEMACS_SESSION_STRESS_COUNT requests=$request_index first_pid=$first_pid second_pid=$second_pid transport=$transport_dir"
