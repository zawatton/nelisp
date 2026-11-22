#!/usr/bin/env bash
# nl-dev-session.sh: one resident NeLisp runtime-image process, probes cost ms.
# usage: nl-dev-session.sh start | eval FORM-FILE | eval-string 'FORM...' | load-el FILE.el [real] |
#        load-neln FILE.neln SYM... | status | log | stop
# env: NLDS_BASE  dir holding target/nelisp + build/nemacs-runtime.nlri (read-only base)
#      NLDS_DIR   session state dir (default $XDG_CACHE_HOME/nl-dev-session)
#      NLDS_CPUS  taskset list (default none; e.g. 22,23)   NLDS_TIMEOUT per-eval seconds (default 120)
#      NLDS_START_TIMEOUT image start budget seconds (default 300)   NLDS_FORCE=1 skip timing guard
set -u
export LC_ALL=C
HERE=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
LANE=$(cd "$HERE/../.." && pwd)   # the lane/repo this script lives in (its lisp/ is put on load-path)
BASE=${NLDS_BASE:-$LANE}
DIR=${NLDS_DIR:-${XDG_CACHE_HOME:-$HOME/.cache}/nl-dev-session}
TMO=${NLDS_TIMEOUT:-120}
STMO=${NLDS_START_TIMEOUT:-300}
LOG=$DIR/session.log
pid() { cat "$DIR/pid" 2>/dev/null; }
alive() { local p; p=$(pid); [ -n "$p" ] && kill -0 "$p" 2>/dev/null; }
cmd=${1:-}; shift || true
case $cmd in
start)
  alive && { echo "already running pid $(pid)"; exit 0; }
  mkdir -p "$DIR"; rm -f "$DIR"/req.el "$DIR"/cur.el "$DIR"/done "$DIR"/stop
  : > "$LOG"
  pin=(); [ -n "${NLDS_CPUS:-}" ] && pin=(taskset -c "$NLDS_CPUS")
  lp="\"$LANE/lisp\""; roots="\"$LANE/lisp/\""
  for d in ${NLDS_LOAD_PATH:-}; do lp="$lp \"$d\""; roots="$roots \"$d/\""; done   # extra dirs (space separated), fast-loaded too
  form="(progn (load \"$HERE/nl-dev-session.el\" nil t t) (setq load-path (append (list $lp) load-path)) $( [ "${NLDS_FAST:-1}" = 1 ] && echo "(setq nl-dev-fast-roots (list $roots)) (nl-dev-fast-load-enable)") (nl-dev-session-run \"$DIR\"))"
  ( cd "$BASE" && NELISP_HOME="$BASE" setsid bash -c 'ulimit -v 8388608; exec "$@"' _ \
      "${pin[@]}" "$BASE/target/nelisp" exec-runtime-image "$BASE/build/nemacs-runtime.nlri" "$form" \
      < /dev/null > "$LOG" 2>&1 & echo $! > "$DIR/pid" )
  t0=$(date +%s)
  until grep -q NLDS-READY "$LOG" 2>/dev/null; do
    alive || { echo "session died during start:"; tail -5 "$LOG"; exit 1; }
    [ $(( $(date +%s) - t0 )) -gt "$STMO" ] && { kill "$(pid)" 2>/dev/null; echo "start timeout"; exit 2; }
    sleep 0.5
  done
  echo "ready pid $(pid) in $(( $(date +%s) - t0 )) s" ;;
eval|eval-string|load-el|load-neln)
  alive || { echo "no session (run start)"; exit 1; }
  req=$DIR/req.tmp
  case $cmd in
    eval) [ -f "${1:-}" ] || { echo "no FORM-FILE"; exit 1; }; cp "$1" "$req" ;;
    eval-string) printf '%s\n' "$1" > "$req" ;;
    load-el) f=$(realpath "$1"); printf '(nl-dev-load-el "%s"%s)\n' "$f" "${2:+ t}" > "$req" ;;
    load-neln) f=$(realpath "$1"); shift
      syms=""; for s in "$@"; do syms="$syms \"$s\""; done
      printf '(nl-dev-load-neln "%s"%s)\n' "$f" "$syms" > "$req" ;;
  esac
  # in-load trap guard: a request that loads AND times is refused; split it in two requests
  if [ "${NLDS_FORCE:-0}" != 1 ] && grep -q '(load[ -]' "$req" && grep -q 'float-time\|current-time\|nl-dev-time-call' "$req"; then
    rm -f "$req"; echo "REFUSED: request has (load ...) and timing code; timing under an active load stays interpreted (F95). Send the load and the timed form as separate requests (or NLDS_FORCE=1)." >&2; exit 3
  fi
  off=$(stat -c %s "$LOG"); rm -f "$DIR/done"
  mv "$req" "$DIR/req.el"
  t0=$((10#${EPOCHREALTIME/./})); lim=$(( ${TMO%.*} * 1000000 ))
  until [ -e "$DIR/done" ]; do
    alive || { echo "SESSION DIED"; tail -c +$((off+1)) "$LOG"; exit 1; }
    if [ $(( 10#${EPOCHREALTIME/./} - t0 )) -gt "$lim" ]; then
      kill "$(pid)" 2>/dev/null; rm -f "$DIR/pid"
      echo "TIMEOUT after ${TMO}s: session killed (restart with start)"; tail -c +$((off+1)) "$LOG"; exit 4
    fi
    sleep 0.01
  done
  tail -c +$((off+1)) "$LOG" | grep -v '^NLDS-END$'
  ;;
status) alive && echo "running pid $(pid) rss_kb=$(awk '/VmRSS/{print $2}' /proc/$(pid)/status)" || echo "not running" ;;
log) cat "$LOG" ;;
stop)
  if alive; then
    p=$(pid); touch "$DIR/stop"
    for _ in $(seq 1 50); do kill -0 "$p" 2>/dev/null || break; sleep 0.1; done
    kill -0 "$p" 2>/dev/null && kill "$p"
    echo stopped
  else echo "not running"; fi
  rm -f "$DIR/pid" "$DIR/stop" ;;
*) sed -n 2,9p "$0"; exit 1 ;;
esac
