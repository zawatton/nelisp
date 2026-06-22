#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
GUI_ROOT=$(CDPATH= cd -- "$SCRIPT_DIR/.." && pwd)
SNAP=${NELISP_SNAP:-/tmp/nelisp-snap}

usage() {
  cat <<'EOF'
Usage: scripts/sync-nelisp-snap.sh

Builds the nelisp standalone reader and copies the executable, lisp/, and src/
into $NELISP_SNAP (default: /tmp/nelisp-snap).

Environment:
  NELISP_ROOT  nelisp checkout; defaults to sibling/home development paths
  NELISP_SNAP  snapshot destination used by bin/nemacs and nemacs-build.sh
EOF
}

case "${1:-}" in
  -h|--help|help)
    usage
    exit 0
    ;;
esac

require_cmd() {
  if ! command -v "$1" >/dev/null 2>&1; then
    printf 'sync-nelisp-snap: missing required command: %s\n' "$1" >&2
    exit 1
  fi
}

for cmd in make cp rm mkdir chmod readlink; do
  require_cmd "$cmd"
done
if [ ! -x /usr/bin/emacs ]; then
  printf 'sync-nelisp-snap: /usr/bin/emacs is required\n' >&2
  exit 1
fi

find_nelisp_root() {
  if [ "${NELISP_ROOT:-}" ]; then
    (cd "$NELISP_ROOT" && pwd -P)
    return
  fi

  for cand in \
    "$GUI_ROOT/../nelisp" \
    "$HOME/Notes/dev/nelisp" \
    "$HOME/Cowork/Notes/dev/nelisp"
  do
    if [ -f "$cand/Makefile" ] && [ -f "$cand/lisp/nelisp-aot-compiler.el" ]; then
      (cd "$cand" && pwd -P)
      return
    fi
  done

  printf 'sync-nelisp-snap: set NELISP_ROOT to the nelisp repository\n' >&2
  exit 1
}

ROOT=$(find_nelisp_root)

stop_exact_exe() {
  exe=$(readlink -f "$1")
  for pid in $(pgrep -f nelisp 2>/dev/null || true); do
    if [ "$(readlink -f "/proc/$pid/exe" 2>/dev/null || true)" = "$exe" ]; then
      kill "$pid" 2>/dev/null || true
    fi
  done
  sleep 0.2
  for pid in $(pgrep -f nelisp 2>/dev/null || true); do
    if [ "$(readlink -f "/proc/$pid/exe" 2>/dev/null || true)" = "$exe" ]; then
      kill -9 "$pid" 2>/dev/null || true
    fi
  done
}

stop_exact_exe "$ROOT/target/nelisp"
if [ -x "$SNAP/nelisp" ]; then
  stop_exact_exe "$SNAP/nelisp"
fi

make -C "$ROOT" EMACS=/usr/bin/emacs standalone-reader

mkdir -p "$SNAP/lisp" "$SNAP/src"
rm -rf "$SNAP/lisp" "$SNAP/src"
mkdir -p "$SNAP/lisp" "$SNAP/src"

cp "$ROOT/target/nelisp" "$SNAP/nelisp"
cp -a "$ROOT/lisp/." "$SNAP/lisp/"
cp -a "$ROOT/src/." "$SNAP/src/"
chmod +x "$SNAP/nelisp"

printf 'synced %s -> %s\n' "$ROOT" "$SNAP"
