#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
GUI_ROOT=$(CDPATH= cd -- "$SCRIPT_DIR/.." && pwd)

usage() {
  cat <<'EOF'
Usage: scripts/doctor-workspace.sh

Checks the local nelisp-gui launch/install prerequisites without starting the
GUI or mutating /tmp/nemacs-* transport state.

Environment:
  NELISP_ROOT             nelisp checkout, defaults to ../nelisp
  NELISP_SNAP             runtime snapshot, defaults to /tmp/nelisp-snap
  NEMACS_EMACS_ROOT       nelisp-emacs checkout, defaults to ../nelisp-emacs
  NEMACS_TRANSPORT_DIR    transport directory, defaults to /tmp
  NEMACS_ARTIFACT_DIR     native GUI cfg/bin/marker directory
  NEMACS_BRIDGE_BACKEND   session, nelisp, or auto
  NEMACS_USER_BIN         install destination, defaults to ~/.local/bin
EOF
}

case "${1:-}" in
  -h|--help|help)
    usage
    exit 0
    ;;
esac

find_repo() {
  local env_value=$1
  local fallback=$2
  local marker=$3
  if [ -n "$env_value" ]; then
    if [ -e "$env_value/$marker" ]; then
      (cd "$env_value" && pwd -P)
      return
    fi
    printf 'missing explicit repo: %s (expected %s)\n' "$env_value" "$marker" >&2
    return 1
  fi
  if [ -e "$fallback/$marker" ]; then
    (cd "$fallback" && pwd -P)
    return
  fi
  printf 'missing sibling repo: %s (expected %s)\n' "$fallback" "$marker" >&2
  return 1
}

check_cmd() {
  local name=$1
  if command -v "$name" >/dev/null 2>&1; then
    printf '  ok:   %-10s %s\n' "$name" "$(command -v "$name")"
  else
    printf '  fail: %-10s missing\n' "$name"
    ok=0
  fi
}

check_optional_cmd() {
  local name=$1
  local reason=$2
  if command -v "$name" >/dev/null 2>&1; then
    printf '  ok:   %-10s %s\n' "$name" "$(command -v "$name")"
  else
    printf '  warn: %-10s missing (%s)\n' "$name" "$reason"
  fi
}

check_link_status() {
  local name=$1
  local src="$GUI_ROOT/bin/$name"
  local dst="$USER_BIN/$name"

  if [ "$(readlink "$dst" 2>/dev/null || true)" = "$src" ]; then
    printf '  ok:   %s -> %s\n' "$dst" "$src"
  elif [ -e "$dst" ] || [ -L "$dst" ]; then
    printf '  warn: %s exists but is not the nelisp-gui launcher\n' "$dst"
  else
    printf '  note: %s is not installed\n' "$dst"
  fi
}

ok=1
NELISP_ROOT_RESOLVED=$(find_repo "${NELISP_ROOT:-}" "$GUI_ROOT/../nelisp" "lisp/nelisp-aot-compiler.el") || ok=0
EMACS_ROOT_RESOLVED=$(find_repo "${NEMACS_EMACS_ROOT:-}" "$GUI_ROOT/../nelisp-emacs" "src/files.el") || ok=0
NELISP_SNAP_RESOLVED=${NELISP_SNAP:-/tmp/nelisp-snap}
TRANSPORT_DIR=${NEMACS_TRANSPORT_DIR:-/tmp}
DEFAULT_TMP=${TMPDIR:-/tmp}
mkdir -p "$DEFAULT_TMP"
DEFAULT_TMP=$(CDPATH= cd -- "$DEFAULT_TMP" && pwd)
if [ -n "${NEMACS_ARTIFACT_DIR:-}" ]; then
  ARTIFACT_DIR=$NEMACS_ARTIFACT_DIR
elif [ "$TRANSPORT_DIR" = "$DEFAULT_TMP" ] || [ "$TRANSPORT_DIR" = "/tmp" ]; then
  ARTIFACT_DIR=$DEFAULT_TMP
else
  ARTIFACT_DIR="$TRANSPORT_DIR/.nemacs-artifacts"
fi
CONFIG_PATH=${NEMACS_CONFIG_PATH:-$ARTIFACT_DIR/nemacs.cfg}
NATIVE_BIN=${NEMACS_NATIVE_BIN:-$ARTIFACT_DIR/nemacs-win.bin}
NATIVE_TRANSPORT_FILE=${NEMACS_NATIVE_TRANSPORT_FILE:-$ARTIFACT_DIR/nemacs-win.transport-dir}
NATIVE_CONFIG_FILE=${NEMACS_NATIVE_CONFIG_FILE:-$ARTIFACT_DIR/nemacs-win.config-path}
BACKEND=${NEMACS_BRIDGE_BACKEND:-session}
USER_BIN=${NEMACS_USER_BIN:-$HOME/.local/bin}

printf 'nelisp-gui workspace\n'
printf '  gui root:          %s\n' "$GUI_ROOT"
printf '  nelisp root:       %s\n' "${NELISP_ROOT_RESOLVED:-missing}"
printf '  nelisp-emacs root: %s\n' "${EMACS_ROOT_RESOLVED:-missing}"
printf '  nelisp snap:       %s\n' "$NELISP_SNAP_RESOLVED"
printf '  transport dir:     %s\n' "$TRANSPORT_DIR"
printf '  artifact dir:      %s\n' "$ARTIFACT_DIR"
printf '  bridge backend:    %s\n' "$BACKEND"
printf '  user bin:          %s\n' "$USER_BIN"

if [ "$ok" -ne 1 ]; then
  printf '\nfix the missing checkout or set NELISP_ROOT / NEMACS_EMACS_ROOT.\n' >&2
  exit 1
fi

printf '\nrequired commands\n'
for cmd in make python3 awk sed grep tr cp rm mkdir chmod readlink; do
  check_cmd "$cmd"
done

printf '\noptional commands\n'
check_optional_cmd timeout "nemacs-build smoke timeout"
check_optional_cmd setsid "background GUI smoke"
check_optional_cmd pgrep "stale nelisp/session cleanup"
check_optional_cmd xwininfo "visual smoke diagnostics"
check_optional_cmd xdotool "interactive X11 smoke helpers"

printf '\nworkspace checks\n'
if [ -x "$NELISP_ROOT_RESOLVED/target/nelisp" ]; then
  printf '  ok: %s\n' "$NELISP_ROOT_RESOLVED/target/nelisp"
else
  printf '  warn: %s is missing; run scripts/sync-nelisp-snap.sh or make -C %s standalone-reader\n' \
    "$NELISP_ROOT_RESOLVED/target/nelisp" "$NELISP_ROOT_RESOLVED"
fi

if [ -x "$NELISP_SNAP_RESOLVED/nelisp" ]; then
  printf '  ok: %s\n' "$NELISP_SNAP_RESOLVED/nelisp"
else
  printf '  warn: %s is missing; run scripts/sync-nelisp-snap.sh\n' "$NELISP_SNAP_RESOLVED/nelisp"
fi

if [ -f "$EMACS_ROOT_RESOLVED/src/nemacs-gui-file-bridge-runtime.el" ]; then
  printf '  ok: %s\n' "$EMACS_ROOT_RESOLVED/src/nemacs-gui-file-bridge-runtime.el"
else
  printf '  warn: GUI bridge runtime is missing in nelisp-emacs\n'
fi

if [ -d "$EMACS_ROOT_RESOLVED/vendor/nelisp/.git" ]; then
  printf '  note: nelisp-emacs/vendor/nelisp exists as a dependency checkout, not as the active GUI workspace\n'
else
  printf '  ok: nelisp-emacs/vendor/nelisp is not required for sibling workspace development\n'
fi

case "$BACKEND" in
  auto|session|nelisp)
    printf '  ok: NEMACS_BRIDGE_BACKEND=%s\n' "$BACKEND"
    ;;
  *)
    printf '  fail: NEMACS_BRIDGE_BACKEND=%s is invalid; use session, nelisp, or auto\n' "$BACKEND"
    ok=0
    ;;
esac

printf '\ntransport checks\n'
if [ -d "$TRANSPORT_DIR" ]; then
  if [ -w "$TRANSPORT_DIR" ]; then
    printf '  ok: %s is writable\n' "$TRANSPORT_DIR"
  else
    printf '  fail: %s exists but is not writable\n' "$TRANSPORT_DIR"
    ok=0
  fi
else
  parent=$(dirname -- "$TRANSPORT_DIR")
  if [ -d "$parent" ] && [ -w "$parent" ]; then
    printf '  ok: %s can be created by bin/nemacs\n' "$TRANSPORT_DIR"
  else
    printf '  fail: %s cannot be created; parent is missing or not writable\n' "$TRANSPORT_DIR"
    ok=0
  fi
fi
if [ "$TRANSPORT_DIR" != "/tmp" ]; then
  compiled_transport_dir=$(cat "$NATIVE_TRANSPORT_FILE" 2>/dev/null || true)
  if [ "$compiled_transport_dir" = "$TRANSPORT_DIR" ]; then
    printf '  ok: compiled GUI transport marker matches %s\n' "$TRANSPORT_DIR"
  else
    printf '  note: next bin/nemacs launch will rebuild GUI for %s\n' "$TRANSPORT_DIR"
  fi
  compiled_config_path=$(cat "$NATIVE_CONFIG_FILE" 2>/dev/null || true)
  if [ "$compiled_config_path" = "$CONFIG_PATH" ]; then
    printf '  ok: compiled GUI config marker matches %s\n' "$CONFIG_PATH"
  else
    printf '  note: next bin/nemacs launch will compile GUI config path %s\n' "$CONFIG_PATH"
  fi
fi
if [ -n "${DISPLAY:-}" ]; then
  printf '  ok: DISPLAY=%s\n' "$DISPLAY"
else
  printf '  warn: DISPLAY is unset; native GUI launch needs X11\n'
fi
printf '  native config: %s\n' "$CONFIG_PATH"
printf '  native bin:    %s\n' "$NATIVE_BIN"

printf '\nuser config checks\n'
if [ -d "$HOME/.nemacs.d" ]; then
  printf '  ok: %s\n' "$HOME/.nemacs.d"
else
  printf '  warn: %s is missing; bin/nemacs expects early-init.el/init.el defaults there\n' "$HOME/.nemacs.d"
fi
for cfg in early-init.el init.el; do
  if [ -f "$HOME/.nemacs.d/$cfg" ]; then
    printf '  ok: %s\n' "$HOME/.nemacs.d/$cfg"
  else
    printf '  warn: %s is missing\n' "$HOME/.nemacs.d/$cfg"
  fi
done

printf '\ninstall checks\n'
check_link_status emacs
check_link_status nemacs
case ":$PATH:" in
  *":$USER_BIN:"*)
    printf '  ok: %s is on PATH\n' "$USER_BIN"
    ;;
  *)
    printf '  warn: %s is not on PATH\n' "$USER_BIN"
    ;;
esac

printf '\nsequential verification commands\n'
printf '  %s/scripts/sync-nelisp-snap.sh\n' "$GUI_ROOT"
printf '  make -C %s test-nemacs-gui-bridge NELISP_BIN=%s/nelisp\n' "$EMACS_ROOT_RESOLVED" "$NELISP_SNAP_RESOLVED"
printf '  %s/scripts/verify-nemacs-gui.sh\n' "$GUI_ROOT"
printf '\nUse non-default NEMACS_TRANSPORT_DIR values for parallel GUI verification; default /tmp compatibility smoke remains sequential.\n'

if [ "$ok" -ne 1 ]; then
  exit 1
fi
