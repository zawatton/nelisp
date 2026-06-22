#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
GUI_ROOT=$(CDPATH= cd -- "$SCRIPT_DIR/.." && pwd)
DEST=${NEMACS_USER_BIN:-$HOME/.local/bin}
DATA_HOME=${XDG_DATA_HOME:-$HOME/.local/share}
MANIFEST=${NEMACS_INSTALL_MANIFEST:-$DATA_HOME/nemacs/install-manifest}

usage() {
  cat <<'EOF'
Usage: scripts/install-user-bin.sh [install|uninstall|rollback|status|doctor]

Installs nelisp-gui compatibility launchers into $NEMACS_USER_BIN
(default: ~/.local/bin):
  emacs  -> <repo>/bin/emacs
  nemacs -> <repo>/bin/nemacs

Existing non-nemacs files are never overwritten.
Rollback removes only symlinks that still match the install manifest.
EOF
}

target_for() {
  case "$1" in
    emacs) printf '%s/bin/emacs\n' "$GUI_ROOT" ;;
    nemacs) printf '%s/bin/nemacs\n' "$GUI_ROOT" ;;
    *) return 1 ;;
  esac
}

manifest_value() {
  key=$1

  [ -f "$MANIFEST" ] || return 1

  while IFS= read -r line; do
    case "$line" in
      "$key="*)
        printf '%s\n' "${line#*=}"
        return 0
        ;;
    esac
  done < "$MANIFEST"

  return 1
}

manifest_target_for() {
  manifest_value "$1"
}

link_target() {
  readlink "$1" 2>/dev/null || true
}

is_current_link() {
  name=$1
  dst="$DEST/$name"
  src=$(target_for "$name")

  [ "$(link_target "$dst")" = "$src" ]
}

is_manifest_link() {
  name=$1
  dst="$DEST/$name"
  expected=$(manifest_target_for "$name" 2>/dev/null || true)

  [ -n "$expected" ] && [ "$(link_target "$dst")" = "$expected" ]
}

preflight_one() {
  name=$1
  src=$(target_for "$name")
  dst="$DEST/$name"

  if [ -L "$dst" ]; then
    current=$(link_target "$dst")
    if [ "$current" = "$src" ]; then
      return
    fi
    if is_manifest_link "$name"; then
      return
    fi
    printf 'refusing to overwrite existing symlink %s -> %s\n' "$dst" "$current" >&2
    printf 'remove it manually if it should be replaced by nelisp-gui\n' >&2
    return 1
  fi

  if [ -e "$dst" ]; then
    printf 'refusing to overwrite existing file %s\n' "$dst" >&2
    return 1
  fi
}

install_one() {
  name=$1
  src=$(target_for "$name")
  dst="$DEST/$name"

  if is_current_link "$name"; then
    printf 'ok: %s already points to %s\n' "$dst" "$src"
    return
  fi

  if [ -L "$dst" ] && is_manifest_link "$name"; then
    old=$(link_target "$dst")
    rm "$dst"
    ln -s "$src" "$dst"
    printf 'updated %s -> %s (was %s)\n' "$dst" "$src" "$old"
    return
  fi

  ln -s "$src" "$dst"
  printf 'linked %s -> %s\n' "$dst" "$src"
}

write_manifest() {
  manifest_dir=$(dirname -- "$MANIFEST")
  tmp="$MANIFEST.$$"

  mkdir -p "$manifest_dir"
  {
    printf 'emacs=%s\n' "$(target_for emacs)"
    printf 'nemacs=%s\n' "$(target_for nemacs)"
    printf 'installed-at=%s\n' "$(date -u '+%Y-%m-%dT%H:%M:%SZ')"
  } > "$tmp"
  mv "$tmp" "$MANIFEST"
  printf 'manifest: %s\n' "$MANIFEST"
}

uninstall_one() {
  name=$1
  src=$(target_for "$name")
  dst="$DEST/$name"
  current=$(link_target "$dst")

  if [ "$current" = "$src" ] || is_manifest_link "$name"; then
    rm "$dst"
    printf 'removed %s\n' "$dst"
  else
    printf 'skip: %s is not a nelisp-gui link\n' "$dst"
  fi
}

cleanup_manifest_if_unused() {
  [ -f "$MANIFEST" ] || return 0

  if is_manifest_link emacs || is_manifest_link nemacs; then
    return 0
  fi

  rm -f "$MANIFEST"
  printf 'removed manifest %s\n' "$MANIFEST"
}

rollback_one() {
  name=$1
  dst="$DEST/$name"
  expected=$(manifest_target_for "$name" 2>/dev/null || true)
  current=$(link_target "$dst")

  if [ -z "$expected" ]; then
    printf 'skip: manifest has no %s entry\n' "$name"
    return 0
  fi

  if [ "$current" = "$expected" ]; then
    rm "$dst"
    printf 'rolled back: removed %s\n' "$dst"
    return 0
  fi

  if [ -e "$dst" ] || [ -L "$dst" ]; then
    printf 'skip: %s does not match manifest target; leaving it untouched\n' "$dst"
    return 1
  fi

  printf 'skip: %s is already absent\n' "$dst"
}

rollback_all() {
  if [ ! -f "$MANIFEST" ]; then
    printf 'skip: no manifest at %s\n' "$MANIFEST"
    status_command
    return 0
  fi

  blocked=0
  rollback_one emacs || blocked=1
  rollback_one nemacs || blocked=1

  if [ "$blocked" -eq 0 ]; then
    rm -f "$MANIFEST"
    printf 'removed manifest %s\n' "$MANIFEST"
  else
    printf 'kept manifest %s because one or more paths were not managed links\n' "$MANIFEST"
  fi

  printf 'hint: run "hash -r" or restart your shell if it cached emacs\n'
  status_command
}

status_one() {
  name=$1
  src=$(target_for "$name")
  dst="$DEST/$name"
  current=$(link_target "$dst")
  manifest_target=$(manifest_target_for "$name" 2>/dev/null || true)

  if [ "$current" = "$src" ]; then
    printf 'installed: %s -> %s\n' "$dst" "$src"
  elif [ -n "$manifest_target" ] && [ "$current" = "$manifest_target" ]; then
    printf 'managed: %s -> %s (expected now: %s)\n' "$dst" "$current" "$src"
  elif [ -L "$dst" ]; then
    printf 'occupied symlink: %s -> %s\n' "$dst" "$current"
  elif [ -e "$dst" ] || [ -L "$dst" ]; then
    printf 'occupied file: %s\n' "$dst"
  else
    printf 'missing: %s\n' "$dst"
  fi
}

status_manifest() {
  printf 'manifest: %s\n' "$MANIFEST"
  if [ -f "$MANIFEST" ]; then
    while IFS= read -r line; do
      printf '  %s\n' "$line"
    done < "$MANIFEST"
  else
    printf '  missing\n'
  fi
}

status_path() {
  case ":$PATH:" in
    *":$DEST:"*)
      printf 'ok: %s is on PATH\n' "$DEST"
      ;;
    *)
      printf 'warn: %s is not on PATH; add it before the system emacs directory to use bin/emacs\n' "$DEST"
      ;;
  esac
}

status_command() {
  found=$(command -v emacs 2>/dev/null || true)
  if [ -n "$found" ]; then
    printf 'command: emacs -> %s\n' "$found"
    if [ "$found" = "$DEST/emacs" ]; then
      printf 'ok: command -v emacs resolves to the user-bin launcher\n'
    elif [ -L "$DEST/emacs" ] || [ -e "$DEST/emacs" ]; then
      printf 'warn: %s exists but command -v emacs resolves elsewhere\n' "$DEST/emacs"
    fi
  else
    printf 'warn: command -v emacs found nothing\n'
  fi
}

status_all() {
  status_one emacs
  status_one nemacs
  status_manifest
  status_path
  status_command
}

cmd=${1:-install}
case "$cmd" in
  install)
    mkdir -p "$DEST"
    preflight_one emacs
    preflight_one nemacs
    install_one emacs
    install_one nemacs
    write_manifest
    status_path
    status_command
    ;;
  uninstall)
    uninstall_one emacs
    uninstall_one nemacs
    cleanup_manifest_if_unused
    ;;
  rollback)
    rollback_all
    ;;
  status)
    status_all
    ;;
  doctor)
    status_all
    printf '\n'
    exec "$GUI_ROOT/scripts/doctor-workspace.sh"
    ;;
  -h|--help|help)
    usage
    ;;
  *)
    usage >&2
    exit 2
    ;;
esac
