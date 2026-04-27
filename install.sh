#!/usr/bin/env bash
# install.sh — anvil (NeLisp-powered Stage D) installer
#
# Usage:
#   curl -fsSL https://raw.githubusercontent.com/zawatton/nelisp/main/install.sh | bash
# or with a pinned version:
#   ANVIL_VERSION=stage-d-v0.1 curl -fsSL ... | bash
#
# What it does:
#   1. Detect Emacs 29+ (required, see docs/design/18-stage-d-standalone.org §2.5).
#   2. Resolve install root (default ~/.local/share/anvil — overridable via $ANVIL_PREFIX).
#   3. Download tarball from GitHub Release or copy from $ANVIL_LOCAL_TARBALL.
#   4. Extract bin/anvil + src/*.el under $ANVIL_PREFIX.
#   5. Symlink $HOME/.local/bin/anvil → $ANVIL_PREFIX/bin/anvil (PATH friendly).
#   6. Print Claude Code .mcp.json snippet.
#
# Exit codes:
#   0  success
#   1  emacs missing or too old
#   2  download / network failure
#   3  install dir not writable

set -euo pipefail

ANVIL_VERSION="${ANVIL_VERSION:-stage-d-v0.1}"
ANVIL_PREFIX="${ANVIL_PREFIX:-$HOME/.local/share/anvil}"
ANVIL_BIN_DIR="${ANVIL_BIN_DIR:-$HOME/.local/bin}"
ANVIL_REPO="${ANVIL_REPO:-zawatton/nelisp}"
ANVIL_LOCAL_TARBALL="${ANVIL_LOCAL_TARBALL:-}"

# Phase 7.5.3 stage-d-v2.0 — when the tarball ships a bundled emacs
# under emacs/bin/emacs the host-Emacs requirement is dropped (= the
# whole point of the bundled artifact).  We auto-detect that layout
# after extract; this flag forces the bundled-mode skip even when the
# detect heuristic is unsure.  Default `auto' = sniff after extract.
ANVIL_BUNDLED_EMACS="${ANVIL_BUNDLED_EMACS:-auto}"

log() { printf "  \033[1;34m==>\033[0m %s\n" "$*"; }
err() { printf "  \033[1;31merror:\033[0m %s\n" "$*" >&2; }

require_emacs() {
  local emacs_bin
  if [[ -n "${EMACS:-}" ]] && [[ -x "$EMACS" ]]; then
    emacs_bin="$EMACS"
  elif command -v emacs >/dev/null 2>&1; then
    emacs_bin="$(command -v emacs)"
  else
    err "Emacs binary not found. Install Emacs 29+ first."
    err "  macOS:    brew install --cask emacs"
    err "  Debian:   sudo apt install emacs"
    err "  Fedora:   sudo dnf install emacs"
    err "  Arch:     sudo pacman -S emacs"
    err "  Windows:  pacman -S mingw-w64-x86_64-emacs   (msys2)"
    err "            choco install emacs                (chocolatey)"
    err "            (run install.sh from an msys2 mingw64 shell)"
    exit 1
  fi
  local version
  version="$("$emacs_bin" --version | head -1 | grep -oE '[0-9]+\.[0-9]+' | head -1)"
  local major="${version%%.*}"
  if (( major < 29 )); then
    err "Emacs $version detected; anvil requires Emacs 29+ (sqlite primitives)."
    exit 1
  fi
  log "emacs $version  ($emacs_bin)"
  echo "$emacs_bin"
}

resolve_tarball() {
  local dest="$1"
  if [[ -n "$ANVIL_LOCAL_TARBALL" ]]; then
    log "using local tarball: $ANVIL_LOCAL_TARBALL"
    cp "$ANVIL_LOCAL_TARBALL" "$dest"
    return 0
  fi
  local url="https://github.com/$ANVIL_REPO/releases/download/$ANVIL_VERSION/anvil-$ANVIL_VERSION.tar.gz"
  log "downloading $url"
  if command -v curl >/dev/null 2>&1; then
    curl -fsSL "$url" -o "$dest" || { err "curl failed (status $?)"; exit 2; }
  elif command -v wget >/dev/null 2>&1; then
    wget -qO "$dest" "$url" || { err "wget failed"; exit 2; }
  else
    err "neither curl nor wget found"
    exit 2
  fi
}

main() {
  log "anvil installer (Stage D, $ANVIL_VERSION)"

  # Phase 7.5.3 — defer the host-Emacs requirement until after we
  # extract the tarball so a bundled-emacs artifact can override the
  # check.  Stage D Phase 6.x tarballs (= no `emacs/' subdir) keep the
  # original behaviour: Emacs 29+ on PATH is required.
  local skip_emacs_check=0
  if [[ "$ANVIL_BUNDLED_EMACS" == "1" || "$ANVIL_BUNDLED_EMACS" == "true" ]]; then
    skip_emacs_check=1
    log "ANVIL_BUNDLED_EMACS=1 — skipping host-Emacs probe (= bundled tarball)"
  fi

  mkdir -p "$ANVIL_PREFIX" "$ANVIL_BIN_DIR" || {
    err "cannot create install dirs ($ANVIL_PREFIX / $ANVIL_BIN_DIR)"
    exit 3
  }

  local tmp
  tmp="$(mktemp -d)"
  trap 'rm -rf "$tmp"' EXIT
  resolve_tarball "$tmp/anvil.tar.gz"

  log "extracting to $ANVIL_PREFIX"
  tar -xzf "$tmp/anvil.tar.gz" -C "$ANVIL_PREFIX" --strip-components=1

  if [[ ! -x "$ANVIL_PREFIX/bin/anvil" ]]; then
    err "extracted bundle missing bin/anvil — abort"
    exit 3
  fi

  # Phase 7.5.3 — auto-detect: if the extracted tarball ships an
  # `emacs/bin/emacs' the install is bundled-mode.  Otherwise fall
  # back to the original host-Emacs require_emacs() check.
  if [[ "$ANVIL_BUNDLED_EMACS" == "auto" ]]; then
    if [[ -x "$ANVIL_PREFIX/emacs/bin/emacs" || -x "$ANVIL_PREFIX/emacs/bin/emacs.exe" ]]; then
      skip_emacs_check=1
      log "bundled emacs detected at $ANVIL_PREFIX/emacs/bin/emacs"
    fi
  fi
  if (( skip_emacs_check == 0 )); then
    require_emacs >/dev/null
  fi

  log "linking $ANVIL_BIN_DIR/anvil → $ANVIL_PREFIX/bin/anvil"
  ln -sf "$ANVIL_PREFIX/bin/anvil" "$ANVIL_BIN_DIR/anvil"

  printf "\n"
  log "✓ install complete"
  printf "\n"
  printf "  Run \033[1m%s\033[0m to verify environment.\n" "$ANVIL_BIN_DIR/anvil doctor"
  printf "\n"
  printf "  Claude Code .mcp.json snippet:\n"
  printf "  \033[36m"
  cat <<'JSON'
  {
    "mcpServers": {
      "anvil": {
        "command": "anvil",
        "args": ["mcp", "serve"]
      }
    }
  }
JSON
  printf "  \033[0m\n"
  printf "  Make sure \033[1m%s\033[0m is on your PATH:\n" "$ANVIL_BIN_DIR"
  printf "    export PATH=\"%s:\$PATH\"\n\n" "$ANVIL_BIN_DIR"
}

main "$@"
