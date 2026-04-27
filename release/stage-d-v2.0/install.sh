#!/usr/bin/env bash
# release/stage-d-v2.0/install.sh — stage-d-v2.0 one-liner installer
#
# Doc 32 v2 LOCKED §3.3 — installer skeleton for the production
# stage-d-v2.0 release artifact pipeline.  This is intentionally a
# *skeleton* until the GitHub Release upload step lands (Doc 32 v2
# §3.3 ships the workflow + artifact bundle but defers public Release
# attach to v1.0); once that lands the `RELEASE_BASE_URL' default flips
# to the GitHub Release CDN URL and the curl fetch becomes the canonical
# path.  Until then the installer accepts a `--from <dir>' override so
# CI / local developers can point it at a downloaded artifact bundle.
#
# Usage (post-v1.0 / curl-bash one-liner — placeholder):
#   curl -fsSL https://github.com/zawatton/nelisp/releases/download/stage-d-v2.0/install.sh | bash
#
# Usage (today / local artifact dir):
#   ./release/stage-d-v2.0/install.sh --from /path/to/dist
#
# Environment overrides:
#   RELEASE_VERSION    — version label (default: stage-d-v2.0)
#   PLATFORM           — auto-detected (linux-x86_64 | macos-arm64 | linux-arm64)
#   ANVIL_PREFIX       — install root (default: ~/.local/share/anvil-stage-d-v2.0)
#   RELEASE_BASE_URL   — artifact base URL (default: GitHub Release URL placeholder)
#
# Exit codes:
#   0 — install succeeded (or --help printed cleanly)
#   1 — checksum mismatch or unsupported platform
#   2 — download / fs failure
#   3 — required tool missing (sha256sum / curl / tar)

set -euo pipefail

RELEASE_VERSION="${RELEASE_VERSION:-stage-d-v2.0}"
ANVIL_PREFIX="${ANVIL_PREFIX:-$HOME/.local/share/anvil-${RELEASE_VERSION}}"
RELEASE_BASE_URL="${RELEASE_BASE_URL:-https://github.com/zawatton/nelisp/releases/download/${RELEASE_VERSION}}"
FROM_DIR=""

usage() {
  cat <<USAGE
stage-d-v2.0 installer (Doc 32 v2 §3.3 skeleton)

Usage:
  install.sh [--from DIR] [--prefix DIR] [--version VERSION] [--help]

Options:
  --from DIR        Install from a local artifact directory instead of
                    downloading from GitHub Release (=DIR must contain
                    \${RELEASE_VERSION}-\${PLATFORM}.tar.gz + .sha256).
  --prefix DIR      Install root (default: \$ANVIL_PREFIX or
                    ~/.local/share/anvil-\${RELEASE_VERSION}).
  --version VER     Override release version (default: stage-d-v2.0).
  --help            Print this message and exit 0.

Environment:
  RELEASE_VERSION   Version label (default: stage-d-v2.0).
  PLATFORM          Override platform detect.
                    Recognised: linux-x86_64 | macos-arm64 | linux-arm64.
  ANVIL_PREFIX      Install root.
  RELEASE_BASE_URL  Artifact base URL.

Exit codes:
  0  success
  1  checksum mismatch / unsupported platform
  2  download / fs failure
  3  required tool missing
USAGE
}

# --- argv parse ----------------------------------------------------------
while [ $# -gt 0 ]; do
  case "$1" in
    --help|-h) usage; exit 0 ;;
    --from) FROM_DIR="${2:?--from requires an arg}"; shift 2 ;;
    --prefix) ANVIL_PREFIX="${2:?--prefix requires an arg}"; shift 2 ;;
    --version) RELEASE_VERSION="${2:?--version requires an arg}"; shift 2 ;;
    *) printf 'install.sh: unknown arg: %s\n' "$1" >&2; usage >&2; exit 1 ;;
  esac
done

log() { printf '  ==> %s\n' "$*"; }
err() { printf '  error: %s\n' "$*" >&2; }

# --- platform detect -----------------------------------------------------
detect_platform() {
  local uname_s uname_m
  uname_s="$(uname -s)"
  uname_m="$(uname -m)"
  case "${uname_s}-${uname_m}" in
    Linux-x86_64)  echo "linux-x86_64" ;;
    Linux-aarch64|Linux-arm64) echo "linux-arm64" ;;
    Darwin-arm64)  echo "macos-arm64" ;;
    *) return 1 ;;
  esac
}

PLATFORM="${PLATFORM:-$(detect_platform || true)}"
if [ -z "${PLATFORM}" ]; then
  err "unsupported platform: $(uname -s)-$(uname -m)"
  err "supported: linux-x86_64 | macos-arm64 | linux-arm64"
  exit 1
fi

# --- required tool probe -------------------------------------------------
require_tool() {
  command -v "$1" >/dev/null 2>&1 || { err "missing tool: $1"; exit 3; }
}
require_tool tar
if [ -z "$FROM_DIR" ]; then
  require_tool curl
fi
# sha256sum (Linux) or shasum (macOS) — accept either.
if command -v sha256sum >/dev/null 2>&1; then
  SHA_CMD='sha256sum --check'
elif command -v shasum >/dev/null 2>&1; then
  SHA_CMD='shasum -a 256 --check'
else
  err "neither sha256sum nor shasum on PATH"
  exit 3
fi

ARTIFACT="${RELEASE_VERSION}-${PLATFORM}.tar.gz"
CHECKSUM="${ARTIFACT}.sha256"

log "release version : ${RELEASE_VERSION}"
log "platform        : ${PLATFORM}"
log "install prefix  : ${ANVIL_PREFIX}"

# --- fetch ---------------------------------------------------------------
WORK="$(mktemp -d -t anvil-install-XXXXXX)"
trap 'rm -rf "$WORK"' EXIT

if [ -n "$FROM_DIR" ]; then
  log "source          : ${FROM_DIR} (--from)"
  cp "${FROM_DIR}/${ARTIFACT}" "${WORK}/${ARTIFACT}" \
    || { err "missing artifact: ${FROM_DIR}/${ARTIFACT}"; exit 2; }
  cp "${FROM_DIR}/${CHECKSUM}" "${WORK}/${CHECKSUM}" \
    || { err "missing checksum: ${FROM_DIR}/${CHECKSUM}"; exit 2; }
else
  log "source          : ${RELEASE_BASE_URL}"
  curl -fsSL -o "${WORK}/${ARTIFACT}"  "${RELEASE_BASE_URL}/${ARTIFACT}" \
    || { err "download failed: ${ARTIFACT}"; exit 2; }
  curl -fsSL -o "${WORK}/${CHECKSUM}"  "${RELEASE_BASE_URL}/${CHECKSUM}" \
    || { err "download failed: ${CHECKSUM}"; exit 2; }
fi

# --- verify --------------------------------------------------------------
log "verifying checksum"
( cd "${WORK}" && ${SHA_CMD} "${CHECKSUM}" ) \
  || { err "checksum verify FAILED — refusing to install"; exit 1; }

# --- install -------------------------------------------------------------
mkdir -p "${ANVIL_PREFIX}" || { err "cannot create prefix: ${ANVIL_PREFIX}"; exit 2; }
log "extracting tarball"
tar -xzf "${WORK}/${ARTIFACT}" -C "${ANVIL_PREFIX}" --strip-components=1 \
  || { err "tar extract failed"; exit 2; }

log "installed: ${ANVIL_PREFIX}"
log "next: add ${ANVIL_PREFIX}/bin to your PATH"
exit 0
