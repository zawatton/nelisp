#!/usr/bin/env bash
# release/stage-d-v3.0/install-v3.sh — stage-d-v3.0 standalone installer

set -euo pipefail

RELEASE_VERSION="${RELEASE_VERSION:-stage-d-v3.0}"
ANVIL_PREFIX="${ANVIL_PREFIX:-$HOME/.local/share/anvil-${RELEASE_VERSION}}"
RELEASE_BASE_URL="${RELEASE_BASE_URL:-https://github.com/zawatton/nelisp/releases/download/${RELEASE_VERSION}}"
FROM_DIR=""

usage() {
  cat <<USAGE
stage-d-v3.0 installer (true standalone, no Emacs runtime dependency)

Usage:
  install-v3.sh [--from DIR] [--prefix DIR] [--version VERSION] [--help]
USAGE
}

while [ $# -gt 0 ]; do
  case "$1" in
    --help|-h) usage; exit 0 ;;
    --from) FROM_DIR="${2:?--from requires an arg}"; shift 2 ;;
    --prefix) ANVIL_PREFIX="${2:?--prefix requires an arg}"; shift 2 ;;
    --version) RELEASE_VERSION="${2:?--version requires an arg}"; shift 2 ;;
    *) printf 'install-v3.sh: unknown arg: %s\n' "$1" >&2; usage >&2; exit 1 ;;
  esac
done

log() { printf '  ==> %s\n' "$*"; }
err() { printf '  error: %s\n' "$*" >&2; }

detect_platform() {
  case "$(uname -s)-$(uname -m)" in
    Linux-x86_64) echo "linux-x86_64" ;;
    Linux-aarch64|Linux-arm64) echo "linux-arm64" ;;
    Darwin-arm64) echo "macos-arm64" ;;
    *) return 1 ;;
  esac
}

PLATFORM="${PLATFORM:-$(detect_platform || true)}"
[[ -n "$PLATFORM" ]] || { err "unsupported platform: $(uname -s)-$(uname -m)"; exit 1; }

command -v tar >/dev/null 2>&1 || { err "missing tool: tar"; exit 3; }
if [ -z "$FROM_DIR" ]; then
  command -v curl >/dev/null 2>&1 || { err "missing tool: curl"; exit 3; }
fi
if command -v sha256sum >/dev/null 2>&1; then
  SHA_CMD='sha256sum --check'
elif command -v shasum >/dev/null 2>&1; then
  SHA_CMD='shasum -a 256 --check'
else
  err "neither sha256sum nor shasum on PATH"
  exit 3
fi

ARTIFACT="anvil-${RELEASE_VERSION}-${PLATFORM}.tar.gz"
CHECKSUM="${ARTIFACT}.sha256"
WORK="$(mktemp -d -t anvil-install-v3-XXXXXX)"
trap 'rm -rf "$WORK"' EXIT

if [ -n "$FROM_DIR" ]; then
  cp "${FROM_DIR}/${ARTIFACT}" "${WORK}/${ARTIFACT}" || { err "missing artifact: ${FROM_DIR}/${ARTIFACT}"; exit 2; }
  cp "${FROM_DIR}/${CHECKSUM}" "${WORK}/${CHECKSUM}" || { err "missing checksum: ${FROM_DIR}/${CHECKSUM}"; exit 2; }
else
  curl -fsSL -o "${WORK}/${ARTIFACT}" "${RELEASE_BASE_URL}/${ARTIFACT}" || { err "download failed: ${ARTIFACT}"; exit 2; }
  curl -fsSL -o "${WORK}/${CHECKSUM}" "${RELEASE_BASE_URL}/${CHECKSUM}" || { err "download failed: ${CHECKSUM}"; exit 2; }
fi

( cd "${WORK}" && ${SHA_CMD} "${CHECKSUM}" ) || { err "checksum verify FAILED"; exit 1; }
mkdir -p "${ANVIL_PREFIX}" || { err "cannot create prefix: ${ANVIL_PREFIX}"; exit 2; }
tar -xzf "${WORK}/${ARTIFACT}" -C "${ANVIL_PREFIX}" --strip-components=1 || { err "tar extract failed"; exit 2; }

log "installed: ${ANVIL_PREFIX}"
log "next: add ${ANVIL_PREFIX}/bin to your PATH"

