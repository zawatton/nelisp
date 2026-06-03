#!/usr/bin/env bash
# tools/build-standalone-tarball.sh — zero-Rust standalone tarball
#
# Produces a no-Rust bundle containing the pure-elisp standalone binary:
#   bin/anvil
#   bin/nelisp   (built by `make standalone-reader` on POSIX targets)
#   bin/nelisp.exe on windows-x86_64
#   src/nelisp*.el
#   scripts/*.el
#   lisp/*.el
#   README.org (or README-stage-d.org)
#   install.sh
#   VERSION / PLATFORM / MANIFEST.txt
#
# Caller must have already run `make standalone-reader` before invoking
# this script so that target/nelisp is present.

set -euo pipefail

default_platform() {
  case "$(uname -s 2>/dev/null || echo)-$(uname -m 2>/dev/null || echo)" in
    Darwin-arm64) echo "macos-aarch64" ;;
    Linux-x86_64) echo "linux-x86_64" ;;
    MINGW*-x86_64|MSYS*-x86_64|CYGWIN*-x86_64) echo "windows-x86_64" ;;
    *) echo "linux-x86_64" ;;
  esac
}

VERSION="${1:-stage-d-v3.0}"
PLATFORM="${2:-${NELISP_STANDALONE_TARGET:-$(default_platform)}}"

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
cd "$REPO_ROOT"

log() { printf "  \033[1;34m==>\033[0m %s\n" "$*"; }
err() { printf "  \033[1;31merror:\033[0m %s\n" "$*" >&2; }

ARTIFACT_NAME="anvil-${VERSION}-${PLATFORM}"
STAGE_DIR="dist/${ARTIFACT_NAME}"
TAR_FILE="dist/${ARTIFACT_NAME}.tar.gz"

log "zero-Rust standalone tarball"
log "  version : $VERSION"
log "  platform: $PLATFORM"

# 1. Ensure the standalone reader binary is built for the requested platform.
case "$PLATFORM" in
  windows-x86_64)
    STANDALONE_BIN="target/nelisp.exe"
    STAGED_BIN_NAME="nelisp.exe"
    ;;
  linux-x86_64|macos-aarch64)
    STANDALONE_BIN="target/nelisp"
    STAGED_BIN_NAME="nelisp"
    ;;
  *)
    err "unsupported platform: $PLATFORM"
    exit 2
    ;;
esac

log "building standalone reader for $PLATFORM"
NELISP_STANDALONE_TARGET="$PLATFORM" make standalone-reader
[[ -f "$STANDALONE_BIN" ]] || { err "standalone binary missing: $STANDALONE_BIN"; exit 1; }

# 2. Stage the tarball directory.
rm -rf "$STAGE_DIR"
mkdir -p "$STAGE_DIR/bin" "$STAGE_DIR/src" "$STAGE_DIR/scripts" "$STAGE_DIR/lisp"

# bin/ — anvil launcher + standalone binary.
cp bin/anvil "$STAGE_DIR/bin/"
[[ -f bin/anvil.cmd ]] && cp bin/anvil.cmd "$STAGE_DIR/bin/" || true
cp "$STANDALONE_BIN" "$STAGE_DIR/bin/$STAGED_BIN_NAME"
chmod +x "$STAGE_DIR/bin/anvil"
if [[ "$STAGED_BIN_NAME" = "nelisp" ]]; then
  chmod +x "$STAGE_DIR/bin/nelisp"
fi

# Elisp sources.
cp src/nelisp*.el "$STAGE_DIR/src/"
[[ -d scripts ]] && cp scripts/*.el "$STAGE_DIR/scripts/" 2>/dev/null || true
[[ -d lisp ]] && cp lisp/*.el "$STAGE_DIR/lisp/" 2>/dev/null || true

# Docs + version stamps.
[[ -f LICENSE ]] && cp LICENSE "$STAGE_DIR/"
if [[ -f README-stage-d-v3.0.org ]]; then
  cp README-stage-d-v3.0.org "$STAGE_DIR/README.org"
elif [[ -f README-stage-d.org ]]; then
  cp README-stage-d.org "$STAGE_DIR/README.org"
elif [[ -f README.org ]]; then
  cp README.org "$STAGE_DIR/README.org"
fi
[[ -f install.sh ]] && cp install.sh "$STAGE_DIR/install.sh" && chmod +x "$STAGE_DIR/install.sh" || true
printf "%s\n" "$VERSION" > "$STAGE_DIR/VERSION"
printf "%s\n" "$PLATFORM" > "$STAGE_DIR/PLATFORM"

{
  printf "zero-Rust standalone manifest\n"
  printf "version    %s\n" "$VERSION"
  printf "platform   %s\n" "$PLATFORM"
  printf "standalone bin/%s\n" "$STAGED_BIN_NAME"
  printf "built      %s\n" "$(date -u +%Y-%m-%dT%H:%M:%SZ)"
} > "$STAGE_DIR/MANIFEST.txt"

# 3. Assemble tarball.
log "assembling tarball"
( cd dist && tar -czf "${ARTIFACT_NAME}.tar.gz" "${ARTIFACT_NAME}/" )

# 4. SHA-256 checksum.
( cd dist && \
    if command -v sha256sum >/dev/null 2>&1; then \
      sha256sum "${ARTIFACT_NAME}.tar.gz" > "${ARTIFACT_NAME}.tar.gz.sha256"; \
    elif command -v shasum >/dev/null 2>&1; then \
      shasum -a 256 "${ARTIFACT_NAME}.tar.gz" > "${ARTIFACT_NAME}.tar.gz.sha256"; \
    else \
      err "neither sha256sum nor shasum found"; \
      exit 1; \
    fi )

# 5. Size cap check (15 MB).
SIZE_BYTES=$(wc -c < "$TAR_FILE" | tr -d ' ')
SIZE_MB=$(( (SIZE_BYTES + 1048575) / 1048576 ))
if (( SIZE_BYTES >= 15 * 1024 * 1024 )); then
  err "tarball exceeds 15 MB cap: ${SIZE_BYTES} bytes"
  exit 2
fi

rm -rf "$STAGE_DIR"

log "built $TAR_FILE ($(du -h "$TAR_FILE" | cut -f1))"
log "checksum: $TAR_FILE.sha256"
log "size cap: OK (< 15 MB; rounded=${SIZE_MB} MiB)"
