#!/usr/bin/env bash
# tools/build-standalone-tarball.sh — stage-d-v3.0 true standalone tarball
#
# Produces a no-Emacs runtime bundle:
#   bin/anvil
#   bin/anvil-runtime
#   src/nelisp*.el
#   anvil-lib/{anvil-host,anvil-shell-filter,anvil-data,...deps}.el
#   lib/libnelisp_runtime.{so,dylib,dll}
#   README-stage-d-v3.0.org
#   install-v3.sh
#   VERSION / PLATFORM / MANIFEST.txt

set -euo pipefail

VERSION="${1:-stage-d-v3.0}"
PLATFORM="${2:-linux-x86_64}"

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
cd "$REPO_ROOT"

log() { printf "  \033[1;34m==>\033[0m %s\n" "$*"; }
err() { printf "  \033[1;31merror:\033[0m %s\n" "$*" >&2; }

ARTIFACT_NAME="anvil-${VERSION}-${PLATFORM}"
STAGE_DIR="dist/${ARTIFACT_NAME}"
TAR_FILE="dist/${ARTIFACT_NAME}.tar.gz"

ANVIL_EL_SOURCE="${ANVIL_EL_SOURCE:-$HOME/Notes/dev/anvil.el}"
ANVIL_FILES=(
  anvil-data.el
  anvil-disk.el
  anvil-file.el
  anvil-host.el
  anvil-server-metrics.el
  anvil-server.el
  anvil-shell-filter.el
  anvil-state.el
)

log "stage-d-v3.0 standalone tarball"
log "  version : $VERSION"
log "  platform: $PLATFORM"
log "  source  : $ANVIL_EL_SOURCE"

RUNTIME_DIR="nelisp-runtime/target/release"
RUNTIME_BIN="$RUNTIME_DIR/anvil-runtime"
RUNTIME_CDYLIB=""
for cand in "$RUNTIME_DIR/libnelisp_runtime.so" \
            "$RUNTIME_DIR/libnelisp_runtime.dylib" \
            "$RUNTIME_DIR/nelisp_runtime.dll"; do
  if [[ -f "$cand" ]]; then
    RUNTIME_CDYLIB="$cand"
    break
  fi
done
RUNTIME_STATICLIB="$RUNTIME_DIR/libnelisp_runtime.a"

[[ -x "$RUNTIME_BIN" ]] || { err "runtime binary missing: $RUNTIME_BIN"; exit 1; }
[[ -n "$RUNTIME_CDYLIB" ]] || { err "runtime cdylib missing under $RUNTIME_DIR"; exit 1; }
[[ -f "$RUNTIME_STATICLIB" ]] || { err "runtime staticlib missing: $RUNTIME_STATICLIB"; exit 1; }

for file in "${ANVIL_FILES[@]}"; do
  [[ -f "$ANVIL_EL_SOURCE/$file" ]] || { err "missing anvil.el source file: $ANVIL_EL_SOURCE/$file"; exit 1; }
done

rm -rf "$STAGE_DIR"
mkdir -p "$STAGE_DIR/bin" "$STAGE_DIR/src" "$STAGE_DIR/anvil-lib" "$STAGE_DIR/lib"

cp bin/anvil "$STAGE_DIR/bin/"
cp "$RUNTIME_BIN" "$STAGE_DIR/bin/anvil-runtime"
chmod +x "$STAGE_DIR/bin/anvil" "$STAGE_DIR/bin/anvil-runtime"

cp src/nelisp*.el "$STAGE_DIR/src/"
for file in "${ANVIL_FILES[@]}"; do
  cp "$ANVIL_EL_SOURCE/$file" "$STAGE_DIR/anvil-lib/"
done
cp "$RUNTIME_CDYLIB" "$STAGE_DIR/lib/"

[[ -f LICENSE ]] && cp LICENSE "$STAGE_DIR/"
cp README-stage-d-v3.0.org "$STAGE_DIR/README.org"
cp release/stage-d-v3.0/install-v3.sh "$STAGE_DIR/install-v3.sh"
chmod +x "$STAGE_DIR/install-v3.sh"
printf "%s\n" "$VERSION" > "$STAGE_DIR/VERSION"
printf "%s\n" "$PLATFORM" > "$STAGE_DIR/PLATFORM"

{
  printf "stage-d-v3.0 standalone manifest\n"
  printf "version  %s\n" "$VERSION"
  printf "platform %s\n" "$PLATFORM"
  printf "runtime  %s\n" "$(basename "$RUNTIME_BIN")"
  printf "cdylib   %s\n" "$(basename "$RUNTIME_CDYLIB")"
  printf "anvil-el %s files\n" "${#ANVIL_FILES[@]}"
  printf "built    %s\n" "$(date -u +%Y-%m-%dT%H:%M:%SZ)"
} > "$STAGE_DIR/MANIFEST.txt"

log "assembling tarball"
( cd dist && tar -czf "${ARTIFACT_NAME}.tar.gz" "${ARTIFACT_NAME}/" )

( cd dist && \
    if command -v sha256sum >/dev/null 2>&1; then \
      sha256sum "${ARTIFACT_NAME}.tar.gz" > "${ARTIFACT_NAME}.tar.gz.sha256"; \
    elif command -v shasum >/dev/null 2>&1; then \
      shasum -a 256 "${ARTIFACT_NAME}.tar.gz" > "${ARTIFACT_NAME}.tar.gz.sha256"; \
    else \
      err "neither sha256sum nor shasum found"; \
      exit 1; \
    fi )

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

