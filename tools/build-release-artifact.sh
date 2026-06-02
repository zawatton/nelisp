#!/usr/bin/env bash
# tools/build-release-artifact.sh — zero-Rust release artifact builder
#
# Doc 32 v2 LOCKED §3.3 / §2.3 採用 A (tarball + checksum + signature) /
# §2.5 (ad-hoc signature、real GPG / notarization は v2.1+ scope per §8).
#
# Usage:
#   tools/build-release-artifact.sh [PLATFORM] [VERSION]
#
#   PLATFORM defaults to linux-x86_64.  Recognised values:
#     - linux-x86_64  (blocker、Doc 32 v2 §11)
#     - macos-arm64   (non-blocker v1.0 時限、§11、v1.1+ で blocker promote 想定)
#     - linux-arm64   (non-blocker v1.0 時限、§11、v1.1+ で blocker promote 想定)
#
#   VERSION defaults to stage-d-v2.0.
#
# Caller must have already run `make standalone-reader` before invoking this
# script so that target/nelisp-standalone-reader is present.
#
# Outputs (under dist/):
#   <VERSION>-<PLATFORM>.tar.gz         — release tarball (Doc 32 v2 §2.3 採用 A)
#   <VERSION>-<PLATFORM>.tar.gz.sha256  — SHA-256 checksum (Doc 32 v2 §2.10 採用 A)
#   <VERSION>-<PLATFORM>.tar.gz.sig     — ad-hoc signature placeholder
#                                         (real GPG signing は v2.1+、§2.5 + §8)
#
# This script is *deliberately* idempotent: re-running on the same
# (PLATFORM, VERSION) pair overwrites the previous artifact.  CI matrix
# drives one job per platform (.github/workflows/release-qualification.yml).

set -euo pipefail

PLATFORM="${1:-linux-x86_64}"
VERSION="${2:-stage-d-v2.0}"

# Resolve repo root from script location so the tool works regardless of
# the caller's cwd (CI invokes it from repo root, but local debug runs
# may invoke it from anywhere).
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
cd "$REPO_ROOT"

log() { printf "  \033[1;34m==>\033[0m %s\n" "$*"; }
err() { printf "  \033[1;31merror:\033[0m %s\n" "$*" >&2; }

echo "zero-Rust release artifact builder"
echo "  version : $VERSION"
echo "  platform: $PLATFORM"
echo "  repo    : $REPO_ROOT"

# 1. Build the pure-elisp standalone binary if not already present.
STANDALONE_BIN="target/nelisp-standalone-reader"
if [[ ! -x "$STANDALONE_BIN" ]]; then
  log "standalone binary not found — running make standalone-reader"
  make standalone-reader
fi
[[ -x "$STANDALONE_BIN" ]] || { err "standalone binary still missing after build: $STANDALONE_BIN"; exit 1; }

# 2. Stage artifact directory under dist/.
ARTIFACT_NAME="${VERSION}-${PLATFORM}"
ARTIFACT_DIR="dist/${ARTIFACT_NAME}"

rm -rf "$ARTIFACT_DIR"
mkdir -p "$ARTIFACT_DIR/bin" "$ARTIFACT_DIR/src" "$ARTIFACT_DIR/lisp" "$ARTIFACT_DIR/scripts"

# bin/anvil launcher.
cp bin/anvil "$ARTIFACT_DIR/bin/"
[[ -f bin/anvil.cmd ]] && cp bin/anvil.cmd "$ARTIFACT_DIR/bin/" || true

# Pure-elisp standalone binary.
cp "$STANDALONE_BIN" "$ARTIFACT_DIR/bin/nelisp-standalone-reader"
chmod +x "$ARTIFACT_DIR/bin/nelisp-standalone-reader"

# Elisp sources.
cp src/nelisp*.el "$ARTIFACT_DIR/src/"
[[ -d lisp ]] && cp lisp/*.el "$ARTIFACT_DIR/lisp/" 2>/dev/null || true
[[ -d scripts ]] && cp scripts/*.el "$ARTIFACT_DIR/scripts/" 2>/dev/null || true

# Documentation + license + version stamp.
[[ -f LICENSE ]] && cp LICENSE "$ARTIFACT_DIR/" || true
[[ -f README.org ]] && cp README.org "$ARTIFACT_DIR/" || true
[[ -f README-stage-d.org ]] && cp README-stage-d.org "$ARTIFACT_DIR/" || true
[[ -f RELEASE_NOTES.md ]] && cp RELEASE_NOTES.md "$ARTIFACT_DIR/" || true
[[ -f install.sh ]] && cp install.sh "$ARTIFACT_DIR/" || true
printf "%s\n" "$VERSION" > "$ARTIFACT_DIR/VERSION"
printf "%s\n" "$PLATFORM" > "$ARTIFACT_DIR/PLATFORM"

# 3. Tarball.
TAR_FILE="${ARTIFACT_NAME}.tar.gz"
( cd dist && tar -czf "$TAR_FILE" "$ARTIFACT_NAME/" )

# 4. SHA-256 checksum (Doc 32 v2 §2.10 採用 A).
( cd dist && \
    if command -v sha256sum >/dev/null 2>&1; then \
        sha256sum "$TAR_FILE" > "$TAR_FILE.sha256"; \
    elif command -v shasum >/dev/null 2>&1; then \
        shasum -a 256 "$TAR_FILE" > "$TAR_FILE.sha256"; \
    else \
        echo "ERROR: neither sha256sum nor shasum found on PATH" >&2; \
        exit 1; \
    fi )

# 5. Ad-hoc signature placeholder (Doc 32 v2 §2.5、real GPG は v2.1+).
#    Records the platform + version + build timestamp; real GPG signing
#    lands when a maintainer key is available (§8 v2.1 scope).
SIG_PAYLOAD="ad-hoc-signature ${VERSION} ${PLATFORM} $(date -u +%Y-%m-%dT%H:%M:%SZ)"
printf "%s\n" "$SIG_PAYLOAD" > "dist/$TAR_FILE.sig"

# 6. Cleanup staged dir (tarball is the artifact).
rm -rf "$ARTIFACT_DIR"

# Report.
echo ""
echo "Built release artifact:"
echo "  tarball  : dist/$TAR_FILE"
echo "  checksum : dist/$TAR_FILE.sha256"
echo "  signature: dist/$TAR_FILE.sig (ad-hoc; GPG = v2.1+ per Doc 32 v2 §2.5)"
