#!/usr/bin/env bash
# tools/verify-bundled-tarball.sh — Phase 7.5.3 stage-d-v2.0 end-to-end smoke
#
# Doc 32 v2 §3.3 ERT smoke #5 (`install.sh' e2e) sibling — runs the
# tarball through extract → `bin/anvil version` → cleanup so a release
# candidate is gated on more than just structural verifier.
#
# Usage:
#   tools/verify-bundled-tarball.sh [VERSION] [PLATFORM]
#
# Asserts:
#   - tarball exists at dist/anvil-<VERSION>-<PLATFORM>.tar.gz
#   - SHA-256 matches the recorded .sha256
#   - extracts cleanly to /tmp/test-anvil-<pid>/
#   - `bin/anvil version` reports the bundled emacs (not the host one)
#   - `tools registered` line is present (= tools list parsed)
#
# Exit codes:
#   0  smoke green
#   1  prerequisite missing
#   2  smoke fail (output mismatch / bundled emacs not used / etc.)

set -euo pipefail

VERSION="${1:-stage-d-v2.0}"
PLATFORM="${2:-linux-x86_64}"

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
cd "$REPO_ROOT"

log() { printf "  \033[1;34m==>\033[0m %s\n" "$*"; }
err() { printf "  \033[1;31merror:\033[0m %s\n" "$*" >&2; }
ok()  { printf "  \033[1;32m✓\033[0m %s\n" "$*"; }

ARTIFACT_NAME="anvil-${VERSION}-${PLATFORM}"
TAR_FILE="dist/${ARTIFACT_NAME}.tar.gz"
SHA_FILE="dist/${ARTIFACT_NAME}.tar.gz.sha256"

log "Phase 7.5.3 stage-d-v2.0 bundled-tarball verify"
log "  tarball : $TAR_FILE"

if [[ ! -f "$TAR_FILE" ]]; then
  err "tarball missing — run 'make stage-d-v2-tarball' first."
  exit 1
fi
if [[ ! -f "$SHA_FILE" ]]; then
  err "checksum missing at $SHA_FILE"
  exit 1
fi

# 1. SHA-256 verification (= mkdir + sha + diff, no hard dep on
#    sha256sum --check which differs across BSD / GNU).
log "verifying SHA-256"
RECORDED=$(awk '{print $1}' "$SHA_FILE")
if command -v sha256sum >/dev/null 2>&1; then
  RECOMPUTED=$(sha256sum "$TAR_FILE" | awk '{print $1}')
elif command -v shasum >/dev/null 2>&1; then
  RECOMPUTED=$(shasum -a 256 "$TAR_FILE" | awk '{print $1}')
else
  err "neither sha256sum nor shasum found"
  exit 1
fi
if [[ "$RECORDED" != "$RECOMPUTED" ]]; then
  err "SHA-256 mismatch: recorded=$RECORDED recomputed=$RECOMPUTED"
  exit 2
fi
ok "SHA-256 matches ($RECORDED)"

# 2. Extract to a fresh temp dir.
TEST_ROOT="/tmp/test-anvil-$$"
trap 'rm -rf "$TEST_ROOT"' EXIT
mkdir -p "$TEST_ROOT"
log "extracting to $TEST_ROOT"
tar -xzf "$TAR_FILE" -C "$TEST_ROOT"

INSTALL_DIR="$TEST_ROOT/$ARTIFACT_NAME"
if [[ ! -d "$INSTALL_DIR" ]]; then
  err "expected $INSTALL_DIR after extract"
  exit 2
fi
if [[ ! -x "$INSTALL_DIR/bin/anvil" ]]; then
  err "bin/anvil missing or not executable"
  exit 2
fi
if [[ ! -x "$INSTALL_DIR/emacs/bin/emacs" ]]; then
  err "emacs/bin/emacs missing or not executable"
  exit 2
fi
ok "tarball layout OK (bin/anvil + emacs/bin/emacs present + executable)"

# 3. Run `bin/anvil version` and assert the bundled-emacs path.
#    Strip $EMACS env var so `detect_emacs' walks the resolution chain
#    naturally; do NOT wipe PATH because the launcher relies on common
#    POSIX utilities.
log "running bin/anvil version (with stripped EMACS env)"
VERSION_OUT=$(env -u EMACS ANVIL_HOME="$INSTALL_DIR" \
  "$INSTALL_DIR/bin/anvil" version 2>&1 || true)

echo "$VERSION_OUT" | sed 's/^/      /'

# 4. Assertions.
if ! echo "$VERSION_OUT" | grep -q "tools registered"; then
  err "missing 'tools registered' line"
  exit 2
fi
ok "tools registered line present"

if ! echo "$VERSION_OUT" | grep -q "(bundled)"; then
  err "expected emacs source 'bundled' but version output reports system path"
  err "(this means detect_emacs fell through to system PATH instead of the"
  err " bundled \$ANVIL_HOME/emacs/bin/emacs — likely a regression in the"
  err " resolution order or a missing executable bit on the bundled binary)"
  exit 2
fi
ok "bundled emacs detected and reported"

if ! echo "$VERSION_OUT" | grep -q "emacs path .*$INSTALL_DIR/emacs/bin/emacs"; then
  err "emacs path line does not point at the bundled binary"
  exit 2
fi
ok "emacs path = bundled binary"

# 5. Optional `doctor' smoke (catches lisp-load regressions).
log "running bin/anvil doctor"
DOCTOR_OUT=$(env -u EMACS ANVIL_HOME="$INSTALL_DIR" \
  "$INSTALL_DIR/bin/anvil" doctor 2>&1 || true)
echo "$DOCTOR_OUT" | sed 's/^/      /'
if ! echo "$DOCTOR_OUT" | grep -q "ready for 'anvil mcp serve'"; then
  err "doctor did not report ready state"
  exit 2
fi
ok "doctor green"

echo ""
ok "Phase 7.5.3 stage-d-v2.0 bundled-tarball smoke PASS"
