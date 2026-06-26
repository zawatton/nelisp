#!/usr/bin/env bash
# Verify a zero-Rust standalone NeLisp reader tarball.
set -euo pipefail

default_platform() {
  case "$(uname -s 2>/dev/null || echo)-$(uname -m 2>/dev/null || echo)" in
    Darwin-arm64) echo "macos-aarch64" ;;
    Linux-x86_64) echo "linux-x86_64" ;;
    MINGW*-x86_64|MSYS*-x86_64|CYGWIN*-x86_64) echo "windows-x86_64" ;;
    *) echo "linux-x86_64" ;;
  esac
}

VERSION="${1:-v0.6.0}"
PLATFORM="${2:-${NELISP_STANDALONE_TARGET:-$(default_platform)}}"
LAYOUT_ONLY=0

if [ "$#" -gt 0 ]; then shift; fi
if [ "$#" -gt 0 ]; then shift; fi
while [ "$#" -gt 0 ]; do
  case "$1" in
    --layout-only) LAYOUT_ONLY=1; shift ;;
    -h|--help)
      echo "usage: $0 [VERSION] [PLATFORM] [--layout-only]"
      exit 0
      ;;
    *) echo "usage: $0 [VERSION] [PLATFORM] [--layout-only]" >&2; exit 2 ;;
  esac
done

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
cd "$REPO_ROOT"

log() { printf "  \033[1;34m==>\033[0m %s\n" "$*"; }
err() { printf "  \033[1;31merror:\033[0m %s\n" "$*" >&2; }
ok()  { printf "  \033[1;32mOK\033[0m %s\n" "$*"; }

ARTIFACT_NAME="anvil-${VERSION}-${PLATFORM}"
TAR_FILE="dist/${ARTIFACT_NAME}.tar.gz"
SHA_FILE="dist/${ARTIFACT_NAME}.tar.gz.sha256"

case "$PLATFORM" in
  windows-x86_64) NELISP_BIN_NAME="nelisp.exe" ;;
  linux-x86_64|macos-aarch64) NELISP_BIN_NAME="nelisp" ;;
  *) err "unsupported platform: $PLATFORM"; exit 2 ;;
esac

[[ -f "$TAR_FILE" ]] || { err "tarball missing - run tools/build-standalone-tarball.sh first"; exit 1; }
[[ -f "$SHA_FILE" ]] || { err "checksum missing: $SHA_FILE"; exit 1; }

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
[[ "$RECORDED" == "$RECOMPUTED" ]] || { err "SHA mismatch"; exit 2; }
ok "SHA-256 matches ($RECORDED)"

TEST_ROOT="$(mktemp -d -t nelisp-standalone-verify-XXXXXX)"
trap 'rm -rf "$TEST_ROOT"' EXIT
tar -xzf "$TAR_FILE" -C "$TEST_ROOT"
INSTALL_DIR="$TEST_ROOT/$ARTIFACT_NAME"
NELISP_EXE="$INSTALL_DIR/bin/$NELISP_BIN_NAME"

[[ -d "$INSTALL_DIR/src" ]] || { err "src/ missing"; exit 2; }
[[ -d "$INSTALL_DIR/scripts" ]] || { err "scripts/ missing"; exit 2; }
[[ -d "$INSTALL_DIR/lisp" ]] || { err "lisp/ missing"; exit 2; }
[[ -f "$INSTALL_DIR/VERSION" ]] || { err "VERSION missing"; exit 2; }
[[ -f "$INSTALL_DIR/PLATFORM" ]] || { err "PLATFORM missing"; exit 2; }
[[ -f "$INSTALL_DIR/MANIFEST.txt" ]] || { err "MANIFEST.txt missing"; exit 2; }
[[ -f "$NELISP_EXE" ]] || { err "bin/$NELISP_BIN_NAME missing"; exit 2; }
grep -qx "$VERSION" "$INSTALL_DIR/VERSION" || { err "VERSION mismatch"; exit 2; }
grep -qx "$PLATFORM" "$INSTALL_DIR/PLATFORM" || { err "PLATFORM mismatch"; exit 2; }
grep -q "standalone bin/$NELISP_BIN_NAME" "$INSTALL_DIR/MANIFEST.txt" || {
  err "MANIFEST missing standalone bin entry"
  exit 2
}
ok "tarball layout OK"

if [ "$LAYOUT_ONLY" -eq 1 ]; then
  ok "layout-only PASS for $PLATFORM"
  exit 0
fi

host_can_run=0
case "$PLATFORM" in
  linux-x86_64)
    [ "$(uname -s)" = "Linux" ] && [ "$(uname -m)" = "x86_64" ] && host_can_run=1
    ;;
  macos-aarch64)
    [ "$(uname -s)" = "Darwin" ] && [ "$(uname -m)" = "arm64" ] && host_can_run=1
    ;;
  windows-x86_64)
    case "$(uname -s 2>/dev/null || echo)" in
      MINGW*|MSYS*|CYGWIN*) host_can_run=1 ;;
    esac
    ;;
esac

if [ "$host_can_run" -ne 1 ]; then
  ok "layout-only PASS for non-native platform $PLATFORM"
  exit 0
fi

if [ "$PLATFORM" = "macos-aarch64" ]; then
  if ! command -v codesign >/dev/null 2>&1; then
    err "codesign is required to verify macOS arm64 tarballs"
    exit 2
  fi
  codesign --verify "$NELISP_EXE" >/dev/null || {
    err "bin/$NELISP_BIN_NAME is not signed; rebuild the tarball on macOS arm64"
    exit 2
  }
  ok "bin/$NELISP_BIN_NAME code signature OK"
fi
chmod +x "$NELISP_EXE" 2>/dev/null || true

run_expect_output() {
  local label="$1" expected="$2"; shift 2
  local output code
  set +e
  output="$("$@")"
  code=$?
  set -e
  if [ "$code" -ne 0 ]; then
    err "$label exited $code"
    printf '%s\n' "$output"
    exit 2
  fi
  if [ "$output" != "$expected" ]; then
    err "$label output mismatch"
    printf 'expected: %s\nactual  : %s\n' "$expected" "$output"
    exit 2
  fi
  ok "$label"
}

run_expect_output "bin/$NELISP_BIN_NAME --eval" "42" "$NELISP_EXE" --eval "(+ 40 2)"

REPL_OUTPUT="$(printf '%s\n' \
  "(+ 40 2)" \
  '(vector 1 "a" nil t)' \
  "(exit)" | "$NELISP_EXE" --repl --no-prompt)"
EXPECTED_REPL=$'42\n[1 "a" nil t]'
if [ "$REPL_OUTPUT" != "$EXPECTED_REPL" ]; then
  err "bin/$NELISP_BIN_NAME repl output mismatch"
  printf 'expected:\n%s\nactual:\n%s\n' "$EXPECTED_REPL" "$REPL_OUTPUT"
  exit 2
fi
ok "bin/$NELISP_BIN_NAME --repl"

echo ""
ok "zero-Rust standalone tarball smoke PASS"
