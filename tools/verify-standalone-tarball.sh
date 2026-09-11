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

. "$(dirname "${BASH_SOURCE[0]}")/nelisp-version.sh"
POSITIONAL=()
LAYOUT_ONLY=0
RELEASE_ARTIFACT=0

while [ "$#" -gt 0 ]; do
  case "$1" in
    --layout-only) LAYOUT_ONLY=1; shift ;;
    --release-artifact) RELEASE_ARTIFACT=1; shift ;;
    -h|--help)
      echo "usage: $0 [VERSION] [PLATFORM] [--layout-only] [--release-artifact]"
      exit 0
      ;;
    --)
      shift
      while [ "$#" -gt 0 ]; do
        POSITIONAL+=("$1")
        shift
      done
      ;;
    --*) echo "usage: $0 [VERSION] [PLATFORM] [--layout-only] [--release-artifact]" >&2; exit 2 ;;
    *) POSITIONAL+=("$1"); shift ;;
  esac
done

if [ "${#POSITIONAL[@]}" -gt 2 ]; then
  echo "usage: $0 [VERSION] [PLATFORM] [--layout-only] [--release-artifact]" >&2
  exit 2
fi
VERSION="${POSITIONAL[0]:-$(nelisp_version)}"
PLATFORM="${POSITIONAL[1]:-${NELISP_STANDALONE_TARGET:-$(default_platform)}}"

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
cd "$REPO_ROOT"

log() { printf "  \033[1;34m==>\033[0m %s\n" "$*"; }
err() { printf "  \033[1;31merror:\033[0m %s\n" "$*" >&2; }
ok()  { printf "  \033[1;32mOK\033[0m %s\n" "$*"; }

if [ "$RELEASE_ARTIFACT" -eq 1 ]; then
  # build-release-artifact.sh intentionally omits the historical `anvil-'
  # prefix.  Keep that naming contract explicit at the verifier boundary.
  ARTIFACT_NAME="${VERSION}-${PLATFORM}"
else
  ARTIFACT_NAME="anvil-${VERSION}-${PLATFORM}"
fi
TAR_FILE="dist/${ARTIFACT_NAME}.tar.gz"
SHA_FILE="dist/${ARTIFACT_NAME}.tar.gz.sha256"

case "$PLATFORM" in
  windows-x86_64) NELISP_BIN_NAME="nelisp.exe" ;;
  linux-x86_64|macos-aarch64) NELISP_BIN_NAME="nelisp" ;;
  macos-arm64|linux-arm64|linux-aarch64)
    NELISP_BIN_NAME="nelisp"
    ;;
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
[[ -x "$INSTALL_DIR/tools/ai/nelisp-ai.sh" ]] || { err "tools/ai/nelisp-ai.sh missing or not executable"; exit 2; }
[[ -f "$INSTALL_DIR/tools/ai/README.md" ]] || { err "tools/ai/README.md missing"; exit 2; }
[[ -f "$INSTALL_DIR/docs/repl-development.md" ]] || { err "docs/repl-development.md missing"; exit 2; }
[[ -f "$INSTALL_DIR/packages/nelisp-eventloop/src/nelisp-async-core.el" ]] || { err "eventloop runtime source missing"; exit 2; }
[[ -f "$INSTALL_DIR/packages/nelisp-process-adapter/src/nelisp-process-adapter.el" ]] || { err "process adapter runtime source missing"; exit 2; }
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
  linux-arm64|linux-aarch64)
    [ "$(uname -s)" = "Linux" ] && [ "$(uname -m)" = "aarch64" ] && host_can_run=1
    ;;
  macos-arm64|macos-aarch64)
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

if [ "$PLATFORM" = "macos-arm64" ] || [ "$PLATFORM" = "macos-aarch64" ]; then
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

REPL_DEV_INPUT="$TEST_ROOT/repl-dev-input.el"
REPL_DEV_OUTPUT="$TEST_ROOT/repl-dev-output"
REPL_DEV_ERR="$TEST_ROOT/repl-dev-stderr"
printf '%s\n' "(progn (require 'nelisp-repl-development) (unless (fboundp 'nelisp-repl-session-call) (error \"session API missing\")) (unless (= (nelisp-repl-session-call 'identity 41) 41) (error \"session call value mismatch\")) (nelisp--write-stdout-bytes \"REPL_DEV_BUNDLE_PASS\\n\") (exit))" > "$REPL_DEV_INPUT"
set +e
(cd "$INSTALL_DIR" && NELISP_BIN="$NELISP_EXE" tools/ai/nelisp-ai.sh repl --no-prompt < "$REPL_DEV_INPUT" > "$REPL_DEV_OUTPUT" 2> "$REPL_DEV_ERR")
REPL_DEV_STATUS=$?
set -e
if [ "$REPL_DEV_STATUS" -ne 0 ] || [ -s "$REPL_DEV_ERR" ] || ! grep -Fxq 'REPL_DEV_BUNDLE_PASS' "$REPL_DEV_OUTPUT"; then
  err "bundled REPL development entry point failed"
  cat "$REPL_DEV_OUTPUT" "$REPL_DEV_ERR"
  exit 2
fi
ok "bundled REPL development entry point"

sed 's/(nelisp-repl-session-call '\''identity 41) 41)/(nelisp-repl-session-call '\''identity 41) 42)/' "$REPL_DEV_INPUT" > "$TEST_ROOT/repl-dev-mutated.el"
set +e
(cd "$INSTALL_DIR" && NELISP_BIN="$NELISP_EXE" tools/ai/nelisp-ai.sh repl --no-prompt < "$TEST_ROOT/repl-dev-mutated.el" > "$TEST_ROOT/repl-dev-mutated-output" 2> "$TEST_ROOT/repl-dev-mutated-stderr")
set -e
if grep -Fxq 'REPL_DEV_BUNDLE_PASS' "$TEST_ROOT/repl-dev-mutated-output"; then
  err "mutated bundled REPL check unexpectedly passed"
  exit 2
fi
if ! grep -Fq 'session call value mismatch' "$TEST_ROOT/repl-dev-mutated-stderr"; then
  err "mutated bundled REPL failed without the expected assertion"
  cat "$TEST_ROOT/repl-dev-mutated-stderr"
  exit 2
fi
ok "bundled REPL mutation check went red"

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
