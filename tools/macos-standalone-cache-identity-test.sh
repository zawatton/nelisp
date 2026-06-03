#!/usr/bin/env bash
# macOS arm64 standalone eval cache identity smoke.
#
# Builds target/nelisp-standalone-eval once from a clean macOS target cache,
# builds it again from cached units, and verifies both Mach-O images are
# byte-stable.  No Rust toolchain is used.
set -euo pipefail

EMACS="${EMACS:-emacs}"

while [ "$#" -gt 0 ]; do
  case "$1" in
    --emacs) EMACS="$2"; shift 2 ;;
    -h|--help) echo "usage: $0 [--emacs EMACS]"; exit 0 ;;
    *) echo "usage: $0 [--emacs EMACS]" >&2; exit 2 ;;
  esac
done

REPO_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
cd "$REPO_ROOT"

echo "--- macOS standalone cache identity smoke ---"
uname -a
"$EMACS" --version | head -1

export NELISP_STANDALONE_TARGET=macos-aarch64
export NELISP_FORM_OP="+"
export NELISP_FORM_A="1"
export NELISP_FORM_B="2"

EXE="$REPO_ROOT/target/nelisp-standalone-eval"
CACHE_DIR="$REPO_ROOT/target/standalone-units/macos-aarch64"

rm -rf "$CACHE_DIR"
rm -f "$EXE"

sha256_file() {
  if command -v shasum >/dev/null 2>&1; then
    shasum -a 256 "$1" | awk '{print $1}'
  elif command -v sha256sum >/dev/null 2>&1; then
    sha256sum "$1" | awk '{print $1}'
  else
    openssl dgst -sha256 "$1" | awk '{print $NF}'
  fi
}

build_eval() {
  local label="$1"
  "$EMACS" --batch -Q -L lisp -L src -L scripts \
    --eval '(setq load-prefer-newer t)' \
    -l nelisp-standalone-build \
    -f nelisp-standalone-build
  if [ ! -f "$EXE" ]; then
    echo "[macos-standalone-cache] FAIL: $label missing $EXE"
    exit 1
  fi
}

build_eval fresh
FRESH_HASH="$(sha256_file "$EXE")"

build_eval cached
CACHED_HASH="$(sha256_file "$EXE")"

if [ "$FRESH_HASH" != "$CACHED_HASH" ]; then
  echo "[macos-standalone-cache] FAIL: fresh/cache hash mismatch"
  echo "  fresh: $FRESH_HASH"
  echo "  cached: $CACHED_HASH"
  exit 1
fi

echo "[macos-standalone-cache] PASS: fresh/cache SHA256 $FRESH_HASH"
