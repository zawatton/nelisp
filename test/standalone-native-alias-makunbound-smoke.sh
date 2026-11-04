#!/usr/bin/env bash
set -euo pipefail
repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
: "${NELISP_BIN:?Set NELISP_BIN to the diagnostic standalone binary}"
: "${NELISP_EXPECTED_SHA256:?Set NELISP_EXPECTED_SHA256 to its reviewed SHA-256}"
case "$NELISP_BIN" in
  /*) binary="$NELISP_BIN" ;;
  *) binary="$repo_root/$NELISP_BIN" ;;
esac
if [[ ! -x "$binary" ]]; then
  printf 'missing or nonexecutable binary: %s\n' "$binary" >&2
  exit 2
fi
actual_sha="$(sha256sum "$binary" | awk '{print $1}')"
if [[ "$actual_sha" != "$NELISP_EXPECTED_SHA256" ]]; then
  printf 'binary SHA mismatch: expected %s got %s\n' "$NELISP_EXPECTED_SHA256" "$actual_sha" >&2
  exit 2
fi
tmp_root="${TMPDIR:-$repo_root/target/tmp}"
mkdir -p "$tmp_root"
tmpdir="$(mktemp -d "$tmp_root/native-alias-makunbound.XXXXXX")"
trap 'rm -rf "$tmpdir"' EXIT
status=0
timeout "${NELISP_ALIAS_TIMEOUT:-90}" "$binary" --batch --load "$repo_root/test/standalone-native-alias-makunbound-driver.el" >"$tmpdir/out" 2>"$tmpdir/err" || status=$?
if [[ "$status" -ne 0 || -s "$tmpdir/err" ]] || [[ "$(grep -Fx -c 'NATIVE-ALIAS-MAKUNBOUND-PASS' "$tmpdir/out" || true)" -ne 1 ]]; then
  printf 'alias makunbound smoke failed: status=%s stderr_bytes=%s\n' "$status" "$(wc -c <"$tmpdir/err")" >&2
  cat "$tmpdir/out" >&2
  cat "$tmpdir/err" >&2
  exit 1
fi
cat "$tmpdir/out"
