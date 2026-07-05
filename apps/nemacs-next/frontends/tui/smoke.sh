#!/usr/bin/env sh
set -eu

ROOT=${NEMACS_NEXT_ROOT:-$(CDPATH= cd -- "$(dirname -- "$0")/../../../.." && pwd -P)}
tmp_file=${TMPDIR:-/tmp}/nemacs-next-tui-smoke.$$.txt
out=${TMPDIR:-/tmp}/nemacs-next-tui-smoke.$$.out
rm -f "$tmp_file" "$out"
trap 'rm -f "$tmp_file" "$out"' EXIT

printf '' > "$tmp_file"

{
  printf '\030\006'
  printf '%s' "$tmp_file"
  printf '\r'
  printf 'abc'
  printf '\033[D'
  printf '\177'
  printf '\030\023'
  printf '\030\003'
} | NEMACS_NEXT_TUI_DRAW=never timeout 90s \
    "$ROOT/apps/nemacs-next/frontends/tui/nemacs-next-tui" > "$out" 2>&1

if [ "$(cat "$tmp_file")" != "ac" ]; then
  cat "$out" >&2
  echo "nemacs-next-tui-smoke: final file content mismatch" >&2
  exit 1
fi

if ! grep -q 'ac' "$out"; then
  cat "$out" >&2
  echo "nemacs-next-tui-smoke: rendered frame never showed edited text" >&2
  exit 1
fi

if ! grep -q "Wrote $tmp_file" "$out"; then
  cat "$out" >&2
  echo "nemacs-next-tui-smoke: save echo was not rendered" >&2
  exit 1
fi

echo "nemacs-next-tui-smoke: scripted terminal loop ok"
