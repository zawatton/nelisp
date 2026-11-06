#!/usr/bin/env bash
# Usage: tools/ai/large-block-pin-probe.sh BINARY
root=$(cd "$(dirname "$0")/../.." && pwd)
bin=$(realpath "$1" 2>/dev/null)
[ -x "$bin" ] || { echo "usage: $0 BINARY" >&2; exit 2; }
tmp=$(mktemp -d) || exit 3
trap 'rm -rf "$tmp"' EXIT
echo "(load \"$root/tools/ai/large-block-pin-probe.el\")" > "$tmp/run.el"
(ulimit -v 8388608; cd "$tmp" && timeout 500 "$bin" --load "$tmp/run.el" 2>&1) | grep -E '^(size=|control-blind|int-held-lost)'
exit "${PIPESTATUS[0]}"
