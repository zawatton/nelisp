#!/usr/bin/env bash
# S6.3 probe-only negative control: the chunk-growth probe (same expression as
# tools/chunk-growth-test.sh) run against the DEFAULT 256 MiB-arena reader must
# read chunk-count=1, so the script's "chunk-count > 1" assertion would FAIL
# there.  Fails when target/nelisp is absent; builds nothing.
cd "$(dirname "$0")/../../.." || exit 2
[ -x target/nelisp ] || { echo "FAIL: target/nelisp is absent"; exit 1; }
w=$(mktemp -d "${TMPDIR:-/tmp}/probe-default.XXXXXX") || exit 2
trap 'rm -rf "$w"' EXIT
sed -n '/^cat > "\$WORK\/stats.el" <<.EOF.$/,/^EOF$/p' tools/chunk-growth-test.sh | sed '1d;$d' > "$w/stats.el"
[ -s "$w/stats.el" ] || { echo "FAIL: probe expression not found in chunk-growth-test.sh"; exit 1; }
stats=$(timeout 40 ./target/nelisp --load "$w/stats.el" 2>&1 | head -1)
n=$(printf '%s' "$stats" | sed -E 's/[()]//g' | awk '{print $10}')
echo "default binary: chunk-count=${n:-?} ($stats)"
[ "$n" = 1 ]
