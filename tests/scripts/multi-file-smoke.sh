#!/usr/bin/env bash
set -euo pipefail

cd "$(git rev-parse --show-toplevel)"

cargo build --workspace --release --quiet

NELISP=target/release/nelisp
RUNTIME=target/release/nelisp-runtime
image=/tmp/doc47-stage8b-multifile.bin

rm -f "$image"

if ! "$NELISP" mint-eval-file tests/fixtures/multi-file/main.el "$image" >/dev/null 2>&1; then
    echo "SKIP: load/require not yet supported (Claude branch unmerged)"
    exit 77
fi

exit_code=0
"$RUNTIME" boot-from-image "$image" || exit_code=$?
if [[ "$exit_code" -ne 25 ]]; then
    echo "FAIL: multi-file expected 25 got $exit_code" >&2
    exit 1
fi

echo "OK: multi-file -> exit 25"
