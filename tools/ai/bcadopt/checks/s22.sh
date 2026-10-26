#!/bin/sh
cd "$(dirname "$0")/../../.." || exit 1
for p in 'weak 24/24 PASS' 'GNU 56/56 PASS' 'AFRT 24/25; unchanged exit-hook-result-gc RED' 'buffer 21/21 PASS'; do grep -q "$p" target/codex/fixtures.log || { echo "missing: $p"; exit 1; }; done
grep -q 'closurevm: PASS (24)' target/codex/win-check.log && grep -q 'switch corpus: PASS' target/codex/win-check.log && grep -q 'features: PASS' target/codex/win-check.log || exit 1
echo "S2.2 ok"
