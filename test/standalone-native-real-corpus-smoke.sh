#!/usr/bin/env bash
# Genuine GNU source/byte-code pins, immutable readers, bounded cache cohorts.
set -euo pipefail
root=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/.." && pwd)
cd "$root"
exec python3 test/support/run-native-real-corpus.py "$@"
