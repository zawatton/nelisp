#!/usr/bin/env bash
# Same ELF, distinct images, genuine launch/reap/cancel and generation safety.
set -euo pipefail
root=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/.." && pwd)
cd "$root"
exec python3 test/support/run-native-tier.py "$@"
