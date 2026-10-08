#!/usr/bin/env bash
# Audit-derived pending set; content/ABI caches; fresh bounded reload cohorts.
set -euo pipefail
root=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/.." && pwd)
cd "$root"
exec python3 test/support/run-native-corpus-u10.py "$@"
