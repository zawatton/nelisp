#!/usr/bin/env bash
# GNU bytecode, native-entry counters, exact results and independent mappings.
set -euo pipefail
root=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/.." && pwd)
cd "$root"
exec python3 test/support/run-native-template-trust.py --backend template "$@"
