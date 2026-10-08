#!/usr/bin/env bash
set -euo pipefail
root=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/.." && pwd)
export F1_FORCE_GC=1
"$root/test/standalone-bytecode-native-funcall-smoke.sh" --both "${1:-target/nelisp-static}" "${2:-target/nelisp-dyn}"
