#!/usr/bin/env bash
set -euo pipefail

script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
binary="${NELISP_BIN:?set NELISP_BIN to the standalone NeLisp executable}"
exec "$binary" --load "$script_dir/nelisp-bytecode-native-cfg-switch-smoke.el"
