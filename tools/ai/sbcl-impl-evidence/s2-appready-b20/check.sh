#!/bin/sh
# S2.5 re-validation from raw data (see check.py). Usage: check.sh [DIR]
exec python3 -I "$(dirname "$0")/check.py" "${1:-$(cd "$(dirname "$0")" && pwd)}"
