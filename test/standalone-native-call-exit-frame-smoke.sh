#!/usr/bin/env bash
set -euo pipefail

script_dir="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
repo_root="$(cd -- "$script_dir/.." && pwd)"
binary="${1:?pass the already-built reader binary}"
expected_hash="${NELISP_EXPECTED_SHA256:?set NELISP_EXPECTED_SHA256}"
actual_hash=$(sha256sum "$binary" | cut -d ' ' -f 1)
[[ "$actual_hash" == "$expected_hash" ]] || {
  printf 'reader hash mismatch: expected %s, got %s\n' "$expected_hash" "$actual_hash" >&2
  exit 1
}

run_driver() {
  local lisp_dir=$1 expected=$2 actual
  actual=$("$binary" \
    -L "$lisp_dir" -L "$repo_root/src" -L "$repo_root/scripts" \
    -L "$repo_root/packages/nl-prelude/src" \
    --load "$repo_root/test/standalone-native-call-exit-frame-driver.el" \
    --eval '(princ (prin1-to-string (nelisp-test-native-call-exit-frame-smoke)))')
  [[ "$actual" == "$expected" ]] || {
    printf 'native call exit frame expected %s, got %s\n' "$expected" "$actual" >&2
    return 1
  }
}

run_driver "$repo_root/lisp" t

# Mutate status 2 to write the status slot. The unchanged-output assertion
# must turn this deliberately broken implementation red.
tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT
cp -a "$repo_root/lisp" "$tmp/lisp"
python3 - "$tmp/lisp/nelisp-native-load.el" <<'PY'
from pathlib import Path
import sys

path = Path(sys.argv[1])
source = path.read_text()
needle = "  (if (= status 2)\n      2\n    (let* ((env (aref frame 0))"
mutation = "  (if (= status 2)\n    (nelisp-native-load-box\n     (nelisp-native-load--call-exit-frame-slot frame 1) 2\n     (aref frame 0) (aref frame 1))\n    (let* ((env (aref frame 0))"
if source.count(needle) != 1:
    raise SystemExit("expected exactly one status-2 capture branch")
path.write_text(source.replace(needle, mutation, 1))
PY
if run_driver "$tmp/lisp" t 2>/dev/null; then
  echo 'native call exit frame red mutation was not detected' >&2
  exit 1
fi

printf 'native-call-exit-frame: PASS (status 0, rooted signal/throw GC identity, status 2 unchanged, stale ticket, red mutation)\n'
