#!/usr/bin/env bash
set -euo pipefail

repo_root=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/.." && pwd)
cd "$repo_root"
out=${NELISP_CALL_FIXTURE_OUTPUT:-target/progress/call-provider-r8g}
frozen=${NELISP_CALL_FROZEN_UNITS:-../recursion-r8d-20261003/target/standalone-rooted-protocol-Sa9a6M}
guard=${NELISP_CALL_GUARD:-../native-alias-value-20261001/target/progress/production-sol-struct-gv-r1/s5-original-bounded-native-probe.py}
lock_directory=${NELISP_CALL_LOCK_DIRECTORY:-/tmp/nelisp-native-probes-1000}
mkdir -p "$out"

guarded() {
  local result=$1 seconds=$2
  shift 2
  local attempt rc
  for ((attempt=0; attempt<=40; attempt++)); do
    rc=0
    python3 "$guard" --output "$result" --seconds "$seconds" \
      --lock-directory "$lock_directory" -- "$@" || rc=$?
    if [[ $rc != 125 ]]; then return "$rc"; fi
    if [[ $attempt == 40 ]]; then return 125; fi
    sleep 15
  done
}

# Parse all authored scripts with the host before spending a native launch.
emacs -Q --batch --eval \
  '(dolist (file (quote ("lisp/nelisp-native-call-v2.el" "test/standalone-native-call-v2-driver.el" "test/support/native-call-v2-fixture.el"))) (with-temp-buffer (insert-file-contents file) (emacs-lisp-mode) (check-parens)))'
NELISP_CALL_CALLEES_ONLY=1 NELISP_CALL_FIXTURE_OUTPUT="$out" \
  emacs -Q --batch -L lisp -L src -L scripts -l test/support/native-call-v2-fixture.el

for variant in ${NELISP_CALL_SMOKE_VARIANTS:-good no-staging no-exit-copy}; do
  image="$out/nelisp-call-$variant"
  if [[ ! -x "$image" || ! -f "$out/$variant-addresses.el" ]]; then
    guarded "$out/build-$variant.json" 600 env \
      NELISP_CALL_FROZEN_UNITS="$frozen" NELISP_CALL_FIXTURE_OUTPUT="$out" \
      NELISP_CALL_VARIANT="$variant" \
      emacs -Q --batch -L lisp -L src -L scripts \
      --eval '(setq print-length 8 print-level 4)' \
      -l test/support/native-call-v2-fixture.el
  fi
  rc=0
  guarded "$out/native-$variant.json" 90 env \
    NELISP_CALL_VARIANT="$variant" \
    NELISP_CALL_ADDRESS_FILE="$out/$variant-addresses.el" \
    NELISP_CALL_CALLEES="$out/native-call-callees.el" \
    "$image" -L lisp -L src -L scripts \
    --load test/standalone-native-call-v2-driver.el || rc=$?
  if [[ "$variant" == good ]]; then
    [[ $rc == 0 ]]
    rg -q '^native-call-v2: PASS ' "$out/native-good.json.stdout"
    [[ ! -s "$out/native-good.json.stderr" ]]
  else
    label='GC argument identity'
    if [[ "$variant" == no-exit-copy ]]; then label='exit value identity'; fi
    # A substantive semantic assertion is required; guard termination is red.
    python3 - "$out/native-$variant.json.status.json" <<'PY'
import json, sys
result = json.load(open(sys.argv[1]))
assert result['launched'] and result['guard'] is None, result
PY
    rg -q "native-call-v2 assertion: $label" \
      "$out/native-$variant.json.stdout" "$out/native-$variant.json.stderr"
    printf 'native-call-v2 negative control: %s RED (%s)\n' "$variant" "$label"
  fi
done
rg '^native-call-v2: PASS ' "$out/native-good.json.stdout"
