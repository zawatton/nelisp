#!/bin/sh
# A loadable standalone ERT driver must not skip the files after it.
set -eu
root=$(cd "$(dirname "$0")/.." && pwd)
case_dir=$(mktemp -d "${TMPDIR:-/tmp}/nelisp-ert-collection.XXXXXX")
trap 'rm -rf "$case_dir"' EXIT HUP INT TERM
cat > "$case_dir/early-test.el" <<'EL'
;;; -*- lexical-binding: t; -*-
(require 'ert)
(ert-deftest collection-early () (should t))
(ert-run-tests-batch-and-exit)
EL
cat > "$case_dir/late-test.el" <<'EL'
;;; -*- lexical-binding: t; -*-
(require 'ert)
(ert-deftest collection-late () (should nil))
EL
if NELISP_GATE_DIR="$case_dir/red" sh "$root/tools/ai/nelisp-ai.sh" test-one \
    "$case_dir/early-test.el" "$case_dir/late-test.el" > "$case_dir/red.log" 2>&1; then
  echo 'ERT-COLLECTION FAIL: a failing later file was skipped'
  exit 1
fi
python3 - "$case_dir/red/ert-focus.json" <<'PY'
import json, sys
report = json.load(open(sys.argv[1]))
assert report['ran'] == 2 and report['failed'] == 1, report
PY
cat > "$case_dir/late-test.el" <<'EL'
;;; -*- lexical-binding: t; -*-
(require 'ert)
(ert-deftest collection-late () (should t))
EL
NELISP_GATE_DIR="$case_dir/green" sh "$root/tools/ai/nelisp-ai.sh" test-one \
    "$case_dir/early-test.el" "$case_dir/late-test.el" > "$case_dir/green.log" 2>&1
python3 - "$case_dir/green/ert-focus.json" <<'PY'
import json, sys
report = json.load(open(sys.argv[1]))
assert report['ran'] == 2 and report['failed'] == 0, report
PY
echo 'ERT-COLLECTION checks=2 negative=FAIL positive=PASS'
