#!/bin/sh
# S1.1: struct-place lowering. Red (seed selector) -> void-function (setf ACCESSOR); green (fixed selector) equals the interpreted result.
cd "$(dirname "$0")/../../.." || exit 1
grep -q 'void-function .*setf' target/codex/s11-red.log || { echo "red log lacks the (setf) failure"; exit 1; }
cmp -s target/codex/s11-interp.log target/codex/s11-green.log || { echo "green differs from interpreted"; exit 1; }
grep -q 'passed .*struct-setf-lowers-to-record-set' target/codex/ert-selector-green.log || exit 1
grep -q 'passed .*struct-accessors-follow-the-prelude-layout' target/codex/ert-selector-green.log || exit 1
grep -q 'passed .*rejects-host-cl-struct-artifacts' target/codex/ert-selector-green.log || exit 1
echo "S1.1 ok"
