#!/usr/bin/env bash
set -euo pipefail
root=$(cd "$(dirname "$0")/../.." && pwd)
bin=${NELISP_BIN:?set NELISP_BIN to the standalone binary}
bundle=${NEMACS_BOOTSTRAP_BUNDLE:?set NEMACS_BOOTSTRAP_BUNDLE to the generated S1.4 bundle}
out=${DOC211_VENDOR_TRACE_OUT:-$root/target/doc211-s5-vendor-trace.log}
mkdir -p "$(dirname "$out")"
stdout_log="$out.stdout"
backup=$(mktemp)
cp -- "$bundle" "$backup"
restore_bundle() { cp -- "$backup" "$bundle"; rm -f -- "$backup"; }
trap restore_bundle EXIT

# Trace the replay itself.  Each generated bundle section is one source file;
# a timestamp before every section records the prior file's elapsed cost.
python3 - "$bundle" "$bundle" <<'PY'
from pathlib import Path
import re
import sys

source = Path(sys.argv[1]).read_text(encoding="latin1")
out = []
offset = 0
for index, match in enumerate(re.finditer(r"^;;; >>> (.+)$", source, re.M)):
    out.append(source[offset:match.start()])
    name = match.group(1).replace('\\', '\\\\').replace('"', '\\"')
    out.append(
        f'\n(princ (format "SEG {index} {name} %S\\n" (current-time)))\n'
    )
    offset = match.start()
out.append(source[offset:])
Path(sys.argv[2]).write_text("".join(out), encoding="latin1")
PY

set +e
cd "$(dirname "$(dirname "$bundle")")"
timeout 50 "$bin" --eval \
  "(progn (load \"$bundle\" nil t) (princ \"BOOT-OK\"))" \
  >"$stdout_log" 2>&1
status=$?
set -e
if (( status != 0 )); then
  echo "FAIL replay/trace status=$status stdout=$stdout_log" >&2
  exit "$status"
fi
if ! grep -q BOOT-OK "$stdout_log"; then
  echo "FAIL replay completion marker absent (inspect $stdout_log)" >&2
  exit 1
fi
grep '^SEG ' "$stdout_log" >"$out"
echo "PASS traced replay $(grep -c '^SEG ' "$out") file sections; trace=$out stdout=$stdout_log"
