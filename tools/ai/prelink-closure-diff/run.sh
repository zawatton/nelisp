#!/usr/bin/env bash
# Usage: run.sh EVIDENCE_DIR VARIANT [--tamper unit|manifest|data]
# VARIANT is rooted, constructor or f1.  EVIDENCE_DIR holds active-build.json,
# active-unit-metadata.json, generated-data-owner.json and the *.unit files;
# the source root is the parent of its "target" directory.
# Runs the Python script and the Elisp port on identical inputs and prints
# PRELINK-SAME (outputs and printed digests equal) or the head of the diff.
# With --tamper the inputs are copied and corrupted first; the result is
# REFUSAL-SAME when both implementations refuse, else REFUSAL-DIFFERS.
# Environment: EMACS (default emacs-31.1), PYSCRIPTS (default SRC/scripts, the
# build's own copies), WORK (default: fresh temp dir), COMPARE (optional file
# the Elisp output must also equal).
here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)" || exit 1
repo="$(cd "$here/../../.." && pwd)" || exit 1
dir="$(cd "${1:?usage: run.sh EVIDENCE_DIR VARIANT [--tamper unit|manifest|data]}" && pwd)" || exit 1
variant="${2:?variant}"
tamper="${4:-}"
emacs_bin="${EMACS:-emacs-31.1}"
work="${WORK:-$(mktemp -d)}"
mkdir -p "$work" || exit 1
ulimit -v 8388608
src="$(cd "$dir/../.." && pwd)" || exit 1
pyscripts="${PYSCRIPTS:-$src/scripts}"
[ -f "$pyscripts/nelisp-native-rooted-prelink-closure.py" ] || pyscripts="$repo/scripts"

case "$variant" in
  rooted) base=nelisp-native-rooted-prelink-closure ;;
  constructor) base=nelisp-native-compiler-constructor-prelink ;;
  f1) base=nelisp-native-compiler-f1-prelink ;;
  *) echo "unknown variant $variant" >&2; exit 2 ;;
esac

if [ -n "$tamper" ]; then
  # Copy the inputs (units must be real files: symlinks escape the owner directory).
  tdir="$work/evidence/target/$(basename "$dir")"
  mkdir -p "$tdir" "$work/evidence/scripts" || exit 1
  cp "$dir"/*.json "$tdir/" || exit 1
  cp "$dir"/*.unit "$tdir/" || exit 1
  cp -r "$src/lisp" "$work/evidence/" 2>/dev/null || true
  builder="$(python3 -c 'import json,sys;print(json.load(open(sys.argv[1]))["builder-source"])' "$dir/active-build.json")"
  mkdir -p "$work/evidence/$(dirname "$builder")" && cp "$src/$builder" "$work/evidence/$builder" || exit 1
  case "$tamper" in
    unit)
      unit="$tdir/rootstack.o.unit"
      python3 - "$unit" <<'PY' || exit 1
import sys
p = sys.argv[1]; b = bytearray(open(p, 'rb').read()); b[-2] ^= 1; open(p, 'wb').write(b)
PY
      ;;
    manifest)
      python3 - "$tdir/active-build.json" <<'PY' || exit 1
import json, sys
p = sys.argv[1]; d = json.load(open(p)); h = d["units"][0]["unit-sha256"]
d["units"][0]["unit-sha256"] = ("0" if h[0] != "0" else "1") + h[1:]
open(p, "w").write(json.dumps(d))
PY
      ;;
    data)
      python3 - "$tdir/generated-data-owner.json" <<'PY' || exit 1
import sys
p = sys.argv[1]; b = open(p, 'rb').read(); open(p, 'wb').write(b + b"\n")
PY
      ;;
    *) echo "unknown tamper $tamper" >&2; exit 2 ;;
  esac
  dir="$tdir"; src="$work/evidence"
fi

roots=()
if [ "$variant" = rooted ]; then
  for r in nl_root_pin_begin_v2 nl_root_pin_reserve_v2 nl_root_pin_end_v2 nl_root_pin_slot_v2 \
           nl_gc_mark_pinned_roots nl_gc_mark_thread_roots nl_gc_mark_recorded_env \
           nl_cold_grow_chunk0 nl_gc_conserv_owner_slow; do roots+=(--root "$r"); done
else
  while read -r r; do roots+=(--root "$r"); done < <(python3 - "$pyscripts/$base.py" <<'PY'
import importlib.util, sys
from pathlib import Path
spec = importlib.util.spec_from_file_location("p", sys.argv[1])
m = importlib.util.module_from_spec(spec)
spec.loader.exec_module(m)
print("\n".join(m.ROOTS))
PY
)
fi
args=(--manifest "$dir/active-build.json" --source-root "$src" --metadata "$dir/active-unit-metadata.json"
      --unit-directory "$dir" --generated-data "$dir/generated-data-owner.json")

pystatus=0; elstatus=0
py_t0="$(date +%s.%N)"
python3 "$pyscripts/$base.py" "${args[@]}" --output "$work/py.json" "${roots[@]}" >"$work/py.out" 2>"$work/py.err" || pystatus=$?
py_t1="$(date +%s.%N)"
(cd "$repo" && "$emacs_bin" --batch -Q -L lisp -l "$here/run-elisp.el" "$repo/scripts/$base.el" \
  "${args[@]}" --output "$work/el.json" "${roots[@]}") >"$work/el.out" 2>"$work/el.err" || elstatus=$?

if [ -n "$tamper" ]; then
  case "$tamper" in data) expect="Changed active metadata or data owner" ;;
    *) expect="Missing or changed active unit source" ;; esac
  if [ "$pystatus" -ne 0 ] && [ "$elstatus" -ne 0 ] \
     && grep -q "$expect" "$work/py.err" && grep -q "$expect" "$work/el.err"; then
    echo "REFUSAL-SAME ($tamper, $variant): python: $(tail -1 "$work/py.err")"
    echo "  elisp: $(grep -m1 ELISP-REFUSED "$work/el.err")"
  else
    echo "REFUSAL-DIFFERS ($tamper, $variant) python=$pystatus elisp=$elstatus"; exit 1
  fi
  exit 0
fi
if [ "$pystatus" -ne 0 ] || [ "$elstatus" -ne 0 ]; then
  echo "FAILED python=$pystatus elisp=$elstatus"; tail -n 3 "$work/py.err"; tail -n 3 "$work/el.err"; exit 1
fi
echo "python-seconds $(echo "$py_t1 - $py_t0" | bc)"
grep -h elisp-seconds "$work/el.err"
if cmp -s "$work/py.json" "$work/el.json" && cmp -s "$work/py.out" "$work/el.out"; then
  extra=""
  if [ -n "${COMPARE:-}" ]; then
    cmp -s "$COMPARE" "$work/el.json" && extra=" matches-build-output" || { echo "BUILD-OUTPUT-DIFFERS $COMPARE"; exit 1; }
  fi
  echo "PRELINK-SAME $variant $(tr -d '\n' <"$work/el.out")$extra"
else
  diff "$work/py.json" "$work/el.json" | head -20
  echo "digests: py=$(cat "$work/py.out") el=$(cat "$work/el.out")"
  exit 1
fi
