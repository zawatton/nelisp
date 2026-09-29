#!/bin/sh
# S7.7.4: a crashing artifact inside the parallel/batched crash-corpus gate
# must be reported against its own name and must fail the gate, without
# masking the other artifacts' results.  A fake NELISP_BIN wrapper execs the
# real binary but dies with SIGSEGV's exit status (139) for one injected
# (label, check) pair -- in its isolated process (corrupt-text) and, in a
# second run, in the shared safe-batch process holding its `normal' check.
set -u
script_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
repo=$(CDPATH= cd -- "$script_dir/.." && pwd)
real=${NELISP_BIN:-$repo/target/nelisp}
[ -x "$real" ] || { echo "NELISP_BIN not executable: $real" >&2; exit 2; }
tmp=$(mktemp -d "${TMPDIR:-/tmp}/eln-gate-attr.XXXXXX")
trap 'rm -rf "$tmp"' EXIT HUP INT TERM
[ -f "$real.cold" ] && ln -s "$real.cold" "$tmp/fake-nelisp.cold"
cat >"$tmp/fake-nelisp" <<W
#!/bin/sh
if [ -n "\${FAKE_CRASH_LABEL:-}" ]; then
  if [ "\${NELISP_ELN_CRASH_CORPUS_LABEL:-}" = "\$FAKE_CRASH_LABEL" ]; then exit 139; fi
  if [ -n "\${NELISP_ELN_CRASH_CORPUS_BATCH_FILE:-}" ] && \\
     grep -q "^\${FAKE_CRASH_LABEL%.*}	\${FAKE_CRASH_LABEL##*.}	" "\$NELISP_ELN_CRASH_CORPUS_BATCH_FILE"; then exit 139; fi
fi
exec "$real" "\$@"
W
chmod +x "$tmp/fake-nelisp"
rc_all=0
run_case() {
  name=$1 label=$2
  out=$tmp/$name.out
  FAKE_CRASH_LABEL=$label NELISP_BIN=$tmp/fake-nelisp \
    NELISP_ELN_CRASH_CORPUS_LOG_ROOT=$tmp/log sh "$repo/tools/nelisp-eln-crash-corpus-gate.sh" >"$out" 2>"$out.err"
  rc=$?
  base=${label%.*} kind=${label##*.}
  if [ "$rc" -eq 0 ]; then echo "FAIL $name: gate passed despite injected crash" >&2; rc_all=1; return; fi
  if ! grep -Eq "^$base +$kind +CRASH " "$out"; then
    echo "FAIL $name: $label not reported as CRASH" >&2; rc_all=1; return
  fi
  if ! grep -Eq '^NELISP-ELN-CRASH-CORPUS-GATE total=[0-9]+ fail=[1-9]' "$out" || grep -q 'GATE-PASS' "$out"; then
    echo "FAIL $name: bad summary" >&2; rc_all=1; return
  fi
  # Other artifacts' isolated checks must still have run and passed.
  if ! grep -Eq '^[a-z0-9-]+ +corrupt-text +PASS ' "$out"; then
    echo "FAIL $name: crash masked other artifacts" >&2; rc_all=1; return
  fi
  echo "ok $name ($label attributed, rc=$rc)"
}
run_case isolated gnu-increment.corrupt-text
run_case batched gnu-increment.normal
[ "$rc_all" -eq 0 ] && echo NELISP-ELN-CRASH-CORPUS-GATE-ATTRIBUTION-PASS
exit "$rc_all"
