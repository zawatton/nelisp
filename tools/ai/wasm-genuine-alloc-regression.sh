#!/usr/bin/env bash
# Run under heavy-run and the 8 GiB virtual-memory cap; children keep its slot.
set -u
phase=${1:?before or after}
out=target/wasm-genuine-alloc/$phase
mkdir -p "$out/tmp" target/wasm-rootstack target/wasm-target-init target/wasm-target-alloc
export TMPDIR="$PWD/$out/tmp"
prefix=(timeout 600 emacs -Q --batch -L lisp -L src -L test -L tools/ai --eval '(setq load-prefer-newer t)' -l lisp/nelisp-asm-wasm.el -l lisp/nelisp-aot-compiler.el)
status=0
run() {
 local name=$1; shift
 "$@" > "$out/$name.log" 2>&1
 local rc=$?
 echo "$name rc=$rc"
 echo "$rc" > "$out/$name.rc"
 if (( rc != 0 && status == 0 )); then status=$rc; fi
}
run_native() {
 local name=$1 directory=$2 extension=$3
 if (( status != 0 )); then exit "$status"; fi
 # Preserve raw stdout and the exit status consumed by the existing checkers.
 run "$name-native" bash -c '
  timeout 600 "$1/native-oracle" > "$1/native-results.$2"
  rc=$?
  echo "$rc" > "$1/native.rc"
  exit "$rc"
 ' _ "$directory" "$extension"
 if (( status != 0 )); then exit "$status"; fi
}
run rootstack-build "${prefix[@]}" -l wasm-rootstack-fixture -f wasm-rootstack-fixture-build
run cas-build "${prefix[@]}" -l wasm-cas-fixture -f wasm-cas-build-native
run_native cas target/wasm-cas txt
run cas-wasm-build "${prefix[@]}" -l wasm-cas-fixture -f wasm-cas-build-wasm
run shift-build "${prefix[@]}" -l wasm-shift-fixture -f wasm-shift-fixture-build-native
run_native shift target/wasm-runtime-shift bin
run shift-wasm-build "${prefix[@]}" -l wasm-shift-fixture -f wasm-shift-fixture-build-wasm
run atomic-build env WASM_RUNTIME_OPS_KIND=atomic "${prefix[@]}" -l wasm-runtime-ops-fixture -f wasm-runtime-ops-fixture-build
run bitwise-build env WASM_RUNTIME_OPS_KIND=bitwise "${prefix[@]}" -l wasm-runtime-ops-fixture -f wasm-runtime-ops-fixture-build
run init-build "${prefix[@]}" -l wasm-target-init-probe
run alloc-build "${prefix[@]}" -l wasm-target-alloc-probe
run full-probe "${prefix[@]}" -l wasm-rootstack-full-probe
for spec in rootstack:wasm-rootstack-check cas:wasm-cas-check shift:wasm-shift-check init:wasm-target-init-check alloc:wasm-target-alloc-check; do
 run "${spec%%:*}-check" timeout 600 node --disable-wasm-trap-handler "tools/ai/${spec#*:}.mjs"
done
for kind in atomic bitwise; do
 run "$kind-check" timeout 600 node --disable-wasm-trap-handler tools/ai/wasm-runtime-ops-check.mjs "$kind"
done
python3 - "$out" <<'PYCODE'
import sys, pathlib, shutil, hashlib, json
out=pathlib.Path(sys.argv[1]); rows={}
fixtures = ['wasm-rootstack/fixture.wasm', 'wasm-rootstack/full.wasm',
 'wasm-cas/fixture.wasm', 'wasm-runtime-shift/fixture.wasm',
 'wasm-runtime-ops/atomic.wasm', 'wasm-runtime-ops/bitwise.wasm',
 'wasm-target-init/init.wasm', 'wasm-target-init/full.wasm',
 'wasm-target-alloc/alloc.wasm', 'wasm-target-alloc/init.wasm']
for name in fixtures:
 f=pathlib.Path('target',name)
 dest=out/name; dest.parent.mkdir(exist_ok=True); shutil.copyfile(f,dest)
 rows[str(f)]=hashlib.sha256(f.read_bytes()).hexdigest()
(out/'hashes.json').write_text(json.dumps(rows,indent=2)+'\n')
PYCODE
rc=$?
if (( rc != 0 && status == 0 )); then status=$rc; fi
exit "$status"
