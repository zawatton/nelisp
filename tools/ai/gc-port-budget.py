#!/usr/bin/env python3
"""GNU's consing_until_gc, the allocation budget a VM poll tests (gc-port lane).

  gc-port-budget.py check --dyn BIN --static BIN [--work DIR]

The binaries must carry a bytecode VM that polls with (if (< nl_gc_until 0) (nl_gc_vm_poll) 0) at every call
(tools/ai/gc-port-vm-poll-reference.patch).  For each binary: tools/ai/gc-port-budget.el with GCB_MODE=run (the
armed state) and fresh (the unarmed state).  Every run is capped at ulimit -v 8 GiB and 300 s and must print exactly
one "GCB MODE checks=N failures=0 PASS".  Prints one line per run and "GC-PORT-BUDGET PASS" or "... FAIL"; exit 0
on pass."""
import argparse
import os
import subprocess
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
SOURCE = ROOT / 'tools/ai/gc-port-budget.el'


def run(binary, mode, log):
    env = dict(os.environ, GCB_MODE=mode, GCB_ELD=str(ROOT / 'test/gc-port-vm-poll/funcall.eld'))
    cmd = ['bash', '-c', 'ulimit -v 8388608; exec timeout 300 "$@"', 'gcb', str(binary), '--load', str(SOURCE)]
    proc = subprocess.run(cmd, cwd=ROOT, env=env, capture_output=True)
    log.write_bytes(proc.stdout + b'\n--- stderr ---\n' + proc.stderr)
    lines = proc.stdout.decode(errors='replace').splitlines()
    summary = [ln for ln in lines if ln.startswith(f'GCB {mode} ')]
    last = summary[-1] if summary else (lines[-1] if lines else '')
    ok = proc.returncode == 0 and len(summary) == 1 and last.endswith(' failures=0 PASS')
    fails = [ln for ln in lines if ln.startswith('GCB FAIL')]
    info = [ln[4:] for ln in lines if ln.startswith('GCB INFO')]
    print(f"{'PASS' if ok else 'FAIL'} {binary.name} {mode}: rc={proc.returncode} "
          f"{last[:80]}{' ' + '; '.join(fails)[:600] if fails else ''}{' ' + '; '.join(info)[:200] if info else ''}")
    return ok


def main(argv):
    ap = argparse.ArgumentParser()
    ap.add_argument('command', choices=['check'])
    ap.add_argument('--dyn', required=True, type=Path)
    ap.add_argument('--static', required=True, type=Path)
    ap.add_argument('--work', type=Path, default=ROOT / 'target/gc-port/budget')
    a = ap.parse_args(argv)
    a.work.mkdir(parents=True, exist_ok=True)
    ok = True
    for name, binary in (('dyn', a.dyn.resolve()), ('static', a.static.resolve())):
        for mode in ('run', 'fresh'):
            ok &= run(binary, mode, a.work / f'{name}-{mode}.log')
    print('GC-PORT-BUDGET ' + ('PASS' if ok else 'FAIL'))
    return 0 if ok else 1


if __name__ == '__main__':
    sys.exit(main(sys.argv[1:]))
