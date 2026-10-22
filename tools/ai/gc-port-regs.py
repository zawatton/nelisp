#!/usr/bin/env python3
"""The collector scans the callee-saved registers (gc-port lane).

  gc-port-regs.py check --dyn BIN --static BIN

Runs tools/ai/gc-port-regs.el on each binary (ulimit -v 8 GiB, 120 s): five sentinels loaded
into rbx and r12-r15 must all appear in the window nl_gc_scan_regs hands the scan, a window of
the same size without the spill must show none (the negative control), and a garbage-collect
must reach the stub.  A target without the self-test fails the check.
Prints one line per binary and "GC-PORT-REGS PASS" or "... FAIL"; exit 0 on pass."""
import argparse
import subprocess
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
SOURCE = ROOT / 'tools/ai/gc-port-regs.el'


def run(binary):
    proc = subprocess.run(['bash', '-c', 'ulimit -v 8388608; exec timeout 120 "$@"', 'gcregs',
                           str(binary), '--load', str(SOURCE)], cwd=ROOT, capture_output=True)
    lines = proc.stdout.decode(errors='replace').splitlines()
    summary = [ln for ln in lines if ln.startswith('GCREGS checks=')]
    last = summary[-1] if summary else (lines[-1] if lines else '')
    ok = proc.returncode == 0 and len(summary) == 1 and last.endswith(' failures=0 PASS')
    fails = [ln for ln in lines if ln.startswith('GCREGS FAIL')]
    print(f"{'PASS' if ok else 'FAIL'} {binary.name}: rc={proc.returncode} {last[:80]}"
          f"{' ' + '; '.join(fails)[:240] if fails else ''}")
    return ok


def main(argv):
    ap = argparse.ArgumentParser()
    ap.add_argument('command', choices=['check'])
    ap.add_argument('--dyn', required=True, type=Path)
    ap.add_argument('--static', required=True, type=Path)
    a = ap.parse_args(argv)
    ok = True
    for binary in (a.dyn.resolve(), a.static.resolve()):
        ok &= run(binary)
    print('GC-PORT-REGS ' + ('PASS' if ok else 'FAIL'))
    return 0 if ok else 1


if __name__ == '__main__':
    sys.exit(main(sys.argv[1:]))
