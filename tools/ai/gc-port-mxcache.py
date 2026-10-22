#!/usr/bin/env python3
"""The macro-expansion cache survives collections (gc-port lane, ledger S1.20).

  gc-port-mxcache.py check --dyn BIN --static BIN

The interpreter caches a macro call site's expansion keyed by the call form's address (nl_mxcache_*); the cache is the only
reference to the expansion, so a collection must visit every used row.  test/gc-port-mxcache-check.el defines 400 functions
with one macro call site each, expands every site, calls them again (hits), runs four collections with allocation churn
between them that reuses freed memory, and calls every function once more.  PASS needs no wrong value, no new expansion after
the first call of each site, and exactly one summary line.  A build whose nl_mxcache_mark_all marks nothing crashes this test
(checked once on the step 3c candidate).  Every run is capped at ulimit -v 8 GiB and 300 s.  Prints one line per run and
"GC-PORT-MXCACHE PASS" or "... FAIL"; exit 0 on pass."""
import argparse
import subprocess
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
SOURCE = ROOT / 'test/gc-port-mxcache-check.el'


def run(binary):
    cmd = ['bash', '-c', 'ulimit -v 8388608; exec timeout 300 "$@"', 'gcmx', str(binary), '--load', str(SOURCE)]
    proc = subprocess.run(cmd, cwd=ROOT, capture_output=True)
    lines = [ln for ln in proc.stdout.decode(errors='replace').splitlines() if ln.startswith('MXC ')]
    ok = proc.returncode == 0 and len(lines) == 1 and lines[0].endswith(' PASS')
    print(f"{'PASS' if ok else 'FAIL'} {binary.name}: rc={proc.returncode} {lines[-1][:100] if lines else '(no summary line)'}")
    return ok


def main(argv):
    ap = argparse.ArgumentParser()
    ap.add_argument('command', choices=['check'])
    ap.add_argument('--dyn', required=True, type=Path)
    ap.add_argument('--static', required=True, type=Path)
    a = ap.parse_args(argv)
    ok = run(a.dyn.resolve())
    ok &= run(a.static.resolve())
    print('GC-PORT-MXCACHE ' + ('PASS' if ok else 'FAIL'))
    return 0 if ok else 1


if __name__ == '__main__':
    sys.exit(main(sys.argv[1:]))
