#!/usr/bin/env python3
"""garbage-collect's counters equal the full census (gc-port lane).

  gc-port-gc-stats.py check --dyn BIN --static BIN [--work DIR]

For each binary: tools/ai/gc-port-gc-stats.el with GCS_MODE=run (scenarios: strings, vectors, symbols, conses,
garbage, a buffer, exact deltas; after each the debug switch 53 collects with the census and with the counters and
the rows must agree), dump (a population built, its rows printed, the heap dumped to WORK/NAME.img) and restore
(under --cold-load-from that image: the image population is not counted, heap objects stored into it are, and the
rows agree).  Every run is capped at ulimit -v 8 GiB and 300 s and must print exactly one "GCS MODE checks=N
failures=0 PASS".  Prints one line per run and "GC-PORT-GC-STATS PASS" or "... FAIL"; exit 0 on pass."""
import argparse
import os
import subprocess
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
SOURCE = ROOT / 'tools/ai/gc-port-gc-stats.el'


def run(binary, mode, extra_env, args, log):
    env = dict(os.environ, GCS_MODE=mode, **extra_env)
    cmd = ['bash', '-c', 'ulimit -v 8388608; exec timeout 300 "$@"', 'gcs', str(binary), *args, '--load', str(SOURCE)]
    proc = subprocess.run(cmd, cwd=ROOT, env=env, capture_output=True)
    log.write_bytes(proc.stdout + b'\n--- stderr ---\n' + proc.stderr)
    lines = proc.stdout.decode(errors='replace').splitlines()
    summary = [ln for ln in lines if ln.startswith(f'GCS {mode} ')]
    last = summary[-1] if summary else (lines[-1] if lines else '')
    ok = proc.returncode == 0 and len(summary) == 1 and last.endswith(' failures=0 PASS')
    fails = [ln for ln in lines if ln.startswith('GCS FAIL')]
    print(f"{'PASS' if ok else 'FAIL'} {binary.name} {mode}: rc={proc.returncode} "
          f"{last[:80]}{' ' + '; '.join(fails)[:600] if fails else ''}")
    stat = [ln for ln in lines if ln.startswith('GCS STAT ')]
    return ok, (stat[-1][len('GCS STAT '):] if stat else None)


def main(argv):
    ap = argparse.ArgumentParser()
    ap.add_argument('command', choices=['check'])
    ap.add_argument('--dyn', required=True, type=Path)
    ap.add_argument('--static', required=True, type=Path)
    ap.add_argument('--work', type=Path, default=ROOT / 'target/gc-port/gc-stats')
    a = ap.parse_args(argv)
    a.work.mkdir(parents=True, exist_ok=True)
    ok = True
    for name, binary in (('dyn', a.dyn.resolve()), ('static', a.static.resolve())):
        img = a.work / f'{name}.img'
        if img.exists():
            img.unlink()
        r, _ = run(binary, 'run', {}, [], a.work / f'{name}-run.log')
        ok &= r
        r, stat = run(binary, 'dump', {'GCS_IMAGE': str(img)}, [], a.work / f'{name}-dump.log')
        ok &= r
        if not img.exists() or not stat:
            print(f'FAIL {name} restore: no image or no rows')
            ok = False
            continue
        r, _ = run(binary, 'restore', {'GCS_PRE': stat}, ['--cold-load-from', str(img)], a.work / f'{name}-restore.log')
        ok &= r
    print('GC-PORT-GC-STATS ' + ('PASS' if ok else 'FAIL'))
    return 0 if ok else 1


if __name__ == '__main__':
    sys.exit(main(sys.argv[1:]))
