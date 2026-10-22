#!/usr/bin/env python3
"""Doc 223 typed float block self-test on NeLisp binaries (gc-port lane).

  gc-port-float-blocks.py check --dyn BIN --static BIN [--work DIR]

For each binary: tools/ai/gc-port-float-blocks.el with GCFB_MODE=run, dump (the heap to
WORK/NAME.img), restore under --cold-load-from that image, redump (a second-generation
dump to WORK/NAME2.img) and restore2.  Every run is capped at ulimit -v 8 GiB and 180 s and
must print exactly one "GCFB MODE checks=N failures=0 PASS".  Prints one line per run and
"GC-PORT-FLOAT-BLOCKS PASS" or "... FAIL"; exit 0 on pass."""
import argparse
import os
import subprocess
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
SOURCE = ROOT / 'tools/ai/gc-port-float-blocks.el'


def run(binary, mode, extra_env, args, log):
    env = dict(os.environ, GCFB_MODE=mode, **extra_env)
    cmd = ['bash', '-c', 'ulimit -v 8388608; exec timeout 180 "$@"', 'gcfb',
           str(binary), *args, '--load', str(SOURCE)]
    proc = subprocess.run(cmd, cwd=ROOT, env=env, capture_output=True)
    log.write_bytes(proc.stdout + b'\n--- stderr ---\n' + proc.stderr)
    lines = proc.stdout.decode(errors='replace').splitlines()
    summary = [ln for ln in lines if ln.startswith(f'GCFB {mode} ')]
    last = summary[-1] if summary else (lines[-1] if lines else '')
    ok = (proc.returncode == 0 and len(summary) == 1 and last.endswith(' failures=0 PASS'))
    fails = [ln for ln in lines if ln.startswith('GCFB FAIL')]
    print(f"{'PASS' if ok else 'FAIL'} {binary.name} {mode}: rc={proc.returncode} "
          f"{last[:80]}{' ' + '; '.join(fails)[:300] if fails else ''}")
    return ok


def main(argv):
    ap = argparse.ArgumentParser()
    ap.add_argument('command', choices=['check'])
    ap.add_argument('--dyn', required=True, type=Path)
    ap.add_argument('--static', required=True, type=Path)
    ap.add_argument('--work', type=Path, default=ROOT / 'target/gc-port/float-blocks')
    a = ap.parse_args(argv)
    a.work.mkdir(parents=True, exist_ok=True)
    ok = True
    for name, binary in (('dyn', a.dyn.resolve()), ('static', a.static.resolve())):
        img1, img2 = a.work / f'{name}.img', a.work / f'{name}2.img'
        for p in (img1, img2):
            if p.exists():
                p.unlink()
        ok &= run(binary, 'run', {}, [], a.work / f'{name}-run.log')
        ok &= run(binary, 'dump', {'GCFB_IMAGE': str(img1)}, [], a.work / f'{name}-dump.log')
        if img1.exists():
            ok &= run(binary, 'restore', {}, ['--cold-load-from', str(img1)], a.work / f'{name}-restore.log')
            ok &= run(binary, 'redump', {'GCFB_IMAGE2': str(img2)}, ['--cold-load-from', str(img1)],
                      a.work / f'{name}-redump.log')
            if img2.exists():
                ok &= run(binary, 'restore2', {}, ['--cold-load-from', str(img2)], a.work / f'{name}-restore2.log')
            else:
                print(f'FAIL {name} restore2: no second image')
                ok = False
        else:
            print(f'FAIL {name} restore: no image')
            ok = False
    print('GC-PORT-FLOAT-BLOCKS ' + ('PASS' if ok else 'FAIL'))
    return 0 if ok else 1


if __name__ == '__main__':
    sys.exit(main(sys.argv[1:]))
