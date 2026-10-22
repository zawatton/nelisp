#!/usr/bin/env python3
"""Image objects reached only through precise edges survive collections (gc-port lane).

  gc-port-image-edges.py check --dyn BIN --static BIN [--work DIR]

For each binary: tools/ai/gc-port-image-edges.el with GCIE_MODE=run (heap objects), dump (the heap to
WORK/NAME.img), restore under --cold-load-from that image (the items are image objects: collections with
the conservative scan off and on, heap objects stored into image conses and held only by them),
redump (restore, fill the slots, dump a second-generation image to WORK/NAME2.img) and restore2 (under the
second image).  Every run is capped at ulimit -v 8 GiB and 180 s and must print exactly one
"GCIE MODE checks=N failures=0 PASS".  Prints one line per run and "GC-PORT-IMAGE-EDGES PASS" or
"... FAIL"; exit 0 on pass."""
import argparse
import os
import subprocess
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
SOURCE = ROOT / 'tools/ai/gc-port-image-edges.el'


def run(binary, mode, extra_env, args, log):
    env = dict(os.environ, GCIE_MODE=mode, **extra_env)
    cmd = ['bash', '-c', 'ulimit -v 8388608; exec timeout 180 "$@"', 'gcie', str(binary), *args, '--load', str(SOURCE)]
    proc = subprocess.run(cmd, cwd=ROOT, env=env, capture_output=True)
    log.write_bytes(proc.stdout + b'\n--- stderr ---\n' + proc.stderr)
    lines = proc.stdout.decode(errors='replace').splitlines()
    summary = [ln for ln in lines if ln.startswith(f'GCIE {mode} ')]
    last = summary[-1] if summary else (lines[-1] if lines else '')
    ok = proc.returncode == 0 and len(summary) == 1 and last.endswith(' failures=0 PASS')
    fails = [ln for ln in lines if ln.startswith('GCIE FAIL')]
    print(f"{'PASS' if ok else 'FAIL'} {binary.name} {mode}: rc={proc.returncode} "
          f"{last[:80]}{' ' + '; '.join(fails)[:300] if fails else ''}")
    return ok


def main(argv):
    ap = argparse.ArgumentParser()
    ap.add_argument('command', choices=['check'])
    ap.add_argument('--dyn', required=True, type=Path)
    ap.add_argument('--static', required=True, type=Path)
    ap.add_argument('--work', type=Path, default=ROOT / 'target/gc-port/image-edges')
    a = ap.parse_args(argv)
    a.work.mkdir(parents=True, exist_ok=True)
    ok = True
    for name, binary in (('dyn', a.dyn.resolve()), ('static', a.static.resolve())):
        img1, img2 = a.work / f'{name}.img', a.work / f'{name}2.img'
        for p in (img1, img2):
            if p.exists():
                p.unlink()
        ok &= run(binary, 'run', {}, [], a.work / f'{name}-run.log')
        ok &= run(binary, 'dump', {'GCIE_IMAGE': str(img1)}, [], a.work / f'{name}-dump.log')
        if not img1.exists():
            print(f'FAIL {name} restore: no image')
            ok = False
            continue
        ok &= run(binary, 'restore', {}, ['--cold-load-from', str(img1)], a.work / f'{name}-restore.log')
        ok &= run(binary, 'redump', {'GCIE_IMAGE2': str(img2)}, ['--cold-load-from', str(img1)],
                  a.work / f'{name}-redump.log')
        if img2.exists():
            ok &= run(binary, 'restore2', {}, ['--cold-load-from', str(img2)], a.work / f'{name}-restore2.log')
        else:
            print(f'FAIL {name} restore2: no second image')
            ok = False
    print('GC-PORT-IMAGE-EDGES ' + ('PASS' if ok else 'FAIL'))
    return 0 if ok else 1


if __name__ == '__main__':
    sys.exit(main(sys.argv[1:]))
