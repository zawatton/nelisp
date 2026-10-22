#!/usr/bin/env python3
"""Live-only images carry no hole map; images that need one still take that path (gc-port lane).

  gc-port-image-holes.py check --dyn BIN --static BIN [--work DIR]

For each binary (with its companion image BIN.cold, a stream dump, trailer version 2):
  liveonly  the default load: state 6 (boot structures built, image loaded, no hole map), no hole
            granule, the synthetic hole scan right;
  legacy    a copy of the image with its trailer version patched to 1 (what an older writer
            stamped) loaded with --cold-load-from: state 7 (the old path builds the map too), no
            hole granule in this image, the same scan;
  none      a copy with the version patched to 3, which the loader must refuse: state 0 (the
            control: the switches tell the three loads apart);
and the probe line must be equal between liveonly and legacy.  Every run is capped at ulimit -v 8 GiB
and 120 s.  Prints one line per run and "GC-PORT-IMAGE-HOLES PASS" or "... FAIL"; exit 0 on pass."""
import argparse
import os
import shutil
import struct
import subprocess
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
SOURCE = ROOT / 'tools/ai/gc-port-image-holes.el'


def run(binary, expect, extra, log):
    env = dict(os.environ, GCIH_EXPECT=expect)
    cmd = ['bash', '-c', 'ulimit -v 8388608; exec timeout 120 "$@"', 'gcih', str(binary), *extra, '--load', str(SOURCE)]
    proc = subprocess.run(cmd, cwd=ROOT, env=env, capture_output=True)
    log.write_bytes(proc.stdout + b'\n--- stderr ---\n' + proc.stderr)
    lines = proc.stdout.decode(errors='replace').splitlines()
    summary = [ln for ln in lines if ln.startswith(f'GCIH {expect} ')]
    probe = [ln for ln in lines if ln.startswith('GCIH probe ')]
    last = summary[-1] if summary else (lines[-1] if lines else '')
    ok = proc.returncode == 0 and len(summary) == 1 and last.endswith(' failures=0 PASS') and len(probe) == 1
    fails = [ln for ln in lines if ln.startswith('GCIH FAIL')]
    print(f"{'PASS' if ok else 'FAIL'} {binary.name} {expect}: rc={proc.returncode} {last[:70]}"
          f"{' ' + '; '.join(fails)[:200] if fails else ''}")
    return ok, (probe[0] if probe else None)


def patched(image, dest, version):
    shutil.copyfile(image, dest)
    with open(dest, 'r+b') as f:
        f.seek(os.path.getsize(dest) - 48 + 8)
        f.write(struct.pack('<Q', version))


def main(argv):
    ap = argparse.ArgumentParser()
    ap.add_argument('command', choices=['check'])
    ap.add_argument('--dyn', required=True, type=Path)
    ap.add_argument('--static', required=True, type=Path)
    ap.add_argument('--work', type=Path, default=ROOT / 'target/gc-port/image-holes')
    a = ap.parse_args(argv)
    a.work.mkdir(parents=True, exist_ok=True)
    ok = True
    for name, binary in (('dyn', a.dyn.resolve()), ('static', a.static.resolve())):
        image = Path(str(binary) + '.cold')
        if not image.exists():
            print(f'FAIL {name}: no companion image {image}')
            ok = False
            continue
        v1, v3 = a.work / f'{name}-v1.cold', a.work / f'{name}-v3.cold'
        patched(image, v1, 1)
        patched(image, v3, 3)
        r1, p1 = run(binary, 'liveonly', [], a.work / f'{name}-liveonly.log')
        r2, p2 = run(binary, 'legacy', ['--cold-load-from', str(v1)], a.work / f'{name}-legacy.log')
        r3, _ = run(binary, 'none', ['--cold-load-from', str(v3)], a.work / f'{name}-none.log')
        same = p1 is not None and p1 == p2
        if not same:
            print(f'FAIL {name}: the probe differs between the live-only and the version 1 load: {p1} / {p2}')
        ok &= r1 and r2 and r3 and same
    print('GC-PORT-IMAGE-HOLES ' + ('PASS' if ok else 'FAIL'))
    return 0 if ok else 1


if __name__ == '__main__':
    sys.exit(main(sys.argv[1:]))
