#!/usr/bin/env python3
"""Doc 226 D9 stage 3 symbol cells root self-test on NeLisp binaries (gc-port lane).

  gc-port-symcells.py check --dyn BIN --static BIN [--work DIR]

For each binary: tools/ai/gc-port-symcells.el with GCSC_MODE=run, then
GCSC_MODE=dump (the heap to WORK/NAME.img) and GCSC_MODE=restore under
--cold-load-from that image.  Every run is capped at ulimit -v 8 GiB and 180 s
and must print exactly one "GCSC MODE checks=N failures=0 PASS".  Prints one
line per run and "GC-PORT-SYMCELLS PASS" or "... FAIL"; exit 0 on pass.
The script carries its own negative controls: a vector hidden from the
collector must not survive, and a cleared root must be reported.
"""
import argparse
import os
import subprocess
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
SOURCE = ROOT / 'tools/ai/gc-port-symcells.el'


def run(binary, mode, extra_env, args, log):
    env = dict(os.environ, GCSC_MODE=mode, **extra_env)
    cmd = ['bash', '-c', 'ulimit -v 8388608; exec timeout 180 "$@"', 'gcsc',
           str(binary), *args, '--load', str(SOURCE)]
    proc = subprocess.run(cmd, cwd=ROOT, env=env, capture_output=True)
    log.write_bytes(proc.stdout + b'\n--- stderr ---\n' + proc.stderr)
    lines = proc.stdout.decode(errors='replace').splitlines()
    # The summary line is the script's last output (the runtime may print
    # the load's final value after it).
    summary = [ln for ln in lines if ln.startswith(f'GCSC {mode} ')]
    last = summary[-1] if summary else (lines[-1] if lines else '')
    ok = (proc.returncode == 0 and len(summary) == 1
          and last.endswith(' failures=0 PASS'))
    fails = [ln for ln in lines if ln.startswith('GCSC FAIL')]
    print(f"{'PASS' if ok else 'FAIL'} {binary.name} {mode}: rc={proc.returncode} "
          f"{last[:80]}{' ' + '; '.join(fails)[:300] if fails else ''}")
    return ok


def main(argv):
    ap = argparse.ArgumentParser()
    ap.add_argument('command', choices=['check'])
    ap.add_argument('--dyn', required=True, type=Path)
    ap.add_argument('--static', required=True, type=Path)
    ap.add_argument('--work', type=Path, default=ROOT / 'target/gc-port/symcells')
    a = ap.parse_args(argv)
    a.work.mkdir(parents=True, exist_ok=True)
    ok = True
    for name, binary in (('dyn', a.dyn.resolve()), ('static', a.static.resolve())):
        image = a.work / f'{name}.img'
        if image.exists():
            image.unlink()
        ok &= run(binary, 'run', {}, [], a.work / f'{name}-run.log')
        ok &= run(binary, 'dump', {'GCSC_IMAGE': str(image)}, [], a.work / f'{name}-dump.log')
        if image.exists():
            ok &= run(binary, 'restore', {}, ['--cold-load-from', str(image)],
                      a.work / f'{name}-restore.log')
        else:
            print(f'FAIL {name} restore: no image')
            ok = False
    print('GC-PORT-SYMCELLS ' + ('PASS' if ok else 'FAIL'))
    return 0 if ok else 1


if __name__ == '__main__':
    sys.exit(main(sys.argv[1:]))
