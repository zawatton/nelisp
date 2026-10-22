#!/usr/bin/env python3
"""The live-only image dump's address-space budget (gc-port lane).

  gc-port-dump-budget.py check --dyn BIN --static BIN [--work DIR] [--slack-mib N]
  gc-port-dump-budget.py control --binary BIN        (expects the check to FAIL on a build that
                                                      sizes the table by the heap)

Each binary is run twice: a first-generation dump of the heap below, then a second-generation
dump under --cold-load-from that image (the restored image plus a little new data), which is
the case whose table was sized by the whole region: 2.99 GB on a library image.

For each binary the harness runs tools/ai/gc-port-dump-budget.el (a 300 MB pointer-free string
and 20,000 small lists), waits for the marker file, samples VmSize every 2 ms through the dump,
and requires  peak VmSize - VmSize at the marker  <=  8 * tlen + slack  (tlen = the relocation
entries in the image header, slack 96 MiB by default for the write buffer, the rank tables and
the collector's index).  A table sized by the heap would add the 300 MB string's size again."""
import argparse
import os
import struct
import subprocess
import sys
import time
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
SOURCE = ROOT / 'tools/ai/gc-port-dump-budget.el'


def vmsize_kib(pid):
    try:
        for line in Path(f'/proc/{pid}/status').read_text().splitlines():
            if line.startswith('VmSize:'):
                return int(line.split()[1])
    except OSError:
        pass
    return None


def run(binary, work, name, slack_mib, gen=1, restore=None):
    image = work / f'{name}.img'
    mark = work / f'{name}.mark'
    for p in (image, mark):
        if p.exists():
            p.unlink()
    env = dict(os.environ, GCDB_IMAGE=str(image), GCDB_MARK=str(mark), GCDB_GEN=str(gen))
    extra = ['--cold-load-from', str(restore)] if restore else []
    # exec, so the sampled pid is the binary's own (not a timeout wrapper's)
    proc = subprocess.Popen(['bash', '-c', 'ulimit -v 8388608; exec "$@"', 'gcdb',
                             str(binary), *extra, '--load', str(SOURCE)], cwd=ROOT, env=env,
                            stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    base = None          # VmSize at the last sample taken before the marker file existed
    last = None
    seen = False
    peak = 0
    t0 = time.monotonic()
    while proc.poll() is None and time.monotonic() - t0 < 300:
        marked = mark.exists()
        v = vmsize_kib(proc.pid)
        if v is not None:
            if not marked:
                last = v
            elif not seen:
                seen = True
                base = last
            if seen:
                peak = max(peak, v)
        time.sleep(0.001)
    if proc.poll() is None:
        proc.kill()
    out, err = proc.communicate()
    text = out.decode(errors='replace')
    ok = proc.returncode == 0 and 'GCDB PASS' in text and image.exists() and base is not None
    tlen = 0
    if ok:
        tlen = struct.unpack_from('<Q', image.read_bytes()[:64], 24)[0]
        delta_mib = (peak - base) / 1024.0
        limit_mib = (8 * tlen) / 1048576.0 + slack_mib
        ok = delta_mib <= limit_mib
        print(f"{'PASS' if ok else 'FAIL'} {binary.name} generation {gen}: dump table budget: VmSize grew "
              f"{delta_mib:.0f} MiB through the dump, limit {limit_mib:.0f} MiB "
              f"(8 x {tlen} relocation entries + {slack_mib} MiB)")
    else:
        print(f'FAIL {binary.name} generation {gen}: rc={proc.returncode} {text.strip()[-120:]!r} '
              f'{err.decode(errors="replace").strip()[-120:]!r}')
    return ok


def main(argv):
    ap = argparse.ArgumentParser()
    sub = ap.add_subparsers(dest='cmd', required=True)
    c = sub.add_parser('check')
    c.add_argument('--dyn', required=True, type=Path)
    c.add_argument('--static', required=True, type=Path)
    c.add_argument('--work', type=Path, default=ROOT / 'target/gc-port/dump-budget')
    c.add_argument('--slack-mib', type=int, default=96)
    k = sub.add_parser('control')
    k.add_argument('--binary', required=True, type=Path)
    k.add_argument('--work', type=Path, default=ROOT / 'target/gc-port/dump-budget')
    k.add_argument('--slack-mib', type=int, default=96)
    a = ap.parse_args(argv)
    a.work.mkdir(parents=True, exist_ok=True)
    def generations(binary, name):
        """The first-generation dump, then a second-generation dump under --cold-load-from it."""
        ok1 = run(binary, a.work, f'{name}-gen1', a.slack_mib, gen=1)
        first = a.work / f'{name}-gen1.img'
        if not first.exists():
            return False
        ok2 = run(binary, a.work, f'{name}-gen2', a.slack_mib, gen=2, restore=first)
        return ok1 and ok2

    if a.cmd == 'control':
        ok = generations(a.binary.resolve(), 'control')
        print('GC-PORT-DUMP-BUDGET control ' + ('PASSED (no control)' if ok else 'failed as expected'))
        return 0 if not ok else 1
    ok = True
    for name, binary in (('dyn', a.dyn.resolve()), ('static', a.static.resolve())):
        ok &= generations(binary, name)
    print('GC-PORT-DUMP-BUDGET ' + ('PASS' if ok else 'FAIL'))
    return 0 if ok else 1


if __name__ == '__main__':
    sys.exit(main(sys.argv[1:]))
