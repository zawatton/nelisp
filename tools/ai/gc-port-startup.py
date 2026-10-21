#!/usr/bin/env python3
"""Startup wall-clock and peak RSS of NeLisp binaries (Doc 223 image work).

  gc-port-startup.py OUT.json NAME=BINARY ... [--cpus N] [--rounds R]

The NAME gnu runs its BINARY as GNU Emacs (-Q --batch); every other NAME is
a NeLisp binary.
Each process loads a one-form file and exits; its cold image is restored on
the way (the probe fails if a NeLisp run boots the prelude instead).  After
one discarded warm-up per binary, R alternating rounds (default 7) run every
binary once, pinned with taskset to CPU N (default 8, the lane's CPU).
Records wall seconds, peak RSS (ru_maxrss from wait4, KiB), scaling_cur_freq
before each run, per-binary medians and, per round, every binary's time and
RSS divided by the first binary's (paired ratios, COMMON "Measurement").
"""
import hashlib
import json
import os
import statistics
import subprocess
import sys
import time
from pathlib import Path


def freq(cpu):
    try:
        return int(Path(f'/sys/devices/system/cpu/cpu{cpu}/cpufreq/scaling_cur_freq').read_text())
    except (OSError, ValueError):
        return None


def main(argv):
    cpu, rounds = '8', 7
    if '--cpus' in argv:
        i = argv.index('--cpus'); cpu = argv[i + 1]; argv = argv[:i] + argv[i + 2:]
    if '--rounds' in argv:
        i = argv.index('--rounds'); rounds = int(argv[i + 1]); argv = argv[:i] + argv[i + 2:]
    out = Path(argv[0])
    engines = dict(a.split('=', 1) for a in argv[1:])
    engines = {k: Path(v).resolve() for k, v in engines.items()}
    work = out.parent / 'startup-probe'
    work.mkdir(parents=True, exist_ok=True)
    probe = work / 'probe.el'
    probe.write_text('(princ (format "STARTUP %S\\n" (if (fboundp (quote nelisp--arena-stats)) (nth 4 (nelisp--arena-stats)) 0)))\n')
    names = list(engines)
    data = {k: {'seconds': [], 'maxrss_kib': [], 'freq_khz': []} for k in names}
    ratios = {k: {'seconds': [], 'maxrss': []} for k in names[1:]}

    def once(name):
        f = freq(cpu)
        cmd = ['bash', '-c', 'ulimit -v 8388608; exec taskset -c "$0" "$@"', cpu, str(engines[name])]
        cmd += (['-Q', '--batch', '--load', str(probe)] if name == 'gnu'
                else ['--load', str(probe)])
        start = time.perf_counter()
        proc = subprocess.Popen(cmd, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        _, status, usage = os.wait4(proc.pid, 0)
        elapsed = time.perf_counter() - start
        text = proc.stdout.read().decode(errors='replace')
        ok = os.waitstatus_to_exitcode(status) == 0 and 'STARTUP' in text
        if name != 'gnu' and 'STARTUP 0' not in text:
            ok = False      # live-after-gc 0: the cold image was restored, no prelude boot
        return elapsed, usage.ru_maxrss, f, ok
    for name in names:
        once(name)
    failures = 0
    for _ in range(rounds):
        row = {}
        for name in names:
            t, rss, f, ok = once(name)
            if not ok:
                failures += 1
                continue
            data[name]['seconds'].append(t)
            data[name]['maxrss_kib'].append(rss)
            data[name]['freq_khz'].append(f)
            row[name] = (t, rss)
        if len(row) == len(names):
            for name in names[1:]:
                ratios[name]['seconds'].append(row[name][0] / row[names[0]][0])
                ratios[name]['maxrss'].append(row[name][1] / row[names[0]][1])
    result = {'cpu': cpu, 'rounds': rounds, 'failures': failures,
              'binaries': {k: str(v) for k, v in engines.items()},
              'sha256': {k: hashlib.sha256(v.read_bytes()).hexdigest() for k, v in engines.items()},
              'runs': data,
              'median': {k: {'seconds': statistics.median(v['seconds']) if v['seconds'] else None,
                             'maxrss_kib': statistics.median(v['maxrss_kib']) if v['maxrss_kib'] else None}
                         for k, v in data.items()},
              'paired_ratio_vs_' + names[0]: {k: {m: statistics.median(v[m]) if v[m] else None
                                                for m in ('seconds', 'maxrss')}
                                            for k, v in ratios.items()}}
    out.write_text(json.dumps(result, indent=1, sort_keys=True) + '\n')
    for k in names:
        m = result['median'][k]
        print(f"{k}: startup {m['seconds']:.4f}s maxrss {m['maxrss_kib'] / 1024:.1f} MiB")
    for k, v in result['paired_ratio_vs_' + names[0]].items():
        print(f"{k}/{names[0]}: time x{v['seconds']:.3f} rss x{v['maxrss']:.3f}")
    print(f'failures={failures}')
    return 0


if __name__ == '__main__':
    sys.exit(main(sys.argv[1:]))
