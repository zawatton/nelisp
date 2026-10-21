#!/usr/bin/env python3
"""Wall-clock companion to the Doc 220 churn Ir profiles (coordinator rule).

  gc-port-wall.py OUT.json BASE CANDIDATE [GNU]

For each workload (cons, string, vector) run one warm-up of every engine,
then 7 alternating rounds (base, candidate, GNU 31.1 -Q --batch), each pinned
to CPUs 28-31 with taskset, under the 8 GiB cap.  Records each run's
wall-clock seconds (whole invocation, startup and the three collections
included) and the per-engine median.  The workload text is the one
gc-port-prof.py profiles.
"""
import json
import os
import statistics
import subprocess
import sys
import time
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(Path(__file__).resolve().parent))
import importlib.util
_spec = importlib.util.spec_from_file_location('prof', Path(__file__).with_name('gc-port-prof.py'))
prof = importlib.util.module_from_spec(_spec)
_spec.loader.exec_module(prof)

ROUNDS = 7
CPUS = '28-31'


def run_once(argv, expected):
    cmd = ['bash', '-c', 'ulimit -v 8388608; exec taskset -c "$0" "$@"', CPUS] + argv
    start = time.perf_counter()
    proc = subprocess.run(cmd, capture_output=True, text=True,
                          env=dict(os.environ, LC_ALL='C'))
    elapsed = time.perf_counter() - start
    ok = proc.returncode == 0 and proc.stdout.startswith(expected)
    return elapsed, ok


def main(argv):
    if len(argv) < 3:
        print(__doc__)
        return 2
    out = Path(argv[0])
    engines = {'base': Path(argv[1]).resolve(), 'candidate': Path(argv[2]).resolve(),
               'gnu': Path(argv[3] if len(argv) > 3 else '/usr/local/bin/emacs')}
    work = out.parent / 'wall-workloads'
    work.mkdir(parents=True, exist_ok=True)
    import hashlib
    result = {'cpus': CPUS, 'rounds': ROUNDS, 'engines': {k: str(v) for k, v in engines.items()},
              'sha256': {k: hashlib.sha256(v.read_bytes()).hexdigest() for k, v in engines.items()},
              'workloads': {}}
    for kind in ('cons', 'string', 'vector'):
        src = work / f'{kind}.el'
        src.write_text(prof.workload(kind))
        expected = f'STUDY {kind} 600 3 {3 * 600 * 599 // 2}\n'

        def argv_for(engine):
            path = engines[engine]
            return ([str(path), '-Q', '--batch', '--load', str(src)] if engine == 'gnu'
                    else [str(path), '--load', str(src)])
        times = {e: [] for e in engines}
        for engine in engines:                     # warm-up, discarded
            run_once(argv_for(engine), expected)
        failures = 0
        for _ in range(ROUNDS):
            for engine in engines:
                t, ok = run_once(argv_for(engine), expected)
                times[engine].append(t)
                failures += 0 if ok else 1
        result['workloads'][kind] = {
            'seconds': times, 'failures': failures,
            'median': {e: statistics.median(v) for e, v in times.items()}}
        med = result['workloads'][kind]['median']
        print(f"{kind}: base {med['base']:.3f}s candidate {med['candidate']:.3f}s "
              f"gnu {med['gnu']:.3f}s failures={failures}", flush=True)
    out.write_text(json.dumps(result, indent=1, sort_keys=True) + '\n')
    return 0


if __name__ == '__main__':
    sys.exit(main(sys.argv[1:]))
