#!/usr/bin/env python3
"""Wall-clock gate for the Doc 220 churn workloads (COMMON "Measurement").

  gc-port-wall.py OUT.json BASE CANDIDATE [GNU] [--cpus LIST]

Each process loads one workload file that runs the churn (600 iterations x
3 rounds, an explicit garbage-collect after each round) once as an in-process
warm-up, then runs it again between two `float-time' readings and prints
"STUDY-TIME SECONDS CHECKSUM".  Process startup is outside the measurement;
the three collections are inside it.

Per workload (cons, string, vector) every engine first runs one discarded
warm-up process, then 7 alternating rounds (base, candidate, GNU 31.1 -Q
--batch) are pinned with taskset to CPUS (default 8-11; pick an idle set of
four with `ps -eo psr,pcpu`).  The JSON records every in-process time, the
per-engine medians and the binaries' sha256.
"""
import hashlib
import json
import os
import re
import statistics
import subprocess
import sys
from pathlib import Path

ROUNDS = 7
CONSTRUCTORS = {'cons': '(cons i i)', 'string': '(make-string 128 120)',
                'vector': '(make-vector 16 i)'}


def workload(kind):
    body = f'''(dotimes (r 3)
  (dotimes (i 600)
    (let ((x {CONSTRUCTORS[kind]}))
      (aset study-roots (% i 64) x)
      (setq study-checksum (+ study-checksum i))))
  (garbage-collect))'''
    return f'''(setq gc-cons-threshold 1073741824 gc-cons-percentage 1.0)
(defvar study-roots (make-vector 64 nil))
(defvar study-checksum 0)
(defvar study-time 0.0)
{body}
(setq study-checksum 0)
(setq study-time (float-time))
{body}
(setq study-time (- (float-time) study-time))
(princ (format "STUDY-TIME %.6f %d\\n" study-time study-checksum))
'''


def run_once(argv, cpus):
    cmd = ['bash', '-c', 'ulimit -v 8388608; exec taskset -c "$0" "$@"', cpus] + argv
    proc = subprocess.run(cmd, capture_output=True, text=True, env=dict(os.environ, LC_ALL='C'))
    m = re.search(r'^STUDY-TIME ([0-9.]+) (\d+)$', proc.stdout, re.M)
    ok = proc.returncode == 0 and m and int(m.group(2)) == 3 * 600 * 599 // 2
    return (float(m.group(1)) if m else None), bool(ok)


def main(argv):
    cpus = '8-11'
    if '--cpus' in argv:
        i = argv.index('--cpus')
        cpus = argv[i + 1]
        argv = argv[:i] + argv[i + 2:]
    if len(argv) < 3:
        print(__doc__)
        return 2
    out = Path(argv[0])
    engines = {'base': Path(argv[1]).resolve(), 'candidate': Path(argv[2]).resolve(),
               'gnu': Path(argv[3] if len(argv) > 3 else '/usr/local/bin/emacs')}
    work = out.parent / 'wall-workloads'
    work.mkdir(parents=True, exist_ok=True)
    result = {'cpus': cpus, 'rounds': ROUNDS, 'timing': 'in-process float-time, after an in-process warm-up run',
              'engines': {k: str(v) for k, v in engines.items()},
              'sha256': {k: hashlib.sha256(v.read_bytes()).hexdigest() for k, v in engines.items()},
              'workloads': {}}
    for kind in ('cons', 'string', 'vector'):
        src = work / f'{kind}.el'
        src.write_text(workload(kind))

        def argv_for(engine):
            path = engines[engine]
            return ([str(path), '-Q', '--batch', '--load', str(src)] if engine == 'gnu'
                    else [str(path), '--load', str(src)])
        for engine in engines:                     # warm-up process, discarded
            run_once(argv_for(engine), cpus)
        times = {e: [] for e in engines}
        failures = 0
        for _ in range(ROUNDS):
            for engine in engines:
                t, ok = run_once(argv_for(engine), cpus)
                if ok:
                    times[engine].append(t)
                else:
                    failures += 1
        med = {e: (statistics.median(v) if v else None) for e, v in times.items()}
        result['workloads'][kind] = {'seconds': times, 'failures': failures, 'median': med}
        print(f"{kind}: base {med['base']:.4f}s candidate {med['candidate']:.4f}s "
              f"gnu {med['gnu']:.4f}s failures={failures}", flush=True)
    out.write_text(json.dumps(result, indent=1, sort_keys=True) + '\n')
    return 0


if __name__ == '__main__':
    sys.exit(main(sys.argv[1:]))
