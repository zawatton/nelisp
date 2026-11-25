#!/usr/bin/env python3
"""Measure smoke substrate rows as paired, checked N-to-4N slopes."""
import argparse
import csv
import json
import os
from pathlib import Path
import re
import statistics
import subprocess
import sys
import time

ROOT = Path(__file__).resolve().parent
TREE = ROOT.parents[1]
GNU = '/usr/local/bin/emacs-31.1'
DUMP = TREE.parent / 'batch/rt-sfwt.dump'
CPUS = (11, 13)
AFFINITY = ','.join(map(str, CPUS))

def parse_args():
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument('--out', required=True, type=Path)
    ap.add_argument('--rows-file', type=Path, default=ROOT / 'rows.tsv')
    ap.add_argument('--gnu-artifact', type=Path, help='Reuse a native-compiled GNU driver')
    ap.add_argument('--rows', help='Comma-separated subset of smoke benchmark rows')
    ap.add_argument('--engine', default='both', choices=['both', 'gnu', 'nelisp'])
    ap.add_argument('--reps', default=5, type=int)
    ap.add_argument('--divisor', default=1, type=int, help='Reduce counts for diagnostic runs')
    ap.add_argument('--dump', default=DUMP, type=Path)
    ap.add_argument('--binary', default=TREE / 'target/nelisp', type=Path)
    ap.add_argument('--control-binary', type=Path, help='Compare two NeLisp binaries')
    ap.add_argument('--control-dump', type=Path)
    ap.add_argument('--nelisp-artifact', type=Path, help='Time separately compiled NeLisp driver')
    ap.add_argument('--nelisp-source', type=Path, help='Override NeLisp source for a mutation control')
    ap.add_argument('--callgrind', action='store_true', help='Diagnostic instruction counts; do not use reported times')
    ap.add_argument('--idle-floor', default=95.0, type=float)
    ap.add_argument('--cpu', type=int, choices=CPUS,
                    help='Use one of the assigned CPUs for the entire comparison')
    ap.add_argument('--allow-mismatch', action='store_true', help='Report divergent rows without timings')
    args = ap.parse_args()
    if bool(args.control_binary) != bool(args.control_dump):
        ap.error('--control-binary and --control-dump must be used together')
    return args

def cpu_ticks(cpus):
    snapshot = {}
    with open('/proc/stat') as stat:
        for line in stat:
            m = re.match(r'^cpu(\d+) (.*)', line)
            if m and int(m[1]) in cpus:
                ticks = list(map(int, m[2].split()[:8]))
                snapshot[int(m[1])] = ticks[3] + ticks[4], sum(ticks)
    return snapshot

def idle_gate(floor, cpu):
    if floor == 0:
        return None
    selected = (cpu,) if cpu else CPUS
    cpus = set(selected)
    for logical in selected:
        siblings = Path(f'/sys/devices/system/cpu/cpu{logical}/topology/thread_siblings_list')
        if siblings.is_file():
            for part in siblings.read_text().strip().split(','):
                first, _, last = part.partition('-')
                cpus.update(range(int(first), int(last or first) + 1))
    for _ in range(120):
        start = cpu_ticks(cpus)
        time.sleep(1)
        end = cpu_ticks(cpus)
        idle = {cpu: 100 * (end[cpu][0] - start[cpu][0]) /
                max(1, end[cpu][1] - start[cpu][1]) for cpu in cpus}
        if min(idle.values()) >= floor:
            return idle
    raise RuntimeError(f'Failed to find idle CPUs: {idle}')

def lisp_string(path):
    return json.dumps(str(path))


def build_gnu_driver(args, directory):
    source = ROOT / 'smoke-driver.el'
    artifact = directory / 'smoke-driver.eln'
    form = (f"(progn (require 'comp) "
            f"(native-compile {lisp_string(source)} {lisp_string(artifact)}))")
    command = ['bash', '-c', 'ulimit -v 8388608; exec timeout 120 taskset -c "$@"',
               'bench', str(args.cpu) if args.cpu else AFFINITY,
               GNU, '-Q', '--batch', '--eval', form]
    log = directory / 'gnu-compile.log'
    with log.open('w') as output:
        status = subprocess.run(command, cwd=TREE, stdin=subprocess.DEVNULL,
                                stdout=output, stderr=subprocess.STDOUT,
                                env={**os.environ, 'TMPDIR': str(directory)})
    if status.returncode or not artifact.is_file():
        raise RuntimeError(f'GNU driver compilation failed, see {log}')
    return artifact


def run(args, engine, rep, factor, rows, run_dir):
    gnu = engine == 'gnu'
    calc = []
    if gnu:
        calc.append('(setq native-comp-jit-compilation nil)')
        calc.append(f"(load {lisp_string(args.gnu_artifact.resolve())} nil t t)")
    elif args.nelisp_artifact:
        calc.append(f'(nelisp-artifact-load-file {lisp_string(args.nelisp_artifact.resolve())})')
    else:
        path = args.nelisp_source or ROOT / 'smoke-driver.el'
        calc.append(f'(load {lisp_string(path.resolve())} nil t t)')
    calc.extend([
        *[f"(es-run '{name} {n * factor})" for name, n in rows.items()],
        '(princ "ES-DONE\\n")',
        '(kill-emacs 0)',
    ])
    binary = args.control_binary if engine == 'control' else args.binary
    dump = args.control_dump if engine == 'control' else args.dump
    cmd = ([GNU, '-Q', '--batch'] if gnu else
           [str(binary.resolve()), '--cold-load-from', str(dump.resolve()),
            '--eval', '(nemacs-runtime-dump-reinit)'])
    # Separate timed calls from load and from one another so the evaluator's
    # load-time tier selection does not disable promotion during the workload.
    for form in calc:
        cmd.extend(['--eval', form])
    env = {**os.environ, 'NELISP_HOME': str(TREE), 'LC_ALL': 'C.UTF-8',
           'TMPDIR': str(run_dir)}
    log = run_dir / f'{rep}-{engine}-{factor}'
    if args.callgrind:
        cmd = ['valgrind', '--tool=callgrind', f'--callgrind-out-file={log}.callgrind', *cmd]
    wrapped = ['bash', '-c', 'ulimit -v 8388608; exec timeout 900 taskset -c "$@"',
               'bench', str(args.cpu) if args.cpu else AFFINITY, *cmd]
    output = subprocess.run(wrapped, env=env, cwd=TREE, stdin=subprocess.DEVNULL,
                            stdout=subprocess.PIPE, stderr=subprocess.PIPE,
                            text=True, errors='replace', timeout=905)
    log.with_suffix('.out').write_text(output.stdout)
    log.with_suffix('.err').write_text(output.stderr)
    if output.returncode or 'ES-DONE' not in output.stdout.splitlines():
        raise RuntimeError(f'{engine} exited {output.returncode}, see {log}.err')
    matches = re.findall(r'^ES (\S+) (\d+) (\d+\.\d+) (.+)$', output.stdout, re.M)
    parsed = {name: (int(count), float(elapsed), value) for name, count, elapsed, value in matches}
    if (len(matches) != len(rows) or parsed.keys() != rows.keys()
            or any(parsed[name][0] != n * factor for name, n in rows.items())):
        raise RuntimeError(f'{engine} did not emit exactly the requested rows and counts, see {log}.out')
    return parsed

def main():
    args = parse_args()
    rows = {name: max(1, int(n) // args.divisor) for name, n in
            csv.reader(args.rows_file.open(), delimiter='\t')}
    if args.rows:
        rows = {name: rows[name] for name in args.rows.split(',')}
    out = args.out.resolve()
    out.mkdir(parents=True, exist_ok=True)
    print(f'timing affinity {args.cpu or AFFINITY}', flush=True)
    reference = 'control' if args.control_binary else 'gnu'
    engines = (reference, 'nelisp') if args.engine == 'both' else (args.engine,)
    if 'gnu' in engines and args.gnu_artifact is None:
        args.gnu_artifact = build_gnu_driver(args, out)
    observed = {}
    invalid = {}
    raw = []
    for rep in range(args.reps):
        order = engines if rep % 2 == 0 else engines[::-1]
        for engine in order:
            idle = idle_gate(args.idle_floor, args.cpu)
            factors = (1, 4) if rep % 2 == 0 else (4, 1)
            samples = {factor: run(args, engine, rep, factor, rows, out) for factor in factors}
            raw.append(dict(rep=rep, engine=engine, idle=idle, samples=samples))
            for name, n in rows.items():
                low, high = samples[1][name], samples[4][name]
                for count, _, result in (low, high):
                    key = name, count
                    previous = observed.setdefault(key, (engine, result))
                    if result.startswith('ERROR:'):
                        m = f'ROW ERROR {name} N={count}: {engine} {result}'
                    elif result != previous[1]:
                        m = f'RESULT MISMATCH {name} N={count}: {engine} {result} vs {previous}'
                    else:
                        continue
                    invalid[name] = m
                    if not args.allow_mismatch:
                        raise RuntimeError(m)
        print(f'finished pair {rep + 1}/{args.reps}', flush=True)
        (out / 'raw.json').write_text(json.dumps(raw, indent=2) + '\n')
    summary = [f'row\t{reference}_ns\tnelisp_ns\tratio\tstatus']
    for name, n in rows.items():
        slopes = {engine: [(sample['samples'][4][name][1] - sample['samples'][1][name][1])
                           * 1e9 / (3 * n) for sample in raw if sample['engine'] == engine]
                  for engine in engines}
        medians = {engine: statistics.median(values) for engine, values in slopes.items()}
        status = 'ok'
        if name in invalid:
            status = invalid[name]
            medians = {}
        elif any(value <= 0 for value in medians.values()):
            status = 'nonpositive slope'
        g, nl = medians.get(reference), medians.get('nelisp')
        ratio = f'{nl / g:.4f}' if g and nl and min(g, nl) > 0 else ''
        summary.append(f'{name}\t{"" if g is None else f"{g:.3f}"}'
                       f'\t{"" if nl is None else f"{nl:.3f}"}\t{ratio}\t{status}')
    (out / 'table.tsv').write_text('\n'.join(summary) + '\n')

if __name__ == '__main__':
    try:
        main()
    except RuntimeError as e:
        sys.exit(str(e))
