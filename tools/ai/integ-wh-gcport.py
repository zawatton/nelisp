#!/usr/bin/env python3
"""Run the GC-port light set under heavy-run, then check its hash-bound receipt.

run --dyn BIN --static BIN --out DIR writes DIR/integ-wh-gcport.json.
check --dyn BIN --static BIN [--receipt FILE] never reruns the native tests.
"""
import argparse
import hashlib
import json
import os
from pathlib import Path
import re
import resource
import signal
import subprocess
import sys
import time

ROOT = Path(__file__).resolve().parents[2]
CHECKS = [('gc-stats', 'GC-PORT-GC-STATS', 1900),
          ('budget', 'GC-PORT-BUDGET', 1300), ('vm-poll', 'GC-PORT-VM-POLL', 4000)]
DETAILS = {
    'gc-stats': [f'{binary}-{mode}.log' for binary in ('dyn', 'static') for mode in ('run', 'dump', 'restore')],
    'budget': [f'{binary}-{mode}.log' for binary in ('dyn', 'static') for mode in ('run', 'fresh')],
    'vm-poll': [],
}
SOURCES = ['tools/ai/integ-wh-gcport.py',
           *[f'tools/ai/gc-port-{name}.py' for name, _, _ in CHECKS],
           'tools/ai/gc-port-gc-stats.el', 'tools/ai/gc-port-budget.el',
           *[f'test/gc-port-vm-poll/{name}' for name in (
               'funcall-driver.el', 'funcall-workload.el', 'funcall.eld',
               'loop-workload.el', 'loop.eld', 'force-check.el')],
           'test/bytecode-word-corpus.eld']


def require(ok, message):
    if not ok:
        raise ValueError(message)


def sha(path):
    with Path(path).open('rb') as stream:
        return hashlib.file_digest(stream, 'sha256').hexdigest()


def inputs(dyn, static):
    binaries = {}
    for name, path in [('dyn', dyn), ('static', static)]:
        path = path.resolve(strict=True)
        require(path.is_file() and os.access(path, os.X_OK), f'{name} is not executable: {path}')
        companions = {}
        for suffix in ('.cold', '.native-startup.el', '.version.json'):
            companion = Path(str(path) + suffix)
            if companion.exists() or companion.is_symlink():
                require(companion.is_file(), f'{name}: companion is not a file: {companion}')
                companions[suffix] = sha(companion)
            else:
                companions[suffix] = None
        binaries[name] = dict(path=str(path), sha256=sha(path), companions=companions)
    return dict(binaries=binaries, sources={name: sha(ROOT / name) for name in SOURCES})


def log_names(name):
    return [f'{name}.stdout', f'{name}.stderr', *[f'{name}/{item}' for item in DETAILS[name]]]


def check(receipt, dyn, static):
    data = json.loads(receipt.read_text())
    require(type(data.get('schema')) is int and data['schema'] == 1, 'unsupported receipt schema')
    require(data.get('inputs') == inputs(dyn, static), 'binary or source identity changed')
    require(data.get('inputs_unchanged') is True, 'inputs changed during execution')
    rows = data.get('checks')
    require(isinstance(rows, list) and [row.get('id') for row in rows] == [name for name, _, _ in CHECKS],
            'expected exactly three unique checks in canonical order')
    for row, (name, footer, _) in zip(rows, CHECKS):
        require(type(row.get('rc')) is int and row['rc'] == 0 and row.get('timed_out') is False,
                f'{name}: nonzero exit or timeout')
        require(isinstance(row.get('logs'), dict) and set(row['logs']) == set(log_names(name)),
                f'{name}: wrong log inventory')
        for relative, recorded in row['logs'].items():
            path = receipt.parent / relative
            require(path.is_file() and sha(path) == recorded, f'{name}: missing or changed log {relative}')
            content = path.read_bytes()
            require(not re.search(rb'\b(?:FAIL|FAILED)\b', content), f'{name}: failure in {relative}')
            if relative.endswith('.stderr'):
                require(not content, f'{name}: stderr is not empty')
            elif relative.endswith('.log'):
                marker = b'\n--- stderr ---\n'
                require(marker in content and not content.split(marker, 1)[1],
                        f'{name}: missing or nonempty detailed stderr in {relative}')
        output = (receipt.parent / f'{name}.stdout').read_text(encoding='utf-8')
        footers = [line for line in output.splitlines() if line.startswith('GC-PORT-')]
        require(footers == [footer + ' PASS'], f'{name}: wrong, duplicate or missing footer')
    print(f'INTEG-WH-GCPORT PASS checks=3 receipt={receipt}')


def run(args):
    out = args.out.resolve()
    out.mkdir(parents=True, exist_ok=True)
    receipt = out / 'integ-wh-gcport.json'
    receipt.unlink(missing_ok=True)  # An interrupted new run must not leave an old success receipt.
    before = inputs(args.dyn, args.static)
    rows = []
    for name, footer, seconds in CHECKS:
        work = out / name
        work.mkdir(exist_ok=True)
        for relative in log_names(name):
            (out / relative).unlink(missing_ok=True)
        command = [sys.executable, str(ROOT / f'tools/ai/gc-port-{name}.py'), 'check',
                   '--dyn', before['binaries']['dyn']['path'], '--static', before['binaries']['static']['path']]
        if DETAILS[name]:
            command += ['--work', str(work)]
        started = time.monotonic()
        timed_out = False
        with (out / f'{name}.stdout').open('wb') as stdout, (out / f'{name}.stderr').open('wb') as stderr:
            proc = subprocess.Popen(command, cwd=ROOT, stdout=stdout, stderr=stderr, start_new_session=True)
            try:
                proc.wait(timeout=args.timeout or seconds)
            except subprocess.TimeoutExpired:
                timed_out = True
                os.killpg(proc.pid, signal.SIGTERM)
                try:
                    proc.wait(timeout=5)
                except subprocess.TimeoutExpired:
                    pass
                try:
                    os.killpg(proc.pid, signal.SIGKILL)
                except ProcessLookupError:
                    pass
                proc.wait()
        rows.append(dict(id=name, rc=proc.returncode, timed_out=timed_out,
                         seconds=time.monotonic() - started,
                         logs={relative: sha(out / relative) if (out / relative).is_file() else None
                               for relative in log_names(name)}))
        print(f'INTEG-WH-GCPORT {name} rc={proc.returncode} timeout={timed_out}', flush=True)
    data = dict(schema=1, inputs=before, inputs_unchanged=before == inputs(args.dyn, args.static), checks=rows)
    temporary = receipt.with_suffix('.json.tmp')
    temporary.write_text(json.dumps(data, indent=2) + '\n')
    temporary.replace(receipt)
    check(receipt, args.dyn, args.static)


def main():
    ap = argparse.ArgumentParser(description=__doc__)
    sub = ap.add_subparsers(dest='command', required=True)
    for command in ('run', 'check'):
        parser = sub.add_parser(command)
        parser.add_argument('--dyn', required=True, type=Path)
        parser.add_argument('--static', required=True, type=Path)
        if command == 'run':
            parser.add_argument('--out', required=True, type=Path)
            parser.add_argument('--timeout', type=float, help='positive per-check timeout override in seconds')
        else:
            parser.add_argument('--receipt', type=Path, default=ROOT / 'target/final-verify/integ-wh-gcport.json')
    args = ap.parse_args()
    cap = 8388608 * 1024
    soft, hard = resource.getrlimit(resource.RLIMIT_AS)
    cap = min(cap, hard) if hard != resource.RLIM_INFINITY else cap
    resource.setrlimit(resource.RLIMIT_AS, (min(soft, cap) if soft != resource.RLIM_INFINITY else cap, cap))
    try:
        if args.command == 'run':
            require(args.timeout is None or 0 < args.timeout < float('inf'), 'timeout must be finite and positive')
            run(args)
        else:
            check(args.receipt.resolve(), args.dyn, args.static)
    except (OSError, ValueError, TypeError, KeyError, AttributeError) as error:
        print(f'INTEG-WH-GCPORT FAIL: {error}', file=sys.stderr)
        return 1
    return 0


if __name__ == '__main__':
    sys.exit(main())
