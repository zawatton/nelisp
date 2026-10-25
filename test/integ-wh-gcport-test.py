#!/usr/bin/env python3
"""Exercise the real receipt CLI against isolated files and child-driver stubs."""
import argparse
import copy
import hashlib
import json
import os
from pathlib import Path
import runpy
import shutil
import subprocess
import sys
import time

ROOT = Path(__file__).resolve().parents[1]


def main():
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument('--work', required=True, type=Path)
    ap.add_argument('--case', action='append', help='run only named cases plus the positive run prerequisite')
    args = ap.parse_args()
    work = args.work.resolve()
    work.mkdir(parents=True, exist_ok=True)
    repo = work / 'repo'
    original = ROOT / 'tools/ai/integ-wh-gcport.py'
    definitions = runpy.run_path(str(original))
    for relative in [*definitions['SOURCES'], 'tools/ai/integ-wh-check.sh']:
        path = repo / relative
        path.parent.mkdir(parents=True, exist_ok=True)
        shutil.copyfile(ROOT / relative, path)
    dyn, static = work / 'dyn', work / 'static'
    for path, content in [(dyn, b'dynamic executable'), (static, b'static executable')]:
        path.write_bytes(content)
        path.chmod(0o755)
    cold = Path(str(dyn) + '.cold')
    cold.write_bytes(b'cold image')
    stub = '''import argparse, os, pathlib, resource, sys, time
p=argparse.ArgumentParser(); p.add_argument('check', choices=['check'])
p.add_argument('--dyn'); p.add_argument('--static'); p.add_argument('--work'); a=p.parse_args()
assert pathlib.Path(a.dyn).read_bytes()==b'dynamic executable'
assert pathlib.Path(a.static).read_bytes()==b'static executable'
assert resource.getrlimit(resource.RLIMIT_AS)[0] <= 8388608*1024
name=pathlib.Path(__file__).stem.removeprefix('gc-port-')
footer={'gc-stats':'GC-PORT-GC-STATS','budget':'GC-PORT-BUDGET','vm-poll':'GC-PORT-VM-POLL'}[name]
if a.work:
    for binary in ('dyn','static'):
        for mode in (('run','dump','restore') if name=='gc-stats' else ('run','fresh')):
            (pathlib.Path(a.work)/(binary+'-'+mode+'.log')).write_bytes(b'PASS control\\n\\n--- stderr ---\\n')
mode=os.getenv('GCPORT_STUB_MODE') if name=='budget' else ''
if mode=='timeout': time.sleep(2)
if mode=='stderr': print('diagnostic',file=sys.stderr)
if mode=='failure': print('FAIL control')
if mode=='duplicate': print(footer+' PASS')
print(('GC-PORT-WRONG' if mode=='wrong' else footer)+' PASS')
sys.exit(7 if mode=='nonzero' else 0)
'''
    for name, _, _ in definitions['CHECKS']:
        (repo / f'tools/ai/gc-port-{name}.py').write_text(stub)
    runner = repo / 'tools/ai/integ-wh-gcport.py'
    base = [sys.executable, str(runner)]
    binary_args = ['--dyn', str(dyn), '--static', str(static)]
    results = []

    def execute(name, command, success, env=None):
        if args.case and name not in args.case and name != 'run-positive':
            return None
        start = time.monotonic()
        proc = subprocess.run(command, cwd=repo, env=env, capture_output=True, timeout=15)
        seconds = time.monotonic() - start
        (work / f'{name}.stdout').write_bytes(proc.stdout)
        (work / f'{name}.stderr').write_bytes(proc.stderr)
        passed = (proc.returncode == 0) == success
        results.append(dict(case=name, passed=passed, rc=proc.returncode, seconds=seconds))
        print(f'{"PASS" if passed else "FAIL"} {name} rc={proc.returncode} {seconds:.4f}s')
        return proc

    legacy = work / 'legacy.log'
    legacy.write_text('GC-PORT-BUDGET PASS\n' * 3)
    execute('old-duplicate-falsegreen', ['bash', '-c',
            '[ "$(grep -c "^GC-PORT-[A-Z-]* PASS" "$1")" = 3 ]', 'legacy', str(legacy)], True)
    out = work / 'positive'
    execute('run-positive', base + ['run', *binary_args, '--out', str(out)], True)
    receipt = out / 'integ-wh-gcport.json'
    check = base + ['check', *binary_args, '--receipt', str(receipt)]
    execute('check-positive', check, True)
    good = json.loads(receipt.read_text())
    assert len(good['inputs']['sources']) == 13 and len(good['checks']) == 3
    assert sum(len(row['logs']) for row in good['checks']) == 16
    assert good['inputs']['binaries']['dyn']['companions'] == {
        '.cold': hashlib.sha256(b'cold image').hexdigest(), '.native-startup.el': None, '.version.json': None}
    assert good['inputs']['binaries']['static']['companions'] == {
        '.cold': None, '.native-startup.el': None, '.version.json': None}
    clean_env = {key: value for key, value in os.environ.items() if key != 'GCPORT_STUB_MODE'}

    for name, path in [('wrong-dynamic', dyn), ('wrong-static', static), ('changed-companion', cold),
                       ('wrong-source', repo / 'tools/ai/gc-port-budget.el')]:
        saved = path.read_bytes()
        path.write_bytes(saved + b'changed')
        execute(name, check, False)
        path.write_bytes(saved)
    cold.unlink()
    execute('removed-companion', check, False)
    cold.write_bytes(b'cold image')
    for suffix in ('.cold', '.native-startup.el', '.version.json'):
        companion = Path(str(static) + suffix)
        companion.write_bytes(b'introduced boot input')
        execute('introduced' + suffix, check, False)
        companion.unlink()
    for name, mutate in [
        ('duplicate-check', lambda d: d['checks'].append(copy.deepcopy(d['checks'][0]))),
        ('missing-check', lambda d: d['checks'].pop()),
        ('failed-check', lambda d: d['checks'][0].update(rc=1)),
        ('timed-out-check', lambda d: d['checks'][0].update(timed_out=True)),
        ('wrong-check-id', lambda d: d['checks'][0].update(id='budget')),
        ('missing-log-inventory', lambda d: d['checks'][0]['logs'].pop('gc-stats.stdout')),
    ]:
        data = copy.deepcopy(good)
        mutate(data)
        receipt.write_text(json.dumps(data))
        execute(name, check, False)
    receipt.write_text(json.dumps(good))
    for name, relative, content, rehash in [
        ('changed-log', 'gc-stats.stdout', b'changed\n', False),
        ('duplicate-footer', 'gc-stats.stdout', b'GC-PORT-GC-STATS PASS\n' * 3, True),
        ('wrong-footer', 'gc-stats.stdout', b'GC-PORT-BUDGET PASS\n', True),
        ('failure-line', 'gc-stats.stdout', b'FAIL injected\nGC-PORT-GC-STATS PASS\n', True),
        ('stderr', 'gc-stats.stderr', b'diagnostic\n', True),
        ('detailed-stderr', 'gc-stats/dyn-run.log', b'PASS\n\n--- stderr ---\ndiagnostic\n', True),
        ('missing-log', 'gc-stats.stdout', None, False),
    ]:
        path = out / relative
        saved = path.read_bytes()
        path.unlink() if content is None else path.write_bytes(content)
        data = copy.deepcopy(good)
        if rehash:
            data['checks'][0]['logs'][relative] = hashlib.sha256(content).hexdigest()
        receipt.write_text(json.dumps(data))
        execute(name, check, False)
        path.write_bytes(saved)
    receipt.write_text(json.dumps(good))
    for mode in ('nonzero', 'stderr', 'failure', 'duplicate', 'wrong', 'timeout'):
        env = dict(clean_env, GCPORT_STUB_MODE=mode)
        if execute('run-' + mode, base + ['run', *binary_args, '--out', str(work / mode), '--timeout', '.2'], False, env) is None:
            continue
        data = json.loads((work / mode / 'integ-wh-gcport.json').read_text())
        assert len(data['checks']) == 3, 'runner stopped before executing its exact three checks'
    env = dict(clean_env, INTEG_WH_DYN=str(dyn), INTEG_WH_STATIC=str(static),
               INTEG_WH_GCPORT_RECEIPT=str(receipt))
    shell = ['bash', str(repo / 'tools/ai/integ-wh-check.sh'), 'log', 'gcport']
    execute('shell-positive', shell, True, env)
    env['INTEG_WH_GCPORT_RECEIPT'] = str(work / 'absent.json')
    execute('shell-no-legacy-fallback', shell, False, env)
    (work / 'receipt.json').write_text(json.dumps(dict(cases=results), indent=2) + '\n')
    failures = sum(not row['passed'] for row in results)
    print(f'GCPORT-RECEIPT checked={len(results)} failures={failures}')
    return int(bool(failures))


if __name__ == '__main__':
    sys.exit(main())
