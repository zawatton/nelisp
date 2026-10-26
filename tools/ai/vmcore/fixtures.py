"""Run the exact A-lane regression inputs without writing to their trees."""
import hashlib
import importlib.util
import json
import os
from pathlib import Path
import re
import sys

sys.dont_write_bytecode = True
ROOT = Path(os.environ.get('FIXROOT', '/home/madblack-21/Cowork/Notes/dev/nelisp/target/agent-worktrees/resume-4943-20261024'))
REF = ROOT
A = ROOT.parent / 'builtin-leaf-20261024'
OUT = Path(os.environ['FIXOUT'])
BIN = Path(sys.argv[1]).resolve()


def sha(path):
    return hashlib.sha256(Path(path).read_bytes()).hexdigest()


def load(name, path):
    spec = importlib.util.spec_from_file_location(name, path)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


OUT.mkdir(exist_ok=False)
runner = load('a_runner', A / 'target/builtin-leaf/qualify-runtime-init.py')
afrt = load('original_afrt', REF / 'target/argframe-produced-acceptance/test/argframe-runtime-acceptance.py')
weak = REF / 'target/resume-acceptance/d7-critical-weak24'
buffer = REF / 'test/nelisp-buffer-unification-standalone-smoke.el'
old_afrt = A / 'target/builtin-leaf/qualification/attempt-init-20261024-01/argframe25/A-default.stdout'
assert old_afrt.is_file()
inputs = [BIN, Path(str(BIN) + '.cold'), weak / 'probe.el', weak / 'expected', buffer,
          Path(__file__), A / 'target/builtin-leaf/qualify-runtime-init.py', afrt.FIXTURE,
          REF / 'tools/ai/integ-wh-eqhash-fixture.sh', REF / 'tools/ai/eqhash-semantics.el',
          REF / 'tools/ai/eqhash-semantics.expected', old_afrt]
pins = {str(p): sha(p) for p in inputs}
rows = {}


def save():
    (OUT / 'report.json').write_text(json.dumps(
        dict(status='IN_PROGRESS', checks=rows, pins=pins), indent=2) + '\n')


row, stdout, stderr = runner.run([BIN, '--load', weak / 'probe.el'], REF, OUT, 'weak24', 45)
row.update(checks=24, pass_count=stdout.count(b'PASS'),
           passed=row['rc'] == 0 and not stderr and stdout == (weak / 'expected').read_bytes())
rows['weak24'] = row
save()
assert row['passed'], row
print('weak 24/24 PASS', flush=True)

row, stdout, stderr = runner.run(['bash', REF / 'tools/ai/integ-wh-eqhash-fixture.sh', BIN], REF, OUT,
                                'gnu56', 130, dict(EQHASH_FIXTURE_OUT=str(OUT / 'gnu56'),
                                                  EQHASH_FIXTURE_TIMEOUT='120', EQHASH_FIXTURE_KILL_AFTER='5'))
row.update(checks=56, passed=row['rc'] == 0 and not stderr and stdout == b'EQHASH-FIXTURE 56/56\n')
rows['gnu56'] = row
save()
assert row['passed'], row
print('GNU 56/56 PASS', flush=True)

afout = OUT / 'argframe25'
afout.mkdir()
row = afrt.execute('stackset', BIN, 'default', afout, 45)
expected = {k: f'AFRT CASE {k} PASS {v}' for k, v in afrt.EXPECTED}
failures = [k for k, v in expected.items() if row['cases'].get(k) != v]
before = afrt.parse(old_afrt.read_text(), '', 1, 'default', True)
old_failures = [k for k, v in expected.items() if before['cases'].get(k) != v]
row.update(checks=25, pass_count=25 - len(failures), failures=failures,
           expected_baseline_red=(failures == old_failures == ['exit-hook-result-gc']
                                  and row['cases'] == before['cases']
                                  and row['summary'] == before['summary'] and row['rc'] == 1
                                  and not (afout / 'stackset-default.stderr').read_bytes()))
rows['afrt25'] = row
save()
assert row['expected_baseline_red'], row
print('AFRT 24/25; unchanged exit-hook-result-gc RED (exit 1)', flush=True)

row, stdout, stderr = runner.run([BIN, '--load', buffer], REF, OUT, 'buffer21', 60)
match = re.search(rb'GATE-COUNT checked=(\d+) findings=(\d+)', stdout)
row.update(checks=int(match[1]) if match else 0,
           passed=row['rc'] == 0 and not stderr and match is not None
           and tuple(map(int, match.groups())) == (21, 0)
           and b'nelisp-buffer-unification-standalone-smoke: PASS' in stdout)
rows['buffer21'] = row
save()
assert row['passed'], row
assert pins == {str(p): sha(p) for p in inputs}, 'Pinned input changed'
(OUT / 'report.json').write_text(json.dumps(dict(
    status='EXPECTED_BASELINE_RED_REMAINS', regression=False, checks=rows, pins=pins,
    full_default_gc_qualification=False), indent=2) + '\n')
print('buffer 21/21 PASS; no new fixture failures', flush=True)
