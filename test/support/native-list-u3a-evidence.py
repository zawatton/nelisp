"""Validate bounded U3a and regression evidence without rerunning native builds."""
import copy
import hashlib
import json
import re
import sys
import tempfile
from collections import Counter
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
TARGET = ROOT / 'target'
FIXTURES = [(67, 1), (68, 2), (69, 3), (70, 4), (175, 0), (175, 1), (175, 4), (175, 33), (175, 255)]
SOURCES = {'lisp/nelisp-bytecode-ir.el', 'lisp/nelisp-bytecode-frame-ir.el',
           'lisp/nelisp-native-funcall-v2.el', 'lisp/nelisp-bytecode-native-rooted-cfg-plan.el',
           'lisp/nelisp-bytecode-native-rooted-cfg-emit.el', 'lisp/nelisp-bytecode-native-rooted-cfg-shared-emit.el',
           'test/standalone-native-list-u3a-driver.el', 'test/support/native-list-u3a-fixtures.el',
           'test/standalone-native-list-u3a-smoke.sh'}


def require_records(directory, count):
    assert directory.is_dir(), str(directory)
    paths = list(directory.glob('*.json'))
    assert sum('rc' in json.loads(path.read_text()) for path in paths) == count, str(directory)
    return paths


def exact_markers(output, backend, fixtures):
    actual = Counter(re.findall(r'^U3A-NATIVE-PASS backend=(in-house|gccjit) opcode=(\d+) count=(\d+) native=1 rebound=1 gc=(\d+) allocations=(\d+)$', output, re.M))
    expected = Counter((backend, str(FIXTURES[i][0]), str(FIXTURES[i][1]),
                        str(1 if i in (7, 8) else 0),
                        str(FIXTURES[i][1] if i in (7, 8) else 0)) for i in fixtures)
    assert actual == expected, f'Expected exact opcode/count/GC markers: {expected}, got {actual}'


def valid_run(record):
    return (record['rc'] == 0 and 0 < record['seconds'] < 300
            and re.fullmatch(r'[0-9a-f]{64}', record['binary_sha256']) is not None)


def native():
    expected = {(backend, case) for backend in ('in-house', 'gccjit') for case in range(9)}
    proved = set()
    output = (TARGET / 'u3a-accept-default.log').read_text()
    directories = re.findall(r'^U3A-EVIDENCE=(.+)$', output, re.M)
    assert len(directories) == 2, 'Require one completed final --both invocation'
    identities = {backend: hashlib.sha256((TARGET / binary).read_bytes()).hexdigest()
                  for backend, binary in [('in-house', 'nelisp-static'), ('gccjit', 'nelisp-dyn')]}
    for directory in map(Path, directories):
        for path in directory.glob('*.json'):
            row = json.loads(path.read_text())
            if not row.get('passed') or not valid_run(row):
                raise AssertionError(f'Failed final native case: {path}')
            assert row['binary_sha256'] == identities[row['backend']]
            if row['backend'] == 'in-house':
                assert row['cold_sha256'] == hashlib.sha256((TARGET / 'nelisp-static.cold').read_bytes()).hexdigest()
            assert not path.with_suffix('.err').read_text()
            output = path.with_suffix('.out').read_text()
            fixtures = row.get('fixtures', [row.get('fixture')])
            exact_markers(output, row['backend'], fixtures)
            assert set(row['source_sha256']) == SOURCES
            for relative, digest in row['source_sha256'].items():
                assert hashlib.sha256((ROOT / relative).read_bytes()).hexdigest() == digest, relative
            assert row['compiler_cache_sha256'] == hashlib.sha256((TARGET / 'nelisp-artifact-runtime.el.nelc').read_bytes()).hexdigest()
            if 3 in fixtures:
                assert 'U3A-ERROR-PASS prior-mutation=1 later-effect=0' in output
            if 7 in fixtures:
                assert f"U3A-JOIN-PASS backend={row['backend']} cases=2 native=2" in output
            for case in fixtures:
                if case in (7, 8):
                    count = 33 if case == 7 else 255
                    assert f'count={count} native=1 rebound=1 gc=1 allocations={count}' in output
                proved.add((row['backend'], case))
    assert proved == expected, f'Missing native cases: {sorted(expected - proved)}'
    print('U3A executed pairs=18, opcodes=5 per backend; forced GC between allocations; diamond both arms')


def host():
    before = json.loads((TARGET / 'u3a-before-host.json').read_text())
    after = {x['file']: x for x in json.loads((TARGET / 'u3a-after-host.json').read_text())}
    assert len(before) == 52
    assert len([x for x in before if x['rc']]) == 9
    for row in before:
        current = after[row['file']]
        assert current['rc'] == row['rc'], row['file']
        assert current['counts'][0] == row['counts'][0], row['file']
        assert current['counts'][2] == row['counts'][2], row['file']
    assert after['test/nelisp-native-list-u3a-test.el']['counts'] == ['5', '5', '0']
    print('Host existing=52 files/400 tests; excluded=9 known-red files; no new failures; new=5 tests')


def regressions():
    families = [('u3a-regression-u2c-compact.log', 'U2C-NATIVE-PASS', 48, 12),
                ('u3a-regression-u6-retry.log', 'U6-NATIVE-PASS', 12, 6),
                ('u3a-regression-cycles-compact.log', 'CYCLES-NATIVE-PASS', 14, 7),
                ('u3a-regression-f1-compact.log', 'F1-CACHE-PASS', 2, 2)]
    identities = {hashlib.sha256((TARGET / binary).read_bytes()).hexdigest()
                  for binary in ('nelisp-static', 'nelisp-dyn')}
    for file, marker, count, records in families:
        output = (TARGET / file).read_text()
        # Top-level reader may also print the returned marker string.
        assert sum(line.startswith(marker) for line in output.splitlines()) == count, file
        assert 'uncaught error' not in output and 'F1B failed' not in output, file
        paths = set(re.findall(r'^(?:U2C|U6|CYCLES|F1)-EVIDENCE=(.+)$', output, re.M))
        assert len(paths) == 2, file
        observed = set()
        for directory in paths:
            paths_json = require_records(Path(directory), records)
            for path in paths_json:
                row = json.loads(path.read_text())
                if 'rc' not in row:
                    assert row.get('passed') is True
                    continue
                assert valid_run(row), str(path)
                assert row['binary_sha256'] in identities, str(path)
                observed.add(row['binary_sha256'])
                assert row.get('passed', True), str(path)
                assert not path.with_suffix('.err').read_text(), str(path)
        assert observed == identities, file
    output = (TARGET / 'u3a-regression-f1-compact.log').read_text()
    assert output.count('\nF1-CORPUS-DIGEST=52c26b63098a83d62c044908b231afd5cce3a85da3b559147e6dc4810eeac2ef\n') == 2
    print('All four requested --both regression suites passed; exact F1 digest twice')


def controls():
    sample = dict(rc=0, seconds=1, binary_sha256='a' * 64)
    assert valid_run(sample)
    for field, value in [('rc', 1), ('seconds', 300), ('binary_sha256', 'bad')]:
        mutant = copy.deepcopy(sample); mutant[field] = value
        assert not valid_run(mutant)
    line = 'U3A-NATIVE-PASS backend=in-house opcode=67 count=1 native=1 rebound=1 gc=0 allocations=0\n'
    exact_markers(line, 'in-house', [0])
    try:
        exact_markers(line * 2, 'in-house', [0, 1])
    except AssertionError:
        pass
    else:
        raise AssertionError('Duplicate/wrong opcode markers were accepted')
    with tempfile.TemporaryDirectory() as scratch:
        for directory in (Path(scratch), Path(scratch) / 'absent'):
            try:
                require_records(directory, 2)
            except AssertionError:
                pass
            else:
                raise AssertionError('Missing/empty execution evidence was accepted')
    print('Evidence controls reject return code, deadline, identity, wrong opcodes and missing/empty records')


if __name__ == '__main__':
    {'native': native, 'host': host, 'regressions': regressions, 'controls': controls}[sys.argv[1]]()
