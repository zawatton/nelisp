#!/usr/bin/env python3
"""Verify raw P3.4/P3.5/P3.6 measurements, independently of pass flags."""
import argparse, hashlib, json, re, traceback
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
BACKENDS = ['in-house', 'gccjit', 'template']
LEAVES = ['file-exists-p', 'file-name-directory', 'p34-arith3']
WIDE = ['expand-file-name', 'directory-files', 'locate-file']

def sha(path):
    with Path(path).open('rb') as stream:
        return hashlib.file_digest(stream, 'sha256').hexdigest()

def explain_failure(error):
    """Keep assertion-only failures reviewable in assembled receipts."""
    if str(error):
        return str(error)
    frame = traceback.extract_tb(error.__traceback__)[-1]
    return f'{type(error).__name__}: {Path(frame.filename).name}:{frame.lineno}: {frame.line}'

def raw(row, work):
    label = f"{row['backend']}/{row['name']}/{row['phase']}"
    assert row['rc'] == 0, f"{label}: exit {row['rc']} after {row['seconds']:.3f}s"
    assert row['identity_unchanged'], label + ': inputs changed during execution'
    assert 0 < row['seconds'] < row['timeout'] + 1 and row['timeout'] <= 1800
    assert row['artifacts'], 'missing native artifact pins'
    for path, digest in row['artifacts'].items():
        assert sha(path) == digest, 'native artifact changed: ' + path
    assert row['executable_command']==['timeout','-k','5',str(row['timeout']),*row['command']]
    binary = Path(row['command'][0])
    assert binary in [ROOT/'target/nelisp-static', ROOT/'target/nelisp-dyn']
    assert row['pins'][str(binary)] == sha(binary)
    assert row['pins'][str(binary)+'.cold'] == sha(str(binary)+'.cold')
    assert row['pins'][str(binary)+'.native-startup.el'] == sha(str(binary)+'.native-startup.el')
    assert row['pins'][str(ROOT/'test/nelisp-native-boundary-driver.el')] == sha(ROOT/'test/nelisp-native-boundary-driver.el')
    assert row['command'][1:3] == ['--cold-load-from', str(binary)+'.cold']
    assert row['command'][-2:] == ['--load', str(ROOT/'test/nelisp-native-boundary-driver.el')]
    for path, digest in row['pins'].items():
        assert sha(path) == digest, 'pin changed: ' + path
    for path in [work/('input-'+row['name']+'.elc'),
                 work/('source-'+row['name']+'.el'),
                 ROOT/'test/nelisp-native-boundary-test.py']:
        assert str(path) in row['pins'], 'missing input/runner pin: ' + str(path)
    if row['name'] == 'emacs-redisplay--ml-spans':
        assert str(work/'gui-helpers.el') in row['pins']
    prefix = work / (row['backend'] + '-' + row['name'] + '-' + row['phase'] + ('-'+row['timing_mode'] if 'timing_mode' in row else ''))
    output = prefix.with_suffix('.out').read_text()
    assert sha(prefix.with_suffix('.out')) == row['stdout_sha256']
    assert sha(prefix.with_suffix('.err')) == row['stderr_sha256']
    assert not prefix.with_suffix('.err').read_text() and 'P34-DONE\n' in output
    assert 'P34-BOOT '+row['name']+' '+row['phase']+'\n' in output
    if row['phase'] == 'compile':
        compiled=re.search(r'P34-COMPILE file=(\".*\") seconds=([0-9.]+)\n',output)
        assert compiled and json.loads(compiled[1]) in row['artifacts'] and float(compiled[2])>0
    if row['phase'] != 'compile':
        assert 'P34-CALL-TRUST-DELTA=(0 0 0 0 0)\n' in output
        assert 'P34-CALLABLE direct=t\n' in output or row['backend'] == 'template'
        assert re.search(r'P34-PARITY cases=[1-9][0-9]*\n', output)
    return output

def verify(data, criterion):
    work = Path(data['work'])
    rows = {(r['backend'], r['name'], r['phase']): r for r in data['rows']}
    assert len(rows) == len(data['rows']), 'duplicate rows'
    for path, digest in data['sources'].items():
        assert sha(ROOT / path) == digest, 'source changed: ' + path
    if criterion == 'P3.6':
        name = 'emacs-redisplay--ml-spans'
        compiled = rows['in-house', name, 'compile']
        output = raw(compiled, work)
        match = re.search(r'P34-COMPILE file=(\".*\") seconds=([0-9.]+)\n', output)
        assert compiled['timeout'] == 900 and compiled['seconds'] < 901
        assert match and 0 < float(match[2]) <= 600, 'ML compile exceeds 600 s'
        output = raw(rows['in-house', name, 'parity'], work)
        observations = re.search(r'P34-PARITY-OBSERVATIONS values=(\d+) errors=(\d+)\n', output)
        assert observations and int(observations[1]) >= 3
        assert int(observations[1]) + int(observations[2]) == 10
        return
    for name, count in [('p34-boundary-host', 34), ('p34-runtime-host', 12)]:
        host = data['host_controls'][name]
        assert host['rc'] == 0 and 0 < host['seconds'] < 1801
        assert host['command'][:4] == ['timeout', '-k', '5', '1800']
        assert sha(host['log']) == host['log_sha256']
        assert f'Ran {count} tests, {count} results as expected, 0 unexpected' in Path(host['log']).read_text()
        for path, digest in host['pins'].items():
            assert sha(ROOT/path) == digest, 'host source changed: '+path
        assert all(path in host['pins'] for path in ['scripts/nelisp-standalone-build.el',
                   'test/nelisp-standalone-gc-test.el','test/nelisp-native-boundary-compiler-test.el',
                   'test/nelisp-native-equal-word-fixture.el',
                   'test/nelisp-native-vm-vector-unit-fixture.el',
                   'test/nelisp-native-vm-arithmetic-unit-fixture.el',
                   'test/nelisp-native-vm-builtin-difference-fixture.el',
                   'test/nelisp-native-vm-cons-fixture.el',
                   'test/nelisp-native-vm-frame-fixture.el',
                   'test/nelisp-native-vm-dynamic-frame-fixture.el',
                   'test/nelisp-native-vm-marker-fixture.el'])
    if criterion == 'P3.4':
        guards = data['guard_controls']
        assert len(guards) == 2 and {r['mode'] for r in guards} == {'negative', 'positive'}
        for guard in guards:
            assert guard['command'][:4] == ['timeout', '-k', '5', '120']
            assert 0 < guard['seconds'] < 121
            for path, digest in guard['pins'].items():
                assert sha(path) == digest, 'guard control pin changed: ' + path
            output = Path(guard['stdout']).read_text()
            errors = Path(guard['stderr']).read_text()
            if guard['mode'] == 'positive':
                assert guard['command'][4] == str(ROOT/'target/nelisp-static')
                assert guard['rc'] == 0 and not errors
                assert 'P34-JIT-GUARD-PASS checks=5 gc=1\n' in output
            else:
                assert guard['rc'] not in [0, 124, 137]
                assert 'True guard must suppress recursive dispatch' in errors
        controls = data['bulk_root_controls']
        assert len(controls) == 4
        for backend in ['in-house', 'gccjit']:
            for mode in ['negative', 'positive']:
                row = next(r for r in controls if r['backend'] == backend and r['mode'] == mode)
                assert 0 < row['seconds'] < 1801 and row['identity_unchanged']
                assert row['command'][:4] == ['timeout', '-k', '5', '1800']
                binary = ROOT/('target/nelisp-dyn' if backend == 'gccjit' else 'target/nelisp-static')
                source = ROOT/'target/evidence-final'/('p34-bulk-roots'+('-negative' if mode == 'negative' else '')+'.el')
                required = [binary, Path(str(binary)+'.cold'), Path(str(binary)+'.native-startup.el'),
                            source, ROOT/'scripts/nelisp-standalone-build.el', ROOT/'lisp/nelisp-cc-rootstack.el']
                assert all(str(path) in row['pins'] for path in required)
                for path, digest in row['pins'].items():
                    assert sha(path) == digest, 'bulk root control pin changed: ' + path
                output = Path(row['log']).read_text()
                assert sha(row['log']) == row['sha256']
                if mode == 'positive':
                    assert row['rc'] == 0 and 'P34-BULK-ROOTS-PASS controls=11 gc=1 nested=1\n' in output
                else:
                    assert row['rc'] not in [0, 124, 137] and 'Bank-only object lost across GC' in output
                    assert 'P34-BULK-ROOTS-PASS' not in output
        for backend in BACKENDS:
            for name in LEAVES:
                raw(rows[backend, name, 'compile'], work)
                parity = raw(rows[backend, name, 'parity'], work)
                if name == 'file-exists-p':
                    assert 'P34-EXISTENCE-REFERENCE-PASS cases=2005\n' in parity
                    assert 'P34-OS-REBINDING-PASS calls=2\n' in parity
                row = rows[backend, name, 'timing']
                assert str(work / ('caller-' + name + '.elc')) in row['pins']
                runs=row['runs']
                assert len(runs)==3 and [r['timing_mode'] for r in runs]==['before','native','after']
                values=[]
                for run in runs:
                    output=raw(run,work)
                    match=re.search(r'P34-TIME mode='+run['timing_mode']+r' seconds=([0-9.]+) calls=(\d+)\n',output)
                    assert match and int(match[2])==100000 and run['calls']==100000
                    value=float(match[1]);assert value>0 and value==run['measured_seconds']
                    assert run['load_before'][0]<4 and run['load_after'][0]<4 and run['load_peak']<4
                    assert 'P34-CALLER-ATTESTATION calls=3\n' in output
                    values.append(value)
                before,native,after=values
                assert values==[row['interpreted_before'],row['native'],row['interpreted_after']]
                assert .8<=before/after<=1.2,'unstable baseline'
                if backend != 'template':
                    assert native <= min(before, after) / 2, (backend, name, before, native, after)
    elif criterion == 'P3.5':
        controls = data['compiler_projection_controls']
        assert len(controls) == 2
        generated = [*sorted((ROOT/'target/nelisp-compiler-bytecode').glob('*.el')),
                     ROOT/'target/nelisp-compiler-bytecode-load.el',
                     ROOT/'target/nelisp-compiler-bytecode-manifest.el',
                     ROOT/'target/nelisp-structural-bytecode.el',
                     ROOT/'target/nelisp-optimizer-bytecode.el',
                     ROOT/'scripts/nelisp-native-optimizer-bytecode.el',
                     ROOT/'scripts/nelisp-stdlib-prelude.el',
                     ROOT/'vendor/staged-emacs-lisp/subr.el',
                     ROOT/'test/nelisp-native-boundary-bytecode-test.el']
        for mode in ['negative', 'positive']:
            row = next(r for r in controls if r['mode'] == mode)
            assert 0 < row['seconds'] < 1801 and row['identity_unchanged']
            assert row['command'][:4] == ['timeout', '-k', '5', '1800']
            assert all(str(path) in row['pins'] for path in generated)
            for path, digest in row['pins'].items():
                assert sha(path) == digest, 'compiler projection pin changed: ' + path
            assert sha(row['stdout']) == row['stdout_sha256']
            assert sha(row['stderr']) == row['stderr_sha256']
            output = Path(row['stdout']).read_text() + Path(row['stderr']).read_text()
            assert len(re.findall(r'^P35-UNIT-BEFORE ', output, re.M)) == 6 and len(re.findall(r'^P35-UNIT-AFTER ', output, re.M)) == 6
            if mode == 'positive':
                assert row['rc'] == 0 and 'P35-AOT-UNIT-PARITY-PASS cases=6\n' in output
            else:
                assert row['rc'] not in [0, 124, 137] and re.search(r'^P35-AOT-BROKEN-UNIT-DETECTED$', output, re.M)
                # GNU backtraces include source strings for unexecuted PRINCs.
                assert not re.search(r'^P35-AOT-UNIT-PARITY-PASS cases=\d+$', output, re.M)
        for backend in BACKENDS:
            raw(rows[backend, 'p34-values', 'compile'], work)
            output = raw(rows[backend, 'p34-values', 'parity'], work)
            assert 'P34-PARITY cases=16\n' in output
            assert 'P35-ATOM-CACHE-PASS unique=1109 capacity=1024\n' in output
            assert 'P35-STRUCTURAL-EQUAL-PASS cases=16\n' in output
            assert 'P35-HELPER-PROJECTION-PASS cases=35\n' in output
            assert 'P35-SEQUENCE-PROJECTION-PASS cases=30\n' in output
            assert 'P35-JIT-STUB-PROJECTION-PASS controls=9\n' in output
            assert 'P35-GENSYM-PROJECTION-PASS controls=2\n' in output
            assert 'P35-VECTOR-VM-PASS controls=104\n' in output
            assert 'P35-ARITHMETIC-VM-PASS controls=506\n' in output
            assert 'P35-CALL-DIFFERENCE-VM-PASS controls=337\n' in output
            for p in [work/'vm-vector-helpers.el',work/'vm-call-difference-helpers.el',
                      ROOT/'test/nelisp-native-vm-call-difference-callers.el',
                      ROOT/'test/nelisp-native-vm-call-difference-fixture.el',ROOT/'test/nelisp-native-vm-vector-callers.el',
                      ROOT/'test/nelisp-native-vm-vector-fixture.el',ROOT/'test/nelisp-native-vm-arithmetic-fixture.el']:
                assert str(p) in rows[backend,'p34-values','parity']['pins']
        for backend in BACKENDS:
            for name in WIDE + ['p34-optional', 'p34-rest']:
                compile_row = rows[backend, name, 'compile']
                raw(compile_row, work)
                if name in WIDE and backend != 'template':
                    assert compile_row['timeout'] == 290 and compile_row['seconds'] <= 290
                output = raw(rows[backend, name, 'parity'], work)
                if name == 'p34-rest':
                    assert 'P34-REST-COPY-PASS\n' in output
                if name in ['p34-optional', 'p34-rest'] and backend != 'template':
                    assert 'P34-ARITY-METADATA-PASS\n' in output
                if name == 'file-exists-p':
                    assert 'P34-EXISTENCE-REFERENCE-PASS cases=2005\n' in output
                if name=='emacs-redisplay--ml-spans':
                    matches=re.search(r'P34-PARITY-OBSERVATIONS values=(\d+) errors=(\d+)\n',output)
                    assert matches and int(matches[1])>=3 and int(matches[1])+int(matches[2])==10
                if name in WIDE[:3]:
                    assert re.search(r'FP-PARITY\|' + re.escape(name) + r'\|cases=2000\|mismatches=0\|', output)
    else:
        raise AssertionError('Unknown criterion: ' + criterion)

def assemble(work):
    rows = [json.loads(p.read_text()) for p in sorted(work.glob('*-compile.json'))
            if p.name.split('-compile')[0]]
    rows += [json.loads(p.read_text()) for phase in ['parity', 'timing']
             for p in sorted(work.glob('*-' + phase + '.json'))]
    sources = {str(p.relative_to(ROOT)): sha(p)
               for p in [ROOT/'test/nelisp-native-boundary-driver.el',
                         ROOT/'scripts/nelisp-standalone-build.el',
                         ROOT/'scripts/nelisp-stdlib-prelude.el', ROOT/'lisp/nelisp-cc-rootstack.el',
                         ROOT/'scripts/nelisp-native-optimizer-bytecode.el',
                         ROOT/'vendor/staged-emacs-lisp/subr.el', ROOT/'test/nelisp-standalone-gc-test.el',
                         *sorted((ROOT/'lisp').glob('*.el')),
                         ROOT/'test/nelisp-native-boundary-bytecode-test.el',
                         ROOT/'test/nelisp-native-boundary-compiler-test.el',
                         ROOT/'test/nelisp-native-equal-word-fixture.el',
                         ROOT/'test/nelisp-native-vm-vector-unit-fixture.el',
                         ROOT/'test/nelisp-native-vm-arithmetic-unit-fixture.el',
                         ROOT/'test/nelisp-native-vm-builtin-difference-fixture.el',
                         ROOT/'test/nelisp-native-vm-cons-fixture.el',
                         ROOT/'test/nelisp-native-vm-frame-fixture.el',
                         ROOT/'test/nelisp-native-vm-dynamic-frame-fixture.el',
                         ROOT/'test/nelisp-native-vm-marker-fixture.el',
                         ROOT/'test/nelisp-native-vm-call-difference-callers.el',
                         ROOT/'test/nelisp-native-vm-call-difference-fixture.el',
                         ROOT/'test/nelisp-native-vm-vector-callers.el',
                         ROOT/'test/nelisp-native-vm-vector-fixture.el',
                         ROOT/'test/nelisp-native-vm-arithmetic-fixture.el',
                         ROOT/'test/nelisp-native-boundary-fixtures.el',
                         ROOT/'test/nelisp-native-boundary-file-corpus.el',
                         ROOT/'test/nelisp-native-boundary-test.py', Path(__file__)]}
    controls = ROOT/'target/evidence-final'
    def control(name):
        path = controls/name
        return json.loads(path.read_text()) if path.exists() else []
    return dict(schema=1, work=str(work.resolve()), rows=rows, sources=sources,
                guard_controls=json.loads((ROOT/'target/p34-cont/guard.json').read_text()),
                bulk_root_controls=control('p34-bulk-roots.json'),
                compiler_projection_controls=control('compiler-projection-controls.json'),
                host_controls={name:control(name+'.json') for name in ['p34-boundary-host','p34-runtime-host']})

def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--work', type=Path)
    parser.add_argument('--receipt', type=Path, required=True)
    parser.add_argument('--criterion', choices=['P3.4', 'P3.5', 'P3.6'])
    parser.add_argument('--assemble', action='store_true')
    args = parser.parse_args()
    if args.assemble:
        assert args.work is not None
        data = assemble(args.work)
        results = {}
        for criterion in ['P3.4', 'P3.5', 'P3.6']:
            try:
                verify(data, criterion)
                results[criterion] = dict(passed=True, reason='Raw bounded, pinned measurements verified.')
            except (AssertionError, KeyError, OSError) as error:
                results[criterion] = dict(passed=False, reason=explain_failure(error))
        data['criteria'] = results
        args.receipt.write_text(json.dumps(data, indent=2) + '\n')
        print(json.dumps(results, indent=2))
    else:
        data = json.loads(args.receipt.read_text())
        verify(data.get('p34', data), args.criterion)
        print(args.criterion + '-PASS')

if __name__ == '__main__':
    main()
