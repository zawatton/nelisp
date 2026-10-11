"""Lightweight synthetic raw evidence controls for the CI importer (no reader)."""
import copy
from pathlib import Path
import shutil
import subprocess
import sys
import tempfile


def self_test(ci):
    (ci.ROOT / 'target').mkdir(exist_ok=True)
    with tempfile.TemporaryDirectory(prefix='ci-corpus-self-test-', dir=ci.ROOT / 'target') as temporary:
        root = Path(temporary)
        artifacts = root / 'artifacts'
        reader_dir = artifacts / 'linux-native-reader'
        reader_dir.mkdir(parents=True)
        files = {}
        for name in ('nelisp', 'nelisp.cold', 'nelisp.native-startup.el', 'nelisp-artifact-runtime.el.nelc'):
            p = reader_dir / name
            p.write_text('synthetic test artifact: ' + name)
            files['target/' + name] = ci.sha(p)
        manifest = dict(schema=1, platform='linux-x86_64', **ci.source_identity(), artifacts_sha256=files,
                        binary_sha256=files['target/nelisp'], cold_sha256=files['target/nelisp.cold'],
                        startup_sha256=files['target/nelisp.native-startup.el'])
        ci.write(reader_dir / 'ci-reader.json', manifest)
        identity = {k: manifest[k] for k in ('source_head', 'source_diff_sha256', 'binary_sha256',
                                             'cold_sha256', 'startup_sha256')}
        identity.update(platform='posix', process_deadline=290,
                        platform_runner_sha256=manifest['sources_sha256']['test/support/native_corpus_platform.py'],
                        process_runner_sha256=manifest['sources_sha256']['test/support/run-windows-native-f1.py'])
        valid = sorted(r['opcode'] for r in ci.read(ci.ROOT / 'test/fixtures/native-bytecode/gnu-31.1-valid-fixtures.json')['valid'])
        expected = ci.expected_f3()
        for backend in ci.BACKENDS:
            for shard in range(ci.SHARDS):
                directory = artifacts / ('linux-native-u10-' + backend + '-' + str(shard)) / ('u10-corpus-' + str(shard))
                work = directory / backend
                work.mkdir(parents=True)
                shutil.copyfile(reader_dir / 'nelisp.native-startup.el', work / 'reader.native-startup.el')
                (directory / 'fixtures.el').write_text('(synthetic-fixture)\n')
                projections = {str(op): 'projection-' + str(op) for op in [*valid, 'protected']}
                ci.write(directory / 'audit.json', dict(pending=[], projections=projections,
                         fixtures=[dict(opcode=op, relocation_refusal=False) for op in valid]))
                selected = valid[shard::ci.SHARDS]
                group = selected + (['protected'] if shard == 0 else [])
                receipts = []
                for phase in ('compile', 'load'):
                    label = '-'.join(map(str, group)) + '-' + phase
                    output = ''.join(f'U10-FIXTURE-PASS backend={backend} opcode={op} phase={phase} entries=1 gc=1\n' for op in selected)
                    if 192 in selected:
                        output += ''.join('U10-STALE-PASS control=' + c + '\n' for c in ('input', 'abi', 'artifact'))
                    if shard == 0:
                        output += 'U10-ORDER-PASS cases=4 entries=4\n'
                    output += f'U10-BATCH-DONE backend={backend} phase={phase} fixtures={len(selected)} entries={len(selected)}\n'
                    (work / (label + '.out')).write_text(output)
                    (work / (label + '.err')).write_text('')
                    fixture = work / (label + '-fixtures.el')
                    fixture.write_text('(synthetic-worker)\n')
                    receipts.append(dict(**identity, backend=backend, phase=phase, opcodes=group, rc=0,
                        seconds=1, passed=True, fixture_sha256=ci.sha(directory / 'fixtures.el'),
                        sources_sha256={'test/support/run-native-corpus-u10.py': manifest['sources_sha256']['test/support/run-native-corpus-u10.py']},
                        projection_sha256={str(op): projections[str(op)] for op in group},
                        worker_fixture_sha256=ci.sha(fixture)))
                ci.write(directory / 'summary.json', dict(passed=True, pending=[], focused=False,
                         shard=[shard, ci.SHARDS], selected=selected, receipts=receipts))
                ci.write(directory / 'ci-u10.json', dict(schema=1, kind='u10', backend=backend,
                         complete=True, reader=manifest, files_sha256=ci.raw_files(directory)))
            directory = artifacts / ('linux-native-f3-' + backend) / 'f3-corpus-test'
            directory.mkdir(parents=True)
            for name in ('fixture.el', 'reproduced.el'):
                shutil.copyfile(ci.ROOT / 'test/support/native-real-corpus-fixtures.el', directory / name)
            ci.write(directory / 'corpus.json', [dict(**r, excluded=None) for r in expected.values()])
            (directory / backend).mkdir()
            shutil.copyfile(reader_dir / 'nelisp.native-startup.el', directory / backend / 'reader.native-startup.el')
            names = list(expected)
            rows = []
            for phase in ('compile', 'load'):
                work = directory / backend / (phase + '-0')
                work.mkdir(parents=True)
                output = ''.join(f'F3-FUNCTION-PASS backend={backend} name={n} phase={phase} cases={expected[n]["cases"]} native-entries={expected[n]["cases"]} validations=0 seconds=0.01\n' for n in names)
                if phase == 'compile':
                    output += ''.join(f'F3-CACHE backend={backend} name={n} status=miss\nF3-CACHE-FILE name={n} file=synthetic/{n}\n' for n in names)
                output += f'F3-BATCH-DONE backend={backend} phase={phase} functions=52 failed=0\n'
                (work / (phase + '.out')).write_text(output)
                (work / (phase + '.err')).write_text('')
                row = dict(**identity, backend=backend, names=names, passed_names=names, failures=[],
                           phase=phase, rc=0, seconds=1, passed=True, cache_mode='fresh',
                           cache_hits=0, cache_misses=52 if phase == 'compile' else 0,
                           fixture_sha256=ci.sha(directory / 'fixture.el'), work_directory=phase + '-0')
                for key, source in [('driver_sha256', 'test/standalone-native-real-corpus-driver.el'),
                                    ('observer_sha256', 'test/support/native-entry-observer.el'),
                                    ('input_protocol_sha256', 'test/support/native-real-corpus-inputs.el'),
                                    ('runner_sha256', 'test/support/run-native-real-corpus.py')]:
                    row[key] = manifest['sources_sha256'][source]
                ci.write(work / (phase + '.json'), row)
                rows.append(row)
            ci.write(directory / 'receipts.json', rows)
            (directory / 'runner.log').write_text(f'F3-CORPUS-PASS backend={backend} functions=52 digest={ci.sha(directory / "fixture.el")}\nF3-PROCESS-LIMIT limit=2 observed-max=2\n')
            ci.write(directory / 'ci-f3.json', dict(schema=1, kind='f3', backend=backend,
                     complete=True, reader=manifest, files_sha256=ci.raw_files(directory)))
            ci.write(artifacts / ('linux-native-u10-verdict-' + backend) / ('linux-u10-' + backend + '-verdict.json'),
                     ci.verify_u10(list((artifacts).glob('linux-native-u10-' + backend + '-*/u10-corpus-*/ci-u10.json')), backend, manifest))
        ci.verify_set(artifacts)
        for backend in ci.BACKENDS:
            shard_set = root / ('shards-' + backend)
            shard_set.mkdir()
            for directory in artifacts.glob('linux-native-u10-' + backend + '-*'):
                shutil.copytree(directory, shard_set / directory.name)
            verdict = root / ('merged-' + backend + '.json')
            command = [sys.executable, str(Path(ci.__file__)), str(shard_set), '--merge-u10',
                       '--backend', backend, '--require-complete', '--reader',
                       str(reader_dir / 'ci-reader.json'), '--output', str(verdict)]
            result = subprocess.run(command, capture_output=True, text=True)
            ci.require(result.returncode == 0, 'Merge CLI failed: ' + result.stdout + result.stderr)
            ci.require(ci.read(verdict) == ci.read(artifacts / ('linux-native-u10-verdict-' + backend)
                       / ('linux-u10-' + backend + '-verdict.json')), 'Merge CLI verdict changed')
        output = root / 'evidence-final'
        ci.write(output / 'u10-both.json', dict(rc=124))
        (output / 'u10-both.log').write_text('old incomplete corpus\n')
        ci.import_set(artifacts, output)
        ci.require(ci.read(output / 'ci-corpus-previous/u10-both.json') == dict(rc=124)
                   and (output / 'ci-corpus-previous/u10-both.log').read_text() == 'old incomplete corpus\n',
                   'Legacy evidence backup changed')
        for kind, markers in [('u10', ['U10-PASS backend=' + b + ' fixtures=230 pending=0' for b in ci.BACKENDS]),
                              ('f3', ['F3-CORPUS-PASS backend=' + b + ' functions=52' for b in ci.BACKENDS])]:
            require = ci.require
            require(ci.read(root / 'evidence-final' / (kind + '-both.json'))['rc'] == 0, 'Bad acceptance rc')
            require(all(m in (root / 'evidence-final' / (kind + '-both.log')).read_text() for m in markers), 'Bad acceptance markers')
        controls = []

        def control(name, mutate, expected_error):
            broken = root / 'broken'
            shutil.copytree(artifacts, broken)
            mutate(broken)
            try:
                ci.verify_set(broken)
            except (ValueError, KeyError, OSError) as error:
                ci.require(expected_error in str(error), name + ' failed for wrong reason: ' + str(error))
                controls.append(name)
            else:
                raise AssertionError(name + ' was accepted')
            shutil.rmtree(broken)

        def update(path, mutate):
            value = ci.read(path)
            mutate(value)
            ci.write(path, value)

        def u10_path(p):
            return next(p.glob('linux-native-u10-in-house-0/u10-corpus-*/ci-u10.json'))

        def f3_path(p):
            return next(p.glob('linux-native-f3-in-house/f3-corpus-*/ci-f3.json'))

        def raw_update(seal, filename, mutate):
            update(seal.parent / filename, mutate)
            update(seal, lambda s: s.update(files_sha256=ci.raw_files(seal.parent)))

        control('different HEAD', lambda p: update(p / 'linux-native-reader/ci-reader.json', lambda r: r.update(source_head='0' * 40)), 'Different source')
        control('different diff', lambda p: update(p / 'linux-native-reader/ci-reader.json', lambda r: r.update(source_diff_sha256='0' * 64)), 'Different source')
        control('different source bytes', lambda p: update(p / 'linux-native-reader/ci-reader.json', lambda r: r['sources_sha256'].update({'Makefile': '0' * 64})), 'Different source')
        control('missing shard', lambda p: shutil.rmtree(u10_path(p).parent), 'Missing shard')
        control('duplicate shard', lambda p: raw_update(u10_path(p), 'summary.json', lambda r: r.update(shard=[1, 8])), 'Missing, duplicate')
        control('incomplete merged verdict', lambda p: update(p / 'linux-native-u10-verdict-in-house/linux-u10-in-house-verdict.json', lambda r: r.update(complete=False)), 'Incomplete or altered')
        control('skip', lambda p: update(u10_path(p), lambda r: r.update(skip=True)), 'skipped shard')
        control('backend mismatch', lambda p: raw_update(u10_path(p), 'summary.json', lambda r: r['receipts'][0].update(backend='gccjit')), 'Backend mismatch')
        control('reader hash', lambda p: (p / 'linux-native-reader/nelisp').write_text('changed'), 'Reader artifact hash')
        control('startup hash', lambda p: (p / 'linux-native-reader/nelisp.native-startup.el').write_text('changed'), 'Reader artifact hash')
        control('pending slot', lambda p: raw_update(u10_path(p), 'summary.json', lambda r: r.update(pending=[48])), 'Incomplete shard')
        control('skipped native receipt', lambda p: raw_update(u10_path(p), 'summary.json', lambda r: r['receipts'][0].update(skip=True)), 'skipped receipt')
        control('relaxed deadline', lambda p: raw_update(u10_path(p), 'summary.json', lambda r: r['receipts'][0].update(process_deadline=1800)), 'relaxed deadline')
        control('incomplete F3', lambda p: raw_update(f3_path(p), 'receipts.json', lambda r: r.pop()), 'Incomplete F3 verdict')
        control('wrong F3 names', lambda p: raw_update(f3_path(p), 'corpus.json', lambda r: r[0].update(name='bogus')), 'altered F3 corpus')
        control('missing F3 backend', lambda p: shutil.rmtree(f3_path(p).parent), 'F3 backend artifact')
        control('F3 skip', lambda p: update(f3_path(p), lambda r: r.update(skip=True)), 'skipped F3')
        control('F3 backend mismatch', lambda p: raw_update(f3_path(p), 'receipts.json', lambda r: r[0].update(backend='gccjit')), 'Backend mismatch')
        control('non-fresh F3', lambda p: raw_update(f3_path(p), 'receipts.json', lambda r: r[0].update(cache_mode='durable')), 'non-fresh cache')
        def changed_transcript(p):
            seal = f3_path(p)
            (seal.parent / 'in-house/compile-0/compile.out').write_text('F3-SKIP\n')
            update(seal, lambda r: r.update(files_sha256=ci.raw_files(seal.parent)))
        control('unsubstantiated transcript', changed_transcript, 'transcript refuses')
        # The same source checker also rejects actual local HEAD/diff changes.
        local = copy.deepcopy(ci.source_identity())
        local['source_diff_sha256'] = '1' * 64
        try:
            ci.verify_set(artifacts, local)
        except ValueError as error:
            ci.require('Different source' in str(error), 'Wrong local source refusal')
            controls.append('local source mismatch')
        else:
            raise AssertionError('Different local source accepted')
        # Sealing path used by CI, including F3 directory mapping, also works.
        ci.stamp(f3_path(artifacts).parent, 'f3', 'in-house', manifest,
                 root / 'evidence-final/ci-corpus/linux-native-f3-in-house/f3-corpus-test/runner.log')
        ci.verify_set(artifacts)
        print('CI-CORPUS-SELF-TEST-PASS positive=6 negative=' + str(len(controls)))
        print('REFUSALS: ' + ', '.join(controls))
