#!/usr/bin/env python3
"""Seal, verify and import Linux CI U10/F3 evidence without running a reader.

Download *all* linux-native-* artifacts from one workflow run into a directory,
then run this script DIRECTORY. It writes target/evidence-final/{u10,f3}-both
.json/.log plus the preserved raw evidence. HEAD and the complete source diff
must match; a remote reader need not match a locally built ELF.

CI uses --record-reader, --seal-u10, --seal-f3 and --merge-u10. All paths in
receipts are relative, so downloaded artifacts can be moved between machines.
"""
import argparse
import hashlib
import importlib.util
import json
from pathlib import Path
import re
import shutil
import subprocess
import sys

ROOT = Path(__file__).resolve().parents[2]
BACKENDS = ('in-house', 'gccjit')
SHARDS = 8
SOURCE_DIRS = ('src', 'lisp', 'scripts', 'packages', 'test', 'tools', '.github')


def require(condition, message):
    if not condition:
        raise ValueError(message)


def sha(path):
    with Path(path).open('rb') as stream:
        return hashlib.file_digest(stream, 'sha256').hexdigest()


def read(path):
    return json.loads(Path(path).read_text())


def write(path, value):
    path = Path(path)
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(json.dumps(value, indent=2, sort_keys=True) + '\n')


def git(root, *args):
    return subprocess.check_output(['git', '-C', str(root), *args])


def source_identity(root=ROOT):
    """Pin HEAD, binary diff (index + worktree), and every tracked source byte.

    Untracked source files also enter the diff hash. Generated/ignored outputs
    and unrelated session directories do not. A clean checkout has the SHA256
    of an empty diff, independent of timestamps and absolute checkout paths.
    """
    paths = git(root, 'ls-files', '-z').decode().split('\0')[:-1]
    sources = {p: sha(root / p) for p in paths if (root / p).is_file()}
    diff = bytearray(git(root, 'diff', '--no-ext-diff', '--binary', 'HEAD', '--'))
    extras = git(root, 'ls-files', '--others', '--exclude-standard', '-z', '--',
                 *SOURCE_DIRS, 'Makefile').decode().split('\0')[:-1]
    for p in sorted(extras):
        if '__pycache__' in Path(p).parts or Path(p).suffix in ('.pyc', '.pyo', '.elc', '.nelc', '.o'):
            continue
        sources[p] = sha(root / p)
        diff.extend(b'\0untracked\0' + p.encode() + b'\0' + (root / p).read_bytes())
    return dict(source_head=git(root, 'rev-parse', 'HEAD').decode().strip(),
                source_diff_sha256=hashlib.sha256(diff).hexdigest(), sources_sha256=sources)


def reader_identity(root=ROOT):
    paths = ['target/nelisp', 'target/nelisp.cold', 'target/nelisp.native-startup.el',
             'target/nelisp-artifact-runtime.el.nelc']
    require(all((root / p).is_file() for p in paths), 'Reader/startup/cold/runtime missing')
    artifacts = {p: sha(root / p) for p in paths}
    # Preserve additional runtime transport files the build produced as well.
    artifacts.update({str(p.relative_to(root)): sha(p)
                      for p in (root / 'target').glob('nelisp-artifact-runtime.el*') if p.is_file()})
    return dict(schema=1, platform='linux-x86_64', **source_identity(root),
                binary_sha256=artifacts[paths[0]], cold_sha256=artifacts[paths[1]],
                startup_sha256=artifacts[paths[2]], artifacts_sha256=artifacts)


def module(name, filename):
    spec = importlib.util.spec_from_file_location(name, Path(__file__).with_name(filename))
    result = importlib.util.module_from_spec(spec)
    sys.path.insert(0, str(Path(__file__).parent))
    spec.loader.exec_module(result)
    return result


U10 = module('ci_u10', 'run-native-corpus-u10.py')
F3 = module('ci_f3', 'run-native-real-corpus.py')


def check_reader(manifest, local=None):
    require(manifest.get('schema') == 1 and manifest.get('platform') == 'linux-x86_64',
            'Reader platform/schema mismatch')
    for key in ('source_head', 'source_diff_sha256', 'sources_sha256', 'artifacts_sha256',
                'binary_sha256', 'cold_sha256', 'startup_sha256'):
        require(bool(manifest.get(key)), 'Missing reader identity: ' + key)
    for key, path in [('binary_sha256', 'target/nelisp'), ('cold_sha256', 'target/nelisp.cold'),
                      ('startup_sha256', 'target/nelisp.native-startup.el')]:
        require(manifest[key] == manifest['artifacts_sha256'].get(path), 'Reader hash disagreement')
    if local is not None:
        require(all(manifest[k] == local[k] for k in
                    ('source_head', 'source_diff_sha256', 'sources_sha256')), 'Different source identity')


def check_row(row, backend, manifest):
    require(row.get('backend') == backend and backend in BACKENDS, 'Backend mismatch')
    require(row.get('platform') == 'posix', 'Platform mismatch')
    require(row.get('passed') is True and row.get('rc') == 0 and not row.get('skip'),
            'Failed or skipped receipt')
    require(row.get('process_deadline') == 290 and 0 <= row['seconds'] < 290,
            'Incomplete verdict or relaxed deadline')
    for key in ('binary_sha256', 'cold_sha256', 'startup_sha256', 'source_head', 'source_diff_sha256'):
        require(row.get(key) == manifest[key], 'Receipt identity mismatch: ' + key)
    for key, filename in [('platform_runner_sha256', 'native_corpus_platform.py'),
                          ('process_runner_sha256', 'run-windows-native-f1.py')]:
        require(row.get(key) == manifest['sources_sha256'].get('test/support/' + filename),
                'Runner source mismatch')


def raw_files(directory):
    return {str(p.relative_to(directory)): sha(p) for p in sorted(directory.rglob('*'))
            if p.is_file() and p.suffix in ('.json', '.out', '.err', '.el', '.log')
            and p.name not in ('ci-u10.json', 'ci-f3.json')}


def check_files(directory, seal):
    for relative, digest in seal['files_sha256'].items():
        p = directory / relative
        require(not Path(relative).is_absolute() and '..' not in Path(relative).parts,
                'Unsafe evidence path')
        require(p.is_file() and not p.is_symlink() and sha(p) == digest,
                'Missing or changed raw evidence: ' + relative)
    require(raw_files(directory) == seal['files_sha256'], 'Raw evidence set changed')


def text(path):
    return Path(path).read_bytes().decode('utf-8', errors='replace').replace('\r\n', '\n')


def verify_u10(paths, backend, manifest):
    require(len(paths) == SHARDS, 'Missing shard: require eight')
    summaries = [read(p.parent / 'summary.json') for p in paths]
    require(sorted(s.get('shard', []) for s in summaries) == [[i, SHARDS] for i in range(SHARDS)],
            'Missing, duplicate or mismatched shard')
    valid = sorted(row['opcode'] for row in read(ROOT / 'test/fixtures/native-bytecode/gnu-31.1-valid-fixtures.json')['valid'])
    coverage = dict(compile=[], load=[])
    protected = dict(compile=0, load=0)
    identity = None
    for path, summary in zip(paths, summaries):
        seal = read(path)
        require(seal['kind'] == 'u10' and seal['backend'] == backend and seal['reader'] == manifest,
                'Backend or source mismatch in shard seal')
        require(seal.get('complete') is True and not seal.get('skip'), 'Incomplete or skipped shard')
        check_files(path.parent, seal)
        require(summary['passed'] is True and not summary['pending'] and not summary['focused']
                and summary['selected'], 'Incomplete shard verdict')
        metadata = read(path.parent / 'audit.json')
        require(sorted(row['opcode'] for row in metadata['fixtures']) == valid and not metadata['pending'],
                'Incomplete 230-slot audit')
        require(len(metadata['fixtures']) == 230 and len(valid) == 230, 'U10 fixture count mismatch')
        refused = [row['opcode'] for row in metadata['fixtures'] if row['relocation_refusal']]
        require(sha(path.parent / backend / 'reader.native-startup.el') == manifest['startup_sha256'],
                'U10 copied startup hash mismatch')
        local = dict(compile=[], load=[])
        for row in summary['receipts']:
            check_row(row, backend, manifest)
            require(row['phase'] in local and row['opcodes'], 'Invalid U10 phase/group')
            key = {k: row[k] for k in ('fixture_sha256', 'sources_sha256', 'platform_runner_sha256', 'process_runner_sha256')}
            if identity is None:
                identity = key
            require(identity == key, 'U10 source/fixture identity changed between shards')
            require(sha(path.parent / 'fixtures.el') == row['fixture_sha256'], 'U10 fixture changed')
            for source, digest in row['sources_sha256'].items():
                expected = manifest['sources_sha256'].get(source, manifest['artifacts_sha256'].get(source))
                require(digest == expected, 'U10 source pin mismatch: ' + source)
            group, phase = row['opcodes'], row['phase']
            label = '-'.join(map(str, group)) + '-' + phase
            work = path.parent / backend
            require(U10.verdict(backend, phase, group, row['rc'], row['seconds'],
                               text(work / (label + '.out')), text(work / (label + '.err')),
                               refused, row['process_deadline']), 'U10 transcript refuses verdict')
            require(sha(work / (label + '-fixtures.el')) == row['worker_fixture_sha256'], 'Worker fixture changed')
            for op in group:
                require(row['projection_sha256'][str(op)] == metadata['projections'][str(op)], 'Projection changed')
                if op == 'protected':
                    protected[phase] += 1
                else:
                    local[phase].append(op)
                    coverage[phase].append(op)
        require(all(sorted(local[p]) == summary['selected'] for p in local), 'Incomplete shard selection')
    require(all(sorted(coverage[p]) == valid and protected[p] == 1 for p in coverage),
            'Incomplete U10 verdict: all 230 slots and protected controls required twice')
    return dict(schema=1, kind='u10-verdict', backend=backend, passed=True, complete=True,
                fixtures=230, entries=460, shards=SHARDS, reader=manifest,
                shard_verdicts=[dict(shard=s['shard'], passed=s['passed'],
                                     seal_sha256=sha(p), summary_sha256=sha(p.parent / 'summary.json'))
                                for p, s in sorted(zip(paths, summaries), key=lambda item: item[1]['shard'])])


def expected_f3():
    # Generated rows start at column two; byte-code strings may contain newlines.
    rows = {}
    for line in re.split(r'(?=^  \(:name )', (ROOT / 'test/support/native-real-corpus-fixtures.el').read_text(), flags=re.M):
        if line.startswith('  (:name ') and ':excluded nil :cases ' in line:
            name = re.match(r'  \(:name (\S+)', line)[1]
            rows[name] = dict(name=name, cases=line.count('(:args '),
                              bytecode_sha256=re.search(r':bytecode-sha256 "([a-f0-9]+)"', line)[1])
    require(len(rows) == 52, 'Local F3 fixture must have 52 admitted functions')
    return rows


def verify_f3(path, backend, manifest):
    seal = read(path)
    require(seal['kind'] == 'f3' and seal['backend'] == backend and seal['reader'] == manifest,
            'Backend or source mismatch in F3 seal')
    require(seal.get('complete') is True and not seal.get('skip'), 'Incomplete or skipped F3 verdict')
    check_files(path.parent, seal)
    require(sha(path.parent / backend / 'reader.native-startup.el') == manifest['startup_sha256'],
            'F3 copied startup hash mismatch')
    expected = expected_f3()
    names = list(expected)
    corpus = [r for r in read(path.parent / 'corpus.json') if not r['excluded']]
    require([r['name'] for r in corpus] == names and all(
        all(r[k] == expected[r['name']][k] for k in ('cases', 'bytecode_sha256')) for r in corpus),
        'Incomplete or altered F3 corpus')
    fixture = sha(ROOT / 'test/support/native-real-corpus-fixtures.el')
    require(sha(path.parent / 'fixture.el') == sha(path.parent / 'reproduced.el') == fixture,
            'F3 fixture/source reproduction mismatch')
    coverage = dict(compile=[], load=[])
    compile_hits = compile_misses = 0
    for row in read(path.parent / 'receipts.json'):
        check_row(row, backend, manifest)
        require(row['phase'] in coverage and row['names'] and row['cache_mode'] == 'fresh'
                and not row['failures'], 'Incomplete F3 phase or non-fresh cache')
        for key, filename in [('driver_sha256', 'test/standalone-native-real-corpus-driver.el'),
                              ('observer_sha256', 'test/support/native-entry-observer.el'),
                              ('input_protocol_sha256', 'test/support/native-real-corpus-inputs.el'),
                              ('runner_sha256', 'test/support/run-native-real-corpus.py')]:
            require(row.get(key) == manifest['sources_sha256'][filename], 'F3 source pin mismatch')
        require(row['fixture_sha256'] == fixture, 'F3 receipt fixture mismatch')
        require(set(row['names']) <= set(names), 'Unexpected F3 name')
        phase = row['phase']
        work = path.parent / backend / row['work_directory']
        require(re.fullmatch(phase + r'-[0-9]+', row['work_directory']), 'Unsafe F3 work path')
        output, errors = text(work / (phase + '.out')), text(work / (phase + '.err'))
        ok, passed, failures = F3.phase_verdict(backend, phase, row['names'], row['rc'], row['seconds'],
                                               output, errors, {n: expected[n]['cases'] for n in names}, 290)
        require(ok and passed == row['passed_names'] and failures == row['failures'],
                'F3 transcript refuses verdict')
        if phase == 'compile':
            cache = re.findall(r'^F3-CACHE backend=' + re.escape(backend) + r' name=(\S+) status=(hit|miss)$', output, re.M)
            files = re.findall(r'^F3-CACHE-FILE name=(\S+) file=(.+)$', output, re.M)
            require(cache == [(n, 'miss') for n in row['names']] and [n for n, _ in files] == row['names']
                    and row['cache_hits'] == 0 and row['cache_misses'] == len(row['names']),
                    'F3 fresh compile skipped or cache accounting mismatch')
            compile_hits += row['cache_hits']
            compile_misses += row['cache_misses']
        coverage[phase].extend(passed)
    require(all(coverage[p] == names for p in coverage) and compile_hits == 0 and compile_misses == 52,
            'Incomplete F3 verdict: all 52 functions required twice')
    log = text(path.parent / 'runner.log')
    require(log.count('F3-CORPUS-PASS backend=' + backend + ' functions=52 digest=' + fixture) == 1
            and log.count('F3-PROCESS-LIMIT limit=2 observed-max=2') == 1, 'F3 runner skipped or process limit missing')
    return dict(backend=backend, passed=True, complete=True, functions=52, reader=manifest,
                seal_sha256=sha(path))


def stamp(directory, kind, backend, manifest, log=None):
    """Add source/startup pins, then verify raw evidence before sealing it."""
    check_reader(manifest, source_identity())
    pins = {k: manifest[k] for k in ('source_head', 'source_diff_sha256')}
    if kind == 'u10':
        summary = read(directory / 'summary.json')
        for row in summary['receipts']:
            require(row['startup_sha256'] == manifest['startup_sha256'], 'U10 startup identity mismatch')
            row.update(pins)
        write(directory / 'summary.json', summary)
        complete = summary['passed'] is True and not summary['pending'] and not summary['focused']
    else:
        require(log is not None, 'F3 runner log required')
        shutil.copyfile(log, directory / 'runner.log')
        rows = read(directory / 'receipts.json')
        startup_sha = sha(directory / backend / 'reader.native-startup.el')
        require(startup_sha == manifest['startup_sha256'], 'F3 startup identity mismatch')
        for row in rows:
            row.update(pins, startup_sha256=startup_sha)
            matches = [p.parent.name for p in (directory / backend).glob(row['phase'] + '-*/' + row['phase'] + '.json')
                       if read(p)['names'] == row['names']]
            require(len(matches) == 1, 'F3 work-directory accounting mismatch')
            row['work_directory'] = matches[0]
        write(directory / 'receipts.json', rows)
        complete = bool(rows) and all(r['passed'] is True for r in rows)
    seal = dict(schema=1, kind=kind, backend=backend, complete=complete, reader=manifest,
                files_sha256=raw_files(directory))
    write(directory / ('ci-' + kind + '.json'), seal)
    if kind == 'f3':
        verify_f3(directory / 'ci-f3.json', backend, manifest)


def unique(paths, description):
    require(len(paths) == 1, 'Missing or duplicate ' + description)
    return paths[0]


def verify_set(directory, local=None):
    manifest_path = unique(list(directory.rglob('ci-reader.json')), 'reader manifest')
    manifest = read(manifest_path)
    check_reader(manifest, source_identity() if local is None else local)
    # Reader artifact files share the manifest directory after download.
    for relative, digest in manifest['artifacts_sha256'].items():
        require(Path(relative).parent == Path('target'), 'Unsafe reader artifact path')
        p = manifest_path.parent / Path(relative).name
        require(p.is_file() and not p.is_symlink() and sha(p) == digest, 'Reader artifact hash mismatch')
    results = dict(u10=[], f3=[])
    u10_seals = list(directory.rglob('ci-u10.json'))
    f3_seals = list(directory.rglob('ci-f3.json'))
    require(len(u10_seals) == 16 and len(f3_seals) == 2, 'Missing shard or F3 backend artifact')
    for backend in BACKENDS:
        paths = [p for p in u10_seals if read(p)['backend'] == backend]
        merged = verify_u10(paths, backend, manifest)
        verdict = unique(list(directory.rglob('linux-u10-' + backend + '-verdict.json')), 'merged verdict')
        require(read(verdict) == merged, 'Incomplete or altered merged verdict')
        results['u10'].append(merged)
        path = unique([p for p in f3_seals if read(p)['backend'] == backend], 'F3 backend')
        results['f3'].append(verify_f3(path, backend, manifest))
    return results


def import_set(directory, output=ROOT / 'target/evidence-final'):
    results = verify_set(directory)
    output.mkdir(parents=True, exist_ok=True)
    destination = output / 'ci-corpus'
    require(not destination.exists(), 'Existing CI import; refusing overwrite')
    previous = output / 'ci-corpus-previous'
    require(not previous.exists(), 'Existing corpus backup; refusing overwrite')
    # Validate the entire set before writing either qualifying criterion.
    shutil.copytree(directory, destination)
    verify_set(destination)
    evidence = {str(p.relative_to(ROOT)): sha(p) for p in destination.rglob('*') if p.is_file()}
    # Replace the stale local corpus pair only after full verification, keeping
    # its original bytes for review/recovery beside the imported evidence.
    for kind in ('u10', 'f3'):
        for suffix in ('.json', '.log'):
            old = output / (kind + '-both' + suffix)
            if old.exists():
                previous.mkdir(exist_ok=True)
                shutil.copy2(old, previous / old.name)
    for kind in ('u10', 'f3'):
        write(output / (kind + '-both.json'), dict(schema=1, rc=0, complete=True, origin='linux-ci',
              backends=list(BACKENDS), verdicts=results[kind], evidence_sha256=evidence))
        if kind == 'u10':
            log = ''.join('U10-PASS backend=' + b + ' fixtures=230 pending=0 entries=460 platform=linux-x86_64\n' for b in BACKENDS)
        else:
            log = ''.join('F3-CORPUS-PASS backend=' + b + ' functions=52\n' for b in BACKENDS)
            log += 'F3-PROCESS-LIMIT limit=2 observed-max=2\n'
        (output / (kind + '-both.log')).write_text(log)
    print('CI-CORPUS-IMPORT-PASS backends=2 shards=16 fixtures=230 functions=52')


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('directory', nargs='?', type=Path)
    modes = parser.add_mutually_exclusive_group()
    modes.add_argument('--record-reader', action='store_true')
    modes.add_argument('--seal-u10', action='store_true')
    modes.add_argument('--seal-f3', action='store_true')
    modes.add_argument('--merge-u10', action='store_true')
    modes.add_argument('--self-test', action='store_true')
    parser.add_argument('--backend', choices=BACKENDS)
    parser.add_argument('--reader', type=Path, default=ROOT / 'target/ci-reader.json')
    parser.add_argument('--output', type=Path)
    parser.add_argument('--log', type=Path)
    parser.add_argument('--require-complete', action='store_true')
    args = parser.parse_args()
    if args.self_test:
        module('ci_receipt_tests', 'import-ci-corpus-receipts-test.py').self_test(sys.modules[__name__])
    elif args.record_reader:
        write(args.reader, reader_identity())
    elif args.seal_u10 or args.seal_f3:
        require(args.directory is not None and args.backend is not None, 'Directory and backend required')
        stamp(args.directory.resolve(), 'u10' if args.seal_u10 else 'f3', args.backend, read(args.reader), args.log)
    elif args.merge_u10:
        require(args.directory is not None and args.backend is not None and args.output is not None
                and args.require_complete, 'Merge needs directory/backend/output/--require-complete')
        manifest = read(args.reader)
        check_reader(manifest, source_identity())
        paths = list(args.directory.rglob('ci-u10.json'))
        verdict = verify_u10(paths, args.backend, manifest)
        write(args.output, verdict)
        print('U10-PASS backend=' + args.backend + ' fixtures=230 pending=0 entries=460 platform=linux-x86_64')
    else:
        require(args.directory is not None, 'Downloaded artifact directory required')
        import_set(args.directory.resolve())


if __name__ == '__main__':
    try:
        main()
    except (ValueError, KeyError, OSError, TypeError) as error:
        raise SystemExit('CI-CORPUS-REFUSED: ' + str(error))
