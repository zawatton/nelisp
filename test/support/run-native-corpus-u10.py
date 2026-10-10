"""Qualify all audited valid slots; durable private caches and bounded processes."""
import argparse
from concurrent.futures import ThreadPoolExecutor, as_completed
import hashlib
import json
import os
from pathlib import Path
import re
import shutil
import subprocess
import tempfile
import threading
import time

ROOT = Path(__file__).resolve().parents[2]
DRIVER = 'test/standalone-native-corpus-u10-driver.el'


def digest(path):
    return hashlib.sha256(Path(path).read_bytes()).hexdigest()


def projected_fixture(destination, directory, hashes, group):
    """Use only generator-authenticated rows, retaining their original bytes."""
    keys = list(map(str, group))
    if not keys or len(set(keys)) != len(keys) or not set(keys) <= set(hashes):
        raise ValueError('Projection group mismatch')
    chunks = [b';;; GNU U10 worker input -*- lexical-binding: t; -*-\n(setq u10-fixtures nil)\n']
    for key in reversed(keys):
        content = (directory / (key + '.el')).read_bytes()
        if hashlib.sha256(content).hexdigest() != hashes[key]:
            raise ValueError('Projection digest mismatch: ' + key)
        chunks.append(content)
    content = b''.join(chunks)
    destination.write_bytes(content)
    return dict(projection_sha256={key: hashes[key] for key in keys},
                worker_fixture_sha256=hashlib.sha256(content).hexdigest())


WARM_HEAVY = frozenset(range(24, 32)) | frozenset(range(41, 51)) | frozenset((97, 114, 138, 140, 142, 144, 145))


def validate_warm_groups(groups, warm, budget):
    if [op for group in groups for op in group] != warm:
        raise ValueError('Warm partition changed the exact fixture sequence')
    for group in groups:
        if not group or len(group) > budget:
            raise ValueError('Warm partition size mismatch')
        if 'protected' in group and group != ['protected']:
            raise ValueError('Protected observations must own their process')
        cost = sum(2 if op in WARM_HEAVY else 1 for op in group)
        if cost > budget and not (budget == 1 and len(group) == 1 and cost == 2):
            raise ValueError('Warm partition weight mismatch')


def warm_groups(warm, budget):
    """Preserve input order; bound frame work and isolate the four-case supplement."""
    groups, group, cost = [], [], 0
    for op in warm:
        weight = 2 if op in WARM_HEAVY else 1
        if op == 'protected':
            if group:
                groups.append(group)
                group, cost = [], 0
            groups.append([op])
        else:
            if group and cost + weight > budget:
                groups.append(group)
                group, cost = [], 0
            group.append(op)
            cost += weight
    if group:
        groups.append(group)
    validate_warm_groups(groups, warm, budget)
    return groups


def verdict(backend, phase, group, rc, seconds, output, errors, refused=()):
    records = re.findall(r'^U10-FIXTURE-PASS backend=(\S+) opcode=(\d+) phase=(\S+) entries=1 gc=1$', output, re.M)
    expected = [(backend, str(op), phase) for op in group if op != 'protected']
    done = f'U10-BATCH-DONE backend={backend} phase={phase} fixtures={len(expected)} entries={len(expected)}'
    stale = re.findall(r'^U10-STALE-PASS control=(\S+)$', output, re.M)
    relocations = re.findall(r'^U10-RELOCATION-REFUSED backend=(\S+) opcode=(\d+) reason=unreadable-live-constant$', output, re.M)
    return (rc == 0 and seconds < 300 and not errors and records == expected
            and output.splitlines().count(done) == 1
            and relocations == [(backend, str(op)) for op in group if op in refused]
            and (192 not in group or stale == ['input', 'abi', 'artifact'])
            and ('protected' not in group or output.splitlines().count('U10-ORDER-PASS cases=4 entries=4') == 1))


def self_test():
    output = ('U10-FIXTURE-PASS backend=in-house opcode=192 phase=load entries=1 gc=1\n'
              'U10-STALE-PASS control=input\nU10-STALE-PASS control=abi\n'
              'U10-STALE-PASS control=artifact\n'
              'U10-BATCH-DONE backend=in-house phase=load fixtures=1 entries=1\n')
    assert verdict('in-house', 'load', [192], 0, 1, output, '')
    controls = [(1, 1, output, ''), (0, 300, output, ''), (0, 1, output, 'error'),
                (0, 1, output.replace('entries=1 gc=1', 'entries=0 gc=1'), ''),
                (0, 1, output.replace('control=abi', 'control=input'), ''),
                (0, 1, output.replace('backend=in-house', 'backend=gccjit'), ''),
                (0, 1, output + output, ''), (0, 1, '', '')]
    assert all(not verdict('in-house', 'load', [192], *args) for args in controls)
    relocation = 'U10-RELOCATION-REFUSED backend=in-house opcode=192 reason=unreadable-live-constant\n'
    assert verdict('in-house', 'load', [192], 0, 1, relocation + output, '', [192])
    assert not verdict('in-house', 'load', [192], 0, 1, output, '', [192])
    assert not verdict('in-house', 'load', [192], 0, 1, relocation * 2 + output, '', [192])
    assert not verdict('in-house', 'load', [192], 0, 1, relocation + output, '')
    with tempfile.TemporaryDirectory() as scratch:
        directory = Path(scratch)
        source = directory / '192.el'
        source.write_bytes(b'(canonical-row)\n')
        hashes = {'192': digest(source)}
        destination = directory / 'worker.el'
        projected_fixture(destination, directory, hashes, [192])
        for key in ('193', 'protected'):
            (directory / (key + '.el')).write_bytes(('(' + key + ')\n').encode())
            hashes[key] = digest(directory / (key + '.el'))
        projected_fixture(destination, directory, hashes, [192, 193, 'protected'])
        assert destination.read_bytes() == b';;; GNU U10 worker input -*- lexical-binding: t; -*-\n(setq u10-fixtures nil)\n(protected)\n(193)\n(canonical-row)\n'
        hashes = {'192': hashes['192']}
        for group in ([], [192, 192], [193], ['protected']):
            try:
                projected_fixture(destination, directory, hashes, group)
            except ValueError:
                continue
            raise AssertionError('Invalid projection group accepted')
        source.write_bytes(b'(changed-row)\n')
        try:
            projected_fixture(destination, directory, hashes, [192])
        except ValueError:
            pass
        else:
            raise AssertionError('Post-readback projection mutation accepted')
    samples = [[], ['protected'], ['protected', 192, 193], [192, 'protected', 193],
               [192, 193, 'protected'], [41, 1, 42, 2, 43, 3, 44],
               list(range(192, 208)) + list(range(24, 32)) + ['protected']]
    for budget in (1, 2, 4, 8, 16):
        for sample in samples:
            warm_groups(sample, budget)
    controls = [([[1, 2, 2]], [1, 2], 8), ([[1]], [1, 2], 8),
                ([[2, 1]], [1, 2], 8), ([[41, 42, 43]], [41, 42, 43], 4),
                ([[1, 'protected']], [1, 'protected'], 8), ([[]], [], 8)]
    for groups, warm, budget in controls:
        try:
            validate_warm_groups(groups, warm, budget)
        except ValueError:
            continue
        raise AssertionError('Broken warm partition accepted')
    print('U10-WARM-PARTITION-VERIFIER-PASS positive=35 negative=6')
    print('U10-VERIFIER-PASS positive=2 negative=11')
    print('U10-PROJECTION-VERIFIER-PASS positive=2 negative=5')


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--both', action='store_true')
    parser.add_argument('--require-complete', action='store_true', help='Ledger: pending slots fail, after running available fixtures.')
    parser.add_argument('--audit-only', action='store_true', help='Strict 230-slot structural audit only; no native claim.')
    parser.add_argument('--self-test', action='store_true')
    parser.add_argument('--opcodes', help='Focused development list, never a U10 qualification PASS.')
    parser.add_argument('--jobs', type=int, default=min(32, max(1, os.cpu_count() or 1)),
                        help='Global native process limit across both backends (1..32; default CPU count, capped at 32).')
    parser.add_argument('--load-batch-size', type=int, default=8,
                        help='Warm weighted work budget (1..16); frames count twice; protected isolated. Cold reloads stay at most four.')
    parser.add_argument('--cold-jobs', type=int, default=6,
                        help='Additional limit on simultaneous cache misses (1..16).')
    parser.add_argument('--cold-batch-size', type=int, default=2,
                        help='Small cache-miss units per process (1..2); complex frames remain isolated.')
    parser.add_argument('static', nargs='?', default='target/nelisp-static')
    parser.add_argument('dynamic', nargs='?', default='target/nelisp-dyn')
    parser.add_argument('--backend', choices=('in-house', 'gccjit', 'template'), default=None)
    args = parser.parse_args()
    if args.both and args.backend:
        parser.error('--backend and --both are exclusive')
    if args.self_test:
        self_test()
        return 0
    if not 1 <= args.jobs <= 32 or not 1 <= args.cold_jobs <= 16 or not 1 <= args.load_batch_size <= 16 or not 1 <= args.cold_batch_size <= 2:
        parser.error('jobs must be 1..32; cold jobs and load batch size 1..16; cold batch size 1..2')
    os.chdir(ROOT)
    (ROOT / 'target').mkdir(exist_ok=True)
    directory = Path(tempfile.mkdtemp(prefix='u10-corpus-', dir=ROOT / 'target'))
    directory.chmod(0o700)
    print('U10-EVIDENCE=' + str(directory), flush=True)
    env = os.environ.copy()
    env.update(U10_FIXTURE=str(directory / 'fixtures.el'), U10_METADATA=str(directory / 'audit.json'),
               U10_PROJECTION_DIR=str(directory / 'projections'))
    host = [env.get('EMACS', 'emacs'), '-Q', '--batch', '-L', 'lisp', '-L', 'src', '-L', 'scripts',
            '-L', 'tools/ai', '-l', 'test/support/generate-native-corpus-u10.el']
    with (directory / 'gnu.out').open('w') as out, (directory / 'gnu.err').open('w') as err:
        result = subprocess.run(['timeout', '-k', '5', '290', *host], env=env, stdout=out, stderr=err)
    if result.returncode:
        print((directory / 'gnu.err').read_text()[-5000:])
        return 1
    metadata = json.loads((directory / 'audit.json').read_text())
    rows = metadata['fixtures']
    projection_hashes = metadata['projections']
    if set(projection_hashes) != {str(row['opcode']) for row in rows} | {'protected'}:
        raise RuntimeError('Projection/fixture accounting mismatch')
    refused = [row['opcode'] for row in rows if row['relocation_refusal']]
    pending = metadata['pending']
    # Exact set equality prevents a lost manifest row from improving the score.
    excluded = set(metadata['audit']['opcodes'][op]['opcode'] for op in range(256)
                   if metadata['audit']['opcodes'][op]['valid-fixture'] is None)
    valid = set(range(256)) - excluded
    if (len(rows) != 230 or len(valid) != 230 or len(excluded) != 26
            or [row['opcode'] for row in rows] != sorted(valid)
            or pending != [row['opcode'] for row in rows if row['status'] != 'complete']
            or not set(pending) <= {48, 49, 50}):
        raise RuntimeError('Audit/fixture accounting mismatch')
    print(f'U10-AUDIT valid=230 complete={230-len(pending)} R/S={len(pending)} pending={pending}', flush=True)
    if args.audit_only:
        return int(bool(pending))
    selected = [row['opcode'] for row in rows if row['status'] == 'complete']
    if args.opcodes:
        focused = list(map(int, args.opcodes.split()))
        if not focused or len(set(focused)) != len(focused) or not set(focused) <= set(selected):
            parser.error('focused opcodes must be unique and audit-complete')
        selected = focused
    sources = [str(path.relative_to(ROOT)) for base in ('lisp', 'src', 'scripts')
               for path in sorted((ROOT / base).glob('*.el'))]
    sources += [DRIVER, 'test/support/native-entry-observer.el', 'test/support/native-corpus-u10-fixtures.el',
                'test/support/native-corpus-u10-projection.el',
                'test/support/native-corpus-u10-state.el',
                'packages/nl-signal/src/nl-signal.el',
                'test/support/generate-native-corpus-u10.el', 'test/support/run-native-corpus-u10.py',
                'test/fixtures/native-bytecode/gnu-31.1-valid-fixtures.json',
                'tools/ai/nelisp-bytecode-coverage-audit.el', 'target/nelisp-artifact-runtime.el.nelc']
    identities = {}
    readers = {}
    caches = {}
    backends = [(args.backend or 'in-house', args.static)] + ([('gccjit', args.dynamic)] if args.both else [])
    for backend, binary in backends:
        binary = Path(binary).resolve()
        work = directory / backend
        work.mkdir(mode=0o700)
        reader = work / 'reader'
        shutil.copy2(binary, reader)
        reader.chmod(0o500)
        cold = Path(str(binary) + '.cold')
        if cold.exists():
            shutil.copy2(cold, Path(str(reader) + '.cold'))
        startup = Path(str(binary) + '.native-startup.el')
        if startup.exists():
            shutil.copy2(startup, Path(str(reader) + '.native-startup.el'))
        identity = dict(binary_sha256=digest(binary), cold_sha256=digest(cold) if cold.exists() else None,
                        startup_sha256=digest(startup) if startup.exists() else None,
                        fixture_sha256=digest(directory / 'fixtures.el'),
                        sources_sha256={source: digest(ROOT / source) for source in sources})
        identities[backend] = identity
        cache_identity = dict(binary_sha256=identity["binary_sha256"], cold_sha256=identity["cold_sha256"],
                              compiler_sha256={name: value for name, value in identity["sources_sha256"].items()
                                               if name.startswith(("lisp/", "src/", "scripts/", "target/"))})
        key = hashlib.sha256(json.dumps(cache_identity, sort_keys=True).encode()).hexdigest()
        cache = ROOT / 'target/u10-native-cache' / backend / key
        cache.mkdir(parents=True, exist_ok=True, mode=0o700)
        caches[backend] = cache
        readers[backend] = reader
    receipts = []
    cold_slots = threading.Semaphore(min(args.cold_jobs, args.jobs))

    def run(backend, phase, group, cold=False):
        label = '-'.join(map(str, group)) + '-' + phase
        work = directory / backend
        worker_fixture = work / (label + '-fixtures.el')
        projection_identity = projected_fixture(worker_fixture, directory / 'projections', projection_hashes, group)
        local = env.copy()
        local.update(U10_BACKEND=backend, U10_PHASE=phase, U10_CASES=' '.join(map(str, group)),
                     U10_CACHE_BASE=str(caches[backend]), U10_FIXTURE=str(worker_fixture),
                     U10_STOP=str(directory / 'STOP'))
        command = [str(readers[backend])]
        cold_image = Path(str(readers[backend]) + '.cold')
        if cold_image.exists():
            command += ['--cold-load-from', str(cold_image)]
        command += ['-L', 'lisp', '-L', 'src', '-L', 'scripts', '-L', 'packages/nl-ffi/src',
                    '-L', 'packages/nl-prelude/src', '--load', DRIVER]
        if cold:
            cold_slots.acquire()
        try:
            load_before = os.getloadavg()
            start = time.monotonic()
            with (work / (label + '.out')).open('w') as out, (work / (label + '.err')).open('w') as err:
                result = subprocess.run(['timeout', '-k', '5', '290', *command], env=local, stdout=out, stderr=err)
        finally:
            if cold:
                cold_slots.release()
        seconds = time.monotonic() - start
        output = (work / (label + '.out')).read_text()
        errors = (work / (label + '.err')).read_text()
        passed = verdict(backend, phase, group, result.returncode, seconds, output, errors, refused)
        receipt = dict(backend=backend, phase=phase, opcodes=group, rc=result.returncode,
                       seconds=seconds, load_before=load_before, load_after=os.getloadavg(),
                       passed=passed, **identities[backend], **projection_identity)
        (work / (label + '.json')).write_text(json.dumps(receipt, indent=2))
        if passed and phase == 'compile':
            for unit, opcodes in units.items():
                if set(opcodes) <= set(group):
                    marker = caches[backend] / unit / 'u10-compiled.json'
                    marker.write_text(json.dumps(dict(fixture_sha256=identities[backend]['fixture_sha256'])))
        print(f'U10-RUN backend={backend} phase={phase} fixtures={group} seconds={seconds:.3f} '
              f'load={load_before[0]:.2f}/{receipt["load_after"][0]:.2f} passed={passed}', flush=True)
        if not passed:
            print((output + errors)[-4000:], flush=True)
        return receipt

    units = {}
    for row in rows:
        if row['opcode'] in selected:
            units.setdefault(row['unit'], []).append(row['opcode'])
    if not pending and not args.opcodes:
        units['protected'] = ['protected']

    def compile_groups(backend):
        """Batch previously qualified hits; keep each cold compiler unit bounded.

        The marker only chooses process size. Every function still goes through
        the unchanged public compile and trust-load APIs and receipt checks.
        """
        cold, warm = [], []
        for unit, group in sorted(units.items(), key=lambda item: 192 not in item[1]):
            marker = caches[backend] / unit / 'u10-compiled.json'
            ready = (marker.exists() and json.loads(marker.read_text()).get('fixture_sha256')
                     == identities[backend]['fixture_sha256'])
            if ready:
                warm.extend(group)
            else:
                cold.append(group)
        bounded, small = [], []
        for group in cold:
            isolated = (len(group) > 1 or any(op in ('protected', 24, 25, 26, 27, 28, 29, 30, 31,
                                                    41, 42, 43, 44, 45, 48, 49, 50,
                                                    97, 114, 138, 140, 142, 144, 145) for op in group))
            if isolated:
                if small:
                    bounded.append(small)
                    small = []
                bounded.append(group)
            else:
                small.extend(group)
                if len(small) == args.cold_batch_size:
                    bounded.append(small)
                    small = []
        if small:
            bounded.append(small)
        return ([(group, False) for group in warm_groups(warm, args.load_batch_size)]
                + [(group, True) for group in bounded])
    started = time.monotonic()
    def compile_group(backend, group, cold):
        # A constant cohort still owns one artifact and runs all eight slots.
        # Publish it with one fixture, then verify the other slots in a fresh
        # process so compiler garbage does not lengthen their forced GC runs.
        if cold and len(group) > 1 and all(isinstance(op, int) and op >= 192 for op in group):
            results = [run(backend, 'compile', group[:1], True)]
            if results[0]['passed']:
                results.append(run(backend, 'compile', group[1:], False))
            if len(results) == 2 and all(row['passed'] for row in results):
                for unit, opcodes in units.items():
                    if set(opcodes) <= set(group):
                        marker = caches[backend] / unit / 'u10-compiled.json'
                        marker.write_text(json.dumps(dict(fixture_sha256=identities[backend]['fixture_sha256'])))
            return results
        return [run(backend, 'compile', group, cold)]

    def qualify_group(backend, group, cold):
        completed = compile_group(backend, group, cold)
        # A fresh process can load this published cohort while other cohorts
        # compile; it still forbids compilation and runs every original check.
        if all(row['passed'] for row in completed) and not (directory / 'STOP').exists():
            load_size = min(4, args.load_batch_size) if cold else args.load_batch_size
            for index in range(0, len(group), load_size):
                receipt = run(backend, 'load', group[index:index+load_size])
                completed.append(receipt)
                if not receipt['passed']:
                    break
        return completed

    queues = [(backend, compile_groups(backend)) for backend, _ in backends]
    # Interleave backend cohorts within the same global process limit.
    scheduled = [(backend, *groups[index])
                 for index in range(max(map(lambda item: len(item[1]), queues)))
                 for backend, groups in queues if index < len(groups)]
    with ThreadPoolExecutor(max_workers=args.jobs) as pool:
        futures = [pool.submit(qualify_group, backend, group, cold)
                   for backend, group, cold in scheduled]
        for future in as_completed(futures):
            if future.cancelled():
                continue
            completed = future.result()
            receipts.extend(completed)
            if not all(receipt['passed'] for receipt in completed):
                (directory / 'STOP').write_text('Qualification blocker; remaining work is unqualified.\n')
                for pending_future in futures:
                    pending_future.cancel()
    good = True
    for backend, _ in backends:
        runs = [row for row in receipts if row['backend'] == backend]
        complete = all(sorted(op for row in runs if row['passed'] and row['phase'] == phase
                              for op in row['opcodes'] if op != 'protected') == sorted(selected)
                       for phase in ('compile', 'load'))
        complete &= all(row['passed'] for row in runs)
        if not pending and not args.opcodes:
            complete &= sum(row['passed'] and 'protected' in row['opcodes'] for row in runs) == 2
        good &= complete
        seconds = sum(row['seconds'] for row in runs)
        maximum = max((row['seconds'] for row in runs), default=0)
        marker = 'FOCUSED' if args.opcodes else ('PASS' if complete else 'FAIL')
        print(f'U10-{marker} backend={backend} fixtures={len(selected)} pending={len(pending)} '
              f'entries={2*len(selected) if complete else "unqualified"} seconds={seconds:.3f} '
              f'max_seconds={maximum:.3f} load={os.getloadavg()[0]:.2f}', flush=True)
    (directory / 'summary.json').write_text(json.dumps(dict(passed=good, pending=pending,
        focused=bool(args.opcodes), wall_seconds=time.monotonic()-started, receipts=receipts), indent=2))
    return int(not good or (args.require_complete and bool(pending)))


if __name__ == '__main__':
    raise SystemExit(main())
