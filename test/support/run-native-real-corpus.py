"""Run F3 cohorts; retain identities, cases, exclusions and subprocess timings."""
import argparse
import concurrent.futures
import hashlib
import json
import os
from pathlib import Path
import re
import shutil
import time
import threading
import native_corpus_platform as platform

ROOT = Path(__file__).resolve().parents[2]
FIXTURE = ROOT / 'test/support/native-real-corpus-fixtures.el'
DRIVER = 'test/standalone-native-real-corpus-driver.el'
_process_slots = threading.BoundedSemaphore(2)
_process_lock = threading.Lock()
_active_processes = 0
_peak_processes = 0


def digest(path):
    return hashlib.sha256(Path(path).read_bytes()).hexdigest()


def run_process(command, environment, directory, prefix):
    global _active_processes, _peak_processes
    # One limit covers host work, both backends, compilers, and loaders.
    # Queue wait is outside the unchanged per-reader deadline/timing.
    with _process_slots:
        with _process_lock:
            _active_processes += 1
            _peak_processes = max(_peak_processes, _active_processes)
        try:
            return platform.run_process(command, environment, directory, prefix)
        finally:
            with _process_lock:
                _active_processes -= 1


def phase_verdict(backend, phase, group, rc, seconds, output, errors, expected_cases, deadline=300):
    """Require every selected name exactly once, a complete batch and clean exit."""
    records = re.findall(r'^F3-FUNCTION-PASS backend=' + re.escape(backend) + r' name=(\S+) phase=' + re.escape(phase) + r' cases=([1-9][0-9]*) native-entries=([1-9][0-9]*) validations=0 seconds=', output, re.M)
    passed = [name for name, _, _ in records]
    failures = re.findall(r'^F3-FUNCTION-FAIL .*', output, re.M)
    marker = 'F3-BATCH-DONE backend={} phase={} functions={} failed=0'.format(backend, phase, len(group))
    ok = bool(group) and rc == 0 and seconds < deadline and not errors and not failures and passed == group and output.splitlines().count(marker) == 1 and all(int(cases) == int(entries) == expected_cases[name] for name, cases, entries in records)
    return ok, passed, failures


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--both', action='store_true')
    parser.add_argument('--fresh', action='store_true', help='Require new compilation in empty private cohort caches.')
    parser.add_argument('--seed-cache', type=Path, help='Seed the durable cache from a completed successful qualification with identical selected objects, cases and readers.')
    parser.add_argument('--discover', action='store_true', help='Retain failed candidates for exclusion review; never qualifies F3.')
    parser.add_argument('--reproduce', help='Replay an excluded witness; never qualifies F3.')
    parser.add_argument('--names', nargs='+', help='Focused development selection; never qualifies F3.')
    parser.add_argument('--batch-size', type=int, default=3)
    parser.add_argument('--load-batch-size', type=int, default=8)
    parser.add_argument('--jobs', type=int, default=4, help='Compile workers per backend.')
    parser.add_argument('--load-jobs', type=int, default=2,
                        help='Independent reload workers per backend (1..2).')
    parser.add_argument('static', nargs='?', default='target/nelisp-static')
    parser.add_argument('dynamic', nargs='?', default='target/nelisp-dyn')
    parser.add_argument('--backend', choices=('in-house', 'gccjit', 'template'), default=None)
    platform.add_arguments(parser)
    args = parser.parse_args()
    if args.both and args.backend:
        parser.error('--backend and --both are exclusive')
    if args.fresh and args.seed_cache:
        parser.error('--fresh cannot reuse a seed cache')
    if not 1 <= args.batch_size <= 8 or not 1 <= args.jobs <= 8 or not 1 <= args.load_batch_size <= 16 or not 1 <= args.load_jobs <= 2:
        parser.error('batch size/jobs must be 1..8; load batch size 1..16; load jobs 1..2')
    platform.configure(args, parser)
    if platform.WINDOWS and args.seed_cache:
        parser.error('Windows seed copy is unsupported: host copies do not preserve protected artifact DACLs')
    os.chdir(ROOT)
    (ROOT / 'target').mkdir(exist_ok=True)
    import tempfile
    directory = Path(tempfile.mkdtemp(prefix='f3-corpus-', dir=ROOT / 'target'))
    directory.chmod(0o700)
    # GNU checks pins and current opcode audit in one host batch. JSON transports
    # metadata only: byte-code objects and Lisp inputs stay in the original fixture.
    host = r'''
(require 'json)
(require 'nelisp-native-cache)
(require 'nelisp-bytecode-ir)
(require 'nelisp-bytecode-coverage-audit)
(load (expand-file-name "test/support/native-real-corpus-inputs.el") nil t t)
(load (getenv "F3_FIXTURE") nil t t)
(when (getenv "F3_SEED_FIXTURE")
 (let ((current f3-real-corpus))
  (load (getenv "F3_SEED_FIXTURE") nil t t)
  (dolist (row current)
   (unless (plist-get row :excluded)
    (let ((old (cl-find (plist-get row :name) f3-real-corpus :key (lambda (item) (plist-get item :name)))))
     (unless (and old (not (plist-get old :excluded))
                  (cl-every (lambda (key) (equal (plist-get old key) (plist-get row key)))
                            '(:name :source :source-sha256 :bytecode-sha256 :function :cases)))
      (error "F3 seed corpus mismatch: %s" (plist-get row :name))))))
  (setq f3-real-corpus current)))
(let ((audit (plist-get (nelisp-bytecode-coverage-audit-run) :opcodes)) rows)
 (unless (equal emacs-version "31.1") (error "GNU Emacs 31.1 required"))
 (dolist (row f3-real-corpus)
  (let ((fn (plist-get row :function)) (name (plist-get row :name)) pending)
   (unless (equal (secure-hash 'sha256 (nelisp-native-cache--print fn)) (plist-get row :bytecode-sha256))
    (error "F3 byte-code hash changed: %s" name))
   (unless (cl-some (lambda (test) (eq (car (plist-get test :gnu)) :value)) (plist-get row :cases))
    (error "F3 requires a normal input: %s" name))
   (dolist (test (plist-get row :cases))
    (unless (equal (f3-real-corpus-observe fn (plist-get test :args)) (plist-get test :gnu))
     (error "F3 GNU oracle changed: %s" name)))
   (dolist (instruction (append (plist-get (nelisp-bytecode-ir-decode-result (aref fn 1) (aref fn 2)) :instructions) nil))
    (let ((status (aref audit (aref instruction 1))))
     (unless (and (memq (plist-get status :status) '(native-raw-i64-slice legacy-jit-only frame-represented structurally-decoded))
                  (eq (plist-get (plist-get status :valid-fixture) :frame-status) 'complete))
      (push (aref instruction 1) pending))))
   (when (and pending (not (plist-get row :excluded))) (error "F3 pending opcode: %s %S" name pending))
   (push `((name . ,(symbol-name name)) (source . ,(plist-get row :source))
           (bytecode_sha256 . ,(plist-get row :bytecode-sha256))
           (excluded . ,(or (plist-get row :excluded) json-null))
           (cases . ,(length (plist-get row :cases)))
           (instruction_count . ,(length (plist-get (nelisp-bytecode-ir-decode-result (aref fn 1) (aref fn 2)) :instructions)))) rows)))
 (with-temp-file (getenv "F3_METADATA") (insert (json-encode (vconcat (nreverse rows))))))
'''
    snapshot = directory / 'fixture.el'
    shutil.copyfile(FIXTURE, snapshot); snapshot.chmod(0o400)
    env = os.environ.copy()
    env.update(F3_FIXTURE=str(snapshot), F3_METADATA=str(directory / 'corpus.json'))
    seed = args.seed_cache.resolve(strict=True) if args.seed_cache else None
    if seed:
        env['F3_SEED_FIXTURE'] = str(seed / 'fixture.el')
    command = [env.get('EMACS', 'emacs'), '-Q', '--batch', '-L', 'lisp', '-L', 'src', '-L', 'scripts', '-L', 'tools/ai', '--eval', '(progn ' + host + ')']
    rc, elapsed, output, errors = run_process(command, env, directory, 'host')
    if rc:
        print(errors[-4000:]); raise SystemExit('F3 host verification failed: ' + str(directory))
    generated_env = env.copy()
    generated_env.update(F3_HOST_DIR=str(directory / 'gnu'), F3_FIXTURE_OUT=str(directory / 'reproduced.el'))
    recipe = [env.get('EMACS', 'emacs'), '-Q', '--batch', '-L', 'lisp', '-L', 'src', '-L', 'scripts', '-L', 'tools/ai', '-l', 'test/support/generate-native-real-corpus.el']
    generated_rc, generated_seconds, _, generated_errors = run_process(recipe, generated_env, directory, 'regenerate')
    if generated_rc or digest(directory / 'reproduced.el') != digest(snapshot):
        print(generated_errors[-4000:]); raise SystemExit('F3 source/fixture pins changed: ' + str(directory))
    rows = json.loads((directory / 'corpus.json').read_text())
    if len({row['name'] for row in rows}) != len(rows):
        raise SystemExit('Duplicate corpus names')
    for row in rows:
        if row['excluded']:
            print('F3-EXCLUDE name={} reason={}'.format(row['name'], json.dumps(row['excluded'])), flush=True)
    names = [row['name'] for row in rows if not row['excluded']]
    if args.reproduce:
        if args.reproduce not in {row['name'] for row in rows}: parser.error('Unknown witness')
        names = [args.reproduce]
        env['F3_REPRODUCE'] = '1'
    if args.names:
        if not set(args.names) <= set(names): parser.error('Selection must contain admitted names')
        names = args.names
    if not args.names and not args.reproduce and not args.discover and len(names) != 52:
        raise SystemExit('F3 requires exactly 52 admitted functions')
    identity = {**platform.identity(), 'fixture_sha256': digest(snapshot), 'driver_sha256': digest(ROOT / DRIVER),
                'observer_sha256': digest(ROOT / 'test/support/native-entry-observer.el'),
                'input_protocol_sha256': digest(ROOT / 'test/support/native-real-corpus-inputs.el'),
                'runner_sha256': digest(__file__), 'host_seconds': elapsed, 'regenerate_seconds': generated_seconds}
    seed_receipts = json.loads((seed / 'receipts.json').read_text()) if seed else []
    if seed and (not seed_receipts or not all(r['passed'] and r['rc'] == 0 and r['seconds'] < r.get('process_deadline', 300) for r in seed_receipts)):
        raise SystemExit('Seed must be a completed successful qualification')
    if seed:
        old_rows = json.loads((seed / 'corpus.json').read_text())
        for backend in {r['backend'] for r in seed_receipts}:
            for phase in ('compile', 'load'):
                expected = [r for r in seed_receipts if r['backend'] == backend and r['phase'] == phase]
                for index, receipt in enumerate(expected):
                    work = seed / backend / (phase + '-' + str(index))
                    ok, parsed_names, _ = phase_verdict(backend, phase, receipt['names'], receipt['rc'], receipt['seconds'],
                                             (work / (phase + '.out')).read_text(), (work / (phase + '.err')).read_text(),
                                             {row['name']: row['cases'] for row in old_rows}, receipt.get('process_deadline', 300))
                    if not ok or parsed_names != receipt['passed_names']:
                        raise RuntimeError('Seed transcript does not substantiate receipt')
    backends = [(args.backend or 'in-house', args.static)]
    if args.both: backends.append(('gccjit', args.dynamic))
    all_receipts = []
    start_all = time.monotonic()
    def run_backend(item):
        backend, path = item
        source = (ROOT / path).resolve(strict=True)
        reader_dir = directory / backend; reader_dir.mkdir(mode=0o700)
        binary = reader_dir / ('reader.exe' if platform.WINDOWS else 'reader')
        shutil.copyfile(source, binary); binary.chmod(0o500)
        startup = Path(str(source) + '.native-startup.el')
        if startup.is_file(): shutil.copyfile(startup, Path(str(binary) + '.native-startup.el'))
        cold = Path(str(source) + '.cold')
        if cold.is_file(): shutil.copyfile(cold, Path(str(binary) + '.cold'))
        backend_identity = {**identity, 'binary_sha256': digest(binary),
                            'cold_sha256': digest(cold) if cold.is_file() else None}
        durable = ROOT / 'target/f3-native-real-cache' / (backend + '-' + backend_identity['binary_sha256'][:16] + '-' + str(backend_identity['cold_sha256'])[:16])
        platform.create_cache(durable)
        if durable.is_symlink() or (not platform.WINDOWS and durable.stat().st_mode & 0o077):
            raise RuntimeError('Durable native cache must be a private directory')
        if seed:
            previous = [r for r in seed_receipts if r['backend'] == backend]
            for phase in ('compile', 'load'):
                witnessed = [name for r in previous if r['phase'] == phase for name in r['passed_names']]
                if len(witnessed) != len(set(witnessed)) or not set(names) <= set(witnessed):
                    raise RuntimeError('Seed does not cover selected functions on both phases')
            if not previous or any(r['binary_sha256'] != backend_identity['binary_sha256'] or r['cold_sha256'] != backend_identity['cold_sha256'] or r['fixture_sha256'] != digest(seed / 'fixture.el') for r in previous):
                raise RuntimeError('Seed reader/image/fixture identity mismatch')
            artifacts = {}
            for artifact in (seed / backend / 'load-cache').rglob('*'):
                if artifact.is_symlink(): raise RuntimeError('Symlink in seed cache')
                if not artifact.is_file(): continue
                relative = artifact.relative_to(seed / backend / 'load-cache')
                destination = durable / relative
                destination.parent.mkdir(mode=0o700, parents=True, exist_ok=True)
                artifact_hash = digest(artifact)
                if destination.exists() and digest(destination) != artifact_hash:
                    raise RuntimeError('Seed conflicts with durable cache')
                if not destination.exists(): shutil.copy2(artifact, destination)
                if digest(destination) != artifact_hash: raise RuntimeError('Seed artifact copy changed')
                artifacts[str(relative)] = artifact_hash
            if not artifacts: raise RuntimeError('Seed contains no artifacts')
            (reader_dir / 'seed.json').write_text(json.dumps(dict(evidence=str(seed), receipts_sha256=digest(seed / 'receipts.json'), artifacts=artifacts), indent=2))
        # Keep larger control-flow bodies in their own bounded processes.
        by_name = {row['name']: row for row in rows}
        groups = []
        for name in names:
            if by_name[name]['instruction_count'] > 14:
                groups.append([name])
            elif groups and len(groups[-1]) < args.batch_size and all(by_name[item]['instruction_count'] <= 14 for item in groups[-1]):
                groups[-1].append(name)
            else:
                groups.append([name])
        def phase_run(index, group, phase, cache):
            work = reader_dir / (phase + '-' + str(index)); work.mkdir(mode=0o700)
            current = env.copy()
            current.update(F3_NAMES=' '.join(group), F3_BACKEND=backend, NELISP_NATIVE_CACHE=str(cache))
            current['F3_PHASE'] = phase
            current['F3_FRESH'] = '1' if args.fresh else '0'
            command = platform.reader_command(binary, Path(str(binary) + '.cold'), DRIVER)
            current = platform.reader_environment(current, ('F3_FIXTURE', 'NELISP_NATIVE_CACHE'))
            rc, elapsed, output, errors = run_process(command, current, work, phase)
            ok, passed_names, failed = phase_verdict(backend, phase, group, rc, elapsed, output, errors, {name: by_name[name]['cases'] for name in group}, platform.PROCESS_DEADLINE)
            cache_records = re.findall(r'^F3-CACHE backend=' + re.escape(backend) + r' name=(\S+) status=(hit|miss)$', output, re.M)
            cache_files = re.findall(r'^F3-CACHE-FILE name=(\S+) file=(.+)$', output, re.M)
            if phase == 'compile':
                ok = ok and [name for name, _ in cache_records] == group and [name for name, _ in cache_files] == group
            receipt = dict(backend=backend, names=group, phase=phase, rc=rc, seconds=elapsed,
                           passed=ok, passed_names=passed_names, failures=failed,
                           cache_mode='fresh' if args.fresh else 'durable',
                           cache_files=[str(platform.cache_file(file, cache)) for _, file in cache_files],
                           cache_hits=len(re.findall(r'^F3-CACHE .* status=hit$', output, re.M)),
                           cache_misses=len(re.findall(r'^F3-CACHE .* status=miss$', output, re.M)), **backend_identity)
            (work / (phase + '.json')).write_text(json.dumps(receipt, indent=2))
            print(output + errors[-2000:], end='', flush=True)
            return receipt
        def compile_cohort(item):
            index, group = item
            cache = reader_dir / ('cache-' + str(index)) if args.fresh else durable
            if args.fresh: platform.create_cache(cache)
            return phase_run(index, group, 'compile', cache)
        backend_start = time.monotonic()
        # Public compilation uses its ordinary durable cache unless --fresh.
        # Consolidating
        # only successfully published immutable files amortizes reload setup;
        # every reload is still a new process with compilation forbidden.
        load_cache = reader_dir / 'load-cache'
        if not platform.WINDOWS: platform.create_cache(load_cache)
        load_names = []
        def merge_cache(index):
            cache = reader_dir / ('cache-' + str(index)) if args.fresh else durable
            files = [Path(file) for file in receipts[index]['cache_files']]
            if backend == 'gccjit': files += [Path(str(file) + '.nelh') for file in files]
            for artifact in files:
                artifact.relative_to(cache)
                if artifact.is_symlink(): raise RuntimeError('Symlink in private native cache')
                if not artifact.is_file(): raise RuntimeError('Published artifact missing')
                destination = load_cache / artifact.relative_to(cache)
                destination.parent.mkdir(mode=0o700, parents=True, exist_ok=True)
                if destination.exists():
                    if digest(destination) != digest(artifact):
                        raise RuntimeError('Conflicting independently compiled cache artifacts')
                else:
                    shutil.copy2(artifact, destination)
        receipts = [None] * len(groups)
        next_compile = 0
        pending_load = []
        load_futures = []
        # Reload an ordered prefix as soon as all its artifacts are published.
        # A loader never observes a partial copy or a still-compiling cohort;
        # unrelated later files can be added without changing those snapshots.
        with concurrent.futures.ThreadPoolExecutor(max_workers=args.load_jobs) as loaders:
            def submit_load(group, cache=load_cache):
                load_futures.append(loaders.submit(phase_run, len(load_futures), group, 'load', cache))
            with concurrent.futures.ThreadPoolExecutor(max_workers=args.jobs) as compilers:
                futures = {compilers.submit(compile_cohort, item): item[0] for item in enumerate(groups)}
                for future in concurrent.futures.as_completed(futures):
                    receipts[futures[future]] = future.result()
                    while next_compile < len(groups) and receipts[next_compile] is not None:
                        receipt = receipts[next_compile]
                        if receipt['passed']:
                            load_names.extend(receipt['names'])
                            if platform.WINDOWS:
                                # Reopen the reader-published files in place. Python
                                # copies would discard their protected TokenUser DACLs.
                                cache = reader_dir / ('cache-' + str(next_compile)) if args.fresh else durable
                                submit_load(receipt['names'], cache)
                            else:
                                merge_cache(next_compile)
                                pending_load.extend(receipt['names'])
                        next_compile += 1
                        while len(pending_load) >= args.load_batch_size:
                            submit_load(pending_load[:args.load_batch_size])
                            del pending_load[:args.load_batch_size]
            if pending_load: submit_load(pending_load)
            loaded = [future.result() for future in load_futures]
        compiled_names = [name for receipt in receipts for name in receipt['passed_names']]
        loaded_names = [name for receipt in loaded for name in receipt['passed_names']]
        receipts.extend(loaded)
        success = compiled_names == load_names == loaded_names == names and all(receipt['passed'] for receipt in receipts)
        seconds = time.monotonic() - backend_start
        print('F3-TIMING backend={} total-seconds={:.3f} max-process-seconds={:.3f}'.format(backend, seconds, max(r['seconds'] for r in receipts)), flush=True)
        print('F3-CACHE-TOTAL backend={} hits={} misses={} mode={}'.format(backend, sum(r['cache_hits'] for r in receipts), sum(r['cache_misses'] for r in receipts), 'fresh' if args.fresh else 'durable'), flush=True)
        return backend, success, receipts
    with concurrent.futures.ThreadPoolExecutor(max_workers=len(backends)) as pool:
        results = list(pool.map(run_backend, backends))
    print('F3-PROCESS-LIMIT limit=2 observed-max=' + str(_peak_processes), flush=True)
    all_receipts = [receipt for _, _, receipts in results for receipt in receipts]
    (directory / 'receipts.json').write_text(json.dumps(all_receipts, indent=2))
    success = all(ok for _, ok, _ in results)
    if success and not args.names and not args.reproduce and not args.discover:
        for backend, _, _ in results:
            print('F3-CORPUS-PASS backend={} functions={} digest={}'.format(backend, len(names), identity['fixture_sha256']), flush=True)
    print('F3-EVIDENCE=' + str(directory))
    print('F3-TOTAL seconds={:.3f}'.format(time.monotonic() - start_all))
    return 0 if success and not args.discover else 1


if __name__ == '__main__':
    raise SystemExit(main())
