#!/usr/bin/env bash
# gnu-tests.sh --- run GNU Emacs 31.1's own test files on the NeLisp standalone.
#
#   tools/ai/gnu-tests.sh [--fresh] FILE...      e.g. src/data-tests lisp/subr-tests
#   tools/ai/gnu-tests.sh [--fresh] --stage S2|S3
#   tools/ai/gnu-tests.sh [--fresh] --all
#   tools/ai/gnu-tests.sh --self-test
#   tools/ai/gnu-tests.sh --freshness-check [--self-test]
#   tools/ai/gnu-tests.sh --check-exclusions
#   tools/ai/gnu-tests.sh --force ...            ignore recorded results
#
# Environment (tools/ai/gnu-language-conformance-progress.org):
#   NELISP_BIN       standalone binary (default target/nelisp of this tree)
#   NELISP_GNU_ELIB  nelisp-emacs-lib tree holding build/nemacs-bootstrap.el
#                    (default target/gnu-tests/elib, a snapshot of one)
#
# The runner dumps one heap image (bundle + GNU ert.el + the support shim,
# tools/ai/gnu-tests-support.el) keyed by the digests of everything that
# goes into it, then runs every test file in its own process started from
# that image.  A process that aborts or times out part way is restarted
# with the finished tests skipped; the test it was running is recorded as
# unexpected (crash or timeout).  One result per (binary, image, test file,
# exclusions) digest is kept under target/gnu-tests/results, so a repeated
# run answers from the record when nothing changed.
#
# A file passes iff it loads without error and reports 0 unexpected
# results.  The binary must be newer than every runtime source of this
# tree (lisp/ src/ scripts/ *.el); otherwise the run is refused (exit 3).
# Exit status: 0 all files pass, 1 some file has unexpected results,
# 2 usage or environment error, 3 stale binary refused.
set -u
here=$(cd "$(dirname "$0")/../.." && pwd) || exit 2
exec python3 - "$here" "$@" <<'PY'
import hashlib, json, os, re, resource, shutil, signal, subprocess, sys, tempfile, time
from pathlib import Path

root = Path(sys.argv[1])
args = sys.argv[2:]
out = root / 'target/gnu-tests'
vendor = root / 'vendor/emacs-test'
tools = root / 'tools/ai'
support = tools / 'gnu-tests-support.el'
runner = tools / 'gnu-tests-run.el'
excl_file = tools / 'gnu-tests-exclusions.txt'
CATEGORIES = {'display', 'tty', 'subprocess', 'network', 'native-comp', 'gnu-internal'}
SUITE_TESTS = 989
FILE_TIMEOUT = int(os.environ.get('GNU_TESTS_FILE_TIMEOUT', '300'))
MAX_RESTARTS = 60


def die(msg, code=2):
    print('gnu-tests: ' + msg, file=sys.stderr)
    sys.exit(code)


def sha(path):
    h = hashlib.sha256()
    with open(path, 'rb') as f:
        for chunk in iter(lambda: f.read(1 << 20), b''):
            h.update(chunk)
    return h.hexdigest()


def all_files():
    names = []
    for line in (vendor / 'SHA256SUMS').read_text().splitlines():
        p = line.split()[-1]
        if p.endswith('-tests.el'):
            names.append(p[len('test/'):-len('.el')])
    return sorted(names)


def stage_files(stage):
    prefix = {'S2': 'src/', 'S3': 'lisp/'}.get(stage)
    if prefix is None:
        die('unknown stage ' + stage)
    return [n for n in all_files() if n.startswith(prefix)]


def read_exclusions():
    """Return {file: {test: (category, reason)}} and a list of problems."""
    table, problems = {}, []
    if not excl_file.exists():
        return table, problems
    for no, line in enumerate(excl_file.read_text().splitlines(), 1):
        line = line.strip()
        if not line or line.startswith('#'):
            continue
        parts = line.split(None, 2)
        if len(parts) < 3 or not parts[2].strip():
            problems.append(f'line {no}: needs FILE:TEST CATEGORY REASON')
            continue
        ref, cat, reason = parts
        if ':' not in ref:
            problems.append(f'line {no}: {ref} is not FILE:TEST')
            continue
        fname, test = ref.split(':', 1)
        if cat not in CATEGORIES:
            problems.append(f'line {no}: category {cat} not in {sorted(CATEGORIES)}')
        src = vendor / 'test' / (fname + '.el')
        if not src.exists():
            problems.append(f'line {no}: no test file {fname}')
        elif not re.search(r'\(ert-deftest\s+' + re.escape(test) + r'\s', src.read_text(errors='replace')):
            problems.append(f'line {no}: {fname} defines no test {test}')
        table.setdefault(fname, {})[test] = (cat, reason)
    return table, problems


def check_exclusions():
    table, problems = read_exclusions()
    count = sum(len(v) for v in table.values())
    limit = SUITE_TESTS * 5 // 100
    if count > limit:
        problems.append(f'{count} exclusions exceed 5% of {SUITE_TESTS} ({limit})')
    for p in problems:
        print('gnu-tests: exclusions: ' + p, file=sys.stderr)
    print(f'gnu-tests: {count} exclusions (limit {limit}), {len(problems)} problems')
    return 0 if not problems else 1


def binary():
    b = Path(os.environ.get('NELISP_BIN') or root / 'target/nelisp')
    if not b.is_file() or not os.access(b, os.X_OK):
        die(f'no executable binary at {b} (build with make standalone-reader)')
    return b


def elib():
    e = Path(os.environ.get('NELISP_GNU_ELIB') or out / 'elib')
    if not (e / 'build/nemacs-bootstrap.el').is_file():
        die(f'no library bundle at {e}/build/nemacs-bootstrap.el (set NELISP_GNU_ELIB)')
    return e.resolve()


def runtime_sources():
    r = subprocess.run(['git', '-C', str(root), 'ls-files', '-z', '--', 'lisp/*.el', 'src/*.el', 'scripts/*.el'],
                       stdout=subprocess.PIPE, check=False)
    return [root / p for p in r.stdout.decode().split('\0') if p]


def check_fresh(b):
    newest, newest_path = 0, None
    for p in runtime_sources():
        try:
            m = p.stat().st_mtime
        except FileNotFoundError:
            continue
        if m > newest:
            newest, newest_path = m, p
    if b.stat().st_mtime < newest:
        die(f'binary {b} is older than {newest_path.relative_to(root)}; rebuild (make standalone-reader)', 3)


child = None


def kill_child(*_):
    if child is not None and child.poll() is None:
        try:
            os.killpg(child.pid, signal.SIGKILL)
        except ProcessLookupError:
            pass


def cancel(signum, _frame):
    kill_child()
    sys.exit(128 + signum)


for s in (signal.SIGINT, signal.SIGTERM, signal.SIGHUP):
    signal.signal(s, cancel)


def run(argv, env, cwd, timeout, stdout, stderr):
    """Run ARGV; return (returncode or 'timeout')."""
    global child
    resource.setrlimit(resource.RLIMIT_STACK, (resource.RLIM_INFINITY, resource.RLIM_INFINITY))
    with open(stdout, 'wb') as o, open(stderr, 'wb') as e:
        child = subprocess.Popen(argv, cwd=cwd, env=env, stdout=o, stderr=e,
                                 stdin=subprocess.DEVNULL, start_new_session=True)
        try:
            rc = child.wait(timeout=timeout)
        except subprocess.TimeoutExpired:
            rc = 'timeout'
        finally:
            kill_child()
            child.wait()
            child = None
    return rc


def base_env(b, e):
    env = os.environ.copy()
    env.update(NELISP_HOME=str(root), XDG_CACHE_HOME=str(out / 'cache'),
               NEMACS_COLD_CACHE_ROOT=str(out / 'cache'), NEMACS_DISABLE_COLD_CACHE='1',
               GNU_TESTS_ELIB=str(e))
    return env


def image(b, e):
    """Return the heap image path for (binary, bundle, ert, support), building it if needed."""
    parts = [sha(b), sha(e / 'build/nemacs-bootstrap.el'), sha(support), sha(runner),
             sha(vendor / 'lisp/emacs-lisp/ert.el'), sha(vendor / 'lisp/emacs-lisp/ert-x.el')]
    key = hashlib.sha256('\n'.join(parts).encode()).hexdigest()
    d = out / 'image'
    d.mkdir(parents=True, exist_ok=True)
    path = d / (key + '.flat')
    if path.is_file() and path.stat().st_size > 0:
        return key, path
    tmp = d / ('.building-' + key)
    marker = 'GT-IMAGE-OK'
    libroot = e / 'build/doc211-bootstrap-root/src'
    # Record the runtime's own definitions the bundle overrides; the support
    # file puts them back (see gnu-tests-support.el).
    saved = ' '.join("(cons (quote %s) (symbol-function (quote %s)))" % (n, n)
                     for n in ('cl-loop', 'nelisp-cl-macros--loop-build'))
    form = ('(progn (setq gnu-tests--runtime-definitions (list ' + saved + ')) '
            '(load %s nil t) (load %s nil t) '
            '(if (fboundp (quote nemacs-main--prepare-image-heap)) (nemacs-main--prepare-image-heap) (garbage-collect)) '
            '(unless (> (nelisp--arena-dump-image-stream %s) 0) (error "gnu-tests: heap dump failed")) '
            '(princ "%s\\n") t)') % (json.dumps(str(e / 'build/nemacs-bootstrap.el')),
                                     json.dumps(str(support)), json.dumps(str(tmp)), marker)
    argv = [str(b)] + (['-L', str(libroot)] if libroot.is_dir() else []) + ['--eval', form]
    print(f'gnu-tests: building image {key[:12]} (bundle + ert)', file=sys.stderr)
    t0 = time.monotonic()
    rc = run(argv, base_env(b, e), str(e), int(os.environ.get('GNU_TESTS_IMAGE_TIMEOUT', '600')),
             d / 'build.out', d / 'build.err')
    if rc != 0 or marker not in (d / 'build.out').read_text(errors='replace') or not tmp.is_file():
        tmp.unlink(missing_ok=True)
        die(f'image build failed (rc={rc}); see {d}/build.err')
    for old in d.glob('*.flat'):
        old.unlink()
    tmp.rename(path)
    print(f'gnu-tests: image built in {time.monotonic() - t0:.0f}s', file=sys.stderr)
    return key, path


def parse(stdout):
    loaded, load_error, done, started, results = None, None, False, None, []
    for line in Path(stdout).read_text(errors='replace').splitlines():
        if not line.startswith('GT|'):
            continue
        f = line.split('|', 3)
        if f[1] == 'LOADED':
            loaded = int(f[2])
        elif f[1] == 'LOAD-ERROR':
            load_error = line[len('GT|LOAD-ERROR|'):]
        elif f[1] == 'START':
            started = line[len('GT|START|'):]
        elif f[1] == 'R':
            results.append((f[2], f[3]))
            started = None
        elif f[1] == 'DONE':
            done = True
    return loaded, load_error, done, started, results


def run_file(name, b, e, img_key, img, excl, force):
    src = vendor / 'test' / (name + '.el')
    if not src.is_file():
        die(f'no test file {src}')
    excluded = sorted(excl.get(name, {}))
    key = hashlib.sha256('\n'.join([sha(b), img_key, sha(src), sha(runner)] + excluded).encode()).hexdigest()
    res_dir = out / 'results'
    res_dir.mkdir(parents=True, exist_ok=True)
    res_path = res_dir / (name.replace('/', '__') + '.json')
    if not force and res_path.is_file():
        try:
            rec = json.loads(res_path.read_text())
            if rec.get('key') == key:
                rec['cached'] = True
                return rec
        except ValueError:
            pass
    work = out / 'work' / name.replace('/', '__')
    shutil.rmtree(work, ignore_errors=True)
    work.mkdir(parents=True)
    env = base_env(b, e)
    env['GNU_TESTS_FILE'] = str(src)
    skip = list(excluded)
    statuses, load_error, loaded, restarts = {}, None, None, 0
    t0 = time.monotonic()
    while True:
        skip_file = work / 'skip.txt'
        skip_file.write_text(''.join(s + '\n' for s in skip))
        env['GNU_TESTS_SKIP'] = str(skip_file)
        so, se = work / f'run{restarts}.out', work / f'run{restarts}.err'
        rc = run([str(b), '--cold-load-from', str(img), '--eval', '(gnu-tests-run)'],
                 env, str(work), FILE_TIMEOUT, so, se)
        n, lerr, done, started, results = parse(so)
        if loaded is None:
            loaded = n
        if lerr and load_error is None:
            load_error = lerr
        for status, test in results:
            statuses[test] = status
            skip.append(test)
        if done and rc == 0:
            break
        if n is None:
            load_error = load_error or f'process ended during load (rc={rc}); see {se}'
            break
        if started is None:
            # Died between tests, or after the last one without DONE.
            if rc == 'timeout' or len(statuses) + len(excluded) >= (loaded or 0):
                if not done:
                    statuses.setdefault('<runner>', f'unexpected-{"timeout" if rc == "timeout" else "abort"}')
                break
            statuses.setdefault('<runner>', f'unexpected-abort-between-tests rc={rc}')
            break
        statuses[started] = 'unexpected-timeout' if rc == 'timeout' else f'unexpected-crash rc={rc}'
        skip.append(started)
        restarts += 1
        if restarts > MAX_RESTARTS:
            statuses['<runner>'] = 'unexpected-too-many-restarts'
            break
    unexpected = sorted(t for t, s in statuses.items() if s.startswith('unexpected'))
    rec = {
        'file': name, 'key': key, 'seconds': round(time.monotonic() - t0, 1),
        'defined': loaded, 'load_error': load_error, 'restarts': restarts,
        'excluded': excluded,
        'counts': {s: sum(1 for v in statuses.values() if v == s) for s in sorted(set(statuses.values()))},
        'unexpected': {t: statuses[t] for t in unexpected},
        'statuses': statuses,
    }
    rec['pass'] = load_error is None and not unexpected
    res_path.write_text(json.dumps(rec, indent=1, sort_keys=True) + '\n')
    rec['cached'] = False
    return rec


def report(recs):
    summary = {r['file']: {'pass': r['pass'], 'load_error': r['load_error'],
                           'unexpected': sorted(r['unexpected'])} for r in recs}
    (out / 'summary.json').write_text(json.dumps(summary, indent=1, sort_keys=True) + '\n')
    for r in recs:
        c = r['counts']
        npass = c.get('passed', 0) + c.get('failed-expected', 0)
        line = (f"gnu-tests: {r['file']}: {'PASS' if r['pass'] else 'FAIL'} "
                f"defined={r['defined']} ok={npass} skipped={c.get('skipped', 0)} "
                f"unexpected={len(r['unexpected'])} excluded={len(r['excluded'])}"
                f"{' (recorded)' if r['cached'] else ''}")
        if r['load_error']:
            line += ' load-error=' + r['load_error'][:160]
        print(line)
    print(f'gnu-tests: summary {out / "summary.json"}')
    return 0 if all(r['pass'] for r in recs) else 1


def run_files(names, force):
    b = binary()
    check_fresh(b)
    _, problems = read_exclusions()
    if problems:
        die('exclusion list invalid; run --check-exclusions')
    excl, _ = read_exclusions()
    e = elib()
    key, img = image(b, e)
    recs = [run_file(n, b, e, key, img, excl, force) for n in names]
    return report(recs)


SELFTEST = tools / 'gnu-tests-selftest.el'
SELFTEST_FAIL = tools / 'gnu-tests-selftest-fail.el'


def self_test():
    b = binary()
    check_fresh(b)
    e = elib()
    key, img = image(b, e)
    ok = True
    # Fixed fixture: 1 pass, 1 expected failure, 1 skip, 1 should-error pass.
    rec = run_selftest_file(SELFTEST, b, e, key, img)
    want = {'passed': 2, 'failed-expected': 1, 'skipped': 1}
    if rec['counts'] != want or rec['load_error'] or rec['defined'] != 4:
        print(f"gnu-tests: self-test: counts {rec['counts']} defined={rec['defined']} "
              f"load_error={rec['load_error']}, want {want} and 4 defined", file=sys.stderr)
        ok = False
    st = rec['statuses']
    for test, status in [('gnu-tests-selftest-pass', 'passed'),
                         ('gnu-tests-selftest-should-error', 'passed'),
                         ('gnu-tests-selftest-expected-failure', 'failed-expected'),
                         ('gnu-tests-selftest-skip', 'skipped')]:
        if st.get(test) != status:
            print(f'gnu-tests: self-test: {test} is {st.get(test)}, want {status}', file=sys.stderr)
            ok = False
    # A failing should prints its form, and the JSON summary names the test.
    rec = run_selftest_file(SELFTEST_FAIL, b, e, key, img)
    err = ''.join(p.read_text(errors='replace') for p in sorted(Path(rec['work']).glob('run*.err')))
    if '(should (equal (gnu-tests-selftest--two) 3))' not in err.replace('\n', ' '):
        print('gnu-tests: self-test: the failing should form is not in the ERT report', file=sys.stderr)
        ok = False
    summary = json.loads((out / 'summary.json').read_text())
    names = summary.get('selftest/gnu-tests-selftest-fail', {}).get('unexpected', [])
    if names != ['gnu-tests-selftest-fails']:
        print(f'gnu-tests: self-test: summary names {names}, want [gnu-tests-selftest-fails]', file=sys.stderr)
        ok = False
    print('gnu-tests: self-test ' + ('PASS' if ok else 'FAIL'))
    return 0 if ok else 1


def run_selftest_file(path, b, e, key, img):
    # Self-test fixtures live in tools/ai, not in the vendored suite.
    global vendor
    name = 'selftest/' + path.stem
    tmp = Path(tempfile.mkdtemp(prefix='gnu-tests-selftest-', dir=out))
    (tmp / 'test/selftest').mkdir(parents=True)
    shutil.copy(path, tmp / 'test/selftest' / path.name)
    saved = vendor
    vendor = tmp
    try:
        rec = run_file(name, b, e, key, img, {}, True)
    finally:
        vendor = saved
    rec['work'] = str(out / 'work' / name.replace('/', '__'))
    summary_path = out / 'summary.json'
    summary = json.loads(summary_path.read_text()) if summary_path.exists() else {}
    summary[name] = {'pass': rec['pass'], 'load_error': rec['load_error'],
                     'unexpected': sorted(rec['unexpected'])}
    summary_path.write_text(json.dumps(summary, indent=1, sort_keys=True) + '\n')
    shutil.rmtree(tmp, ignore_errors=True)
    return rec


def freshness_check():
    b = binary()
    check_fresh(b)
    tmpd = Path(tempfile.mkdtemp(prefix='gnu-tests-fresh-', dir=out))
    try:
        stale = tmpd / 'nelisp'
        shutil.copy2(b, stale)
        os.utime(stale, (946684800, 946684800))
        env = os.environ.copy()
        env['NELISP_BIN'] = str(stale)
        r = subprocess.run(['bash', str(tools / 'gnu-tests.sh'), 'src/alloc-tests'], env=env,
                           stdout=subprocess.PIPE, stderr=subprocess.PIPE, timeout=120)
    finally:
        shutil.rmtree(tmpd, ignore_errors=True)
    if r.returncode != 3 or b'older than' not in r.stderr:
        print(f'gnu-tests: freshness: a stale binary was not refused (rc={r.returncode})', file=sys.stderr)
        return 1
    print('gnu-tests: freshness: stale binary refused')
    return 0


def main():
    out.mkdir(parents=True, exist_ok=True)
    force = '--force' in args
    rest = [a for a in args if a not in ('--force', '--fresh')]
    if not rest:
        die('usage: gnu-tests.sh [--fresh] FILE...|--stage S2|S3|--all|--self-test|--freshness-check|--check-exclusions')
    if rest[0] == '--check-exclusions':
        return check_exclusions()
    if rest[0] == '--freshness-check':
        rc = freshness_check()
        if rc == 0 and '--self-test' in rest[1:]:
            rc = self_test()
        return rc
    if rest[0] == '--self-test':
        return self_test()
    if rest[0] == '--all':
        return run_files(all_files(), force)
    if rest[0] == '--stage':
        if len(rest) != 2:
            die('--stage takes S2 or S3')
        return run_files(stage_files(rest[1]), force)
    known = set(all_files())
    for n in rest:
        if n not in known:
            die(f'unknown test file {n}; expected one of the 35 (e.g. src/data-tests)')
    return run_files(rest, force)


sys.exit(main())
PY
