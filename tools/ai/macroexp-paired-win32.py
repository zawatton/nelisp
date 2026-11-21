#!/usr/bin/env python3
"""Windows-native paired ns/step bench runner (optional --single-pass: seconds per first expansion pass) (adjacent to the Linux paired_bench.py).

Each sample is a fresh process pinned to one CPU with SetProcessAffinityMask and
owned by the kill-on-close 8 GiB job from bin/windows-process-job.py.  The runner
takes no heavy lock; the parent wraps it in ulimit/flock.  Per-core idle checking
is NOT implemented, so --allow-busy is mandatory and recorded as idle_check.
"""
import argparse
import ctypes
import datetime
import hashlib
import importlib.util
import json
import math
import os
import re
import statistics
import subprocess
import sys
import time
from ctypes import wintypes

IDLE_CHECK = 'skipped-explicit-allow-busy'
CHECK_RE = re.compile(r'BENCH-CHECK n=(\d+) result=(\S|\S.*?\S) elapsed=(\d+(?:\.\d+)?)')
NAME_RE = re.compile(r'[A-Za-z0-9_.-]+')
JOB_MEMORY = 8 * 1024 ** 3


class BenchError(Exception):
    def __init__(self, message, code=3):
        super().__init__(message)
        self.code = code


def sha256(path):
    digest = hashlib.sha256()
    with open(path, 'rb') as handle:
        for block in iter(lambda: handle.read(1 << 20), b''):
            digest.update(block)
    return digest.hexdigest()


def load_job_class():
    root = os.environ.get('NDTW_NOTES_ROOT') or os.getcwd()
    path = os.path.join(root, 'bin', 'windows-process-job.py')
    if not os.path.isfile(path):
        raise BenchError('job helper not found: %s (set NDTW_NOTES_ROOT)' % path, 2)
    spec = importlib.util.spec_from_file_location('windows_process_job', path)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module.ProcessJob, path


class Affinity:
    def __init__(self):
        api = ctypes.WinDLL('kernel32', use_last_error=True)
        api.GetCurrentProcess.argtypes = []
        api.GetCurrentProcess.restype = wintypes.HANDLE
        api.GetProcessAffinityMask.argtypes = [wintypes.HANDLE, ctypes.POINTER(ctypes.c_size_t),
                                               ctypes.POINTER(ctypes.c_size_t)]
        api.GetProcessAffinityMask.restype = wintypes.BOOL
        api.SetProcessAffinityMask.argtypes = [wintypes.HANDLE, ctypes.c_size_t]
        api.SetProcessAffinityMask.restype = wintypes.BOOL
        self.api = api

    def get(self, handle):
        proc, system = ctypes.c_size_t(0), ctypes.c_size_t(0)
        if not self.api.GetProcessAffinityMask(handle, ctypes.byref(proc), ctypes.byref(system)):
            raise BenchError('GetProcessAffinityMask failed: %s' % ctypes.WinError(ctypes.get_last_error()))
        return proc.value, system.value

    def own_mask(self):
        return self.get(self.api.GetCurrentProcess())[0]

    def pin(self, process, cpu):
        handle = int(process._handle)
        mask = 1 << cpu
        if not self.api.SetProcessAffinityMask(handle, mask):
            raise BenchError('SetProcessAffinityMask cpu=%d failed: %s'
                             % (cpu, ctypes.WinError(ctypes.get_last_error())))
        actual = self.get(handle)[0]
        if actual != mask:
            raise BenchError('affinity readback 0x%x != requested 0x%x' % (actual, mask))
        return actual


def parse_args(argv):
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument('--engine', action='append', default=[], metavar='NAME=PATH',
                    help='repeatable; the first engine is the baseline')
    ap.add_argument('--engine-env', action='append', default=[], metavar='NAME=KEY=VALUE',
                    help='repeatable per-engine env override (negative controls)')
    ap.add_argument('--driver', required=True)
    ap.add_argument('--low', type=int, default=None)
    ap.add_argument('--high', type=int, default=None)
    ap.add_argument('--single-pass', action='store_true',
                    help='one fresh BENCH_N=1 process per engine per rep; reports seconds per first '
                         'expansion pass (driver expansion time only), no two-trip delta')
    ap.add_argument('--cpu', type=int, required=True)
    ap.add_argument('--out', required=True)
    ap.add_argument('--reps', type=int, default=7)
    ap.add_argument('--allow-busy', action='store_true',
                    help='required on Windows: per-core idle validation is not implemented')
    ap.add_argument('--timeout', type=float, default=580.0, help='total wall budget in seconds (<=580)')
    a = ap.parse_args(argv)
    if len(a.engine) < 2:
        ap.error('need at least two --engine NAME=PATH (baseline first)')
    if a.single_pass:
        if a.low is not None or a.high is not None:
            ap.error('--single-pass cannot be combined with --low/--high')
    elif a.low is None or a.high is None or not (0 < a.low < a.high):
        ap.error('need --low and --high with 0 < --low < --high (or use --single-pass)')
    if a.reps < 1:
        ap.error('--reps must be >= 1')
    if not (0 < a.timeout <= 580):
        ap.error('--timeout must be in (0, 580]')
    if not 0 <= a.cpu < 64:
        ap.error('--cpu must be 0..63 (single processor group only)')
    if not a.allow_busy:
        ap.error('--allow-busy is required on Windows (idle_check=%s is recorded)' % IDLE_CHECK)
    engines = []
    for item in a.engine:
        name, sep, path = item.partition('=')
        if not sep or not NAME_RE.fullmatch(name) or not path:
            ap.error('bad --engine %r (want NAME=PATH, NAME in [A-Za-z0-9_.-])' % item)
        if any(e['name'] == name for e in engines):
            ap.error('duplicate engine name %r' % name)
        resolved = os.path.abspath(path)
        if not os.path.isfile(resolved):
            ap.error('engine not a file: %s' % path)
        engines.append({'name': name, 'supplied': path, 'path': resolved, 'env': {}})
    for item in a.engine_env:
        name, sep, rest = item.partition('=')
        key, sep2, value = rest.partition('=')
        target = next((e for e in engines if e['name'] == name), None)
        if not sep or not sep2 or not key or target is None or key == 'BENCH_N' or '\0' in item:
            ap.error('bad --engine-env %r (want NAME=KEY=VALUE, known NAME, KEY != BENCH_N)' % item)
        target['env'][key] = value
    a.driver_resolved = os.path.abspath(a.driver)
    if not os.path.isfile(a.driver_resolved):
        ap.error('driver not a file: %s' % a.driver)
    a.engines = engines
    return a


def command_for(engine, driver):
    if os.path.basename(engine['path']).lower().startswith('emacs'):
        return [engine['path'], '-Q', '--batch', '-l', driver]
    return [engine['path'], '--load', driver]


def parse_check(stdout_path, n):
    with open(stdout_path, 'rb') as handle:
        text = handle.read().decode('utf-8', errors='replace')
    lines = text.splitlines()
    if len(lines) != 1:
        raise BenchError('stdout must hold exactly one line, got %d (%s)' % (len(lines), stdout_path))
    match = CHECK_RE.fullmatch(lines[0])
    if not match or match.group(1) != str(n):
        raise BenchError('bad BENCH-CHECK line for n=%d in %s' % (n, stdout_path))
    elapsed = float(match.group(3))
    if not math.isfinite(elapsed) or elapsed < 0:
        raise BenchError('bad elapsed in %s' % stdout_path)
    return {'n': n, 'result': match.group(2), 'elapsed': elapsed}


def run_sample(ctx, engine, rep, n):
    a = ctx['args']
    base = '%s-r%02d-n%d' % (engine['name'], rep, n)
    out_path = os.path.join(a.out, base + '.stdout')
    err_path = os.path.join(a.out, base + '.stderr')
    cmd = command_for(engine, a.driver_resolved)
    env = dict(os.environ)
    env['BENCH_N'] = str(n)
    env.update(engine['env'])
    env['MEXP_SAMPLE_ID'] = base
    remaining = ctx['deadline'] - time.monotonic()
    if remaining <= 0:
        raise BenchError('total timeout exhausted before %s' % base)
    record = {'engine': engine['name'], 'rep': rep, 'n': n, 'sample_id': base,
              'cmd': cmd, 'stdout': out_path, 'stderr': err_path}
    start = time.perf_counter()
    with open(out_path, 'wb') as so, open(err_path, 'wb') as se:
        proc = subprocess.Popen(cmd, stdin=subprocess.DEVNULL, stdout=so, stderr=se, env=env,
                                shell=False, creationflags=subprocess.CREATE_NO_WINDOW)
        record['pid'] = proc.pid
        try:
            ctx['job'].add(proc)
            record['affinity_mask'] = '0x%x' % ctx['aff'].pin(proc, a.cpu)
            try:
                proc.wait(timeout=remaining)
            except subprocess.TimeoutExpired:
                raise BenchError('sample %s timed out (total budget %.0fs)' % (base, a.timeout))
        except BaseException:
            try:
                proc.kill()
                proc.wait(timeout=10)
            except Exception:
                pass
            raise
    record['wall_s'] = time.perf_counter() - start
    record['exit_code'] = proc.returncode
    if proc.returncode != 0:
        raise BenchError('sample %s exited %s (see %s)' % (base, proc.returncode, err_path))
    record['check'] = parse_check(out_path, n)
    return record


def check_consistency(samples, baseline):
    ref = {}
    for s in samples:
        if s['engine'] == baseline:
            if ref.setdefault(s['n'], s['check']['result']) != s['check']['result']:
                raise BenchError('baseline result changed between reps for n=%d' % s['n'])
    for s in samples:
        want = ref.get(s['n'])
        if want is not None and s['check']['result'] != want:
            raise BenchError('result mismatch n=%d engine=%s rep=%d: %r != baseline %r'
                             % (s['n'], s['engine'], s['rep'], s['check']['result'], want))


def per_rep_ns(samples, engine, a):
    values = []
    for rep in range(a.reps):
        t = {s['n']: s['check']['elapsed'] for s in samples if s['engine'] == engine and s['rep'] == rep}
        delta = t[a.high] - t[a.low]
        if delta <= 0:
            raise BenchError('nonpositive delta engine=%s rep=%d (t_high=%r t_low=%r)'
                             % (engine, rep, t[a.high], t[a.low]), 4)
        values.append(delta / (a.high - a.low) * 1e9)
    return values


def per_rep_seconds(samples, engine, a):
    """Single-pass mode: driver expansion-only seconds of the one BENCH_N=1 sample per rep."""
    values = []
    for rep in range(a.reps):
        t = [s['check']['elapsed'] for s in samples if s['engine'] == engine and s['rep'] == rep]
        if len(t) != 1 or t[0] <= 0:
            raise BenchError('single-pass needs exactly one positive elapsed engine=%s rep=%d (got %r)'
                             % (engine, rep, t), 4)
        values.append(t[0])
    return values


def build_report(a, samples, meta):
    base_name = a.engines[0]['name']
    per_fn = per_rep_seconds if a.single_pass else per_rep_ns
    per = {e['name']: per_fn(samples, e['name'], a) for e in a.engines}
    base_med = statistics.median(per[base_name])
    engines = {}
    for e in a.engines:
        vals = per[e['name']]
        med = statistics.median(vals)
        entry = {('elapsed_s_per_rep' if a.single_pass else 'ns_step_per_rep'): vals, 'median': med, 'min': min(vals), 'max': max(vals),
                 'ratio_to_baseline_median': med / base_med,
                 'ratio_to_baseline_per_pair': [v / b for v, b in zip(vals, per[base_name])],
                 'sha256': sha256(e['path']), 'path': e['supplied'], 'env_overrides': e['env'],
                 'startup_companion_sha256': (sha256(e['path'] + '.native-startup.el')
                                             if os.path.isfile(e['path'] + '.native-startup.el') else None),
                 'cold_image_sha256': (sha256(e['path'] + '.cold')
                                       if os.path.isfile(e['path'] + '.cold') else None)}
        if e['name'] != base_name:
            entry['pairs_faster'] = sum(1 for v, b in zip(vals, per[base_name]) if v < b)
            entry['pairs_total'] = a.reps
        engines[e['name']] = entry
    if a.single_pass:
        return dict(meta, status='ok', mode='single_pass', unit='seconds_per_expansion_pass',
                    baseline=base_name, reps=a.reps, cpu=a.cpu, idle_check=IDLE_CHECK,
                    ratio_definition='engine seconds/pass divided by baseline seconds/pass (>1 = slower)',
                    metric_note='driver elapsed = macroexpand-all only for the FIRST pass of a fresh process '
                                '(BENCH_N=1); reading, evaluation, checks, prefix load and startup are excluded; '
                                'not cold source-load time',
                    driver={'path': a.driver, 'sha256': sha256(a.driver_resolved)}, engines=engines)
    return dict(meta, status='ok', baseline=base_name, low=a.low, high=a.high, reps=a.reps, cpu=a.cpu,
                idle_check=IDLE_CHECK, ratio_definition='engine ns/step divided by baseline ns/step (>1 = slower)',
                driver={'path': a.driver, 'sha256': sha256(a.driver_resolved)}, engines=engines)


def main(argv):
    if sys.platform != 'win32':
        print('paired-bench-win32 runs on Windows only', file=sys.stderr)
        return 2
    a = parse_args(argv)
    os.makedirs(a.out, exist_ok=True)
    for name in ('raw.json', 'report.json'):
        if os.path.exists(os.path.join(a.out, name)):
            print('refusing to overwrite %s in %s' % (name, a.out), file=sys.stderr)
            return 2
    job_class, job_path = load_job_class()
    aff = Affinity()
    own_mask = aff.own_mask()
    if not own_mask & (1 << a.cpu):
        print('cpu %d not permitted by current affinity mask 0x%x' % (a.cpu, own_mask), file=sys.stderr)
        return 2
    meta = {'schema': 1, 'started_utc': datetime.datetime.now(datetime.timezone.utc).isoformat(),
            'idle_check': IDLE_CHECK, 'cpu': a.cpu, 'allowed_affinity_mask': '0x%x' % own_mask,
            'runner': {'path': os.path.abspath(__file__), 'sha256': sha256(__file__)},
            'job_helper': {'path': job_path, 'sha256': sha256(job_path)}}
    samples, error, code, report = [], None, 0, None
    job = job_class()
    try:
        limits = job.limits()
        if not limits['kill_on_job_close'] or limits['job_memory_limit'] != JOB_MEMORY:
            raise BenchError('job limits not as required: %r' % (limits,), 2)
        meta['job_limits'] = limits
        ctx = {'args': a, 'job': job, 'aff': aff, 'deadline': time.monotonic() + a.timeout}
        base_name = a.engines[0]['name']
        for rep in range(a.reps):
            order = a.engines if rep % 2 == 0 else a.engines[::-1]
            for engine in order:
                for n in ((1,) if a.single_pass else (a.low, a.high)):
                    samples.append(run_sample(ctx, engine, rep, n))
                    check_consistency(samples, base_name)
            for engine in ([] if a.single_pass else a.engines):  # fail fast on nonpositive deltas
                t = {s['n']: s['check']['elapsed'] for s in samples
                     if s['engine'] == engine['name'] and s['rep'] == rep}
                if t[a.high] - t[a.low] <= 0:
                    raise BenchError('nonpositive delta engine=%s rep=%d (t_high=%r t_low=%r)'
                                     % (engine['name'], rep, t[a.high], t[a.low]), 4)
        report = build_report(a, samples, meta)
    except BenchError as exc:
        error, code = str(exc), exc.code
    except Exception as exc:  # noqa: BLE001 - recorded, then reported as failure
        error, code = '%s: %s' % (type(exc).__name__, exc), 5
    finally:
        job.close()
    raw = dict(meta, status='ok' if error is None else 'failed', error=error, samples=samples,
               mode='single_pass' if a.single_pass else 'two_trip',
               engines=[{k: e[k] for k in ('name', 'supplied', 'path', 'env')} for e in a.engines])
    with open(os.path.join(a.out, 'raw.json'), 'w', encoding='utf-8') as handle:
        json.dump(raw, handle, indent=1, sort_keys=True)
    if error is not None:
        print('FAILED (exit %d): %s' % (code, error), file=sys.stderr)
        print('raw: %s' % os.path.join(a.out, 'raw.json'), file=sys.stderr)
        return code
    with open(os.path.join(a.out, 'report.json'), 'w', encoding='utf-8') as handle:
        json.dump(report, handle, indent=1, sort_keys=True)
    if a.single_pass:
        print('mode=single_pass unit=seconds_per_expansion_pass idle_check=%s cpu=%d reps=%d'
              % (IDLE_CHECK, a.cpu, a.reps))
    else:
        print('idle_check=%s cpu=%d low=%d high=%d reps=%d' % (IDLE_CHECK, a.cpu, a.low, a.high, a.reps))
    for name, entry in report['engines'].items():
        extra = ' faster=%d/%d' % (entry['pairs_faster'], entry['pairs_total']) if 'pairs_faster' in entry else ' (baseline)'
        if a.single_pass:
            print('%s median=%.6f min=%.6f max=%.6f s/pass ratio=%.3f%s' % (
                name, entry['median'], entry['min'], entry['max'], entry['ratio_to_baseline_median'], extra))
        else:
            print('%s median=%.1f min=%.1f max=%.1f ns/step ratio=%.3f%s' % (
                name, entry['median'], entry['min'], entry['max'], entry['ratio_to_baseline_median'], extra))
    print('report: %s' % os.path.join(a.out, 'report.json'))
    return 0


if __name__ == '__main__':
    sys.exit(main(sys.argv[1:]))
