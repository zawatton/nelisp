#!/usr/bin/env python3
"""Strict report adapter: 5 complete pairs of a failed paired campaign + 2 fresh tail pairs.

Reads the failed prefix raw.json, the tail campaign dir and an independently trusted identity
report, verifies every identity/ordering/stdout invariant, then builds a derived report.json plus
manifest.json with the ORIGINAL runner's build_report.  Source artifacts are never modified and no
raw.json is fabricated.  Stdlib only; performs no benchmark runs.
"""
import argparse
import copy
import hashlib
import importlib.util
import json
import math
import os
import re
import sys

PREFIX_PAIRS, TAIL_PAIRS = 5, 2
HEX64 = re.compile(r'[0-9a-f]{64}')


class AdapterError(Exception):
    pass


def need(cond, msg):
    if not cond:
        raise AdapterError(msg)


def norm(p):
    return os.path.normcase(os.path.abspath(p))


def file_sha(path):
    h = hashlib.sha256()
    with open(path, 'rb') as f:
        for block in iter(lambda: f.read(1 << 20), b''):
            h.update(block)
    return h.hexdigest()


def load_json(path):
    with open(path, encoding='utf-8') as f:
        return json.load(f)


def load_runner(path):
    spec = importlib.util.spec_from_file_location('macroexp_paired_win32_adapter', path)
    mod = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(mod)  # module guards main() behind __name__ == '__main__'
    for name in ('check_consistency', 'build_report', 'parse_check', 'per_rep_seconds', 'BenchError'):
        need(hasattr(mod, name), 'runner lacks %s' % name)
    return mod


def expected_order(first_rep, count, engines_first):
    """Alternation used by the runner: even local rep = given order, odd = reversed."""
    seq = []
    for local in range(count):
        order = engines_first if local % 2 == 0 else engines_first[::-1]
        seq += [(first_rep + local, name) for name in order]
    return seq


def engine_set(engines):
    return {(e['name'], norm(e['path']), tuple(sorted(e['env'].items()))) for e in engines}


def check_meta(label, rec, ident, runner_sha, current):
    """Metadata every campaign record must share with the trusted identity report."""
    for key in ('cpu', 'allowed_affinity_mask', 'idle_check', 'mode', 'schema', 'job_limits'):
        need(rec.get(key) == ident.get(key), '%s %s differs from identity report' % (label, key))
    need(rec['job_limits'].get('job_memory_limit') == 8 * 1024 ** 3 and rec['job_limits'].get('kill_on_job_close') is True,
         '%s job limits not 8GiB/kill-on-close' % label)
    need(rec['job_helper']['sha256'] == ident['job_helper']['sha256'] == current['job_helper'],
         '%s job helper hash differs' % label)
    need(norm(rec['job_helper']['path']) == norm(ident['job_helper']['path']), '%s job helper path differs' % label)
    need(rec['runner']['sha256'] == ident['runner']['sha256'] == runner_sha, '%s runner hash differs' % label)


def check_sample(runner, s, dirpath, engines, cpu, driver):
    need(s.get('engine') in engines and s.get('n') == 1 and isinstance(s.get('rep'), int), 'bad sample header %r' % s.get('sample_id'))
    need(s['sample_id'] == '%s-r%02d-n1' % (s['engine'], s['rep']), 'sample id/rep mismatch %s' % s['sample_id'])
    need(s.get('exit_code') == 0 and isinstance(s.get('wall_s'), float) and s['wall_s'] > 0, 'bad exit/wall %s' % s['sample_id'])
    need(s.get('affinity_mask') == '0x%x' % (1 << cpu), 'bad affinity %s' % s['sample_id'])
    need(len(s['cmd']) == 3 and norm(s['cmd'][0]) == norm(engines[s['engine']]['path']) and s['cmd'][1] == '--load'
         and norm(s['cmd'][2]) == norm(driver), 'cmd does not use identity engine/driver: %s' % s['sample_id'])
    stdout = os.path.join(dirpath, os.path.basename(s['stdout'].replace('\\', '/')))
    need(os.path.isfile(stdout), 'stdout artifact missing: %s' % s['sample_id'])
    parsed = runner.parse_check(stdout, 1)
    c = s['check']
    need(c == parsed and c['n'] == 1, 'check differs from stdout for %s' % s['sample_id'])
    need(isinstance(c['elapsed'], float) and math.isfinite(c['elapsed']) and c['elapsed'] > 0, 'nonpositive elapsed %s' % s['sample_id'])
    need(isinstance(c['result'], str) and HEX64.fullmatch(c['result']), 'malformed result %s' % s['sample_id'])
    return file_sha(stdout)


def validate_selected(runner, selected, base, cand):
    need(len(selected) == 2 * (PREFIX_PAIRS + TAIL_PAIRS), 'need exactly 14 selected samples, got %d' % len(selected))
    want = expected_order(0, PREFIX_PAIRS + TAIL_PAIRS, [base, cand])
    need([(s['rep'], s['engine']) for s in selected] == want, 'selected samples not the exact alternating 7-pair order')
    need(len({(s['source'], s['source_sample_id']) for s in selected}) == 14, 'duplicate source sample')
    need(len({s['check']['result'] for s in selected}) == 1, 'selected samples disagree on the conditional result')
    runner.check_consistency(selected, base)


def verify(runner, runner_sha, prefix, prefix_dir, tail_raw, tail_rep, tail_dir, ident, mutate_selected=None):
    """Return (report, manifest_body). Raises AdapterError/BenchError on any unverifiable input."""
    need(ident.get('status') == 'ok' and ident.get('mode') == 'single_pass' and ident.get('reps') == 7
         and ident.get('baseline') == 'base' and set(ident['engines']) == {'base', 'candidate'}, 'identity report is not a 7-pair single-pass base/candidate campaign')
    cpu, base, cand = ident['cpu'], 'base', 'candidate'
    drv = ident['driver']
    ident_eng = [{'name': n, 'supplied': e['path'], 'path': os.path.abspath(e['path']), 'env': e['env_overrides']}
                 for n, e in ident['engines'].items()]
    ident_eng.sort(key=lambda e: e['name'] != base)
    current = {'job_helper': file_sha(ident['job_helper']['path'])}
    need(os.path.isfile(drv['path']) and file_sha(drv['path']) == drv['sha256'], 'current driver differs from identity report')
    for e in ident['engines'].values():
        p = e['path']
        need(file_sha(p) == e['sha256'], 'current engine exe differs from identity report: %s' % p)
        for suffix, key in (('.cold', 'cold_image_sha256'), ('.native-startup.el', 'startup_companion_sha256')):
            need(os.path.isfile(p + suffix) and file_sha(p + suffix) == e[key], 'current %s differs from identity report: %s' % (suffix, p))
    engines = {e['name']: e for e in ident_eng}
    # --- prefix: failed raw, 5 complete pairs + explicit orphan ---
    need(prefix.get('status') == 'failed' and 'base-r05-n1 timed out' in str(prefix.get('error')), 'prefix is not the base-r05 timeout failure')
    check_meta('prefix', prefix, ident, runner_sha, current)
    need(engine_set(prefix['engines']) == engine_set(ident_eng), 'prefix engines differ from identity engines')
    ps = prefix['samples']
    need([(s['rep'], s['engine']) for s in ps] == expected_order(0, PREFIX_PAIRS, [base, cand]) + [(5, cand)],
         'prefix must be exactly 5 alternating pairs plus orphan candidate-r05')
    hashes = {s['sample_id']: check_sample(runner, s, prefix_dir, engines, cpu, drv['path']) for s in ps[:-1]}
    orphan = ps[-1]
    try:
        orphan_check = runner.parse_check(os.path.join(prefix_dir, os.path.basename(orphan['stdout'].replace('\\', '/'))), 1)
    except Exception as exc:  # orphan is unselected; its state is recorded, never used
        orphan_check = 'unreadable: %s' % exc
    # --- tail: fresh complete campaign ---
    need(tail_raw.get('status') == 'ok' and tail_raw.get('error') is None and tail_rep.get('status') == 'ok', 'tail is not status ok')
    need(tail_raw['mode'] == tail_rep['mode'] == 'single_pass' and tail_rep.get('reps') == TAIL_PAIRS, 'tail is not single_pass reps2')
    need(tail_raw['started_utc'] == tail_rep['started_utc'] and tail_raw['started_utc'] > prefix['started_utc'] > ident['started_utc'],
         'tail/prefix/identity not chronological or raw/report mismatch')
    check_meta('tail raw', tail_raw, ident, runner_sha, current)
    check_meta('tail report', tail_rep, ident, runner_sha, current)
    need(tail_rep['driver'] == drv, 'tail driver differs from identity')
    need(engine_set(tail_raw['engines']) == engine_set(ident_eng) and [e['name'] for e in tail_raw['engines']] == [cand, base],
         'tail engines differ from identity or are not in the intended reversed order')
    for n, e in ident['engines'].items():
        te = tail_rep['engines'][n]
        for k in ('sha256', 'cold_image_sha256', 'startup_companion_sha256', 'env_overrides'):
            need(te[k] == e[k], 'tail report %s %s differs from identity' % (n, k))
        need(norm(te['path']) == norm(e['path']), 'tail report engine path differs: %s' % n)
    ts = tail_raw['samples']
    need([(s['rep'], s['engine']) for s in ts] == expected_order(0, TAIL_PAIRS, [cand, base]), 'tail order is not candidate/base then base/candidate')
    for s in ts:
        hashes[s['sample_id'] + '@tail'] = check_sample(runner, s, tail_dir, engines, cpu, drv['path'])
    for n in (base, cand):  # report values must be the raw elapsed values
        need(tail_rep['engines'][n]['elapsed_s_per_rep'] == [s['check']['elapsed'] for s in sorted(ts, key=lambda s: s['rep']) if s['engine'] == n],
             'tail report values differ from tail raw for %s' % n)
    # --- selection: first 5 prefix pairs + tail remapped in memory (local rep + 5) ---
    selected = [dict(copy.deepcopy(s), source='prefix', source_sample_id=s['sample_id']) for s in ps[:-1]]
    selected += [dict(copy.deepcopy(s), rep=s['rep'] + PREFIX_PAIRS, source='tail', source_rep=s['rep'], source_sample_id=s['sample_id'])
                 for s in ts]
    # chronological alternating layout: even rep base,cand ; odd rep cand,base
    order = {k: i for i, k in enumerate(expected_order(0, PREFIX_PAIRS + TAIL_PAIRS, [base, cand]))}
    selected.sort(key=lambda s: order.get((s['rep'], s['engine']), 10 ** 6))
    if mutate_selected:
        selected = mutate_selected(selected, orphan)
    validate_selected(runner, selected, base, cand)
    meta = {k: prefix[k] for k in ('schema', 'allowed_affinity_mask', 'runner', 'job_helper', 'job_limits')}
    meta.update(started_utc=prefix['started_utc'], tail_started_utc=tail_rep['started_utc'],
                campaign={'kind': 'continuation', 'layout': '5+2', 'prefix_pairs': PREFIX_PAIRS, 'tail_pairs': TAIL_PAIRS,
                          'manifest': 'manifest.json', 'note': 'derived from a failed prefix + fresh tail; no complete raw.json exists'})
    ns = argparse.Namespace(engines=ident_eng, single_pass=True, reps=7, cpu=cpu, low=None, high=None,
                            driver=drv['path'], driver_resolved=os.path.abspath(drv['path']))
    report = runner.build_report(ns, selected, meta)
    need(report['status'] == 'ok' and report['driver']['sha256'] == drv['sha256'], 'derived report driver differs')
    for n, e in ident['engines'].items():
        for k in ('sha256', 'cold_image_sha256', 'startup_companion_sha256', 'env_overrides'):
            need(report['engines'][n][k] == e[k], 'derived report %s %s differs from identity' % (n, k))
    manifest = {'selected': [{'logical_rep': s['rep'], 'engine': s['engine'], 'source': s['source'], 'source_sample_id': s['source_sample_id'],
                              'source_rep': s.get('source_rep', s['rep']), 'elapsed': s['check']['elapsed']} for s in selected],
                'unselected_orphan': {'sample_id': orphan['sample_id'], 'reason': 'coordinator discarded the orphan before continuation results',
                                      'stdout_check': orphan_check},
                'stdout_sha256': hashes, 'conditional_result': selected[0]['check']['result'],
                'runner_sha256': runner_sha, 'prefix_error': prefix['error']}
    return report, manifest


def self_test(runner, runner_sha, data):
    def run(mut, selected=None):
        d = copy.deepcopy(data)
        if mut:
            mut(d)
        try:
            verify(runner, runner_sha, d['prefix'], d['prefix_dir'], d['tail_raw'], d['tail_rep'], d['tail_dir'], d['ident'], selected)
        except (AdapterError, runner.BenchError, KeyError, IndexError, TypeError):
            return True
        return False
    def drop(d): del d['prefix']['samples'][2]
    def mism(d): d['prefix']['samples'][1]['check']['result'] = 'f' * 64
    def elap(d): d['tail_raw']['samples'][0]['check']['elapsed'] += 1.0
    def cpu(d): d['tail_rep']['cpu'] = 3
    def mem(d): d['prefix']['job_limits']['job_memory_limit'] = 4 * 1024 ** 3
    def flip(d): d['tail_raw']['samples'][0], d['tail_raw']['samples'][1] = d['tail_raw']['samples'][1], d['tail_raw']['samples'][0]
    def env(d): d['tail_raw']['engines'][0]['env'] = {'X': '1'}
    def eng_sha(d): d['ident']['engines']['candidate']['sha256'] = '0' * 64
    def drv_sha(d): d['ident']['driver']['sha256'] = '0' * 64
    def job_sha(d): d['ident']['job_helper']['sha256'] = '0' * 64
    def cold(d): d['tail_rep']['engines']['base']['cold_image_sha256'] = '0' * 64
    def runner_sha_(d): d['prefix']['runner']['sha256'] = '0' * 64
    def reps(d): d['tail_rep']['reps'] = 3
    def failed(d): d['tail_raw']['status'] = 'failed'
    def extra(sel, orphan): return sel + [dict(orphan, rep=5, source='prefix', source_sample_id=orphan['sample_id'])]
    def dup(sel, orphan): return sel[:-2] + [dict(s, rep=sel[-4]['rep']) for s in sel[-4:-2]]
    controls = [('remove selected sample', drop, None), ('check result vs stdout', mism, None), ('elapsed vs stdout', elap, None),
                ('tail cpu', cpu, None), ('job memory limit', mem, None), ('tail order', flip, None), ('tail env override', env, None),
                ('engine hash', eng_sha, None), ('driver hash', drv_sha, None), ('job helper hash', job_sha, None),
                ('cold image hash', cold, None), ('runner hash', runner_sha_, None), ('tail rep count', reps, None),
                ('tail status', failed, None), ('orphan as extra selected', None, extra), ('duplicate pair', None, dup)]
    survived = [name for name, mut, sel in controls if not run(mut, sel)]
    need(controls and not survived, 'self-test controls accepted invalid input: %s' % survived)
    return len(controls)


def main(argv):
    ap = argparse.ArgumentParser(description=__doc__)
    for name in ('prefix', 'tail', 'identity-report', 'runner', 'out'):
        ap.add_argument('--' + name, required=True)
    ap.add_argument('--self-test', action='store_true')
    a = ap.parse_args(argv)
    for name in ('report.json', 'manifest.json'):
        need(not os.path.exists(os.path.join(a.out, name)), 'refusing to overwrite %s in %s' % (name, a.out))
    runner, runner_sha = load_runner(a.runner), file_sha(a.runner)
    tail_raw_path, tail_rep_path = os.path.join(a.tail, 'raw.json'), os.path.join(a.tail, 'report.json')
    data = dict(prefix=load_json(a.prefix), prefix_dir=os.path.dirname(os.path.abspath(a.prefix)), tail_raw=load_json(tail_raw_path),
                tail_rep=load_json(tail_rep_path), tail_dir=a.tail, ident=load_json(a.identity_report))
    report, manifest = verify(runner, runner_sha, data['prefix'], data['prefix_dir'], data['tail_raw'], data['tail_rep'],
                              data['tail_dir'], data['ident'])
    if a.self_test:
        print('self-test: %d negative controls rejected' % self_test(runner, runner_sha, data))
    manifest['sources'] = {'prefix_raw_sha256': file_sha(a.prefix), 'tail_raw_sha256': file_sha(tail_raw_path),
                           'tail_report_sha256': file_sha(tail_rep_path), 'identity_report_sha256': file_sha(a.identity_report)}
    os.makedirs(a.out, exist_ok=True)
    body = json.dumps(report, indent=1, sort_keys=True)
    manifest['derived_report_sha256'] = hashlib.sha256(body.encode('utf-8')).hexdigest()
    with open(os.path.join(a.out, 'report.json'), 'w', encoding='utf-8') as f:
        f.write(body)
    with open(os.path.join(a.out, 'manifest.json'), 'w', encoding='utf-8') as f:
        json.dump(manifest, f, indent=1, sort_keys=True)
    for n, e in report['engines'].items():
        print('%s median=%.6f ratio=%.3f' % (n, e['median'], e['ratio_to_baseline_median']))
    print('report: %s' % os.path.join(a.out, 'report.json'))
    return 0


if __name__ == '__main__':
    try:
        sys.exit(main(sys.argv[1:]))
    except (AdapterError, OSError, ValueError) as exc:
        print('REJECTED: %s' % exc, file=sys.stderr)
        sys.exit(2)
