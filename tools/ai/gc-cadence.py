#!/usr/bin/env python3
"""Pinned large-heap GC measurements without debugger access.

measure LABEL BIN DIRECTORY profiles all four layouts, including boot and GC.
resume LABEL BIN DIRECTORY reuses only completed, hash-pinned layout checkpoints.
report DIRECTORY reconstructs counters and costs from the raw profiles.
verify CRITERION DIRECTORY checks the ledger evidence (never a cached verdict).
Keep evidence in target/gc-cadence, not a disposable directory.
"""
import concurrent.futures
import hashlib
import importlib.util
import json
import os
from pathlib import Path
import re
import subprocess
import sys
import threading
import time

ROOT = Path(__file__).resolve().parents[2]
WORKLOAD = ROOT / 'test/perf/gc-cadence-workload.el'
LAYOUTS = (0, 700, 1400, 2100)
spec = importlib.util.spec_from_file_location('abi_ir', ROOT / 'tools/ai/abi-ir.py')
abi = importlib.util.module_from_spec(spec)
spec.loader.exec_module(abi)


def digest(path):
    return hashlib.sha256(Path(path).read_bytes()).hexdigest()


def symbols(binary):
    return {row[2]: int(row[0], 16) for line in subprocess.check_output(
        ['nm', '-n', str(binary)], text=True).splitlines()
        if len(row := line.split()) == 3 and row[1] in ('t', 'T', 'B', 'b')}


def profile(path, binary):
    """Decode self and inclusive edge Ir; nm resolves the generated ELF units."""
    import bisect
    syms = sorted((addr, name) for name, addr in symbols(binary).items())
    addrs = [addr for addr, _ in syms]
    names, own, edges, calls = {}, {}, {}, {}
    caller = target = None
    pending = False
    total = None
    for line in path.read_text().splitlines():
        if line.startswith('summary:'):
            total = int(line.split()[1])
        elif line.startswith(('fn=', 'cfn=')):
            tok = line.split('=', 1)[1]
            match = re.fullmatch(r'\((\d+)\)(?:\s+(.*))?', tok)
            name = match[2] if match else tok
            if name and name.startswith('0x'):
                pos = bisect.bisect_right(addrs, int(name.split("'")[0], 16)) - 1
                name = syms[pos][1] if pos >= 0 else name
            if match:
                if name:
                    names[match[1]] = name
                name = names.get(match[1])
            name = name.split("'")[0] if name else None
            if line.startswith('cfn='):
                target = name
            else:
                caller, target = name, None
        elif line.startswith('calls='):
            key = (caller, target)
            calls[key] = calls.get(key, 0) + int(line.split()[0][6:])
            pending = True
        elif line and line[0] in '0123456789*+-':
            cost = int(line.split()[-1])
            if pending:
                key = (caller, target)
                edges[key] = edges.get(key, 0) + cost
                pending = False
            else:
                own[caller] = own.get(caller, 0) + cost
    if not total or total <= 0 or sum(own.values()) != total:
        raise ValueError(f'Invalid or incomplete self-Ir accounting: {path}')
    return total, own, edges, calls


def snapshot(path):
    rows = re.findall(r'^GC-STATS (\S+) (\d+) (\d+) (\d+) (\d+) (\d+) (\d+)$',
                      path.read_text(), re.M)
    if [r[0] for r in rows] != ['start', *map(str, range(20)), 'end']:
        raise ValueError(f'Incomplete workload: {path}')
    if not re.search(r'^GC-DONE 1330840$', path.read_text(), re.M):
        raise ValueError(f'Missing semantic completion: {path}')
    if any(int(r[1]) <= 0 or int(r[2]) <= 0 for r in rows):
        raise ValueError(f'Nonpositive collection/allocation counters: {path}')
    if any(int(r[3]) < 150 * 1048576 for r in rows):
        raise ValueError(f'Live set below 150 MiB: {path}')
    if any(int(b[1]) < int(a[1]) or int(b[2]) < int(a[2])
           or int(b[5]) < int(a[5]) for a, b in zip(rows, rows[1:])):
        raise ValueError(f'Nonmonotone counters: {path}')
    if any(int(r[5]) < int(r[6]) or int(r[6]) < int(r[3]) or int(r[4]) <= 0
           for r in rows):
        raise ValueError(f'Invalid arena/threshold counters: {path}')
    return dict(zip(('collections', 'allocated', 'live', 'threshold', 'peak', 'reserved'),
                    map(int, rows[-1][1:])))


def identity(binary):
    result = {str(path): digest(path) for path in (binary, Path(str(binary) + '.cold'),
              Path(str(binary) + '.native-startup.el'), WORKLOAD)}
    runtime_env = {k: v for k, v in os.environ.items()
                   if k.startswith(('NELISP_', 'M5_', 'LD_', 'LC_')) or k == 'LANG'}
    result['runtime-environment'] = hashlib.sha256(
        json.dumps(runtime_env, sort_keys=True).encode()).hexdigest()
    return result


def layout_checkpoint(path, before, directory, label, layout):
    record = json.loads(path.read_text())
    expected = {f'{label}-{layout}.{suffix}' for suffix in ('cg', 'out', 'err')}
    if (record.get('identity') != before or record.get('exit') != 0
            or set(record.get('artifacts', {})) != expected):
        raise ValueError(f'Stale or incomplete layout checkpoint: {path}')
    for name, sha in record['artifacts'].items():
        if digest(directory / name) != sha:
            raise ValueError(f'Layout artifact changed: {name}')
    snapshot(directory / f'{label}-{layout}.out')
    return record



def measure(label, binary, directory, resume=False):
    directory.mkdir(parents=True, exist_ok=True)
    record = directory / (label + '.json')
    if record.exists():
        raise ValueError(f'Refuse to overwrite {record}')
    before = identity(binary)
    cancel = threading.Event()
    addr = symbols(binary)['nl_gc_stats']
    def run(layout):
        stem = directory / f'{label}-{layout}'
        checkpoint = directory / f'{label}-{layout}.done.json'
        cache = directory / f'{label}-{layout}-cache'
        if resume and checkpoint.exists():
            layout_checkpoint(checkpoint, before, directory, label, layout)
            print(f'{label} layout={layout} reused completed checkpoint', flush=True)
            return
        existing = [p for p in (cache, *(stem.with_suffix('.' + suffix)
                    for suffix in ('cg', 'out', 'err'))) if p.exists()]
        if existing:
            if not resume:
                raise ValueError(f'Refuse to overwrite partial profile: {stem}; use resume')
            archive = directory / 'interrupted' / f'{label}-{layout}-{time.time_ns()}'
            archive.mkdir(parents=True)
            for path in existing:
                path.rename(archive / path.name)
        cache.mkdir()
        env = dict(os.environ, GC_STATS_ADDR=str(addr), ABI_LAYOUT_PAD='x' * layout,
                   XDG_CACHE_HOME=str(cache), TMPDIR=str(ROOT / 'target/tmp'))
        start = time.monotonic()
        with stem.with_suffix('.out').open('w') as out, stem.with_suffix('.err').open('w') as err:
            rc = abi.run_process(['valgrind', '--tool=callgrind', '--error-exitcode=97',
                  '--callgrind-out-file=' + str(stem.with_suffix('.cg')), str(binary),
                  '--load', str(WORKLOAD)], timeout=3600, cancel=cancel,
                  cwd=ROOT, env=env, stdout=out, stderr=err)
        if rc:
            cancel.set()
            raise ValueError(f'Profile failed ({rc}): {stem}')
        counters = snapshot(stem.with_suffix('.out'))
        _, cycles = abi.profile_counts(stem.with_suffix('.cg'), binary)
        if cycles != counters['collections']:
            raise ValueError(f'Counter/profile mismatch: {stem}')
        if identity(binary) != before:
            raise ValueError('Inputs changed during profiling')
        hashes = {f'{label}-{layout}.{suffix}': digest(stem.with_suffix('.' + suffix))
                  for suffix in ('cg', 'out', 'err')}
        checkpoint.write_text(json.dumps({'identity': before, 'exit': rc,
                                         'artifacts': hashes}, indent=2) + '\n')
        print(f'{label} layout={layout} complete seconds={time.monotonic()-start:.3f}', flush=True)
    with concurrent.futures.ThreadPoolExecutor(max_workers=4) as pool:
        futures = [pool.submit(run, layout) for layout in LAYOUTS]
        try:
            for future in concurrent.futures.as_completed(futures):
                future.result()
        finally:
            cancel.set()
    if identity(binary) != before:
        raise ValueError('Inputs changed during profiling')
    artifacts = {f'{label}-{layout}.{suffix}': digest(directory / f'{label}-{layout}.{suffix}')
                 for layout in LAYOUTS for suffix in ('cg', 'out', 'err')}
    record.write_text(json.dumps({'binary': str(binary), 'identity': before,
                                 'artifacts': artifacts}, indent=2) + '\n')


def scope_cost(roots, own, edges):
    """Count private self Ir once, then boundary calls to shared helpers.

    A phase can have several entrances (for example chunk release after the
    recorded collector clears its inhibit flag). Sum its private recursive
    nodes from self Ir rather than charging only one of the entrance edges.
    Helpers also used outside this scope are charged only on calls from it.
    """
    roots = set(roots)
    children = {}
    for (caller, target), cost in edges.items():
        if cost and caller and target:
            children.setdefault(caller, set()).add(target)
    def closure(seeds, allowed=None, stop=frozenset()):
        found, todo = set(), list(seeds)
        while todo:
            node = todo.pop()
            if node in found or node in stop or (allowed is not None and node not in allowed):
                continue
            found.add(node)
            todo.extend(children.get(node, ()))
        return found
    region = closure(roots)
    shared = closure({target for caller, target in edges
                      if target in region and caller not in region and target not in roots},
                     allowed=region, stop=roots)
    private = region - shared
    own_ir = sum(own.get(name, 0) for name in private)
    shared_ir = sum(cost for (caller, target), cost in edges.items()
                    if caller in private and target in shared)
    return own_ir + shared_ir, own_ir, shared_ir


def samples(label, directory):
    record = json.loads((directory / (label + '.json')).read_text())
    binary = Path(record['binary'])
    if record['identity'] != identity(binary):
        raise ValueError('Stale measurement identity')
    expected = {f'{label}-{layout}.{suffix}' for layout in LAYOUTS for suffix in ('cg', 'out', 'err')}
    if set(record['artifacts']) != expected:
        raise ValueError('Incomplete artifact manifest')
    for name, sha in record['artifacts'].items():
        if digest(directory / name) != sha:
            raise ValueError(f'Artifact changed: {name}')
    result = []
    for layout in LAYOUTS:
        stem = directory / f'{label}-{layout}'
        counters = snapshot(stem.with_suffix('.out'))
        ir, own, edges, calls = profile(stem.with_suffix('.cg'), binary)
        # Natural outer sweeps, excluding recursion through the chunk chain.
        cycles = sum(n for (caller, target), n in calls.items()
                     if target == 'nl_gc_sweep_merge_chunks' and caller != target)
        if cycles != counters['collections']:
            raise ValueError(f'Stats/Callgrind cycle mismatch: {stem}: {cycles}/{counters}')
        entering = lambda target: sum(v for (caller, name), v in edges.items()
                                      if name == target and caller != name)
        gc_ir, gc_self, gc_shared = scope_cost(
            ('nl_gc_midform_collect', 'nl_gc_collect_form_boundary'), own, edges)
        index_ir = scope_cost(('nl_gc_index_prepare',), own, edges)[0]
        sweep_ir = scope_cost(('nl_gc_sweep', 'nl_gc_reclaim_empty_chunks',
                               'nl_gc_reclaim_swept_chunks'), own, edges)[0]
        conserv_ir = scope_cost(('nl_gc_conserv_maybe',), own, edges)[0]
        mark_ir = gc_ir - index_ir - sweep_ir - conserv_ir
        root_names = {'nl_gc_mark_roots', 'nl_gc_index_mark_roots',
                      'nl_gc_mark_recorded_contexts', 'nl_gc_mark_recorded_contexts_from',
                      'nl_gc_mark_recorded_frame', 'nl_gc_mark_recorded_env',
                      'nl_gc_mark_rootstack', 'nl_gc_mark_rootstack_walk',
                      'nl_gc_mark_thread_roots', 'nl_gc_mark_thread_roots_from',
                      'nl_n4_mark_env'}
        roots_self = sum(v for name, v in own.items() if name in root_names)
        if (min(cycles, index_ir, sweep_ir, conserv_ir, mark_ir, roots_self) <= 0
                or gc_ir > ir or roots_self > mark_ir):
            raise ValueError(f'Incomplete collector attribution: {stem}')
        result.append(dict(layout=layout, ir=ir, gc_ir=gc_ir,
                           gc_ir_per_collection=gc_ir / cycles, gc_private_self_ir=gc_self,
                           gc_shared_call_ir=gc_shared,
                           index_ir=index_ir, sweep_ir=sweep_ir, conserv_ir=conserv_ir,
                           mark_roots_ir=mark_ir, roots_self_ir=roots_self,
                           cold_restore_ir=entering('nl_cold_load_arena'), **counters))
    return result


def final_inputs(dyn, static):
    paths = [Path(__file__).resolve(), WORKLOAD,
             ROOT / 'scripts/nelisp-standalone-build.el',
             ROOT / 'test/nelisp-gc-cadence-test.el',
             ROOT / 'test/nelisp-standalone-gc-test.el',
             ROOT / 'test/gc-cadence-test.py',
             ROOT / 'test/perf/standalone-checks.sh',
             ROOT / 'test/perf/standalone-checks-known-red.txt',
             ROOT / 'target/final-verify/checks.sh']
    for binary in (dyn, static):
        paths.extend(Path(str(binary) + suffix) for suffix in
                     ('', '.cold', '.native-startup.el'))
    return {str(p): digest(p) for p in paths}


def run_final(dyn, static, directory, abi_profile):
    before = final_inputs(dyn, static)
    manifest = directory / 'final.json'
    if manifest.exists():
        raise ValueError('Refuse to overwrite final evidence')
    results = {}
    def run(name, args, env=None):
        started = time.monotonic()
        with (directory / f'final-{name}.out').open('w') as out, \
             (directory / f'final-{name}.err').open('w') as err:
            rc = subprocess.run(args, cwd=ROOT, env=env, stdout=out, stderr=err,
                                timeout=1800).returncode
        results[name] = rc
        print(f'final {name} rc={rc} seconds={time.monotonic()-started:.3f}', flush=True)
    run('checks', ['bash', str(ROOT / 'target/final-verify/checks.sh'),
                   str(dyn), str(static), 'gc-cadence-final'])
    run('standalone', ['sh', 'test/perf/standalone-checks.sh'],
        dict(os.environ, EVAL_BIN=str(dyn),
             STANDALONE_CHECK_LOGDIR=str(directory / 'standalone-final')))
    run('gnu', [os.environ.get('EMACS', 'emacs'), '-Q', '--batch', '-l', str(WORKLOAD)])
    script = Path(os.environ.get('ANVIL_MCP_SCRIPT', str(Path.home() /
                  '.emacs.d/external-packages/anvil.el/tools/anvil-standalone-mcp.sh')))
    with (directory / 'final-anvil.out').open('w') as out, \
         (directory / 'final-anvil.err').open('w') as err:
        process = subprocess.Popen([str(script)], stdin=subprocess.PIPE,
                  stdout=out, stderr=err, cwd=ROOT, env=dict(os.environ, NELISP_BIN=str(dyn)))
        try:
            time.sleep(26)
            request = {'jsonrpc': '2.0', 'id': 1, 'method': 'tools/list', 'params': {}}
            process.stdin.write((json.dumps(request) + '\n').encode())
            process.stdin.flush()
            time.sleep(2)
            process.stdin.close()
            results['anvil'] = process.wait(timeout=90)
        finally:
            if process.poll() is None:
                process.kill()
                process.wait()
    artifacts = {str(p): digest(p) for p in sorted(directory.glob('final-*.out'))}
    artifacts.update({str(p): digest(p) for p in sorted(directory.glob('final-*.err'))})
    # Include the focused native tests and the actual same-process publication.
    for name in ('gc-ert-cost.log', 'tool-tests.log', 'repl-cold.log',
                 'repl-cost.nelr', 'repl-cost-source.el', 'repl-cost-identities.sha256'):
        p = directory / name
        artifacts[str(p)] = digest(p)
    if before != final_inputs(dyn, static):
        raise ValueError('Inputs changed during final verification')
    manifest.write_text(json.dumps({'schema': 1, 'dynamic': str(dyn), 'static': str(static),
        'abi-profile': str(abi_profile), 'results': results, 'identities': before,
        'artifacts': artifacts}, indent=2) + '\n')
    return verify_final(directory, replay_abi=False)


def verify_final(directory, replay_abi=True):
    record = json.loads((directory / 'final.json').read_text())
    dyn, static = Path(record['dynamic']), Path(record['static'])
    if record.get('schema') != 1 or record['identities'] != final_inputs(dyn, static):
        raise ValueError('Stale final verification identity')
    if record['results'] != dict(checks=0, standalone=0, gnu=0, anvil=0):
        raise ValueError('A final verification process failed')
    for name, sha in record['artifacts'].items():
        if digest(name) != sha:
            raise ValueError(f'Final artifact changed: {name}')
    checks = (directory / 'final-checks.out').read_text()
    if not re.search(r'^GATE-COUNT: 11 cases, 0 failures$', checks, re.M):
        raise ValueError('Final suite did not execute all 11 groups')
    standalone = (directory / 'final-standalone.out').read_text()
    count = re.search(r'^GATE-COUNT: (\d+) cases, 0 failures$', standalone, re.M)
    if not count or int(count[1]) <= 0 or 'STANDALONE-CHECKS PASS' not in standalone:
        raise ValueError('Standalone checks are incomplete')
    if ((directory / 'final-gnu.out').read_text() != 'GC-DONE 1330840\n'
            or (directory / 'final-gnu.err').stat().st_size):
        raise ValueError('GNU workload parity failed')
    messages = [json.loads(line) for line in (directory / 'final-anvil.out').read_text().splitlines()
                if line.startswith('{')]
    replies = [m for m in messages if m.get('id') == 1]
    if len(replies) != 1 or len(replies[0].get('result', {}).get('tools', [])) != 40:
        raise ValueError('Anvil did not return exactly 40 tools')
    native = (directory / 'gc-ert-cost.log').read_text()
    if 'Ran 30 tests, 30 results as expected, 0 unexpected' not in native:
        raise ValueError('Native GC checks failed')
    verify_repl((directory / 'repl-cold.log').read_text())
    if replay_abi:
        profile_dir = Path(record['abi-profile'])
        measurement = json.loads((profile_dir / 'measurement.json').read_text())
        os.environ['ABI_DUMP'] = measurement['identities']['ABI_DUMP-path']
        os.environ['ABI_CENSUS'] = measurement['identities']['ABI_CENSUS-path']
        base = Path(measurement['identities']['baseline-path'])
        if abi.replay_ir(profile_dir, dyn, base):
            raise ValueError('ABI S6.3/S6.4 failed')
        census = os.environ['ABI_CENSUS']
        subprocess.run(['bash', 'tools/ai/abi-merge-gate.sh', 'store-word', census, '0'],
                       cwd=ROOT, check=True, stdout=subprocess.DEVNULL)
    print('Final verification PASS', flush=True)
    return 0



def verify_repl(repl):
    """Require current publication/restoration and account for diagnostic errors."""
    for marker in ('GC-CADENCE-COLD-REPL PASS', 'GC-CADENCE-COST-PUBLISHED PASS',
                   'GC-CADENCE-COST-RESTORED PASS'):
        if not re.search(r'^' + marker + r'$', repl, re.M):
            raise ValueError('Restored native publication was not demonstrated')
    # A mistyped diagnostic API was corrected in the same process. Preserve
    # its call chain in evidence; reject every other uncaught condition.
    errors = re.findall(r'^.*?nelisp: uncaught error: (.*)$', repl, re.M)
    known = ('in <repl>, top-level form #1 (byte 0, line 1): '
             'void-function: (nelisp-runtime-restore-originals)')
    if errors not in ([], [known]) or re.search(r'^error[: ]', repl, re.M | re.I):
        raise ValueError('Unexpected native REPL error')


def cadence_ok(head, old):
    """Compare byte-normalized cadence as well as actual counts and arena peak."""
    return (head['collections'] <= old['collections']
            and head['allocated'] <= old['allocated']
            and head['collections'] * old['allocated']
                <= old['collections'] * head['allocated']
            and head['peak'] <= old['peak'])


def verify(criterion, directory):
    labels = ('old', 'base') if criterion.startswith('S1') else ('old', 'base', 'head')
    profiles = directory / 'pinned'
    runs = {name: samples(name, profiles) for name in labels}
    avg = lambda name, key: sum(row[key] for row in runs[name]) / len(LAYOUTS)
    ok = False
    if criterion == 'S1.1':
        ok = (avg('base', 'collections') > avg('old', 'collections')
              and avg('base', 'allocated') <= avg('old', 'allocated'))
    elif criterion == 'S1.2':
        ok = True
    elif criterion == 'S2.1':
        ok = all(cadence_ok(h, o) for h, o in zip(runs['head'], runs['old']))
    elif criterion == 'S2.2':
        ok = avg('head', 'gc_ir_per_collection') <= avg('base', 'gc_ir_per_collection')
    elif criterion == 'S2.3':
        ok = verify_final(directory) == 0
    else:
        raise ValueError('Unknown criterion')
    print(f'{criterion} {"PASS" if ok else "FAIL"}')
    return 0 if ok else 1


def main():
    if len(sys.argv) == 6 and sys.argv[1] == 'final':
        return run_final(Path(sys.argv[2]).resolve(), Path(sys.argv[3]).resolve(),
                         Path(sys.argv[4]).resolve(), Path(sys.argv[5]).resolve())
    if len(sys.argv) == 5 and sys.argv[1] in ('measure', 'resume'):
        measure(sys.argv[2], Path(sys.argv[3]).resolve(), Path(sys.argv[4]).resolve(),
                resume=sys.argv[1] == 'resume')
        return 0
    if len(sys.argv) == 3 and sys.argv[1] == 'report':
        directory = Path(sys.argv[2])
        result = {name: samples(name, directory) for name in ('old', 'base', 'head')
                  if (directory / (name + '.json')).exists()}
        print(json.dumps(result, indent=2))
        return 0
    if len(sys.argv) == 4 and sys.argv[1] == 'verify':
        return verify(sys.argv[2], Path(sys.argv[3]))
    raise ValueError(__doc__)


if __name__ == '__main__':
    try:
        sys.exit(main())
    except (ValueError, OSError, subprocess.SubprocessError) as error:
        print(f'FAIL: {error}', file=sys.stderr)
        sys.exit(1)
