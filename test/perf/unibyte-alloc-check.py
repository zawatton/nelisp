#!/usr/bin/env python3
"""Reproducible allocation-regression evidence; preserve whole-process Ir/GC.

Set UA_PRE_BIN, UA_POST_BIN, UA_BIN and UA_STATIC. `profile LABEL BIN`
records all four layouts. `probe LABEL BIN` records the three producers.
Checks consume hash-verified profiles; UA_GDB=1 additionally executes the
unmodified-workload exit-statistics check delegated to the coordinator.
"""
import bisect
from collections import Counter
import hashlib
import json
import os
from pathlib import Path
import re
import subprocess
import sys
import time

ROOT = Path(__file__).resolve().parents[2]
OUT = Path(os.environ.get('UA_EVIDENCE', ROOT / 'target/unibyte-alloc')).resolve()
LAYOUTS = (0, 700, 1400, 2100)
CAP = 2558239688
MARKER = b'ABI-PRESSURE 60080 360991440\n'


def digest(path):
    return hashlib.sha256(Path(path).read_bytes()).hexdigest()


def symbols(binary):
    rows = [r.split() for r in subprocess.check_output(['nm', '-n', binary], text=True).splitlines()]
    return sorted((int(r[0], 16), r[2]) for r in rows if len(r) == 3 and r[1] in ('t', 'T', 'B', 'b'))


def counts(profile, binary):
    syms = symbols(binary)
    addresses = [a for a, _ in syms]
    lines = Path(profile).read_text().splitlines()
    names = {}
    for line in lines:
        match = re.match(r'c?fn=\((\d+)\) (.*)', line)
        if match:
            name = match[2]
            if name.startswith('0x'):
                i = bisect.bisect_right(addresses, int(name.split("'")[0], 16)) - 1
                if i >= 0:
                    name = syms[i][1]
            names[match[1]] = name.split("'")[0]
    caller = callee = None
    edges = Counter()
    gc = 0
    for line in lines:
        match = re.fullmatch(r'(c?fn)=\((\d+)\)(?: .*)?', line)
        if match:
            if match[1] == 'fn':
                caller, callee = names[match[2]], None
            else:
                callee = names[match[2]]
        if line.startswith('calls='):
            n = int(line[6:].split()[0])
            if callee == 'nl_alloc_bytes':
                edges[caller] += n
            if callee == 'nl_gc_sweep_merge_chunks' and caller != callee:
                gc += n
    total = next(int(l.split()[1]) for l in lines if l.startswith('summary:'))
    assert total > 0 and sum(edges.values()) > 0, 'empty instruction/allocator profile'
    return dict(ir=total, calls=sum(edges.values()), edges=dict(edges), gc=gc)


def compact_options(binary):
    # Collapse private edges, retaining all instructions and allocator/GC entry.
    keep = {'nl_alloc_bytes', 'nl_gc_sweep_merge_chunks', 'nl_gc_sweep'}
    return [f'--fn-skip=0x{a:016x}' for a,n in symbols(binary)
            if n not in keep and n.startswith(('nl_', 'nelisp_', 'bf_', 'wf_', 'm5_'))]


def profile(label, binary, probe=False):
    from concurrent.futures import ThreadPoolExecutor
    binary = str(Path(binary).resolve())
    OUT.mkdir(parents=True, exist_ok=True)
    binary_sha = digest(binary)
    cold = Path(binary+'.cold')
    assert cold.exists(), 'finish the complete reader build, including its cold image'
    cold_sha = digest(cold)
    jobs = [('number', '(number-to-string i)'), ('concat', '(concat "s" "123")'),
            ('format', '(format "key-%d" i)')] if probe else [(n, None) for n in LAYOUTS]
    options = [] if probe else compact_options(binary)

    def run_job(job):
        layout, expression = job
        stem = OUT / f'{label}-{layout}'
        env = dict(os.environ, TMPDIR=str(OUT), ABI_LAYOUT_PAD='x' * (0 if probe else layout))
        args = ['--eval', f'(let ((i 0)) (while (< i 100) {expression} (setq i (+ i 1))) (princ "PROBE-OK"))'] if probe else ['--load', str(ROOT / 'test/unibyte-alloc-pressure80.el')]
        start = time.monotonic()
        cached = (os.environ.get('UA_RESUME') == '1' and stem.with_suffix('.cg').exists()
                  and ('cmd:  '+binary+' ') in stem.with_suffix('.cg').read_text()
                  and stem.with_suffix('.valgrind').exists()
                  and 'Collected :' in stem.with_suffix('.valgrind').read_text())
        if not cached:
            with stem.with_suffix('.stdout').open('wb') as out, stem.with_suffix('.stderr').open('wb') as err:
                subprocess.run(['valgrind', '--tool=callgrind', '--error-exitcode=97',
                                f'--callgrind-out-file={stem}.cg', f'--log-file={stem}.valgrind',
                                *options, binary, *args], cwd=ROOT, env=env,
                               stdout=out, stderr=err, check=True)
        stdout = stem.with_suffix('.stdout').read_bytes()
        assert not stem.with_suffix('.stderr').stat().st_size, 'unexpected stderr'
        assert (b'PROBE-OK' in stdout if probe else stdout in (MARKER, MARKER+b'"'+MARKER+b'"\n')), repr(stdout)
        row = counts(stem.with_suffix('.cg'), binary)
        row.update(layout=layout, seconds=time.monotonic()-start,
                   profile=str(stem.with_suffix('.cg').relative_to(ROOT)),
                   sha256=digest(stem.with_suffix('.cg')), stdout=stdout.decode(),
                   stdout_path=str(stem.with_suffix('.stdout').relative_to(ROOT)),
                   stdout_sha256=digest(stem.with_suffix('.stdout')))
        print(label, layout, row['calls'], row['ir'], flush=True)
        return row
    with ThreadPoolExecutor(max_workers=int(os.environ.get('UA_JOBS','2'))) as pool:
        rows = list(pool.map(run_job, jobs))
    assert digest(binary) == binary_sha and digest(cold) == cold_sha, 'binary/image changed during profiling'
    result = dict(binary=binary,binary_sha256=binary_sha,cold_sha256=cold_sha,
                  workload_sha256=digest(ROOT/'test/unibyte-alloc-pressure80.el'),rows=rows)
    (OUT / f'{label}{"-probes" if probe else ""}.json').write_text(json.dumps(result, indent=2)+'\n')


def record(label, probe=False):
    result = json.loads((OUT / f'{label}{"-probes" if probe else ""}.json').read_text())
    expected = {'pre':'UA_PRE_BIN','post':'UA_POST_BIN','head':'UA_BIN'}.get(label)
    if expected and os.environ.get(expected):
        assert Path(result['binary']).resolve() == Path(os.environ[expected]).resolve(), 'wrong measured binary'
    assert digest(result['binary']) == result['binary_sha256'], 'changed binary'
    if 'cold_sha256' in result:
        assert digest(result['binary']+'.cold') == result['cold_sha256'], 'changed cold image'
    if not probe:
        assert digest(ROOT/'test/unibyte-alloc-pressure80.el') == result['workload_sha256']
    for row in result['rows']:
        assert digest(ROOT / row['profile']) == row['sha256'], 'changed profile'
        if not probe:
            assert digest(ROOT/row['stdout_path']) == row['stdout_sha256']
            assert (ROOT/row['stdout_path']).read_text() == row['stdout']
        measured = counts(ROOT / row['profile'], result['binary'])
        assert all(measured[k] == row[k] for k in ('calls','ir','edges','gc')), 'altered measurement summary'
        if not probe:
            assert row['stdout'].encode() in (MARKER, MARKER+b'\"'+MARKER+b'\"\n')
    assert [r['layout'] for r in result['rows']] == (['number','concat','format'] if probe else list(LAYOUTS))
    return result['rows']


def check(mode):
    if mode == 'locate':
        pre, post = record('pre', True), record('post', True)
        changes = {a['layout']: b['calls']-a['calls'] for a,b in zip(pre,post)}
        print('producer allocation-call deltas:', changes)
        assert changes['format'] > 10000 and changes['number'] < 100 and changes['concat'] < 100
        # The unmodified R=80 workload must identify the same allocating path.
        a = counts(OUT/'pre-pad0.cg', json.loads((OUT/'pre-probes.json').read_text())['binary'])
        b = counts(OUT/'post-pad0.cg', json.loads((OUT/'post-probes.json').read_text())['binary'])
        delta = {k:b['edges'].get(k,0)-a['edges'].get(k,0) for k in set(a['edges'])|set(b['edges'])}
        assert b['calls'] > a['calls'] and delta.get('m5_fx_word_list',0) > 1000000
        summary = dict(pre=a,post=b,producer_deltas=changes,
                       growing_paths=sorted(delta.items(),key=lambda x:-x[1])[:12])
        (OUT/'located.json').write_text(json.dumps(summary,indent=2)+'\n')
        print('R=80 allocation calls:',a['calls'],'->',b['calls'])
        return
    if mode in ('calls','pressure'):
        pre, head = record('pre'), record('head')
        assert all(h['calls'] <= p['calls'] for p,h in zip(pre,head)), 'allocator call regression'
        if mode == 'pressure':
            assert sum(h['ir'] for h in head)*100 <= sum(p['ir'] for p in pre)*101, 'Ir > pre x 1.01'
            samples = {}
            for label in ('pre','post','base','head'):
                sample = json.loads((OUT/f'{label}-bytes.json').read_text())
                assert digest(sample['binary']) == sample['binary_sha256']
                assert digest(ROOT/sample['stdout']) == sample['sha256']
                raw = (ROOT/sample['stdout']).read_bytes()
                assert raw.startswith(MARKER)
                assert int(raw[len(MARKER):].strip()) == sample['allocated']
                samples[label] = sample
            for label in ('pre','head'):
                prof = json.loads((OUT/f'{label}.json').read_text())
                assert samples[label]['binary_sha256'] == prof['binary_sha256']
            projected = CAP + samples['head']['allocated'] - samples['pre']['allocated']
            proxy = dict(projected_head_bytes=projected, cap=CAP,
                         pre_calls=pre[0]['calls'],head_calls=head[0]['calls'], samples=samples)
            (OUT/'bytes-proxy.json').write_text(json.dumps(proxy,indent=2)+'\n')
            assert projected <= CAP, 'byte-volume regression in matched statistics proxy'
            # This command is intentionally runnable outside the sandbox.
            cmd = ['gdb','-q','-batch','-x','target/codex/gcstats.gdb','--args',os.environ['UA_BIN'],'--load','test/unibyte-alloc-pressure80.el']
            (OUT/'gcstats-command.json').write_text(json.dumps(cmd)+'\n')
            if os.environ.get('UA_GDB') == '1':
                output = subprocess.check_output(cmd, cwd=ROOT, stderr=subprocess.STDOUT, text=True)
                (OUT/'gcstats.log').write_text(output)
                m = re.search(r'GCSTATS debt=(\d+).* total=(\d+)', output)
                assert m and int(m[1])+int(m[2]) <= CAP
            print('allocation proxy:',proxy['projected_head_bytes'],'cap:',CAP,'gdb:',os.environ.get('UA_GDB','coordinator'))
        print(mode, 'PASS')
        return
    if mode in ('semantics','final'):
        result = json.loads((OUT/f'{mode}.json').read_text())
        required_sources={'scripts/nelisp-standalone-build.el','scripts/nelisp-stdlib-prelude.el','tools/nelisp-substrate-parity-corpus.el','test/standalone-unibyte-format-check.el','test/standalone-unibyte-format-check.expected','test/perf/unibyte-alloc-check.py'}
        assert set(result['sources']) == required_sources, 'incomplete source inventory'
        assert all(digest(ROOT/p)==sha for p,sha in result['sources'].items()), 'stale source verification'
        for key in ('UA_BIN','UA_STATIC'):
            assert result[key] == digest(os.environ[key]), 'stale verification binary'
        required = ({'dynamic','static','doc200-dynamic','doc200-static','gnu'}
                    if mode == 'semantics' else {'checks','standalone','anvil','syntax'})
        assert set(result['checks']) == required, 'incomplete verification inventory'
        for name,item in result['checks'].items():
            assert item['rc'] == 0, f'{name} failed'
            assert digest(ROOT/item['path']) == item['sha256'], 'changed verification artifact'
            log = (ROOT/item['path']).read_text()
            assert item['marker'] in log, f'{name} incomplete'
            if name == 'checks':
                assert 'GATE-COUNT: 11 cases, 0 failures' in log
            if name in ('dynamic','static'):
                assert 'GATE-COUNT: 3 cases, 0 failures' in log
            if name.startswith('doc200-'):
                assert re.search(r'Ran 12 tests, 12 results as expected, 0 unexpected',log)
            if name == 'anvil':
                assert 'ANVIL-MCP: 40 tools' in log
        print(mode,'PASS')
        return
    raise ValueError(mode)


def byte_sample(label, binary):
    binary = str(Path(binary).resolve())
    assert Path(binary+'.cold').exists(), 'finish the complete reader build first'
    address = next(a for a,n in symbols(binary) if n == 'nl_gc_stats')
    expr = f'(princ (+ (ptr-read-u64 {address} 0) (ptr-read-u64 {address} 32)))'
    args = [binary, '--load', 'test/unibyte-alloc-pressure80.el', '--eval', expr]
    start = time.monotonic()
    result = subprocess.run(args, cwd=ROOT, env=dict(os.environ, TMPDIR=str(OUT), ABI_LAYOUT_PAD=''), capture_output=True, check=True)
    elapsed_us = round((time.monotonic()-start)*1000000)
    assert not result.stderr and result.stdout.startswith(MARKER), result.stderr
    allocated = int(result.stdout[len(MARKER):].strip())
    path = OUT / f'{label}-bytes.stdout'
    path.write_bytes(result.stdout)
    row = dict(binary=binary,binary_sha256=digest(binary),allocated=allocated,
               stdout=str(path.relative_to(ROOT)),sha256=digest(path),args=args,elapsed_us=elapsed_us)
    (OUT/f'{label}-bytes.json').write_text(json.dumps(row,indent=2)+'\n')
    print(label, allocated, 'bytes;', elapsed_us, 'us (informational)', flush=True)


if __name__ == '__main__':
    if sys.argv[1] == 'bytes':
        byte_sample(sys.argv[2],sys.argv[3])
    elif sys.argv[1] in ('profile','probe'):
        profile(sys.argv[2],sys.argv[3],sys.argv[1]=='probe')
    else:
        check(sys.argv[1])
