#!/usr/bin/env python3
"""Doc 220 churn harness for the GC port (Doc 223), reduced to what S2 needs.

  gc-port-prof.py run ENGINE KIND LAYOUT BINARY OUTDIR
      One callgrind profile of the Doc 220 workload KIND (cons, string,
      vector, mixed; 600 iterations x 3 rounds, three explicit collections)
      with ABI_LAYOUT_PAD of LAYOUT bytes.  ENGINE is gnu or nelisp.  Run it
      through bin/heavy-run.sh: one profile is one heavy unit.
  gc-port-prof.py summarize OUTDIR
      Re-read OUTDIR/callgrind.out and print the phase table as JSON.

Phase bins follow Doc 220 (gc-cost-study-bench.py classify), extended with
the typed-block owners this port adds (nl_cons_*, nl_sweep_*, nl_typed_*,
nl_ablock_*).  GC plus allocation is every bin except 'other'.
"""
import bisect
import hashlib
import json
import os
import re
import subprocess
import sys
from collections import Counter
from pathlib import Path

PHASES = ('mark', 'sweep', 'index_prepare', 'freelist_rebuild',
          'allocation_fast', 'allocation_slow', 'zeroing', 'other')


def workload(kind, count=600, rounds=3, size=128, keep=64):
    constructors = {'cons': '(cons i i)', 'string': f'(make-string {size} 120)',
                    'vector': f'(make-vector {max(1, size // 8)} i)',
                    'mixed': f'(cons (make-string {size} 120) (make-vector 8 i))'}
    return f'''(setq gc-cons-threshold 1073741824 gc-cons-percentage 1.0)
(defvar study-roots (make-vector {keep} nil))
(defvar study-checksum 0)
(dotimes (r {rounds})
  (dotimes (i {count})
    (let ((x {constructors[kind]}))
      (aset study-roots (% i {keep}) x)
      (setq study-checksum (+ study-checksum i))))
  (garbage-collect))
(princ (format "STUDY {kind} {count} {rounds} %d\\n" study-checksum))
'''


def classify(engine, owner, source, line):
    if engine == 'nelisp':
        if 'zero_fill' in owner:
            return 'zeroing'
        if owner.startswith(('nl_gc_index_prepare', 'nl_gc_index_fill',
                             'nl_gc_index_bytes', 'nl_gc_index_end')):
            return 'index_prepare'
        if owner.startswith(('nl_gc_index_contains', 'nl_gc_index_test')):
            return 'mark'
        if owner.startswith(('nl_gc_sweep_merge', 'nl_gc_sweep_link', 'nl_freelist_bucket_put',
                             'nl_freelist_reset', 'nl_freelist_clear', 'nl_gc_clear_freelist_',
                             'nl_gc_coalesce_', 'nl_gc_relink_free_')):
            return 'freelist_rebuild'
        if owner.startswith(('nl_gc_sweep', 'nl_gc_symname_sweep', 'nl_sweep_')):
            return 'sweep'
        if owner.startswith(('nl_gc_mark', 'nl_gc_object_start', 'nl_gc_in_arena',
                             'nl_gc_chunk_contains', 'nl_gc_is_boot', 'nl_gc_symname_mark',
                             'nl_gc_typed_', 'nl_gc_conserv_', 'nl_gc_index_mark_',
                             'nl_gc_block_elem_cap', 'nl_typed_mark', 'nl_typed_find',
                             'nl_live_', 'nl_cons_mark')):
            return 'mark'
        if owner.startswith(('nl_alloc_bytes_slow', 'nl_alloc_bytes_uncheck',
                             'nl_chunk_alloc_new', 'nl_os_alloc', 'nl_freelist_take',
                             'nl_freelist_bucket_split', 'nl_freelist_bucket_pop',
                             'nl_alloc_check_', 'nl_ablock_', 'nl_cons_alloc_block',
                             'nl_typed_registry')):
            return 'allocation_slow'
        if owner.startswith(('nl_alloc_', 'nl_block_total', 'nl_chunk_try_alloc',
                             'nl_cons_alloc')):
            return 'allocation_fast'
    else:
        if source == 'alloc.c' and line in (1797, 2617):
            return 'zeroing'
        if source == 'alloc.c' and line in (2087, 2088, 3521, 3522, 3523):
            return 'other'
        if source == 'alloc.c' and 2611 <= line <= 2623:
            return 'allocation_slow'
        if owner.startswith(('mark_', 'process_mark_stack', 'grow_mark_stack',
                             'mem_find', 'mark_memory', 'mark_maybe')):
            return 'mark'
        if owner.startswith(('sweep_', 'gc_sweep', 'compact_small_strings',
                             'free_large_strings', 'unchain_dead_markers')):
            return 'sweep'
        if owner.startswith(('mem_insert', 'mem_delete', 'mem_init')):
            return 'index_prepare'
        if owner.startswith(('allocate_vector_from_block', 'lisp_malloc', 'lisp_align_malloc',
                             'alloc_large_vector', 'xmalloc', 'xzalloc', 'xrealloc')):
            return 'allocation_slow'
        if owner.startswith(('Fcons', 'Fmake_string', 'Fmake_vector', 'make_vector',
                             'allocate_string', 'allocate_vector', 'allocate_pseudovector',
                             'make_uninit', 'make_specified_string', 'make_string',
                             'make_pure_', 'allocate_float', 'Fmake_symbol')):
            return 'allocation_fast'
        if owner.startswith('bzero'):
            return 'zeroing'
    return 'other'


def symbol_table(binary):
    rows = []
    for line in subprocess.check_output(['nm', '-n', str(binary)], text=True).splitlines():
        cols = line.split()
        if len(cols) == 3 and cols[1] in ('t', 'T'):
            rows.append((int(cols[0], 16), cols[2]))
    rows.sort()
    return [r[0] for r in rows], rows


def self_costs(path, symbols):
    """Exclusive (owner, file, line) -> Ir and callee entry counts."""
    lines = Path(path).read_text().splitlines()
    functions, files = {}, {}
    for line in lines:
        m = re.match(r'(c?fn|c?fl|fi|fe)=\((\d+)\)(?: (.*))?$', line)
        if m and m[3] is not None:
            (functions if m[1].endswith('fn') else files)[m[2]] = m[3]
    addresses, rows = symbols

    def name(value, table, resolve=False):
        m = re.match(r'\((\d+)\)(?: (.*))?$', value)
        n = (m[2] if m[2] is not None else table.get(m[1], '?')) if m else value
        n = n.split("'")[0]
        if resolve and n.startswith('0x'):
            i = bisect.bisect_right(addresses, int(n, 16)) - 1
            return rows[i][1] if i >= 0 else n
        return n
    owner = source = callee = '?'
    position, edge, total = 0, False, None
    costs, calls = Counter(), Counter()
    for line in lines:
        if line.startswith('summary:'):
            total = int(line.split()[1])
        elif line.startswith('fn='):
            owner, edge = name(line[3:], functions, True), False
        elif line.startswith(('fl=', 'fi=', 'fe=')):
            source = Path(name(line[3:], files)).name
        elif line.startswith('cfn='):
            callee = name(line[4:], functions, True)
        elif line.startswith('calls='):
            calls[callee] += int(line[6:].split()[0])
            edge = True
        elif re.match(r'^[+*\-0-9]', line):
            cols = line.split()
            token = cols[0]
            if token != '*':
                position = position + int(token) if token[0] in '+-' else int(token)
            if edge:
                edge = False
            else:
                costs[(owner, source, position)] += int(cols[1])
    return costs, calls, total


def summarize(outdir):
    outdir = Path(outdir)
    meta = json.loads((outdir / 'run.json').read_text())
    costs, calls, total = self_costs(outdir / 'callgrind.out', symbol_table(meta['binary']))
    phase = Counter({p: 0 for p in PHASES})
    owners = Counter()
    for (owner, source, line), ir in costs.items():
        phase[classify(meta['engine'], owner, source, line)] += ir
        owners[owner] += ir
    meta.update(total_ir=total, phase_ir=dict(phase),
                gc_alloc_ir=sum(v for k, v in phase.items() if k != 'other'),
                top_owners=[[o, v] for o, v in owners.most_common(12)],
                calls={n: calls[n] for n in ('nl_gc_object_start_p', 'nl_alloc_bytes',
                                             'nl_alloc_consbox', 'nl_cons_alloc',
                                             'nl_gc_index_prepare', 'nl_sweep_conses',
                                             'garbage_collect', 'Fcons')})
    (outdir / 'summary.json').write_text(json.dumps(meta, indent=1, sort_keys=True) + '\n')
    return meta


def run(engine, kind, layout, binary, outdir):
    outdir = Path(outdir)
    outdir.mkdir(parents=True, exist_ok=True)
    src = outdir / 'workload.el'
    src.write_text(workload(kind))
    raw = outdir / 'callgrind.out'
    argv = ['valgrind', '--tool=callgrind', '--error-exitcode=97',
            '--callgrind-out-file=' + str(raw), str(binary)]
    argv += ['-Q', '--batch', '--load', str(src)] if engine == 'gnu' else ['--load', str(src)]
    env = dict(os.environ, ABI_LAYOUT_PAD='x' * int(layout), LC_ALL='C',
               TMPDIR=str(outdir), XDG_CACHE_HOME=str(outdir))
    cmd = ['bash', '-c', 'ulimit -v 8388608; exec timeout 900 "$@"', 'gc-port-prof'] + argv
    with (outdir / 'stdout').open('w') as out, (outdir / 'stderr').open('w') as err:
        rc = subprocess.run(cmd, env=env, stdout=out, stderr=err).returncode
    expected = f'STUDY {kind} 600 3 {3 * 600 * 599 // 2}\n'
    observed = (outdir / 'stdout').read_text()
    ok = observed == expected or observed == expected + '"' + expected + '"\n'
    digest = hashlib.sha256(Path(binary).read_bytes()).hexdigest()
    meta = {'engine': engine, 'workload': kind, 'layout': int(layout),
            'binary': str(Path(binary).resolve()), 'binary_sha256': digest,
            'returncode': rc, 'output_ok': ok}
    (outdir / 'run.json').write_text(json.dumps(meta, indent=1, sort_keys=True) + '\n')
    if rc != 0 or not ok:
        print(f'FAIL {outdir}: rc={rc} output_ok={ok}')
        return 1
    meta = summarize(outdir)
    print(f'{outdir.name}: total={meta["total_ir"]} gc+alloc={meta["gc_alloc_ir"]}')
    return 0


def main(argv):
    if len(argv) == 6 and argv[0] == 'run':
        return run(*argv[1:])
    if len(argv) == 2 and argv[0] == 'summarize':
        meta = summarize(argv[1])
        print(json.dumps({k: meta[k] for k in ('total_ir', 'gc_alloc_ir', 'phase_ir', 'calls')}))
        return 0
    print(__doc__)
    return 2


if __name__ == '__main__':
    sys.exit(main(sys.argv[1:]))
