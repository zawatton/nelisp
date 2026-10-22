#!/usr/bin/env python3
"""Instruction-count checks against GNU 31.1 for the allocation path and the collector (gc-port lane, ledger S2.6, S2.7).

  gc-port-ir.py alloc --dyn BIN [--gnu EMACS] [--work DIR] [--n N]
  gc-port-ir.py collect --dyn BIN [--gnu EMACS] [--work DIR]

alloc:   callgrind runs of N allocations in an interpreted while loop (the object is dropped at the next iteration) on
         the candidate and on GNU, with a no-allocation loop as the baseline.  The figure per allocation is the inclusive
         Ir per call of the constructor, the loop's other costs (the evaluator's call dispatch, which belongs to other
         lanes) subtracted by the baseline run:
           cons    NeLisp nelisp_cons_construct_w            GNU Fcons
           string  NeLisp bf_make_string_w (128 bytes)       GNU Fmake_string (allocate_string, allocate_string_data,
                                                             memset)
           vector  NeLisp bf_make_vector_w (16 slots)        GNU Fmake_vector (allocate_vectorlike)
         PASS when each is at most GNU's.
collect: the steady-state instructions of one explicit collection (callgrind runs of 6 and of 3 collections of the Doc 220
         cons churn, the difference over 3) on the candidate and on GNU, and the calls of nl_gc_mark_block per collection.
         PASS when the candidate's figure is at most GNU's.
Both need valgrind and take a few minutes; run them through bin/heavy-run.sh.  Exit 0 only on PASS."""
import argparse
import bisect
import importlib.util
import os
import re
import subprocess
import sys
from collections import Counter
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
spec = importlib.util.spec_from_file_location('gp', ROOT / 'tools/ai/gc-port-prof.py')
gp = importlib.util.module_from_spec(spec)
spec.loader.exec_module(gp)

KINDS = {'none': 'i', 'cons': '(cons i i)', 'string': '(make-string 128 120)', 'vector': '(make-vector 16 i)'}
NELISP_ENTRIES = {'cons': ['nelisp_cons_construct_w'],
                  'string': ['bf_make_string_w'],
                  'vector': ['bf_make_vector_w']}
GNU_ENTRIES = {'cons': ['Fcons'], 'string': ['Fmake_string'], 'vector': ['Fmake_vector']}


def symtab(binary):
    rows = []
    for line in subprocess.check_output(['nm', '-n', str(binary)], text=True).splitlines():
        c = line.split()
        if len(c) == 3 and c[1] in 'tT':
            rows.append((int(c[0], 16), c[2]))
    rows.sort()
    return [r[0] for r in rows], rows


def edges(path, tab):
    """(caller, callee) -> calls and inclusive Ir, from a callgrind output file."""
    addr, rows = tab
    names, cur, callee, pend = {}, '?', None, None
    cnt, inc = Counter(), Counter()
    for line in Path(path).read_text().splitlines():
        m = re.match(r'(c?fn)=\((\d+)\)(?: (.*))?$', line)
        if m:
            if m[3] is not None:
                n = m[3].split("'")[0]
                if n.startswith('0x'):
                    i = bisect.bisect_right(addr, int(n, 16)) - 1
                    n = rows[i][1] if i >= 0 else n
                names[m[2]] = n
            n = names.get(m[2], '?')
            if m[1] == 'fn':
                cur, pend = n, None
            else:
                callee = n
            continue
        if line.startswith('calls='):
            pend = int(line[6:].split()[0])
            continue
        if pend is not None and re.match(r'^[+*\-0-9]', line):
            cnt[(cur, callee)] += pend
            inc[(cur, callee)] += int(line.split()[1])
            pend = None
    return cnt, inc


def callgrind(engine, binary, workload, outdir):
    outdir.mkdir(parents=True, exist_ok=True)
    src = outdir / 'workload.el'
    src.write_text(workload)
    raw = outdir / 'callgrind.out'
    argv = ['valgrind', '--tool=callgrind', '--error-exitcode=97', '--callgrind-out-file=' + str(raw), str(binary)]
    argv += ['-Q', '--batch', '--load', str(src)] if engine == 'gnu' else ['--load', str(src)]
    env = dict(os.environ, LC_ALL='C', TMPDIR=str(outdir), XDG_CACHE_HOME=str(outdir))
    cmd = ['bash', '-c', 'ulimit -v 8388608; exec timeout 1500 "$@"', 'gcir'] + argv
    with (outdir / 'stdout').open('w') as out, (outdir / 'stderr').open('w') as err:
        rc = subprocess.run(cmd, env=env, stdout=out, stderr=err).returncode
    if rc != 0:
        raise SystemExit(f'FAIL callgrind {engine} in {outdir}: rc={rc}')
    total = 0
    for line in raw.read_text().splitlines():
        if line.startswith('summary:') or line.startswith('totals:'):
            total = int(line.split()[1])
    return raw, total


def loop(expr, n):
    return ('(setq gc-cons-threshold 1073741824 gc-cons-percentage 1.0)\n(defvar sink nil)\n'
            f'(let ((i 0)) (while (< i {n}) (setq sink {expr}) (setq i (1+ i))))\n(princ "IR done\\n")\n')


def inclusive_per_call(kind_edges, base_edges, names):
    """Inclusive Ir per call of the functions NAMES (summed), the baseline run's share subtracted."""
    (cnt, inc), (bcnt, binc) = kind_edges, base_edges
    calls = {}
    ir = 0
    for name in names:
        c = sum(v for (caller, callee), v in cnt.items() if callee == name)
        i = sum(v for (caller, callee), v in inc.items() if callee == name)
        bc = sum(v for (caller, callee), v in bcnt.items() if callee == name)
        bi = sum(v for (caller, callee), v in binc.items() if callee == name)
        calls[name] = c - bc
        ir += i - bi
    n = max(calls.values()) if calls else 0
    return (ir / n if n else float('inf')), calls


def cmd_alloc(a):
    n = a.n
    tabs = {'nelisp': symtab(a.dyn), 'gnu': symtab(a.gnu)}
    bins = {'nelisp': a.dyn, 'gnu': a.gnu}
    fig = {}
    for engine in ('nelisp', 'gnu'):
        base = edges(callgrind(engine, bins[engine], loop(KINDS['none'], n), a.work / f'{engine}-none')[0], tabs[engine])
        for kind in ('cons', 'string', 'vector'):
            raw, _ = callgrind(engine, bins[engine], loop(KINDS[kind], n), a.work / f'{engine}-{kind}')
            names = (NELISP_ENTRIES if engine == 'nelisp' else GNU_ENTRIES)[kind]
            fig[(engine, kind)], _calls = inclusive_per_call(edges(raw, tabs[engine]), base, names)
    ok = True
    for kind in ('cons', 'string', 'vector'):
        ours, gnu = fig[('nelisp', kind)], fig[('gnu', kind)]
        good = ours <= gnu
        ok &= good
        print(f"{'PASS' if good else 'FAIL'} alloc {kind}: NeLisp {ours:.0f} Ir per allocation, GNU {gnu:.0f} (x{ours / gnu:.1f})")
    print('GC-PORT-IR alloc ' + ('PASS' if ok else 'FAIL'))
    return 0 if ok else 1


def cmd_collect(a):
    tabs = {'nelisp': symtab(a.dyn), 'gnu': symtab(a.gnu)}
    bins = {'nelisp': a.dyn, 'gnu': a.gnu}
    per, marks = {}, None
    for engine in ('nelisp', 'gnu'):
        res = {}
        for k in (3, 6):
            res[k] = callgrind(engine, bins[engine], gp.workload('cons', count=1, rounds=k), a.work / f'{engine}-collect{k}')
        per[engine] = (res[6][1] - res[3][1]) / 3
        if engine == 'nelisp':
            c6, _ = edges(res[6][0], tabs[engine])
            c3, _ = edges(res[3][0], tabs[engine])
            marks = (sum(v for (caller, callee), v in c6.items() if callee == 'nl_gc_mark_block')
                     - sum(v for (caller, callee), v in c3.items() if callee == 'nl_gc_mark_block')) / 3
    good = per['nelisp'] <= per['gnu']
    print(f"{'PASS' if good else 'FAIL'} collect: NeLisp {per['nelisp'] / 1e6:.1f}M Ir per steady-state collection, GNU "
          f"{per['gnu'] / 1e6:.1f}M (x{per['nelisp'] / per['gnu']:.2f}); nl_gc_mark_block {marks:,.0f} calls per collection")
    print('GC-PORT-IR collect ' + ('PASS' if good else 'FAIL'))
    return 0 if good else 1


def main(argv):
    ap = argparse.ArgumentParser()
    ap.add_argument('command', choices=['alloc', 'collect'])
    ap.add_argument('--dyn', required=True, type=Path)
    ap.add_argument('--gnu', type=Path, default=Path('/usr/local/bin/emacs'))
    ap.add_argument('--work', type=Path, default=ROOT / 'target/gc-port/ir')
    ap.add_argument('--n', type=int, default=100000)
    a = ap.parse_args(argv)
    a.dyn, a.gnu, a.work = a.dyn.resolve(), a.gnu.resolve(), a.work.resolve()
    a.work.mkdir(parents=True, exist_ok=True)
    return cmd_alloc(a) if a.command == 'alloc' else cmd_collect(a)


if __name__ == '__main__':
    sys.exit(main(sys.argv[1:]))
