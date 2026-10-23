#!/usr/bin/env python3
"""Instructions of a steady-state collection of the conservative-cost check's workload, scan on and off (integ-wh S2.1, S2.21).

  integ-wh-gc-collect-ir.py measure --dyn BIN [--dyn BIN ...] [--work DIR] [--gnu EMACS]
  integ-wh-gc-collect-ir.py check --dyn BIN [--work DIR] [--gnu EMACS]

The workload has the shape of test/standalone-gc-conservative-cost-check.el: two (garbage-collect) at the top level, then
a defun'd function that collects in a loop (a bare top-level loop leaves the stack the scan walks without the evaluator's
frames of a function call and shows no difference between the scan's forms).  For each binary two callgrind runs (3 and 6 collections) with the stack scan on and the same with it
off (nelisp--debug-switch 28), so the figure is (Ir of 6 collections - Ir of 3) / 3: the average of collections 4 to 6.  The
difference between the two figures is the scan's cost; the calls of the scan's functions per collection and their inclusive
Ir come from the same runs.  GNU 31.1 runs the same loop once for the reference.  Each run is one callgrind process under an
8 GiB address-space cap; run the whole measurement as one heavy unit (bin/heavy-run.sh).  Prints one line per binary:

  IR-COLLECT <label> scan-on <M> Ir, scan-off <M> Ir, scan <M> Ir; calls per collection: nl_gc_conserv_word N ...

`measure` exits 0 when every run completed.  `check` is the ledger criterion: PASS iff the scan costs at most 2.0 M Ir of a
collection and a collection with the scan on costs at most 1.10 times GNU's (batch D: scan 9.9 M, collection 57.5 M against
GNU's 47.4 M; the prototype of the string areas: 0.3 M and 48.0 M).
"""
import argparse
import importlib.util
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
spec = importlib.util.spec_from_file_location('gi', ROOT / 'tools/ai/gc-port-ir.py')
gi = importlib.util.module_from_spec(spec)
spec.loader.exec_module(gi)

# the functions of the scan and of the string areas whose calls and inclusive Ir per collection are reported
WATCH = ('nl_gc_conserv_scan', 'nl_gc_conserv_word', 'nl_gc_conserv_string_area', 'nl_gc_conserv_drain',
         'nl_string_pin_word', 'nl_string_mapping_op', 'nl_gc_conserv_resolve', 'nl_gc_conserv_pin',
         'nl_gc_conserv_maybe')


def workload(count, scan_on):
    # The shape of the check: two collections at the top level, then a defun'd function that collects in a loop, so the
    # stack the scan walks holds the evaluator's frames of a function call as the check's does.
    off = '' if scan_on else '(nelisp--debug-switch 28)\n'
    return ('(defun gcir--run (n) (let ((i 0)) (while (< i n) (garbage-collect) (setq i (1+ i)))))\n'
            f'(garbage-collect)\n(garbage-collect)\n{off}(gcir--run {count})\n(princ "IR done\\n")\n')


def gnu_workload(count):
    return ('(defun gcir--run (n) (let ((i 0)) (while (< i n) (garbage-collect) (setq i (1+ i)))))\n'
            f'(garbage-collect)\n(garbage-collect)\n(gcir--run {count})\n(princ "IR done\\n")\n')


def per_collection(a, label, binary, scan_on):
    """(Ir per collection, {function: (calls, inclusive Ir)} per collection) from runs of 3 and 6 collections."""
    tab = gi.symtab(binary)
    runs = {}
    for k in (3, 6):
        raw, total = gi.callgrind('nelisp', binary, workload(k, scan_on), a.work / f'{label}-{"on" if scan_on else "off"}-{k}')
        runs[k] = (total, gi.edges(raw, tab))
    ir = (runs[6][0] - runs[3][0]) / 3
    detail = {}
    for name in WATCH:
        calls = {}
        incl = {}
        for k in (3, 6):
            cnt, inc = runs[k][1]
            calls[k] = sum(v for (caller, callee), v in cnt.items() if callee == name)
            incl[k] = sum(v for (caller, callee), v in inc.items() if callee == name)
        detail[name] = ((calls[6] - calls[3]) / 3, (incl[6] - incl[3]) / 3)
    return ir, detail


def main(argv):
    ap = argparse.ArgumentParser()
    ap.add_argument('command', choices=['measure', 'check'])
    ap.add_argument('--dyn', required=True, type=Path, action='append')
    ap.add_argument('--gnu', type=Path, default=Path('/usr/local/bin/emacs'))
    ap.add_argument('--work', type=Path, default=ROOT / 'target/integ-wh/collect-ir')
    ap.add_argument('--no-gnu', action='store_true')
    a = ap.parse_args(argv)
    a.work = a.work.resolve()
    a.work.mkdir(parents=True, exist_ok=True)
    verdict = 0
    for binary in a.dyn:
        binary = binary.resolve()
        label = binary.name
        on, detail_on = per_collection(a, label, binary, True)
        off, _ = per_collection(a, label, binary, False)
        calls = ' '.join(f'{n} {detail_on[n][0]:,.0f} ({detail_on[n][1] / 1e6:.2f}M)' for n in WATCH if detail_on[n][0] > 0)
        print(f'IR-COLLECT {label} scan-on {on / 1e6:.1f}M Ir, scan-off {off / 1e6:.1f}M Ir, scan {(on - off) / 1e6:.1f}M Ir; '
              f'per collection, calls (inclusive Ir): {calls}')
        sys.stdout.flush()
        if a.command == 'check':
            a.scan_on, a.scan = on, on - off
    if not a.no_gnu or a.command == 'check':
        gnu = a.gnu.resolve()
        r = {}
        for k in (3, 6):
            r[k] = gi.callgrind('gnu', gnu, gnu_workload(k), a.work / f'gnu-{k}')[1]
        per_gnu = (r[6] - r[3]) / 3
        print(f'IR-COLLECT gnu {per_gnu / 1e6:.1f}M Ir per collection')
        if a.command == 'check':
            ok = a.scan <= 2.0e6 and a.scan_on <= 1.10 * per_gnu
            print(f"{'PASS' if ok else 'FAIL'} gc-collect-ir: scan {a.scan / 1e6:.1f}M Ir (limit 2.0M), collection with the scan "
                  f"{a.scan_on / 1e6:.1f}M against GNU {per_gnu / 1e6:.1f}M (x{a.scan_on / per_gnu:.2f}, limit x1.10)")
            verdict = 0 if ok else 1
    return verdict


if __name__ == '__main__':
    sys.exit(main(sys.argv[1:]))
