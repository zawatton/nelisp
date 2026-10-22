#!/usr/bin/env python3
"""The allocation constructors against GNU 31.1 (gc-port lane, ledger S2.6 companion).

  gc-port-alloc-golden.py check --dyn BIN --static BIN [--gnu EMACS]

Runs test/gc-port-make-string-check.el (make-string: unibyte, multibyte and forced-multibyte ASCII, every encoded
width from one byte to five and the raw-byte characters, sizes around a word and large, mutation, errors) on GNU
Emacs and on each NeLisp binary under ulimit -v 8 GiB, and requires the outputs to be identical (a NeLisp
--load prints the value of its last form, nil, which GNU's does not; both files end in nil and that line is
dropped).  A negative control changes one line of GNU's output and requires the comparison to fail.
Prints one line per run and "GC-PORT-ALLOC-GOLDEN PASS" or "... FAIL"; exit 0 on pass."""
import argparse
import difflib
import subprocess
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
FILES = ['test/gc-port-make-string-check.el']


def run(argv):
    proc = subprocess.run(['bash', '-c', 'ulimit -v 8388608; exec timeout 300 "$@"', 'golden', *argv],
                          cwd=ROOT, capture_output=True)
    text = proc.stdout.decode(errors='replace').replace('\r', '')
    lines = text.split('\n')
    while lines and lines[-1] == '':
        lines.pop()
    if lines and lines[-1] == 'nil':
        lines.pop()
    return proc.returncode, lines


def main(argv):
    ap = argparse.ArgumentParser()
    ap.add_argument('command', choices=['check'])
    ap.add_argument('--dyn', required=True, type=Path)
    ap.add_argument('--static', required=True, type=Path)
    ap.add_argument('--gnu', type=Path, default=Path('/usr/local/bin/emacs'))
    a = ap.parse_args(argv)
    ok = True
    for f in FILES:
        rc, gnu = run([str(a.gnu), '-Q', '--batch', '-l', 'cl-lib', '--load', f])
        if rc != 0 or len(gnu) < 20:
            print(f'FAIL GNU {f}: rc={rc} lines={len(gnu)}')
            ok = False
            continue
        for name, binary in (('dyn', a.dyn.resolve()), ('static', a.static.resolve())):
            rc, got = run([str(binary), '--load', f])
            same = rc == 0 and got == gnu
            diff = [] if same else list(difflib.unified_diff(gnu, got, 'gnu', name, lineterm='', n=0))[2:8]
            print(f"{'PASS' if same else 'FAIL'} {Path(f).name} {name}: {len(got)} lines, GNU {len(gnu)}"
                  f"{'' if same else ' ' + ' | '.join(d[:90] for d in diff)}")
            ok &= same
        mutated = list(gnu)
        mutated[len(mutated) // 2] += ' ?'
        if mutated == gnu:
            print('FAIL negative control did not change the output')
            ok = False
    print('GC-PORT-ALLOC-GOLDEN ' + ('PASS' if ok else 'FAIL'))
    return 0 if ok else 1


if __name__ == '__main__':
    sys.exit(main(sys.argv[1:]))
