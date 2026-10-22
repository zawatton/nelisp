#!/usr/bin/env python3
"""Check the build's box-census walker (nelisp-standalone--box-census-record).

  box-census-walker-check.py check [--script PATH]

The census counts call heads (nl_root_reserve, wf_copy32, ...) per defun while
the build compiles each unit.  Its walk once visited the cdr of every list but
never the car, so a call head in the first binding of a let or let*, or in a
cond clause's test, was not counted (2026-10-22: nl_let_setup and
wf_numeric_marker_copy_w each held a nl_root_reserve site the census could not
see).  This check extracts the census function from the build script, runs it
in a batch Emacs on a synthetic unit and compares the rows with the true
counts.  A negative control removes the walker's car visit from the extracted
text and must produce wrong counts, so the check cannot pass on a walker that
still skips the car.
"""
import os
import re
import subprocess
import sys
import tempfile
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
SCRIPT = ROOT / 'scripts/nelisp-standalone-build.el'
CAP = 8 << 30

# Forms the census function needs, in definition order.
FORMS = [r'\(defconst nelisp-standalone--box-census-idioms',
         r'\(defun nelisp-standalone--box-dump-json',
         r'\(defun nelisp-standalone--box-dump-record',
         r'\(defun nelisp-standalone--box-census-record']

# One synthetic unit; every call head sits where the old walker did not look.
UNIT = '''(seq
 (defun ctl_first_binding (env)
   (let* ((a (nl_root_reserve env)) (b (nl_root_reserve env))) a))
 (defun ctl_second_binding (env)
   (let* ((x 1) (a (nl_root_reserve env))) a))
 (defun ctl_let_first (env)
   (let ((a (nl_root_reserve env))) a))
 (defun ctl_cond_test (env)
   (cond ((= (wf_copy32 env env) 1) 2) (t 3)))
 (defun ctl_call_arg (env)
   (nl_foo (nl_root_reserve env) (alloc-bytes 32 8))))'''

# (defun, idiom) -> count the census must report.
EXPECTED = {
    ('ctl_first_binding', 'nl_root_reserve'): 2,
    ('ctl_second_binding', 'nl_root_reserve'): 1,
    ('ctl_let_first', 'nl_root_reserve'): 1,
    ('ctl_cond_test', 'wf_copy32'): 1,
    ('ctl_call_arg', 'nl_root_reserve'): 1,
    ('ctl_call_arg', 'alloc-bytes-32-8'): 1,
}


def form_end(s, i):
    """Index just past the form starting at s[i] == '('."""
    d, n = 0, len(s)
    while i < n:
        c = s[i]
        if c == ';':
            j = s.find('\n', i)
            i = n if j < 0 else j
            continue
        if c == '"':
            i += 1
            while i < n and s[i] != '"':
                i += 2 if s[i] == '\\' else 1
            i += 1
            continue
        if c == '?':
            i += 3 if i + 1 < n and s[i + 1] == '\\' else 2
            continue
        if c == '(':
            d += 1
        elif c == ')':
            d -= 1
            if d == 0:
                return i + 1
        i += 1
    raise ValueError('unbalanced form')


def extract(text):
    out = []
    for pat in FORMS:
        hits = [m.start() for m in re.finditer(pat + r'[\s)]', text)]
        if len(hits) != 1:
            raise ValueError(f'{pat}: {len(hits)} definitions')
        out.append(text[hits[0]:form_end(text, hits[0])])
    return out


def census(forms):
    """Rows (defun, idiom) -> count that the extracted census function writes."""
    with tempfile.TemporaryDirectory(prefix='boxwalk-', dir=ROOT / 'target' if (ROOT / 'target').is_dir() else None) as d:
        el, tsv = Path(d) / 'ctl.el', Path(d) / 'census.tsv'
        el.write_text(';;; -*- lexical-binding: t -*-\n(require (quote cl-lib))\n(require (quote subr-x))\n'
                      '(require (quote json))\n'
                      f'(defvar nelisp-standalone--repo-root "{ROOT}/")\n'
                      + '\n'.join(forms)
                      + f'\n(nelisp-standalone--box-census-record "ctl.o" (quote {UNIT}))\n')
        env = dict(os.environ, NELISP_BOX_CENSUS=str(tsv))
        env.pop('NELISP_BOX_DUMP', None)

        def capped():
            import resource
            resource.setrlimit(resource.RLIMIT_AS, (CAP, CAP))
        r = subprocess.run(['emacs', '--batch', '-Q', '-l', str(el)], env=env, cwd=ROOT,
                           capture_output=True, text=True, preexec_fn=capped, timeout=120)
        if r.returncode or not tsv.exists():
            raise RuntimeError(f'emacs rc={r.returncode}: {r.stderr[:300]}')
        rows = {}
        for line in tsv.read_text().splitlines():
            unit, fn, idiom, count = line.split('\t')
            if fn.startswith('ctl_'):
                rows[(fn, idiom)] = rows.get((fn, idiom), 0) + int(count)
        return rows


def wrong(rows):
    """The expected counts the census got wrong, as (key, wanted, got)."""
    return [(k, v, rows.get(k, 0)) for k, v in EXPECTED.items() if rows.get(k, 0) != v]


def old_walker(forms):
    """The extracted census function with the walker's visit of a list's car removed."""
    text = forms[-1]
    new = re.sub(r"\n\s*\(when \(consp \(car x\)\) \(walk \(car x\)\)\)", '', text)
    if new == text:
        raise ValueError('no car visit to remove: the control would test nothing')
    return forms[:-1] + [new]


def check(script):
    forms = extract(script.read_text())
    bad = wrong(census(forms))
    if bad:
        print('FAIL box-census walker misses call heads:', bad)
        return 1
    control = wrong(census(old_walker(forms)))
    if not control:
        print('FAIL box-census walker: the negative control (no car visit) was not rejected')
        return 1
    print(f'PASS box-census walker counts a call head in the first let binding, a second binding, '
          f'a let first binding, a cond test and a call argument ({len(EXPECTED)} rows); '
          f'the walker without the car visit gets {len(control)} of them wrong')
    return 0


def main(argv):
    if not argv or argv[0] != 'check':
        print(__doc__)
        return 2
    script = Path(argv[argv.index('--script') + 1]) if '--script' in argv else SCRIPT
    return check(script)


if __name__ == '__main__':
    sys.exit(main(sys.argv[1:]))
