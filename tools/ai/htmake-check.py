#!/usr/bin/env python3
"""htmake ledger checks.  Usage: htmake-check.py src FILE | pmc LOG | sem LOG | fix LOG | win LOG | build LOG"""
import re, sys

def defun(text, name):
    m = re.search(r'\(defun %s \(' % re.escape(name), text)
    if not m: sys.exit('no defun ' + name)
    i = m.start(); depth = 0
    for j in range(i, len(text)):
        depth += text[j] == '('
        depth -= text[j] == ')'
        if depth == 0: return text[i:j + 1]

mode, arg = sys.argv[1], sys.argv[2]
t = open(arg).read()
if mode == 'src':
    body = defun(t, 'nl_ht_make_w')
    bad = [n for n in ('nl_ht_write_eql_sym_w', 'nl_ht_write_marker_sym_w', 'nl_alloc_symbol') if n in body]
    print('nl_ht_make_w direct symbol constructors:', bad or 'none')
    sys.exit(1 if bad else 0)
if mode == 'pmc':
    d = {}
    for l in t.splitlines():
        m = re.match(r'(\S+) (\S+) (\d+) instr=(\d+)', l)
        if m: d.setdefault((m[1], m[2], int(m[3])), []).append(int(m[4]))
    def trip(k, e):
        ns = sorted(n for (a, b, n) in d if a == k and b == e)
        return (min(d[(k, e, ns[-1])]) - min(d[(k, e, ns[0])])) / (ns[-1] - ns[0])
    mk = (trip('r7-make-hash-table', 'base'), trip('r7-make-hash-table', 'cand'))
    print('make-hash-table inst/trip base %.1f cand %.1f' % mk)
    ok = mk[1] < mk[0]
    for k in ('r7-gethash-eq-fix', 'reg-sum-bc'):
        b, c = trip(k, 'base'), trip(k, 'cand'); print(k, 'base %.1f cand %.1f' % (b, c))
        ok = ok and abs(c - b) / b < 0.001
    sys.exit(0 if ok else 1)
if mode == 'sem':
    ok = 'SEM-DONE fail=0' in t and t.count('PASS ') >= 490 and 'FAIL ' not in t
    print('sem pass lines', t.count('PASS '), 'ok', ok); sys.exit(0 if ok else 1)
if mode == 'fix':
    ok = all(s in t for s in ('weak 24/24 PASS', 'GNU 56/56 PASS', 'AFRT 24/25; unchanged exit-hook-result-gc RED', 'buffer 21/21 PASS'))
    print('fixtures ok', ok); sys.exit(0 if ok else 1)
if mode == 'win':
    ok = 'closurevm: PASS' in t and 'switch corpus: PASS' in t and 'features: PASS' in t and 'rc=0' in t
    print('wincheck ok', ok); sys.exit(0 if ok else 1)
if mode == 'build':
    ok = 'BUILD rc=0' in t
    n = [int(x) for x in re.findall(r'\[root-evidence\] nodes=(\d+)', t)]
    print('build rc0', ok, 'max root-evidence nodes', max(n), '(bound 98304)')
    sys.exit(0 if ok and max(n) <= 98304 else 1)
sys.exit(2)
