#!/usr/bin/env python3
"""lexbc census: classify every adopted prelude/core/vendor byte-code function.

usage: census.py BIN [OUT]
Names come from census-names.tsv (NAME, SOURCE, ORIGIN), captured from the v26
build's byte-code reports plus the GENERATED nelisp-timefns block.  For each
name bound in BIN to byte code, the kind is `lex' (integer arglist template)
or `dyn' (arglist list).  A row is a defect when its SOURCE file declares
lexical-binding t and the function is `dyn'.  Exit 0 iff there is none.
"""
import os, re, subprocess, sys, tempfile, collections
HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.dirname(os.path.dirname(os.path.dirname(HERE)))
bin_ = os.path.realpath(sys.argv[1])
rows = [l.rstrip('\n').split('\t') for l in open(os.path.join(HERE, 'census-names.tsv'))]
def lexical(src):
    p = os.path.join(ROOT, src)
    first = open(p, encoding='utf-8', errors='replace').readline()
    m = re.search(r'lexical-binding: *([a-z]+)', first)
    return bool(m and m.group(1) != 'nil')
with tempfile.TemporaryDirectory() as d:
    el = os.path.join(d, 'probe.el')
    with open(el, 'w') as o:
        o.write('(dolist (n (quote (%s)))\n' % ' '.join(r[0] for r in rows))
        o.write('  (let ((f (and (fboundp n) (symbol-function n))))\n'
                '    (princ (format "%s\\t%s\\n" n (cond ((not f) "unbound")\n'
                '      ((byte-code-function-p f) (if (integerp (aref f 0)) "lex" "dyn")) (t "other"))))))\n')
    r = subprocess.run(['sh', '-c', 'ulimit -v 8388608; exec timeout 100 "$0" --load "$1"', bin_, el],
                       cwd=d, capture_output=True, text=True)
kind = {}
for l in r.stdout.splitlines():
    p = l.split('\t')
    if len(p) == 2: kind.setdefault(p[0], p[1])
lines, bad = [], []
c = collections.Counter()
for name, src, origin in rows:
    k = kind.get(name, 'missing')
    lx = lexical(src)
    c[(origin, src, 'lexical' if lx else 'dynamic', k)] += 1
    if lx and k == 'dyn': bad.append((name, src, origin))
    if not lx and k == 'dyn': lines.append('dynamic-source-dyn\t%s\t%s' % (name, src))
out = ['# lexbc census: rows = (origin, source, source cookie, runtime kind) -> count']
for key, v in sorted(c.items()): out.append('%d\t%s' % (v, '\t'.join(key)))
out += ['DEFECT\t%s\t%s\t%s' % b for b in bad]
out += lines
out.append('CENSUS names=%d dyn-lexical-source=%d dyn-dynamic-source=%d rc=%d'
           % (len(rows), len(bad), len(lines), r.returncode))
text = '\n'.join(out) + '\n'
if len(sys.argv) > 2: open(sys.argv[2], 'w').write(text)
print('\n'.join(out[-1:]))
sys.exit(0 if (not bad and r.returncode == 0 and kind) else 1)
