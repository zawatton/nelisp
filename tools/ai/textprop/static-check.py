#!/usr/bin/env python3
"""static-check.py: no callback walker (nelisp--tp-walk with a lambda) remains in the text-property engine except in
the cold printer helpers, and the lane's native rows are well formed (ids 200-229 unique, in the data file)."""
import re, sys
P = open('scripts/nelisp-stdlib-prelude.el').read()
# split into top-level forms by "\n(" at column 0
forms = re.split(r'\n(?=\()', P)
users = set()
for f in forms:
    m = re.match(r'\((?:defun|defmacro) (\S+)', f) or re.match(r'\(unless \(fboundp \'(\S+)\)\s*\(defun', f)
    if not m: continue
    name = m.group(1)
    if name in ('nelisp--tp-walk', 'nelisp--tp-walk-back'): continue
    if '(nelisp--tp-walk' in f: users.add(name)
allowed = {'nelisp--prn-charset-mode', 'nelisp--prn-propertized'}
print('walk users:', sorted(users))
ok = users <= allowed
D = open('lisp/nelisp-fxir-native-data.el').read()
ids = [int(x) for x in re.findall(r'^    \([^ ]+ (\d+) ', D, re.M)]
new = [i for i in ids if 200 <= i <= 229]
print('rows', len(ids), 'in 200-229:', len(new))
ok = ok and len(set(ids)) == len(ids) and len(new) >= 20
sys.exit(0 if ok else 1)
