#!/usr/bin/env python3
"""static-check.py: no callback walker (nelisp--tp-walk with a lambda) remains in the text-property engine except in
the cold printer helpers, and the lane's native rows are well formed (unique ids; the lane's 21 functions have rows)."""
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
rows = re.findall(r"^(?:    \(|  '\(\()([^ ]+) (\d+) ", D, re.M)
ids = [int(i) for _, i in rows]
# compall: the table is generated (tools/ai/compall-gen.el, ids from sorted names), so
# the lane's rows are checked by name: the 21 functions it harvested as ids 200-229.
LANE = ['nelisp--tp-key-listed', 'nelisp--tp-scan-fwd', 'nelisp--tp-scan-back', 'nelisp--tp-remove-props', 'nelisp--tp-add-props', 'nelisp--tp-build-range', 'nelisp--tp-build', 'nelisp--tp-plists-equal', 'nelisp--w202-property-find', 'nelisp--tp-copy-clipped', 'nelisp--tp-copy-buffer-range-to-string', 'nelisp--tp-sub-props', 'nelisp--tp-cat-props', 'nelisp--tp-strings-equal', 'nelisp--tp-graft', 'nelisp--tp-insert-nil', 'nelisp--tp-delete', 'nelisp--tp-grow', 'nelisp--tp-leftmost', 'nelisp-ov--char-property-and-overlay', 'next-single-char-property-change']
have = {n for n, _ in rows}
new = [n for n in LANE if n in have]
print('rows', len(ids), 'lane rows present:', len(new), 'of', len(LANE))
ok = ok and len(set(ids)) == len(ids) and len(new) == len(LANE)
sys.exit(0 if ok else 1)
