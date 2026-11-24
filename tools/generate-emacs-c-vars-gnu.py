#!/usr/bin/env python3
"""Generate emacs-c-vars-gnu.el from GNU Emacs 31.1 C sources (syms_of_* initial values).

Usage: tools/generate-emacs-c-vars-gnu.py GNU-31.1-SRC-DIR OUT.el
GNU-31.1-SRC-DIR is the src/ directory of an Emacs 31.1 source tree."""
import re, sys, glob, os
SRC = sys.argv[1]; OUT = sys.argv[2]
files = sorted(glob.glob(os.path.join(SRC, '*.c')))
text = {f: open(f, encoding='utf-8', errors='replace').read() for f in files}
# DEFSYM (Qfoo, "foo")
qsym = {}
for t in text.values():
    for m in re.finditer(r'DEFSYM\s*\(\s*(Q\w+)\s*,\s*"([^"]*)"\s*\)', t):
        qsym[m.group(1)] = m.group(2)
def cstr(s):
    out = s.encode('utf-8').decode('unicode_escape').encode('latin-1', 'replace').decode('utf-8', 'replace')
    return '"' + out.replace('\\', '\\\\').replace('"', '\\"').replace('\n', '\\n') + '"'
def split_args(s):
    args, depth, cur, q = [], 0, '', False
    for ch in s:
        if ch == '"' and (not cur.endswith('\\')): q = not q
        if not q and ch == '(': depth += 1
        if not q and ch == ')': depth -= 1
        if not q and ch == ',' and depth == 0: args.append(cur.strip()); cur = ''; continue
        cur += ch
    if cur.strip(): args.append(cur.strip())
    return args
def conv(e, kind):
    e = e.strip()
    e = re.sub(r'^\((.*)\)$', r'\1', e)
    if kind == 'BOOL':
        if e in ('true', '1'): return 't'
        if e in ('false', '0'): return 'nil'
        return None
    if kind == 'INT':
        if re.fullmatch(r'-?\d+', e): return e
        return None
    if e == 'Qnil': return 'nil'
    if e == 'Qt': return 't'
    m = re.fullmatch(r'make_(?:fixnum|int)\s*\(\s*(-?\d+)\s*\)', e)
    if m: return m.group(1)
    m = re.fullmatch(r'make_float\s*\(\s*(-?[\d.]+)\s*\)', e)
    if m: return m.group(1) if '.' in m.group(1) else m.group(1) + '.0'
    m = re.fullmatch(r'(?:build_string|build_pure_c_string|build_unibyte_string|make_pure_c_string)\s*\(\s*("(?:[^"\\]|\\.)*")\s*(?:,\s*\d+\s*)?\)', e)
    if m: return cstr(m.group(1)[1:-1])
    if re.fullmatch(r'Q\w+', e) and e in qsym: return "'" + qsym[e]
    m = re.fullmatch(r'intern(?:_c_string)?\s*\(\s*"([^"]*)"\s*\)', e)
    if m: return "'" + m.group(1)
    m = re.fullmatch(r'Fmake_char_table\s*\(\s*(Q\w+)\s*,\s*(.+)\)', e, re.S)
    if m:
        sub = 'nil' if m.group(1) == 'Qnil' else ("'" + qsym[m.group(1)] if m.group(1) in qsym else None)
        init = conv(m.group(2), 'LISP')
        if sub is not None and init is not None:
            return '(make-char-table ' + sub + ' ' + init + ')'
    m = re.fullmatch(r'list(\d)\s*\((.*)\)', e, re.S)
    if m:
        items = [conv(a, 'LISP') for a in split_args(m.group(2))]
        if all(i is not None for i in items):
            return "'(" + ' '.join(i.lstrip("'") for i in items) + ")"
    return None
ok, skipped = [], []
# Read-only in GNU (make_symbol_constant): never assign them.
constants = set()
for t in text.values():
    constants.update(re.findall(r'make_symbol_constant\s*\(\s*intern_c_string\s*\(\s*"([^"]+)"', t))
defvar_re = re.compile(r'DEFVAR_(LISP|LISP_NOPRO|INT|BOOL|PER_BUFFER|KBOARD)\s*\(\s*"([^"]+)"\s*,\s*([^,]+?)\s*,', re.S)
buf = text.get(os.path.join(SRC, 'buffer.c'), '')
for f, t in text.items():
    for m in defvar_re.finditer(t):
        kind, name, cvar = m.group(1), m.group(2), m.group(3).strip().lstrip('&')
        k = 'LISP' if kind.startswith('LISP') else kind
        if name in constants: skipped.append((name, 'read-only constant in GNU')); continue
        if kind == 'KBOARD': skipped.append((name, 'kboard-local')); continue
        if kind == 'PER_BUFFER':
            field = re.sub(r'.*\(\s*\w+\s*,\s*(\w+)\s*\)', r'\1', cvar) if 'BVAR' in cvar else cvar
            mm = re.search(r'bset_' + re.escape(field) + r'\s*\(\s*&buffer_defaults\s*,\s*(.+?)\);', buf, re.S) \
                 or re.search(r'XSETFASTINT\s*\(\s*BVAR\s*\(\s*&buffer_defaults\s*,\s*' + re.escape(field) + r'\s*\)\s*,\s*(\d+)\s*\)', buf)
            if not mm: v = 'nil'
            else:
                v = mm.group(1) if mm.group(1).isdigit() else conv(mm.group(1), 'LISP')
            if v is None: skipped.append((name, 'buffer default not a literal')); continue
            ok.append((name, v, True)); continue
        # function body: from DEFVAR to the end of the enclosing function
        end = t.find('\n}', m.end())
        body = t[m.end(): end if end > 0 else len(t)]
        am = re.search(r'(?<![\w.>])' + re.escape(cvar) + r'\s*=\s*(.+?);', body, re.S)
        if not am:
            v = {'LISP': 'nil', 'INT': '0', 'BOOL': 'nil'}[k]
        else:
            v = conv(am.group(1), k)
            if v is None: skipped.append((name, 'initial value computed in C: ' + ' '.join(am.group(1).split())[:60])); continue
        ok.append((name, v, False))
seen = set(); res = []
for n, v, bl in sorted(ok):
    if n in seen: continue
    seen.add(n); res.append((n, v, bl))
with open(OUT, 'w') as o:
    o.write(";;; emacs-c-vars-gnu.el --- GNU 31.1 C-level variables and their initial values  -*- lexical-binding: t; -*-\n"
            ";; GENERATED by scripts/generate-emacs-c-vars-gnu.py from the GNU Emacs 31.1 C sources\n"
            ";; (src/*.c DEFVAR_* and their syms_of_* initial values; DEFVAR_PER_BUFFER defaults from\n"
            ";; buffer.c's buffer_defaults).  Do not edit by hand.  Each variable gets GNU's C initial\n"
            ";; value unless the NeLisp substrate already binds it; Lisp files set the rest as in GNU.\n\n")
    for n, v, bl in res:
        en = re.sub(r'([^A-Za-z0-9+*/_<>=:!?.%&~^@-])', r'\\\1', n); o.write(f"(unless (boundp '{en}) (condition-case nil (defvar {en} {v}) (error nil)))\n")
        if bl: o.write(f"(make-variable-buffer-local '{en})\n")
    o.write("\n;; Skipped (value computed at run time in C, or kboard-local):\n")
    for n, why in sorted(set(skipped)):
        if n not in seen: o.write(f";;   {n}: {why}\n")
    o.write("\n(provide 'emacs-c-vars-gnu)\n;;; emacs-c-vars-gnu.el ends here\n")
print(f"vars={len(res)} skipped={len(set(n for n,_ in skipped) - seen)} buffer-local={sum(1 for r in res if r[2])}")
