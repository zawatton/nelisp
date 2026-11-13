#!/usr/bin/env bash
# process-owner-inventory.sh --- who defines the standard process functions
#
# Scans scripts/nelisp-stdlib-prelude.el, lisp/*process*.el and
# packages/emacs-lib-*/src/*.el for definers of the standard process
# functions (list below).
#
# Rule (mechanical, text based).  A DEFINER of NAME is a `defun', `defalias',
# `fset', `defsubst' or `cl-defun' form naming NAME (quoted, `#'' optional)
# anywhere in a top-level form.  The definer is GUARDED when its top-level
# form, or an enclosing less-indented form on an earlier line, is headed by
# `(unless (fboundp 'NAME) ...)' or by an install gate that names NAME
# (`(when (...install-function-p 'NAME) ...)'); a guarded definer
# only yields to an existing definition.  Every other definer is
# UNCONDITIONAL.  A name is DUPLICATED when it has more than one unconditional
# definer.
# Usage: --list   TSV rows `NAME<TAB>FILE:LINE<TAB>unconditional|guarded'
#        --count  number of DUPLICATED names (0 means one owner each)
#        --dups   the duplicated names with their definers
set -uo pipefail
root=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/../.." && pwd)
cd "$root" || exit 2
mode=${1:---list}
python3 -I - "$mode" <<'PY'
import re, sys, glob
mode = sys.argv[1]
names = """make-process start-process start-file-process make-pipe-process process-filter
set-process-filter process-sentinel set-process-sentinel process-buffer set-process-buffer
process-status process-exit-status process-live-p process-name process-command process-mark
process-id process-list get-process processp accept-process-output
internal-default-process-filter internal-default-process-sentinel delete-process kill-process
process-send-string process-send-region process-send-eof process-get process-put
process-contact process-type""".split()
files = (["scripts/nelisp-stdlib-prelude.el"] + sorted(glob.glob("lisp/*process*.el"))
         + sorted(glob.glob("packages/emacs-lib-*/src/*.el")))
rows = []
for f in files:
    try: lines = open(f, encoding="utf-8", errors="replace").read().split("\n")
    except OSError: continue
    forms, cur, start = [], [], 0
    for i, l in enumerate(lines):
        if l.startswith("(") :
            if cur: forms.append((start, cur))
            cur, start = [l], i
        else: cur.append(l)
    if cur: forms.append((start, cur))
    for start, body in forms:
        head = body[0]
        for n in names:
            pat = r"\((?:defun|defalias|fset|defsubst|cl-defun)\s+'?(?:#')?" + re.escape(n) + r"[\s)]"
            for k, l in enumerate(body):
                if re.search(pat, l) and not l.lstrip().startswith(";"):
                    qn = re.escape(n)
                    gpat = (r"\((?:unless|when)\s+(?:\(not\s+)?\(fboundp\s+'" + qn + r"\)",
                            r"\(when\s+\([\w-]*install-function-p\s+'" + qn + r"\)")
                    ind = len(l) - len(l.lstrip())
                    nested = any(len(b) - len(b.lstrip()) < ind and any(re.match(g, b.lstrip()) for g in gpat)
                                 for b in body[:k])
                    guarded = nested or bool(re.match(r"\((?:unless|when)\s+(?:\(not\s+)?\(fboundp\s+'" + qn + r"\)", head)
                                   or re.match(r"\(when\s+\([\w-]*install-function-p\s+'" + qn + r"\)", head))
                    rows.append((n, f"{f}:{start+k+1}", "guarded" if guarded else "unconditional"))
# Table-driven definers: a `dolist' over a quoted table whose body does
# `(fset VAR ...)' or `(fset (car ENTRY) ...)' defines every name in the
# table (the car of a `(NAME . FN)' pair, or a bare symbol in the list).
# Renaming a defun and installing it through such a table does not remove
# a definer, so these count exactly like a direct `fset'.
for f in files:
    try: text = open(f, encoding="utf-8", errors="replace").read()
    except OSError: continue
    lines = text.split("\n")
    for i, l in enumerate(lines):
        if not re.search(r"\(dolist\s+\(\w+\s+'\(", l): continue
        j = i
        while j < len(lines) and j < i + 80 and not re.search(r"\(fset\s+(?:\(car\b|[a-z][\w-]*\s)", lines[j]):
            j += 1
        if j >= len(lines) or j >= i + 80: continue
        seg = "\n".join(lines[i:j])
        found = set(re.findall(r"\(([a-z][\w-]*)\s+\.", seg))
        bare = re.search(r"'\(([^()]*)", seg)
        if bare: found |= set(re.findall(r"[a-z][\w-]*", bare.group(1)))
        for n in names:
            if n in found:
                rows.append((n, f"{f}:{i+1}", "unconditional"))
by = {}
for n, loc, g in rows:
    if g == "unconditional": by.setdefault(n, []).append(loc)
dups = {n: v for n, v in by.items() if len(v) > 1}
if mode == "--count": print(len(dups))
elif mode == "--dups":
    for n, v in sorted(dups.items()): print(n + "\t" + " ".join(v))
else:
    for r in rows: print("\t".join(r))
PY
