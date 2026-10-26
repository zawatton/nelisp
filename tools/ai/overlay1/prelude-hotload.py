#!/usr/bin/env python3
"""Make a file that re-defines parts of scripts/nelisp-stdlib-prelude.el inside an already built binary.

A prelude edit normally costs a 17-20 minute standalone build to try.  The built image resolves
functions by symbol, so loading new definitions over it (`BIN --load FILE --`) runs them at once:
overlay1 iterated ~60 times this way and used two builds, the second to confirm.

usage: prelude-hotload.py PRELUDE OUT [--region START_RE END_RE]... [--defun NAME]... [--defmacro NAME]...
  --region: copy the text from the first line matching START_RE up to (not including) the first
            later line matching END_RE (top-level forms; use for new structs, macros, defvars).
  --defun/--defmacro: copy that definition, even when it sits inside an `(unless (fboundp ..)' form.
Caveats: the binary's own compiled callers that were inlined or called directly are not replaced;
definitions that need a newer macro than the image has (e.g. markgc's `nelisp-buffer--do-markers')
must be hot-loaded from the prelude the binary was built from, not from a newer one.
The output starts with a `lexical-binding: nil' cookie, like the prelude.
"""
import re, sys


def match_end(s, i):
    depth = 0; n = len(s); j = i
    while j < n:
        c = s[j]
        if c == '"':
            j += 1
            while s[j] != '"':
                if s[j] == '\\':
                    j += 1
                j += 1
        elif c == ';':
            while s[j] != '\n':
                j += 1
        elif c == '?':
            j += 1
            if s[j] == '\\':
                j += 1
        elif c == '\\':
            j += 1
        elif c in '([':
            depth += 1
        elif c in ')]':
            depth -= 1
            if depth == 0:
                return j + 1
        j += 1
    raise SystemExit('unbalanced form')


def main(argv):
    prelude, out = argv[0], argv[1]
    s = open(prelude).read()
    lines = s.split('\n')
    parts = [';;; -*- lexical-binding: nil -*-\n']
    i = 2
    while i < len(argv):
        a = argv[i]
        if a == '--region':
            start = next(k for k, l in enumerate(lines) if re.search(argv[i + 1], l))
            end = next(k for k in range(start + 1, len(lines)) if re.search(argv[i + 2], lines[k]))
            parts.append('\n'.join(lines[start:end]))
            i += 3
        elif a in ('--defun', '--defmacro'):
            kind = a[2:]
            m = re.search(r'^[ ]*\(' + kind + ' ' + re.escape(argv[i + 1]) + r'[ \n]', s, re.M)
            if not m:
                raise SystemExit('no %s %s' % (kind, argv[i + 1]))
            b = s.index('(', m.start())
            parts.append(s[b:match_end(s, b)])
            i += 2
        else:
            raise SystemExit('bad argument ' + a)
    open(out, 'w').write('\n'.join(parts) + '\n')


main(sys.argv[1:])
