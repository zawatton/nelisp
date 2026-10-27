#!/usr/bin/env python3
"""lane-ledger-bincheck.py: flag lane-ledger checks that hard-code a NeLisp binary.

A lane ledger is re-metered by the coordinator against the integrated build,
so every cmd: criterion must take the binary under test from an environment
variable (e.g. `$NC4_BIN` or `${NC4_BIN:-target/nc4-build/dynamic/nelisp-dynamic}`),
never from a literal lane build path.  A literal path silently measures the
lane's own build, and turns red once that build is pruned.

Usage: lane-ledger-bincheck.py LEDGER.org [...]
Exit 0 when no cmd: line names a literal binary, 1 otherwise (offenders are
printed as FILE:LINE: TEXT), 2 on usage errors.
"""
import re
import sys

# Paths that name a NeLisp executable: lane/integ builds, target/nelisp*, .exe.
BIN = re.compile(r'[\w./${}:-]*(?:-build/dynamic/nelisp-dynamic|/nelisp-dynamic\b'
                 r'|\btarget/nelisp(?:-[\w.-]+)?\b(?!-)|nelisp(?:-[\w.-]+)?\.exe\b)')
# ${VAR} and ${VAR:-default} spans: the default is an override-able fallback.
PARAM = re.compile(r'\$\{[A-Za-z_][A-Za-z0-9_]*(?::?[-=][^}]*)?\}')


def offenders(path):
    out = []
    with open(path, encoding='utf-8') as f:
        for n, line in enumerate(f, 1):
            s = line.strip()
            if not s.startswith('cmd:'):
                continue
            if BIN.search(PARAM.sub('', s[4:])):
                out.append((n, s))
    return out


def main(argv):
    if not argv:
        print(__doc__.strip(), file=sys.stderr)
        return 2
    bad = 0
    for p in argv:
        for n, s in offenders(p):
            bad += 1
            print(f'{p}:{n}: {s[:200]}')
    print(f'lane-ledger-bincheck: {bad} cmd line(s) with a literal binary path'
          f' in {len(argv)} ledger(s)')
    return 1 if bad else 0


if __name__ == '__main__':
    sys.exit(main(sys.argv[1:]))
