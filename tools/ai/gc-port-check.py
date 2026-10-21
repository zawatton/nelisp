#!/usr/bin/env python3
"""Checks for the GC port ledger (tools/ai/gc-port-progress.org, Doc 223).

Each subcommand prints one verdict line and exits 0 only on PASS.  Every
check also runs one negative control: the same verification applied to a
deliberately broken input must fail, or the check itself fails.
"""
import os
import re
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
DOC = ROOT / 'docs/design/223-gnu-alloc-port.org'
GNU = Path(os.environ.get('GNU_EMACS_SRC', '/home/madblack-21/Downloads/emacs-31.1/src'))

# Ledger S0.1: the mechanisms the port plan must map, each with a pattern
# that must appear in the first cell of its table row.
MECHANISMS = [
    ('block types', r'block types'),
    ('mark bits', r'mark bits'),
    ('free lists', r'free lists'),
    ('sweep_conses', r'sweep_conses'),
    ('sweep_floats', r'sweep_floats'),
    ('sweep_symbols', r'sweep_symbols'),
    ('sweep_strings with compact_small_strings', r'sweep_strings.*compact_small_strings'),
    ('sweep_vectors', r'sweep_vectors'),
    ('mem_node tree with mem_find and live_*_p', r'mem_node.*mem_find.*live_'),
    ('mark_memory', r'mark_memory'),
    ('GC threshold', r'gc threshold'),
]
STATUS = re.compile(r'\b(ported|adapted|impossible)\b')
CITE = re.compile(r'\b(alloc\.c|pdumper\.c|lisp\.h):(\d+)\s*=([A-Za-z_][A-Za-z0-9_]*)=')


def fail(msg):
    print('FAIL ' + msg)
    return 1


def portplan_problems(text):
    """Return a list of problems with the Doc 223 text."""
    problems = []
    rows = [line for line in text.splitlines() if line.startswith('|')]
    for name, pattern in MECHANISMS:
        hits = [r for r in rows
                if re.search(pattern, r.split('|')[1], re.I)]
        if not hits:
            problems.append(f'no table row for {name}')
            continue
        row = hits[0]
        cells = row.split('|')
        if len(cells) < 6:
            problems.append(f'{name}: row has fewer than five columns')
            continue
        if not STATUS.search(cells[4]):
            problems.append(f'{name}: no ported/adapted/impossible status')
        if 'impossible' in cells[4] and not re.search(r'block|S[0-9]', cells[4]):
            problems.append(f'{name}: impossible without a blocking layer')
        if not CITE.search(cells[2]):
            problems.append(f'{name}: no GNU file:line =identifier= citation')
        if not cells[5].strip():
            problems.append(f'{name}: no Doc 216 interaction')
    for heading in ('* Interaction with the word heap', '* Implementation order'):
        if heading not in text:
            problems.append(f'missing section {heading!r}')
    # Every citation must name an identifier that occurs on the cited line
    # or within the next four lines of the GNU 31.1 source.
    cache = {}
    for match in CITE.finditer(text):
        fname, line, ident = match.group(1), int(match.group(2)), match.group(3)
        if fname not in cache:
            path = GNU / fname
            if not path.exists():
                problems.append(f'GNU source missing: {path}')
                cache[fname] = []
                continue
            cache[fname] = path.read_text(errors='replace').split('\n')
        lines = cache[fname]
        window = '\n'.join(lines[line - 1:line + 4]) if 0 < line <= len(lines) else ''
        if ident not in window:
            problems.append(f'{fname}:{line} does not mention {ident}')
    return problems


def cmd_portplan():
    if not DOC.exists():
        return fail(f'portplan: {DOC} missing')
    text = DOC.read_text()
    problems = portplan_problems(text)
    # Negative control: drop the sweep_vectors status and corrupt one cite.
    broken = re.sub(r'(\| sweep_vectors[^\n]*?\|[^|]*\|[^|]*\|)\s*adapted', r'\1 ', text, count=1)
    broken = broken.replace('alloc.c:6814', 'alloc.c:6815', 1)
    if not portplan_problems(broken):
        return fail('portplan: negative control passed (checker cannot fail)')
    if problems:
        return fail('portplan: ' + '; '.join(problems[:6]))
    print(f'PASS portplan: {len(MECHANISMS)} mechanisms mapped, '
          f'{len(CITE.findall(text))} GNU citations verified, negative control rejected')
    return 0


def not_ready(name):
    return fail(f'{name}: implementation or acceptance evidence outstanding')


def main(argv):
    if not argv:
        print(__doc__)
        return 2
    cmd = argv[0]
    if cmd == 'portplan':
        return cmd_portplan()
    if cmd in ('conses', 'all-types', 'company', 'final'):
        return not_ready(cmd)
    if cmd == 'speed' and len(argv) == 2 and argv[1] in ('cons', 'string', 'vector'):
        return not_ready('speed ' + argv[1])
    print('usage: gc-port-check.py portplan|conses|all-types|speed KIND|company|final')
    return 2


if __name__ == '__main__':
    sys.exit(main(sys.argv[1:]))
