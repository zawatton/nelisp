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


CUR = ROOT / 'target/gc-port/cur'
CONS_BLOCK_SIZE = 1356


def binaries():
    """The candidate dynamic and static binaries (env overrides)."""
    return [Path(os.environ.get('GC_PORT_DYN', CUR / 'nelisp-dyn')),
            Path(os.environ.get('GC_PORT_STATIC', CUR / 'nelisp-static'))]


def run_nelisp(binary, path, timeout=300):
    import subprocess
    cmd = ['bash', '-c', 'ulimit -v 8388608; exec timeout "$0" "$1" --load "$2"',
           str(timeout), str(binary), str(path)]
    proc = subprocess.run(cmd, cwd=ROOT, capture_output=True, text=True)
    return proc.returncode, proc.stdout, proc.stderr


def cons_probe_problems(binary):
    rc, out, err = run_nelisp(binary, ROOT / 'tools/ai/gc-port-cons-probe.el')
    m = re.search(r'^GCPORT-CONS (.*)$', out, re.M)
    if rc != 0 or not m or 'uncaught error' in err:
        return [f'{binary.name}: probe rc={rc}']
    v = {k: int(x) for k, x in (kv.split('=') for kv in m.group(1).split())}
    problems = []
    if min(v.values()) < 0:
        problems.append('no cons-block fields in nelisp--arena-stats')
    if v['sum3'] != 1249975000 or v['len3'] != 50000:
        problems.append('wrong list contents')
    if v['live1'] - v['live0'] < 100000:
        problems.append(f"sweep counted {v['live1'] - v['live0']} new live conses, not 100000")
    if v['blocks1'] * CONS_BLOCK_SIZE < v['live1']:
        problems.append('more live conses than block cells')
    if v['live1'] - v['live2'] < 99000 or v['free2'] <= 0:
        problems.append(f"drop freed {v['live1'] - v['live2']} (free list {v['free2']})")
    if v['blocks2'] >= v['blocks1']:
        problems.append('sweep_conses released no wholly free block')
    if v['blocks3'] > v['blocks1'] or v['live3'] - v['live2'] < 50000:
        problems.append('second allocation grew past the earlier peak')
    return problems


def cmd_conses():
    src = (ROOT / 'scripts/nelisp-standalone-build.el').read_text()
    alloc = (ROOT / 'lisp/nelisp-cc-nlconsbox-alloc.el').read_text()
    problems = []
    for name in ('nl_cons_alloc', 'nl_sweep_conses', 'nl_cons_mark_cell',
                 'nl_live_cons_holding', 'nl_gc_mark_cons_payload', 'nl_ablock_alloc',
                 'nl_typed_find'):
        if f'(defun {name} ' not in src:
            problems.append(f'source lacks {name}')
    if 'extern-call nl_cons_alloc' not in alloc:
        problems.append('nl_alloc_consbox does not allocate from cons blocks')
    m = re.search(r'\(defun nl_gc_mark_cons_payload \(box\)\n(.*?)\n      ;;', src, re.S)
    if not m or 'nl_gc_object_start_p' in m.group(1) or '(nl_cons_mark_cell box)' not in m.group(1):
        problems.append('cons-block marking still probes the start index')
    if '(nl_sweep_conses)' not in src:
        problems.append('no per-type cons sweep in the collector')
    for binary in binaries():
        if not binary.exists():
            problems.append(f'missing binary {binary}')
            continue
        cold = Path(str(binary.resolve()) + '.cold')
        if not cold.exists() or cold.stat().st_nlink != 1:
            problems.append(f'{binary.name}: no single-link cold image companion')
        problems += cons_probe_problems(binary)
        for test in ('test/standalone-gc-pressure-check.el',
                     'test/standalone-gc-conservative-cost-check.el'):
            rc, out, err = run_nelisp(binary, ROOT / test)
            if rc != 0 or 'PASS' not in out or 'uncaught error' in err:
                problems.append(f'{binary.name}: {test} rc={rc}')
    base = Path(os.environ.get('GC_PORT_BASE', ROOT / 'target/gc-port/base/nelisp-dyn'))
    if not base.exists() or not cons_probe_problems(base):
        problems.append(f'negative control: base binary {base} passed the probe or is missing')
    if problems:
        return fail('conses: ' + '; '.join(problems[:8]))
    print('PASS conses: cons-block allocation, side marks, sweep_conses and free-list reuse '
          'on dynamic and static; GC pressure and conservative checks pass; base binary rejected')
    return 0


SPEED = ROOT / 'target/gc-port/speed'
LAYOUTS = (0, 700, 1400, 2100)


def speed_means(kind, binary):
    """Four-layout mean GC+alloc Ir for GNU and NeLisp, or a problem string."""
    import hashlib
    import json
    digest = hashlib.sha256(binary.read_bytes()).hexdigest() if binary.exists() else None
    means = {}
    for engine in ('gnu', 'nelisp'):
        values = []
        for layout in LAYOUTS:
            path = SPEED / f'{engine}-{kind}-{layout}' / 'summary.json'
            if not path.exists():
                return None, f'missing {path.relative_to(ROOT)}'
            meta = json.loads(path.read_text())
            if engine == 'nelisp' and meta['binary_sha256'] != digest:
                return None, f'{path.relative_to(ROOT)} profiles another binary'
            if not meta.get('output_ok') or meta.get('returncode') != 0:
                return None, f'{path.relative_to(ROOT)} failed'
            if engine == 'nelisp' and not meta.get('image_loaded'):
                return None, f'{path.relative_to(ROOT)} booted the prelude, not the cold image'
            values.append(meta['gc_alloc_ir'])
        means[engine] = sum(values) / len(values)
    return means, None


def speed_verdict(means):
    return means['nelisp'] <= means['gnu']


def cmd_speed(kind):
    binary = binaries()[0]
    if speed_verdict({'nelisp': 2.0, 'gnu': 1.0}):
        return fail('speed: negative control passed')
    means, problem = speed_means(kind, binary)
    if problem:
        return fail(f'speed {kind}: {problem}')
    ratio = means['nelisp'] / means['gnu']
    line = (f"speed {kind}: NeLisp GC+alloc {means['nelisp']/1e6:.1f}M Ir vs GNU "
            f"{means['gnu']/1e6:.1f}M Ir (x{ratio:.2f}, four-layout means)")
    if not speed_verdict(means):
        return fail(line)
    print('PASS ' + line)
    return 0


# S1.2: each type's GNU block structure, as source evidence.  Floats,
# symbols and string compaction are blocked by word-heap layers (Doc 223);
# vectors are the next increment.
TYPE_EVIDENCE = {
    'cons blocks (cons_block, sweep_conses)': ('nl_cons_alloc', 'nl_sweep_conses'),
    'float blocks (float_block, sweep_floats)': ('nl_float_alloc', 'nl_sweep_floats'),
    'symbol blocks (symbol_block, sweep_symbols)': ('nl_symbol_alloc', 'nl_sweep_symbols'),
    'string headers and sdata (sweep_strings, compact_small_strings)':
        ('nl_string_alloc', 'nl_compact_small_strings'),
    'vector blocks and large vectors (sweep_vectors)': ('nl_vector_alloc', 'nl_sweep_vectors'),
    'persistent block metadata (mem_find, live_*_p)': ('nl_typed_find', 'nl_live_cons_holding'),
}


def all_types_missing(src):
    return [name for name, defs in TYPE_EVIDENCE.items()
            if not all(f'(defun {d} ' in src for d in defs)]


def cmd_all_types():
    src = (ROOT / 'scripts/nelisp-standalone-build.el').read_text()
    if not all_types_missing(''):
        return fail('all-types: negative control passed')
    missing = all_types_missing(src)
    if missing:
        return fail('all-types: not ported: ' + '; '.join(missing))
    print('PASS all-types: every GNU block type has its allocator and sweep')
    return 0


def company_verdict(med):
    return med['candidate'] <= 0.67 * med['base']


def cmd_company():
    """S2.4 (COMMON Measurement): company workload wall-clock at most 0.67x
    the base's, the wall-clock equivalent of cutting GC plus allocation from
    42.79% to 15% with the rest unchanged (0.5721 / 0.85)."""
    import json
    if company_verdict({'candidate': 1.0, 'base': 1.0}):
        return fail('company: negative control passed')
    path = ROOT / 'target/gc-port/company/wall.json'
    if not path.exists():
        return fail('company: wall-clock not measured on this branch (the (require '
                    "'company) workload runs on a nelisp-emacs-lib library image, "
                    'rebuilt for this binary with tools/c-core-image.sh)')
    med = json.loads(path.read_text())['median']
    line = (f"company: {med['candidate']:.3f}s vs base {med['base']:.3f}s "
            f"(x{med['candidate'] / med['base']:.2f}, bar 0.67)")
    if not company_verdict(med):
        return fail(line)
    print('PASS ' + line)
    return 0


def final_problems(out, dyn, st):
    import hashlib
    import json
    problems = []
    shas = (out / 'binaries.sha256').read_text() if (out / 'binaries.sha256').exists() else ''
    for b in (dyn, st):
        if not b.exists() or hashlib.sha256(b.read_bytes()).hexdigest() not in shas:
            problems.append(f'{b.name} is not the verified binary')
    log = (out / 'checks.log').read_text() if (out / 'checks.log').exists() else ''
    if 'GATE-COUNT: 11 cases, 0 failures' not in log:
        problems.append('checks.sh did not report 11 cases with 0 failures')
    if 'STANDALONE-CHECKS PASS' not in log and 'STANDALONE-CHECK FAIL' in log:
        problems.append('standalone-checks failed')
    mcp = out / 'mcp/mcp-result.json'
    if not mcp.exists() or json.loads(mcp.read_text()).get('tool_count') != 40:
        problems.append('MCP smoke did not list 40 tools')
    wh = (out / 'word-heap.log').read_text() if (out / 'word-heap.log').exists() else ''
    if 'WORD-HEAP-BINARY-CHECKS PASS' not in wh:
        problems.append('word-heap binary criteria did not pass')
    return problems


def cmd_final():
    out = ROOT / 'target/gc-port' / os.environ.get('GC_PORT_FINAL_DIR', 'final')
    dyn, st = binaries()
    if not final_problems(ROOT / 'target/gc-port/no-such-final', dyn, st):
        return fail('final: negative control passed')
    problems = final_problems(out, dyn, st)
    if problems:
        return fail('final: ' + '; '.join(problems))
    print('PASS final: checks.sh 11 groups 0 failures, standalone-checks, MCP 40 tools '
          'and the word-heap binary criteria on the verified binaries')
    return 0


WALL = ROOT / 'target/gc-port/wall/final.json'


def wall_verdict(med):
    return med['candidate'] <= med['gnu']


def cmd_wall(kind):
    """S2.1-S2.3 (COMMON Measurement): median wall-clock of 7 alternating runs,
    GC included, against GNU 31.1 on the same Lisp workload."""
    import hashlib
    import json
    if wall_verdict({'candidate': 2.0, 'gnu': 1.0}):
        return fail('wall: negative control passed')
    if not WALL.exists():
        return fail(f'wall {kind}: no measurement at {WALL.relative_to(ROOT)}')
    data = json.loads(WALL.read_text())
    dyn = binaries()[0]
    if not dyn.exists() or data['sha256']['candidate'] != hashlib.sha256(dyn.read_bytes()).hexdigest():
        return fail(f'wall {kind}: measured binary is not {dyn}')
    w = data['workloads'].get(kind)
    if not w or w['failures'] or data['rounds'] < 7:
        return fail(f'wall {kind}: incomplete or failing runs')
    med = w['median']
    line = (f"wall {kind}: NeLisp {med['candidate']:.3f}s vs GNU {med['gnu']:.3f}s "
            f"(x{med['candidate'] / med['gnu']:.2f}; base {med['base']:.3f}s), "
            f"median of {data['rounds']}, GC included")
    if not wall_verdict(med):
        return fail(line)
    print('PASS ' + line)
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
    if cmd == 'conses':
        return cmd_conses()
    if cmd == 'all-types':
        return cmd_all_types()
    if cmd == 'company':
        return cmd_company()
    if cmd == 'final':
        return cmd_final()
    if cmd == 'speed' and len(argv) == 2 and argv[1] in ('cons', 'string', 'vector'):
        return cmd_speed(argv[1])
    if cmd == 'wall' and len(argv) == 2 and argv[1] in ('cons', 'string', 'vector'):
        return cmd_wall(argv[1])
    print('usage: gc-port-check.py portplan|conses|all-types|speed KIND|wall KIND|company|final')
    return 2


if __name__ == '__main__':
    sys.exit(main(sys.argv[1:]))
