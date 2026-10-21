#!/usr/bin/env python3
"""Doc 216 checks for word-heap S2.1, S2.2, S5.1 and S6.2.

  word-heap-stage-check.py s2.1|s2.2|s5.1 --dyn BIN --static BIN [--control BASE]
                           [--gnu EMACS]
  word-heap-stage-check.py s6.2 --census CENSUS.tsv
  word-heap-stage-check.py controls --base BIN

Retained arena bytes per object come from fresh processes running
word-heap-objsize.el (individually retained objects) and
word-heap-cons-list.el (one retained make-list, so every cdr is a stored
cons reference).  Each slope is (B(4000) - B(2000)) / 2000 and is measured
on both linkages.

s2.1  header-less cons.  Both cons slopes are at most 32 B, the cons cell is
      at most 24 B, and the dumped image of each linkage has no cons header
      view (word-heap-census.py: cons-unrelocated-view allocations = 0).
s2.2  refcount removed.  The s2.1 checks with a 24 B slope and a 16 B cell
      limit.  No count reader or writer stays linked: in each ELF,
      nelisp_nlconsbox_clone, nl_alloc_consbox and nl_alloc_consbox_init
      contain no lock-prefixed instruction and no call to
      nl_atomic_fetch_add, nelisp_nlconsbox_drop is absent, and the
      constructor source writes only the car and cdr words.
s5.1  no object type allocates a 32 B header box.  The census of each
      linkage's dumped image counts no header alias for any object type
      and no view of an immediate.  Retained slopes stay within the S0
      revision: float <= 24 B, one-limb bignum <= 32 B, detached marker
      <= 40 B.  word-heap-numeric-parity.el prints the same lines on both
      linkages as on GNU Emacs.
s6.2  precise roots are WORD slots.  The build census (NELISP_BOX_CENSUS)
      counts no nl_root_reserve site outside the call path.  The call path
      (CALL_PATH below) belongs to word-heap S6.3.
Negative controls: with --control BASE, s2.1, s2.2 and s5.1 first run on
a binary that still has header boxes (the base) and fail unless its slopes
are rejected.  s6.2 always checks two synthetic censuses: a site outside
the call path must be rejected and a call-path site kept.  `controls'
runs all four controls.

Processes run under an 8 GiB address-space cap.  Exit 0 iff every check
passes; one summary line per check.
"""
import argparse
import re
import resource
import subprocess
import sys
import tempfile
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
OBJSIZE = ROOT / 'tools/ai/word-heap-objsize.el'
CONSLIST = ROOT / 'tools/ai/word-heap-cons-list.el'
PARITY = ROOT / 'tools/ai/word-heap-numeric-parity.el'
CENSUS = ROOT / 'tools/ai/word-heap-census.py'
BUILD = ROOT / 'scripts/nelisp-standalone-build.el'
CONSBOX_ALLOC = ROOT / 'lisp/nelisp-cc-nlconsbox-alloc.el'
CAP = 8 << 30

# word-heap S6.3 (callpath-perf): roots taken while a call is set up, run or
# unwound -- funcall/apply, eval of a call form, native frames and the whole
# byte-code VM (wf_bytecode_*, which callpath-perf owns).  Anything else is
# S6.2's.  Provisional (2026-10-21) until callpath-perf confirms it.
CALL_PATH = re.compile(
    r'^(nelisp_apply|nl_apply|bf_apply|nl_ali_|nl_funcall|nl_eic_|nl_n3_eval|'
    r'nl_n4_frame_eval|m5_fx_|nl_fx_|nl_native_frame|wf_bytecode_|'
    r'bf_n2_call_lisp|bf_n2_direct_arity)')

IMMEDIATE_VIEWS = ('integer-view', 'nil-view', 't-view')


def capped():
    resource.setrlimit(resource.RLIMIT_AS, (CAP, CAP))


def run(cmd, env=None, timeout=300):
    return subprocess.run(cmd, cwd=ROOT, env=env, capture_output=True, text=True,
                          errors='replace', timeout=timeout, preexec_fn=capped)


def population(binary, probe, kind, n):
    import os
    env = dict(os.environ, WH_TYPE=kind, WH_N=str(n))
    cmd = [str(binary), '--load', str(probe), '--eval', 'nil']
    r = run(cmd, env=env, timeout=180)
    if r.returncode or 'WH-OBJECT-DONE' not in r.stdout:
        raise RuntimeError(f'{binary.name} {probe.name} {kind} {n}: exit {r.returncode} {r.stderr[:120]!r}')
    m = re.search(r'^WH-(?:OBJECT \S+|LIST) \d+ (-?\d+)$', r.stdout, re.M)
    if not m:
        raise RuntimeError(f'{binary.name} {kind} {n}: no measurement')
    return int(m[1])


def slope(binary, probe, kind):
    return (population(binary, probe, kind, 4000) - population(binary, probe, kind, 2000)) / 2000


def cons_slopes(binary):
    return {'cons': slope(binary, OBJSIZE, 'cons'), 'list': slope(binary, CONSLIST, 'list')}


def cell_bytes():
    text = BUILD.read_text()
    cell = int(re.search(r'\(defconst nelisp-standalone--cons-cell-bytes (\d+)', text)[1])
    src = CONSBOX_ALLOC.read_text()
    fallback = [int(x) for x in re.findall(r'\(alloc-bytes (\d+) 8\)', src)]
    writes = [int(x) for x in re.findall(r'\(ptr-write-u64 box-ptr (\d+) ', src)]
    return cell, fallback, writes


def image_census(binary):
    import json
    cold = Path(str(binary) + '.cold')
    if not cold.is_file():
        raise RuntimeError(f'no image beside {binary}')
    with tempfile.TemporaryDirectory() as tmp:
        out = Path(tmp) / 'census.json'
        r = run([sys.executable, str(CENSUS), str(cold)], timeout=900)
        if r.returncode:
            raise RuntimeError(f'census of {cold.name}: exit {r.returncode} {r.stderr[-200:]!r}')
        out.write_text(r.stdout)
        return json.loads(out.read_text())['rows']


def disasm_counts(binary, fn):
    """(lock-prefixed instructions, calls to nl_atomic_fetch_add) in FN, or None."""
    r = run(['objdump', '-d', '--no-show-raw-insn', f'--disassemble={fn}', str(binary)], timeout=300)
    body = r.stdout.split(f'<{fn}>:', 1)
    if len(body) < 2:
        return None
    text = body[1]
    return (len(re.findall(r'\block\b', text)), text.count('nl_atomic_fetch_add'))


def symbols(binary):
    r = run(['nm', str(binary)], timeout=120)
    return {line.split()[-1] for line in r.stdout.splitlines() if line.strip()}


def check_s2(level, bins):
    limit, cell_limit = (32, 24) if level == '2.1' else (24, 16)
    problems, facts = [], []
    for name, b in bins.items():
        s = cons_slopes(b)
        facts.append(f"{name} cons {s['cons']:g} B, list {s['list']:g} B")
        for k, v in s.items():
            if v > limit:
                problems.append(f'{name} {k} slope {v:g} B > {limit}')
        views = image_census(b).get('cons-unrelocated-view', {}).get('allocations', 0)
        if views:
            problems.append(f'{name} image holds {views} cons header views')
    cell, fallback, writes = cell_bytes()
    facts.append(f'cell {cell} B')
    if cell > cell_limit or any(x > cell_limit for x in fallback):
        problems.append(f'cons cell {cell} B / fallback {fallback} > {cell_limit}')
    if level == '2.2':
        if any(off >= 16 for off in writes):
            problems.append(f'nl_alloc_consbox_init writes offsets {writes}')
        for name, b in bins.items():
            syms = symbols(b)
            if 'nelisp_nlconsbox_drop' in syms:
                problems.append(f'{name} links nelisp_nlconsbox_drop')
            for fn in ('nelisp_nlconsbox_clone', 'nl_alloc_consbox', 'nl_alloc_consbox_init'):
                if fn not in syms:
                    continue
                c = disasm_counts(b, fn)
                if c is None:
                    problems.append(f'{name} {fn}: no disassembly')
                elif c != (0, 0):
                    problems.append(f'{name} {fn}: {c[0]} lock, {c[1]} atomic calls')
    return problems, facts


def check_s5(bins, gnu):
    limits = {'float': 24, 'bignum': 32, 'marker': 40}
    problems, facts = [], []
    for name, b in bins.items():
        row = []
        for kind, lim in limits.items():
            v = slope(b, OBJSIZE, kind)
            row.append(f'{kind} {v:g}')
            if v > lim:
                problems.append(f'{name} {kind} slope {v:g} B > {lim}')
        facts.append(f"{name} {', '.join(row)} B")
        rows = image_census(b)
        aliases = {k: r['header_aliases'] for k, r in rows.items()
                   if r.get('objects') is not None and r.get('header_aliases')}
        views = {k: rows[k]['allocations'] for k in IMMEDIATE_VIEWS + ('cons-unrelocated-view',)
                 if rows.get(k, {}).get('allocations')}
        if aliases:
            problems.append(f'{name} image header aliases {aliases}')
        if views:
            problems.append(f'{name} image views {views}')
    want = run([str(gnu), '-Q', '--batch', '-l', str(PARITY)], timeout=120)
    lines = [l for l in want.stdout.splitlines() if l.startswith('WH-NUM ')]
    if not lines or 'WH-NUM-DONE' not in want.stdout:
        problems.append('GNU parity run failed')
    for name, b in bins.items():
        got = run([str(b), '--load', str(PARITY), '--eval', 'nil'], timeout=120)
        mine = [l for l in got.stdout.splitlines() if l.startswith('WH-NUM ')]
        diff = [f'{a!r} vs GNU {g!r}' for a, g in zip(mine, lines) if a != g]
        if len(mine) != len(lines) or 'WH-NUM-DONE' not in got.stdout:
            diff.append(f'{len(mine)} of {len(lines)} lines')
        if diff:
            problems.append(f'{name} numeric parity: {len(diff)} differences, first {diff[0][:160]}')
    facts.append(f'parity cases {len(lines)}')
    return problems, facts


def census_rows(path):
    rows = {}
    for line in Path(path).read_text().splitlines():
        parts = line.split('\t')
        if len(parts) == 4 and parts[2] == 'nl_root_reserve':
            rows[(parts[0], parts[1])] = int(parts[3])   # last record of a unit wins
    return rows


def check_s6(census):
    rows = census_rows(census)
    outside = {fn: n for (_, fn), n in rows.items() if n and not CALL_PATH.match(fn)}
    inside = sum(n for (_, fn), n in rows.items() if CALL_PATH.match(fn))
    problems = []
    if outside:
        top = sorted(outside.items(), key=lambda kv: -kv[1])[:5]
        problems.append(f'{sum(outside.values())} nl_root_reserve sites in {len(outside)} '
                        f'functions outside the call path, e.g. {top}')
    return problems, [f'call path keeps {inside} (S6.3)']


def report(name, problems, facts):
    verdict = 'FAIL' if problems else 'PASS'
    print(f"{verdict} word-heap {name}: {'; '.join(facts)}"
          + (f" -- {'; '.join(problems)}" if problems else ''))
    return 1 if problems else 0


def controls(base):
    rc = 0
    for what in ('s2.1', 's2.2', 's5.1', 's6.2'):
        ok = control_one(what, base)
        print(f"{'PASS' if ok else 'FAIL'} control {what}: known-bad input rejected")
        rc |= 0 if ok else 1
    return rc


def control_one(what, base):
    """1 iff the negative control for WHAT is rejected as it must be."""
    if what in ('s2.1', 's2.2'):
        probs, _ = check_s2(what[1:], {'control': Path(base).resolve()})
        return any('slope' in p for p in probs)
    if what == 's5.1':
        return slope(Path(base).resolve(), OBJSIZE, 'float') > 24
    with tempfile.TemporaryDirectory() as tmp:
        bad = Path(tmp) / 'bad.tsv'
        bad.write_text('u.o\tnl_sf_quote_w\tnl_root_reserve\t1\n'
                       'u.o\tnelisp_apply_function_w\tnl_root_reserve\t3\n')
        good = Path(tmp) / 'good.tsv'
        good.write_text('u.o\tnelisp_apply_function_w\tnl_root_reserve\t3\n'
                        'u.o\tnl_sf_quote_w\tdefun\t1\n')
        return bool(check_s6(bad)[0]) and not check_s6(good)[0]


def main(argv):
    ap = argparse.ArgumentParser()
    ap.add_argument('what', choices=['s2.1', 's2.2', 's5.1', 's6.2', 'controls'])
    ap.add_argument('--dyn')
    ap.add_argument('--static')
    ap.add_argument('--control')
    ap.add_argument('--gnu', default='/usr/local/bin/emacs')
    ap.add_argument('--census')
    ap.add_argument('--base')
    a = ap.parse_args(argv)
    if a.what == 'controls':
        return controls(a.base)
    if a.what == 's6.2' or a.control:
        if not control_one(a.what, a.control):
            print(f'FAIL word-heap {a.what.upper()}: the negative control was not rejected')
            return 1
    if a.what == 's6.2':
        return report('S6.2', *check_s6(a.census))
    bins = {'dyn': Path(a.dyn).resolve(), 'static': Path(a.static).resolve()}
    if a.what == 's5.1':
        return report('S5.1', *check_s5(bins, a.gnu))
    return report(a.what.upper(), *check_s2(a.what[1:], bins))


if __name__ == '__main__':
    sys.exit(main(sys.argv[1:]))
