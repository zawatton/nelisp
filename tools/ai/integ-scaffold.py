#!/usr/bin/env python3
"""integ-scaffold.py: create the next integrated-build directory from the last one.

Copies target/integ-BASE/{build.sh,chain.sh,fixtures.py,accept/run.sh,
win/run-win.sh,win/features*,matrix/{agg.py,runall.py,specs.json,calib.json,
rows.txt,drv}} to target/integ-NEW/ and rewrites every version reference:

- integ-BASE -> integ-NEW everywhere (build dirs, wine prefix, NELISP_VERSION);
- the matrix compares the new build with the base build: the older engine
  (integ-PREV, label PREVLABEL) becomes integ-BASE (BASELABEL), and BASELABEL
  becomes the new label;
- the build.sh header line is replaced by --note;
- chain.sh gets the two post-build checks (add_gates): tools/ai/native-row-census.sh
  on the integrated source tree right after the build, a hard gate before the
  fixtures/accept step (a stale harvested native row stops the chain), and
  tools/ai/matrix-layout-check.py after agg.py (report only: LAYOUT vs WORK for
  every matrix row at or above the 1.03 new/old ratio);
- matrix/agg.py prints the CPU the matrix ran on (MCPU, as chain.sh sets it)
  instead of a hard-coded number;
- with --lanes, accept/run.sh meters exactly those lane worktrees, after
  lane-ledger-bincheck.py has checked that their ledgers take the binary from
  the environment; each lane's ledger gets <LANE>_BIN (e.g. NC4_BIN) set to
  the integrated binary.

Outputs (build results, logs, wine prefixes) are never copied.  Afterwards it
prints every integ-NNNN reference left per file, so a missed rewrite is
visible; references to older builds used as fixed baselines (e.g. V17) are
expected.

Usage: integ-scaffold.py --base 1039 --new 1040 --label v23 \
         --note "v22 + nc4, bufstore" [--lanes nc4-20261104 bufstore-20261104]
         [--target DIR]   (default: <repo>/target)
"""
import argparse
import os
import re
import shutil
import sys

FILES = ['build.sh', 'chain.sh', 'fixtures.py', 'accept/run.sh', 'win/run-win.sh',
         'matrix/agg.py', 'matrix/runall.py', 'matrix/specs.json', 'matrix/calib.json',
         'matrix/rows.txt']
GLOBS = [('win', re.compile(r'features.*\.(el|gnu)$')), ('matrix/drv', None)]
TEXT = ('.sh', '.py')
CHECK = os.path.join(os.path.dirname(os.path.abspath(__file__)), 'lane-ledger-bincheck.py')


def main():
    ap = argparse.ArgumentParser(description=__doc__.split('\n')[0])
    ap.add_argument('--base', required=True, type=int)
    ap.add_argument('--new', required=True, type=int)
    ap.add_argument('--label', required=True, help='new version label, e.g. v23')
    ap.add_argument('--note', required=True, help='build.sh header: what this build adds')
    ap.add_argument('--lanes', nargs='*', default=None,
                    help='lane worktree names (under agent-worktrees) to meter in accept/run.sh')
    ap.add_argument('--target', default=os.path.join(
        os.path.dirname(os.path.abspath(__file__)), '..', '..', 'target'))
    a = ap.parse_args()
    tgt = os.path.abspath(a.target)
    src, dst = f'{tgt}/integ-{a.base}', f'{tgt}/integ-{a.new}'
    if not os.path.isdir(src):
        sys.exit(f'no base dir {src}')
    if os.path.exists(dst):
        sys.exit(f'{dst} exists; refusing to overwrite')

    agg = open(f'{src}/matrix/agg.py').read()
    m = re.search(r'# MATRIX (v\d+) \(integ-(\d+)\) vs (v\d+) \(integ-(\d+)\)', agg)
    if not m or int(m.group(2)) != a.base:
        sys.exit('cannot read base/prev labels from matrix/agg.py header')
    base_label, prev_label, prev = m.group(1), m.group(3), m.group(4)

    for f in FILES:
        os.makedirs(os.path.dirname(f'{dst}/{f}'), exist_ok=True)
        shutil.copy2(f'{src}/{f}', f'{dst}/{f}')
    for d, pat in GLOBS:
        if pat is None:
            shutil.copytree(f'{src}/{d}', f'{dst}/{d}')
        else:
            for f in os.listdir(f'{src}/{d}'):
                if pat.search(f):
                    shutil.copy2(f'{src}/{d}/{f}', f'{dst}/{d}/{f}')

    for f in FILES:
        if not f.endswith(TEXT):
            continue
        p = f'{dst}/{f}'
        s = open(p).read().replace(f'integ-{a.base}', f'integ-{a.new}')
        if f.startswith('matrix/'):
            s = s.replace(f'integ-{prev}', f'integ-{a.base}')
            # Labels also occur inside identifiers (pairs_faster_than_v21), so
            # match on digits, not word boundaries.
            s = re.sub(rf'(?<![0-9A-Za-z])({prev_label}|{base_label})(?!\d)',
                       lambda k: base_label if k.group(1) == prev_label else a.label, s)
        if f == 'chain.sh':
            s = add_gates(s)
            s = harden_chain(s)
            # The Win64 checks leave a wine prefix of ~1 GB; drop it once the chain ends.
            if 'win/wineprefix' not in s:
                s += ('W=$RT/target/$N/win; f=wineprefix; '
                      '[ -d "$W/$f" ] && rm -rf "${W:?}/${f:?}"\n')
        if f == 'matrix/agg.py':
            s = fix_agg_cpu(s, open(f'{dst}/chain.sh').read() if os.path.exists(f'{dst}/chain.sh') else '')
        if f == 'build.sh':
            s = re.sub(r'^# v\d+:.*$', f'# {a.label}: {a.note}', s, count=1, flags=re.M)
            s = harden_build(s)
        if f == 'win/run-win.sh':
            s = harden_win_build(s)
        if f == 'accept/run.sh':
            s = re.sub(r'^# v\d+ acceptance', f'# {a.label} acceptance', s, flags=re.M)
            if a.lanes is not None:
                s = rewrite_lanes(s, a.lanes)
            s = harden_accept(s)
        open(p, 'w').write(s)

    print(f'created {dst}: {a.label} (base {base_label} integ-{a.base},'
          f' matrix vs {base_label}; prev {prev_label} integ-{prev} dropped)')
    for f in FILES:
        if f.endswith(TEXT):
            refs = sorted(set(re.findall(r'integ-\d+', open(f'{dst}/{f}').read())))
            print(f'  {f}: {" ".join(refs) or "-"}')


GATE_MARK = 'native-row-census'
CENSUS = (
    'RT=$PWD\n'
    '( ulimit -v 8388608; bash tools/ai/native-row-census.sh . $B ) > target/$N/native-row-census.log 2>&1\n'
    'rc=$?; echo "== native-row-census rc=$rc: $(tail -1 target/$N/native-row-census.log)"\n'
    '[ $rc = 0 ] || { grep -E "^(STALE|DUP-)" target/$N/native-row-census.log | head -20; exit 1; }\n')
LAYOUT = (
    '\ncd $RT && ( ulimit -v 8388608; timeout 2400 python3 tools/ai/matrix-layout-check.py target/$N '
    '> target/$N/matrix/layout-check.log 2>&1; echo "== layout-check (report only) rc=$?"; '
    'tail -14 target/$N/matrix/layout-check.log | cut -c1-160 )\n')


def add_gates(s):
    """chain.sh: native-row-census as a hard gate after the build, matrix-layout-check after agg.py."""
    if GATE_MARK in s:
        return s
    m = re.search(r'^B=\S+; \[ -x \$B \] \|\| exit 1\n', s, flags=re.M)
    if not m or 'python3 agg.py' not in s:
        sys.exit('chain.sh: build check line or agg.py step not found; cannot add the gates')
    s = s[:m.end()] + CENSUS + s[m.end():]
    # agg.py reads MCPU for its header: give it the value runall.py ran with.
    mc = re.search(r'MCPU=(\d+) python3 runall\.py', s)
    if mc:
        s = re.sub(r'(?<![=\w])python3 agg\.py', f'MCPU={mc.group(1)} python3 agg.py', s, count=1)
    return s.rstrip('\n') + '\n' + LAYOUT.lstrip('\n')


def fix_agg_cpu(s, chain):
    """agg.py header: the CPU of the matrix run is MCPU (from chain.sh), not a fixed 11."""
    m = re.search(r'MCPU=(\d+) python3 runall\.py', chain)
    default = m.group(1) if m else '12'
    s = re.sub(r'CPU \d+\.', "CPU {os.environ.get('MCPU', '%s')}." % default, s, count=1)
    if "{os.environ.get('MCPU'" in s:
        s = re.sub(r'print\((f?)"# MATRIX', 'print(f"# MATRIX', s, count=1)
    return s


def harden_build(s):
    """Keep the compiler status after Linux build logging, before later steps."""
    guard = '[ "$rc" -eq 0 ] || exit "$rc"'
    source = s.splitlines(True)
    lines = []
    found = False
    for index, line in enumerate(source):
        lines.append(line)
        if line.lstrip().startswith('rc=$?;') and 'BUILD rc=$rc' in line:
            found = True
            if index + 1 == len(source) or source[index + 1].strip() != guard:
                indent = line[:len(line) - len(line.lstrip())]
                if not line.endswith('\n'):
                    lines[-1] += '\n'
                lines.append(indent + guard + '\n')
    if not found:
        raise ValueError('build.sh: compiler-status log shape not found')
    return ''.join(lines)


def harden_win_build(s):
    """Capture Win64 compiler status before say/stat can replace it."""
    guard = '[ "$integ_build_rc" -eq 0 ] || exit "$integ_build_rc"'
    if 'integ_build_rc=$?' in s and guard in s:
        return s
    lines = []
    found = False
    for line in s.splitlines(True):
        if line.lstrip().startswith('say "build win rc=$? '):
            found = True
            indent = line[:len(line) - len(line.lstrip())]
            lines.append(indent + 'integ_build_rc=$?\n')
            lines.append(line.replace('rc=$?', 'rc=$integ_build_rc', 1).rstrip('\n') + '\n')
            lines.append(indent + guard + '\n')
        else:
            lines.append(line)
    if not found:
        raise ValueError('run-win.sh: compiler-status log shape not found')
    return ''.join(lines)


def harden_matrix(s):
    """Retain stage logs while propagating the original matrix child status."""
    lines = []
    for line in s.splitlines(True):
        if 'python3 runall.py' in line and 'integ_matrix_rc=' not in line:
            line, count = re.subn(
                r'((?:MCPU=\d+ )?python3 runall\.py[^;\n]*)(;\s*grep[^;\n]+)?',
                lambda m: m[1] + '; integ_matrix_rc=$?' + (m[2] or '')
                + '; [ "$integ_matrix_rc" -eq 0 ] || exit "$integ_matrix_rc"', line, count=1)
            if count != 1:
                raise ValueError('chain.sh: unknown matrix runner shape')
        if 'python3 agg.py' in line and 'integ_agg_rc=' not in line:
            line, count = re.subn(
                r'((?:MCPU=\d+ )?python3 agg\.py[^;\n]*)(;\s*tail[^;\n]+)?',
                lambda m: m[1] + '; integ_agg_rc=$?' + (m[2] or '')
                + '; [ "$integ_agg_rc" -eq 0 ] || exit "$integ_agg_rc"', line, count=1)
            if count != 1:
                raise ValueError('chain.sh: unknown matrix aggregator shape')
        if 'python3 tools/ai/matrix-layout-check.py' in line and 'integ_layout_rc=' not in line:
            old = '; echo "== layout-check (report only) rc=$?";'
            if old not in line or not line.rstrip().endswith(')'):
                raise ValueError('chain.sh: unknown matrix layout checker shape')
            line = line.replace(old, '; integ_layout_rc=$?; echo "== layout-check (report only) rc=$integ_layout_rc";', 1)
            line = line.rstrip()[:-1] + '; exit "$integ_layout_rc" ) || exit $?\n'
        lines.append(line)
    return ''.join(lines)


def harden_chain(s):
    """Wait for every parallel prerequisite and stop before later stages on failure."""
    if 'integ_cleanup_wineprefix' not in s:
        cleanup = ('INTEG_CLEANUP_ROOT=$PWD\n'
                   'integ_cleanup_wineprefix() {\n'
                   '  [[ "${N:-}" =~ ^integ-[0-9]+$ ]] || return 0\n'
                   '  local prefix="$INTEG_CLEANUP_ROOT/target/$N/win/wineprefix"\n'
                   '  [ ! -d "$prefix" ] || rm -rf -- "$prefix"\n'
                   '}\n'
                   'trap integ_cleanup_wineprefix EXIT\n')
        s = re.sub(r'^(N=integ-[0-9]+[^\n]*\n)', lambda m: m[0] + cleanup,
                   s, count=1, flags=re.M)
    lines = []
    for line in s.splitlines(True):
        if line.startswith('( (') and line.rstrip().endswith('& wait )'):
            body = line.rstrip()[1:-1].strip()
            body = body[:-len('wait')]
            # Build and acceptance already hold one admission each. Add one
            # only to the fixture/check children that lack it, never nest it.
            body = re.sub(r'(ulimit -v 8388608; )(?!\$H\s)', r'\1$H ', body)
            body = re.sub(r'&(?=\s)', '& gate_pids+=("$!");', body)
            line = ('( gate_pids=(); ' + body + ' gate_rc=0; '
                    'for gate_pid in "${gate_pids[@]}"; do '
                    'wait "$gate_pid" || gate_rc=1; done; '
                    'exit "$gate_rc" ) || exit $?\n')
        lines.append(line)
    return harden_matrix(''.join(lines))


def harden_accept(s):
    """Do not replace failed ledger validation with a successful status echo."""
    lines = []
    for line in s.splitlines(True):
        if 'lane-ledger-bincheck.py' in line and ' || echo ' in line:
            command, message = line.rstrip().split(' || echo ', 1)
            line = command + ' || { echo ' + message + '; exit 1; }\n'
        elif 'progress-meter.sh' in line and ' || ' not in line:
            line = line.rstrip() + ' || { echo "$n meter FAIL (see $n-meter.log)"; exit 1; }\n'
        lines.append(line)
    return ''.join(lines)


def rewrite_lanes(s, lanes):
    """Meter LANES in accept/run.sh with <LANE>_BIN=$B, each ledger gated by lane-ledger-bincheck."""
    loop = re.search(r'^for l in [^;]*; do\n(.*?)^done\n', s, flags=re.M | re.S)
    if not loop:
        sys.exit('accept/run.sh: lane loop not found')
    # Drop the lines an earlier run inserted, so they do not pile up per version.
    rest = ''.join(ln for ln in loop.group(1).splitlines(True)
                   if not re.match(r'  (n=\$\{l%-\*\}|L=\$R/|export "\$\(printf|python3 \S*lane-ledger-bincheck)',
                                   ln))
    # A re-ported lane worktree (libcomp3-...) keeps its ledger name (libcomp);
    # lanes whose own name ends in a digit (body1, nc4) have a ledger under it.
    body = ('  n=${l%-*}; [ -f $R/$l/tools/ai/$n-progress.org ] || n=${n%[0-9]}\n'
            '  L=$R/$l/tools/ai/$n-progress.org\n'
            '  export "$(printf %s "$n" | tr a-z- A-Z_)_BIN=$B"\n'
            f'  python3 {CHECK} $L'
            ' > $O/$n-bincheck.log 2>&1 || echo "$n bincheck FAIL (see $n-bincheck.log)"\n'
            + rest)
    new = f'for l in {" ".join(lanes)}; do\n{body}done\n'
    return s[:loop.start()] + new + s[loop.end():]


if __name__ == '__main__':
    main()
