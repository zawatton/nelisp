#!/usr/bin/env python3
"""Bounded subprocess controls for generated integration prerequisite checks."""
import importlib.util
import json
import os
from pathlib import Path
import subprocess
import sys
import tempfile
import time

ROOT = Path(__file__).resolve().parents[1]
spec = importlib.util.spec_from_file_location('scaffold', ROOT / 'tools/ai/integ-scaffold.py')
scaffold = importlib.util.module_from_spec(spec)
spec.loader.exec_module(scaffold)


def run(script, directory):
    start = time.monotonic()
    result = subprocess.run(['bash', '-c', script], cwd=directory,
                            capture_output=True, text=True, timeout=8)
    return result.returncode, time.monotonic() - start


def chain(directory, failure, hardened):
    # The last child succeeds: bare wait loses the first child's failure.
    script = ('N=integ-1\n'
              'mkdir -p target/$N/win/wineprefix\n'
              'H=heavy; heavy() { echo admitted >> admissions; "$@"; }\n'
              '( (ulimit -v 8388608; timeout 2 bash -c "sleep 0.05; exit ' + str(failure) + '") > first.log 2>&1 & '
              '(ulimit -v 8388608; $H timeout 2 bash -c "sleep 0.1; echo child > second") > second.log 2>&1 & wait )\n'
              'sleep 0.4; echo matrix > matrix\n')
    if hardened:
        script = scaffold.harden_chain(script)
        assert scaffold.harden_chain(script) == script
    return run(script, directory)


def check_chain(control=False):
    with tempfile.TemporaryDirectory() as name:
        directory = Path(name)
        old_rc, old_time = chain(directory, 7, False)
        assert old_rc == 0 and (directory / 'matrix').exists()
        (directory / 'matrix').unlink()
        (directory / 'admissions').unlink()
        new_rc, new_time = chain(directory, 7, True)
        assert new_rc != 0 and not (directory / 'matrix').exists()
        assert not (directory / 'target/integ-1/win/wineprefix').exists()
        assert (directory / 'second').exists(), 'Every recorded child must finish'
        assert (directory / 'admissions').read_text().splitlines() == ['admitted', 'admitted']
        assert old_time - new_time >= 0.3
        if not control:
            rc, _ = chain(directory, 0, True)
            assert rc == 0 and (directory / 'matrix').exists()
        print(json.dumps(dict(old_failed_prerequisite_matrix_runs=1,
                              new_failed_prerequisite_matrix_runs=0,
                              old_seconds=round(old_time, 3),
                              new_seconds=round(new_time, 3),
                              seconds_saved=round(old_time-new_time, 3))))


def check_accept():
    with tempfile.TemporaryDirectory() as name:
        directory = Path(name)
        # Match inherited lines without executing a real lane meter.
        script = ('n=control; O=.; python3() { return "$BIN_RC"; }; '
                  'timeout() { return "$METER_RC"; }\n'
                  '  python3 lane-ledger-bincheck.py ledger > $O/$n-bincheck.log 2>&1 || echo "$n bincheck FAIL"\n'
                  '  ( timeout 3600 progress-meter.sh --ledger ledger ) > $O/$n-meter.log 2>&1\n'
                  '  echo "$n measured"\n'
                  'echo matrix > matrix\n')
        for bin_rc, meter_rc in ((8, 0), (0, 9)):
            env = f'BIN_RC={bin_rc}; METER_RC={meter_rc}\n'
            rc, _ = run(env + script, directory)
            assert rc == 0 and (directory / 'matrix').exists()
            (directory / 'matrix').unlink()
            fixed = scaffold.harden_accept(script)
            assert scaffold.harden_accept(fixed) == fixed
            rc, _ = run(env + fixed, directory)
            assert rc != 0 and not (directory / 'matrix').exists()
        rc, _ = run('BIN_RC=0; METER_RC=0\n' + scaffold.harden_accept(script), directory)
        assert rc == 0 and (directory / 'matrix').exists()
        print('ACCEPT PASS bincheck and meter failures stop; success proceeds')


# Preserve the inherited capture/log/artifact-check shapes, but replace all
# build/setup/removal commands with one compiler in a private temporary tree.
LINUX_WRAPPER = '''#!/bin/bash
set -u
O=$PWD; EXE=$O/nelisp-dynamic
start=$(date +%s)
bash "$O/fake-compiler.sh" "$EXE" > $O/build.log 2>&1
rc=$?; echo "BUILD rc=$rc secs=$(($(date +%s)-start))" >> $O/build.log; echo "BUILD rc=$rc"
echo complete > complete
'''
WIN_WRAPPER = '''#!/bin/bash
set -u
W=$PWD; EXE=$W/nelisp.exe; log=$W/win.log
say() { echo "$(date +%T) $*" | tee -a $log; }
case "$1" in
build)
  s=$(date +%s)
  (bash "$W/fake-compiler.sh" "$EXE" > $W/build.log 2>&1)
  say "build win rc=$? $(( $(date +%s) - s ))s exe=$(stat -c %s $EXE 2>/dev/null) cold=$(stat -c %s $EXE.cold 2>/dev/null)"
  [ -f $EXE.cold ] || { say "no cold image"; exit 1; } ;;
esac
echo complete > complete
'''
FAKE_COMPILER = '''#!/bin/bash
echo compiler-output
if [ "$COMPILER_RC" -eq 0 ]; then
  echo fresh-exe > "$1"
  echo fresh-cold > "$1.cold"
fi
exit "$COMPILER_RC"
'''


def wrapper_run(directory, text, compiler_rc, artifacts, windows):
    directory.mkdir()
    (directory / 'fake-compiler.sh').write_text(FAKE_COMPILER)
    exe = directory / ('nelisp.exe' if windows else 'nelisp-dynamic')
    for suffix in artifacts:
        Path(str(exe) + suffix).write_text('stale-artifact\n')
    # The fixture never contains the real templates' checkout/removal steps.
    assert 'rm -' not in text and 'git ls-files' not in text
    result = subprocess.run(['bash', '-c', text, 'wrapper', 'build'], cwd=directory,
                            env=dict(os.environ, COMPILER_RC=str(compiler_rc)),
                            capture_output=True, text=True, timeout=8)
    log = (directory / ('win.log' if windows else 'build.log')).read_text()
    assert 'compiler-output' in (directory / 'build.log').read_text()
    assert f'rc={compiler_rc}' in log
    if compiler_rc == 0:
        assert (directory / 'complete').exists()
        assert exe.read_text() == 'fresh-exe\n'
        assert Path(str(exe) + '.cold').read_text() == 'fresh-cold\n'
    else:
        for suffix in artifacts:
            assert Path(str(exe) + suffix).read_text() == 'stale-artifact\n'
    return result.returncode


def generated_wrappers(directory, matrix_body=None):
    target = directory / 'target'
    base = target / 'integ-1'
    for name in scaffold.FILES:
        path = base / name
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text('{}\n' if path.suffix == '.json' else '# safe unused fixture\n')
    (base / 'matrix/drv').mkdir()
    (base / 'matrix/agg.py').write_text('print("# MATRIX v1 (integ-1) vs v0 (integ-0)")\n')
    (base / 'build.sh').write_text(LINUX_WRAPPER)
    (base / 'win/run-win.sh').write_text(WIN_WRAPPER)
    (base / 'chain.sh').write_text('#!/bin/bash\nN=integ-1\n'
                                  'B=./fake-binary; [ -x $B ] || exit 1\n'
                                  + (matrix_body or 'MCPU=11 python3 runall.py\npython3 agg.py\n'))
    result = subprocess.run([sys.executable, str(ROOT / 'tools/ai/integ-scaffold.py'),
                             '--base', '1', '--new', '2', '--label', 'v2',
                             '--note', 'safe compiler-status control', '--target', str(target)],
                            capture_output=True, text=True, timeout=8)
    assert result.returncode == 0, result.stderr
    return [(target / 'integ-2' / name).read_text() for name in ('build.sh', 'win/run-win.sh')]


def check_wrappers():
    with tempfile.TemporaryDirectory() as name:
        directory = Path(name)
        # Calibrate the checker on the actual inherited failure-masking shape.
        old = [wrapper_run(directory / f'old-{windows}', text, 7, ('', '.cold'), windows)
               for windows, text in enumerate((LINUX_WRAPPER, WIN_WRAPPER))]
        assert old == [0, 0], f'Old wrappers must demonstrate the bug: {old}'
        print(f'OLD-WRAPPERS compiler=7 stale_exe_and_cold=1 wrapper_status={old}', flush=True)
        generated = generated_wrappers(directory)
        cases = 0
        for windows, text in enumerate(generated):
            for compiler_rc in (7, 0):
                for index, artifacts in enumerate(((), ('',), ('.cold',), ('', '.cold'))):
                    case = directory / f'new-{windows}-{compiler_rc}-{index}'
                    rc = wrapper_run(case, text, compiler_rc, artifacts, windows)
                    assert rc == compiler_rc, f'compiler={compiler_rc}, wrapper={rc}, windows={windows}, artifacts={artifacts}'
                    assert (case / 'complete').exists() == (compiler_rc == 0)
                    cases += 1
        assert scaffold.harden_build(generated[0]) == generated[0]
        assert scaffold.harden_win_build(generated[1]) == generated[1]
        print(f'WRAPPERS PASS old_failed_compiler_returns={old} generated_cases={cases} failure=7 success=0 stale_artifacts_preserved=1 idempotent=1')


def check_matrix():
    with tempfile.TemporaryDirectory() as name:
        directory = Path(name)
        matrix = directory / 'target/integ-1/matrix'
        matrix.mkdir(parents=True)
        (matrix / 'rows.txt').write_text('')
        # Inherited actual wrapper shapes; fake shell children only. No real
        # build, removal, benchmark, runtime or layout-checker is executed.
        setup = ('#!/bin/bash\nN=integ-1\nRT=$PWD\n'
                 'timeout() { shift; "$@"; }\n'
                 'python3() { local stage; case "$*" in '
                 '*runall.py*) stage=run;; *agg.py*) stage=agg;; '
                 '*matrix-layout-check.py*) stage=layout;; esac; '
                 'echo "$stage" >> "$RT/events"; echo "$stage rc 0"; '
                 'case $stage in run) return "$RUN_RC";; agg) return "$AGG_RC";; '
                 'layout) return "$LAYOUT_RC";; esac; }\n')
        body = ('cd target/$N/matrix && MCPU=12 python3 runall.py $(cat rows.txt) '
                '> runall.log 2>&1; grep -c "rc 0" runall.log; '
                'MCPU=12 python3 agg.py > REPORT.md 2>&1; tail -1 REPORT.md\n'
                + scaffold.LAYOUT + 'echo completed\n')
        generated_wrappers(directory / 'generated', body.split(scaffold.LAYOUT)[0])
        generated = (directory / 'generated/target/integ-2/chain.sh').read_text()
        assert scaffold.harden_chain(generated) == generated
        # Execute only the generated matrix/layout lines with fake children;
        # never execute its real prerequisite or cleanup template operations.
        hardened = ''.join(line + '\n' for line in generated.splitlines()
                           if 'python3 runall.py' in line or
                           'python3 tools/ai/matrix-layout-check.py' in line) + 'echo completed\n'
        assert scaffold.harden_chain(hardened) == hardened
        for failed in ('RUN_RC', 'AGG_RC', 'LAYOUT_RC', None):
            statuses = dict(RUN_RC='0', AGG_RC='0', LAYOUT_RC='0')
            if failed:
                statuses[failed] = '7'
            prefix = ''.join(f'export {k}={v}\n' for k, v in statuses.items())
            for old in (True, False):
                events = directory / 'events'
                if events.exists():
                    events.unlink()
                rc, _ = run(setup + prefix + (body if old else hardened), directory)
                expected = 0 if old or failed is None else 7
                assert rc == expected, (failed, old, rc, expected)
                stages = events.read_text().splitlines()
                expected_stages = (['run'] if not old and failed == 'RUN_RC' else
                                   ['run', 'agg'] if not old and failed == 'AGG_RC' else
                                   ['run', 'agg', 'layout'])
                assert stages == expected_stages
                assert 'run rc 0' in (matrix / 'runall.log').read_text()
                if 'agg' in stages:
                    assert 'agg rc 0' in (matrix / 'REPORT.md').read_text()
                if 'layout' in stages:
                    assert 'layout rc 0' in (matrix / 'layout-check.log').read_text()
        print('MATRIX PASS old_masked=7->0 new_propagated=7->7 stages=run,agg,layout success=0 idempotent=1 logs_preserved=1')


if __name__ == '__main__':
    mode = sys.argv[1] if len(sys.argv) > 1 else '--all'
    if mode in ('--old-control', '--chain', '--all'):
        check_chain(mode == '--old-control')
    if mode in ('--accept', '--all'):
        check_accept()
    if mode in ('--wrappers', '--all'):
        check_wrappers()
    if mode in ('--matrix', '--all'):
        check_matrix()
