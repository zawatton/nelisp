#!/usr/bin/env python3
"""Replay source-copy controls with no inherited target artifacts."""
import argparse
import json
import os
from pathlib import Path
import shutil
import subprocess
import time

if not __debug__:
    raise SystemExit('clean-control refuses optimized Python execution')


def require(condition, diagnostic):
    if not condition:
        raise SystemExit(diagnostic)


parser = argparse.ArgumentParser()
parser.add_argument('mode', choices=['init-shrink', 'regression-red', 'regression-green'])
mode = parser.parse_args().mode
root = Path(__file__).resolve().parents[2]
evidence = root / 'target/wasm-intern-symbol/rev4'
evidence.mkdir(parents=True, exist_ok=True)
copy = evidence / ('clean-' + mode)
if copy.exists():
    shutil.rmtree(copy)
shutil.copytree(root, copy, symlinks=True,
                ignore=shutil.ignore_patterns('target', '.git', '__pycache__'))
require(not (copy / 'target').exists(), 'copy inherited target')
(evidence / (mode + '.initial.json')).write_text(json.dumps({
    'inherited_target': False,
    'prior_output_directories': {lane: (copy / ('target/wasm-' + lane)).exists()
                                 for lane in ['target-init', 'target-alloc', 'genuine-alloc']}
}, indent=2) + '\n')
notes = Path(os.environ['NOTES_ROOT'])
if mode == 'init-shrink':
    provider = copy / 'scripts/nelisp-standalone-build.el'
    source = provider.read_text()
    anchor = '(defun nl_intern_region_init ()\n      (let ((rbase (syscall-direct 9 0 67108864 3 34 -1 0)'
    require(source.count(anchor) == 1, 'init-shrink anchor')
    provider.write_text(source.replace(anchor, anchor.replace('67108864', '33554432')))
    # The ledger's shell redirect needs its own output directory.
    (copy / 'target/wasm-intern-symbol').mkdir(parents=True)
    ledger = (copy / 'tools/ai/wasm-intern-symbol-progress.org').read_text()
    command = ledger.split('** S1.3 ', 1)[1].split('cmd: ', 1)[1].splitlines()[0]
    (evidence / (mode + '.command')).write_text(command + '\n')
    argv = ['bash', '-c', command]
else:
    driver = copy / 'tools/ai/wasm-intern-symbol-regression.sh'
    if mode == 'regression-red':
        source = driver.read_text()
        anchor = '  mkdir -p "target/wasm-$lane"\n'
        require(source.count(anchor) == 1, 'directory control anchor')
        driver.write_text(source.replace(anchor, ''))
        # Prior mode retains the same parent heavy slot for all children.
        argv = [str(notes / 'bin/heavy-run.sh'), str(driver), 'prior']
    else:
        argv = [str(driver), 'all']
    # Set the cap before the driver can launch any Emacs or Node child.
    argv = ['bash', '-c', 'ulimit -v 8388608; exec "$@"', 'clean-replay', *argv]
started = time.monotonic()
with (evidence / (mode + '.log')).open('w') as log:
    result = subprocess.run(argv, cwd=copy, stdout=log, stderr=subprocess.STDOUT)
elapsed = time.monotonic() - started
(evidence / (mode + '.rc')).write_text(str(result.returncode) + '\n')
if mode == 'init-shrink':
    diagnostic = (copy / 'target/wasm-intern-symbol/current.log').read_text()
    require(result.returncode != 0 and 'Intern region extent mismatch: init=33554432 consumer=67108864' in diagnostic, diagnostic)
    # The rejection precedes even native compilation, let alone Wasm emission.
    require(not (copy / 'target/wasm-intern-symbol/native.o').exists(), 'rejected init compiled native.o')
    require(not (copy / 'target/wasm-intern-symbol/green.wasm').exists(), 'rejected init compiled green.wasm')
    print('control=init-shrink exact-S1.3-rc=%d before-compilation=1 message=Intern region extent mismatch: init=33554432 consumer=67108864' % result.returncode)
elif mode == 'regression-red':
    out = copy / 'target/wasm-intern-symbol/regressions'
    diagnostic = (out / 'target-init.log').read_text()
    require(result.returncode != 0 and 'target/wasm-target-init/current.log: No such file or directory' in diagnostic, diagnostic)
    require((out / 'target-init.rc').read_text().strip() != '0', 'directory defect returned zero')
    print('control=regression-directory prior-rc=%d message=target/wasm-target-init/current.log: No such file or directory' % result.returncode)
else:
    require(result.returncode == 0, (evidence / (mode + '.log')).read_text())
    out = copy / 'target/wasm-intern-symbol/regressions'
    for label in ['genuine-accept', 'target-init', 'target-alloc', 'genuine-alloc', 'intern-symbol']:
        require((out / (label + '.rc')).read_text().strip() == '0', label + ': regression step failed')
        print('clean-regression=%s rc=0' % label)
(evidence / (mode + '.seconds')).write_text('%.3f\n' % elapsed)
print('source-copy=%s inherited-target=0 elapsed=%.3f' % (mode, elapsed))
