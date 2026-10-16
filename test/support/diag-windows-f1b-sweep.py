#!/usr/bin/env python3
"""Local Windows sweep for the F1B exit-load startup corruption.

On CI (Windows Server 2025) the F1B load step fails deterministically with
void-function (nil) or (signal-p) while `nelisp-native-startup-ensure'
evaluates the native startup companion, but only for particular memory
layouts: any env padding of 64 B or more made it pass.  This script finds a
failing layout on the local machine:

  1. creates an exit-cache once (three compile steps);
  2. runs the load step without F1_FORCE_GC under env padding
     START..STOP step STEP bytes (DIAG_PAD), stopping at the first failure
     unless --all is given.

Usage (from the repository root, PowerShell or cmd):
  python test/support/diag-windows-f1b-sweep.py target\\nelisp.exe [--start 0] [--stop 4096] [--step 8] [--all]

Each run's stdout/stderr is kept in target/diag-f1b-sweep/.  A failing pad
reproduces deterministically: rerun it alone with
  python test/support/diag-windows-f1b-sweep.py target\\nelisp.exe --start PAD --stop PAD
"""
import argparse
import importlib.util
import os
import sys
from pathlib import Path

HERE = Path(__file__).resolve().parent
spec = importlib.util.spec_from_file_location('f1', HERE / 'run-windows-native-f1.py')
f1 = importlib.util.module_from_spec(spec)
spec.loader.exec_module(f1)
ROOT = f1.ROOT
DRIVER = 'test/standalone-native-funcall-v2-exits-driver.el'

parser = argparse.ArgumentParser()
parser.add_argument('binary', type=Path)
parser.add_argument('--start', type=int, default=0)
parser.add_argument('--stop', type=int, default=4096)
parser.add_argument('--step', type=int, default=8)
parser.add_argument('--all', action='store_true', help='keep going after a failure')
parser.add_argument('--force-gc', action='store_true', help='set F1_FORCE_GC=1 (CI default)')
args = parser.parse_args()

binary = args.binary.resolve(strict=True)
work = ROOT / 'target/diag-f1b-sweep'
work.mkdir(parents=True, exist_ok=True)
base = dict(os.environ, F1_BACKEND='in-house',
            F1B_FIXTURE=str(ROOT / 'test/fixtures/diag-f1b-exits.elc'), F1B_BACKEND='in-house',
            F1B_COLD='0', NELISP_NATIVE_CACHE=str(work / 'exit-cache'))
base.pop('F1_FORCE_GC', None)
if args.force_gc:
    base['F1_FORCE_GC'] = '1'


def go(phase, changes):
    env = dict(base, **changes)
    result = f1.run(f1.reader_command(binary, None, DRIVER), env, work, phase, deadline=900)
    err = (work / (phase + '.err')).read_text(errors='replace')
    line = next((l for l in err.splitlines() if 'uncaught' in l), '')
    print('SWEEP %-14s rc=%s %.1fs %s' % (phase, result['rc'], result['seconds'], line[-140:]), flush=True)
    return result['rc']


if not (work / 'exit-cache').exists():
    for unit in ('f1b-one', 'f1b-zero', 'f1b-six'):
        if go('compile-' + unit, dict(F1B_PHASE='compile', F1B_COMPILE_UNIT=unit)):
            sys.exit('compile failed; see ' + str(work))
failures = []
for pad in range(args.start, args.stop + 1, args.step):
    if go('load-pad%d' % pad, dict(F1B_PHASE='load', DIAG_PAD='x' * pad)):
        failures.append(pad)
        if not args.all:
            break
print('SWEEP-SUMMARY failing pads:', failures or 'none')
