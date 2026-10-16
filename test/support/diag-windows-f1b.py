#!/usr/bin/env python3
"""Diagnose the Windows F1B exit-load failure: void-function (signal-p).

Since 2026-10-16 the F1B `exit-load' step of run-windows-native-f1.py fails
in every Windows run (3 of 3) with `void-function: (signal-p)' raised while
`nelisp-native-startup-ensure' evaluates the native startup companion, while
the three compile steps before it evaluate the same companion fine.  The
binary, companion, fixtures and compiled exit-cache are byte-identical to the
last passing run.  This script, run on a Windows runner against a downloaded
reader build, repeats the steps exactly as the runner does and then varies one
thing at a time:

  compile-*      the three F1B compile steps (they create exit-cache);
  load, load-2   the failing step, twice (determinism);
  raw            the companion evaluated with no condition-case around it, so an
                 uncaught error prints the original backtrace;
  forms          the companion read and evaluated form by form in Lisp, naming
                 the first failing form;
  load-pad*      the failing step with 64 B .. 16 KB of extra environment
                 (layout sensitivity, as the Linux flake showed);
  load-nogc      the failing step without F1_FORCE_GC;
  load-cold      the failing step from the cold image.

Prints one DIAG line per step and the head of its output.
"""
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

binary = Path(sys.argv[1]).resolve(strict=True)
cold = Path(str(binary) + '.cold')
work = ROOT / 'target/diag-f1b'
work.mkdir(parents=True, exist_ok=False)
source = work / 'fixture.el'
base = dict(os.environ, F1_SOURCE=str(source), F1_FIXTURE=str(source.with_suffix('.elc')),
            F1_BACKEND='in-house', F1_FORCE_GC='1', NELISP_NATIVE_CACHE=str(work / 'cache'))
extra = dict(F1B_FIXTURE=str(ROOT / 'test/fixtures/diag-f1b-exits.elc'), F1B_BACKEND='in-house',
             F1B_COLD='0', NELISP_NATIVE_CACHE=str(work / 'exit-cache'))
print('DIAG binary', f1.digest(binary), 'companion', f1.digest(Path(str(binary) + '.native-startup.el')),
      'fixture', f1.digest(ROOT / 'test/fixtures/diag-f1b-exits.elc'), flush=True)


def go(phase, driver, changes, image=None):
    env = dict(base, **extra)
    for key, value in changes.items():
        if value is None:
            env.pop(key, None)
        else:
            env[key] = value
    result = f1.run(f1.reader_command(binary, image, driver), env, work, phase, deadline=600)
    out = (work / (phase + '.out')).read_text(errors='replace').splitlines()
    err = (work / (phase + '.err')).read_text(errors='replace').splitlines()
    print('DIAG %s rc=%s seconds=%.1f' % (phase, result['rc'], result['seconds']), flush=True)
    for line in out[-6:] + err[:30]:
        print('    ' + line[:400], flush=True)
    return result['rc']


for unit in ('f1b-one', 'f1b-zero', 'f1b-six'):
    go('compile-' + unit, DRIVER, dict(F1B_PHASE='compile', F1B_COMPILE_UNIT=unit))
# Iteration 2: without F1_FORCE_GC the load failed deterministically in 21 s with
# void-function (nil); with it, it passed.  Discriminate the mechanism.
NOGC = dict(F1B_PHASE='load', F1_FORCE_GC=None)
# Iteration 3: the nogc failure is deterministic but vanishes with any env
# padding or the checked allocator (layout-dependent).  Toggle mechanisms.
WRAP = 'test/diag-f1b-wrap.el'
go('load-nogc', DRIVER, NOGC)
go('wrap-none', WRAP, dict(NOGC, DIAG_WRAP=''))
go('wrap-nomidform', WRAP, dict(NOGC, DIAG_WRAP='nomidform'))
go('wrap-nomxcache', WRAP, dict(NOGC, DIAG_WRAP='nomxcache'))
go('wrap-threshold', WRAP, dict(NOGC, DIAG_WRAP='threshold'))
for pad in (8, 16, 24, 32, 40, 48, 56):
    go('load-nogc-pad%d' % pad, DRIVER, dict(NOGC, DIAG_PAD='x' * pad))
