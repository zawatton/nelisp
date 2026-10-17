#!/usr/bin/env python3
"""Fast Windows layout sweep for the F1B exit-load startup corruption.

The F1B failure (a GC marking gap fixed by de273349d) only fires for
particular memory layouts, which the process environment shifts: locally it
failed at exactly DIAG_PAD=216 and nowhere else in 0..512.  Each load run is
killed at --survive seconds; a run still alive then with no 'uncaught' line
has passed the failing point (form #1, about 20 s in) and counts as a pass.

The layout depends on the whole environment, not only the pad.  A 6-digit
instead of a 5-digit CLAUDE_PID was enough to hide the repro, so the base
environment (everything except the pad variable) is measured on every run:
the BASE-ENV line prints its size, and --base-env-len makes a mismatch fatal.
Override session-specific variables with --extra NAME=VALUE to match.

Usage (from the repository root):
  python test/support/f1b-fast-sweep.py target\\nelisp.exe --fixture EXITS.elc
      [--pads 216,216 | --start A --stop B --step S] [--all]
      [--base-env-len 5468] [--extra CLAUDE_PID=12345]

EXITS.elc is the byte-compiled exit fixture that run-windows-native-f1.py
writes as WORK/exits.elc.  The exit-cache under --work is built once with the
three compile units when it is missing.
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
DRIVER = 'test/standalone-native-funcall-v2-exits-driver.el'


def base_environment(fixture, work, extra):
    """Return the F1B load environment without the pad variable."""
    env = dict(os.environ, F1_BACKEND='in-house', F1B_FIXTURE=str(fixture), F1B_BACKEND='in-house',
               F1B_COLD='0', NELISP_NATIVE_CACHE=str(work / 'exit-cache'), F1B_PHASE='load')
    env.pop('F1_FORCE_GC', None)
    for item in extra:
        name, _, value = item.partition('=')
        env[name] = value
    return env


def check_base_environment(env, expected):
    """Print the base environment size; exit when it differs from EXPECTED."""
    size = sum(len(name) + len(value) + 2 for name, value in env.items())
    print('BASE-ENV entries=%d chars=%d' % (len(env), size), flush=True)
    if expected is not None and size != expected:
        sys.exit('base environment is %d chars, expected %d; adjust with --extra NAME=VALUE'
                 % (size, expected))


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('binary', type=Path)
    parser.add_argument('--fixture', type=Path, required=True)
    parser.add_argument('--work', type=Path, default=f1.ROOT / 'target/diag-f1b-sweep')
    parser.add_argument('--start', type=int, default=0)
    parser.add_argument('--stop', type=int, default=512)
    parser.add_argument('--step', type=int, default=8)
    parser.add_argument('--pads', default='', help='comma-separated pads; overrides --start/--stop/--step')
    parser.add_argument('--survive', type=float, default=75.0)
    parser.add_argument('--all', action='store_true', help='keep going after a failure')
    parser.add_argument('--padvar', default='DIAG_PAD')
    parser.add_argument('--extra', action='append', default=[], metavar='NAME=VALUE')
    parser.add_argument('--base-env-len', type=int, help='required base environment size in chars')
    parser.add_argument('--tag', default='')
    args = parser.parse_args()

    binary = args.binary.resolve(strict=True)
    fixture = args.fixture.resolve(strict=True)
    work = args.work.resolve()
    work.mkdir(parents=True, exist_ok=True)
    base = base_environment(fixture, work, args.extra)
    check_base_environment(base, args.base_env_len)
    command = f1.reader_command(binary, None, DRIVER)

    if not (work / 'exit-cache').exists():
        for unit in ('f1b-one', 'f1b-zero', 'f1b-six'):
            result = f1.run(command, dict(base, F1B_PHASE='compile', F1B_COMPILE_UNIT=unit),
                            work, 'compile-' + unit, deadline=900)
            print('COMPILE %-8s rc=%s %.1fs' % (unit, result['rc'], result['seconds']), flush=True)
            if result['rc']:
                sys.exit('compile failed; see ' + str(work))

    pads = ([int(pad) for pad in args.pads.split(',')] if args.pads
            else list(range(args.start, args.stop + 1, args.step)))
    failures = []
    for pad in pads:
        phase = 'fast%s-pad%d' % (args.tag, pad)
        result = f1.run(command, dict(base, **{args.padvar: 'x' * pad}), work, phase, deadline=args.survive)
        errors = (work / (phase + '.err')).read_text(errors='replace')
        line = next((l for l in errors.splitlines() if 'uncaught' in l), '')
        verdict = ('FAIL' if line else
                   'SURVIVED' if result['seconds'] >= args.survive - 1 else 'rc=%s' % result['rc'])
        print('FAST pad=%-5d %-8s %.1fs %s' % (pad, verdict, result['seconds'], line[-120:]), flush=True)
        if line:
            failures.append(pad)
            if not args.all:
                break
    print('FAST-SUMMARY failing pads:', failures or 'none', flush=True)


if __name__ == '__main__':
    main()
