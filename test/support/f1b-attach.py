#!/usr/bin/env python3
"""Start one F1B load run normally, then attach cdb to it after --delay seconds.

Launching under cdb hides the F1B failure (cdb -hd even injects
_NO_DEBUG_HEAP into the debuggee), so the debuggee starts with exactly the
environment of f1b-fast-sweep.py and cdb attaches late.  Use hardware
breakpoints only (ba e1 / ba w8): software breakpoints trip the native
runtime's helper-memory check.

Usage (from the repository root):
  python test/support/f1b-attach.py target\\nelisp.exe PAD CDB_SCRIPT OUT --fixture EXITS.elc
      [--delay 3] [--timeout 600] [--suspended] [--base-env-len 5468] [--extra NAME=VALUE]

The debuggee's stdout/stderr go to OUT.nelisp.out / OUT.nelisp.err and cdb's
output to OUT.  --work must name a directory whose exit-cache was built by
f1b-fast-sweep.py.
"""
import argparse
import importlib.util
import os
import subprocess
import sys
import time
from pathlib import Path

HERE = Path(__file__).resolve().parent
spec = importlib.util.spec_from_file_location('sweep', HERE / 'f1b-fast-sweep.py')
sweep = importlib.util.module_from_spec(spec)
spec.loader.exec_module(sweep)
CDB = os.environ.get('CDB', r'C:\Program Files (x86)\Windows Kits\10\Debuggers\x64\cdb.exe')
CREATE_SUSPENDED = 0x4

parser = argparse.ArgumentParser()
parser.add_argument('binary', type=Path)
parser.add_argument('pad', type=int)
parser.add_argument('script', type=Path)
parser.add_argument('out', type=Path)
parser.add_argument('--fixture', type=Path, required=True)
parser.add_argument('--work', type=Path, default=sweep.f1.ROOT / 'target/diag-f1b-sweep')
parser.add_argument('--padvar', default='DIAG_PAD')
parser.add_argument('--extra', action='append', default=[], metavar='NAME=VALUE')
parser.add_argument('--base-env-len', type=int, help='required base environment size in chars')
parser.add_argument('--delay', type=float, default=3.0)
parser.add_argument('--timeout', type=float, default=600)
parser.add_argument('--suspended', action='store_true', help='create suspended; cdb -pr resumes it')
args = parser.parse_args()

work = args.work.resolve()
if not (work / 'exit-cache').exists():
    sys.exit('no exit-cache in %s; run f1b-fast-sweep.py first' % work)
env = sweep.base_environment(args.fixture.resolve(strict=True), work, args.extra)
sweep.check_base_environment(env, args.base_env_len)
env[args.padvar] = 'x' * args.pad
command = sweep.f1.reader_command(args.binary.resolve(strict=True), None, sweep.DRIVER)
# The debugger must not change the debuggee's environment: cdb gets the
# parent environment, not the padded one.
with open(str(args.out) + '.nelisp.out', 'wb') as output, \
        open(str(args.out) + '.nelisp.err', 'wb') as errors, args.out.open('wb') as log:
    process = subprocess.Popen(command, env=env, cwd=sweep.f1.ROOT, stdout=output, stderr=errors,
                               creationflags=subprocess.CREATE_NEW_PROCESS_GROUP
                               | (CREATE_SUSPENDED if args.suspended else 0))
    time.sleep(args.delay)
    try:
        rc = subprocess.run([CDB] + (['-pr'] if args.suspended else [])
                            + ['-p', str(process.pid), '-cf', str(args.script.resolve())],
                            stdout=log, stderr=subprocess.STDOUT, stdin=subprocess.DEVNULL,
                            timeout=args.timeout).returncode
    except subprocess.TimeoutExpired:
        rc = 'timeout'
    if process.poll() is None:
        subprocess.run(['taskkill', '/PID', str(process.pid), '/T', '/F'],
                       stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
    print('ATTACH cdb-rc=%s nelisp-rc=%s' % (rc, process.wait()))
