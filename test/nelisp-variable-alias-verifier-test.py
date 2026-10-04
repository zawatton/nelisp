"""Calibrate alias parity evidence without starting either runtime."""
import contextlib
import importlib.util
import io
from pathlib import Path
import subprocess
import sys
from unittest.mock import patch

spec = importlib.util.spec_from_file_location(
    'alias_probe', Path(__file__).with_name('nelisp-variable-alias-test.py'))
probe = importlib.util.module_from_spec(spec)
spec.loader.exec_module(probe)
healthy = (b'(nelisp-route-base t 17)\n'
           b'((first x y nil) (first x y nil) (second z w nil))\n')
defect = b'(nelisp-route-base t 17)\n(y (first x y nil) w)\n'


def invoke(args, records):
    with patch.object(sys, 'argv', ['probe'] + args), \
            patch.object(probe, 'run', side_effect=records) as launch, \
            contextlib.redirect_stdout(io.StringIO()), \
            contextlib.redirect_stderr(io.StringIO()):
        try:
            code = probe.main()
        except SystemExit as result:
            code = result.code
        return code, launch.call_count


def record(output, code=0, errors=b''):
    return subprocess.CompletedProcess(['unused'], code, output, errors)


checked = 0
for args, code in ((['--help'], 0), (['--bad'], 2), (['--provider'], 2),
                   (['--expect-defect'], 2)):
    actual, calls = invoke(args, [])
    assert actual == code and calls == 0
    checked += 1

version = record(b'GNU Emacs 31.1\n')
for output, code, errors, succeeds in (
        (defect, 0, b'', True), (healthy, 0, b'', False),
        (b'', 0, b'', False), (defect[:-1], 0, b'', False),
        (defect, 1, b'', False), (defect, 0, b'failure', False),
        (b'(unrelated-error)\n', 0, b'', False)):
    actual, calls = invoke(['--expect-defect', 'unused'],
                           [version, record(healthy), record(output, code, errors)])
    assert (actual == 0) == succeeds and calls == 3
    checked += 1
for baseline in (record(b''), record(healthy, 1), record(healthy, 0, b'failure')):
    actual, calls = invoke(['--routing'], [version, baseline])
    assert actual != 0 and calls == 2
    checked += 1
actual, calls = invoke(['--routing'], [version, record(healthy), record(healthy)])
assert actual == 0 and calls == 3
checked += 1
print(f'GATE-COUNT checked={checked} findings=0; CLI runtime-startups=0')
