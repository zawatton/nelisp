"""Reject unhealthy parity evidence and prove CLI requests launch nothing."""
import contextlib
import hashlib
import importlib.util
import io
from pathlib import Path
import subprocess
import sys
from unittest.mock import patch

spec = importlib.util.spec_from_file_location(
    'focused', Path(__file__).with_name('nelisp-focused-parity.py'))
focused = importlib.util.module_from_spec(spec)
spec.loader.exec_module(focused)
healthy, broken = b'healthy\n', b'broken\n'
signature = hashlib.sha256(broken).hexdigest()


def invoke(args, results):
    with patch.object(sys, 'argv', ['probe'] + args), \
            patch.object(focused, 'run', side_effect=results) as launches, \
            contextlib.redirect_stdout(io.StringIO()), contextlib.redirect_stderr(io.StringIO()):
        try:
            status = focused.main(['t'], defect=signature)
        except SystemExit as result:
            status = result.code
        return status, launches.call_count


def record(output, code=0, errors=b''):
    return subprocess.CompletedProcess(['unused'], code, output, errors)


checked = 0
for args, expected in ((['--help'], 0), (['--bad'], 2), (['--expect-defect'], 2)):
    code, launches = invoke(args, [])
    assert code == expected and launches == 0
    checked += 1
version = record(b'GNU Emacs 31.1\n')
for output, code, errors, expected in (
        (broken, 0, b'', True), (healthy, 0, b'', False),
        (b'', 0, b'', False), (broken[:-1], 0, b'', False),
        (broken, 1, b'', False), (broken, 0, b'failure', False),
        (b'unrelated\n', 0, b'', False)):
    result, launches = invoke(['--expect-defect', 'unused'],
        [version, record(healthy), record(output, code, errors)])
    assert (result == 0) == expected and launches == 3
    checked += 1
for baseline in (record(b''), record(healthy, 1), record(healthy, 0, b'failure')):
    code, launches = invoke([], [version, baseline])
    assert code != 0 and launches == 2
    checked += 1
code, launches = invoke([], [version, record(healthy), record(healthy)])
assert code == 0 and launches == 3
checked += 1
print(f'GATE-COUNT checked={checked} findings=0; CLI runtime-startups=0')
