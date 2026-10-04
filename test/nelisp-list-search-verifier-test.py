"""Reject false defect evidence and prove options launch no runtimes."""
import contextlib
import importlib.util
import io
from pathlib import Path
import subprocess
from unittest.mock import patch

spec = importlib.util.spec_from_file_location(
    'search_probe', Path(__file__).with_name('nelisp-list-search-arity-test.py'))
probe = importlib.util.module_from_spec(spec)
spec.loader.exec_module(probe)
checked = 0
for arguments, code in ((['--help'], 0), (['--bad'], 2), (['--expect-defect'], 2)):
    with patch.object(probe.subprocess, 'run', side_effect=AssertionError('runtime started')) as launch, \
            contextlib.redirect_stdout(io.StringIO()), contextlib.redirect_stderr(io.StringIO()):
        try:
            probe.main(arguments)
        except SystemExit as result:
            assert result.code == code
        else:
            raise AssertionError('missing CLI exit')
        assert launch.call_count == 0
        checked += 1

records = probe.corpus()
expected = b''.join((row[2] + '\n').encode() for row in records)
gnu = subprocess.CompletedProcess(['gnu'], 0, expected, b'')
wrong = expected.splitlines(keepends=True)
wrong[1] = b'(division-by-zero)\n'
controls = [(0, expected, b''), (139, b'', b''), (1, b'', b'failure'),
            (0, b''.join(wrong), b''), (0, expected[:-1], b'')]
for code, output, errors in controls:
    with patch.object(probe.subprocess, 'run', side_effect=[
            gnu, subprocess.CompletedProcess(['native'], code, output, errors)]), \
            contextlib.redirect_stdout(io.StringIO()), contextlib.redirect_stderr(io.StringIO()):
        assert probe.main(['--expect-defect', 'unused']) == 1
        checked += 1

for arguments, code, output in ((['--expect-defect', 'unused'], -11,
                               expected.splitlines(keepends=True)[0]),
                              ([], 0, expected)):
    with patch.object(probe.subprocess, 'run', side_effect=[
            gnu, subprocess.CompletedProcess(['native'], code, output, b'')]), \
            contextlib.redirect_stdout(io.StringIO()), contextlib.redirect_stderr(io.StringIO()):
        assert probe.main(arguments) == 0
        checked += 1
with patch.object(probe.subprocess, 'run', return_value=
                  subprocess.CompletedProcess(['gnu'], 0, b'', b'')) as launch, \
        contextlib.redirect_stdout(io.StringIO()), contextlib.redirect_stderr(io.StringIO()):
    assert probe.main([]) == 2
    assert launch.call_count == 1
    checked += 1
print(f'GATE-COUNT checked={checked} findings=0; CLI runtime-startups=0')
