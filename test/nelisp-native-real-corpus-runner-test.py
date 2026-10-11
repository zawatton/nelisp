"""F3 backend/compiler/loader pools must share one two-process limit."""
from pathlib import Path
import concurrent.futures
import importlib.util
import subprocess
import tempfile
import threading
import time
import unittest
from unittest.mock import patch

ROOT = Path(__file__).resolve().parents[1]
SPEC = importlib.util.spec_from_file_location(
    'f3_runner', ROOT / 'test/support/run-native-real-corpus.py')
RUNNER = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(RUNNER)


class ProcessLimitTest(unittest.TestCase):
    def test_independent_pools_share_limit_and_keep_reader_deadline(self):
        active = peak = calls = 0
        lock = threading.Lock()

        def child(command, **_):
            nonlocal active, peak, calls
            self.assertEqual(command[:4], ['timeout', '-k', '5', '290'])
            with lock:
                active += 1
                peak = max(peak, active)
                calls += 1
            time.sleep(0.04)
            with lock:
                active -= 1
            return subprocess.CompletedProcess(command, 0)

        with tempfile.TemporaryDirectory() as directory, patch.object(
                RUNNER.subprocess, 'run', side_effect=child):
            def backend(index):
                with concurrent.futures.ThreadPoolExecutor(max_workers=3) as pool:
                    return list(pool.map(lambda job: RUNNER.run_process(
                        ['reader'], {}, Path(directory), f'{index}-{job}'), range(3)))
            with concurrent.futures.ThreadPoolExecutor(max_workers=2) as backends:
                results = list(backends.map(backend, range(2)))
        self.assertEqual(calls, 6)
        self.assertEqual(peak, 2)
        self.assertTrue(all(row[0] == 0 for batch in results for row in batch))

    def test_launch_failure_releases_both_slots(self):
        with tempfile.TemporaryDirectory() as directory:
            with patch.object(RUNNER.subprocess, 'run', side_effect=OSError('launch')):
                for i in range(2):
                    with self.assertRaises(OSError):
                        RUNNER.run_process(['reader'], {}, Path(directory), f'failed-{i}')
            self.assertEqual(RUNNER._active_processes, 0)
            with patch.object(RUNNER.subprocess, 'run', return_value=subprocess.CompletedProcess([], 0)):
                self.assertEqual(RUNNER.run_process(['reader'], {}, Path(directory), 'recovered')[0], 0)


if __name__ == '__main__':
    unittest.main()
