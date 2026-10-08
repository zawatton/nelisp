#!/usr/bin/env python3
"""Exercise scenario shutdown supervision with a real, controlled PTY child."""
import importlib.util
import json
from pathlib import Path
import tempfile
import time
from types import SimpleNamespace
import unittest
from unittest.mock import patch

ROOT = Path(__file__).resolve().parents[2]


def load_tool(name, filename):
    spec = importlib.util.spec_from_file_location(name, ROOT / 'tools' / filename)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


smoke = load_tool('pty_smoke', 'nemacs-pty-smoke.py')
layout = load_tool('redisplay_layout', 'redisplay-layout-parity.py')

CHILD = r'''#!/usr/bin/env python3
import os
import termios
import time
import tty

saved = termios.tcgetattr(0)
tty.setraw(0)
os.write(1, b'*scratch*\x1b[?25h\x1b[1;1H')
keys = b''
while not keys.endswith(b'\x18\x03'):
    keys += os.read(0, 1024)
time.sleep(DELAY)
termios.tcsetattr(0, termios.TCSANOW, saved)
os._exit(EXIT)
'''


class ScenarioShutdownTest(unittest.TestCase):
    def capture(self, delay, exit_code=0, timeout=20):
        scratch = ROOT / 'build/nemacs-pty-smoke-tests'
        scratch.mkdir(parents=True, exist_ok=True)
        with tempfile.TemporaryDirectory(prefix='pty-shutdown-', dir=scratch) as temporary:
            root = Path(temporary)
            (root / 'bin').mkdir()
            launcher = root / 'bin/nemacs-nw'
            launcher.write_text(CHILD.replace('DELAY', repr(delay))
                                .replace('EXIT', str(exit_code)))
            launcher.chmod(0o755)
            output = root / 'output'
            output.mkdir()
            args = SimpleNamespace(lib=root, binary='/unused-runtime', output=output,
                                   timeout=timeout, diagnostic=False)
            init = output / 'init.el'
            init.write_text('')
            start = time.monotonic()
            with patch.object(smoke, 'scenario_steps', return_value=[('quit', b'\x18\x03')]):
                result, _ = smoke.scenario_capture(args, 'nelisp', output / 'fixture',
                                                   init, layout.Screen)
            # Check the persisted verdict as well as the in-memory result.
            self.assertEqual(result['checks'], json.loads(
                (output / 'scenario.nelisp.json').read_text())['checks'])
            return result, time.monotonic() - start

    def test_slow_shutdown_exits_and_restores_tty(self):
        # Exceeds the old 1.5s settlement + 5s grace, like a heap GC at quit.
        result, _ = self.capture(8)
        self.assertTrue(all(result['checks'].values()), result['checks'])
        self.assertEqual(result['exit'], 0)
        self.assertGreaterEqual(result['shutdown_seconds'], 8)

    def test_hung_shutdown_still_obeys_editor_deadline(self):
        result, elapsed = self.capture(60, timeout=3)
        self.assertFalse(result['checks']['no_hang'])
        self.assertFalse(result['checks']['exit_zero'])
        self.assertEqual(result['exit'], -9)
        self.assertLess(elapsed, 5)

    def test_nonzero_exit_still_fails(self):
        result, _ = self.capture(0, exit_code=7)
        self.assertTrue(result['checks']['no_hang'])
        self.assertTrue(result['checks']['tty_restored'])
        self.assertFalse(result['checks']['exit_zero'])
        self.assertEqual(result['exit'], 7)


if __name__ == '__main__':
    unittest.main()
