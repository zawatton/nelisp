#!/usr/bin/env python3
"""Controls for portable runners, wall deadlines and strict shard receipts."""
import copy
import importlib.util
import json
import os
from pathlib import Path
import subprocess
import sys
import tempfile
import time
import unittest
from unittest.mock import patch
import native_corpus_platform as platform


def module(name, filename):
    spec = importlib.util.spec_from_file_location(name, Path(__file__).with_name(filename))
    result = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(result)
    return result


u10 = module('u10', 'run-native-corpus-u10.py')
f3 = module('f3', 'run-native-real-corpus.py')
merge = module('merge', 'merge-native-u10-receipts.py')


class PlatformControls(unittest.TestCase):
    def tearDown(self):
        platform.WINE = False
        platform.WINDOWS = False
        platform._f1.RUN_DEADLINE = None

    def test_reader_paths_and_environment_are_separate(self):
        with patch.object(platform._f1, 'wine_path', side_effect=lambda p: 'Q:\\' + str(p).lstrip('/').replace('/', '\\')):
            platform.WINE = True
            env = dict(F3_FIXTURE='/fixture with spaces/f.el', EMACS='/host/emacs')
            converted = platform.reader_environment(env, ('F3_FIXTURE',))
            self.assertEqual(converted['F3_FIXTURE'], 'Q:\\fixture with spaces\\f.el')
            self.assertEqual(converted['EMACS'], env['EMACS'])
            self.assertEqual(env['F3_FIXTURE'], '/fixture with spaces/f.el')
            with tempfile.TemporaryDirectory() as directory:
                binary = Path(directory) / 'reader.exe'
                cold = Path(str(binary) + '.cold'); cold.write_bytes(b'image')
                command = platform.reader_command(binary, cold, u10.DRIVER)
            self.assertEqual(command[0], 'wine')
            self.assertEqual(command[2], '--cold-load-from')
            self.assertEqual(command[-2], '-l')
            self.assertTrue(command[-1].startswith('Q:\\'))

    def test_wine_cache_path_is_confined(self):
        platform.WINE = True
        with patch.object(platform._f1, 'wine_path', return_value='Q:\\cache with spaces'):
            cache = Path('/cache with spaces')
            self.assertEqual(platform.cache_file('Q:/cache with spaces/unit/f.nelr', cache), cache / 'unit/f.nelr')
            for bad in ('Q:/other/f', 'Q:/cache with spaces/../f', 'Q:/cache with spaces/unit/../../f',
                        'Q:/cache with spaces/unit/C:/f', 'Q:/cache with spaces//f'):
                with self.assertRaises(ValueError): platform.cache_file(bad, cache)

    def test_real_process_timeout_without_shell_timeout(self):
        with tempfile.TemporaryDirectory() as directory:
            work = Path(directory)
            rc, seconds, _, _ = platform.run_process([sys.executable, '-c', 'import time; time.sleep(30)'],
                                                     os.environ.copy(), work, 'timeout', deadline=0.15)
            self.assertEqual(rc, 124)
            self.assertLess(seconds, 3)
            rc, _, output, errors = platform.run_process([sys.executable, '-c', 'print("portable")'],
                                                         os.environ.copy(), work, 'pass', deadline=3)
            self.assertEqual((rc, output, errors), (0, 'portable\n', ''))

    def test_expired_wall_budget_does_not_launch_a_child(self):
        platform._f1.RUN_DEADLINE = time.monotonic() - 1
        with tempfile.TemporaryDirectory() as directory, patch.object(platform._f1.subprocess, 'Popen') as launch:
            self.assertEqual(platform.run_process(['missing-program'], {}, Path(directory), 'expired')[0], 124)
            launch.assert_not_called()

    def test_missing_external_timeout_is_no_longer_a_blocker(self):
        with tempfile.TemporaryDirectory() as directory:
            environment = dict(os.environ, PATH=directory)
            command = [sys.executable, '-c', 'print("no-shell-timeout")']
            # This is the old corpus runner's process boundary. Windows has
            # no GNU timeout; a minimal PATH reproduces the dependency defect.
            with self.assertRaises((FileNotFoundError, subprocess.CalledProcessError)):
                subprocess.run(['timeout', '-k', '5', '290', *command], env=environment,
                               check=True, capture_output=True, timeout=3)
            rc, _, output, errors = platform.run_process(command, environment, Path(directory), 'portable', deadline=3)
            self.assertEqual((rc, output, errors), (0, 'no-shell-timeout\n', ''))

    def test_windows_load_average_is_optional(self):
        with patch.object(platform.os, 'getloadavg', new=None, create=True):
            self.assertEqual(platform.load_text(platform.load_average()), 'unavailable')

    def test_windows_cache_is_created_by_the_reader(self):
        with tempfile.TemporaryDirectory() as directory:
            path = Path(directory) / 'cache'
            platform.WINDOWS = True
            self.assertEqual(platform.create_cache(path), path)
            self.assertFalse(path.exists())
            platform.WINDOWS = False
            platform.create_cache(path)
            self.assertTrue(path.is_dir())

    def test_deadline_receipts_remain_strict(self):
        output = ('F3-FUNCTION-PASS backend=in-house name=one phase=load cases=2 native-entries=2 validations=0 seconds=1\n'
                  'F3-BATCH-DONE backend=in-house phase=load functions=1 failed=0\n')
        args = ('in-house', 'load', ['one'], 0, 500, output, '', {'one': 2})
        self.assertFalse(f3.phase_verdict(*args)[0])
        self.assertTrue(f3.phase_verdict(*args, deadline=900)[0])
        for text in ('', output + output, output.replace('cases=2', 'cases=1'),
                     output.replace('validations=0', 'validations=1')):
            self.assertFalse(f3.phase_verdict(*args[:5], text, '', {'one': 2}, deadline=900)[0])


class ShardControls(unittest.TestCase):
    def make_receipts(self, root):
        # Synthetic full coverage exercises the merger only, never native execution.
        paths = []
        canonical = json.loads((u10.ROOT / 'test/fixtures/native-bytecode/gnu-31.1-valid-fixtures.json').read_text())
        admitted = sorted(row['opcode'] for row in canonical['valid'])
        metadata = dict(fixtures=[dict(opcode=op, relocation_refusal=False) for op in admitted],
                        pending=[], projections={str(op): 'pin' for op in admitted + ['protected']})
        for shard in range(2):
            directory = root / str(shard); directory.mkdir()
            work = directory / 'in-house'; work.mkdir()
            selected = admitted[shard::2]
            groups = [selected[i:i+8] for i in range(0, len(selected), 8)]
            if shard == 0: groups.append(['protected'])
            receipts = []
            for phase in ('compile', 'load'):
                for group in groups:
                    opcodes = [op for op in group if op != 'protected']
                    label = '-'.join(map(str, group)) + '-' + phase
                    fixture = work / (label + '-fixtures.el'); fixture.write_text('(fixture)\n')
                    output = ''.join(f'U10-FIXTURE-PASS backend=in-house opcode={op} phase={phase} entries=1 gc=1\n' for op in opcodes)
                    if 192 in opcodes:
                        output += ''.join('U10-STALE-PASS control=' + control + '\n' for control in ('input', 'abi', 'artifact'))
                    if 'protected' in group: output += 'U10-ORDER-PASS cases=4 entries=4\n'
                    output += f'U10-BATCH-DONE backend=in-house phase={phase} fixtures={len(opcodes)} entries={len(opcodes)}\n'
                    (work / (label + '.out')).write_text(output)
                    (work / (label + '.err')).write_text('')
                    receipt = dict(backend='in-house', platform='windows', passed=True, opcodes=group, phase=phase,
                                   rc=0, seconds=1, process_deadline=900, worker_fixture_sha256=u10.digest(fixture),
                                   projection_sha256={str(op): 'pin' for op in group}, sources_sha256={})
                    for key in ('binary_sha256', 'cold_sha256', 'startup_sha256', 'fixture_sha256',
                                'platform_runner_sha256', 'process_runner_sha256'): receipt[key] = 'pin'
                    receipts.append(receipt)
            path = directory / 'summary.json'
            path.write_text(json.dumps(dict(passed=True, pending=[], focused=False, selected=selected,
                                            shard=[shard, 2], receipts=receipts)))
            (directory / 'audit.json').write_text(json.dumps(metadata))
            paths.append(path)
        return paths

    def test_complete_shards_and_negative_controls(self):
        with tempfile.TemporaryDirectory() as scratch:
            paths = self.make_receipts(Path(scratch))
            self.assertEqual(merge.verify(paths)['fixtures'], 230)
            for selection in ([], paths[:1], paths + paths[:1], [paths[0], paths[0]]):
                with self.assertRaises(ValueError): merge.verify(selection)
            baseline = json.loads(paths[0].read_text())
            for mutation in ('pending', 'focused', 'wine', 'image', 'coverage', 'projection', 'deadline', 'exit'):
                broken = copy.deepcopy(baseline)
                if mutation == 'pending': broken['pending'] = [48]
                elif mutation == 'focused': broken['focused'] = True
                elif mutation == 'wine': broken['receipts'][0]['platform'] = 'wine'
                elif mutation == 'image': broken['receipts'][0]['cold_sha256'] = None
                elif mutation == 'coverage': broken['receipts'].pop()
                elif mutation == 'projection': broken['receipts'][0]['projection_sha256'][str(broken['receipts'][0]['opcodes'][0])] = 'changed'
                elif mutation == 'deadline': broken['receipts'][0]['process_deadline'] = 20000
                else: broken['receipts'][0]['rc'] = 124
                paths[0].write_text(json.dumps(broken))
                with self.assertRaises(ValueError, msg=mutation): merge.verify(paths)
                paths[0].write_text(json.dumps(baseline))
            self.assertTrue(merge.verify(paths)['passed'])


if __name__ == '__main__': unittest.main()
