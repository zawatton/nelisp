#!/usr/bin/env python3
"""Negative controls for pinned GC telemetry and Callgrind attribution."""
import importlib.util
import contextlib
import io
from pathlib import Path
import tempfile
import unittest
from unittest import mock

spec = importlib.util.spec_from_file_location(
    'cadence', Path(__file__).resolve().parents[1] / 'tools/ai/gc-cadence.py')
gate = importlib.util.module_from_spec(spec)
spec.loader.exec_module(gate)


class RawEvidenceTests(unittest.TestCase):
    def snapshot(self, text):
        with tempfile.TemporaryDirectory() as directory:
            path = Path(directory) / 'sample.out'
            path.write_text(text)
            return gate.snapshot(path)

    def valid(self):
        labels = ['start', *map(str, range(20)), 'end']
        return ''.join(f'GC-STATS {label} {i+2} {i+1000} 200000000 600000000 800000000 700000000\n'
                       for i, label in enumerate(labels)) + 'GC-DONE 1330840\n'

    def test_complete_output(self):
        self.assertEqual(self.snapshot(self.valid())['collections'], 23)

    def test_missing_row(self):
        with self.assertRaises(ValueError):
            self.snapshot(self.valid().replace('GC-STATS 3 ', 'not-a-row 3 '))

    def test_duplicate_round(self):
        with self.assertRaises(ValueError):
            self.snapshot(self.valid().replace('GC-STATS 3 ', 'GC-STATS 4 '))

    def test_missing_semantic_completion(self):
        with self.assertRaises(ValueError):
            self.snapshot(self.valid().replace('GC-DONE', 'unfinished'))

    def test_small_live_heap(self):
        with self.assertRaises(ValueError):
            self.snapshot(self.valid().replace('200000000', '1000000', 1))

    def test_zero_allocation_is_not_measurement(self):
        with self.assertRaises(ValueError):
            self.snapshot(self.valid().replace('start 2 1000', 'start 2 0'))

    def test_impossible_peak(self):
        with self.assertRaises(ValueError):
            self.snapshot(self.valid().replace('800000000 700000000', '1 700000000', 1))

    def test_backwards_allocation_counter(self):
        with self.assertRaises(ValueError):
            self.snapshot(self.valid().replace('end 23 1021', 'end 23 1'))

    def profile(self, total=100):
        with tempfile.TemporaryDirectory() as directory:
            path = Path(directory) / 'sample.cg'
            path.write_text(f'''summary: {total}
fn=(1) 0x1000
0 10
cfn=(2) 0x2000
calls=2 0
0 90
fn=(2)
0 30
cfn=(3) 0x2000'2
calls=4 0
0 60
fn=(3)
0 60
''')
            with mock.patch.object(gate, 'symbols', return_value={
                    'nl_gc_collect': 0x1000, 'nl_gc_sweep_merge_chunks': 0x2000}):
                return gate.profile(path, 'unused')

    def test_self_costs_and_recursive_sweeps_are_separate(self):
        total, own, edges, calls = self.profile()
        self.assertEqual(total, sum(own.values()))
        self.assertEqual(calls[('nl_gc_collect', 'nl_gc_sweep_merge_chunks')], 2)
        self.assertEqual(calls[('nl_gc_sweep_merge_chunks', 'nl_gc_sweep_merge_chunks')], 4)
        self.assertEqual(edges[('nl_gc_collect', 'nl_gc_sweep_merge_chunks')], 90)

    def test_fake_positive_summary_is_red(self):
        with self.assertRaises(ValueError):
            self.profile(99)

    def test_manifest_missing_a_layout_is_red(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            (root / 'head.json').write_text('{"binary":"unused","identity":{},"artifacts":{}}')
            with mock.patch.object(gate, 'identity', return_value={}):
                with self.assertRaisesRegex(ValueError, 'Incomplete artifact'):
                    gate.samples('head', root)

    def test_manifest_with_changed_binary_is_red(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            (root / 'head.json').write_text('{"binary":"unused","identity":{},"artifacts":{}}')
            with mock.patch.object(gate, 'identity', return_value={'new': 'sha'}):
                with self.assertRaisesRegex(ValueError, 'Stale measurement'):
                    gate.samples('head', root)


class ReplEvidenceTests(unittest.TestCase):
    def valid(self):
        return ('GC-CADENCE-COLD-REPL PASS\nGC-CADENCE-COST-PUBLISHED PASS\n'
                'nelisp> nelisp: uncaught error: in <repl>, top-level form #1 '
                '(byte 0, line 1): void-function: (nelisp-runtime-restore-originals)\n'
                'GC-CADENCE-COST-RESTORED PASS\n')

    def test_clean_session(self):
        gate.verify_repl('\n'.join(line for line in self.valid().splitlines()
                                   if 'uncaught error' not in line))

    def test_recovered_diagnostic_error(self):
        gate.verify_repl(self.valid())

    def test_unknown_error_is_red(self):
        with self.assertRaisesRegex(ValueError, 'Unexpected'):
            gate.verify_repl(self.valid().replace('void-function', 'heap-corruption'))

    def test_missing_current_publication_is_red(self):
        with self.assertRaisesRegex(ValueError, 'not demonstrated'):
            gate.verify_repl(self.valid().replace('GC-CADENCE-COST-PUBLISHED PASS', ''))


class ResumeTests(unittest.TestCase):
    def fake_run(self, args, **kwargs):
        profile = next(x.split('=', 1)[1] for x in args if x.startswith('--callgrind-out-file='))
        Path(profile).write_text('summary: 100\nfn=(1) nl_gc_collect\n0 100\n')
        kwargs['stdout'].write(RawEvidenceTests().valid())
        return 0

    def test_completed_layouts_survive_interruption_before_group_record(self):
        with tempfile.TemporaryDirectory() as directory, \
             mock.patch.object(gate, 'identity', return_value={'pin': 'sha'}), \
             mock.patch.object(gate, 'symbols', return_value={'nl_gc_stats': 123}), \
             mock.patch.object(gate.abi, 'profile_counts', return_value=(100, 23)), \
             mock.patch.object(gate.abi, 'run_process', side_effect=self.fake_run) as runner, \
             contextlib.redirect_stdout(io.StringIO()):
            root = Path(directory)
            gate.measure('head', Path('unused'), root)
            self.assertEqual(runner.call_count, 4)
            (root / 'head.json').unlink()
            runner.reset_mock()
            gate.measure('head', Path('unused'), root, resume=True)
            self.assertEqual(runner.call_count, 0)
            self.assertTrue((root / 'head.json').is_file())
            (root / 'head.json').unlink()
            (root / 'head-0.out').write_text('corrupted after checkpoint')
            with self.assertRaisesRegex(ValueError, 'artifact changed'):
                gate.measure('head', Path('unused'), root, resume=True)
            self.assertEqual(runner.call_count, 0)

    def test_incomplete_layout_is_preserved_and_rerun(self):
        with tempfile.TemporaryDirectory() as directory, \
             mock.patch.object(gate, 'identity', return_value={'pin': 'sha'}), \
             mock.patch.object(gate, 'symbols', return_value={'nl_gc_stats': 123}), \
             mock.patch.object(gate.abi, 'profile_counts', return_value=(100, 23)), \
             mock.patch.object(gate.abi, 'run_process', side_effect=self.fake_run) as runner, \
             contextlib.redirect_stdout(io.StringIO()):
            root = Path(directory)
            (root / 'head-0.cg').write_text('incomplete')
            (root / 'head-0-cache').mkdir()
            gate.measure('head', Path('unused'), root, resume=True)
            self.assertEqual(runner.call_count, 4)
            copies = list((root / 'interrupted').glob('*/head-0.cg'))
            self.assertEqual(len(copies), 1)
            self.assertEqual(copies[0].read_text(), 'incomplete')


class CadenceVerdictTests(unittest.TestCase):
    def test_same_count_at_less_allocation_is_more_frequent(self):
        self.assertFalse(gate.cadence_ok(dict(collections=12, allocated=95, peak=90),
                                         dict(collections=12, allocated=100, peak=100)))

    def test_fewer_cycles_cannot_hide_a_higher_peak(self):
        self.assertFalse(gate.cadence_ok(dict(collections=10, allocated=95, peak=101),
                                         dict(collections=12, allocated=100, peak=100)))

    def test_lower_rate_and_peak_pass(self):
        self.assertTrue(gate.cadence_ok(dict(collections=11, allocated=95, peak=90),
                                        dict(collections=12, allocated=100, peak=100)))


class ScopeCostTests(unittest.TestCase):
    def test_mutual_recursion_is_charged_once_from_self_cost(self):
        own = dict(main=5, root=10, a=20, b=30, shared=40)
        edges = {('main', 'root'): 1, ('root', 'a'): 1,
                 ('a', 'b'): 1, ('b', 'a'): 1,
                 ('a', 'shared'): 30, ('main', 'shared'): 10}
        self.assertEqual(gate.scope_cost(('root',), own, edges), (90, 60, 30))
        self.assertEqual(gate.scope_cost(('a',), own, edges), (80, 50, 30))

    def test_shared_helper_subtree_is_not_charged_as_gc_self(self):
        own = dict(main=5, root=10, shared=40, child=20)
        edges = {('main', 'root'): 1, ('main', 'shared'): 30,
                 ('root', 'shared'): 30, ('shared', 'child'): 20}
        self.assertEqual(gate.scope_cost(('root',), own, edges), (40, 10, 30))


if __name__ == '__main__':
    unittest.main()
