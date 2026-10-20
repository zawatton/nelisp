#!/usr/bin/env python3
"""Negative controls for the Doc 215 four-layout measurement verdict."""
import contextlib
import importlib.util
import io
import json
import os
import signal
from pathlib import Path
import tempfile
import threading
import time
import unittest
from unittest import mock

spec = importlib.util.spec_from_file_location(
    "abi_ir", Path(__file__).resolve().parents[1] / "tools/ai/abi-ir.py")
gate = importlib.util.module_from_spec(spec)
spec.loader.exec_module(gate)


class LayoutVerdictTest(unittest.TestCase):
    def verdict(self, samples, mode="regex-gc"):
        with contextlib.redirect_stdout(io.StringIO()):
            return gate.summarize(mode, samples)

    def samples(self):
        return [(k, (100, 10, b"done"), (99, 10, b"done")) for k in gate.LAYOUTS]

    def test_mean_allows_one_regressing_layout(self):
        samples = self.samples()
        samples[0] = (0, (100, 10, b"done"), (102, 10, b"done"))
        self.assertTrue(self.verdict(samples))

    def test_regressing_mean_fails(self):
        samples = self.samples()
        samples[0] = (0, (100, 10, b"done"), (104, 10, b"done"))
        self.assertFalse(self.verdict(samples))

    def test_mismatching_output_in_one_layout_fails(self):
        samples = self.samples()
        samples[1] = (700, (100, 10, b"done"), (99, 10, b"bad"))
        self.assertFalse(self.verdict(samples))

    def test_common_output_difference_between_layouts_fails(self):
        samples = self.samples()
        samples[1] = (700, (100, 10, b"bad"), (99, 10, b"bad"))
        self.assertFalse(self.verdict(samples))

    def test_too_few_gc_cycles_in_one_layout_fails(self):
        samples = self.samples()
        samples[2] = (1400, (100, 9, b"done"), (99, 10, b"done"))
        self.assertFalse(self.verdict(samples))
        self.assertTrue(self.verdict(samples, mode="files"))

    def test_missing_layout_fails(self):
        self.assertFalse(self.verdict(self.samples()[:-1]))

    def test_duplicated_layout_cannot_replace_a_required_layout(self):
        samples = self.samples()
        samples[-1] = samples[0]
        self.assertFalse(self.verdict(samples))

    def test_plain_regex_keeps_its_own_ir_cap_without_the_gc_floor(self):
        samples = [(k, (100, 4, b"done"), (99, 4, b"done")) for k in gate.LAYOUTS]
        self.assertTrue(self.verdict(samples, mode="regex"))
        samples[0] = (0, (100, 4, b"done"), (104, 4, b"done"))
        self.assertFalse(self.verdict(samples, mode="regex"))


class ProfileCounterTest(unittest.TestCase):
    def count(self, body):
        with tempfile.TemporaryDirectory() as directory:
            profile = Path(directory) / "profile.cg"
            profile.write_text("summary: 100\n" + body)
            with mock.patch.object(gate.subprocess, "check_output", return_value=(
                    "00001000 T nl_gc_collect\n"
                    "00002000 T nl_gc_sweep_merge_chunks\n")):
                return gate.profile_counts(profile, "unused-binary")

    def test_inlined_sweep_counts_outer_calls_once(self):
        self.assertEqual((100, 11), self.count(
            "fn=(1) 0x0000000000001000\n"
            "cfn=(2) 0x0000000000002000\ncalls=11 0\n"
            "fn=(2)\ncfn=(3) 0x0000000000002000'2\ncalls=22 0\n"
            "fn=(3)\ncfn=(3)\ncalls=33 0\n"))

    def test_no_outer_calls_is_zero_even_with_recursive_edges(self):
        self.assertEqual((100, 0), self.count(
            "fn=(2) 0x0000000000002000\n"
            "cfn=(3) 0x0000000000002000'2\ncalls=20 0\n"))


class ProfileCommandTest(unittest.TestCase):
    def test_explicit_output_keeps_profiles_in_requested_directory(self):
        def profile(directory, mode, who, binary, layout, cancel=None):
            stem = directory / f"{mode}-{who}-{layout}"
            stem.with_suffix(".cg").write_text("fixture\n")
            stem.with_suffix(".err").write_text("")
            stem.with_suffix(".out").write_text("ABI-FILES (1000 527500)\n")
            return 100, 2
        with tempfile.TemporaryDirectory() as directory:
            output = Path(directory) / "permanent-evidence"
            started = time.monotonic()
            with mock.patch.object(gate.sys, "argv", [
                    "abi-ir.py", "files", "cand", "base", "--output", str(output)]), \
                    mock.patch.object(gate, "run_one", side_effect=profile) as jobs, \
                    contextlib.redirect_stdout(io.StringIO()):
                self.assertEqual(0, gate.main())
            self.assertEqual(8, jobs.call_count)
            self.assertTrue(all(call.args[0] == output for call in jobs.call_args_list))
            self.assertEqual(8, len(list(output.glob("*.cg"))))
            print(f"EXPLICIT-OUTPUT elapsed={time.monotonic() - started:.6f}s")

    def test_explicit_output_refuses_existing_evidence(self):
        with tempfile.TemporaryDirectory() as directory:
            output = Path(directory)
            sentinel = output / "retained.cg"
            sentinel.write_text("keep this evidence")
            with mock.patch.object(gate.sys, "argv", [
                    "abi-ir.py", "files", "cand", "base", "--output", str(output)]), \
                    mock.patch.object(gate, "run_one") as jobs:
                with self.assertRaises(FileExistsError):
                    gate.main()
            jobs.assert_not_called()
            self.assertEqual("keep this evidence", sentinel.read_text())

    def test_command_records_all_layouts_without_forced_collections(self):
        def profile(directory, mode, who, binary, layout, cancel=None):
            stem = directory / f"{mode}-{who}-{layout}"
            for suffix in ("cg", "err"):
                stem.with_suffix("." + suffix).write_text("fixture\n")
            stem.with_suffix(".out").write_text({
                "boot": "", "regex": "ABI-REGEX 6000\n",
                "regex-gc": "ABI-REGEX-GC 24000\n"}[mode])
            return (100 if who == "base" else 99), 10

        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            with mock.patch.object(gate, "ROOT", root), \
                    mock.patch.object(gate.sys, "argv", ["abi-ir.py", "ir", "cand", "base"]), \
                    mock.patch.object(gate, "identities", return_value={}), \
                    mock.patch.object(gate, "run_one", side_effect=profile) as jobs, \
                    contextlib.redirect_stdout(io.StringIO()):
                self.assertEqual(0, gate.main())
            self.assertEqual(24, jobs.call_count)
            output, = (root / "target/abi-ir").iterdir()
            gate.validate_record(output, {})
            for name, iterations in (("regex.el", 20), ("regex-gc.el", 80)):
                source = (output / name).read_text()
                self.assertNotIn("garbage-collect", source)
                self.assertIn(f"(while (< i {iterations})", source)


class ProfileCancellationTest(unittest.TestCase):
    def test_failed_job_does_not_wait_for_unrelated_profiles(self):
        started = threading.Event()
        def profile(tmp, mode, who, binary, layout, cancel=None):
            if mode == "boot" and who == "base" and layout == 0:
                if not started.wait(3):
                    raise AssertionError("sibling profile did not start")
                raise RuntimeError("deliberate profile failure")
            started.set()
            until = time.monotonic() + 1.5
            while time.monotonic() < until:
                if cancel is not None and cancel.is_set():
                    raise concurrent.futures.CancelledError()
                time.sleep(0.01)
            return 1, 10
        import concurrent.futures
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            before = time.monotonic()
            with mock.patch.object(gate, "ROOT", root), \
                    mock.patch.object(gate.sys, "argv", ["abi-ir.py", "ir", "cand", "base"]), \
                    mock.patch.object(gate, "identities", return_value={}), \
                    mock.patch.object(gate, "run_one", side_effect=profile), \
                    contextlib.redirect_stdout(io.StringIO()):
                with self.assertRaisesRegex(RuntimeError, "deliberate profile failure"):
                    gate.main()
            elapsed = time.monotonic() - before
            print(f"PROFILE-CANCEL elapsed={elapsed:.6f}s")
            self.assertLess(elapsed, 0.8)
            self.assertFalse(any((root / "target/abi-ir").glob("*/measurement.json")))

    def test_cancelled_process_is_reaped(self):
        import concurrent.futures
        cancel = threading.Event()
        original_popen = gate.subprocess.Popen
        processes = []
        def launch(*args, **kwargs):
            process = original_popen(*args, **kwargs)
            processes.append(process)
            cancel.set()
            return process
        with mock.patch.object(gate.subprocess, "Popen", side_effect=launch):
            with self.assertRaises(concurrent.futures.CancelledError):
                gate.run_process([gate.sys.executable, "-c", "import time; time.sleep(30)"],
                                 timeout=3, cancel=cancel)
        self.assertEqual(1, len(processes))
        self.assertIsNotNone(processes[0].poll())

    @unittest.skipUnless(os.name == "posix", "requires POSIX process groups")
    def test_sigterm_cancels_and_reaps_profile_groups(self):
        parent_code = r"""
import importlib.util, sys
from pathlib import Path
spec = importlib.util.spec_from_file_location("gate", sys.argv[1])
gate = importlib.util.module_from_spec(spec); spec.loader.exec_module(gate)
gate.ROOT = Path(sys.argv[2]); pid_file = Path(sys.argv[3])
gate.PROFILE_WORKERS = 1
gate.identities = lambda *_: {}
original_popen = gate.subprocess.Popen
def launch(*args, **kwargs):
    process = original_popen(*args, **kwargs)
    temporary = pid_file.with_suffix(".tmp")
    temporary.write_text(str(process.pid))
    temporary.replace(pid_file)
    return process
gate.subprocess.Popen = launch
def profile(tmp, mode, who, binary, layout, cancel):
    return gate.run_process([sys.executable, "-c", "import time; time.sleep(30)"],
                            timeout=30, cancel=cancel)
gate.run_one = profile
sys.argv = ["abi-ir.py", "ir", "cand", "base"]
gate.main()
"""
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            pid_file = root / "child.pid"
            parent = gate.subprocess.Popen(
                [gate.sys.executable, "-c", parent_code, str(Path(gate.__file__).resolve()),
                 str(root), str(pid_file)], stdout=gate.subprocess.DEVNULL,
                stderr=gate.subprocess.DEVNULL)
            child = None
            try:
                deadline = time.monotonic() + 5
                while not pid_file.exists() and time.monotonic() < deadline:
                    time.sleep(0.01)
                self.assertTrue(pid_file.exists(), "profile did not start")
                child = int(pid_file.read_text())
                parent.terminate()
                parent.wait(timeout=5)
                with self.assertRaises(ProcessLookupError):
                    os.kill(child, 0)
            finally:
                if parent.poll() is None:
                    parent.terminate()
                try:
                    parent.wait(timeout=5)
                except gate.subprocess.TimeoutExpired:
                    parent.kill()
                    parent.wait()
                if child is not None:
                    try:
                        os.killpg(child, signal.SIGKILL)
                    except ProcessLookupError:
                        pass

    def test_timed_out_process_is_reaped(self):
        original_popen = gate.subprocess.Popen
        processes = []
        def launch(*args, **kwargs):
            process = original_popen(*args, **kwargs)
            processes.append(process)
            return process
        with mock.patch.object(gate.subprocess, "Popen", side_effect=launch):
            with self.assertRaises(gate.subprocess.TimeoutExpired):
                gate.run_process([gate.sys.executable, "-c", "import time; time.sleep(30)"],
                                 timeout=0.05, cancel=threading.Event())
        self.assertEqual(1, len(processes))
        self.assertIsNotNone(processes[0].poll())


class RecordedEvidenceTest(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.addCleanup(self.temp.cleanup)
        self.directory = Path(self.temp.name)
        self.inputs = {"candidate-path": "candidate", "baseline-path": "baseline"}
        for name in gate.ir_artifacts():
            (self.directory / name).write_text("ABI-REGEX 6000\nABI-REGEX-GC 24000\n")
        self.record = {"schema": 1, "modes": list(gate.IR_MODES),
                       "layouts": list(gate.LAYOUTS), "identities": self.inputs,
                       "completed_jobs": gate.completed_jobs(),
                       "artifacts": {n: gate.fingerprint(self.directory / n)
                                     for n in gate.ir_artifacts()}}
        self.write_record()

    def write_record(self):
        (self.directory / "measurement.json").write_text(json.dumps(self.record))

    def test_complete_unchanged_record_is_accepted(self):
        gate.validate_record(self.directory, self.inputs)

    def test_workload_mutation_during_measurement_is_rejected(self):
        before = {n: gate.fingerprint(self.directory / n)
                  for n in ("regex.el", "regex-gc.el")}
        gate.verify_workloads(self.directory, before)
        (self.directory / "regex-gc.el").write_text("different workload")
        with self.assertRaises(ValueError):
            gate.verify_workloads(self.directory, before)

    def test_swapped_binary_roles_are_rejected(self):
        with self.assertRaises(ValueError):
            gate.validate_record(self.directory, {"candidate-path": "baseline",
                                                   "baseline-path": "candidate"})

    def test_changed_raw_profile_is_rejected(self):
        (self.directory / "regex-gc-cand-0.cg").write_text("changed")
        with self.assertRaises(ValueError):
            gate.validate_record(self.directory, self.inputs)

    def test_missing_or_failed_job_is_rejected(self):
        self.record["completed_jobs"]["regex-gc-base-0"] = 1
        self.write_record()
        with self.assertRaises(ValueError):
            gate.validate_record(self.directory, self.inputs)
        del self.record["completed_jobs"]["regex-gc-base-0"]
        self.write_record()
        with self.assertRaises(ValueError):
            gate.validate_record(self.directory, self.inputs)

    def test_missing_marker_fails_even_if_its_hash_is_updated(self):
        name = "regex-gc-cand-0.out"
        (self.directory / name).write_text("incomplete")
        self.record["artifacts"][name] = gate.fingerprint(self.directory / name)
        self.write_record()
        with self.assertRaises(ValueError):
            gate.validate_record(self.directory, self.inputs)


if __name__ == "__main__":
    unittest.main()
