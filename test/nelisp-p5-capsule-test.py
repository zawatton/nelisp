#!/usr/bin/env python3
"""Calibrate the P5 capsule with two real input mutations and one pass."""

import importlib.util
import json
import os
from pathlib import Path
import subprocess
import tempfile
import unittest


RUNNER = Path(__file__).resolve().parents[1] / "tools/nelisp-p5-capsule.py"
SPEC = importlib.util.spec_from_file_location("nelisp_p5_capsule", RUNNER)
CAPSULE = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(CAPSULE)


def write_executable(path, text):
    path.write_text(text)
    path.chmod(0o755)


class CapsuleCalibration(unittest.TestCase):
    def test_snapshot_includes_real_standalone_compat_json_input(self):
        source = Path(__file__).resolve().parents[1]
        relative = "standalone-compat/json.el"
        source_json = source / relative
        self.assertTrue(source_json.is_file())
        self.assertIn(relative, CAPSULE.source_manifest(source))
        with tempfile.TemporaryDirectory(prefix="nelisp-p5-json-snapshot-") as temp:
            snapshot = Path(temp) / "snapshot"
            CAPSULE.copy_source(source, snapshot)
            copied_json = snapshot / relative
            self.assertTrue(copied_json.is_file())
            self.assertEqual(CAPSULE.sha256(copied_json),
                             CAPSULE.sha256(source_json))

    def test_binary_and_source_mutations_fail_then_patch_passes(self):
        with tempfile.TemporaryDirectory(prefix="nelisp-p5-capsule-test-") as temp:
            temporary = Path(temp)
            source = temporary / "source"
            source.mkdir()
            for name in CAPSULE.SOURCE_DIRS:
                (source / name).mkdir()
            shared_target = source / "target" / "nelisp"
            shared_target.parent.mkdir()
            write_executable(shared_target, "#!/bin/sh\necho 99\n")
            (source / "Makefile").write_text(
                "standalone-reader:\n"
                "\tcp scripts/candidate.sh $(NELISP_STANDALONE_READER_OUTPUT)\n")
            script = source / "scripts/candidate.sh"
            write_executable(script, "#!/bin/sh\necho 2\n")
            probe = source / "test/focused.sh"
            write_executable(probe, "#!/bin/sh\nsleep 0.8\n"
                                    "\"$1\" --eval \"(cadr '(1 2 3))\" >/dev/null\n"
                                    "echo 'focused JIT fixture: PASS'\n")
            binary = temporary / "baseline-bin"
            write_executable(binary, "#!/bin/sh\necho 2\n")
            output = Path(os.environ.get("NELISP_P5_TEST_OUTPUT", temporary / "reports"))

            def replace_candidate(candidate):
                write_executable(candidate, "#!/bin/sh\necho 99\n")

            red_binary_path, red_binary = CAPSULE.run_capsule(
                source, binary, output, "test/focused.sh",
                after_probe_start=replace_candidate)
            self.assertEqual(red_binary["status"], "failed")
            self.assertEqual(red_binary["first_failing_stage"], "focused_probe:during")
            self.assertIn("changed candidate", red_binary["failure"])

            def change_source(original, _snapshot):
                write_executable(original / "scripts/candidate.sh",
                                 "#!/bin/sh\necho 3\n")

            red_source_path, red_source = CAPSULE.run_capsule(
                source, binary, output, "test/focused.sh",
                after_snapshot=change_source)
            self.assertEqual(red_source["status"], "failed")
            self.assertEqual(red_source["first_failing_stage"], "baseline:before")
            self.assertIn("hash changed: scripts/candidate.sh", red_source["failure"])

            patch = temporary / "candidate.patch"
            improved = temporary / "candidate-new.sh"
            improved.write_text("#!/bin/sh\n# candidate patch applied\necho 3\n")
            diff = subprocess.run(
                ["diff", "-u", "--label", "a/scripts/candidate.sh",
                 "--label", "b/scripts/candidate.sh", str(script), str(improved)],
                text=True, stdout=subprocess.PIPE, check=False)
            self.assertEqual(diff.returncode, 1)
            patch.write_text(diff.stdout)
            green_path, green = CAPSULE.run_capsule(
                source, binary, output, "test/focused.sh", patch=patch)
            self.assertEqual(green["status"], "passed", green.get("failure"))
            self.assertEqual(green["stages"]["baseline"]["stdout"], "2")
            self.assertIn("PASS", green["stages"]["focused_probe"]["stdout"])
            self.assertNotIn("candidate patch applied", script.read_text())
            self.assertEqual(green["candidate_sha256"],
                             CAPSULE.sha256(Path(green["candidate_binary"])))
            for path in (red_binary_path, red_source_path, green_path):
                self.assertTrue(json.loads(path.read_text())["input_manifest_sha256"])

            env_probe = source / "test/environment-focused.sh"
            write_executable(
                env_probe,
                "#!/bin/sh\nset -eu\n"
                "root=$(CDPATH= cd -- \"$(dirname \"$0\")/..\" && pwd)\n"
                "[ \"${NELISP_BIN:-}\" = \"$root/target/nelisp\" ]\n"
                "result=$(\"$NELISP_BIN\" --eval \"(cadr '(1 2 3))\")\n"
                "[ \"$result\" = 3 ]\n"
                "echo 'candidate-via-NELISP_BIN: PASS'\n")
            env_path, env_report = CAPSULE.run_capsule(
                source, binary, output, "test/environment-focused.sh", patch=patch)
            self.assertEqual(env_report["status"], "passed", env_report.get("failure"))
            self.assertIn("candidate-via-NELISP_BIN: PASS",
                          env_report["stages"]["focused_probe"]["stdout"])
            self.assertEqual(env_report["candidate_binary"],
                             str(Path(env_report["candidate_binary"]).resolve()))
            self.assertNotEqual(env_report["candidate_binary"], str(shared_target))
            self.assertTrue(json.loads(env_path.read_text())["input_manifest_sha256"])
            print("red-binary", red_binary_path, red_binary["first_failing_stage"])
            print("red-source", red_source_path, red_source["first_failing_stage"])
            print("green", green_path, green["status"])
            print("environment-probe", env_path, env_report["status"])

    def test_source_manifest_rejects_linked_source_directories(self):
        with tempfile.TemporaryDirectory(prefix="nelisp-p5-capsule-symlink-") as temp:
            temporary = Path(temp)
            source = temporary / "source"
            outside = temporary / "outside"
            source.mkdir()
            outside.mkdir()
            (source / "Makefile").write_text("standalone-reader:\n\t@true\n")
            for name in CAPSULE.SOURCE_DIRS:
                (source / name).mkdir()
            (outside / "unhashed.py").write_text("print('outside')\n")
            (source / "tools" / "linked").symlink_to(outside, target_is_directory=True)
            with self.assertRaisesRegex(CAPSULE.CapsuleError,
                                        "linked source directory"):
                CAPSULE.source_manifest(source)


if __name__ == "__main__":
    unittest.main()
