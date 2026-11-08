#!/usr/bin/env python3
"""Hermetic test of tools/nelisp-standalone-reader-build.sh.

Every test builds a throwaway tree (fake lisp/ src/ scripts/ vendor/, a
Makefile, a copy of the tool) and a fake "Emacs" that counts how many times it
was asked to build.  Each assertion is on that counter, never only on the
messages: a skip that printed `up to date' but still built, or a rebuild that
only claimed to, must fail here.
"""

import os
import shutil
import stat
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path

REPO = Path(__file__).resolve().parent.parent
TOOL_SRC = REPO / "tools" / "nelisp-standalone-reader-build.sh"

FAKE_EMACS = r"""#!/usr/bin/env bash
# Fake build Emacs.  `--version' prints the contents of ./fake-version.
# Anything else is a "build": count it, optionally fail or dirty the tree, and
# write the output where the real reader build would.
if [ "${1:-}" = "--version" ]; then
  cat "$(dirname "$0")/fake-version"
  exit 0
fi
echo build >> "$(dirname "$0")/counter"
n=$(wc -l < "$(dirname "$0")/counter")
if [ -n "${FAKE_BUILD_FAIL:-}" ]; then echo "fake build failing" >&2; exit 7; fi
if [ -n "${FAKE_WRITE_ROOT:-}" ]; then echo ";; generated" >> lisp/generated.el; fi
if [ -n "${NELISP_STANDALONE_READER_OUTPUT:-}" ]; then
  out="$NELISP_STANDALONE_READER_OUTPUT"
else
  case "${NELISP_STANDALONE_TARGET:-}" in
    windows-x86_64) out=target/nelisp.exe ;;
    *) out=target/nelisp ;;
  esac
fi
mkdir -p "$(dirname "$out")"
echo "cold image" > "$out.cold"
echo "binary build=$n target=${NELISP_STANDALONE_TARGET:-} tco=${NELISP_TCO:-}" > "$out"
"""


class BuildStampTest(unittest.TestCase):
    def setUp(self):
        self._tmp = tempfile.TemporaryDirectory(prefix="nl-stamp-test-")
        self.addCleanup(self._tmp.cleanup)
        self.root = Path(self._tmp.name) / "repo"
        self.root.mkdir()
        for d, f in (("lisp", "a.el"), ("src", "b.el"), ("scripts", "c.el"), ("vendor", "d.el"),
                     ("packages", "e.el"), ("templates", "f.el.in"), ("standalone-compat", "g.el")):
            (self.root / d).mkdir()
            (self.root / d / f).write_text(f"(provide '{f})\n")
        (self.root / "Makefile").write_text("standalone-reader:\n\t@true\n")
        (self.root / "test" / "fixtures" / "native-bytecode").mkdir(parents=True)
        self.fixture_a = self.root / "test" / "nelisp-prelude-bytecode-parity-fixtures.tsv"
        self.fixture_b = self.root / "test" / "fixtures" / "native-bytecode" / "gnu-31.1-opcodes.json"
        self.fixture_a.write_text("a\n")
        self.fixture_b.write_text("{}\n")
        (self.root / "tools").mkdir()
        shutil.copy(TOOL_SRC, self.root / "tools" / TOOL_SRC.name)
        self.bin = Path(self._tmp.name) / "bin"
        self.bin.mkdir()
        self.emacs = self.bin / "fake-emacs"
        self.emacs.write_text(FAKE_EMACS)
        self.emacs.chmod(self.emacs.stat().st_mode | stat.S_IXUSR)
        (self.bin / "fake-version").write_text("Fake Emacs 1.0\nsecond line\n")
        self.counter = self.bin / "counter"
        self.base_env = {
            "PATH": os.environ["PATH"],
            "HOME": str(self.root),
            "TMPDIR": self._tmp.name,
            "LC_ALL": "C",
        }

    # -- helpers ------------------------------------------------------------
    def run_tool(self, target="linux-x86_64", env=None, path=None, status=False, cmd=None):
        e = dict(self.base_env)
        if path is not None:
            e["PATH"] = path
        e.update(env or {})
        args = ["bash", "tools/nelisp-standalone-reader-build.sh"]
        if status:
            args.append("--status")
        args += ["--target", target, "--"]
        args += cmd if cmd is not None else [str(self.emacs), "--batch", "build"]
        return subprocess.run(args, cwd=self.root, env=e, capture_output=True,
                              text=True, timeout=40)

    def builds(self):
        return len(self.counter.read_text().splitlines()) if self.counter.exists() else 0

    def out(self, rel="target/nelisp"):
        return self.root / rel

    def stamp(self, rel="target/nelisp"):
        return self.root / (rel + ".build-stamp")

    def assert_built(self, expected_total, res, msg=""):
        self.assertEqual(res.returncode, 0, res.stderr + res.stdout)
        self.assertEqual(self.builds(), expected_total, f"{msg}\n{res.stdout}{res.stderr}")
        self.assertIn("rebuilding", res.stdout)

    def assert_skipped(self, expected_total, res, msg=""):
        self.assertEqual(res.returncode, 0, res.stderr + res.stdout)
        self.assertEqual(self.builds(), expected_total, f"{msg}\n{res.stdout}{res.stderr}")
        self.assertEqual(res.stdout.count("\n"), 1, res.stdout)
        self.assertTrue(res.stdout.startswith("standalone-reader: up to date"), res.stdout)

    def first_build(self, **kw):
        res = self.run_tool(**kw)
        self.assert_built(1, res)
        self.assertTrue(self.stamp().exists() or kw.get("env"), "stamp not written")
        return res

    # -- tests ----------------------------------------------------------------
    def test_no_change_skips(self):
        self.first_build()
        before = self.out().read_bytes()
        self.assert_skipped(1, self.run_tool())
        self.assert_skipped(1, self.run_tool())
        self.assertEqual(self.out().read_bytes(), before)

    def test_status_mode_never_builds(self):
        res = self.run_tool(status=True)
        self.assertEqual(res.returncode, 1, res.stdout)
        self.assertIn("stale: no stamp", res.stdout)
        self.assertEqual(self.builds(), 0)
        self.first_build()
        res = self.run_tool(status=True)
        self.assertEqual(res.returncode, 0, res.stdout)
        self.assertTrue(res.stdout.startswith("standalone-reader: up to date"))
        self.assertEqual(self.builds(), 1)
        (self.root / "src" / "b.el").write_text("changed\n")
        res = self.run_tool(status=True)
        self.assertEqual(res.returncode, 1)
        self.assertIn("key changed", res.stdout)
        self.assertEqual(self.builds(), 1)

    def test_source_content_change_rebuilds(self):
        self.first_build()
        (self.root / "lisp" / "a.el").write_text("(provide 'a-changed)\n")
        self.assert_built(2, self.run_tool())
        self.assert_skipped(2, self.run_tool())

    def test_same_size_content_change_rebuilds(self):
        self.first_build()
        p = self.root / "vendor" / "d.el"
        data = p.read_text()
        p.write_text(data[:-2] + "x\n")
        self.assertEqual(len(data), len(p.read_text()))
        self.assert_built(2, self.run_tool())

    def test_added_elc_file_rebuilds(self):
        self.first_build()
        (self.root / "lisp" / "a.elc").write_bytes(b"\x00bytecode\n")
        self.assert_built(2, self.run_tool())
        self.assert_skipped(2, self.run_tool())

    def test_deleted_file_rebuilds(self):
        self.first_build()
        (self.root / "scripts" / "c.el").unlink()
        self.assert_built(2, self.run_tool())

    def test_makefile_change_rebuilds(self):
        self.first_build()
        (self.root / "Makefile").write_text("standalone-reader:\n\t@echo different\n")
        self.assert_built(2, self.run_tool())

    def test_target_change_rebuilds(self):
        self.first_build()
        self.assert_built(2, self.run_tool(target="macos-aarch64"))
        self.assert_skipped(2, self.run_tool(target="macos-aarch64"))
        # Going back must not reuse the macos stamp: same output path, new key.
        self.assert_built(3, self.run_tool(target="linux-x86_64"))

    def test_nelisp_mode_switch_rebuilds(self):
        self.first_build()
        self.assert_built(2, self.run_tool(env={"NELISP_TCO": "0"}))
        self.assert_skipped(2, self.run_tool(env={"NELISP_TCO": "0"}))
        self.assert_built(3, self.run_tool())  # back to unset
        self.assert_built(4, self.run_tool(env={"NELISP_SOME_NEW_SWITCH": "1"}))

    def test_denylisted_env_does_not_rebuild(self):
        self.first_build()
        self.assert_skipped(1, self.run_tool(env={
            "NELISP_GATE_MUTATION_ONLY": "some-gate",
            "NELISP_GATE_MUTATION_TIMEOUT": "5",
            "NELISP_CHECK_SKIP": "x",
        }))

    def test_jobs_and_wrapper_do_not_rebuild(self):
        # A stamp written with JOBS unset stays fresh under JOBS=4 and under a
        # job wrapper; any other NELISP_* switch still rebuilds.
        self.first_build()
        self.assert_skipped(1, self.run_tool(env={"NELISP_STANDALONE_JOBS": "4"}))
        self.assert_skipped(1, self.run_tool(env={"NELISP_STANDALONE_JOB_WRAPPER": "nice -n 5"}))
        self.assert_skipped(1, self.run_tool(env={"NELISP_STANDALONE_JOBS": "1",
                                                  "NELISP_STANDALONE_JOB_WRAPPER": "x"}))
        self.assert_built(2, self.run_tool(env={"NELISP_STANDALONE_JOBS": "4", "NELISP_TCO": "0"}))
        self.assert_built(3, self.run_tool(env={"NELISP_STANDALONE_JOBS": "4",
                                                "NELISP_STANDALONE_JOBX": "1"}))

    def test_extra_non_nelisp_env_rebuilds(self):
        self.first_build()
        self.assert_built(2, self.run_tool(env={"TZ": "Asia/Tokyo"}))

    def test_output_path_change_rebuilds(self):
        self.first_build()
        env = {"NELISP_STANDALONE_READER_OUTPUT": "target/other-nelisp"}
        self.assert_built(2, self.run_tool(env=env))
        self.assertTrue(self.out("target/other-nelisp").exists())
        self.assertTrue(self.stamp("target/other-nelisp").exists())
        self.assert_skipped(2, self.run_tool(env=env))
        # The default output was not touched, so its own stamp still holds.
        self.assert_skipped(2, self.run_tool())
        # Damaging it is still detected.
        self.out().write_text("tampered\n")
        self.assert_built(3, self.run_tool())

    def test_emacs_identity_change_rebuilds(self):
        self.first_build()
        (self.bin / "fake-version").write_text("Fake Emacs 2.0\n")
        self.assert_built(2, self.run_tool())
        self.assert_skipped(2, self.run_tool())

    def test_emacs_executable_content_change_rebuilds(self):
        self.first_build()
        self.emacs.write_text(FAKE_EMACS + "\n# patched executable, same --version\n")
        self.assert_built(2, self.run_tool())

    def test_emacs_path_change_rebuilds(self):
        self.first_build()
        other = self.bin / "fake-emacs-2"
        shutil.copy(self.emacs, other)
        res = self.run_tool(cmd=[str(other), "--batch", "build"])
        self.assert_built(2, res)

    def test_output_overwritten_rebuilds(self):
        self.first_build()
        self.out().write_text("someone else's binary\n")
        res = self.run_tool()
        self.assert_built(2, res)
        self.assertIn("output changed", res.stdout)
        self.assertIn("binary build=2", self.out().read_text())

    def test_output_deleted_rebuilds(self):
        self.first_build()
        self.out().unlink()
        res = self.run_tool()
        self.assert_built(2, res)
        self.assertIn("output missing", res.stdout)

    def test_stamp_deleted_rebuilds(self):
        self.first_build()
        self.stamp().unlink()
        self.assert_built(2, self.run_tool())

    def test_corrupt_stamp_rebuilds(self):
        self.first_build()
        self.stamp().write_text("garbage\n")
        self.assert_built(2, self.run_tool())

    def test_failing_build_leaves_no_stamp(self):
        self.first_build()
        (self.root / "src" / "b.el").write_text("broken\n")
        res = self.run_tool(env={"FAKE_BUILD_FAIL": "1"})
        self.assertEqual(res.returncode, 7, "build exit code must propagate")
        self.assertEqual(self.builds(), 2)
        self.assertFalse(self.stamp().exists(), "failed build left a stamp")
        # The next run must build again even though inputs did not change.
        self.assert_built(3, self.run_tool())
        self.assertTrue(self.stamp().exists())
        self.assert_skipped(3, self.run_tool())

    def test_failed_build_then_restore_still_rebuilds(self):
        self.first_build()
        p = self.root / "src" / "b.el"
        original = p.read_text()
        p.write_text("broken\n")
        self.assertEqual(self.run_tool(env={"FAKE_BUILD_FAIL": "1"}).returncode, 7)
        p.write_text(original)  # inputs now equal the stamped state again
        self.assert_built(3, self.run_tool(), "stamp was removed before the failed build")

    def test_mutate_then_restore_rebuilds_both_times(self):
        # Shape of tools/nelisp-gate-mutation.sh: mutate, build, restore, build.
        self.first_build()
        p = self.root / "lisp" / "a.el"
        original = p.read_text()
        p.write_text(original + ";; injected defect\n")
        self.assert_built(2, self.run_tool(), "mutated build")
        mutated_bin = self.out().read_text()
        p.write_text(original)
        res = self.run_tool()
        self.assert_built(3, res, "restored build must not skip: binary on disk is the mutated one")
        self.assertNotEqual(self.out().read_text(), mutated_bin)
        self.assert_skipped(3, self.run_tool())

    def test_force_switch_builds(self):
        self.first_build()
        res = self.run_tool(env={"NELISP_STANDALONE_FORCE_REBUILD": "1"})
        self.assert_built(2, res)
        self.assertIn("forced", res.stdout)
        res = self.run_tool(env={"NELISP_STANDALONE_FORCE_REBUILD": "1"})
        self.assert_built(3, res)

    def test_force_switch_zero_does_not_force(self):
        self.first_build()
        self.assert_skipped(1, self.run_tool(env={"NELISP_STANDALONE_FORCE_REBUILD": "0"}))

    def test_unavailable_hash_tool_builds(self):
        # A PATH that has the other tools but neither sha256sum nor shasum.
        fake_path = Path(self._tmp.name) / "nohash-path"
        fake_path.mkdir()
        for tool in ("find", "sort", "xargs", "cut", "mktemp", "rm", "mv", "cat",
                     "dirname", "mkdir", "wc", "bash", "tr", "git", "python3", "env", "head"):
            found = shutil.which(tool)
            if found:
                (fake_path / tool).symlink_to(found)
        self.assertIsNone(shutil.which("sha256sum", path=str(fake_path)))
        self.assertIsNone(shutil.which("shasum", path=str(fake_path)))
        res = self.run_tool(path=str(fake_path))
        self.assertEqual(res.returncode, 0, res.stderr)
        self.assertEqual(self.builds(), 1)
        self.assertIn("no sha256 tool", res.stdout)
        self.assertFalse(self.stamp().exists())
        res = self.run_tool(path=str(fake_path))
        self.assertEqual(self.builds(), 2, "without a hash tool every run must build")

    def test_emacs_version_failure_builds_without_stamp(self):
        bad = self.bin / "bad-emacs"
        bad.write_text("#!/usr/bin/env bash\n"
                       "[ \"${1:-}\" = --version ] && exit 3\n"
                       "echo build >> \"$(dirname \"$0\")/counter\"\n"
                       "mkdir -p target; echo bin > target/nelisp\n")
        bad.chmod(bad.stat().st_mode | stat.S_IXUSR)
        res = self.run_tool(cmd=[str(bad), "--batch", "build"])
        self.assertEqual(self.builds(), 1)
        self.assertIn("--version", res.stdout)
        self.assertFalse(self.stamp().exists())
        self.run_tool(cmd=[str(bad), "--batch", "build"])
        self.assertEqual(self.builds(), 2)

    def test_unknown_target_builds_without_stamp(self):
        res = self.run_tool(target="plan9-mips")
        self.assertEqual(self.builds(), 1)
        self.assertIn("no known output path", res.stdout)
        self.assertFalse(self.stamp().exists())
        self.run_tool(target="plan9-mips")
        self.assertEqual(self.builds(), 2)

    def test_missing_input_directory_builds(self):
        shutil.rmtree(self.root / "vendor")
        res = self.run_tool()
        self.assertEqual(self.builds(), 1)
        self.assertIn("cannot", res.stdout)
        self.assertFalse(self.stamp().exists())

    def test_build_that_writes_into_inputs_is_not_stamped(self):
        res = self.run_tool(env={"FAKE_WRITE_ROOT": "1"})
        self.assertEqual(res.returncode, 0)
        self.assertEqual(self.builds(), 1)
        self.assertIn("CHANGED DURING THE BUILD", res.stderr)
        self.assertFalse(self.stamp().exists())
        # With the generated file now present, a clean build stamps normally.
        (self.root / "lisp" / "generated.el").unlink()
        self.assert_built(2, self.run_tool())
        self.assert_skipped(2, self.run_tool())

    def test_missing_output_after_successful_build_is_not_stamped(self):
        noout = self.bin / "noout-emacs"
        noout.write_text("#!/usr/bin/env bash\n"
                         "[ \"${1:-}\" = --version ] && { echo noout; exit 0; }\n"
                         "echo build >> \"$(dirname \"$0\")/counter\"\n")
        noout.chmod(noout.stat().st_mode | stat.S_IXUSR)
        res = self.run_tool(cmd=[str(noout), "--batch", "build"])
        self.assertEqual(res.returncode, 0)
        self.assertFalse(self.stamp().exists())
        self.run_tool(cmd=[str(noout), "--batch", "build"])
        self.assertEqual(self.builds(), 2)

    def test_packages_and_templates_change_rebuild(self):
        self.first_build()
        (self.root / "packages" / "e.el").write_text("changed\n")
        self.assert_built(2, self.run_tool())
        (self.root / "templates" / "f.el.in").write_text("changed\n")
        self.assert_built(3, self.run_tool())
        (self.root / "packages" / "new.el").write_text("added\n")
        self.assert_built(4, self.run_tool())

    def test_test_fixture_files_change_rebuild(self):
        self.first_build()
        self.fixture_a.write_text("b\n")
        self.assert_built(2, self.run_tool())
        self.fixture_b.write_text('{"x": 1}\n')
        self.assert_built(3, self.run_tool())

    def test_companion_file_change_rebuilds(self):
        self.first_build()
        Path(str(self.out()) + ".cold").write_text("stale cold image\n")
        res = self.run_tool()
        self.assert_built(2, res)
        self.assertIn("companion", res.stdout)
        Path(str(self.out()) + ".cold").unlink()
        self.assert_built(3, self.run_tool())

    def test_git_identity_change_rebuilds(self):
        if not shutil.which("git"):
            self.skipTest("git not installed")
        def git(*a):
            subprocess.run(["git", "-c", "user.name=t", "-c", "user.email=t@example.invalid", *a],
                           cwd=self.root, check=True, capture_output=True)
        (self.root / ".gitignore").write_text("target/\n")
        git("init", "-q")
        git("add", "-A")
        git("commit", "-q", "-m", "one")
        self.first_build()
        self.assert_skipped(1, self.run_tool())
        # Dirty tree: `git describe --dirty' changes, and the binary embeds it.
        # (lisp/a.el is tracked, so editing it dirties the tree.)
        (self.root / "lisp" / "a.el").write_text("dirty\n")
        self.assert_built(2, self.run_tool())
        git("commit", "-q", "-am", "two")
        self.assert_built(3, self.run_tool())
        self.assert_skipped(3, self.run_tool())
        git("commit", "-q", "--allow-empty", "-m", "three")  # only HEAD moves
        self.assert_built(4, self.run_tool())

    def test_target_is_exported_to_build(self):
        res = self.run_tool(target="windows-x86_64")
        self.assertEqual(self.builds(), 1, res.stdout)
        self.assertIn("target=windows-x86_64", self.out("target/nelisp.exe").read_text())
        self.assertTrue(self.stamp("target/nelisp.exe").exists())


if __name__ == "__main__":
    result = unittest.main(verbosity=2, exit=False).result
    checked = result.testsRun - len(result.skipped)
    failed = len({getattr(test, "test_case", test).id()
                  for test, _ in result.failures + result.errors})
    print(f"GATE-COUNT checked={checked} findings={failed} passed={checked - failed} failed={failed}")
    sys.exit(0 if result.wasSuccessful() else 1)
