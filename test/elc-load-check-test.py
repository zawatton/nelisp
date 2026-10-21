#!/usr/bin/env python3
"""Negative controls for the retained .elc acceptance evidence (no rebuild)."""
import contextlib
import importlib.util
import io
import json
import os
from pathlib import Path
import sys
import tempfile
import unittest
from unittest.mock import patch

ROOT = Path(__file__).resolve().parents[1]
SPEC = importlib.util.spec_from_file_location("elc_check", ROOT / "tools/ai/elc-load-check.py")
CHECK = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(CHECK)


class EvidenceControls(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        self.addCleanup(self.tmp.cleanup)
        self.out = Path(self.tmp.name)
        self.patchers = [patch.object(CHECK, "OUT", self.out),
                         patch.object(CHECK, "CORPUS", self.out / "corpus"),
                         patch.object(CHECK, "FINAL_ONCE", self.out / "once-started.json"),
                         patch.dict(os.environ, ELC_LOAD_BIN=str(self.out / "dynamic"),
                                    ELC_LOAD_STATIC=str(self.out / "static"))]
        for p in self.patchers:
            p.start()
            self.addCleanup(p.stop)
        CHECK.FINAL_ONCE.write_text('{"test-only": true}\n')
        (self.out / "corpus").mkdir()
        for _, binary in CHECK.binaries():
            for suffix in ("", ".cold", ".native-startup.el"):
                Path(str(binary) + suffix).write_text("synthetic identity\n")
        for ext in ("el", "elc"):
            (CHECK.CORPUS / ("mxlib." + ext)).write_text("synthetic fixture\n")
        CHECK.save("corpus.json", dict(inputs=CHECK.pins([CHECK.CORPUS / "mxlib.el"])))

    def invoke(self, fn):
        with contextlib.redirect_stdout(io.StringIO()):
            return fn()

    def row_run(self, stdout="ELC-ROW t\nELC-DONE\n", stderr=""):
        out, err = self.out / "rows.out", self.out / "rows.err"
        out.write_text(stdout)
        err.write_text(stderr)
        return dict(rc=0, stdout=str(out), stderr=str(err))

    def test_rows_valid_control(self):
        self.assertEqual(CHECK.rows(self.row_run()), ["t"])

    def test_rows_rejects_stderr(self):
        with self.assertRaisesRegex(ValueError, "unexpected stderr"):
            CHECK.rows(self.row_run(stderr="runtime warning\n"))

    def test_rows_rejects_unknown_stdout(self):
        with self.assertRaisesRegex(ValueError, "unexpected stdout"):
            CHECK.rows(self.row_run(stdout="ELC-ROW t\nunexpected\nELC-DONE\n"))

    def test_rows_rejects_fake_completion(self):
        with self.assertRaisesRegex(ValueError, "completion marker"):
            CHECK.rows(self.row_run(stdout='ELC-ROW t\n"ELC-DONE\\n"\n'))

    def test_rows_rejects_nonterminal_completion(self):
        with self.assertRaisesRegex(ValueError, "completion marker"):
            CHECK.rows(self.row_run(stdout="ELC-ROW t\nELC-DONE\nELC-ROW t\n"))

    def test_rows_rejects_duplicate_completion(self):
        with self.assertRaisesRegex(ValueError, "completion marker"):
            CHECK.rows(self.row_run(stdout="ELC-ROW t\nELC-DONE\nELC-DONE\n"))

    def speed_record(self, elc_cost=3000):
        inputs = CHECK.speed_inputs()
        rows = []
        for function in ("mx-a", "mx-b"):
            for kind, cost in (("el", 12000), ("elc", elc_cost)):
                for layout in CHECK.LAYOUTS:
                    samples = []
                    for n in (CHECK.SMALL, CHECK.LARGE):
                        stem = self.out / f"{function}-{kind}-{layout}-{n}"
                        profile, stdout, stderr = [Path(str(stem) + s) for s in (".cg", ".out", ".err")]
                        profile.write_text(f"events: Ir\nsummary: {1000000+n*cost}\n")
                        stdout.write_text(f"ELC-SPEED {n}\n")
                        stderr.write_text("")
                        inputs += [profile, stdout, stderr]
                        samples.append(dict(n=n, profile=str(profile), run=dict(rc=0, stdout=str(stdout), stderr=str(stderr))))
                    rows.append(dict(function=function, kind=kind, layout=layout, samples=samples, ir_per_call=cost))
        record = dict(inputs=CHECK.pins(inputs), binary=str(CHECK.binaries()[0][1]), layouts=list(CHECK.LAYOUTS), rows=rows)
        CHECK.save("speed.json", record)
        return record

    def final_record(self):
        # A fixed past instant avoids filesystem timestamp tick granularity in
        # these synthetic files; the stale-artifact control still uses ns=1.
        with patch.object(CHECK.time, "time_ns", return_value=10 ** 9):
            CHECK.start_final()
        paths = CHECK.final_inputs() + [self.out / "final-start.json"]
        common = self.out / "common.out"
        common.write_text("".join(f"{g} rc=0 1s\n" for g in sorted(CHECK.FINAL_GROUPS)) + "GATE-COUNT: 11 cases, 0 failures\n")
        logs = []
        for g in CHECK.FINAL_GROUPS:
            path = self.out / (g + ".log")
            path.write_text("synthetic group output\n")
            paths.append(path)
            logs.append(str(path))
        standalone = self.out / "standalone.out"
        n = len(list((ROOT / "test").glob("standalone-*-check.el")))
        standalone.write_text(f"STANDALONE-CHECKS PASS\nGATE-COUNT: {n} cases, 0 failures\n")
        mcp = self.out / "mcp.out"
        mcp.write_text(json.dumps(dict(jsonrpc="2.0", id=1, result=dict(protocolVersion="2024-11-05"))) + "\n" +
                       json.dumps(dict(jsonrpc="2.0", id=2, result=dict(tools=[dict(name=f"tool-{i}") for i in range(40)]))) + "\n")
        paths += [common, standalone, mcp]
        record = dict(inputs=CHECK.pins(paths), binaries={tag: str(p) for tag, p in CHECK.binaries()},
                      runs=dict(common=dict(rc=0, stdout=str(common), logs=logs),
                                standalone=dict(rc=0, stdout=str(standalone)), mcp=dict(rc=0, stdout=str(mcp))))
        for name, run in record["runs"].items():
            stderr = self.out / (name + ".stderr")
            stderr.write_text("")
            metadata = self.out / (name + ".metadata.json")
            metadata.write_text(json.dumps(dict(address_space_cap_bytes=8 * 1024 ** 3,
                max_vm_size_kib=1024, max_vm_peak_kib=1024, max_group_vm_size_kib=1024,
                max_group_rss_kib=512, proc_samples=2, peak_rss_kib=512, peak_rss_source="wait4")))
            run.update(stderr=str(stderr), metadata=str(metadata))
            paths += [stderr, metadata]
        record["inputs"] = CHECK.pins(paths)
        CHECK.save("final.json", record)
        return record

    def repin(self, name, record):
        record["inputs"] = CHECK.pins(record["inputs"])
        CHECK.save(name, record)

    def test_speed_valid_control(self):
        self.speed_record()
        self.invoke(CHECK.speed)

    def test_speed_below_threshold(self):
        self.speed_record(elc_cost=4100)
        with self.assertRaisesRegex(ValueError, "below 3x"):
            self.invoke(CHECK.speed)

    def test_speed_missing_layout(self):
        record = self.speed_record()
        record["rows"].pop()
        CHECK.save("speed.json", record)
        with self.assertRaisesRegex(ValueError, "missing speed layouts"):
            self.invoke(CHECK.speed)

    def test_speed_stale_profile(self):
        record = self.speed_record()
        Path(record["rows"][0]["samples"][0]["profile"]).write_text("summary: 1\n")
        with self.assertRaisesRegex(ValueError, "stale evidence"):
            self.invoke(CHECK.speed)

    def test_speed_missing_completion(self):
        record = self.speed_record()
        Path(record["rows"][0]["samples"][0]["run"]["stdout"]).write_text("")
        self.repin("speed.json", record)
        with self.assertRaisesRegex(ValueError, "missing speed completion"):
            self.invoke(CHECK.speed)

    def test_speed_omitted_binary_identity(self):
        record = self.speed_record()
        record["inputs"].pop(str(CHECK.binaries()[0][1]))
        CHECK.save("speed.json", record)
        with self.assertRaisesRegex(ValueError, "omits required"):
            self.invoke(CHECK.speed)

    def test_speed_unexpected_runtime_stderr(self):
        record = self.speed_record()
        Path(record["rows"][0]["samples"][0]["run"]["stderr"]).write_text("runtime failed\n")
        self.repin("speed.json", record)
        with self.assertRaisesRegex(ValueError, "unexpected runtime stderr"):
            self.invoke(CHECK.speed)

    def test_speed_missing_evidence(self):
        with self.assertRaises(FileNotFoundError):
            self.invoke(CHECK.speed)

    def test_final_valid_control(self):
        self.invoke(self.final_record)
        self.invoke(CHECK.final)

    def test_final_wrong_tool_count(self):
        record = self.invoke(self.final_record)
        path = Path(record["runs"]["mcp"]["stdout"])
        responses = [json.loads(line) for line in path.read_text().splitlines()]
        responses[1]["result"]["tools"] = []
        path.write_text("".join(json.dumps(obj) + "\n" for obj in responses))
        self.repin("final.json", record)
        with self.assertRaisesRegex(ValueError, "40 distinct tools"):
            self.invoke(CHECK.final)

    def test_final_failed_group(self):
        record = self.invoke(self.final_record)
        path = Path(record["runs"]["common"]["stdout"])
        path.write_text(path.read_text().replace("rc=0", "rc=1", 1))
        self.repin("final.json", record)
        with self.assertRaisesRegex(ValueError, "11 passing groups"):
            self.invoke(CHECK.final)

    def test_final_omitted_group_log(self):
        record = self.invoke(self.final_record)
        record["runs"]["common"]["logs"].pop()
        CHECK.save("final.json", record)
        with self.assertRaisesRegex(ValueError, "missing subordinate logs"):
            self.invoke(CHECK.final)

    def test_final_stale_binary(self):
        self.invoke(self.final_record)
        CHECK.binaries()[0][1].write_text("changed build\n")
        with self.assertRaisesRegex(ValueError, "stale evidence"):
            self.invoke(CHECK.final)

    def test_final_record_interface(self):
        record = self.invoke(self.final_record)
        args = type("Arguments", (), {})()
        for name, run in record["runs"].items():
            for suffix, key in (("", "stdout"), ("_rc", "rc"), ("_stderr", "stderr"),
                                ("_metadata", "metadata")):
                setattr(args, name + suffix, run[key])
        args.common_dir = str(self.out)
        self.invoke(lambda: CHECK.record_final(args))
        args.common_stderr = None
        with self.assertRaisesRegex(ValueError, "missing final common stderr"):
            self.invoke(lambda: CHECK.record_final(args))

    def test_final_missing_stderr_record(self):
        record = self.invoke(self.final_record)
        del record["runs"]["common"]["stderr"]
        CHECK.save("final.json", record)
        with self.assertRaisesRegex(ValueError, "missing final run artifacts"):
            self.invoke(CHECK.final)

    def test_final_unpinned_stderr(self):
        record = self.invoke(self.final_record)
        del record["inputs"][record["runs"]["common"]["stderr"]]
        CHECK.save("final.json", record)
        with self.assertRaisesRegex(ValueError, "omits required input identities"):
            self.invoke(CHECK.final)

    def test_final_common_stderr(self):
        record = self.invoke(self.final_record)
        Path(record["runs"]["common"]["stderr"]).write_text("runtime Error\n")
        self.repin("final.json", record)
        with self.assertRaisesRegex(ValueError, "unexpected final stderr: common"):
            self.invoke(CHECK.final)

    def test_final_standalone_stderr(self):
        record = self.invoke(self.final_record)
        Path(record["runs"]["standalone"]["stderr"]).write_text("runtime Error\n")
        self.repin("final.json", record)
        with self.assertRaisesRegex(ValueError, "unexpected final stderr: standalone"):
            self.invoke(CHECK.final)

    def test_final_known_mcp_diagnostics(self):
        record = self.invoke(self.final_record)
        Path(record["runs"]["mcp"]["stderr"]).write_text("anvil-standalone: image hit " + "a" * 64 +
             '\nANVIL-START-TIMINGS {"runtime_identity": 0.1, "runtime_to_first_response": 1.2}\n')
        self.repin("final.json", record)
        self.invoke(CHECK.final)

    def test_final_unknown_mcp_stderr(self):
        record = self.invoke(self.final_record)
        Path(record["runs"]["mcp"]["stderr"]).write_text("anvil-standalone: runtime Error\n")
        self.repin("final.json", record)
        with self.assertRaisesRegex(ValueError, "unexpected final stderr: mcp"):
            self.invoke(CHECK.final)

    def test_final_fake_mcp_timing_diagnostic(self):
        record = self.invoke(self.final_record)
        Path(record["runs"]["mcp"]["stderr"]).write_text('ANVIL-START-TIMINGS {"error": "failed"}\n')
        self.repin("final.json", record)
        with self.assertRaisesRegex(ValueError, "invalid MCP timing diagnostics"):
            self.invoke(CHECK.final)

    def test_final_unknown_mcp_stdout(self):
        record = self.invoke(self.final_record)
        path = Path(record["runs"]["mcp"]["stdout"])
        path.write_text("runtime Error\n" + path.read_text())
        self.repin("final.json", record)
        with self.assertRaises(ValueError):
            self.invoke(CHECK.final)

    def test_final_mcp_error_response(self):
        record = self.invoke(self.final_record)
        path = Path(record["runs"]["mcp"]["stdout"])
        path.write_text(path.read_text() + '{"jsonrpc":"2.0","id":3,"error":{"code":1}}\n')
        self.repin("final.json", record)
        with self.assertRaisesRegex(ValueError, "unexpected MCP JSON-RPC responses"):
            self.invoke(CHECK.final)

    def test_final_missing_initialize(self):
        record = self.invoke(self.final_record)
        path = Path(record["runs"]["mcp"]["stdout"])
        path.write_text(path.read_text().splitlines()[1] + "\n")
        self.repin("final.json", record)
        with self.assertRaisesRegex(ValueError, "unexpected MCP JSON-RPC responses"):
            self.invoke(CHECK.final)

    def test_final_missing_metadata_record(self):
        record = self.invoke(self.final_record)
        del record["runs"]["mcp"]["metadata"]
        CHECK.save("final.json", record)
        with self.assertRaisesRegex(ValueError, "missing final run artifacts"):
            self.invoke(CHECK.final)

    def test_final_unpinned_metadata(self):
        record = self.invoke(self.final_record)
        del record["inputs"][record["runs"]["mcp"]["metadata"]]
        CHECK.save("final.json", record)
        with self.assertRaisesRegex(ValueError, "omits required input identities"):
            self.invoke(CHECK.final)

    def test_final_stale_metadata(self):
        record = self.invoke(self.final_record)
        path = Path(record["runs"]["mcp"]["metadata"])
        os.utime(path, ns=(1, 1))
        with self.assertRaisesRegex(ValueError, "predates snapshot"):
            self.invoke(CHECK.final)

    def test_final_invalid_metadata(self):
        record = self.invoke(self.final_record)
        Path(record["runs"]["mcp"]["metadata"]).write_text("{}\n")
        self.repin("final.json", record)
        with self.assertRaisesRegex(ValueError, "invalid final memory cap"):
            self.invoke(CHECK.final)

    def test_final_omitted_once_guard(self):
        record = self.invoke(self.final_record)
        del record["inputs"][str(CHECK.FINAL_ONCE)]
        CHECK.save("final.json", record)
        with self.assertRaisesRegex(ValueError, "omits required input identities"):
            self.invoke(CHECK.final)

    def test_heavy_wrapper_argv_and_environment(self):
        runner = self.out / "heavy-run.sh"
        with patch.object(CHECK, "heavy_runner", return_value=runner):
            command, environment = CHECK.heavy_command([Path("binary"), "arg with spaces"], {"KEEP": "value"})
        self.assertEqual(command, [str(runner), "binary", "arg with spaces"])
        self.assertEqual(environment, {"KEEP": "value", CHECK.HEAVY_UNIT_ENV: "1"})

    def test_heavy_suite_enters_once_without_nesting(self):
        with patch.dict(os.environ, {CHECK.HEAVY_UNIT_ENV: ""}), \
             patch.object(sys, "argv", [str(CHECK.__file__), "resolve"]), \
             patch.object(CHECK, "execute", side_effect=RuntimeError("intercepted supervision")) as supervise:
            CHECK.enter_suite_slot("prepare")
            supervise.assert_not_called()
            with self.assertRaisesRegex(RuntimeError, "intercepted supervision"):
                CHECK.enter_suite_slot("resolve")
            self.assertEqual(supervise.call_args.args[0], [sys.executable, Path(CHECK.__file__), "resolve"])
            self.assertEqual(supervise.call_args.kwargs, dict(timeout=90, heavy=True))
            with patch.dict(os.environ, {CHECK.HEAVY_UNIT_ENV: "1"}), \
                 patch.object(CHECK, "heavy_runner", side_effect=AssertionError("nested slot")):
                CHECK.enter_suite_slot("resolve")
                nested, _ = CHECK.heavy_command(["child"])
            self.assertEqual(nested, ["child"])
            self.assertEqual(supervise.call_count, 1)

    def delayed_heavy_runner(self, delay):
        runner = self.out / "heavy-run.sh"
        runner.write_text(f"#!{sys.executable}\nimport os, sys, time\ntime.sleep({delay})\n"
                          "os.execvpe(sys.argv[1], sys.argv[1:], os.environ)\n")
        runner.chmod(0o755)
        return runner

    def test_heavy_queue_does_not_consume_runtime_budget(self):
        with patch.object(CHECK, "heavy_runner", return_value=self.delayed_heavy_runner(0.25)):
            run = CHECK.execute([sys.executable, "-c", "print('ran')"], self.out / "queued",
                                timeout=0.15, heavy=True, env=dict(os.environ, ELC_LOAD_HEAVY_UNIT=""))
        self.assertEqual(run["rc"], 0)
        self.assertEqual(Path(run["stdout"]).read_text(), "ran\n")
        self.assertGreater(run["memory"]["queue_seconds"], 0.2)
        self.assertLess(run["memory"]["execution_seconds"], 0.15)

    def test_heavy_runtime_deadline_still_expires(self):
        stem = self.out / "runtime-expiry"
        with patch.object(CHECK, "heavy_runner", return_value=self.delayed_heavy_runner(0.05)):
            with self.assertRaisesRegex(ValueError, "^timeout:"):
                CHECK.execute([sys.executable, "-c", "import time; time.sleep(30)"], stem,
                              timeout=0.08, heavy=True, env=dict(os.environ, ELC_LOAD_HEAVY_UNIT=""))
        metadata = json.loads(Path(str(stem) + ".metadata.json").read_text())
        self.assertEqual(metadata["execution_state"], "RUN")
        self.assertGreaterEqual(metadata["execution_seconds"], 0.07)

    def test_heavy_queue_expiry_never_starts_command(self):
        stem, child = self.out / "queue-expiry", self.out / "should-not-run"
        with patch.object(CHECK, "heavy_runner", return_value=self.delayed_heavy_runner(0.4)), \
             patch.object(CHECK, "HEAVY_QUEUE_TIMEOUT", 0.05):
            with self.assertRaisesRegex(ValueError, "^NOTRUN heavy slot queue expired:"):
                CHECK.execute([sys.executable, "-c", f"open({str(child)!r}, 'w').close()"], stem,
                              timeout=0.08, heavy=True, env=dict(os.environ, ELC_LOAD_HEAVY_UNIT=""))
        metadata = json.loads(Path(str(stem) + ".metadata.json").read_text())
        self.assertFalse(child.exists())
        self.assertEqual(metadata["execution_state"], "NOTRUN")
        self.assertEqual(metadata["execution_seconds"], 0)

    def test_child_address_space_limit_and_memory_record(self):
        run = CHECK.execute([sys.executable, "-c",
                             "import resource; print(resource.getrlimit(resource.RLIMIT_AS)[0])"],
                            self.out / "child-cap", timeout=5)
        self.assertEqual(run["rc"], 0)
        inherited = CHECK.resource.getrlimit(CHECK.resource.RLIMIT_AS)
        expected = min([CHECK.ADDRESS_SPACE_CAP] + [n for n in inherited if n != CHECK.resource.RLIM_INFINITY])
        self.assertEqual(int(Path(run["stdout"]).read_text()), expected)
        self.assertGreater(run["memory"]["peak_rss_kib"], 0)
        self.assertTrue(Path(run["metadata"]).is_file())

    def test_timeout_reaps_the_process_group(self):
        pidfile = self.out / "descendant.pid"
        program = ("import os,time,pathlib; p=os.fork(); os.setsid() if p==0 else None; "
                   f"pathlib.Path({str(pidfile)!r}).write_text(str(os.getpid())) if p==0 else None; "
                   "time.sleep(30)")
        with self.assertRaisesRegex(ValueError, "timeout"):
            CHECK.execute([sys.executable, "-c", program], self.out / "timeout", timeout=0.3)
        self.assertTrue(pidfile.is_file(), "negative control failed to launch its descendant")
        status = Path("/proc") / pidfile.read_text() / "status"
        if status.exists():
            self.assertRegex(status.read_text(), r"State:\s+Z", "descendant survived timeout cleanup")

    def test_provider_snapshot_rejects_timestamp_change(self):
        path = self.out / "provider.el"
        path.write_text("(provide 'provider)\n")
        stat = path.stat()
        record = {str(path): dict(sha256=CHECK.sha(path), mtime_ns=stat.st_mtime_ns, mode=stat.st_mode & 0o777)}
        CHECK.validate_provider_snapshot(record)
        os.utime(path, ns=(stat.st_atime_ns, stat.st_mtime_ns + 1))
        with self.assertRaisesRegex(ValueError, "stale provider snapshot"):
            CHECK.validate_provider_snapshot(record)

    def test_provider_snapshot_rejects_parent_alias(self):
        real = self.out / "real"
        real.mkdir()
        path = real / "provider.el"
        path.write_text("(provide 'provider)\n")
        alias = self.out / "alias"
        alias.symlink_to(real, target_is_directory=True)
        stat = path.stat()
        record = {str(alias / path.name): dict(sha256=CHECK.sha(path), mtime_ns=stat.st_mtime_ns, mode=stat.st_mode & 0o777)}
        with self.assertRaisesRegex(ValueError, "aliased provider parent"):
            CHECK.validate_provider_snapshot(record)

    def test_original_provider_alias_retarget_with_identical_bytes(self):
        first, second, alias = [self.out / name for name in ("first.el", "second.el", "original.el")]
        first.write_text("(provide 'same-provider)\n")
        second.write_bytes(first.read_bytes())
        alias.symlink_to(first)
        stat = alias.stat()
        record = {str(alias): dict(target=str(first), sha256=CHECK.sha(alias),
                                   mtime_ns=stat.st_mtime_ns, mode=stat.st_mode & 0o777)}
        CHECK.validate_provider_sources(record)
        alias.unlink()
        alias.symlink_to(second)
        with self.assertRaisesRegex(ValueError, "retargeted consumer provider"):
            CHECK.validate_provider_sources(record)

    def test_provider_membership_rejects_new_shadowing_file(self):
        directory = self.out / "providers"
        directory.mkdir()
        source = directory / "provider.el"
        source.write_text("(provide 'provider)\n")
        record = {str(source): {}}
        CHECK.validate_provider_membership([directory], record)
        source.with_suffix(".elc").write_bytes(b";ELC")
        with self.assertRaisesRegex(ValueError, "provider membership changed"):
            CHECK.validate_provider_membership([directory], record)


if __name__ == "__main__":
    unittest.main()
