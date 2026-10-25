#!/usr/bin/env python3
"""Private fake-child controls for the read-only MCP smoke launcher."""
import json
import os
from pathlib import Path
import shlex
import signal
import subprocess
import sys
import tempfile
import time
import unittest

ROOT = Path(__file__).resolve().parents[1]


def fake_child(case):
    def record():
        with open(os.environ["FAKE_SMOKE_PID_FILE"], "a") as stream:
            stream.write(json.dumps(dict(pid=os.getpid(), pgid=os.getpgrp())) + "\n")
    record()
    if case == "exit-early":
        return 9
    for line in sys.stdin:
        request = json.loads(line)
        identifier = request.get("id")
        if identifier is None:
            continue
        if case == "missing" and identifier == 4:
            return 0
        if identifier == 1:
            result = dict(protocolVersion="2025-06-18", capabilities={"tools": {}},
                          serverInfo={"name": "fake", "version": "1"})
        elif identifier == 2:
            result = {"tools": [{"name": f"tool-{i}"} for i in range(40)]}
            if case == "duplicate-tools":
                result["tools"][39] = result["tools"][0]
        else:
            query = request["params"]["arguments"]["query"]
            if identifier == 4 and query != '"cl-threads"':
                result = {"isError": True, "content": [{"type": "text", "text": "FTS syntax error"}]}
            else:
                result = {"content": [{"type": "text", "text": '{"rows":[]}'}]}
            if identifier == 4 and case == "iserror":
                result["isError"] = True
            if identifier == 3 and case == "bad-result":
                result = {"content": [{"type": "text", "text": "not a search result"}]}
        response = {"jsonrpc": "2.0", "id": identifier, "result": result}
        if identifier == 4 and case == "rpc-error":
            response = {"jsonrpc": "2.0", "id": 4, "error": {"code": -32603, "message": "failure"}}
        print(json.dumps(response), flush=True)
        if identifier == 1 and case == "unsolicited":
            print(json.dumps({"jsonrpc": "2.0", "id": 4, "result": {"content": [{"type": "text", "text": '{"rows":[]}'}]}}), flush=True)
        if identifier == 2 and case == "duplicate-response":
            print(json.dumps(response), flush=True)
        if identifier == 4 and case == "late-duplicate":
            print(json.dumps(response), flush=True)
        if identifier == 4 and case == "extra-error":
            print(json.dumps({"jsonrpc": "2.0", "id": 999, "error": {"code": -32603, "message": "late failure"}}), flush=True)
        if identifier == 4 and case == "nonzero":
            return 7
        if identifier == 4 and case in ("notification-error", "notification-result"):
            key = "error" if case.endswith("error") else "result"
            print(json.dumps({"jsonrpc": "2.0", "method": "notifications/progress", key: {}}), flush=True)
        if identifier == 4 and case in ("flood", "unterminated", "leader-exit"):
            sys.stdin.read()  # Exercise the final EOF drain, not initial receipt.
            if case == "leader-exit":
                read_end, write_end = os.pipe()
                if os.fork() == 0:
                    os.close(read_end)
                    signal.signal(signal.SIGTERM, signal.SIG_IGN)
                    record()
                    os.write(write_end, b"1")
                    os.close(write_end)
                    while True:
                        time.sleep(1)
                os.close(write_end)
                os.read(read_end, 1)
                os.close(read_end)
                return 0
            block = (json.dumps({"jsonrpc": "2.0", "method": "notifications/progress"}) + "\n").encode() if case == "flood" else b"x" * 65536
            while True:
                os.write(sys.stdout.fileno(), block)
    return 0


class SmokeTests(unittest.TestCase):
    def run_smoke(self, case, script=None):
        with tempfile.TemporaryDirectory(prefix="mcp-smoke-test-") as directory:
            temporary = Path(directory)
            launcher = temporary / "launcher.sh"
            launcher.write_text(f"#!/bin/sh\nexec {shlex.quote(sys.executable)} {shlex.quote(str(Path(__file__).resolve()))} --fake-child {shlex.quote(case)}\n")
            # The original verifier's sleeps are irrelevant to its parser
            # defect; this test-only shim keeps its false-green control fast.
            sleep = temporary / "sleep"
            sleep.write_text("#!/bin/sh\nexit 0\n")
            sleep.chmod(0o755)
            environment = dict(os.environ, ANVIL_MCP_LAUNCHER=str(launcher),
                               ANVIL_MCP_SMOKE_DIR=str(temporary / "out"),
                               FAKE_SMOKE_PID_FILE=str(temporary / "pids"),
                               ANVIL_MCP_SMOKE_TIMEOUT="0.6",
                               ANVIL_MCP_SMOKE_CLEANUP_TIMEOUT="0.2",
                               PATH=str(temporary) + os.pathsep + os.environ["PATH"])
            result = None
            try:
                result = subprocess.run(["bash", str(script or ROOT / "tools/ai/integ-wh-mcp-smoke.sh"), "/bin/true"],
                                        cwd=ROOT, env=environment, capture_output=True, text=True, timeout=4)
                recorded = [json.loads(line) for line in (temporary / "pids").read_text().splitlines()]
                survivors = []
                for item in recorded:
                    try:
                        state = Path(f'/proc/{item["pid"]}/stat').read_text().rsplit(")", 1)[1].split()[0]
                        if state != "Z":
                            survivors.append(item["pid"])
                    except FileNotFoundError:
                        pass
                result.owned_survivors = survivors
                if os.environ.get("MCP_SMOKE_TEST_ARTIFACTS"):
                    artifacts = Path(os.environ["MCP_SMOKE_TEST_ARTIFACTS"]) / case
                    artifacts.mkdir(parents=True, exist_ok=True)
                    (artifacts / "summary.out").write_text(result.stdout)
                    (artifacts / "summary.err").write_text(result.stderr)
                    (artifacts / "result.json").write_text(json.dumps(dict(
                        returncode=result.returncode, owned_survivors=survivors,
                        recorded_private_processes=recorded)) + "\n")
                    for name in ("anvil-smoke.out", "anvil-smoke.err"):
                        if (temporary / "out" / name).exists():
                            (artifacts / name).write_bytes((temporary / "out" / name).read_bytes())
                return result
            finally:
                if (temporary / "pids").exists():
                    for item in [json.loads(line) for line in (temporary / "pids").read_text().splitlines()]:
                        try:
                            os.killpg(item["pgid"], signal.SIGKILL)
                        except ProcessLookupError:
                            pass

    def test_valid_response_driven_handshake_and_quoted_fts(self):
        result = self.run_smoke("valid")
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(len(result.stdout.splitlines()), 4)
        self.assertEqual(result.stdout.splitlines()[1], "2 tools=40")

    def test_all_negative_controls_fail_closed(self):
        for case in ("rpc-error", "iserror", "bad-result", "duplicate-tools",
                     "duplicate-response", "nonzero", "missing", "exit-early", "unsolicited",
                     "late-duplicate", "extra-error", "notification-error", "notification-result"):
            with self.subTest(case=case):
                result = self.run_smoke(case)
                self.assertNotEqual(result.returncode, 0, result.stdout)
                self.assertEqual(len(result.stdout.splitlines()), 4)
                self.assertEqual(result.owned_survivors, [])

    def test_final_drain_and_leader_exit_cleanup_are_bounded(self):
        for case in ("flood", "unterminated", "leader-exit"):
            with self.subTest(case=case):
                started = time.monotonic()
                result = self.run_smoke(case)
                self.assertNotEqual(result.returncode, 0, result.stdout)
                self.assertLess(time.monotonic() - started, 3)
                self.assertEqual(result.owned_survivors, [])
                self.assertEqual(len(result.stdout.splitlines()), 4)
                expected = {"flood": "deadline exceeded", "unterminated": "1 MiB",
                            "leader-exit": "did not close after stdin EOF"}[case]
                self.assertIn(expected, result.stderr)


if __name__ == "__main__":
    if len(sys.argv) == 3 and sys.argv[1] == "--fake-child":
        raise SystemExit(fake_child(sys.argv[2]))
    if len(sys.argv) == 3 and sys.argv[1] == "--old-control":
        result = SmokeTests().run_smoke("iserror", Path(sys.argv[2]).resolve())
        print(result.stdout, end="")
        print("OLD-SMOKE-CHILD-EXIT", result.returncode)
        # This assertion must fail against the original false-green script.
        raise SystemExit(0 if result.returncode != 0 else 1)
    if len(sys.argv) == 3 and sys.argv[1] == "--notification-old-control":
        result = SmokeTests().run_smoke("notification-error", Path(sys.argv[2]).resolve())
        print(result.stdout, end="")
        print("OLD-SMOKE-CHILD-EXIT", result.returncode)
        raise SystemExit(0 if result.returncode != 0 else 1)
    unittest.main()
