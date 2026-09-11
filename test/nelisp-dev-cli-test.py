#!/usr/bin/env python3
"""Subprocess contract tests for the machine DEV CLI."""
import json
import subprocess
import tempfile
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
CLI = ROOT / "tools" / "ai" / "nelisp-ai.sh"


class DevCliTests(unittest.TestCase):
    def run_request(self, payload):
        with tempfile.NamedTemporaryFile("w", encoding="utf-8", suffix=".json") as request:
            json.dump(payload, request, ensure_ascii=False)
            request.flush()
            return subprocess.run([str(CLI), "dev", "--request", request.name, "--json"],
                                  capture_output=True, text=True, encoding="utf-8",
                                  timeout=5)

    def base(self, operation="capabilities", arguments=None, session=None):
        return {"schema_version": "1", "operation": operation,
                "request_id": "日本語-request", "arguments": arguments or {},
                "target": "host-emacs", "session_id": session, "limits": {}}

    def test_one_utf8_json_and_stderr_separation(self):
        result = self.run_request(self.base())
        self.assertEqual(result.returncode, 0)
        self.assertEqual(len(result.stdout.splitlines()), 1)
        self.assertEqual(result.stderr, "")
        decoded = json.loads(result.stdout)
        self.assertEqual(decoded["request_id"], "日本語-request")
        self.assertIsNone(decoded["identity"]["session_id"])
        self.assertIsNone(decoded["identity"]["source_content_hash"])
        self.assertIsNone(decoded["next_cursor"])
        self.assertEqual(decoded["diagnostics"], [])

    def test_malformed_is_one_json_exit_two(self):
        with tempfile.NamedTemporaryFile("w", encoding="utf-8") as request:
            request.write("{bad")
            request.flush()
            result = subprocess.run([str(CLI), "dev", "--request", request.name, "--json"],
                                    capture_output=True, text=True, timeout=5)
        self.assertEqual(result.returncode, 2)
        self.assertEqual(len(result.stdout.splitlines()), 1)
        self.assertEqual(json.loads(result.stdout)["status"], "failed")

    def test_live_session_four_and_missing_report_three(self):
        live = self.run_request(self.base("test", session="foreign-session"))
        self.assertEqual(live.returncode, 4)
        missing = self.run_request(self.base("test", {"gate": "missing-for-contract"}))
        self.assertEqual(missing.returncode, 3)
        self.assertEqual(json.loads(missing.stdout)["status"], "inconclusive")

    def test_invalid_contracts_are_data_errors(self):
        for payload in ([], None, 1, {**self.base(), "limits": {"bytes": 999}},
                        {**self.base(), "arguments": []},
                        {**self.base(), "operation": "does-not-exist"}):
            result = self.run_request(payload)
            self.assertEqual(result.returncode, 2, result.stdout)
            self.assertEqual(json.loads(result.stdout)["status"], "failed")

    def test_duplicate_keys_and_trailing_json_are_rejected(self):
        for text in ('{"schema_version":"1","schema_version":"1"}',
                     json.dumps(self.base()) + '\n{}'):
            with tempfile.NamedTemporaryFile("w", encoding="utf-8") as request:
                request.write(text)
                request.flush()
                result = subprocess.run([str(CLI), "dev", "--request", request.name, "--json"],
                                        capture_output=True, text=True, timeout=5)
            self.assertEqual(result.returncode, 2, result.stdout)
            self.assertEqual(json.loads(result.stdout)["status"], "failed")


if __name__ == "__main__":
    unittest.main(verbosity=2)
