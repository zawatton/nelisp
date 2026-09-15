#!/usr/bin/env python3
"""Offline resolver contracts; no host Emacs or native rebuild required."""
import copy
import hashlib
import sys
from pathlib import Path
import unittest

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "tools"))
from nelisp_packages import resolve, lock_bytes, read_lock, verify_artifact


def release(version, dependencies=None, yanked=False):
    return {"version": version, "dependencies": dependencies or {},
            "sha256": hashlib.sha256(version.encode()).hexdigest(),
            "url": "https://example.invalid/" + version + ".nl",
            "yanked": yanked}


class ResolverContract(unittest.TestCase):
    def setUp(self):
        self.index = {"schema_version": 1, "packages": {
            "a": [release("1.0.0", {"shared": "1"}),
                  release("1.1.0", {"shared": "2"})],
            "b": [release("1.0.0", {"shared": "1.2"})],
            "shared": [release("1.1.0"), release("1.2.0"), release("2.0.0")],
        }}

    def test_backtracks_and_orders_dependencies_before_dependents(self):
        lock = resolve({"a": "1", "b": "1"}, self.index)
        self.assertEqual([(p["name"], p["version"]) for p in lock["packages"]],
                         [("shared", "1.2.0"), ("a", "1.0.0"), ("b", "1.0.0")])
        self.assertEqual(read_lock(lock_bytes(lock)), lock)

    def test_order_independent_bytes(self):
        first = resolve({"a": "1", "b": "1"}, self.index)
        other = copy.deepcopy(self.index)
        other["packages"] = dict(reversed(list(other["packages"].items())))
        for releases in other["packages"].values():
            releases.reverse()
        self.assertEqual(lock_bytes(first), lock_bytes(resolve({"b": "1", "a": "1"}, other)))

    def test_conflict_and_cycle_are_errors(self):
        with self.assertRaisesRegex(ValueError, "shared"):
            resolve({"a": "=1.1.0", "b": "1"}, self.index)
        self.index["packages"]["shared"][1]["dependencies"] = {"b": "1"}
        with self.assertRaisesRegex(ValueError, "cycle"):
            resolve({"b": "1"}, self.index)

    def test_locked_version_is_preferred_until_update(self):
        old = resolve({"shared": "1"}, self.index)
        self.index["packages"]["shared"].append(release("1.9.0"))
        self.assertEqual(resolve({"shared": "1"}, self.index, old), old)
        self.assertEqual(resolve({"shared": "1"}, self.index)["packages"][0]["version"], "1.9.0")

    def test_yanked_release_only_usable_from_matching_lock(self):
        old = resolve({"b": "1"}, self.index)
        self.index["packages"]["b"][0]["yanked"] = True
        self.assertEqual(resolve({"b": "1"}, self.index, old), old)
        with self.assertRaisesRegex(ValueError, "b"):
            resolve({"b": "1"}, self.index)

    def test_changed_locked_integrity_is_rejected(self):
        old = resolve({"b": "1"}, self.index)
        self.index["packages"]["b"][0]["sha256"] = "0" * 64
        with self.assertRaisesRegex(ValueError, "changed.*b"):
            resolve({"b": "1"}, self.index, old)

    def test_zero_major_bounds_and_exact_versions(self):
        self.index["packages"]["x"] = [release(v) for v in ["0.0.1", "0.0.2", "0.1.0", "0.1.9", "0.2.0"]]
        for constraint, expected in [("0.0.1", "0.0.1"), ("0.1", "0.1.9"),
                                     ("0", "0.2.0"), ("=0.1.0", "0.1.0")]:
            self.assertEqual(resolve({"x": constraint}, self.index)["packages"][0]["version"], expected)

    def test_malformed_input_rejected(self):
        for requirement in ["*", "01.2", "1.0-beta", "=1", "1 || 2", 1]:
            with self.subTest(requirement=requirement), self.assertRaises(ValueError):
                resolve({"a": requirement}, self.index)
        self.index["packages"]["a"].append(release("1.0.0"))
        with self.assertRaisesRegex(ValueError, "duplicate"):
            resolve({"a": "1"}, self.index)

    def test_lock_requires_complete_reachable_consistent_graph(self):
        good = resolve({"b": "1"}, self.index)
        bad = copy.deepcopy(good)
        bad["packages"].pop(0)
        with self.assertRaises(ValueError):
            lock_bytes(bad)
        bad = copy.deepcopy(good)
        unused = dict(release("1.0.0"), name="unused")
        del unused["yanked"]
        bad["packages"].append(unused)
        with self.assertRaisesRegex(ValueError, "unreachable"):
            lock_bytes(bad)
        with self.assertRaises(ValueError):
            read_lock(b'schema_version = 2\n')

    def test_artifact_checksum_is_checked_over_actual_bytes(self):
        locked = resolve({"b": "1"}, self.index)["packages"][-1]
        self.assertEqual(verify_artifact(locked, b"1.0.0"), b"1.0.0")
        with self.assertRaisesRegex(ValueError, "integrity"):
            verify_artifact(locked, b"1.0.1")

    def test_empty_graph_and_unicode_urls_round_trip(self):
        empty = resolve({}, self.index)
        self.assertEqual(read_lock(lock_bytes(empty)), empty)
        self.index["packages"]["b"][0]["url"] = "https://example.invalid/日本語-📦.nl"
        lock = resolve({"b": "1"}, self.index)
        self.assertEqual(read_lock(lock_bytes(lock)), lock)

    def test_failed_resolution_does_not_mutate_inputs(self):
        old = resolve({"b": "1"}, self.index)
        before = copy.deepcopy((self.index, old))
        with self.assertRaises(ValueError):
            resolve({"b": "2"}, self.index, old)
        self.assertEqual((self.index, old), before)

    def test_cyclic_latest_release_can_fall_back(self):
        self.index["packages"]["b"].append(release("1.1.0", {"b": "1"}))
        self.assertEqual(resolve({"b": "1"}, self.index)["packages"][-1]["version"], "1.0.0")


if __name__ == "__main__":
    result = unittest.main(exit=False).result
    findings = len(result.failures) + len(result.errors)
    print(f'GATE-COUNT checked={result.testsRun} findings={findings}')
    raise SystemExit(not result.wasSuccessful())
