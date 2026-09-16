"""Doc 203 Section 5 artifact-service seam: batch/read ops and error identities.

Reuses the real local-HTTPS fixture from `nelisp-package-store-test.py`, the
same way `nelisp-registry-test.py` does, so these cases exercise the actual
`nelisp_package_store.source_bytes` path the seam sits in front of -- not a
mock of it.
"""
import hashlib
import importlib.util
import os
from pathlib import Path
import sys
import unittest
from unittest.mock import patch

ROOT = Path(__file__).resolve().parents[1]
spec = importlib.util.spec_from_file_location("https_fixture", ROOT / "test/nelisp-package-store-test.py")
fixture = importlib.util.module_from_spec(spec)
spec.loader.exec_module(fixture)

sys.path.insert(0, str(ROOT / "tools"))
from nelisp_artifact_service import ArtifactError, ensure_artifacts, read_artifact


RECEIPT_FIELDS = {"schema_version", "name", "version", "sha256", "size"}


class ArtifactService(unittest.TestCase):
    def setUp(self):
        self.https = fixture.PackageStore()
        self.https.setUp()
        self.addCleanup(self.https.doCleanups)
        self.locked = self.https.locked
        self.content = self.https.content

    def entry(self, path, content):
        """A second locked descriptor served at its own path, own content."""
        self.https.responses["/" + path] = content
        return dict(self.locked, name=path, url=self.locked["url"].replace("source", path),
                    sha256=hashlib.sha256(content).hexdigest())

    def test_ensure_artifacts_returns_versioned_receipts_in_plan_order(self):
        second = self.entry("second", b"(provide 'second)\n")
        receipts = ensure_artifacts([self.locked, second], {"offline": False})
        self.assertEqual(self.https.requests, ["/source", "/second"])
        self.assertEqual([set(r) for r in receipts], [RECEIPT_FIELDS, RECEIPT_FIELDS])
        self.assertEqual(receipts[0]["name"], self.locked["name"])
        self.assertEqual(receipts[0]["sha256"], self.locked["sha256"])
        self.assertEqual(receipts[0]["size"], len(self.content))
        self.assertEqual(receipts[0]["schema_version"], 1)
        self.assertEqual(receipts[1]["name"], "second")
        self.assertEqual(receipts[1]["size"], len(b"(provide 'second)\n"))
        # Same cache layout as calling source_bytes directly: one file per digest.
        cached = self.https.cache / (self.locked["sha256"] + ".nl")
        self.assertEqual(cached.read_bytes(), self.content)

    def test_ensure_artifacts_empty_plan_touches_nothing(self):
        self.assertEqual(ensure_artifacts([]), [])
        self.assertEqual(self.https.requests, [])
        self.assertFalse(self.https.cache.exists())

    def test_read_artifact_matches_source_bytes(self):
        self.assertEqual(read_artifact(self.locked, offline=False), self.content)
        self.assertEqual(read_artifact(self.locked), self.content)
        self.assertEqual(self.https.requests, ["/source"])

    def test_offline_miss_is_classified_missing_and_makes_no_request(self):
        with self.assertRaises(ArtifactError) as cm:
            ensure_artifacts([self.locked])
        self.assertEqual(cm.exception.identity, "missing")
        self.assertEqual(str(cm.exception), f"missing cached package {self.locked['name']}; run nelisp fetch")
        self.assertEqual(self.https.requests, [])
        with self.assertRaises(ArtifactError) as cm:
            read_artifact(self.locked)
        self.assertEqual(cm.exception.identity, "missing")

    def test_digest_mismatch_is_classified(self):
        bad = dict(self.locked, sha256="0" * 64)
        with self.assertRaises(ArtifactError) as cm:
            ensure_artifacts([bad], {"offline": False})
        self.assertEqual(cm.exception.identity, "digest-mismatch")
        self.assertIn("artifact integrity mismatch", str(cm.exception))
        self.assertFalse(self.https.cache.exists())
        # A corrupted cache entry (not just a bad locked digest) is the same identity.
        self.assertEqual(ensure_artifacts([self.locked], {"offline": False})[0]["sha256"], self.locked["sha256"])
        cached = self.https.cache / (self.locked["sha256"] + ".nl")
        cached.write_bytes(b"corruption")
        with self.assertRaises(ArtifactError) as cm:
            read_artifact(self.locked, offline=False)
        self.assertEqual(cm.exception.identity, "digest-mismatch")

    def test_oversized_is_classified_and_cap_applies_during_transfer(self):
        with patch("nelisp_package_store.MAX_ARTIFACT_BYTES", 4):
            with self.assertRaises(ArtifactError) as cm:
                ensure_artifacts([self.locked], {"offline": False})
        self.assertEqual(cm.exception.identity, "oversized")
        self.assertIn("exceeds", str(cm.exception))
        self.assertFalse(self.https.cache.exists())

    def test_redirect_is_classified_and_never_published(self):
        redirected = dict(self.locked, url=self.locked["url"].replace("source", "redirect"))
        with self.assertRaises(ArtifactError) as cm:
            ensure_artifacts([redirected], {"offline": False})
        self.assertEqual(cm.exception.identity, "redirect")
        self.assertFalse(self.https.cache.exists())

    def test_invalid_utf8_and_embedded_nul_are_both_classified_invalid_content(self):
        not_utf8 = self.entry("badutf8", b"\xff\xfe not valid utf-8")
        with self.assertRaises(ArtifactError) as cm:
            ensure_artifacts([not_utf8], {"offline": False})
        self.assertEqual(cm.exception.identity, "invalid-content")
        has_nul = self.entry("hasnul", b"(provide 'x)\0(more)\n")
        with self.assertRaises(ArtifactError) as cm:
            ensure_artifacts([has_nul], {"offline": False})
        self.assertEqual(cm.exception.identity, "invalid-content")
        self.assertIn("contains NUL", str(cm.exception))
        self.assertFalse(self.https.cache.exists())

    def test_transport_failures_are_not_reclassified(self):
        with patch.dict(os.environ, SSL_CERT_FILE=str(self.https.cache / "absent-ca.pem")):
            with self.assertRaises(OSError) as cm:
                ensure_artifacts([self.locked], {"offline": False})
        self.assertNotIsInstance(cm.exception, ArtifactError)

    def test_unclassified_value_error_passes_through_unwrapped(self):
        with patch("nelisp_artifact_service.source_bytes", side_effect=ValueError("an unrelated failure")):
            with self.assertRaises(ValueError) as cm:
                ensure_artifacts([self.locked])
        self.assertNotIsInstance(cm.exception, ArtifactError)
        self.assertEqual(str(cm.exception), "an unrelated failure")

    def test_policy_validation(self):
        with self.assertRaises(ValueError):
            ensure_artifacts([], {"offline": False, "unknown": 1})
        with self.assertRaises(ValueError):
            ensure_artifacts([], {"offline": "no"})
        self.assertEqual(ensure_artifacts([], {}), [])


if __name__ == "__main__":
    result = unittest.main(exit=False).result
    findings = len(result.failures) + len(result.errors)
    print(f'GATE-COUNT checked={result.testsRun} findings={findings}')
    raise SystemExit(not result.wasSuccessful())
